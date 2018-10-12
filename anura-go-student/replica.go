package main

import (
	"time"
	"fmt"
	"sync"
)

type Replica struct {
	id Authority

	// blockGenerator is a synchronous read-only channel. It is written to
	// whenever the Replica should generate a Block and propagate it to other
	// Replicas in the network.
	blockGenerator <-chan time.Time

	// blockQueryReceiver is a synchronous read-only channel used to receive
	// Queries that have been sent by other Replicas.
	blockQueryReceiver <-chan Query

	// blockReceiver is a synchronous read-only channel used to receive Blocks
	// that have been sent by other Replicas. Blocks received from this channel
	// are not necessarily to be valid.
	blockReceiver <-chan Blocks

	// conns is an array of connections. It is useful for sending Blocks and
	// BlockQueries to other Replicas in the network. Connections are not
	// guaranteed to be alive, and sending on a dead connection will block
	// forever.
	conns []chan<- Blocks

	blockchains  Blockchains
	authorities  []Authority
	stepDuration int64
}

func NewReplica(id Authority, authorities []Authority, blockReceiver <-chan Blocks, blockQueryReceiver <-chan Query, blockGenerator <-chan time.Time, conns []chan<- Blocks) Replica {
	return Replica{
		id: id,

		blockReceiver:      blockReceiver,
		blockQueryReceiver: blockQueryReceiver,
		blockGenerator:     blockGenerator,

		conns: conns,

		blockchains:  NewBlockchains(),
		authorities:  authorities,
		stepDuration: configStep,
	}
}

// Sign a Block and return it. This marks the Block as being produced by this
// Replica. If the Replica is a not a valid Authority, or the it was not the
// Replicas turn to produce a Block, other Replicas will reject the Block. It is
// assumed that the signing of a Block cannot be forged.
func (replica *Replica) Sign(block Block) Block {
	block.Signature = Signature(string(block.Header) + ":" + string(replica.id))
	return block
}

// Run the Replica until the done channel is closed.
func (replica *Replica) Run(done <-chan struct{}) {
	// make new Block buffer
	NBB := make(chan Block, 30)
	// make Query Buffer
	QB := make(chan Query, 30)
	// make Blocks Buffer
	BSB := make(chan Blocks, 30)
	// inform changes channel
	ReadytoSendchan := make(chan Blocks)
	bcupdatemutex := sync.RWMutex{}
	consensusbmutex := sync.RWMutex{}
	waitingbmutex := sync.RWMutex{}

	ConsensusBlocks := NewBlocks() // these are the blocks that wants to make consensus
	WaitingBlocks := NewBlocks()   // these are the local blocks that waits to append to the blockchain
	/*********************************************
	the block below is the receiving tasks
	 */
	go	func() {
			for{
				theTime :=<-replica.blockGenerator
				//generate next block
				bcupdatemutex.RLock()
				NB := NewBlock(replica.blockchains.EndOfLongestBlockchain().Header, replica.blockchains.EndOfLongestBlockchain().Height + 1, int64(theTime.Second()))
				bcupdatemutex.RUnlock()
				fmt.Println("Id:", replica.id, "New Block Coming")
				SNB := replica.Sign(NB)
				NBB <- SNB
			}
		}()
	go	func(){
			for{
				theQuery :=<-replica.blockQueryReceiver
				QB <- theQuery
			}
		}()
	go func() {
			for{
				theBlocks := <-replica.blockReceiver
				//fmt.Println(replica.id,"Incoming Blocks is", theBlocks)
				BSB <- theBlocks
			}
		}()
	/****************************************/

	go func() { // deal with Query
		for{
			theQuery := <-QB
			bcupdatemutex.RLock()
			theBlocks := replica.blockchains.LongestBlockchain()
			bcupdatemutex.RUnlock()
			outputBlocks := theBlocks.Range(theQuery.Begin,theQuery.End)
			theQuery.Responder <- outputBlocks // send back to the query
		}
	}()

	go func() {// deal with new Blocks
		for {
			NB:=<-NBB
			ReadytoSendchan <- NewBlocks().Append(NB)
			consensusbmutex.Lock()
			ConsensusBlocks = ConsensusBlocks.Append(NB)
			consensusbmutex.Unlock()

			waitingbmutex.Lock()
			WaitingBlocks = WaitingBlocks.Append(NB)
			waitingbmutex.Unlock()
		}
	}()

	go func() { // deal with incoming blocks
		for{
			theBlocks := <-BSB
			// if theBlocks size is 1. then it is the new block broad cast by other replica
			if len(theBlocks.Blocks) == 1{
				// check if it is outdated
				bcupdatemutex.RLock()
				Temp := replica.blockchains.endOfLongestBlockchain.Height
				bcupdatemutex.RUnlock()
				if theBlocks.Latest().Height > Temp{ // if the comming block is not outdated, help to broadcast, else broadcast the newest blockchain
					// check if it is already in ConsensusBlock
					flag := true
					consensusbmutex.RLock()
					for _,v := range ConsensusBlocks.Blocks{
						if v.Signature.Authority() == theBlocks.Latest().Signature.Authority(){
							flag = false;
						}
					}
					consensusbmutex.RUnlock()
					if flag {
						consensusbmutex.Lock()
						ConsensusBlocks = ConsensusBlocks.Append(theBlocks.Latest())
						//help to broadcast if it is the first time
						ReadytoSendchan <- theBlocks
						consensusbmutex.Unlock()
					}

				}/*else {
					bcupdatemutex.RLock()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					bcupdatemutex.RUnlock()
				}*/

			}else {
				// means it is the blockchain comming
				bcupdatemutex.RLock()
				Temp := len(replica.blockchains.LongestBlockchain().Blocks)
				bcupdatemutex.RUnlock()
				if len(theBlocks.Blocks) <= Temp {
					// broad cast the local blockchain again in order to guard the longest block chain status
					bcupdatemutex.RLock()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					bcupdatemutex.RUnlock()
				}else {
					/**
					this function verify the incoming blocks and then update if the incoming block is valid
					In branch conflicts, the function will also help to re-input the invalid block into waiting blocks list
					it will also update the waiting list and the consensus list
					 */
					 bcupdatemutex.RLock()

					// check whether the incoming block is valid
					 isValid, divergeIndex, divergeHeight :=CheckValid(&theBlocks, replica.authorities, &replica.blockchains)
					 bcupdatemutex.RUnlock()

					if isValid {
					 	// delete invalid local blocks and readding the block into waiting list
					 	fmt.Println(replica.blockchains.endOfLongestBlockchain.Height, replica.id,"Going to converge", theBlocks.Latest().Height,theBlocks.Latest().Header)
						bcupdatemutex.Lock()
						heightrange := replica.blockchains.endOfLongestBlockchain.Height - divergeHeight
					 	for i:=0 ; i<int(heightrange); i++{
					 		Temp :=replica.blockchains.endOfLongestBlockchain
					 		replica.blockchains.endOfLongestBlockchain = replica.blockchains.blocks[Temp.ParentHeader]
					 		delete(replica.blockchains.blocks, Temp.Header)
							if Temp.Signature.Authority() == replica.id{
								go func() { // readding the block into waiting list
									NWB := NewBlocks()
									waitingbmutex.RLock()
									Length := len(WaitingBlocks.Blocks)
									if Length < 2 {
										NWB = WaitingBlocks.Append(Temp)
									}else{
										NWB = NWB.Append(WaitingBlocks.Blocks[0])
										NWB = NWB.Append(Temp)
										NWB.Blocks = append(NWB.Blocks, WaitingBlocks.Blocks[1:]...)
									}
									waitingbmutex.RUnlock()

									waitingbmutex.Lock()
									WaitingBlocks = NWB
									waitingbmutex.Unlock()
								}()
							}
						}
						bcupdatemutex.Unlock()


					 	// add new local blocks
					 	bcupdatemutex.Lock()
					 	for i:=divergeIndex + 1; i < len(theBlocks.Blocks); i++{
							replica.blockchains.ReplaceEndOfLongestBlockchain(theBlocks.Blocks[i])
						}
						ReadytoSendchan <- replica.blockchains.LongestBlockchain() // first time update, help to broadcast
					 	bcupdatemutex.Unlock()
					 	//update observe list and also update the first element of waiting list
					 	go func() {
					 		NCB := NewBlocks()
					 		bcupdatemutex.RLock()
					 		CurrentHeight := replica.blockchains.endOfLongestBlockchain.Height
					 		bcupdatemutex.RUnlock()
							consensusbmutex.Lock()
							for _,v := range ConsensusBlocks.Blocks{
								if v.Height == CurrentHeight + 1{
									NCB = NCB.Append(v)
								}
							}
							ConsensusBlocks = NCB
							consensusbmutex.Unlock()
						}()
					 	waitingbmutex.RLock()
					 	Length := len(WaitingBlocks.Blocks)
					 	waitingbmutex.RUnlock()
					 	if Length > 0{
							go func() { // update the first element of the waiting list
								waitingbmutex.Lock()
								bcupdatemutex.RLock()
								TempBlock := NewBlock(replica.blockchains.endOfLongestBlockchain.Header,replica.blockchains.endOfLongestBlockchain.Height+1,WaitingBlocks.Blocks[0].Timestamp)
								TempBlock  = replica.Sign(TempBlock)
								WaitingBlocks.Blocks[0] = TempBlock
								ReadytoSendchan <- NewBlocks().Append(TempBlock)
								bcupdatemutex.RUnlock()
								WaitingBlocks.Blocks[0] = replica.Sign(WaitingBlocks.Blocks[0])
								consensusbmutex.Lock()
								ConsensusBlocks = ConsensusBlocks.Append(WaitingBlocks.Blocks[0])
								consensusbmutex.Unlock()
								waitingbmutex.Unlock()
							}()
						}
					 }
					 bcupdatemutex.RLock()
					 ReadytoSendchan <- replica.blockchains.LongestBlockchain() // re broadcast the longest block chain
					 bcupdatemutex.RUnlock()


				}
			}


		}
	}()
	go func() { // make consensus each step duration
		for{
			time.Sleep(time.Duration(replica.stepDuration) * time.Second)
			// check consensus blocks
			SeniorB := NewBlocks()
			index := -1
			consensusbmutex.RLock()
			bcupdatemutex.RLock()
			for i, v:= range ConsensusBlocks.Blocks{
				if len(SeniorB.Blocks) == 0{
					if v.Height == replica.blockchains.endOfLongestBlockchain.Height + 1{
						SeniorB = SeniorB.Append(v)
						index = i
					}
				}else {
					if SeniorB.Latest().Timestamp > v.Timestamp && v.Height == replica.blockchains.endOfLongestBlockchain.Height + 1{
						SeniorB = NewBlocks().Append(v)
						index = i
					}
				}
			}
			bcupdatemutex.RUnlock()
			consensusbmutex.RUnlock()

			if index != -1 {
				// all replicate delete the target block too in case the target authority accidentally crash and hence cause deadlock
				consensusbmutex.Lock()
				Temp := NewBlocks()
				//fmt.Println("ID",replica.id,"make consensus to delete block", ConsensusBlocks.Blocks[index].Signature)
				Temp.Blocks = append(Temp.Blocks, ConsensusBlocks.Blocks[:index]...)
				Temp.Blocks = append(Temp.Blocks, ConsensusBlocks.Blocks[index+1:]...)
				ConsensusBlocks = Temp
				consensusbmutex.Unlock()
				if SeniorB.Latest().Signature.Authority() == replica.id {
					bcupdatemutex.Lock()
					replica.blockchains.ReplaceEndOfLongestBlockchain(SeniorB.Latest())
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					bcupdatemutex.Unlock()
					// delete the block in the waiting block ,update Waiting blocks
					NWB := NewBlocks()
					waitingbmutex.RLock()
					NWB.Blocks =  append(NWB.Blocks, WaitingBlocks.Blocks[1:]...)
					waitingbmutex.RUnlock()
					Length := len(NWB.Blocks)
					if Length > 0{
						NWB.Blocks[0] = NewBlock(SeniorB.Latest().Header, SeniorB.Latest().Height + 1, NWB.Blocks[0].Timestamp)
						NWB.Blocks[0] = replica.Sign(NWB.Blocks[0])
					}

					waitingbmutex.Lock()
					WaitingBlocks = NWB
					waitingbmutex.Unlock()
					// re-input the new waiting block into consensus block and send it out.
					Temp := NewBlocks()
					if Length > 0{
						waitingbmutex.RLock()
						Temp = NewBlocks().Append(WaitingBlocks.Blocks[0])
						waitingbmutex.RUnlock()
						ReadytoSendchan <- Temp
					}
					//update consensus
					consensusbmutex.Lock()
					ConsensusBlocks = Temp
					consensusbmutex.Unlock()
				}
			}
		}
	}()

	for {
		// check whether end
		select {
		case <-done:
			return // means everything is done
		case ReadyBS := <-ReadytoSendchan: // this will block the for loop when no Ready BS comming in
			// send out the ready new Blockchain
			for _, v := range replica.conns{
				go func() { // make the sending loop concurrent so that it will not block the main program
					timeout := make(chan bool)
					go func() {
						time.Sleep(10 * time.Second)
						timeout <- true
					}()
					select {
					case v <- ReadyBS: // in case the connection is broken, we use timeout mechanism
					case <-timeout:
					}
				}()
			}
		}
		// TODO:
		// 1. Generate the next Block in the blockchain whenever the
		//    blockGenerator channel signals that it is time to do so.
		// 2. Respond to BlockQueries that received on the blockQueryReceiver
		//    channel.
		// 3. Send Blocks to other Replicas using the conns channels, and
		//    validate Blocks received by other Replicas.
		// 4. Drop the connectivity to 0.25 and see if consensus make sure that
		//    consensus can still be reached.
	}
}
func CheckValid(theBlocks *Blocks, authorities []Authority, blockchains *Blockchains) (bool, int, int64)  {
	LocalHeight := blockchains.endOfLongestBlockchain.Height
	LocalDivergeBlock := blockchains.endOfLongestBlockchain
	indexrange := -1
	outputindex := len(theBlocks.Blocks) -2
	if len(theBlocks.Blocks) < 20 {
		indexrange = 0
	}else {
		indexrange = len(theBlocks.Blocks) - 20
	}
	for i:= len(theBlocks.Blocks) -1 ; i >indexrange ; i--{
		flag := true
		for _,v := range authorities{
			if v == theBlocks.Blocks[i].Signature.Authority(){
				flag = false
			}
		}
		if flag{
			return false, -1, -1
		}else {
			if theBlocks.Blocks[i].ParentHeader != theBlocks.Blocks[i-1].Header {
				return false, -1, -1
			}
			if theBlocks.Blocks[i].Height == LocalHeight{
				// check if they are the same
				if theBlocks.Blocks[i].Signature != LocalDivergeBlock.Signature{
					LocalHeight --
					LocalDivergeBlock = blockchains.blocks[LocalDivergeBlock.ParentHeader]
					if LocalHeight != LocalDivergeBlock.Height{
						panic("Height is wrong")
					}
				}else {
					outputindex = i
				}
			}
		}
	}
	return true, outputindex, LocalHeight
}

