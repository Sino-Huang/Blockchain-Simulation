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
	NBB := make(chan time.Time, 500)
	// make Blocks Buffer
	BSB := make(chan Blocks, 500)
	// inform changes channel
	ReadytoSendchan := make(chan Blocks)
	bcupdatemutex := sync.RWMutex{}
	consensusbmutex := sync.RWMutex{}
	waitingbmutex := sync.RWMutex{}
	Timer := time.Now().Add(time.Duration(replica.stepDuration) * time.Second)

	ConsensusBlocks := NewBlocks() // these are the blocks that wants to make consensus
	WaitingBlocks := NewBlocks()   // these are the local blocks that waits to append to the blockchain
	/*********************************************
	the block below is the receiving tasks
	 */
	go func() {
		for {
			theTime := <-replica.blockGenerator
			time.Sleep(time.Duration(replica.stepDuration) * time.Second) // lower down the writen-in process so that it can receive latest longer blockchain
			NBB <- theTime
		}
	}()
	go func() { // deal with query
		for {
			theQuery := <-replica.blockQueryReceiver
			bcupdatemutex.RLock()
			theBlocks := replica.blockchains.LongestBlockchain()
			bcupdatemutex.RUnlock()
			outputBlocks := theBlocks.Range(theQuery.Begin, theQuery.End)
			theQuery.Responder <- outputBlocks // send back to the query
		}
	}()

	go func() {
		duration := replica.stepDuration
		cbmutex := sync.RWMutex{}
		chainBlocks := []Blocks{}
		go func() {
			for {
				time.Sleep(time.Duration(duration) * time.Second)
				if len(chainBlocks) > 0{
					cbmutex.Lock()
					theLong := chainBlocks[0]
					for _,v := range chainBlocks{
						if len(v.Blocks) > len(theLong.Blocks){
							theLong = v
						}
					}
					chainBlocks = []Blocks{}
					BSB <- theLong
					cbmutex.Unlock()
				}
			}
		}()
		for {
			TheB := <-replica.blockReceiver
			if len(TheB.Blocks) == 1 { // branch them
				BSB <- TheB
			} else {
				cbmutex.Lock()
				chainBlocks = append(chainBlocks, TheB) // buffer the chains
				cbmutex.Unlock()
			}
		}
	}()

	/****************************************/

	go func() { // deal with new Blocks
		for {
			theTime := <-NBB
			//generate next block
			consensusbmutex.Lock()
			bcupdatemutex.Lock()
			waitingbmutex.Lock()
			NB := NewBlock(replica.blockchains.EndOfLongestBlockchain().Header, replica.blockchains.EndOfLongestBlockchain().Height+1, int64(theTime.Second()))
			NB = replica.Sign(NB)
			ConsensusBlocks = ConsensusBlocks.Append(NB)
			WaitingBlocks = WaitingBlocks.Append(NB)
			if len(WaitingBlocks.Blocks) == 1{ // broad cast this new block so as to make consensus
				ReadytoSendchan <- NewBlocks().Append(NB)
				ReadytoSendchan <- NewBlocks().Append(NB)
				ReadytoSendchan <- NewBlocks().Append(NB)
			}
			waitingbmutex.Unlock()
			bcupdatemutex.Unlock()
			consensusbmutex.Unlock()
		}
	}()

	go func() { // deal with incoming blocks
		for {
			theBlocks := <-BSB
			// if theBlocks size is 1. then it is the new block broad cast by other replica
			if len(theBlocks.Blocks) == 1 {
				// check if it is outdated
				bcupdatemutex.RLock()
				Temp := replica.blockchains.endOfLongestBlockchain
				bcupdatemutex.RUnlock()
				if theBlocks.Latest().Height > Temp.Height + 1 || (theBlocks.Latest().Height == Temp.Height + 1 && theBlocks.Latest().ParentHeader == Temp.Header) { // if the comming block is not outdated, help to broadcast, else broadcast the newest blockchain
					// check if it is already in ConsensusBlock
					flag := true
					consensusbmutex.RLock()
					for _, v := range ConsensusBlocks.Blocks {
						if v.Signature.Authority() == theBlocks.Latest().Signature.Authority() {
							flag = false;
						}
					}
					consensusbmutex.RUnlock()
					if flag {
						consensusbmutex.Lock()
						ConsensusBlocks = ConsensusBlocks.Append(theBlocks.Latest())
						//help to broadcast if it is the first time
						ReadytoSendchan <- theBlocks
						ReadytoSendchan <- theBlocks
						ReadytoSendchan <- theBlocks
						consensusbmutex.Unlock()
					}
				}else if theBlocks.Latest().Height < Temp.Height + 1{ // it means someone is outdated, thus broadcast local blockchain to help it
					bcupdatemutex.RLock()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					bcupdatemutex.RUnlock()
				}

			} else {
				// means it is the blockchain comming
				bcupdatemutex.RLock()
				Temp := len(replica.blockchains.LongestBlockchain().Blocks)
				bcupdatemutex.RUnlock()
				if len(theBlocks.Blocks) > Temp {
					/**
					this function verify the incoming blocks and then update if the incoming block is valid
					In branch conflicts, the function will also help to re-input the invalid block into waiting blocks list
					it will also update the waiting list and the consensus list
					 */
					consensusbmutex.Lock()
					bcupdatemutex.Lock()
					waitingbmutex.Lock()

					// check whether the incoming block is valid
					isValid, divergeHeight := CheckValid(&theBlocks, replica.authorities, &replica.blockchains)
					if isValid {
						preheight := replica.blockchains.endOfLongestBlockchain.Height
						ReadytoSendchan <- replica.blockchains.LongestBlockchain() //  help to broadcast the longest chain
						ReadytoSendchan <- replica.blockchains.LongestBlockchain() //  help to broadcast the longest chain
						ReadytoSendchan <- replica.blockchains.LongestBlockchain() //  help to broadcast the longest chain
						// delete invalid local blocks and readding the block into waiting list
						heightrange := replica.blockchains.endOfLongestBlockchain.Height - divergeHeight
						for i := 0; i < int(heightrange); i++ {
							Temp := replica.blockchains.endOfLongestBlockchain
							replica.blockchains.endOfLongestBlockchain = replica.blockchains.blocks[Temp.ParentHeader]
							delete(replica.blockchains.blocks, Temp.Header)
							if Temp.Signature.Authority() == replica.id {
								go func() { // readding the block into waiting list
									NWB := NewBlocks()
									Length := len(WaitingBlocks.Blocks)
									if Length < 2 {
										NWB = WaitingBlocks.Append(Temp)
									} else {
										NWB = NWB.Append(WaitingBlocks.Blocks[0])
										NWB = NWB.Append(Temp)
										NWB.Blocks = append(NWB.Blocks, WaitingBlocks.Blocks[1:]...)
									}

									WaitingBlocks = NWB
								}()
							}
						}

						// add new local blocks
						for i := int(divergeHeight) + 1; i < len(theBlocks.Blocks); i++ {
							replica.blockchains.ReplaceEndOfLongestBlockchain(theBlocks.Blocks[i])
						}
						fmt.Println("(",preheight, ")", replica.id, "converge to", "(", theBlocks.Latest().Height, ")", theBlocks.Latest().Header)
						//update observe list and also update the first element of waiting list
						go func() {
							NCB := NewBlocks()
							CurrentHeight := replica.blockchains.endOfLongestBlockchain.Height
							for _, v := range ConsensusBlocks.Blocks {
								if v.Height >= CurrentHeight+1 {
									NCB = NCB.Append(v)
								}
							}
							ConsensusBlocks = NCB
						}()
						Length := len(WaitingBlocks.Blocks)
						if Length > 0 {
							go func() { // update the first element of the waiting list
								if WaitingBlocks.Blocks[0].Height != replica.blockchains.endOfLongestBlockchain.Height+1 && WaitingBlocks.Blocks[0].ParentHeader != replica.blockchains.endOfLongestBlockchain.Header {
									TempBlock := NewBlock(replica.blockchains.endOfLongestBlockchain.Header, replica.blockchains.endOfLongestBlockchain.Height+1, WaitingBlocks.Blocks[0].Timestamp)
									TempBlock = replica.Sign(TempBlock)
									WaitingBlocks.Blocks[0] = TempBlock
									WaitingBlocks.Blocks[0] = replica.Sign(WaitingBlocks.Blocks[0])
									// sending the new Block request
									ReadytoSendchan <- NewBlocks().Append(WaitingBlocks.Blocks[0])
									ReadytoSendchan <- NewBlocks().Append(WaitingBlocks.Blocks[0])
									ReadytoSendchan <- NewBlocks().Append(WaitingBlocks.Blocks[0])
									ConsensusBlocks = ConsensusBlocks.Append(WaitingBlocks.Blocks[0])
								}
							}()
						}
					}
					waitingbmutex.Unlock()
					bcupdatemutex.Unlock()
					consensusbmutex.Unlock()
				}else { // it means someone is outdated, thus broadcast local blockchain to help it
					bcupdatemutex.RLock()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain() // re-broadcast the longest block chain
					ReadytoSendchan <- replica.blockchains.LongestBlockchain() // re-broadcast the longest block chain
					bcupdatemutex.RUnlock()
				}
			}

		}
	}()
	go func() { // make consensus each step duration
		for {
			time.Sleep(time.Until(Timer))
			Timer = Timer.Add(time.Duration(replica.stepDuration) * time.Second)
			// check consensus blocks
			SeniorB := NewBlocks()
			index := -1
			consensusbmutex.Lock()
			bcupdatemutex.Lock()
			waitingbmutex.Lock()
			for i, v := range ConsensusBlocks.Blocks {
				if len(SeniorB.Blocks) == 0 {
					if v.Height == replica.blockchains.endOfLongestBlockchain.Height+1 && v.ParentHeader == replica.blockchains.endOfLongestBlockchain.Header {
						SeniorB = SeniorB.Append(v)
						index = i
					}
				} else {
					if SeniorB.Latest().Timestamp > v.Timestamp && v.Height == replica.blockchains.endOfLongestBlockchain.Height+1 && v.ParentHeader == replica.blockchains.endOfLongestBlockchain.Header {
						SeniorB = NewBlocks().Append(v)
						index = i
					}
				}
			}
			if index != -1 {
				//fmt.Println(replica.id,"Consensus Block is", ConsensusBlocks.Blocks[index].Signature, ConsensusBlocks.Blocks[index].Height, replica.blockchains.endOfLongestBlockchain.Height)
				// all replicate delete the target block too in case the target authority accidentally crash and hence cause deadlock
				Temp := NewBlocks()
				Temp.Blocks = append(Temp.Blocks, ConsensusBlocks.Blocks[:index]...)
				Temp.Blocks = append(Temp.Blocks, ConsensusBlocks.Blocks[index+1:]...)
				ConsensusBlocks = Temp

				//replica.blockchains.ReplaceEndOfLongestBlockchain(SeniorB.Latest())
				//ReadytoSendchan <- replica.blockchains.LongestBlockchain()
				// delete the block in the waiting block ,update Waiting blocks
				if SeniorB.Latest().Signature.Authority() == replica.id {
					replica.blockchains.ReplaceEndOfLongestBlockchain(SeniorB.Latest())
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
					fmt.Println(replica.id, "made a new block")
					NWB := NewBlocks()
					NWB.Blocks = append(NWB.Blocks, WaitingBlocks.Blocks[1:]...)
					Length := len(NWB.Blocks)
					if Length > 0 {
						NWB.Blocks[0] = NewBlock(SeniorB.Latest().Header, SeniorB.Latest().Height+1, NWB.Blocks[0].Timestamp)
						NWB.Blocks[0] = replica.Sign(NWB.Blocks[0])
					}

					WaitingBlocks = NWB

					// re-input the new waiting block into consensus block and send it out.
					Temp := NewBlocks()
					if Length > 0 {
						Temp = NewBlocks().Append(WaitingBlocks.Blocks[0])
						ReadytoSendchan <- Temp
						ReadytoSendchan <- Temp
						ReadytoSendchan <- Temp
					}
					//update consensus
					NCB := NewBlocks()
					CurrentHeight := replica.blockchains.endOfLongestBlockchain.Height
					for _, v := range ConsensusBlocks.Blocks {
						if v.Height >= CurrentHeight+1 {
							NCB = NCB.Append(v)
						}
					}
					ConsensusBlocks = NCB
				}
			}
			waitingbmutex.Unlock()
			bcupdatemutex.Unlock()
			consensusbmutex.Unlock()
		}
	}()

	for {
		// check whether end
		select {
		case <-done:
			return // means everything is done
		case ReadyBS := <-ReadytoSendchan: // this will block the for loop when no Ready BS comming in
			// send out the ready new Blockchain
			for _, v := range replica.conns {
				go func() { // make the sending loop concurrent so that it will not block the main program
					timeout := make(chan bool)
					go func() {
						time.Sleep(10 * time.Second)
						timeout <- true
					}()
					select {
					case v <- ReadyBS: // in case the connection is broken, we use timeout mechanism
					case <-timeout:
						//fmt.Println("Connection timeout")
					}
				}()
			}
		}
	}
}
func CheckValid(theBlocks *Blocks, authorities []Authority, blockchains *Blockchains) (bool, int64) {
	LocalHeight := blockchains.endOfLongestBlockchain.Height
	LocalDivergeBlock := blockchains.endOfLongestBlockchain
	for i := len(theBlocks.Blocks) - 1; i > 0; i-- {
		flag := true
		for _, v := range authorities {
			if v == theBlocks.Blocks[i].Signature.Authority() {
				flag = false
			}
		}
		if flag {
			return false, -1
		} else {
			if theBlocks.Blocks[i].ParentHeader != theBlocks.Blocks[i-1].Header {
				return false, -1
			}
			if theBlocks.Blocks[i].Height == LocalHeight {
				// check if they are the same
				if theBlocks.Blocks[i].Signature != LocalDivergeBlock.Signature {
					LocalHeight --
					LocalDivergeBlock = blockchains.blocks[LocalDivergeBlock.ParentHeader]
					if LocalHeight != LocalDivergeBlock.Height {
						panic("Height is wrong")
					}
				}
			}
		}
	}
	return true, LocalHeight
}
