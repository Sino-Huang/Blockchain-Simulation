package main

import (
	"time"
	"fmt"
	"sync"
	"strings"
	"github.com/republicprotocol/co-go"
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
	// make Blocks Buffer
	BSB := make(chan Blocks, 500)
	// inform changes channel
	ReadytoSendchan := make(chan Blocks, 100)
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
		duration := replica.stepDuration
		cbmutex := sync.RWMutex{}
		var chainBlocks []Blocks
		go func() {
			for {
				time.Sleep(time.Duration(duration / 2) * time.Second)
				if len(chainBlocks) > 0 {
					cbmutex.Lock()
					theLong := chainBlocks[0]
					for _, v := range chainBlocks {
						if len(v.Blocks) > len(theLong.Blocks) {
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
			//BSB <-<- replica.blockReceiver
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

	/****************************************/

	go func() { // deal with new Blocks
		for {
			theTime := <-replica.blockGenerator
			//generate next block
			consensusbmutex.Lock()
			bcupdatemutex.Lock()
			waitingbmutex.Lock()
			NB := NewBlock(replica.blockchains.EndOfLongestBlockchain().Header, replica.blockchains.EndOfLongestBlockchain().Height+1, theTime.Unix())
			WaitingBlocks = WaitingBlocks.Append(NB)
			if len(WaitingBlocks.Blocks) == 1 { // broad cast this new block so as to make consensus
				WaitingBlocks.Blocks[0] = replica.Sign(WaitingBlocks.Blocks[0])
				ConsensusBlocks = ConsensusBlocks.Append(WaitingBlocks.Blocks[0])
				ReadytoSendchan <- NewBlocks().Append(WaitingBlocks.Blocks[0])
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
				if theBlocks.Latest().Timestamp < time.Now().Unix() && theBlocks.Latest().Timestamp > Temp.Timestamp &&(theBlocks.Latest().Height > Temp.Height+1 || (theBlocks.Latest().Height == Temp.Height+1 && theBlocks.Latest().ParentHeader == Temp.Header)) { // verify the incoming block
					// check if it is already in ConsensusBlock
					flag := true
					consensusbmutex.RLock()
					for _, v := range ConsensusBlocks.Blocks {
						if v.Signature == theBlocks.Latest().Signature {
							flag = false
						}
					}
					consensusbmutex.RUnlock()
					if flag {
						consensusbmutex.Lock()
						ConsensusBlocks = ConsensusBlocks.Append(theBlocks.Latest())
						consensusbmutex.Unlock()
						//help to broadcast if it is the first time ****** or do not help as it may produce duplicate message ******
						//ReadytoSendchan <- theBlocks
					}
				}// else if theBlocks.Latest().Height < Temp.Height+1 { // it means someone is outdated, thus broadcast local blockchain to help it ****** or do not help as it may produce duplicate message ******
				//	bcupdatemutex.RLock()
				//	ReadytoSendchan <- replica.blockchains.LongestBlockchain()
				//	bcupdatemutex.RUnlock()
				//}

			} else {
				// means it is the blockchain comming
				bcupdatemutex.RLock()
				Temp := replica.blockchains.LongestBlockchain()
				bcupdatemutex.RUnlock()
				if len(theBlocks.Blocks) > len(Temp.Blocks) {
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
						//preheight := replica.blockchains.endOfLongestBlockchain.Height
						//ReadytoSendchan <- theBlocks //  help to broadcast the longest chain ****** or do not help as it may produce duplicate message ******
						// delete invalid local blocks and readding the block into waiting list
						heightrange := replica.blockchains.endOfLongestBlockchain.Height - divergeHeight
						for i := 0; i < int(heightrange); i++ {
							Temp := replica.blockchains.endOfLongestBlockchain
							replica.blockchains.endOfLongestBlockchain = replica.blockchains.blocks[Temp.ParentHeader]
							delete(replica.blockchains.blocks, Temp.Header)
							if Temp.Signature.Authority() == replica.id {
								// readding the block into waiting list
								Temp.Timestamp = time.Now().Unix()
								WaitingBlocks = WaitingBlocks.Append(Temp)
							}
						}

						// add new local blocks
						for i := int(divergeHeight) + 1; i < len(theBlocks.Blocks); i++ {
							replica.blockchains.ReplaceEndOfLongestBlockchain(theBlocks.Blocks[i])
						}
						//fmt.Println("(",preheight, ")", replica.id, "converge to", "(", theBlocks.Latest().Height, ")", theBlocks.Latest().Header)
						//update observe list and also update the first element of waiting list

						NCB := NewBlocks()
						CurrentHeight := replica.blockchains.endOfLongestBlockchain.Height
						for _, v := range ConsensusBlocks.Blocks {
							if v.Height >= CurrentHeight+1 {
								NCB = NCB.Append(v)
							}
						}
						ConsensusBlocks = NCB
						// update the first element of the waiting list (incoming blocks condition)
						if len(WaitingBlocks.Blocks) > 0 {
							if WaitingBlocks.Blocks[0].Height != replica.blockchains.endOfLongestBlockchain.Height+1 && WaitingBlocks.Blocks[0].ParentHeader != replica.blockchains.endOfLongestBlockchain.Header {
								TempBlock := NewBlock(replica.blockchains.endOfLongestBlockchain.Header, replica.blockchains.endOfLongestBlockchain.Height+1, WaitingBlocks.Blocks[0].Timestamp)
								TempBlock = replica.Sign(TempBlock)
								WaitingBlocks.Blocks[0] = TempBlock
								ConsensusBlocks = ConsensusBlocks.Append(TempBlock)
								// sending the new Block request
								ReadytoSendchan <- NewBlocks().Append(WaitingBlocks.Blocks[0])
							}
						}
					}
					waitingbmutex.Unlock()
					bcupdatemutex.Unlock()
					consensusbmutex.Unlock()
				} //else if len(theBlocks.Blocks) < len(Temp.Blocks){ // it means someone is outdated, thus broadcast local blockchain to help it ****** or do not help as it may produce duplicate message ******
					//ReadytoSendchan <- Temp // re-broadcast the longest block chain
				//}
			}

		}
	}()
	go func() { // make consensus and produce the consensus block at each step duration
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
				if v.Height == replica.blockchains.endOfLongestBlockchain.Height+1 && v.ParentHeader == replica.blockchains.endOfLongestBlockchain.Header && v.Timestamp < time.Now().Unix() {
					if len(SeniorB.Blocks) == 0 {
						SeniorB = SeniorB.Append(v)
						index = i
					} else if SeniorB.Latest().Timestamp > v.Timestamp {
						SeniorB = NewBlocks().Append(v)
						index = i
					}
				}
			}
			if index != -1 {
				//fmt.Println(replica.id,"Consensus Block is", ConsensusBlocks.Blocks[index].Signature, ConsensusBlocks.Blocks[index].Height, replica.blockchains.endOfLongestBlockchain.Height)
				// all replicates delete the target block in their consensus blocks group ,
				Temp := NewBlocks()
				Temp.Blocks = append(Temp.Blocks, ConsensusBlocks.Blocks[:index]...)
				Temp.Blocks = append(Temp.Blocks, ConsensusBlocks.Blocks[index+1:]...)
				ConsensusBlocks = Temp
				//but only the responsible replicate update the blockchain in case of Hacking issue
				if SeniorB.Latest().Signature.Authority() == replica.id {
					// check the local waiting list to check whether this is a fake message
					if len(WaitingBlocks.Blocks) == 0 {
						fmt.Println("No LocalBackup For Consensus Block! Potential FAKE, Abandon the Block!")
					} else if WaitingBlocks.Blocks[0].Signature != SeniorB.Latest().Signature ||
						WaitingBlocks.Blocks[0].Header != SeniorB.Latest().Header ||
						WaitingBlocks.Blocks[0].ParentHeader != SeniorB.Latest().ParentHeader ||
						WaitingBlocks.Blocks[0].Timestamp != SeniorB.Latest().Timestamp ||
						WaitingBlocks.Blocks[0].Height != SeniorB.Latest().Height{
						fmt.Println("Consensus Block is not same as the LocalBackup! Potential FAKE, Abandon the Block!")
					} else {
						replica.blockchains.ReplaceEndOfLongestBlockchain(SeniorB.Latest())
						ReadytoSendchan <- replica.blockchains.LongestBlockchain()
						//fmt.Println(replica.id, "Made a new block")

						// delete the block in the waiting block ,update Waiting blocks
						NWB := NewBlocks()
						NWB.Blocks = append(NWB.Blocks, WaitingBlocks.Blocks[1:]...)
						if len(NWB.Blocks) > 0 { // New Block Consensus condition
							NWB.Blocks[0] = NewBlock(SeniorB.Latest().Header, SeniorB.Latest().Height+1, NWB.Blocks[0].Timestamp)
							NWB.Blocks[0] = replica.Sign(NWB.Blocks[0])
							// re-input the new waiting block into consensus block and send it out.
							ConsensusBlocks = ConsensusBlocks.Append(NWB.Blocks[0])

							ReadytoSendchan <- NewBlocks().Append(NWB.Blocks[0])
						}

						WaitingBlocks = NWB

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
			timeout := make(chan bool)
			success := make(chan bool)
			go func() {
				time.Sleep(time.Duration(replica.stepDuration) * time.Second) // after step duration Sec if the channel is not active then timeout
				timeout <- true
			}()
			go func() {
				// **** NOTE *****
				// I was thinking use goroutine to send message concurrently. However, i found out that if the number of goroutine increases
				// , then it is highly possible that starvation happens!
				// therefore I do not create too many goroutines
				co.ForAll(len(replica.conns), func(i int) {
					replica.conns[i] <- ReadyBS
				})
				success <- true
			}()
			select {
			case <-success:
				//fmt.Println("Success")
			case <-timeout:
				//fmt.Println("Timeout")
			}
		}
	}
}
func CheckValid(theBlocks *Blocks, authorities []Authority, blockchains *Blockchains) (bool, int64) {
	defer func() (bool, int64) {
		if r := recover(); r != nil {
			fmt.Println(r, "Abandon the request!")
			return false, -1
		}
		return false, -1
	}()
	LocalHeight := blockchains.endOfLongestBlockchain.Height
	LocalDivergeBlock := blockchains.endOfLongestBlockchain
	if theBlocks.Blocks[0].Header != blockchains.LongestBlockchain().Blocks[0].Header || theBlocks.Blocks[0].ParentHeader != blockchains.LongestBlockchain().Blocks[0].ParentHeader {
		return false, -1 // verify first block
	}

	for i := len(theBlocks.Blocks) - 1; i > 0; i-- {
		if theBlocks.Blocks[i].Timestamp > time.Now().Unix() {
			fmt.Println("Invalid TimeStamp! Abandon the request!", )
			return false, -1
		}
		if len(theBlocks.Blocks[i].Header) < 44 {
			fmt.Println("Invalid Header! Abandon the request!", len(theBlocks.Blocks[i].Header))
			return false, -1
		}
		flag := true
		for _, v := range authorities {
			authorName := theBlocks.Blocks[i].Signature.Authority()
			if v == authorName {
				flag = false
			}
		}
		if flag {
			fmt.Println("Invalid Authority! Abandon the request!")
			return false, -1
		} else {
			if theBlocks.Blocks[i].ParentHeader != theBlocks.Blocks[i-1].Header || strings.Split(string(theBlocks.Blocks[i].Signature), ":")[0] != string(theBlocks.Blocks[i].Header) {
				fmt.Println("Invalid Header! Abandon the request!")
				return false, -1
			}
			if theBlocks.Blocks[i].Timestamp < theBlocks.Blocks[i-1].Timestamp{
				fmt.Println("Invalid TimeStamp! Abandon the request!")
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
