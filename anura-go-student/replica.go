package main

import (
	"time"
	"fmt"
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

	ConsensusBlocks := NewBlocks() // these are the blocks that wants to make consensus
	WaitingBlocks := NewBlocks()   // these are the local blocks that waits to append to the blockchain
	/*********************************************
	the block below is the receiving tasks
	 */
	go	func() {
			for{
				theTime :=<-replica.blockGenerator
				//generate next block
				NB := NewBlock(replica.blockchains.EndOfLongestBlockchain().Header, replica.blockchains.EndOfLongestBlockchain().Height + 1, int64(theTime.Second()))
				fmt.Println("Id:", replica.id, "New Block Coming")
				NBB <- NB
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
				fmt.Println("Incoming Blocks is", theBlocks)
				BSB <- theBlocks
			}
		}()
	/****************************************/
	go func() { // deal with Query
		for{
			theQuery := <-QB
			theBlocks := replica.blockchains.LongestBlockchain()
			outputBlocks := theBlocks.Range(theQuery.Begin,theQuery.End)
			theQuery.Responder <- outputBlocks // send back to the query
		}
	}()

	go func() {// deal with new Blocks
		for {
			NB:=<-NBB
			ReadytoSendchan <- NewBlocks().Append(NB)
			ConsensusBlocks.Append(NB)
			WaitingBlocks.Append(NB)
		}
	}()

	go func() { // deal with incoming blocks
		for{
			theBlocks := <-BSB
			// if theBlocks size is 1. then it is the new block broad cast by other replica
			if len(theBlocks.Blocks) == 1{
				// check if it is already in ConsensusBlock
				flag := true
				for _,v := range ConsensusBlocks.Blocks{
					if v.Signature.Authority() == theBlocks.Latest().Signature.Authority(){
						flag = false;
					}
				}
				if flag {
					ConsensusBlocks.Append(theBlocks.Latest())
				}
				//help to broadcast to
				ReadytoSendchan <- theBlocks
			}else {
				// means it is the blockchain comming
				if len(theBlocks.Blocks) < len(replica.blockchains.LongestBlockchain().Blocks) {
					// broad cast the local blockchain again
					ReadytoSendchan <- replica.blockchains.LongestBlockchain()
				}else {
					// check difference and update
				}
			}


		}
	}()
	go func() { // make consensus each step duration
		for{
			time.Sleep(time.Duration(replica.stepDuration) * time.Second)
			// check consensus blocks
		}
	}()



	for {
		// check whether end
		select {
		case <-done:
			return // means everything is done
		case ReadyBS := <-ReadytoSendchan: // this will block the for loop when no Ready BS comming in
			// update the ready BS
			// if BS is 1 length, then broadcast it
			if len(ReadyBS.Blocks) > 1 {

			}

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
