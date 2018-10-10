package main

import (
	"time"
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
	// init sending server
	// init receving server
	// sync them first?
	for {
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
