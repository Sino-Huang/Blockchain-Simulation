package main

// An Authority is an identity that is authorised by the network to generate
// blocks.
type Authority string

// Authorities are the replicas that will run on the local machine and are
// authorised to generate Blocksq.
var Authorities = []string{
	"lamport@localhost:4000",
	"dijkstra@localhost:4001",
	"knuth@localhost:4002",
	"linus@localhost:4003",
	"turing@localhost:4004",
	"neumann@localhost:4005",
	"babbage@localhost:4006",
	"satoshi@localhost:4007",
	"zimmer@localhost:4008",
}
