package main

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"strings"
)

// A Signature is an assymetric cryptographic signature produced by an Authority
// for the Header of a Block. It is assumed to be unique with respect to the
// Header, and unforgeable by other Authorities. Using the Signature, it is
// possible to verify that a Header has been produced by an Authority. For
// simplicity, this application does not use a real cryptographic signature.
// Instead, a Signature is of the form "header:authority". (In real world
// applications, an ECDSA secp256k1 elliptic curve is commonly used.)
type Signature string

// Authority that produced the Signature. This function will panic if the
// Signature is malformed.
func (sig Signature) Authority() Authority {
	components := strings.Split(string(sig), ":")
	if len(components) != 2 {
		panic("malformed signature")
	}
	return Authority(components[1])
}

// A Header is a cryptographic hash produced based on the contents of a Block.
// It is assumed to be unique with respect to the contents of the Block. For
// simplicty, this application does not use a real cryptographic hash function.
// Instead, a Header is the base64 encoding of 32 random bytes.  (In real world
// applications, a hashing function from the SHA familty is commonly used.)
type Header string

// A Block is an atomic unit within a blockchain.
type Block struct {

	// A Signature can be used to verify the Authority that produced the Block.
	// To be valid, a Block must have a Signature that was produced by a valid
	// Authority.
	Signature Signature `json:"signature"`

	// The Header is dependent on the contents of the Block, and the Signature
	// of the Block is produced using the Header. If the contents of the Block
	// is changed, the Header must be also be changed. Since the Signature
	// cannot be forged, nobody is able to mutate the contents of the Block
	// without detection.
	Header Header `json:"header"`

	// The ParentHeader is the Header of the previous Block in the blockchain.
	ParentHeader Header `json:"parentHeader"`

	// The Height of the Block. To be valid, the height of a Block must be
	// exactly one greater than the height of the parent Block.
	Height int64 `json:"height"`

	// The Timestamp at which the Block was generated. It is impossible to
	// verify the correctness of the timestamp, since an Authority can be
	// dishonest about what time the Block was generated. Instead, to be valid,
	// the timestamp of a Block must be later in time than the timestamp of its
	// parent Block and must be in past.
	Timestamp int64 `json:"timestamp"`
}

// NewBlock returns a new Block. A Header for the Block is automatically
// generated, but the Signature is blank until the Block is signed.
func NewBlock(parentHeader Header, height, timestamp int64) Block {
	return Block{
		Header:       RandomBlockHeader(),
		ParentHeader: parentHeader,
		Height:       height,
		Timestamp:    timestamp,
	}
}

// GenesisBlock returns the genesis Block. To be valid, a blockchain must have
// the genesis Block as its first Block. The genesis Block points to itself as
// its parent, and has a height of zero.
func GenesisBlock() Block {
	return Block{
		Header:       GenesisBlockHeader(),
		ParentHeader: GenesisBlockHeader(),
		Height:       0,
		Timestamp:    0,
	}
}

// RandomBlockHeader returns 32 random bytes encoded in base64. A Block Header
// is usually the output of a cryptographic hash over the contents of the Block,
// but for this application we do not want to focus on low-level cryptography.
func RandomBlockHeader() Header {
	header := [32]byte{}
	_, err := rand.Read(header[:])
	if err != nil {
		panic(fmt.Sprintf("cannot generate random header = %v", err))
	}
	return Header(base64.StdEncoding.EncodeToString(header[:]))
}

// GenesisBlockHeader returns the Header of the genesis Block. The Header of the
// genesis Block is 32 zero bytes encoded in bas64.
func GenesisBlockHeader() Header {
	header := [32]byte{}
	return Header(base64.StdEncoding.EncodeToString(header[:]))
}

// Blocks is a list of connected Blocks, where the Block at `i` points to the
// parent Block at index `i-1`. A complete blockchain is a list of Blocks
// beginning with the genesis Block at index `0`.
type Blocks struct {
	Blocks []Block `json:"blocks"`
}

func NewBlocks() Blocks {
	return Blocks{
		Blocks: []Block{},
	}
}

// Append returns Blocks with the appended Block at the end of the list.
func (blocks Blocks) Append(block Block) Blocks {
	return Blocks{Blocks: append(blocks.Blocks, block)}
}

// Prepend returns Blocks with the prepended Block at the beginning of the list.
func (blocks Blocks) Prepend(block Block) Blocks {
	return Blocks{Blocks: append([]Block{block}, blocks.Blocks...)}
}

// Latest returns the most recently generated Block from the list.
func (blocks Blocks) Latest() Block {
	return blocks.Blocks[len(blocks.Blocks)-1]
}

// LatestN returns the most recently generated Blocks from the list.
func (blocks Blocks) LatestN(n int) Blocks {
	if n >= len(blocks.Blocks) {
		return Blocks{blocks.Blocks}
	}
	return Blocks{blocks.Blocks[len(blocks.Blocks)-n-1:]}
}

func (blocks Blocks) Range(begin, end int) Blocks {
	if begin < 0 {
		begin = 0
	}
	if end > len(blocks.Blocks) {
		end = len(blocks.Blocks)
	}
	return Blocks{Blocks: blocks.Blocks[begin:end]}
}

// ForEach will iterate over each Block and run the function. It will stop the
// iterating early if the function returns false.
func (blocks Blocks) ForEach(f func(block Block) bool) {
	for _, block := range blocks.Blocks {
		if !f(block) {
			return
		}
	}
}

func (blocks Blocks) Reverse() Blocks {
	reversed := Blocks{Blocks: make([]Block, len(blocks.Blocks))}
	for i := range blocks.Blocks {
		reversed.Blocks[len(blocks.Blocks)-i-1] = blocks.Blocks[i]
	}
	return reversed
}

// Blockchains is a store for the longest known blockchain and all Blocks that
// have ever been seen.
type Blockchains struct {
	endOfLongestBlockchain Block
	blocks                 map[Header]Block
}

// NewBlockchains returns an empty Blockchains store.
func NewBlockchains() Blockchains {
	blockchains := Blockchains{
		endOfLongestBlockchain: GenesisBlock(),
		blocks:                 map[Header]Block{},
	}
	blockchains.blocks[blockchains.endOfLongestBlockchain.Header] = blockchains.endOfLongestBlockchain
	return blockchains
}

func (blockchains *Blockchains) LongestBlockchain() Blocks {
	blocks := NewBlocks()
	blockchains.WalkToGenesisBlock(blockchains.EndOfLongestBlockchain(), func(block Block) {
		blocks = blocks.Prepend(block)
	})
	return blocks
}

// EndOfLongestBlockchain known to the store.
func (blockchains *Blockchains) EndOfLongestBlockchain() Block {
	return blockchains.endOfLongestBlockchain
}

// ReplaceEndOfLongestBlockchain known to the store. All Blocks from the new
// Block, tracing back to the genesis Block, must already be known to the store.
func (blockchains *Blockchains) ReplaceEndOfLongestBlockchain(newEndOfLongestBlockchain Block) {
	blockchains.InsertBlock(newEndOfLongestBlockchain)
	blockchains.endOfLongestBlockchain = newEndOfLongestBlockchain

	blockchains.WalkToGenesisBlock(blockchains.endOfLongestBlockchain, func(block Block) {
		if _, ok := blockchains.Block(block.Header); !ok {
			panic("unknown block header")
		}
	})
}

// Block returns the Block associated with the Header. If no such Block is
// known, the genesis Block is returned. A boolean is also returned that is
// false when no such Block is known, otherwise it is true.
func (blockchains *Blockchains) Block(header Header) (Block, bool) {
	block, ok := blockchains.blocks[header]
	return block, ok
}

func (blockchains *Blockchains) InsertBlock(block Block) {
	blockchains.blocks[block.Header] = block
}

func (blockchains *Blockchains) WalkToGenesisBlock(iter Block, f func(block Block)) {
	for {
		f(iter)
		parent, ok := blockchains.Block(iter.ParentHeader)
		if !ok {
			panic("unknown block header")
		}
		if iter.Header == parent.Header {
			break
		}
		iter = parent
	}
}
