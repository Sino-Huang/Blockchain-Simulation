package main

import (
	"flag"
	"fmt"
	"math/rand"
	"os"
	"os/signal"
	"sort"
	"strings"
	"syscall"
	"time"

	"github.com/republicprotocol/co-go"
)

var configAttack = false
var configStep = int64(3)
var configConnectivity = float64(1.0)

func main() {
	rand.Seed(time.Now().Unix())

	flag.BoolVar(&configAttack, "attack", configAttack, "attack replicas with bad blocks")
	flag.Int64Var(&configStep, "step", configStep, "step duration in seconds")
	flag.Float64Var(&configConnectivity, "connectivity", configConnectivity, "connectivity of the network")
	flag.Parse()

	done := make(chan struct{})
	defer close(done)

	// TODO: Support for remote authorities
	authorities := make([]Authority, len(Authorities))
	addrs := make([]string, len(Authorities))
	for i := range authorities {
		authorityAndAddr := Authorities[i]
		authorityAndAddrSplit := strings.Split(authorityAndAddr, "@")
		authority := authorityAndAddrSplit[0]
		addr := authorityAndAddrSplit[1]
		authorities[i] = Authority(authority)
		addrs[i] = addr
	}

	go func() {
		fmt.Println("========")
		for i := range authorities {
			fmt.Printf("%-8v http://%v/q\n", authorities[i], addrs[i])
		}
		fmt.Println("========")

		co.ParForAll(authorities, func(i int) {
			runLocalReplica(done, i, authorities, addrs)
		})
	}()

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-signals
		os.Exit(0)
	}()

	for {
		time.Sleep(time.Duration(2*configStep) * time.Second)
		printConsensus(addrs)
	}
}

func runLocalReplica(done <-chan struct{}, i int, authorities []Authority, addrs []string) {
	blockReceiver, queryReceiver := Serve(authorities[i], addrs[i])
	blockGenerator := runLocalReplicaGenerator(done, int64(i), int64(len(authorities)))
	connections := runLocalReplicaConnections(done, i, addrs)

	replica := NewReplica(authorities[i], authorities, blockReceiver, queryReceiver, blockGenerator, connections)
	replica.Run(done)
}

func runLocalReplicaGenerator(done <-chan struct{}, i int64, numAuthorities int64) <-chan time.Time {
	blockGenerator := make(chan time.Time)

	go func() {
		ticker := time.NewTicker(time.Second)
		defer ticker.Stop()

		step := int64(0)
		for {
			select {
			case <-done:
				return
			case <-ticker.C:
				nextStep := (time.Now().Unix() / configStep)
				if nextStep <= step {
					continue
				}
				step = nextStep

				if i == step%numAuthorities {
					blockGenerator <- time.Now()
				}
			}
		}
	}()

	return blockGenerator
}

func runLocalReplicaConnections(done <-chan struct{}, i int, addrs []string) []chan<- Blocks {
	conns := make([]chan<- Blocks, 0, int(configConnectivity*float64(len(addrs))))
	for _, addr := range addrs {
		if addr == addrs[i] {
			continue
		}
		conns = append(
			conns,
			connect(done, addr, configConnectivity),
		)
	}
	return conns
}

func printConsensus(addrs []string) {
	if len(addrs) == 0 {
		return
	}

	blockchains := make([]Blocks, len(addrs))
	co.ParForAll(addrs, func(i int) {
		query := Query{
			Begin:     0,
			End:       1000,
			Responder: make(chan Blocks, 1),
		}
		postQuery(addrs[i], query)
		blockchains[i] = <-query.Responder
	})

	commonBlockchain := []Block{}
	for i := 0; ; i++ {
		for _, blocks := range blockchains {
			if i >= len(blocks.Blocks) {
				goto Done
			}
		}
		block := blockchains[0].Blocks[i]
		for _, blocks := range blockchains {
			if blocks.Blocks[i].Header != block.Header {
				goto Done
			}
		}
		commonBlockchain = append(commonBlockchain, block)
	}
Done:

	longestBlockchain := []Block{}
	for _, blocks := range blockchains {
		if len(blocks.Blocks) > len(longestBlockchain) {
			longestBlockchain = blocks.Blocks
		}
	}

	lastCommonBlockIndex := len(commonBlockchain) - 2
	if lastCommonBlockIndex < 0 {
		lastCommonBlockIndex = 0
	}

	numberOfChains := map[string]int{}
	for _, blocks := range blockchains {

		headers := []string{}
		for i := lastCommonBlockIndex; i < len(blocks.Blocks); i++ {
			if i < len(blocks.Blocks) {
				runes := []rune(string(blocks.Blocks[i].Header))
				headers = append(headers, string(runes[:8])+"...")
			}
		}
		header := strings.Join(headers, " -> ")

		num := numberOfChains[header]
		numberOfChains[header] = num + 1
	}

	numberOfChainsKeys := []string{}
	for key := range numberOfChains {
		numberOfChainsKeys = append(numberOfChainsKeys, key)
	}
	sort.Slice(numberOfChainsKeys, func(i, j int) bool {
		return numberOfChainsKeys[i] < numberOfChainsKeys[j]
	})

	blockchainsDesc := ""
	for _, header := range numberOfChainsKeys {
		blockchainsDesc += fmt.Sprintf("%v => %v\n", header, numberOfChains[header])
	}

	longestBlockchainHeight := int64(0)
	commonBlockchainHeight := int64(0)
	if len(longestBlockchain) > 0 {
		longestBlockchainHeight = longestBlockchain[len(longestBlockchain)-1].Height
	}
	if len(commonBlockchain) > 0 {
		commonBlockchainHeight = commonBlockchain[len(commonBlockchain)-1].Height
	}

	fmt.Printf("\n========\nLongest blockchain height: %v\nCommon blockchain height: %v\n--------\n%v========\n", longestBlockchainHeight, commonBlockchainHeight, blockchainsDesc)
}
