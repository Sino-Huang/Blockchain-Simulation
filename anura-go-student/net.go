package main

import (
	"bytes"
	"encoding/json"
	"log"
	"math/rand"
	"net/http"
	"strings"
	"time"
)

// A Query is request for a range of Blocks. It is sent by a Replica to another
// remote Replica to synchronise Blocks. This allows Replicas to pull Blocks
// from each other to recover from network partitions.
type Query struct {

	// Begin is an offset from the genesis Block. The Block at this index should
	// be the first Block returned in response to this Query.
	Begin int `json:"begin"`

	// End is an offset from the genesis Block. The Block at this index should
	// be the last Block returned in response to the Query.
	End int `json:"end"`

	// Responder is a synchronous write-only channel used to send the response
	// of the Query.
	Responder chan Blocks `json:"-"`
}

func Serve(authority Authority, addr string) (<-chan Blocks, <-chan Query) {

	blockReceiver := make(chan Blocks)
	queryReceiver := make(chan Query)

	// Handle the posting of blocks
	b := func(w http.ResponseWriter, r *http.Request) {
		switch r.Method {
		case http.MethodPost:
		default:
			return
		}

		// Decode blocks and send them to the receiver
		blocks := Blocks{}
		if err := json.NewDecoder(r.Body).Decode(&blocks); err != nil {
			w.Write([]byte(err.Error()))
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		if configAttack && len(blocks.Blocks) > 0 {
			x := rand.Intn(10)
			if x < 4 {
				b := blocks.Blocks[len(blocks.Blocks)-1]
				switch x {
				// Change signature
				case 0:
					log.Printf("changed signature")
					b.Header = Header("HACKED!!")
					authority := strings.Split(string(b.Signature), ":")
					if len(authority) == 2 {
						newAuthority := strings.Split(Authorities[rand.Intn(len(Authorities))], "@")[0]
						for newAuthority == authority[1] {
							newAuthority = strings.Split(Authorities[rand.Intn(len(Authorities))], "@")[0]
						}
						b.Signature = Signature("HACKED!!:" + newAuthority)
					}
				// Change parent header
				case 1:
					log.Printf("changed parent header")
					b.Header = Header("HACKED!!")
					b.ParentHeader = Header("bad header")
				// Change timestamp
				case 2:
					log.Printf("changed timestamp")
					b.Header = Header("HACKED!!")
					b.Timestamp = time.Now().Unix() + 1000000
				// Turn it into the genesis block
				case 3:
					b = GenesisBlock()
				}
				blocks.Blocks[len(blocks.Blocks)-1] = b
				blockReceiver <- blocks
				return
			}
		}

		blockReceiver <- blocks
	}

	// Handle the posting of requests
	q := func(w http.ResponseWriter, r *http.Request) {
		query := Query{}

		switch r.Method {
		case http.MethodGet:
			query.Begin = 0
			query.End = 128
		case http.MethodPost:
			if err := json.NewDecoder(r.Body).Decode(&query); err != nil {
				w.Write([]byte(err.Error()))
				w.WriteHeader(http.StatusBadRequest)
				return
			}
		default:
			return
		}

		// Add responder to the query and send the query to the receiver
		responder := make(chan Blocks)
		query.Responder = responder
		queryReceiver <- query

		// Read response and write back
		blocks := <-responder
		if err := json.NewEncoder(w).Encode(blocks); err != nil {
			w.Write([]byte(err.Error()))
			w.WriteHeader(http.StatusBadRequest)
			return
		}
	}

	server := http.NewServeMux()
	server.HandleFunc("/b", b)
	server.HandleFunc("/q", q)
	go func() {
		if err := http.ListenAndServe(addr, server); err != nil {
			log.Fatal(err)
		}
	}()

	return blockReceiver, queryReceiver
}

func connect(done <-chan struct{}, addr string, connectivity float64) chan Blocks {

	conn := make(chan Blocks)

	if rand.Intn(10000) > int(10000.0*connectivity) {
		// Drop the connection randomly
		return conn
	}

	go func() {
		for {
			time.Sleep(time.Duration(configStep*int64(rand.Intn(1000))) * time.Millisecond)

			select {
			case <-done:
				return
			case blocks := <-conn:
				postBlocks(addr, blocks)
			}
		}
	}()

	return conn
}

func postBlocks(addr string, blocks Blocks) {
	data, err := json.Marshal(blocks)
	if err != nil {
		log.Fatalf("cannot marshal blocks = %v", err)
		return
	}

	res, err := http.Post("http://"+addr+"/b", "application/json", bytes.NewBuffer(data))
	if err != nil {
		log.Printf("cannot post blocks = %v", err)
		return
	}
	defer res.Body.Close()
}

func postQuery(addr string, query Query) {
	data, err := json.Marshal(query)
	if err != nil {
		log.Fatalf("cannot marshal query = %v", err)
		return
	}

	res, err := http.Post("http://"+addr+"/q", "application/json", bytes.NewBuffer(data))
	if err != nil {
		log.Printf("cannot post query = %v", err)
		return
	}
	defer res.Body.Close()

	blocks := NewBlocks()
	if err := json.NewDecoder(res.Body).Decode(&blocks); err != nil {
		log.Fatalf("cannot unmarshal query response = %v", err)
		return
	}

	query.Responder <- blocks
}
