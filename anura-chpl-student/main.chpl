use block;
use replica;

config const attack = false;
config const step = 3;
config const connectivity = 1.0;

proc main() {
    writeconfig();

    var replicas: [authorities.domain] Replica;
    for i in authorities.domain {
        replicas[i] = new unmanaged Replica(
            id          = authorities[i],
            blockchains = new Blockchains(
                endOfLongestBlockchain = genesisBlock
            )
        );
        replicas[i].blockchains.blocks[genesisBlock.header] = genesisBlock;
    }

    for i in replicas.domain {
        runReplicaConnections(i, replicas);
    }

    cobegin {
        runConsensusPrinter(replicas);
        coforall i in replicas.domain {
            cobegin {
                runReplicaGenerator(i, replicas);
                runReplicaConnections(i, replicas);
                replicas[i].run();
            }
        }
    }
}

proc writeconfig() {
    writeln("========");
    writeln("step         = ", step);
    writeln("connectivity = ", connectivity);
    writeln("--------");
    for i in authorities.domain.sorted() {
        writeln("authority ", i, " => ", authorities[i]);
    }
    writeln("========");
}

proc runReplicaGenerator(i: int, replicas: []Replica) {
    use DateTime;
    use Time;

    var currStep: int = 0;

    while true {
        sleep(1);

        var now: int = (datetime.now() - unixEpoch).total_seconds() : int;
        var nextStep: int = now / step;
        if nextStep <= currStep {
            continue;
        }
        currStep = nextStep;

        if i == currStep % authorities.size {
            replicas[i].blockGenerator$ = now;
        }
    }
}

proc runReplicaConnections(i: int, replicas: []Replica) {
    use Random;

    for j in authorities.domain {

        var rand: [1 .. 1]uint;
        fillRandom(rand);
        if rand[1] % 10000 < ((10000 * connectivity) : int) {
            if replicas[i].id != replicas[j].id {
                begin runReplicaConnection(replicas[i].conns$[replicas[j].id], replicas[j].blockReceiver$);
            }
        }
    }
}

proc runReplicaConnection(input$, output$: sync Block) {
    use Random;

    while true {

        const b = input$;
        output$ = b;

        var rand: [1 .. 1]uint;
        fillRandom(rand);

        if attack && (rand[1]%10 < 4) {
            select rand[1]%10 {
                when 0 {
                    const a = b.authorityFromSignature();
                    while a == authorities[(rand[1]%(authorities.size : uint)) : int] {
                        fillRandom(rand);
                    }
					output$ = new unmanaged Block(
                        signature    = b.signature,
                        header       = "HACKED!!" + authorities[(rand[1]%(authorities.size : uint)) : int],
                        parentHeader = "bad header",
                        height       = b.height,
                        timestamp    = b.timestamp
                    );
                }
                when 1 {
                    output$ = new unmanaged Block(
                        signature    = b.signature,
                        header       = "HACKED!!",
                        parentHeader = "bad header",
                        height       = b.height,
                        timestamp    = b.timestamp
                    );
                }
                when 2 {
                    output$ = new unmanaged Block(
                        signature    = b.signature,
                        header       = "HACKED!!",
                        parentHeader = "bad header",
                        height       = b.height,
                        timestamp    = b.timestamp + 1000000
                    );
                }
                when 3 {
                    output$ = genesisBlock;
                }
            }
        }
    }
}

proc runConsensusPrinter(replicas: []Replica) {
    use Time;

    while true {
        sleep(step);

        var longestBlockchainHeight = 0;
        for replica in replicas {
            if longestBlockchainHeight < replica.blockchains.endOfLongestBlockchain.height {
                longestBlockchainHeight = replica.blockchains.endOfLongestBlockchain.height;
                break;
            }
        }

        var commonBlockchainHeight = 0;
        var commonBlockchainIndex = 0;
        var stop = false;
        while true {
            for replica in replicas {
                if commonBlockchainIndex >= replica.blockchains.longestBlockchain().size {
                    stop = true;
                    break;
                }
            }
            if stop {
                break;
            }

            var blockHeight = 0;
            var blockHeader = "";
            for replica in replicas {
                if blockHeader == "" {
                    blockHeight = replica.blockchains.longestBlockchain()[commonBlockchainIndex].height;
                    blockHeader = replica.blockchains.longestBlockchain()[commonBlockchainIndex].header;
                    continue;
                }
                if blockHeader != replica.blockchains.longestBlockchain()[commonBlockchainIndex].header {
                    stop = true;
                    break;
                }
            }
            if stop {
                break;
            }

            commonBlockchainHeight = blockHeight;
            commonBlockchainIndex += 1;
        }

        commonBlockchainIndex -= 1;
        if commonBlockchainIndex < 0 {
            commonBlockchainIndex = 0;
        }

        var tailsDomain: domain(string);
        var tails: [tailsDomain]int;
        for replica in replicas {
            var tail = "";
            for i in commonBlockchainIndex .. replica.blockchains.longestBlockchain().size-1 {
                tail += "-> " + replica.blockchains.longestBlockchain()[i].header[1..8] + " ";
            }
            if !tailsDomain.member(tail) {
                tails[tail] = 1;
            } else {
                tails[tail] = tails[tail] + 1;
            }
        }

        writeln("");
        writeln("========");
        writeln("Longest blockchain height: ", longestBlockchainHeight);
        writeln("Common blockchain height: ", commonBlockchainHeight);
        writeln("--------");
        for i in tails.domain.sorted() {
            writeln(i, "=> ", tails[i]);
        }
        writeln("========");
    }
}