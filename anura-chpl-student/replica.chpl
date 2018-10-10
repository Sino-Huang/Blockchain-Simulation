module replica {

    use block;

    const authoritiesNum: int = 9;
    const authoritiesDomain: domain(int) = { 0 .. authoritiesNum-1 };
    const authorities: [authoritiesDomain]string = [
        0 => "lamport",
        1 => "dijkstra",
        2 => "knuth",
        3 => "linus",
        4 => "turing",
        5 => "neumann",
        6 => "babbage",
        7 => "satoshi",
        8 => "zimmer"
    ];

    class Replica {
        var id: string;
        
        // blockGenerator is a synchronous variable. It is written to whenever
        // the Replica should generate a block and propagate it to other
        // Replicas in the network. The value written to the variable is the
        // time (in seconds, since Unix epoch) that the block should have been 
        // generated.
        var blockGenerator$: sync int;

        // blockReceiver is used to receive Blocks that have been sent by other Replicas. Blocks received from this channel
        // are not necessarily to be valid.
        var blockReceiver$: sync Block;

        // blockQueryReceiver is used to receive BlockQueries that have been 
        // send by other Replicas. Each BlockQuery has a sync variable that can
        // be used to respond to the BlockQuery.
        var blockQueryReceiver$: sync BlockQuery;

        // conns is an array of synchronous variables used to send Blocks to
        // other Replicas in the network. Connections are not always healthy,
        // and writing to a synchronous variable for an unhealthy connection
        // will block indefinitely.
        var connsDomain: domain(string);
        var conns$: [connsDomain]sync Block;

        var stepDuration: int;
        var blockchains:  Blockchains;
    }

    proc Replica.sign(block: owned Block): owned Block {
        block.signature = block.header + ":" + this.id;
        return block;
    }

    // Run the Replica.
    proc Replica.run() {
        // TODO:
        // 1. Generate the next Block in the blockchain whenever the
        //    blockGenerator signals that it is time to do so.
        // 2. Respond to BlockQueries that received on the blockQueryReceiver.
        // 3. Send Blocks to other Replicas using the conns sync variables, and
        //    validate Blocks received by other Replicas.
        // 4. Drop the connectivity to 0.25 and see if consensus make sure that
        //    consensus can still be reached.
    }
}