module block {
    
    class Block {
        // A signature can be used to verify the Authority that produced the Block.
        // To be valid, a Block must have a Signature that was produced by a valid
        // Authority.
        var signature: string;

        // The header is dependent on the contents of the Block, and the Signature
        // of the Block is produced using the Header. If the contents of the Block
        // is changed, the Header must be also be changed. Since the Signature
        // cannot be forged, nobody is able to mutate the contents of the Block
        // without detection.
        var header: string;

        // The parentHeader is the Header of the previous Block in the blockchain.
        var parentHeader: string;

        // The height of the Block. To be valid, the height of a Block must be
        // exactly one greater than the height of the parent Block.
        var height: int;

        // The timestamp at which the Block was generated. It is impossible to
        // verify the correctness of the timestamp, since an Authority can be
        // dishonest about what time the Block was generated. Instead, to be valid,
        // the timestamp of a Block must be later in time than the timestamp of its
        // parent Block and must be in past.
        var timestamp: int;
    }

    // authorityFromSignature returns the authority that has signed the Block.
    // If the Block was not correctly signed, then this function may throw.
    proc Block.authorityFromSignature(): string {
        const it = this.signature.split(":");
        return it[1];
    }

    const genesisBlock = new unmanaged Block(
        signature    = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
        header       = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
        parentHeader = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
        height       = 0,
        timestamp    = 0
    );

    proc randomBlockHeader(): string {
        use Random;
        use utils;

        var header: [0 .. 31]uint(8);
        fillRandom(header);

        return encodeBase64(header[0.. 31]);
    }

    proc currentBlockTimestamp(): int {
        use DateTime;
        return (datetime.now() - unixEpoch).total_seconds() : int;
    }

    class Blockchains {
        var blocksDomain: domain(string);
        var blocks: [blocksDomain] Block;
        var endOfLongestBlockchain: Block;
    }

    proc Blockchains.longestBlockchain(): []Block {
        var nth = this.endOfLongestBlockchain.height;
        if nth < 1 {
            nth = 1;
        }

        var b = this.endOfLongestBlockchain;
        var blockchain: [0 .. nth-1]Block;
        for i in 0 .. nth-1 {
            blockchain[nth-1-i] = b;
            b = this.blocks[b.parentHeader];
        }

        return blockchain;
    }

    // A BlockQuery is sent to a Replica to query its view on the longest
    // blockchain.
    class BlockQuery {

        // The height of the first block that is expected in the response.
        var start: int;

        // The height of the last block that is expected in the response.
        var end: int;

        // A synchronous variable that can be used to write a response to the 
        // sender of the query.
        var responder$: sync BlockQueryResponse;
    }

    class BlockQueryResponse {
        var blockDomain: domain(int);
        var blocks: [blockDomain]Block;
    }
}