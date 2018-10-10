module utils {


    proc encodeBase64(src: []uint(8)): string {
        /*
         * This is a tranlation of C code from hwlock provided at
         * https://github.com/chapel-lang/chapel/blob/master/third-party/hwloc/hwloc-src/src/base64.c
         * into Chapel. Translated by Loong https://github.com/loongy.
         */
        /*
         * Copyright (c) 1996 by Internet Software Consortium.
         *
         * Permission to use, copy, modify, and distribute this software for any
         * purpose with or without fee is hereby granted, provided that the above
         * copyright notice and this permission notice appear in all copies.
         *
         * THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE CONSORTIUM DISCLAIMS
         * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
         * OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET SOFTWARE
         * CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
         * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
         * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
         * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
         * SOFTWARE.
         */
        
        const base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        const pad64 = "=";

        var dst = "";
        var input: [0..2]uint(8);
        var output: [0..3]uint(8);

        var ptr: int = 0;
        var len: int = 32;

        // Encoding
        while (2 < len) {
            input[0] = src[ptr]; ptr+=1;
            input[1] = src[ptr]; ptr+=1;
            input[2] = src[ptr]; ptr+=1;
            len -= 3;

            output[0] = input[0] >> 2;
            output[1] = ((input[0] & 0x03) << 4) + (input[1] >> 4);
            output[2] = ((input[1] & 0x0f) << 2) + (input[2] >> 6);
            output[3] = input[2] & 0x3f;

            dst += base64[output[0] + 1];
            dst += base64[output[1] + 1];
            dst += base64[output[2] + 1];
            dst += base64[output[3] + 1];
        }

        // Padding
        if (len != 0) {
            input[0] = 0;
            input[1] = 0;
            input[2] = 0;
            for i in 0 .. len-1 {
                input[i] = src[ptr]; ptr+=1;
            }

            output[0] = input[0] >> 2;
            output[1] = ((input[0] & 0x03) << 4) + (input[1] >> 4);
            output[2] = ((input[1] & 0x0f) << 2) + (input[2] >> 6);

            dst += base64[output[0] + 1];
            dst  += base64[output[1] + 1];
            if (len == 1) {
                dst += pad64;
            } else {
                dst += base64[output[2] + 1];
            }
            dst += pad64;
        }

        return dst;
    }
}