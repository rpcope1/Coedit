module z85;

//  --------------------------------------------------------------------------
//  Z85 is Copyright (c) 2010-2013 iMatix Corporation and Contributors
//
//  Permission is hereby granted, free of charge, to any person obtaining a
//  copy of this software and associated documentation files (the "Software"),
//  to deal in the Software without restriction, including without limitation
//  the rights to use, copy, modify, merge, publish, distribute, sublicense,
//  and/or sell copies of the Software, and to permit persons to whom the
//  Software is furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
//  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
//  DEALINGS IN THE SOFTWARE.
//  --------------------------------------------------------------------------

import std.stdio, std.conv;

///  Maps base 256 to base 85
static string encoder =
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#";

/**
 * Encodes a byte array as a string whose charset is defined by encoder.
 *
 * Modified version of the reference implementation of Z85_encode.
 * The tailling value is integrated to the input data before encoding:
 * - the tail is added as an ubyte[tail_size]
 * - the tail size is indicated in the first slot of an additional ubyte[4].
 * Finally the encoded text contains up to 7 * 5 / 4 chars in more from the original z85 form.
 *
 * This modified version is destructive for the input data.
 */
char[] Z85_encode(ref ubyte[] input)
in
{
    assert(input.length);
}
body
{
    // appends the tail things
    ubyte[] tail;
    ubyte added = 4 - (input.length % 4);
    tail.length = added + 4;
    tail[added] = added;
    input ~= tail;
    assert(input.length % 4 == 0);

    // reference implementation
    size_t encoded_size = input.length * 5 / 4;
    char[] encoded;
    encoded.length = encoded_size;
    uint char_nbr;
    uint byte_nbr;
    uint value;
    while (byte_nbr < input.length)
    {
        value = value * 256 + input [byte_nbr++];
        if ((byte_nbr & 3) == 0)
        {
            uint divisor = 85 * 85 * 85 * 85;
            while (divisor)
            {
                encoded[char_nbr++] = encoder[value / divisor % 85];
                divisor /= 85;
            }
            value = 0;
        }
    }
    assert (char_nbr == encoded_size);
    return encoded;
}

///  Maps base 85 to base 256
static immutable ubyte[96] z85_decoder = [
    0x00, 0x44, 0x00, 0x54, 0x53, 0x52, 0x48, 0x00,
    0x4B, 0x4C, 0x46, 0x41, 0x00, 0x3F, 0x3E, 0x45,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x40, 0x00, 0x49, 0x42, 0x4A, 0x47,
    0x51, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A,
    0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32,
    0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A,
    0x3B, 0x3C, 0x3D, 0x4D, 0x00, 0x4E, 0x43, 0x00,
    0x00, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
    0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
    0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
    0x21, 0x22, 0x23, 0x4F, 0x00, 0x50, 0x00, 0x00
];

/**
 * Decodes a string as a byte array.
 *
 * Modified version of the reference implementation of Z85_decode.
 * It automatically handles the tail added to grant the 4/5 i/o ratio,
 * as described in Z85_endcode()
 */
ubyte[] Z85_decode(in char[] input)
in
{
    assert(input.length % 5 == 0);
}
body
{
    // reference implementation
    size_t decoded_size = input.length * 4 / 5;
    ubyte[] decoded;
    decoded.length = decoded_size;
    uint byte_nbr;
    uint char_nbr;
    uint value;
    while (char_nbr < input.length)
    {
        value = value * 85 + z85_decoder [cast(ubyte) input[char_nbr++] - 32];
        if (char_nbr % 5 == 0)
        {
            uint divisor = 256 * 256 * 256;
            while (divisor)
            {
                decoded[byte_nbr++] = value / divisor % 256;
                divisor /= 256;
            }
            value = 0;
        }
    }
    assert (byte_nbr == decoded_size);

    // removes the tail things.
    ubyte added = decoded[$-4];
    decoded = decoded[0..$- (4 + added)];

    return decoded;
}
