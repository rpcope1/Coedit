module e7F;

private const t = true;
private const f = false;

/// Returns true if a white has to be encoded.
private bool[14] isWhite7F = [
    t, t, t, f, f, f, t, f, f, t, t, f, f, t
];

/**
 * "7F" binary-to-text encoder.
 * Kind of "percent encoding" except that the prefix is the character 0x7F.
 * non-ASCII chars and "whites" are prefixed and represented in base 16.
 */
char[] encode_7F(ubyte[] input)
{
    import utils : ubyte2Hex;
    char[] result;
    foreach(b; input){
        //if ((b < 14 && isWhite7F[b]) || b > 0x7E)
        if (b < 32 || b > 0x7E || b == '"' || b == '\\')
            result ~= (cast(char) 0x7F) ~ ubyte2Hex(b);
        else
            result ~= cast(char)b;
    }
    return result;
}

/**
 * "7F" text-to-binary decoder.
 */
ubyte[] decode_7F(in char[] input)
{
    ubyte[] result;
    size_t i;
    while(i < input.length)
    {
        char c = input[i];
        assert(c <= 0x7F);
        if (c == 0x7F)
        { 
            assert(i+2 < input.length);
            char[2] digits = input[i+1 .. i+3];
            import std.conv : to;
            result ~= to!ubyte(digits[], 16);    
            i += 2;
        } 
        else result ~= c;
        ++i;
    }    
    return result;
}
