module utils;


private static const hexDigits = "0123456789ABCDEF";

/**
 * Returns the hexadecimal representation of a ubyte.
 * "to!string(int, radix)" is not used because it strips the leading 0 off.
 */
public char[2] ubyte2Hex(ubyte aUbyte)
{
    char[2] result;
    result[1] = hexDigits[(aUbyte & 0x0F)];
    result[0] = hexDigits[(aUbyte & 0xF0) >> 4];
    return result;
}

/**
 * Converts a CRC32 to a BigEndian uint.
 * It grants a resource module to be cross-plateform.
 */
public uint crc322uint(in ubyte[4] aCrc32)
{
    uint result;
    ubyte* ptr = cast(ubyte*) &result;
    version(BigEndian)
        foreach(i; 0..4) * (ptr + i) = aCrc32[i];
    else
        foreach(i; 0..4) * (ptr + i) = aCrc32[3-i];
    return result;
}

/**
 * splits the encoded resource representation in multiple lines.
 * Problem: not UTF-8 aware (does not always split on a code point)
 */
string splitConstString(char[] input, size_t columns = 80)
{
    size_t lo, hi;
    string result;
    auto lines = input.length / columns;
    foreach(i; 0 .. lines){
        lo = i * columns;
        hi = lo + columns;
        result ~= "\t\"" ~ input[lo .. hi] ~ "\" ~\r\n";
    }
    result ~= "\t" ~ "\"" ~ input[hi .. $-1] ~ "\"";
    return result;
}

/**
 * Adds a log message if verbose.
 */
void writeMessage(bool verbose, lazy string aMessage){
    import std.stdio : writefln;
    if (!verbose) return;
    writefln("%s", aMessage);
}

/**
 * Self-centered init.
 */
void reset(T)(ref T aValue){
    aValue = aValue.init;
}
