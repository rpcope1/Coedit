module common;

alias AstToken = ptrdiff_t;

__gshared immutable AstToken invalidAstToken = 0;

enum SerializationFormat {json, pascal}

enum logcall = q{
    import std.file;
    append("cedast_log.txt", cast(ubyte[])(__PRETTY_FUNCTION__ ~ "\r\n"));
};
