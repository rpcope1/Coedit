module common;

alias AstHandle = ptrdiff_t;

alias AstNotification = extern(C) void function(void* param);

__gshared immutable AstHandle invalidAstHandle = 0;

enum SerializationFormat : byte
{
    json,
    pascal
}

enum logcall = q{
    import std.file;
    append("cedast_log.txt", cast(ubyte[])(__PRETTY_FUNCTION__ ~ "\r\n"));
};
