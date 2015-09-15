module common;

alias AstHandle = ptrdiff_t;

alias AstNotification = extern(C) void function(void* param);

__gshared immutable AstHandle invalidAstHandle = 0;

enum SerializationFormat : byte
{
    json,
    pascal
}
