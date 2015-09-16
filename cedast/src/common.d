module common;

extern(C):

alias AstHandle = ptrdiff_t;

alias AstNotification = void function(void* param);

__gshared immutable AstHandle invalidAstHandle = 0;

enum SerializationFormat : byte
{
    json,
    pascal
}
