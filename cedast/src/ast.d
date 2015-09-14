module ast;

import std.d.lexer, std.d.parser, common;

struct Ast
{
    this(string filename)
    {
        mixin(logcall);
    }

    this(ubyte[] buffer)
    {
        mixin(logcall);
    }

    void rescan()
    {
        mixin(logcall);
    }
}
