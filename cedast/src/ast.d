module ast;

import std.d.lexer, std.d.parser, std.d.ast;
import common;
import iz.enumset;

private
{
    enum AstInfos {moduleName, symbolList, todoList, WarnAndErr}
    alias CachedInfos = EnumSet!(AstInfos, Set8);
}


struct Ast
{

private:

    ubyte[] src;
    string fname;
    LexerConfig config;
    StringCache strcache;
    Module mod;
    CachedInfos cachedInfos;
    bool scanned;
    string modName;

    final static void parserError(string fname, size_t line, size_t col, string msg, bool isErr)
    {
    }

    final void resetCachedInfo()
    {
        cachedInfos = 0;
        modName = "";
    }

    final void scan()
    {
        resetCachedInfo;
        scanned = false;
        scope(success) scanned = true;

        config = LexerConfig(fname, StringBehavior.source, WhitespaceBehavior.skip);
        mod = parseModule(getTokensForParser(src, config, &strcache), fname, null, &parserError);
    }

public:

    this(string filename)
    {
        mixin(logcall);

        import std.file;
        fname = filename;
        src = cast(ubyte[]) read(fname, size_t.max);
        strcache = StringCache(StringCache.defaultBucketCount);
        scan;
    }

    this(ubyte[] buffer)
    {
        mixin(logcall);
        src = buffer.dup;
        strcache = StringCache(StringCache.defaultBucketCount);
        scan;
    }

    final void rescan()
    {
        mixin(logcall);
        resetCachedInfo;
    }

    final string moduleName()
    {
        string result;

        if (!scanned)
            return result;
        if (AstInfos.moduleName in cachedInfos)
            return modName;

        cachedInfos += AstInfos.moduleName;
        if (mod.moduleDeclaration)
        foreach(Token t; mod.moduleDeclaration.moduleName.identifiers)
            result ~= t.text ~ ".";

        if (result.length)
            modName = result[0 .. $-1];
        return modName;
    }
}
