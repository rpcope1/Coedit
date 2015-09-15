module cedast;

import core.runtime, common, ast;

__gshared Ast*[] modules;

extern(C) export
AstToken scanFile(char* filename)
{
    AstToken result;
    try
    {
        import std.string: fromStringz;
        import std.algorithm: countUntil;
        Ast* ast = new Ast(filename.fromStringz.idup);
        result = countUntil(modules, null);
        if (result == -1)
        {
            modules ~= ast;
            result = modules.length;
        }
        else
        {
            modules[result] = ast;
            ++result;
        }
    }
    catch(Exception e)
    {
        if (result != 0) unleash(result);
        result = invalidAstToken;
    }
    return result;
}

extern(C) export
AstToken scanBuffer(ubyte* buffer, size_t len)
{
    AstToken result;
    try
    {
        import std.algorithm: countUntil;
        Ast* ast = new Ast(buffer[0 .. len]);
        result = countUntil(modules, null);
        if (result == -1)
        {
            modules ~= ast;
            result = modules.length;
        }
        else
        {
            modules[result] = ast;
            ++result;
        }
    }
    catch(Exception e)
    {
        if (result != 0) unleash(result);
        result = invalidAstToken;
    }
    return result;
}

extern(C) export
void rescan(AstToken tok)
{
    if (tok < 1 || tok > modules.length)
        return;
    modules[tok - 1].rescan;
}

extern(C) export
void unleash(AstToken tok)
{
    mixin(logcall);
    if (tok < 1 || tok > modules.length)
        return;
    if (modules[tok - 1] == null)
        return;
    delete modules[tok - 1];
    modules[tok - 1] = null;
    if (tok == modules.length)
        modules.length -= 1;
}

extern(C) export
immutable(char*) moduleName(AstToken tok)
{
    if (tok < 1 || tok > modules.length)
        return null;
    Ast* mod = modules[tok - 1];
    if (mod == null)
        return null;
    import std.string: toStringz;
    return toStringz(mod.moduleName);
}

extern(C) export
ubyte* symbolList(AstToken tok, ref size_t len, SerializationFormat fmt)
{
    ubyte* result = null;
    return result;
}

extern(C) export
ubyte* todoList(AstToken tok, ref size_t len, SerializationFormat fmt)
{
    ubyte* result = null;
    return result;
}

version(Windows)
{
    import core.sys.windows.windows;
    import core.sys.windows.dll;

    __gshared HINSTANCE g_hInst;

    extern (Windows)
    BOOL DllMain(HINSTANCE hInstance, ULONG ulReason, LPVOID pvReserved)
    {
        switch (ulReason)
        {
            case DLL_PROCESS_ATTACH:
                Runtime.initialize;
                g_hInst = hInstance;
                dll_process_attach( hInstance, true );
                break;

            case DLL_PROCESS_DETACH:
                Runtime.terminate;
                dll_process_detach( hInstance, true );
                break;

            case DLL_THREAD_ATTACH:
                dll_thread_attach( true, true );
                break;

            case DLL_THREAD_DETACH:
                dll_thread_detach( true, true );
                break;

            default:
                assert(0);
        }

        return true;
    }
}


