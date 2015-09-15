module cedast;

import core.runtime, common, ast;
import iz.memory;

__gshared Ast[] modules;


extern(C) export
AstHandle newAst(void* param, AstNotification clbck)
{
    AstHandle result;
    try
    {
        import std.algorithm: countUntil;
        Ast ast = construct!Ast;
        ast.notification = clbck;
        ast.notificationParameter = param;
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
        if (result != 0) deleteAst(result);
        result = invalidAstHandle;
    }
    return result;
}

extern(C) export
void deleteAst(AstHandle hdl)
{
    if (hdl < 1 || hdl > modules.length)
        return;
    if (modules[hdl - 1] is null)
        return;

    destruct(modules[hdl - 1]);
    modules[hdl - 1] = null;
    if (hdl == modules.length)
        modules.length -= 1;
}

extern(C) export
void scanFile(AstHandle hdl, char* filename)
{
    if (hdl < 1 || hdl > modules.length)
        return;
    if (modules[hdl - 1] is null)
        return;

    import std.string: fromStringz;
    modules[hdl - 1].scanFile(fromStringz(filename).idup);
}

extern(C) export
void scanBuffer(AstHandle hdl, ubyte* buffer, size_t len)
{
    if (hdl < 1 || hdl > modules.length)
        return;
    if (modules[hdl - 1] is null)
        return;

    modules[hdl - 1].scanBuffer(buffer[0 .. len]);
}

extern(C) export
immutable(char*) moduleName(AstHandle hdl)
{
    if (hdl < 1 || hdl > modules.length)
        return null;
    if (modules[hdl - 1] is null)
        return null;

    import std.string: toStringz;
    return toStringz(modules[hdl - 1].moduleName);
}

extern(C) export
ubyte* symbolList(AstHandle hdl, ref size_t len, SerializationFormat fmt)
{
    if (hdl < 1 || hdl > modules.length)
        return null;
    if (modules[hdl - 1] is null)
        return null;

    ubyte[] result;
    if (fmt == SerializationFormat.json)
        result = modules[hdl - 1].symbolListJson;
    else
        result = modules[hdl - 1].symbolListPas;

    len = result.length;
    return result.ptr;
}

extern(C) export
ubyte* todoList(AstHandle hdl, ref size_t len, SerializationFormat fmt)
{
    if (hdl < 1 || hdl > modules.length)
        return null;
    if (modules[hdl - 1] is null)
        return null;

    ubyte[] result;
    if (fmt == SerializationFormat.json)
        result = modules[hdl - 1].todoListJson;
    else
        result = modules[hdl - 1].todoListPas;

    len = result.length;
    return result.ptr;
}

version(Windows)
{
    import core.sys.windows.windows;
    import core.sys.windows.dll;

    __gshared HINSTANCE g_hInst;

    extern (Windows)
    BOOL DllMain(HINSTANCE hInstance, ULONG ulReason, LPVOID pvReserved)
    {
        final switch (ulReason)
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
        }
        return true;
    }
}

