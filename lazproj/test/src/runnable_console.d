module console;

/*
 The most simple console wrappr'.

 usage:
 ------
 - Use Coedit menu, Compile and Run file to execute.
 - use the "Process input widget to pass" commands.
 - Don't forget that the Messages widget restricts the item count.
 - type "exit" and press SEND to quit.
*/
void main(string args[])
{
    import std.process;
    //
    version(Windows)
        string prgname = "cmd";
    /*else
        version(linux) string prgname = "xterm";*/
    else
        assert(0, "unsupported target");
    //
    prgname.spawnProcess.wait;
}
