module runnable;

import std.stdio;

void main(string args[])
{
    writeln("use <<Process input>> widget to continue: just click 'Send'");
    stdout.flush;
    readln;
    writeln("type <kbd>ENTER</kbd> in the field");
    stdout.flush;
    readln;
    writeln("it was not possible when the process was declared as a local variable...");
}
