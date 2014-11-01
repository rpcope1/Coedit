module expandSymString;

import std.stdio;
import std.getopt;

/*
pass:

--a=<CPF> --b=<CPP> --c=<CPR> --d=<CFF> --e=<CFP> --f=<CI> --g=<CAF> --h=<CAP> --j=<CPN> --k=<CPFS>

as parameters in "Run, Compile and Run file..."
<CPFS>: only the first item is displayed but the symbol is expanded as expected.
*/
void main(string args[])
{

    string expanded;
    for (char c = 'a'; c != 'z'; c++)
    {
        expanded = "";
        getopt(args, std.getopt.config.passThrough, c, &expanded);
        if (expanded.length != 0)
            writeln(c, " : ", expanded);
    }
}
