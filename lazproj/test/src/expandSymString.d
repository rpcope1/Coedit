module expandSymString;

import std.stdio;
import std.getopt;

/*
pass:

--a=<CAF> --b=<CAP> --c=<CFF> --d=<CFP> --e=<CI> --f=<CFP> --g=<CPP> --h=<CPR> --i=<CPN> --j=<CPFS>

as parameters in "Run, Compile and Run file..."
<CPFS>: only the first item is displayed but the symbol is expanded as expected.
*/

void main(string args[])
{
    auto opt2symbol = [
    // coedit
    'a' : "CoeditApplicationFile..: ",
    'b' : "CoeditApplicationPath..: ",
    // file
    'c' : "currentFileFilename....: ",
    'd' : "CurrentFilePath........: ",
    'e' : "CurrentIdentifier......: ",
    // project
    'f' : "CurrentProjectFile.....: ",
    'g' : "CurrentProjectPath.....: ",
    'h' : "CurrentProjectRoot.....: ",
    'i' : "CurrentProjectName.....: ",
    'j' : "CurrentProjectFiles....: "
    ];

    string expanded;
    foreach (c; 'a'..'z')
    {
        expanded = "";
        getopt(args, std.getopt.config.passThrough, c, &expanded);
        if (expanded.length)
            writeln(opt2symbol[c], expanded);
    }
}
