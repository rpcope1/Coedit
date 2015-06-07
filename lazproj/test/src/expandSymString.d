module expandSymString;

import std.stdio;
import std.getopt;

/*
pass:

"--a=<CAF>" "--b=<CAP>" "--c=<CFF>" "--d=<CFP>" "--e=<CI>" "--f=<CPF>" "--g=<CPP>" "--h=<CPR>" "--i=<CPN>" "--j=<CPO>" "--k=<CPFS>" "--l=<CPCD>"

as parameters in "Run, Compile and Run file..."
*/

void main(string[] args)
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
    'j' : "CurrentProjectOutput...: ",
    'k' : "CurrentProjectFiles....: ",
    'l' : "CurrentProjectCommonSourceDirectory....: "
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
