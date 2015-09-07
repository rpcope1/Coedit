#!runnable-flags: -version=a -version=b
module runnable;

/* 

compile fine and run, feature #36: 
- see specs at https://github.com/BBasile/Coedit/issues/36
- dmd switches can be passed in the script line
- dups are automatically eliminated.

*/

import std.stdio;

void main(string[] args)
{
    version(a) "a".writeln;
    version(b) "b".writeln;
}
