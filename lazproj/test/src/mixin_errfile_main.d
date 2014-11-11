module runnable;

import std.stdio;

void main(string args[])
{
    // purpose: test if error message in mixed content could be clicked
    // => no, the filename from where the data come is not passed
    mixin(import("mixin_errfile_data.txt"));
}
