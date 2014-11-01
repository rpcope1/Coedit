module googlesearch;

/*
Coedit custom tool:
search some help about the current identifier.
executable: <CAP>\googlesearch.exe
parameters: <CI>
*/

static const string prefix = "http://www.google.com/search?q=\"dlang.org+";
static const string suffix = "&btnI=Im+Feeling+Lucky";

void main(string args[])
{
    if (args.length == 1)
        return;

    auto url = prefix ~ args[1]~ suffix;
	import std.process: browse;
    browse(url);
}
