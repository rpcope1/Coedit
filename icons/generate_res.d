// generate the Pascal code for using Coedit icon data at run-time.
// usage: open in Coedit then click File, compile and run file.
module generate_res;

import std.file;
import std.path;
import std.string;
import std.process;

version(linux) string pathto = "/usr/lib64/lazarus/tools/";
else string pathto = "";

void main(string[] args)
{
    auto path = args[0].dirName;
    string[] cmd;
    // resource compiler
    cmd ~= pathto ~ "lazres";
    // pascal source to include
    cmd ~= ".." ~ dirSeparator ~ "src" ~ dirSeparator ~ "ce_icons.inc";
    // resource items
    foreach(f; dirEntries(path, "*.png", SpanMode.depth)) {
        cmd ~= format("%s=%s",f.name, f.name.baseName.stripExtension);   
    }
    spawnProcess(cmd).wait;
}
