module resman;

import std.stdio, std.getopt, std.path;
import std.json, std.file, std.conv;

enum ResType {aFile, aFolder}
enum ResFormat {bytes, utf8, base16, base64}

struct ResourceItem{}
alias ResourceItems = ResourceItem * [];

void main(string[] args)
{
    string[] files;
    string basePath;
    bool verbose;
    ResourceItems items;
    
    getopt(args, config.passThrough, "v|verbose", &verbose);
    getopt(args, config.passThrough, "b|basepath", &basePath);
    files = args[1..$];
    
    if (basePath.length && basePath.exists)
        {/*setCUrrentDirectory(basePath) : use to solve relative resource path\name*/}
    
    if (!files.length) return; // + verbose, msg
    foreach(f; files) 
    {
        json2Items(f, items);
        Items2Module(f, items);
    } 
    readln;
}

void json2Items(string fname, out ResourceItems items)
{
    if (!fname.exists) return; // + verbose, msg
    size_t size = cast(size_t) getSize(fname);
    if (size == 0) return;   // + verbose, msg
    
    auto json_string = cast(string) std.file.read(fname, size);
    JSONValue root = parseJSON(json_string);
    if (root.type != JSON_TYPE.OBJECT) return;      // invalid format
    JSONValue * itms = ("items" in root.object);
    if (itms == null) return;                       // invalid format
    if (itms.type != JSON_TYPE.ARRAY) return;       // invalid format
    foreach(itm; itms.array)
    {
        if (itm.type != JSON_TYPE.OBJECT) continue;   // invalid format
        
        JSONValue * itm_tpe = ("resourceType" in itm.object);
        JSONValue * itm_nme = ("name" in itm.object);   
        JSONValue * itm_idt = ("identifier" in itm.object);
        JSONValue * itm_fmt = ("format" in itm.object);
        JSONValue * itm_mdt = ("metadata" in itm.object);
        
        if (itm_tpe == null) continue; // invalid format
        if (itm_nme == null) continue; // invalid format
        if (itm_idt == null) continue; // invalid format
        if (itm_fmt == null) continue; // invalid format
        if (itm_mdt == null) continue; // invalid format
        
        if (itm_tpe.type != JSON_TYPE.STRING) continue; // invalid format
        if (itm_nme.type != JSON_TYPE.STRING) continue; // invalid format
        if (itm_idt.type != JSON_TYPE.STRING) continue; // invalid format
        if (itm_fmt.type != JSON_TYPE.STRING) continue; // invalid format
        if (itm_mdt.type != JSON_TYPE.STRING) continue; // invalid format
        
        string[] nme_vs;
        string nme_v = itm_nme.str;
        string idt_v = itm_idt.str;
        string mdt_v = itm_mdt.str;
        ResType tpe_v = to!ResType(itm_tpe.str);
        ResFormat fmt_v = to!ResFormat(itm_fmt.str);
        
        if (!nme_v.exists) continue; // path or filename must exists
        
        if (nme_v.isDir)
            foreach(e; dirEntries(nme_v, SpanMode.shallow))
                nme_vs ~= e;
        else nme_vs ~= nme_v;
        
        foreach(n; nme_vs) 
        {
            // creates item for the file n
        } 
        
    }
    
    
    
    
    
    
    
    void json_print(ref JSONValue value)
    {
    
        writeln("-------------");
        JSON_TYPE tp = value.type;
        final switch(tp){
            case JSON_TYPE.ARRAY:  
                foreach(v; value.array)    
                    json_print(v); 
                break;
            case JSON_TYPE.FALSE:   
                writeln(value);
                break;
            case JSON_TYPE.FLOAT:
                writeln(value);   
                break;
            case JSON_TYPE.INTEGER:
                writeln(value); 
                break;
            case JSON_TYPE.NULL:  
                break;
            case JSON_TYPE.OBJECT:
                writeln(value);
                writeln(("identifier" in value.object) != null);
                foreach(v; value.object)    
                    json_print(v);  
                break;
            case JSON_TYPE.STRING:
                writeln(value); 
                break;            
            case JSON_TYPE.TRUE: 
                writeln(value); 
                break;            
            case JSON_TYPE.UINTEGER:
                writeln(value); 
                break;            
        }
    } 
    
    json_print(root);
}

void Items2Module(string fname, ref ResourceItems items)
{
}


