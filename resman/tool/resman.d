module resman;

import std.stdio, std.getopt, std.path;
import std.json, std.file, std.conv, std.string;

import utils, item;

enum ResType {aFile, aFolder}

/** 
 * Resman main procedure.
 * Params:
 * args = the options and a list of file.
 * - -v or --verbose: detailed messages for each operation.
 * - -b or --basepath: a path used to solve relative resource filenames.
 * - -files: a list of files produced by the Resman widget.
 */
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
    //readln;
}
 
static string fmt_json_err = "invalid JSON format: %s, in file '%s'";
static string fmt_file_skp = "invalid JSON file: %s, '%s' is skipped";
static string fmt_item_skp = "invalid JSON item: %s, '%s' is skipped";
static string fmt_item_rep = "invalid JSON value: %s, has been set to '%s'";

/**
 * Transforms the items contained in the JSON file fname to an array of 
 * ResourceItem.
 * Params:
 * fname = a valid filename.
 * items = container for the ResourceItem found in the file.
 */
void json2Items(string fname, out ResourceItems items)
{
    if (!fname.exists) {
        writeMessage(true, format(fmt_file_skp, "it does not exists", fname));
        return; 
    }
    
    size_t size = cast(size_t) getSize(fname);
    if (size == 0) {
        writeMessage(true, format(fmt_file_skp, "empty file", fname));
        return; 
    }
    
    auto json_string = cast(string) std.file.read(fname, size);
    JSONValue root = parseJSON(json_string);
    if (root.type != JSON_TYPE.OBJECT)  {
        writeMessage(true, format(fmt_json_err, "first value must be of type OBJECT", fname));
        return; 
    }
     
    JSONValue * itms = ("resources" in root.object);
    if (itms == null) {
        writeMessage(true, format(fmt_json_err, "the member 'resources' is missing", fname));
        return; 
    }
    if (itms.type != JSON_TYPE.ARRAY) {
        writeMessage(true, format(fmt_json_err, "'resources' must be of type ARRAY", fname));
        return;
    }   
         
    foreach(itm; itms.array) 
    {
        if (itm.type != JSON_TYPE.OBJECT) {
            writeMessage(true, format(fmt_item_skp, "an item must be of type OBJECT", itm.toString));
            continue;
        }   
        
        JSONValue * itm_tpe = ("resourceType" in itm.object);
        JSONValue * itm_nme = ("name" in itm.object);   
        JSONValue * itm_idt = ("identifier" in itm.object);
        JSONValue * itm_fmt = ("format" in itm.object);
        JSONValue * itm_mdt = ("metadata" in itm.object);
        
        if (itm_tpe == null || itm_nme == null || itm_idt == null || 
            itm_fmt == null || itm_mdt == null) 
        {
            writeMessage(true, format(fmt_item_skp, "an item must contain all the properties", 
                itm.toString));
            continue;           
        } 
        
        if (itm_tpe.type != JSON_TYPE.STRING || itm_nme.type != JSON_TYPE.STRING ||
            itm_idt.type != JSON_TYPE.STRING || itm_fmt.type != JSON_TYPE.STRING ||
            itm_mdt.type != JSON_TYPE.STRING)
        {
            writeMessage(true, format(fmt_item_skp, "an item value must be of type STRING", 
                itm.toString));
            continue;           
        }    
        
        string[] nme_vs;
        string nme_v = itm_nme.str;
        string idt_v = itm_idt.str;
        string mdt_v = itm_mdt.str;
        ResType tpe_v;
        ResFormat fmt_v;
        
        if (!nme_v.exists) {
            writeMessage(true, format(fmt_item_skp, "the item filename or path is invalid", nme_v));
            continue;
        }      
        
        try tpe_v = to!ResType(itm_tpe.str);
        catch (Exception e) { 
            if (isDir(nme_v)) tpe_v = ResType.aFolder;
            else tpe_v = ResType.aFile;
            writeMessage(true, format(fmt_item_rep, "resourceType", to!string(tpe_v)));
        }
        
        try fmt_v = to!ResFormat(itm_fmt.str);
        catch (Exception e) {
            fmt_v = ResFormat.bytes;
            writeMessage(true, format(fmt_item_rep, "format", to!string(fmt_v)));
        }
        
        if (idt_v == "" && tpe_v == ResType.aFile)
            writeMessage(true, format(fmt_item_rep, "identifier", "the striped filename"));
            
        if (idt_v != "" && tpe_v == ResType.aFolder)
            writeMessage(true, format(fmt_item_rep, 
            "identifier", "the striped filename of each entry in the direrctory")); 
        
        if (nme_v.isDir)
        {
            foreach(e; dirEntries(nme_v, SpanMode.shallow)) nme_vs ~= e;
            idt_v = "";
        }
        else nme_vs ~= nme_v;
        
        foreach(n; nme_vs) 
        {
            items ~= new ResourceItem(n, fmt_v, idt_v, mdt_v);
        }
        
    }
}

/** 
 * Writes the ResourceItems *items* as valid D code D in a file whose content 
 * will be imported by the library template activateResman(). The file written
 * in this method has the same name as the JSON file analyzed before, but with
 * another extension.
 * Params:
 * fname = the name of the JSON file parsed previously. 
 * items = an array of *ResourceItem*
 */
void Items2Module(string fname, ref ResourceItems resItems)
{
    string outputFname = fname.stripExtension ~ ".resdata";
    if (outputFname.exists) std.file.remove(outputFname);
    
    // writes the resource representations to the module
    writeMessage(true, "writing the resources data as text...");
    outputFname.append("\r\n\r\n");
    outputFname.append("private static const resource_txt = [");
    foreach (i; 0 .. resItems.length -1)
        outputFname.append(format("\r\n\t" ~ "\"" ~ "%s" ~ "\"" ~ ",", resItems[i].resText));
    outputFname.append(format("\r\n\t" ~ "\"" ~ "%s" ~ "\"" ~ "\r\n];", resItems[$-1].resText));

    // writes the resources identifiers to the module
    writeMessage(true, "writing the resources identifiers...");
    outputFname.append("\r\n\r\n");
    outputFname.append("private static const resource_idt = [");
    foreach (i; 0 .. resItems.length -1)
        outputFname.append(format("\r\n\t" ~ "\"" ~ "%s" ~ "\"" ~ ",", resItems[i].resIdentifier));
    outputFname.append(format("\r\n\t" ~ "\"" ~ "%s" ~ "\"" ~ "\r\n];", resItems[$-1].resIdentifier));
    
    // writes the resources metadata to the module
    writeMessage(true, "writing the resources metadata...");
    outputFname.append("\r\n\r\n");
    outputFname.append("private static const resource_mdt = [");
    foreach (i; 0 .. resItems.length -1)
        outputFname.append(format("\r\n\t" ~ "\"" ~ "%s" ~ "\"" ~ ",", resItems[i].resMetaData));
    outputFname.append(format("\r\n\t" ~ "\"" ~ "%s" ~ "\"" ~ "\r\n];", resItems[$-1].resMetaData));    

    // writes the resources encoder kind to the module
    writeMessage(true, "writing the resources kind...");
    outputFname.append("\r\n\r\n");
    outputFname.append("private static const resource_enc = [");
    foreach (i; 0 .. resItems.length -1)
        outputFname.append(format("\r\n\t%s.%s,", ResFormat.stringof, resItems[i].resFormat));
    outputFname.append(format("\r\n\t%s.%s \r\n];", ResFormat.stringof, resItems[$-1].resFormat));

    // writes the initial sums to the module
    writeMessage(true, "writing the resources initial sum...");
    outputFname.append("\r\n\r\n");
    outputFname.append("private static const uint[] resource_sumi = [");
    foreach (i; 0 .. resItems.length -1)
        outputFname.append(format("\r\n\t" ~ "%.d" ~ ",", resItems[i].initialSum));
    outputFname.append(format("\r\n\t" ~ "%.d" ~ "\r\n];", resItems[$-1].initialSum));

    // writes the encoded sums to the module
    writeMessage(true, "writing the resources encoded sum...");
    outputFname.append("\r\n\r\n");
    outputFname.append("private static const uint[] resource_sume = [");
    foreach (i; 0 .. resItems.length -1)
        outputFname.append(format("\r\n\t" ~ "%.d" ~ ",", resItems[i].encodedSum));
    outputFname.append(format("\r\n\t" ~ "%.d" ~ "\r\n];", resItems[$-1].encodedSum));
}


