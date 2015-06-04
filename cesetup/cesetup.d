module cesetup;

import std.stdio: writeln, readln;
import std.file: mkdir, exists;
import std.getopt;
import std.path;
import std.conv;
import std.string;
import std.process: environment, executeShell;


version(X86)    version(linux)  version = nux32;
version(X86_64) version(linux)  version = nux64;
version(X86)    version(Windows)version = win32;

version(win32) enum exeExt = ".exe";
else enum exeExt = "";

struct Resource
{
    ubyte[] data;
    string destName;
    bool isExe;
}

auto coedit = Resource(cast(ubyte[]) import("coedit" ~ exeExt), "coedit" ~ exeExt, true);
auto cesyms = Resource(cast(ubyte[]) import("cesyms" ~ exeExt), "cesyms" ~ exeExt, true);
auto cetodo = Resource(cast(ubyte[]) import("cetodo" ~ exeExt), "cetodo" ~ exeExt, true);

auto dcd_server = Resource(cast(ubyte[]) import("dcd-server" ~ exeExt), "dcd-server" ~ exeExt, true);
auto dcd_client = Resource(cast(ubyte[]) import("dcd-client" ~ exeExt), "dcd-client" ~ exeExt, true);

auto icon   = Resource(cast(ubyte[]) import("coedit.ico"), "coedit.ico", false);
auto png    = Resource(cast(ubyte[]) import("coedit.png"), "coedit.png", false);

auto celic   = Resource(cast(ubyte[]) import("coedit.license.txt"), "coedit.license.txt", false);
auto dcdlic  = Resource(cast(ubyte[]) import("dcd.license.txt"), "dcd.license.txt", false);


static string exePath, appDataPath;
version(win32){} else static bool asSu;

static this()
{
    version(win32)
    { 
        exePath = environment.get("PROGRAMFILES") ~ r"\Coedit\";
        appDataPath = environment.get("APPDATA") ~ r"\Coedit\";
    }
    else
    {
        asSu = environment.get("SUDO_USER") != "";
        if (asSu)
        {
            exePath = "/usr/bin";
            appDataPath = "/home/" ~ environment.get("SUDO_USER") ~ "/Coedit/";
        }
        else
        {
            exePath = "/home/" ~ environment.get("USER") ~ "/bin/";
            appDataPath = "/home/" ~ environment.get("USER") ~ "/Coedit/";
        }
    }
} 

void main(string[] args)
{
    bool nodcd;
    bool uninstall;

    getopt(args, config.passThrough, 
        "nodcd", &nodcd, 
        "u|uninstall", &uninstall
    );
    
    writeln("|---------------------------------------------|");
    writeln("|            Coedit 1.0 RC1 setup             |");
    writeln("|---------------------------------------------|");
    version(win32)
    writeln("| the setup must be run as admin              |");
    writeln("| options:                                    |");
    writeln("| --nodcd: skip DCD setup                     |");
    writeln("| -u: uninstall                               |");
    writeln("| press a key to continue...                  |");
    writeln("|---------------------------------------------|");
    
    
    /*version(win32){} else if (environment.get("SUDO_USER") == "")
    {
        writeln("the extractor must be run with sudo !");
        readln;
        return;
    }*/    
    
    readln;
    writeln("|---------------------------------------------|");    
    size_t failures; 
    if(!uninstall)
    {
        if (installResource!(coedit)(exePath))
            writeln("| Coedit main application extracted           |");
        else failures++;
        if (installResource!(cesyms)(exePath))
            writeln("| Coedit symbol list builder extracted        |");
        else failures++;        
        if (installResource!(cetodo)(exePath))
            writeln("| Coedit todo comment parser extracted        |");
        else failures++;
        if (installResource!celic(appDataPath))
            writeln("| Coedit license file extracted               |");
        else failures++;   
        if (installResource!icon(appDataPath))
            writeln("| Coedit icon file extracted                  |");
        else failures++;  
        if (installResource!png(appDataPath))
            writeln("| Coedit big png logo extracted               |");
        else failures++;                       
        
        if (!nodcd)
        {
            if (installResource!(dcd_server)(exePath))
                writeln("| Completion daemon server extracted          |");
            else failures++; 
            if (installResource!(dcd_client)(exePath))
                writeln("| Completion daemon client extracted          |");
            else failures++;  
            if (installResource!dcdlic(appDataPath))
                writeln("| Completion daemon license extracted         |");
            else failures++;                                     
        }
        
        writeln("|---------------------------------------------|");
        if (failures)
        {
            writeln("| there are ERRORS, plz contact the support   |");
        }
        else
        {
            version(win32) win32PostInstall();
            writeln("| the files are correctly extracted           |");
        }
        writeln("| press a key to exit...                      |");
        writeln("|---------------------------------------------|");
        readln;   
    }
    else
    {
        failures += !uinstallResource!coedit(exePath);
        failures += !uinstallResource!cesyms(exePath);
        failures += !uinstallResource!cetodo(exePath);
        failures += !uinstallResource!celic(appDataPath);
        failures += !uinstallResource!icon(appDataPath);
        failures += !uinstallResource!png(appDataPath);
        failures += !uinstallResource!dcd_client(exePath);
        failures += !uinstallResource!dcd_server(exePath);
        failures += !uinstallResource!dcdlic(appDataPath); 
        
        version(win32) 
        {
            import std.file: rmdir;
            try std.file.rmdir(exePath);
            catch(Exception e) failures++;
        }
        
        if (failures)
        {
            writeln("| there are ERRORS, plz contact the support   |");
        }
        else
        {
            writeln("| the files are correctly removed             |");
        }
        writeln("| press a key to exit...                      |");
        writeln("|---------------------------------------------|");
        readln;         
    }
}

bool installResource(alias resource)(string path)
{
    import std.stream: File, FileMode;
    import std.file: exists, mkdir;
    
    if (!path.exists)
        mkdir(path);
    if (!path.exists)
        return false;
    
    try 
    {
        string fname = path ~ dirSeparator ~ resource.destName;
        File f = new File(fname, FileMode.OutNew);
        f.write(resource.data);
        f.close;
        
        version(win32) {} else 
            if (resource.isExe)
                if (fname.exists)
                {
                    string cmd = "chmod +x " ~ fname;
                    executeShell(cmd);
                }
    } 
    catch (Exception e) 
        return false;
    
    return true;
}

bool uinstallResource(alias resource)(string path)
{
    import std.file: exists, remove;  
    string fname = path ~ dirSeparator ~ resource.destName;
    if (!fname.exists) return true;
    try remove(fname);
    catch (Exception e) return false;
    return true;
     
}

void nuxPostInstall()
{
}

void win32PostInstall()
{
    /*
        IconFile does not work
    */

    string link = environment.get(r"USERPROFILE") ~ "\\Desktop\\Coedit.url";
    string target = exePath ~ "coedit.exe";
    string ico = appDataPath ~ "coedit.ico";

    import std.stream: File, FileMode;
    File f = new File(link, FileMode.OutNew);
    f.writeLine("[InternetShortcut]");
    f.writeString("URL=");
    f.writeLine("\"" ~ target ~ "\"");
    f.writeString("IconFile=");
    f.writeLine("\"" ~ target ~ "\"");
    f.writeLine("IconIndex=0");
    f.close;

}
