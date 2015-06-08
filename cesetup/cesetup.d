module cesetup;

import std.stdio;
import std.file: mkdir, exists, remove, rmdir, getSize, FileException;
import std.stream: File, FileMode;
import std.process: environment, executeShell;
import std.path: dirSeparator;
import std.string: strip, toLower;
import std.getopt;

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


static string exePath, appDataPath, shortCutPath;
version(win32){} else immutable bool asSu;

static this()
{
    version(win32)
    { 
        exePath = environment.get("PROGRAMFILES") ~ r"\Coedit\";
        appDataPath = environment.get("APPDATA") ~ r"\Coedit\";
        shortCutPath = environment.get("USERPROFILE") ~ r"\Desktop\";
    }
    else
    {
        asSu = environment.get("SUDO_USER") != "";
        if (asSu)
        {
            exePath = "/usr/bin";
            appDataPath = "/home/" ~ environment.get("SUDO_USER") ~ "/Coedit/";
            shortCutPath = "/usr/share/applications/";
        }
        else
        {
            exePath = "/home/" ~ environment.get("USER") ~ "/bin/";
            appDataPath = "/home/" ~ environment.get("USER") ~ "/Coedit/";
            shortCutPath = "/home/" ~ environment.get("USER") ~ "/.local/share/applications/";
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
    if (!uninstall)
    writeln("|              Coedit 1.0 setup               |");
    else
    writeln("|           Coedit 1.0 uninstaller            |");
    writeln("|---------------------------------------------|");
    
    version(win32)
    writeln("| the setup program must be run as admin      |");
    else if(!asSu)
    writeln("| the program can be setup globally (sudo)    |");
    
    writeln("| options:                                    |");
    writeln("| --nodcd: skip DCD setup                     |");
    writeln("| -u: uninstall                               |");
    writeln("| press A to abort or another key to start... |");
    writeln("|---------------------------------------------|");   
    
    const string inp = readln.strip;
    if (inp.toLower == "a") return;
    
    writeln("|---------------------------------------------|");    
    size_t failures; 
    if(!uninstall)
    {
        if (installResource(coedit, exePath))
            writeln("| Coedit main application extracted           |");
        else failures++;
        if (installResource(cesyms, exePath))
            writeln("| Coedit symbol list builder extracted        |");
        else failures++;        
        if (installResource(cetodo, exePath))
            writeln("| Coedit todo comment parser extracted        |");
        else failures++;
        if (installResource(celic, appDataPath))
            writeln("| Coedit license file extracted               |");
        else failures++;   
        if (installResource(icon, appDataPath))
            writeln("| Coedit icon file extracted                  |");
        else failures++;  
        if (installResource(png, appDataPath))
            writeln("| Coedit big png logo extracted               |");
        else failures++;                       
        
        if (!nodcd)
        {
            if (installResource(dcd_server, exePath))
                writeln("| Completion daemon server extracted          |");
            else failures++; 
            if (installResource(dcd_client, exePath))
                writeln("| Completion daemon client extracted          |");
            else failures++;  
            if (installResource(dcdlic, appDataPath))
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
            else nuxPostInstall();
            writeln("| the files are correctly extracted           |");
        }
        writeln("| press a key to exit...                      |");
        writeln("|---------------------------------------------|");
        readln;  
    }
    else
    {
        failures += !uninstallResource(coedit, exePath);
        failures += !uninstallResource(cesyms, exePath);
        failures += !uninstallResource(cetodo, exePath);
        failures += !uninstallResource(celic, appDataPath);
        failures += !uninstallResource(icon, appDataPath);
        failures += !uninstallResource(png, appDataPath);
        failures += !uninstallResource(dcd_client, exePath);
        failures += !uninstallResource(dcd_server, exePath);
        failures += !uninstallResource(dcdlic, appDataPath); 
        
        version(win32) 
        {
            try rmdir(exePath);
            catch(FileException e) failures++;
        }
        
        if (failures)
        {
            writeln("| there are ERRORS, plz contact the support   |");
        }
        else
        {
            version(win32) win32PostUninstall();
            else nuxPostUninstall();
            writeln("| the files are correctly removed             |");
        }
        writeln("| press a key to exit...                      |");
        writeln("|---------------------------------------------|");
        readln;         
    }
}

bool installResource(Resource resource, string path)
{
    if (!path.exists)
        mkdir(path);
    if (!path.exists)
        return false;
    
    try 
    {
        const string fname = path ~ dirSeparator ~ resource.destName;
        File f = new File(fname, FileMode.OutNew);
        f.write(resource.data);
        f.close;
        
        version(win32) {} else 
            if (resource.isExe)
                if (fname.exists)
                {
                    string cmd = "chmod a+x " ~ fname;
                    executeShell(cmd);
                }
    } 
    catch (Exception e) 
        return false;
    
    return true;
}

bool uninstallResource(Resource resource, string path)
{ 
    string fname = path ~ dirSeparator ~ resource.destName;
    if (!fname.exists) return true;
    try remove(fname);
    catch (FileException e) return false;
    return true;  
}

void nuxPostInstall()
{
    File f = new File(shortCutPath ~ "coedit.desktop", FileMode.OutNew);
    f.writeLine("[Desktop Entry]");
    f.writeLine("Name=coedit");
    f.writeLine("Exec=coedit %f");
    f.writeLine("Icon=" ~ appDataPath ~ "/coedit.png");
    f.writeLine("Type=Application");
    f.writeLine("Categories=Utility;Application;Development;");
    f.writeLine("Terminal=false"); 
    f.close;    
}

void nuxPostUninstall()
{
    try remove(shortCutPath ~ "coedit.desktop");
    catch (FileException e) {}
}

void win32PostInstall()
{
    // notice: this is not a true shortcut, other options are
    // - create a true lnk by generating and executing a vbs
    // - use winapi...
    string target = exePath ~ "coedit.exe";
    File f = new File(shortCutPath ~ "Coedit.url", FileMode.OutNew);
    f.writeLine("[InternetShortcut]");
    f.writeString("URL=");
    f.writeLine("\"" ~ target ~ "\"");
    f.writeString("IconFile=");
    f.writeLine("\"" ~ target ~ "\"");
    f.writeLine("IconIndex=0");
    f.close;
}

void win32PostUninstall()
{
    try remove(shortCutPath ~ "Coedit.url");
    catch (FileException e) {}
}
