module cesetup;

import std.stdio;
import std.file: mkdir, exists, remove, rmdir, getSize, FileException;
import std.stream: File, FileMode;
import std.process: environment, executeShell;
import std.path: dirSeparator;
import std.string: strip, toLower, center, leftJustify, rightJustify;
import std.getopt;

version(X86)    version(linux)  version = nux32;
version(X86_64) version(linux)  version = nux64;
version(X86)    version(Windows)version = win32;

version(win32) enum exeExt = ".exe";
else enum exeExt = "";

alias ImpType = immutable ubyte[];
alias ResType = immutable Resource;

struct Resource
{
    ImpType data;
    immutable string destName;
    immutable bool isExe;
}

ResType coedit = Resource(cast(ImpType) import("coedit" ~ exeExt), "coedit" ~ exeExt, true);
ResType cesyms = Resource(cast(ImpType) import("cesyms" ~ exeExt), "cesyms" ~ exeExt, true);
ResType cetodo = Resource(cast(ImpType) import("cetodo" ~ exeExt), "cetodo" ~ exeExt, true);

ResType dcd_server = Resource(cast(ImpType) import("dcd-server" ~ exeExt), "dcd-server" ~ exeExt, true);
ResType dcd_client = Resource(cast(ImpType) import("dcd-client" ~ exeExt), "dcd-client" ~ exeExt, true);

ResType icon   = Resource(cast(ImpType) import("coedit.ico"), "coedit.ico", false);
ResType png    = Resource(cast(ImpType) import("coedit.png"), "coedit.png", false);

ResType celic   = Resource(cast(ImpType) import("coedit.license.txt"), "coedit.license.txt", false);
ResType dcdlic  = Resource(cast(ImpType) import("dcd.license.txt"), "dcd.license.txt", false);


static struct Formater
{
    private enum width = 48;
    private static __gshared char[] separator;
    
    static this()
    {
        separator.length = width + 4;
        separator[] =  '-';
        separator[0] = '+';
        separator[$-1] = '+';
    }
    
    static void justify(char A, string s)()
    {
        static assert (s.length <= width, "too long to fit on a line...");
        static if (A == 'L') 
            writeln("| ",  leftJustify(s, width, ' '), " |");
        else static if (A == 'C') 
            writeln("| ",  center(s, width, ' '), " |");
        else static if (A == 'R') 
            writeln("| ",  rightJustify(s, width, ' '), " |");  
        else static assert(0, "invalid justification, L|C|R expected");      
    }  
    
    static void separate(){separator.writeln;}
    
    static void emptyLine(){justify!('L', "");}
}


static immutable string exePath, appDataPath, shortCutPath;
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
            appDataPath = "/home/" ~ environment.get("SUDO_USER") ~ "/.config/Coedit/";
            shortCutPath = "/usr/share/applications/";
        }
        else
        {
            exePath = "/home/" ~ environment.get("USER") ~ "/bin/";
            appDataPath = "/home/" ~ environment.get("USER") ~ "/.config/Coedit/";
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
    
    Formater.separate;
    if (!uninstall) Formater.justify!('C', "Coedit 1 update 2 - setup");
    else Formater.justify!('C', "Coedit uninstaller");
    
    Formater.separate;
    version(win32) Formater.justify!('L', "the setup program must be run as admin");
    else 
    {   
        if(!asSu) Formater.justify!('L', "Coedit can also be setup globally (sudo)");
        else Formater.justify!('L', "Coedit will be accessible from all the accounts");
    }
    
    Formater.separate;
    Formater.justify!('L', "options:");
    Formater.emptyLine;
    if (!uninstall) 
    {
        if (!nodcd) Formater.justify!('L', "--nodcd: skip DCD setup");
        Formater.justify!('L', "-u: uninstall");
    }
    else if (!nodcd) Formater.justify!('L', "--nodcd: do not remove DCD");
    Formater.justify!('L', "press A to abort or another key to start...");
    Formater.separate;   
    
    const string inp = readln.strip;
    if (inp.toLower == "a") return;
    
    Formater.separate;    
    size_t failures; 
    if(!uninstall)
    {
        if (installResource(coedit, exePath))
            Formater.justify!('L', "Coedit main application extracted");
        else failures++;
        if (installResource(cesyms, exePath))
            Formater.justify!('L', "Coedit symbol list builder extracted");
        else failures++;        
        if (installResource(cetodo, exePath))
            Formater.justify!('L', "Coedit todo comment parser extracted");
        else failures++;
        if (installResource(celic, appDataPath))
            Formater.justify!('L', "Coedit license file extracted");
        else failures++;   
        if (installResource(icon, appDataPath))
            Formater.justify!('L', "Coedit icon file extracted");
        else failures++;  
        if (installResource(png, appDataPath))
            Formater.justify!('L', "Coedit big png logo extracted");
        else failures++;                       
        
        if (!nodcd)
        {
            if (installResource(dcd_server, exePath))
                Formater.justify!('L', "Completion daemon server extracted");
            else failures++; 
            if (installResource(dcd_client, exePath))
                Formater.justify!('L', "Completion daemon client extracted");
            else failures++;  
            if (installResource(dcdlic, appDataPath))
                Formater.justify!('L', "Completion daemon license extracted");
            else failures++;                                     
        }
        
        Formater.separate;
        if (failures)
            Formater.justify!('L', "there are ERRORS, plz contact the support");
        else
        {
            version(win32) win32PostInstall();
            else nuxPostInstall();
            Formater.justify!('L', "the files are correctly extracted...");
        }
        Formater.emptyLine;
        Formater.justify!('R', "...press a key to exit");
        Formater.separate;
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
        if (!nodcd)
        {
            failures += !uninstallResource(dcd_client, exePath);
            failures += !uninstallResource(dcd_server, exePath);
            failures += !uninstallResource(dcdlic, appDataPath);
        } 
        
        version(win32) 
        {
            try rmdir(exePath);
            catch(FileException e) failures++;
        }
        
        if (failures)
            Formater.justify!('L', "there are ERRORS, plz contact the support");
        else
        {
            version(win32) win32PostUninstall();
            else nuxPostUninstall();
            Formater.justify!('L', "the files are correctly removed...");
        }
        Formater.emptyLine;
        Formater.justify!('R', "...press a key to exit");
        Formater.separate;
        readln;         
    }
}

string extractedName(Resource resource, string path)
{
    return path ~ dirSeparator ~ resource.destName;    
}

bool installResource(Resource resource, string path)
{
    if (!path.exists)
        mkdir(path);
    if (!path.exists)
        return false;
    
    try 
    {
        const string fname = extractedName(resource, path);
        File f = new File(fname, FileMode.OutNew);
        f.write(resource.data);
        f.close;
        
        version(win32) {} 
        else if (resource.isExe && fname.exists)
            executeShell("chmod a+x " ~ fname);
    } 
    catch (Exception e) 
        return false;
    
    return true;
}

bool uninstallResource(Resource resource, string path)
{ 
    const string fname = extractedName(resource, path);
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
    // note: this is not a true shortcut, other options are
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
