module cesetup;

import std.stdio;
import std.file: mkdirRecurse, exists, remove, rmdir, getSize, FileException;
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

Resource[] ceResources =
[
    Resource(cast(ImpType) import("coedit" ~ exeExt), "coedit" ~ exeExt, true),
    Resource(cast(ImpType) import("cesyms" ~ exeExt), "cesyms" ~ exeExt, true),
    Resource(cast(ImpType) import("cetodo" ~ exeExt), "cetodo" ~ exeExt, true),
    Resource(cast(ImpType) import("coedit.ico"), "coedit.ico", false),
    Resource(cast(ImpType) import("coedit.png"), "coedit.png", false),
    Resource(cast(ImpType) import("coedit.license.txt"), "coedit.license.txt", false)
];

Resource[] dcdResources =
[
    Resource(cast(ImpType) import("dcd-server" ~ exeExt), "dcd-server" ~ exeExt, true),
    Resource(cast(ImpType) import("dcd-client" ~ exeExt), "dcd-client" ~ exeExt, true),
    Resource(cast(ImpType) import("dcd.license.txt"), "dcd.license.txt", false)
];


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
    
    static void justify(char A)(string s)
    in
    {
        assert (s.length <= width, "too long to fit on a line...");
    }
    body
    {
        static if (A == 'L') 
            writeln("| ",  leftJustify(s, width, ' '), " |");
        else static if (A == 'C') 
            writeln("| ",  center(s, width, ' '), " |");
        else static if (A == 'R') 
            writeln("| ",  rightJustify(s, width, ' '), " |");  
        else static assert(0, "invalid justification, L|C|R expected");      
    }  
    
    static void separate(){separator.writeln;}
    
    static void emptyLine(){justify!'L'("");}
}


static immutable string exePath, appDataPath, shortCutPath;
version(linux) immutable bool asSu;


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
    bool listfiles;

    getopt(args, config.passThrough, 
        "nodcd", &nodcd, 
        "u|uninstall", &uninstall,
        "l|list", &listfiles
    );
    
    Formater.separate;


    if (listfiles)
    {
        static immutable fmtRes = "%s installed: %s";
        string fname;

        Formater.separate;
        Formater.justify!'C'("files list and status");
        Formater.separate;

        foreach(res; ceResources)
        {
            fname = res.isExe ? exePath ~ res.destName : appDataPath ~ res.destName;
            writefln(fmtRes, fname, exists(fname));
        }
        foreach(res; dcdResources)
        {
            fname = res.isExe ? exePath ~ res.destName : appDataPath ~ res.destName;
            writefln(fmtRes, fname, exists(fname));
        }

        Formater.separate;
        return;
    }

    if (!uninstall) Formater.justify!'C'("Coedit 2 alpha 2 - setup");
    else Formater.justify!'C'("Coedit uninstaller");
    
    Formater.separate;
    version(win32) Formater.justify!'L'("the setup program must be run as admin");
    else 
    {   
        if(!asSu) Formater.justify!'L'("Coedit can also be setup globally (sudo)");
        else Formater.justify!'L'("Coedit will be accessible from all the accounts");
    }
    
    Formater.separate;
    Formater.justify!'L'("options:");
    Formater.emptyLine;
    Formater.justify!'L'("-l | --list: list files and status");
    if (!uninstall) 
    {
        if (!nodcd) Formater.justify!'L'("--nodcd: skip DCD setup");
        Formater.justify!'L'("-u | --uninstall: uninstall");
    }
    else if (!nodcd) Formater.justify!'L'("--nodcd: do not remove DCD");
    Formater.justify!'L'("press A to abort or another key to start...");
    Formater.separate;   
    
    const string inp = readln.strip;
    if (inp.toLower == "a") return;
    
    Formater.separate;

    size_t failures;
    bool done;
    if(!uninstall)
    {
        static immutable extractMsg = [": FAILURE", ": extracted"];
        foreach(res; ceResources)
        {
            if (res.isExe)
                done = installResource(res, exePath);
            else
                done = installResource(res, appDataPath);
            Formater.justify!'L'(res.destName ~ extractMsg[done]);
            failures += !done;
        }
        if (!nodcd) foreach(res; dcdResources)
        {
            if (res.isExe)
                done = installResource(res, exePath);
            else
                done = installResource(res, appDataPath);
            Formater.justify!'L'(res.destName ~ extractMsg[done]);
            failures += !done;
        }
        
        Formater.separate;
        if (failures)
            Formater.justify!'L'("there are ERRORS, plz contact the support");
        else
        {
            version(win32) win32PostInstall();
            else nuxPostInstall();
            Formater.justify!'L'("the files are correctly extracted...");
        }
    }
    else
    {
        if (!asSu && exists("/usr/bin/coedit"))
        {
            Formater.separate;
            Formater.justify!'L'("warning, CE seems to be installed with sudo");
            Formater.justify!'L'("but the uninstaller is not launched with sudo.");
            Formater.separate;
        }
        else if (asSu && exists("/home/" ~ environment.get("USER") ~ "/bin/coedit"))
        {
            Formater.separate;
            Formater.justify!'L'("warning, CE seems not to be installed with sudo");
            Formater.justify!'L'("...but the uninstaller is launched with sudo.");
            Formater.separate;
        }
        static immutable rmMsg = [": FAILURE", ": deleted"];
        foreach(res; ceResources)
        {
            if (res.isExe)
                done = uninstallResource(res, exePath);
            else
                done = uninstallResource(res, appDataPath);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        if (!nodcd) foreach(res; dcdResources)
        {
            if (res.isExe)
                done = uninstallResource(res, exePath);
            else
                done = uninstallResource(res, appDataPath);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        
        version(win32) 
        {
            try rmdir(exePath);
            catch(FileException e) failures++;
        }

        Formater.separate;
        if (failures)
            Formater.justify!'L'("there are ERRORS, plz contact the support");
        else
        {
            version(win32) win32PostUninstall();
            else nuxPostUninstall();
            Formater.justify!'L'("the files are correctly removed...");
        }
    }
    Formater.emptyLine;
    Formater.justify!'R'("...press a key to exit");
    Formater.separate;
    readln;
}

string extractedName(Resource resource, string path)
{
    return path ~ dirSeparator ~ resource.destName;    
}

bool installResource(Resource resource, string path)
{
    if (!path.exists)
        mkdirRecurse(path);
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
    else return tryRemove(fname);  
}

bool tryRemove(string fname)
{
    bool result = true;
    try remove(fname);
    catch (FileException e) {result = false;}
    return result;  
}

version(linux) void nuxPostInstall()
{
    mkdirRecurse(shortCutPath);
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

version(linux) void nuxPostUninstall()
{
    tryRemove(shortCutPath ~ "coedit.desktop");
}

version (win32) void win32PostInstall()
{
    import std.conv: to;
    import std.random: uniform;
    
    // shortcut prior to v 1 upd 2 was actually an url.
    tryRemove(shortCutPath ~ "Coedit.url");
    
    string target = exePath ~ "coedit.exe";
    string vbsName;
    do vbsName = environment.get("TEMP") ~ r"\cesh" ~ uniform(0,int.max).to!string ~ ".vbs";
    while (vbsName.exists);
    
    string vbsCode = "
        set WshShell = CreateObject(\"WScript.shell\")
        strDesktop = WshShell.SpecialFolders(\"Desktop\")
        set lnk = WshShell.CreateShortcut(strDesktop + \"\\Coedit.lnk\")
        lnk.TargetPath = \"%s\" 
        lnk.Save
    ";
    File vbs = new File(vbsName, FileMode.OutNew);
    vbs.writefln(vbsCode, target);
    vbs.close;
    executeShell(vbsName);
    
    tryRemove(vbsName); 
}

version (win32) void win32PostUninstall()
{
    // shortcut prior to v 1 upd 2 was actually an url.
    tryRemove(shortCutPath ~ "Coedit.url"); 
    tryRemove(shortCutPath ~ "Coedit.lnk");
}
