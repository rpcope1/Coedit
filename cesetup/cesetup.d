module cesetup;

import
    std.stdio, std.file, std.process, std.path, std.string, std.getopt;

version(X86)    version(linux)  version = nux32;
version(X86_64) version(linux)  version = nux64;
version(X86)    version(Windows)version = win32;

version(win32) enum exeExt = ".exe";
else enum exeExt = "";

alias ImpType = immutable ubyte[];
alias ResType = immutable Resource;

enum Kind
{
    exe,
    dat,
    doc,
}

struct Resource
{
    ImpType data;
    immutable string destName;
    immutable Kind kind;
}

Resource[] ceResources =
[
    Resource(cast(ImpType) import("coedit" ~ exeExt), "coedit" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("cesyms" ~ exeExt), "cesyms" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("cetodo" ~ exeExt), "cetodo" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("coedit.ico"), "coedit.ico", Kind.dat),
    Resource(cast(ImpType) import("coedit.png"), "coedit.png", Kind.dat),
    Resource(cast(ImpType) import("coedit.license.txt"), "coedit.license.txt", Kind.doc)
];

Resource[] dcdResources =
[
    Resource(cast(ImpType) import("dcd-server" ~ exeExt), "dcd-server" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dcd-client" ~ exeExt), "dcd-client" ~ exeExt, Kind.exe),
    Resource(cast(ImpType) import("dcd.license.txt"), "dcd.license.txt", Kind.doc)
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

static immutable string exePath, datPath, shortCutPath;
version(linux) immutable bool asSu;

static this()
{
    version(win32)
    { 
        exePath = environment.get("PROGRAMFILES") ~ r"\Coedit\";
        datPath = environment.get("APPDATA") ~ r"\Coedit\";
        shortCutPath = environment.get("USERPROFILE") ~ r"\Desktop\";
    }
    else
    {
        asSu = environment.get("SUDO_USER") != "";
        if (asSu)
        {
            exePath = "/usr/bin";
            datPath = "/home/" ~ environment.get("SUDO_USER") ~ "/.config/Coedit/";
            shortCutPath = "/usr/share/applications/";
        }
        else
        {
            exePath = "/home/" ~ environment.get("USER") ~ "/bin/";
            datPath = "/home/" ~ environment.get("USER") ~ "/.config/Coedit/";
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
            fname = targetFilename(res);
            writefln(fmtRes, fname, exists(fname));
        }
        foreach(res; dcdResources)
        {
            fname = targetFilename(res);
            writefln(fmtRes, fname, exists(fname));
        }

        Formater.separate;
        return;
    }

    if (!uninstall) Formater.justify!'C'("Coedit 2 alpha 3 - setup");
    else Formater.justify!'C'("Coedit uninstaller");
    
    Formater.separate;
    version(win32) Formater.justify!'L'("the setup program must be run as admin");
    else 
    {   
        if(!asSu) Formater.justify!'L'("Coedit will be accessible to the current user");
        else Formater.justify!'L'("Coedit will be accessible to all the users");
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
            done = installResource(res);
            Formater.justify!'L'(res.destName ~ extractMsg[done]);
            failures += !done;
        }
        if (!nodcd) foreach(res; dcdResources)
        {
            done = installResource(res);
            Formater.justify!'L'(res.destName ~ extractMsg[done]);
            failures += !done;
        }
        Formater.separate;
        if (failures)
            Formater.justify!'L'("there are ERRORS, plz contact the support");
        else
        {
            postInstall();
            Formater.justify!'L'("the files are correctly extracted...");
        }
    }
    else
    {
        // check that uninstall is executed as install (sudo or not)
        version(linux)
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
        }
        // uninstall
        static immutable rmMsg = [": FAILURE", ": deleted"];
        foreach(res; ceResources)
        {
            done = uninstallResource(res);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        if (!nodcd) foreach(res; dcdResources)
        {
            done = uninstallResource(res);
            Formater.justify!'L'(res.destName ~ rmMsg[done]);
            failures += !done;
        }
        // remove $PF folder
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
            postUninstall();
            Formater.justify!'L'("the files are correctly removed...");
        }
    }
    Formater.emptyLine;
    Formater.justify!'R'("...press a key to exit");
    Formater.separate;
    readln;
}

/// Returns the resource target filename, according to its Kind
string targetFilename(Resource resource)
{
    with(Kind) final switch(resource.kind)
    {
        case Kind.exe: return exePath ~ resource.destName;
        case Kind.dat: return datPath ~ resource.destName;
        case Kind.doc: return datPath ~ resource.destName;
    }
}

/// Extracts and writes a resource to a file.
bool installResource(Resource resource)
{
    const string fname = resource.targetFilename;
    const string path = fname.dirName;
    if (!path.exists)
        mkdirRecurse(path);
    if (!path.exists)
        return false;
    
    try 
    {
        File f = File(resource.targetFilename, "w");
        f.rawWrite(resource.data);
        f.close;
        
        version(linux) if (resource.kind == Kind.exe && fname.exists)
            executeShell("chmod a+x " ~ fname);
    } 
    catch (Exception e) 
        return false;
    
    return true;
}

/// Deletes the file created for a resource
bool uninstallResource(Resource resource)
{
    const string fname = resource.targetFilename;
    if (!fname.exists) return true;
    else return tryRemove(fname);  
}

/// returns true if fname is deleted
bool tryRemove(string fname)
{
    bool result = true;
    try remove(fname);
    catch (FileException e) {result = false;}
    return result;  
}

/// adds menu entry, shortcut, etc
void postInstall()
{
    version(Win32)
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
        File vbs = File(vbsName, "w");
        vbs.writefln(vbsCode, target);
        vbs.close;
        executeShell(vbsName);

        tryRemove(vbsName);
    }
    else version(linux)
    {
        mkdirRecurse(shortCutPath);
        File f = File(shortCutPath ~ "coedit.desktop", "w");
        f.writeln("[Desktop Entry]");
        f.writeln("Name=coedit");
        f.writeln("Exec=coedit %f");
        f.writeln("Icon=" ~ datPath ~ "coedit.png");
        f.writeln("Type=Application");
        f.writeln("Categories=Application;IDE;Development;");
        f.writeln("Keywords=editor;Dlang;IDE;dmd;");
        f.writeln("Terminal=false");
        f.close;
    }
}

/// removes menu entry shortcuts, etc
void postUninstall()
{
    version(Win32)
    {
        // shortcut prior to v 1 upd 2 was actually an url.
        tryRemove(shortCutPath ~ "Coedit.url");
        tryRemove(shortCutPath ~ "Coedit.lnk");
    }
    else version(linux)
    {
        tryRemove(shortCutPath ~ "coedit.desktop");
    }
}

