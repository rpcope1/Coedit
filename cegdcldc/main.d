module cegdcldc;

import
    core.thread, std.stdio, std.process, std.file;

void convertToGdc(string option, ref string[] options)
{
    // files
    if (option.exists)
        options ~= option;
    // switches
    else switch(option)
    {
        default:    break;
        case "w":   break;
        case "wi":  break;
    }
}

void convertToLdc2(string option, ref string[] options)
{
    // files
    if (option.exists)
        options ~= option;
    // switches
    switch(option)
    {
        default: break;
        case "w":   break;
        case "wi":  break;
    }
}

int main(string[] args)
{
    string[] commandLine = [readln];

    if (args.length == 1)
        return 1;

    if (commandLine[0] != "gdc" && commandLine[0] != "ldc2")
        return 2;

    switch(commandLine[0])
    {
        case "gdc":
            foreach(option; args[1..$])
                convertToGdc(option, commandLine);
            break;
        case "ldc2":
            foreach(option; args[1..$])
                convertToLdc2(option, commandLine);
            break;
        default: break;
    }

    ProcessPipes ppid = pipeProcess(commandLine);
    while(true)
    {
        Thread.sleep(dur!"msecs"(250));
        auto result = tryWait(ppid.pid);

        foreach(line; ppid.stdout.byLine)
            stdout.writeln(line);
        foreach(line; ppid.stderr.byLine)
            stderr.writeln(line);

        if (result.terminated)
            return result.status;
    }
}

