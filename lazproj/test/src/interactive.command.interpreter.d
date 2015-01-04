module interactive.command.interpreter;

import std.stdio;
import std.array;

string[] arguments;
string command;
void function()[string] dispatcher;

static this(){
    dispatcher["hello"] = &hello;
    dispatcher["showargs"] = &showArgs;
}

void showArgs(){
    foreach(arg;arguments) writeln(arg);
}
void hello(){
    writeln("hello world");
}

void main(string args[])
{
    string input;    
    while (true)  
    {
        input = readln;
        if (input == "exit\n") return;
        // split command and args
        auto s = split(input);
        if (s.length){
            command = s[0];
            if (s.length > 1)
                arguments = s[1..$];
            else
                arguments.length = 0;   
            if (command in dispatcher)
                dispatcher[command]();
            else
                writeln("unrecognized command: ", command);
            stdout.flush;
        }
    }
}
