/*******************************************************************************
TODO source code analyzer for Coedit projects/files

Format: // TODO [fields] : content

- TODO: used to detect that the comment is a "TODO" comment. The keyword is not
case sensitive.

- fields: an optional list of property with a format similar to the execution argument 
of a program: -<char x><property for char x>-<char y><property for char y>.
possible fields include:
  - c: TODO category, e.g: -cserialization -cpersistence -cerrorhandling
  - a: TODO assignee, e.g: -aMisterFreeze -aMadameMichou -aJhonSmith
  - p: TODO priority, eg: -p8 -p0
  - s: TODO status, e.g -sPartiallyFixed, -sDone
  
- content: the literal message, e.g:  "set this property as const() to set it read only".

full examples:

// TODO: set this property as const() to set it read only.
// TODO-cfeature: save this property in the inifile.
// TODO-cannnotations-p8: annotates the member of the module as @safe and if possible nothrow.

- widget to tool IPC:

The widget call the tool with a list of file as argument and read the process
output on exit. the widget expects to find some TODO items in LFM format, according
to the classes declarations of TTodoItems (the collection container) and TTodoItem(the collection item)

********************************************************************************/
module cetodo;

// std
import std.stdio, std.getopt, std.string;
import std.array, std.conv, std.traits;
import std.file, std.path, std.range;
// libdparse
import std.d.ast, std.d.lexer, std.d.parser;


private struct TodoItem 
{
    private enum TodoField {file, line, text, category, assignee, priority, status}
    private string[TodoField] fFields;
     
    /**
     * Constructs a TODO item with its fields.
     * Params:
     * fname = the file where the item is located. mandatory.
     * line = the line where the item is located. mandatory.
     * text = the TODO content. mandatory.
     */
    @safe public this(string fname, string line, string text, string cat = "",  string ass = "", string prior = "", string status = "")
    {   
        // line and fname must really be valid
        if (!fname.exists) throw new Exception("TodoItem exception, the file name is invalid");
        try auto l = to!long(line);
        catch(Exception e) throw new Exception("TodoItem exception, the line number is invalid");
        
        // priority must be convertible to int
        if (prior.length) try auto i = to!long(prior);
        catch(Exception e) prior = "";
              
        fFields[TodoField.file]     = fname;
        fFields[TodoField.line]     = line;
        fFields[TodoField.text]     = text;
        fFields[TodoField.category] = cat;
        fFields[TodoField.assignee] = ass;
        fFields[TodoField.priority] = prior;
        fFields[TodoField.status]   = status;
    }
    
    /**
     * The item writes itself as a TCollectionItem.
     * Params:
     * LfmString = the string containing the LFM script.
     */
    @safe public void serialize(ref string LfmString)
    {
        LfmString  ~= "\t\titem\n";
        foreach(member; EnumMembers!TodoField)
            if (fFields[member].length)
                LfmString  ~= format("\t\t\t%s = '%s'\n", to!string(member), fFields[member]);   
        LfmString  ~= "\t\tend\n";
    }
}

private alias TodoItems = TodoItem * [];

/**
 * Application main procedure.
 * Params:
 * args = a list of files to analyze. 
 * Called each time a document is focused. Args is set using:
 * - the symbolic string <CFF> (current file is not in a project).
 * - the symbolic string <CPFS> (current file is in a project).
 */
void main(string[] args)
{
    string[] files = args[1..$];
    string LfmString;
    TodoItems todoItems;
   
    foreach(f; files)
    {
        if (!f.exists) continue;
        
        // loadand parse the file
        auto src = cast(ubyte[]) read(f, size_t.max);
        auto config = LexerConfig(f, StringBehavior.source);
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        auto lexer = DLexer(src, config, &cache);
        // analyze the tokens
        foreach(tok; lexer) token2TodoItem(tok, f, todoItems);                     
    }
    // serialize the items using the pascal component streaming text format
    foreach(todoItem; todoItems) todoItem.serialize(LfmString);
    // the widget has the TODOs in the output
    if (LfmString.length) writefln("object TTodoItems\n\titems = <\n%s>\nend", LfmString);
}

/// Try to transforms a Token into a a TODO item
@safe private void token2TodoItem(const(Token) atok, string fname,  ref TodoItems todoItems)
{
    if (atok.type != (tok!"comment")) 
        return;
    if (atok.text.length < 3) 
        return;
    if (atok.text[1] == '*' || atok.text[1] == '+' || atok.text[2] == '/')
        return; 
        
    auto text = atok.text[2..$];
    string identifier;
    bool isTodoComment;
    size_t pos;

    // "TODO" 
    while (true) {
        if (pos == text.length) break;  
        if (identifier.strip.toUpper == "TODO") {
            isTodoComment = true;
            text = text[pos..$];
            break;
        } 
        identifier ~= text[pos++]; 
    }
    if (!isTodoComment) return;
    
    //fields : content
    identifier = identifier.init;
    auto fc = text.split(':');
    if (fc.length < 2) return;
    auto raw_fields  = fc[0];
    auto raw_content = fc[1..$];
    
    // fields
    string a,c,p,s;
    foreach(field; raw_fields.split('-')) {
        if (field.length < 2) continue;
        switch (field[0]) {
            default: break;
            case 'a': case 'A': 
                a = field[1..$].strip; 
                break;
            case 'c': case 'C': 
                c = field[1..$].strip; 
                break;
            case 'p': case 'P': 
                p = field[1..$].strip; 
                break;
            case 's': case 'S': 
                s = field[1..$].strip; 
                break;
        }
    }
    
    // content, must exists.
    string content = raw_content.join.strip;
    if (!content.length) return;
    
    // item
    todoItems ~= new TodoItem(fname, to!string(atok.line), content, c, a, p, s);
}

// samples for testing the program as a runnable module with <CFF>

// TODO-cINVALID_because_no_content:              
// TODO: set this property as const() to set it read only.
// TODO-cfeature: save this property in the inifile.
// TODO-cannnotations-p8: annotates the member of the module as @safe and if possible nothrow.

