/**
Usage
=====

- In Coedit: 
  the program must be located somewhere in the PATH.

- Elsewhere:
  invoke with `[-j] [<filename>]`.
  - `-j`: optional, if set then the program outputs the list (in stdout) in JSON 
     otherwise in Pascal streaming text format.
  - `<filename>`: optional, the D module filename, if not set then the program 
    reads the module from stdin.
  - see the source for more information about how to use the output. 
    It's basically a tree of struct with 3 members: symbol type, name and location.
  
- Test in CE as a runnable module:
  click `Compile file and run ...` and type either `<CFF>` or `-j <CFF>` in the 
  input query dialog. Note that this will only work if libdparse is setup in the 
  library manager.
  
*/
module cesyms;

import std.stdio, std.path, std.file, std.array, std.string;
import std.getopt, std.json, std.conv;
import dparse.lexer, dparse.ast, dparse.parser;
import std.traits;

enum ListFmt
{
    Pascal,
    JSON
}

void main(string[] args)
{
    // format
    bool asJson;
    getopt(args, std.getopt.config.passThrough,'j', &asJson); 
    
    // get either the module from stdin or from first arg
    string fname;
    ubyte[] source;
    if (args.length == 1)
    {
        foreach(buff; stdin.byChunk(1024))
            source ~= buff;
    }
    else if (args.length == 2)
    {
        fname = args[$-1];
        if (!fname.exists) return;
        source = cast(ubyte[]) read(fname, size_t.max);
    }
    else return;

    // load and parse the file
    auto config = LexerConfig(fname, StringBehavior.source, WhitespaceBehavior.skip);
    auto scache = StringCache(StringCache.defaultBucketCount);
    auto ast = parseModule(getTokensForParser(source, config, &scache), fname, null, &(SymbolListBuilder.astError));

    // visit each root member
    SymbolListBuilder slb = construct!SymbolListBuilder;
    foreach(Declaration decl; ast.declarations)
    {
        slb.resetRoot;
        slb.visit(decl);
    }
    
    version(none)
    {
        int level = -1;
        void print(Symbol * s)
        {
            foreach(i; 0 .. level) write(".");
            level++;
            write(s.name, '\r');
            foreach(ss; s.subs)
                print(ss);
            
            level--;
        }
        print(slb.root);
    } 
    else 
    {
        if (asJson) write(slb.serializeJson);
        else write(slb.serializePascal);
    }      

    slb.destruct;
}

// libdparse warnings includes some "'", which in Pascal are string delim
string patchPasStringLitteral(const ref string p)
{
    string result;
    for (auto i = 0; i < p.length; i++)
    {    
        auto curr = p[i];
        if (curr == 0)
            break;
        else if (curr == 13 || curr == 10)
            result ~= ' ';
        else if (curr == '\'') 
            result ~= "'#39'";
        else  
            result ~= curr; 
    }
    return result;
}

// Memory utils ---------------------------------------------------------------+
void * getMem(size_t size) nothrow
{
    import std.c.stdlib;
    auto result = malloc(size);
    assert(result, "Out of memory");
    return result;
}

CT construct(CT, A...)(A a) 
if (is(CT == class))
{
    import std.conv : emplace;
    auto size = __traits(classInstanceSize, CT);
    auto memory = getMem(size)[0 .. size];
    return emplace!(CT, A)(memory, a);
}

ST * construct(ST, A...)(A a)
if(is(ST==struct))
{
    import std.conv : emplace;
    auto size = ST.sizeof;
    auto memory = getMem(size)[0 .. size];
    return emplace!(ST, A)(memory, a);
}

void destruct(T)(ref T instance) 
if (is(T == class) || (isPointer!T && is(PointerTarget!T == struct)))
{
    if (!instance) return;
    destroy(instance);
    instance = null;
}
//----

// Serializable Symbol --------------------------------------------------------+
enum SymbolType
{
    _alias,
    _class, 
    _enum, 
    _error,   
    _function, 
    _interface, 
    _import,   
    _mixin,   // (template decl)
    _struct, 
    _template,
    _union,
    _variable,
    _warning
}

struct Symbol
{
    size_t line;
    size_t col;
    string name;
    SymbolType type; 
    Symbol * [] subs;
    
    ~this()
    {
        foreach_reverse(i; 0..subs.length)
            subs[i].destruct;
    }
    
    void serialize(List)(auto ref List list)
    {
        static if (is(List == Appender!string))
            serializePascal(list);
        else static if (is(List == JSONValue))
            serializeJson(list);
        else static assert(0, "serialization kind cannot be deduced from list");
    }
    
    void serializePascal(ref Appender!string lfmApp)
    {
        lfmApp.put("\ritem\r");
        
        lfmApp.put(format("line = %d\r", line));
        lfmApp.put(format("col = %d\r", col));
        lfmApp.put(format("name = '%s'\r", name));
        lfmApp.put(format("symType = %s\r", type));
         
        lfmApp.put("subs = <");
        if (subs.length) foreach(Symbol * sub; subs)
            sub.serialize(lfmApp);
        lfmApp.put(">\r");
        lfmApp.put("end");
    }
    
    void serializeJson(ref JSONValue json)
    {
        auto vobj = parseJSON("{}");
        vobj["line"]= JSONValue(line);
        vobj["col"] = JSONValue(col);
        vobj["name"]= JSONValue(name);
        vobj["type"]= JSONValue(to!string(type));
        if (subs.length)
        {
            auto vsubs = parseJSON("[]");
            foreach(Symbol * sub; subs)
                sub.serializeJson(vsubs);
            vobj["items"] = vsubs;
        }
        json.array ~= vobj;
    }
}
//----

// AST visitor/Symbol list ----------------------------------------------------+
class SymbolListBuilder : ASTVisitor
{
    Symbol * root;
    Symbol * parent;
    
    // for some reason (?) the .name of a (static Symbol* []) item was lost 
    __gshared static Symbol[] illFormed;
    
    size_t count;
    
    alias visit = ASTVisitor.visit;
    
    this()
    {
        root = construct!Symbol;
        resetRoot;
    }
    
    ~this()
    {
        root.destruct;  
    }
    
    static void astError(string fname, size_t line, size_t col, string msg, bool isErr)
    {
        Symbol * newSym = construct!Symbol;
        newSym.col = col;
        newSym.line = line;
        newSym.name = patchPasStringLitteral(msg);
        newSym.type = isErr ? SymbolType._error : SymbolType._warning; 
        illFormed ~= * newSym;
    }
    
    final void resetRoot(){parent = root;}
    
    final string serializePascal()
    {
        Appender!string lfmApp;
        lfmApp.reserve(count * 64);
        
        lfmApp.put("object TSymbolList\rsymbols = <");
        foreach(sym; illFormed) sym.serialize(lfmApp);
        foreach(sym; root.subs) sym.serialize(lfmApp);
        lfmApp.put(">\rend\r\n");
        
        return lfmApp.data; 
    }
    
    final string serializeJson()
    {
        JSONValue result = parseJSON("{}");
        JSONValue vsubs = parseJSON("[]");
        foreach(sym; illFormed) sym.serialize(vsubs);
        foreach(sym; root.subs) sym.serialize(vsubs);
        result["items"] = vsubs;
        version(assert) return result.toPrettyString;
        // else: release mode
        else return result.toString;
    }    
    
    /// returns a new symbol if the declarator is based on a Token named "name".
    final Symbol * addDeclaration(DT)(DT adt)
    {
        static if
        (
            is(DT == const(EponymousTemplateDeclaration)) ||
            is(DT == const(AnonymousEnumMember))    ||
            is(DT == const(AliasInitializer))       ||
            is(DT == const(ClassDeclaration))       ||
            is(DT == const(Declarator))             ||
            is(DT == const(EnumDeclaration))        ||
            is(DT == const(FunctionDeclaration))    ||
            is(DT == const(InterfaceDeclaration))   ||
            is(DT == const(StructDeclaration))      ||
            is(DT == const(TemplateDeclaration))    ||
            is(DT == const(UnionDeclaration))
            
        )
        {
            count++;
            auto result = construct!Symbol;
            result.name = adt.name.text;
            result.line = adt.name.line;
            result.col  = adt.name.column;
            parent.subs ~= result;
            return result;
        }
        
        version(none) assert(0, "addDeclaration no implemented for " ~ DT.stringof);
    }
    
    /// visitor implementation if the declarator is based on a Token named "name".
    final void namedVisitorImpl(DT, SymbolType st, bool dig = true)(const(DT) dt)
    {
        auto newSymbol = addDeclaration(dt);
        newSymbol.type = st;
        //
        static if (dig)
        {
            auto previousParent = parent;
            scope(exit) parent = previousParent;
            parent = newSymbol;
            dt.accept(this);
        }
    }

    /// visitor implementation for special cases.
    final void otherVisitorImpl(SymbolType st, string name, size_t line, size_t col)
    {
        count++;
        auto result = construct!Symbol;
        result.name = name;
        result.line = line;
        result.col  = col; 
        result.type = st;            
        parent.subs ~= result;     
    }

    final override void visit(const AliasDeclaration decl) 
    { 
        // why is initializers an array ?
        if (decl.initializers.length > 0)
            namedVisitorImpl!(AliasInitializer, SymbolType._alias)(decl.initializers[0]);  
    }   
    
    final override void visit(const AnonymousEnumDeclaration decl) 
    {
        if (decl.members.length) foreach(mem; decl.members)
            namedVisitorImpl!(AnonymousEnumMember, SymbolType._enum)(mem);
    }
     
    final override void visit(const ClassDeclaration decl) 
    {
        namedVisitorImpl!(ClassDeclaration, SymbolType._class)(decl);
    }
    
    final override void visit(const Constructor decl) 
    {
        otherVisitorImpl(SymbolType._function, "this", decl.line, decl.column); 
    }
    
    final override void visit(const Destructor decl) 
    {
        otherVisitorImpl(SymbolType._function, "~this", decl.line, decl.column); 
    }
    
    final override void visit(const EnumDeclaration decl) 
    {
        namedVisitorImpl!(EnumDeclaration, SymbolType._enum)(decl);
    }
    
    final override void visit(const EponymousTemplateDeclaration decl)
    {
        namedVisitorImpl!(EponymousTemplateDeclaration, SymbolType._template)(decl);
    }
    
    final override void visit(const FunctionDeclaration decl) 
    {
        namedVisitorImpl!(FunctionDeclaration, SymbolType._function)(decl);    
    }
    
    final override void visit(const InterfaceDeclaration decl) 
    {
        namedVisitorImpl!(InterfaceDeclaration, SymbolType._interface)(decl);    
    }
    
    final override void visit(const ImportDeclaration decl) 
    {
        foreach(const(SingleImport) si; decl.singleImports)
        { 
            if (!si.identifierChain.identifiers.length)
                continue;
            //
            string[] modules;
            foreach(ident; si.identifierChain.identifiers)
            {
                modules ~= ident.text;
                modules ~= ".";
            }
            //
            otherVisitorImpl(SymbolType._import, modules[0..$-1].join, 
                si.identifierChain.identifiers[0].line,
                si.identifierChain.identifiers[0].column
            ); 
        }   
    }
    
    final override void visit(const MixinTemplateDeclaration decl) 
    {
        namedVisitorImpl!(TemplateDeclaration, SymbolType._mixin)(decl.templateDeclaration);  
    }   
    
    final override void visit(const StructDeclaration decl) 
    {
        namedVisitorImpl!(StructDeclaration, SymbolType._struct)(decl);   
    }
    
    final override void visit(const TemplateDeclaration decl) 
    {
        namedVisitorImpl!(TemplateDeclaration, SymbolType._template)(decl);    
    }
    
    final override void visit(const UnionDeclaration decl) 
    {
        namedVisitorImpl!(UnionDeclaration, SymbolType._union)(decl);    
    }
    
    final override void visit(const VariableDeclaration decl) 
    {
        foreach(elem; decl.declarators)
            namedVisitorImpl!(Declarator, SymbolType._variable, false)(elem);  
    }
    
    final override void visit(const StaticConstructor decl) 
    {
        otherVisitorImpl(SymbolType._function, "static this", decl.line, decl.column); 
    }
    
    final override void visit(const StaticDestructor decl) 
    {
        otherVisitorImpl(SymbolType._function, "static ~this", decl.line, decl.column); 
    }
}
//----
