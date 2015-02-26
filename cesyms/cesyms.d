module cesyms;

import std.stdio, std.path, std.file, std.array, std.string;
import std.d.lexer, std.d.ast, std.d.parser;

enum SymbolType
{
    _alias,     // X
    _class,     // X
    _enum,      // X
    _function,  // X
    _interface, // X
    _import,    // X
    _mixin,
    _struct,    // X
    _template,  // X
    _union,     // X
    _variable   // X
}

struct Symbol
{
    int line;
    int col;
    string name;
    SymbolType type; 
    Symbol * [] subs;
    
    void serialize(ref Appender!string lfmApp)
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
        lfmApp.put("end\r");
    }
}

void main(string[] args)
{
    if (args.length < 2) return;
    auto fname = args[1];
    if (!fname.exists) return;

    // load and parse the file
    auto config = LexerConfig(fname, StringBehavior.source, WhitespaceBehavior.include);
    auto source = cast(ubyte[]) read(fname, size_t.max);
    auto scache = StringCache(StringCache.defaultBucketCount);
    auto ast = parseModule(getTokensForParser(source, config, &scache), fname);

    // visit each root member
    auto slb = new SymbolListBuilder; 
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
        print(&slb.root);
    }
    
    auto str = slb.serialize;
    //std.file.write(r"C:\tool.txt",cast(ubyte[])str);
    write(str);
}

class SymbolListBuilder : ASTVisitor
{
    Symbol root;
    Symbol * parent;
    size_t count;
    
    alias visit = ASTVisitor.visit;
    
    this(){resetRoot;}
    
    void resetRoot(){parent = &root;}
    
    string serialize()
    {
        Appender!string lfmApp;
        lfmApp.reserve(count * 64);
        
        lfmApp.put("object TSymbolList\rsymbols = <");
        foreach(Symbol * sym; root.subs) sym.serialize(lfmApp);
        lfmApp.put(">\rend\r\n");
        
        return lfmApp.data; 
    }
    
    /// returns a new symbol if the declarator is based on a Token named "name".
    Symbol * addDeclaration(DT)(DT adt)
    {
        static if 
        (
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
            auto result = new Symbol;
            result.name = adt.name.text;
            result.line = adt.name.line;
            result.col  = adt.name.column;             
            parent.subs ~= result;  
            return result;
        }
        
        assert(0, "addDeclaration no implemented for " ~ DT.stringof);
    }
    
    /// visitor implementation if the declarator is based on a Token named "name".
    void namedVisitorImpl(DT, SymbolType st, bool dig = true)(const(DT) dt)
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
    
    //TODO-cfeature: anonymous enum, accept visitor and add Symbol based on the 1st member name.

    final override void visit(const AliasDeclaration decl) 
    { 
        // old alias syntax not supported by this method
        // why is initializers an array ?
        if (decl.initializers.length == 1)
            namedVisitorImpl!(AliasInitializer, SymbolType._alias)(decl.initializers[0]);  
    }   
     
    final override void visit(const ClassDeclaration decl) 
    {
        namedVisitorImpl!(ClassDeclaration, SymbolType._class)(decl);
    }
    
    final override void visit(const EnumDeclaration decl) 
    {
        namedVisitorImpl!(EnumDeclaration, SymbolType._enum)(decl);
    }
    
    void visit(const EponymousTemplateDeclaration decl)
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
            count++;
            auto result = new Symbol;
            result.name = modules[0..$-1].join;
            result.line = si.identifierChain.identifiers[0].line;
            result.col  = si.identifierChain.identifiers[0].column; 
            result.type = SymbolType._import;            
            parent.subs ~= result;  
        }   
    }
    
    final override void visit(const MixinTemplateDeclaration decl) 
    {
        namedVisitorImpl!(TemplateDeclaration, SymbolType._template)(decl.templateDeclaration);  
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
}

