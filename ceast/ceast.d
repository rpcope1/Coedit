module runnable;

import std.path, std.file;
import std.stdio;
import std.d.lexer;
import std.d.ast;
import std.d.parser;


interface I{}

class A
{
    class AA
    {
        class AA1{}
        class AA2{}
    }
    
    class BB
    {
        class BB1{}
        class BB2{}
    }
}

enum SymbolType
{
    _alias,
    _class,     // X
    _enum,      // X
    _function,  // X
    _interface, // X
    _import,
    _mixin,
    _struct,    // X
    _template,  // X
    _union,     // X
    _variable 
}

struct Symbol
{
    uint line;
    uint col;
    string name;
    SymbolType type; 
    Symbol * [] subs;
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
    
    int level = -1;
    void print(Symbol * s)
    {
        
        foreach(i; 0..level) write(".");
        level++;
        write(s.name, '\r');
        foreach(ss; s.subs)
            print(ss);
        
        level--;
    }
    
    print(&slb.root);
    writeln();
    
}

class SymbolListBuilder : ASTVisitor
{
    Symbol root;
    Symbol * parent;
    
    alias visit = ASTVisitor.visit;
    
    this(){resetRoot;}
    
    void resetRoot(){parent = &root;}
    
    /// returns a new symbol if the declarator is based on a Token named "name".
    Symbol * addDeclaration(DT)(DT adt)
    {
        static if 
        (
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
        auto previousParent = parent;
        parent = newSymbol;
        static if (dig) dt.accept(this);
        parent = previousParent;       
    }
    
    
    
    final override void visit(const AliasDeclaration aliasDeclaration) 
    { 
        // IdentifierList
    }
     
    final override void visit(const ClassDeclaration decl) 
    {
        namedVisitorImpl!(ClassDeclaration, SymbolType._class)(decl);
    }
    
    final override void visit(const EnumDeclaration decl) 
    {
        namedVisitorImpl!(EnumDeclaration, SymbolType._class)(decl);
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
        // singlesImp[]  
        
        // singleImport.identifierchain.join
        
        import std.array;
        foreach(const(SingleImport) si; decl.singleImports)
            
            writeln(  si.identifierChain.identifiers[0].text ); 
    }
    
    final override void visit(const MixinDeclaration decl) 
    {
        // identifier  
    }
    
    final override void visit(const StructDeclaration decl) 
    {
        namedVisitorImpl!(StructDeclaration, SymbolType._struct)(decl);   
    }
    
    final override void visit(const TemplateDeclaration decl) 
    {
        namedVisitorImpl!(TemplateDeclaration, SymbolType._function)(decl);    
    }
    
    final override void visit(const UnionDeclaration decl) 
    {
        namedVisitorImpl!(UnionDeclaration, SymbolType._function)(decl);    
    }
    
    final override void visit(const VariableDeclaration decl) 
    {
        foreach(elem; decl.declarators)
            namedVisitorImpl!(Declarator, SymbolType._variable, false)(elem);  
    }
}


