program cedast_loader;

uses dynlibs, classes, sysutils;

type

  TAstToken = NativeInt;

  {$Z1}
  TSerializationFormat = (json, pas);

  TScanFile     = function(filename: PChar): TAstToken; cdecl;
  TScanBuffer   = function(buffer: PByte; len: NativeUint): TAstToken; cdecl;
  TRescanFile   = procedure(tok: TAstToken); cdecl;
  TRescanBuffer = procedure(tok: TAstToken; buffer: PByte; len: NativeUint); cdecl;
  TUnleash      = procedure(tok: TAstToken); cdecl;

  TModuleName   = function(tok: TAstToken): PChar; cdecl;
  TSymbolList   = function(tok: TAstToken; var len: NativeUint ; fmt: TSerializationFormat): PByte; cdecl;



var
  dast: TLibHandle;
  scanfile: TScanFile;
  scanbuffer: TScanBuffer;
  rescanfile: TRescanFile;
  rescanbuffer: TRescanBuffer;
  unleash: TUnleash;
  moduleName: TModuleName;
  symlist: TSymbolList;
  tok: TAstToken;
  len: NativeUint = 0;
  ptr: PByte;

const
  testModule = 'module a.b.c.d.e.f.g.h; import std.stdio; uint a; struct F{long c;}';


begin

  dast := LoadLibrary('cedast');
  if dast = NilHandle then
    writeln('dast invalid handle')
  else begin
    scanfile := TScanFile(GetProcAddress(dast, 'scanFile'));
    if scanFile = nil then writeln('invalid scanfile proc ptr')
    else tok := scanfile(PChar('exception in call so ticket value is 0'));

    rescanfile := TRescanFile(GetProcAddress(dast, 'rescanFile'));
    if rescanfile = nil then writeln('invalid rescanFile proc ptr')
    else rescanfile(tok);

    scanbuffer := TScanBuffer(GetProcAddress(dast, 'scanBuffer'));
    if scanbuffer = nil then writeln('invalid scanBuffer proc ptr')
    else tok := scanbuffer(@testModule[1], length(testModule));

    rescanbuffer := TRescanBuffer(GetProcAddress(dast, 'rescanBuffer'));
    if rescanbuffer = nil then writeln('invalid rescanBuffer proc ptr')
    else rescanbuffer(tok, @testmodule[1], length(testModule));

    moduleName := TModuleName(GetProcAddress(dast, 'moduleName'));
    if moduleName = nil then writeln('invalid moduleName proc ptr')
    else if tok <> 0 then writeln(moduleName(tok));

    symlist := TSymbolList(GetProcAddress(dast, 'symbolList'));
    if symlist = nil then writeln('invalid symbolList proc ptr')
    else if tok <> 0 then with TMemoryStream.Create do try
      ptr := symlist(tok, len, TSerializationFormat.json);
      write(ptr^, len);
      SaveToFile('testsymlist.txt');
    finally
      free;
    end;

    unleash := TUnleash(GetProcAddress(dast, 'unleash'));
    if unleash = nil then writeln('invalid unleash proc ptr')
    else unleash(tok);

  end;

  readln;
end.

