program cedast_loader;

{$MODE OBJFPC}

uses dynlibs, classes, sysutils;

type

  TAstHandle = NativeInt;

  TAstNotification = procedure(param: pointer); cdecl;

  {$Z1}

  TSerializationFormat = (json, pas);

  TNewAst     = function(param: Pointer; clbck: TAstNotification): TAstHandle; cdecl;
  TDeleteAst  = procedure(tok: TAstHandle); cdecl;

  TScanFile   = procedure(tok: TAstHandle; filename: PChar); cdecl;
  TScanBuffer = procedure(tok: TAstHandle; buffer: PByte; len: NativeUint); cdecl;


  TModuleName = function(tok: TAstHandle): PChar; cdecl;
  TSymbolList = function(tok: TAstHandle; var len: NativeUint ; fmt: TSerializationFormat): PByte; cdecl;



var
  dast: TLibHandle;
  newAst: TNewAst;
  deleteast: TDeleteAst;
  scanFile: TScanFile;
  scanBuffer: TScanBuffer;
  moduleName: TModuleName;
  symbolList: TSymbolList;
  hdl: TAstHandle;
  len: NativeUint = 0;
  ptr: PByte;
  done: boolean;

const
  testModule = 'module a.b.c.d.e.f.g.h; import std.stdio; uint a; struct F{long c;}';

procedure notif(param: Pointer); cdecl;
begin
  done := true;
end;


begin

  dast := LoadLibrary('cedast');
  if dast = NilHandle then
    writeln('dast invalid handle')
  else begin

    newAst := TNewAst(GetProcAddress(dast, 'newAst'));
    if newAst = nil then writeln('invalid newAst proc ptr')
    else hdl := newAst(nil, @notif);

    scanFile := TScanFile(GetProcAddress(dast, 'scanFile'));
    if scanFile = nil then writeln('invalid scanfile proc ptr')
    else begin
      done := false;
      scanFile(hdl, PChar('exception in call'));
      while not done do sleep(20);
    end;

    scanBuffer := TScanBuffer(GetProcAddress(dast, 'scanBuffer'));
    if scanBuffer = nil then writeln('invalid scanBuffer proc ptr')
    else  begin
      done := false;
      scanBuffer(hdl, @testModule[1], length(testModule));
      while not done do sleep(20);
    end;

    moduleName := TModuleName(GetProcAddress(dast, 'moduleName'));
    if moduleName = nil then writeln('invalid moduleName proc ptr')
    else if hdl <> 0 then writeln(moduleName(hdl));

    symbolList := TSymbolList(GetProcAddress(dast, 'symbolList'));
    if symbolList = nil then writeln('invalid symbolList proc ptr')
    else if hdl <> 0 then with TMemoryStream.Create do try
      ptr := symbolList(hdl, len, TSerializationFormat.json);
      write(ptr^, len);
      SaveToFile('testsymlist.txt');
    finally
      free;
    end;

    deleteAst := TDeleteAst(GetProcAddress(dast, 'deleteAst'));
    if deleteAst = nil then writeln('invalid deleteAst proc ptr')
    else deleteAst(hdl);

  end;

  readln;
end.

