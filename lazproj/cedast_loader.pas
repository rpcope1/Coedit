program cedast_loader;

uses dynlibs, classes, sysutils;

type

  TAstToken = NativeInt;

  TScanFile   = function(filename: PChar): TAstToken; cdecl;
  TScanBuffer = function (buffer: PByte; len: NativeUint): TAstToken; cdecl;
  TRescan     = procedure (tok: TAstToken); cdecl;
  TUnleash    = procedure (tok: TAstToken); cdecl;
  TModuleName = function (tok: TAstToken): PChar; cdecl;

var
  dast: TLibHandle;
  scanfile: TScanFile;
  scanbuffer: TScanBuffer;
  rescan: TRescan;
  unleash: TUnleash;
  moduleName: TModuleName;
  tok: TAstToken;

const
  testModule = 'module a.b.c.d.e.f.g.h; import std.stdio;';


begin

  dast := LoadLibrary('cedast');
  if dast = NilHandle then
    writeln('dast invalid handle')
  else begin
    scanfile := TScanFile(GetProcAddress(dast, 'scanFile'));
    if scanFile = nil then writeln('invalid scanfile proc ptr')
    else tok := scanfile(PChar('exception in call so ticket value is 0'));

    rescan := TRescan(GetProcAddress(dast, 'rescan'));
    if rescan = nil then writeln('invalid rescan proc ptr')
    else rescan(tok);

    scanbuffer := TScanBuffer(GetProcAddress(dast, 'scanBuffer'));
    if scanbuffer = nil then writeln('invalid scanBuffer proc ptr')
    else tok := scanbuffer(@testModule[1], length(testModule));

    moduleName := TModuleName(GetProcAddress(dast, 'moduleName'));
    if moduleName = nil then writeln('invalid moduleName proc ptr')
    else if tok <> 0 then writeln(moduleName(tok));

    unleash := TUnleash(GetProcAddress(dast, 'unleash'));
    if unleash = nil then writeln('invalid unleash proc ptr')
    else unleash(tok);

  end;

  readln;
end.

