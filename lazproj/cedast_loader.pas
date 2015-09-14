program cedast_loader;

uses dynlibs, classes, sysutils;

type

  TAstToken = NativeInt;

  TScanFile   = function(filename: PChar): TAstToken; cdecl;
  TScanBuffer = function (buffer: PByte; len: NativeUint): TAstToken; cdecl;
  TRescan     = procedure (tok: TAstToken); cdecl;
  TUnleash    = procedure (tok: TAstToken); cdecl;

var
  dast: TLibHandle;
  scanfile: TScanFile;
  scanbuffer: TScanBuffer;
  rescan: TRescan;
  unleash: TUnleash;
  tok: TAstToken;


begin

  dast := LoadLibrary('cedast.dll');
  if dast = NilHandle then
    writeln('dast invalid handle')
  else begin
    scanfile := TScanFile(GetProcAddress(dast, 'scanFile'));
    if scanFile = nil then writeln('invalid scanfile proc ptr')
    else tok := scanfile(PChar('blah'));

    scanbuffer := TScanBuffer(GetProcAddress(dast, 'scanBuffer'));
    if scanbuffer = nil then writeln('invalid scanBuffer proc ptr')
    else scanbuffer(nil, 0);

    rescan := TRescan(GetProcAddress(dast, 'rescan'));
    if rescan = nil then writeln('invalid rescan proc ptr')
    else rescan(tok);

    unleash := TUnleash(GetProcAddress(dast, 'unleash'));
    if unleash = nil then writeln('invalid unleash proc ptr')
    else unleash(tok);
  end;

  readln;
end.

