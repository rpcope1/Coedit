unit ce_dcd;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, process, forms, strutils,
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  ce_common, ce_writableComponent, ce_interfaces, ce_observer, ce_synmemo;

type


  (**
   * Wrap the dcd-server and dcd-client processes.
   *
   * Projects folders are automatically imported: ICEProjectObserver.
   * Completion, hints and declaration finder automatically work on the current
   *   document: ICEMultiDocObserver.
   *)
  TCEDcdWrapper = class(TWritableLfmTextComponent, ICEProjectObserver, ICEMultiDocObserver)
  private
    fTempLines: TStringList;
    fInputSource: string;
    fImportCache: TStringList;
    fPortNum: Word;
    fServerWasRunning: boolean;
    fClient, fServer: TProcess;
    fAvailable: boolean;
    fServerListening: boolean;
    fDoc: TCESynMemo;
    fProj: ICECommonProject;
    procedure killServer;
    procedure terminateClient; inline;
    procedure waitClient; inline;
    procedure updateServerlistening;
    procedure writeSourceToInput; inline;
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
  published
    property port: word read fPortNum write fPortNum default 0;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure addImportFolders(const folders: TStrings);
    procedure addImportFolder(const aFolder: string);
    procedure getComplAtCursor(aList: TStrings);
    procedure getCallTip(out tips: string);
    procedure getDdocFromCursor(out aComment: string);
    procedure getDeclFromCursor(out aFilename: string; out aPosition: Integer);
    //
    property available: boolean read fAvailable;
  end;

var
  DcdWrapper: TCEDcdWrapper;

implementation

const
  clientName = 'dcd-client' + exeExt;
  serverName = 'dcd-server' + exeExt;
  optsname = 'dcdoptions.txt';


function checkDcdSocket: boolean;
var
  str: string;
begin
  sleep(100);
  // nix/osx: the file might exists from a previous session that crashed
  // however the 100 ms might be enough for DCD to initializes
  {$IFDEF LINUX}
  str := sysutils.GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if (str + DirectorySeparator + 'dcd.socket').fileExists then
    exit(true);
  str := sysutils.GetEnvironmentVariable('UID');
  if ('/tmp/dcd-' + str + '.socket').fileExists then
    exit(true);
  {$ENDIF}
  {$IFDEF DARWIN}
  str := sysutils.GetEnvironmentVariable('UID');
  if ('/var/tmp/dcd-' + str + '.socket').fileExists then
    exit(true);
  {$ENDIF}
  //windows: just hope that the 100 ms were enough
  {$IFDEF WINDOWS}
  exit(true);
  {$ENDIF}
  exit(false);
end;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEDcdWrapper.create(aOwner: TComponent);
var
  fname: string;
  i: integer = 0;
begin
  inherited;
  //
  fname := getCoeditDocPath + optsname;
  if fname.fileExists then
    loadFromFile(fname);
  //
  fAvailable := exeInSysPath(clientName) and exeInSysPath(serverName);
  if not fAvailable then
    exit;
  //
  fClient := TProcess.Create(self);
  fClient.Executable := exeFullName(clientName);
  fClient.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
  fClient.ShowWindow := swoHIDE;
  //
  fServerWasRunning := AppIsRunning((serverName));
  if not fServerWasRunning then begin
    fServer := TProcess.Create(self);
    fServer.Executable := exeFullName(serverName);
    fServer.Options := [{$IFDEF WINDOWS} poNewConsole{$ENDIF}];
    {$IFNDEF DEBUG}
    fServer.ShowWindow := swoHIDE;
    {$ENDIF}
    if fPortNum <> 0 then
      fServer.Parameters.Add('-p' + intToStr(port));
  end;
  fTempLines := TStringList.Create;
  fImportCache := TStringList.Create;

  if fServer.isNotNil then
  begin
    fServer.Execute;
    while true do
    begin
      if (i = 10) or checkDcdSocket then
        break;
      i += 1;
    end;
  end;
  updateServerlistening;
  //
  EntitiesConnector.addObserver(self);
end;

procedure TCEDcdWrapper.updateServerlistening;
begin
  fServerListening := AppIsRunning((serverName));
end;

destructor TCEDcdWrapper.destroy;
begin
  saveToFile(getCoeditDocPath + optsname);
  EntitiesConnector.removeObserver(self);
  fImportCache.Free;
  if fTempLines.isNotNil then
    fTempLines.Free;
  if fServer.isNotNil then begin
    if not fServerWasRunning then
      killServer;
    fServer.Free;
  end;
  fClient.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEDcdWrapper.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEDcdWrapper.projChanged(aProject: ICECommonProject);
var
  i: Integer;
  fold: string;
  folds: TStringList;
begin
  if fProj <> aProject then
    exit;
  if fProj = nil then
    exit;
  //
  folds := TStringList.Create;
  try
  	for i:= 0 to fProj.sourcesCount-1 do
    begin
      fold := fProj.sourceAbsolute(i).extractFilePath;
      if folds.IndexOf(fold) = -1 then
        folds.Add(fold);
    end;
  	for i := 0 to fProj.importsPathCount-1 do
  	begin
    	fold := fProj.importPath(i);
      if fold.dirExists and (folds.IndexOf(fold) = -1) then
        folds.Add(fold);
    end;
    addImportFolders(folds);
  finally
    folds.Free;
  end;
end;

procedure TCEDcdWrapper.projClosing(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  fProj := nil;
end;

procedure TCEDcdWrapper.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEDcdWrapper.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCEDcdWrapper.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEDcdWrapper.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEDcdWrapper.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEDcdWrapper.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
end;

procedure TCEDcdWrapper.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION DCD things ------------------------------------------------------------}
procedure TCEDcdWrapper.terminateClient;
begin
  if fClient.Running then
    fClient.Terminate(0);
end;

procedure TCEDcdWrapper.killServer;
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  //
  fClient.Parameters.Clear;
  fClient.Parameters.Add('--shutdown');
  fClient.Execute;
  sleep(500);
end;

procedure TCEDcdWrapper.waitClient;
begin
  while fClient.Running do
    sleep(5);
end;

procedure TCEDcdWrapper.writeSourceToInput;
begin
  fInputSource := fDoc.Text;
  fClient.Input.Write(fInputSource[1], fInputSource.length);
  fClient.CloseInput;
end;

procedure TCEDcdWrapper.addImportFolder(const aFolder: string);
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  //
  if fImportCache.IndexOf(aFolder) <> -1 then exit;
  fImportCache.Add(aFolder);
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-I' + aFolder);
  fClient.Execute;
  waitClient;
end;

procedure TCEDcdWrapper.addImportFolders(const folders: TStrings);
var
  imp: string;
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  //
  fClient.Parameters.Clear;
  for imp in folders do
  begin
    if fImportCache.IndexOf(imp) <> -1 then
      continue;
    fImportCache.Add(imp);
    fClient.Parameters.Add('-I' + imp);
  end;
  if fClient.Parameters.Count <> 0 then
  begin
    fClient.Execute;
    waitClient;
  end;
end;

procedure TCEDcdWrapper.getCallTip(out tips: string);
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  if fDoc = nil then exit;
  //
  terminateClient;
  //
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Execute;
  writeSourceToInput;
  //
  fTempLines.LoadFromStream(fClient.Output);
  if fTempLines.Count = 0 then
  begin
    updateServerlistening;
    exit;
  end;
  if not (fTempLines[0] = 'calltips') then exit;
  //
  fTempLines.Delete(0);
  tips := fTempLines.Text;
  tips := tips[1..tips.length-1];
end;

procedure TCEDcdWrapper.getComplAtCursor(aList: TStrings);
var
  i: Integer;
  kind: Char;
  item: string;
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  if fDoc = nil then exit;
  //
  terminateClient;
  //
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Execute;
  writeSourceToInput;
  //
  fTempLines.LoadFromStream(fClient.Output);
  if fTempLines.Count = 0 then
  begin
    updateServerlistening;
    exit;
  end;
  if not (fTempLines[0] = 'identifiers') then exit;
  //
  aList.Clear;
  for i := 1 to fTempLines.Count-1 do
  begin
    item := fTempLines[i];
    kind := item[item.length];
    setLength(item, item.length-2);
    case kind of
      'c': item += ' (class)            ';
      'i': item += ' (interface)        ';
      's': item += ' (struct)           ';
      'u': item += ' (union)            ';
      'v': item += ' (variable)         ';
      'm': item += ' (member)           ';
      'k': item += ' (reserved word)    ';
      'f': item += ' (function)         ';
      'g': item += ' (enum)             ';
      'e': item += ' (enum member)      ';
      'P': item += ' (package)          ';
      'M': item += ' (module)           ';
      'a': item += ' (array)            ';
      'A': item += ' (associative array)';
      'l': item += ' (alias)            ';
      't': item += ' (template)         ';
      'T': item += ' (mixin)            ';
      // see https://github.com/Hackerpilot/dsymbol/blob/master/src/dsymbol/symbol.d#L47
      '*', '?': continue; // internal DCD stuff, said not to happen but actually it did
      // https://github.com/Hackerpilot/DCD/issues/261
    end;
    aList.Add(item);
  end;
end;

procedure TCEDcdWrapper.getDdocFromCursor(out aComment: string);
var
  i: Integer;
  str: string;
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  if fDoc = nil then exit;
  //
  i := fDoc.MouseStart;
  if i = 0 then exit;
  //
  terminateClient;
  //
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-d');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(i - 1));
  fClient.Execute;
  writeSourceToInput;
  //
  aComment := '';
  fTempLines.LoadFromStream(fClient.Output);
  if fTempLines.Count = 0 then
    updateServerlistening;
  for str in fTempLines do
    aComment += ReplaceStr(str, '\n', LineEnding);
  //
  aComment := ReplaceText(aComment, 'ditto' + LineEnding + LineEnding, '');
  aComment := ReplaceText(aComment, 'ditto', '');
  aComment := TrimLeft(aComment);
  aComment := TrimRight(aComment);
end;

procedure TCEDcdWrapper.getDeclFromCursor(out aFilename: string; out aPosition: Integer);
var
   i: Integer;
   str, loc: string;
begin
  if not fAvailable then exit;
  if not fServerListening then exit;
  if fDoc = nil then exit;
  //
  terminateClient;
  //
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-l');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Execute;
  writeSourceToInput;
  //
  setlength(str, 256);
  i := fClient.Output.Read(str[1], 256);
  if i = 0 then
    updateServerlistening;
  setLength(str, i);
  if str.isNotEmpty then
  begin
    i := Pos(#9, str);
    if i = -1 then
      exit;
    loc := str[i+1..str.length];
    aFilename := str[1..i-1];
    loc := ReplaceStr(loc, LineEnding, '');
    aPosition := strToIntDef(loc, -1);
  end;
end;
{$ENDREGION}

initialization
  DcdWrapper := TCEDcdWrapper.create(nil);
finalization
  DcdWrapper.Free;
end.
