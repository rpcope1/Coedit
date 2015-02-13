unit ce_dcd;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, process, forms, strutils,
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  ce_common, ce_writableComponent, ce_interfaces, ce_observer, ce_synmemo, ce_project;

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
    //fPortNum: Word;
    fServerWasRunning: boolean;
    fClient, fServer: TProcess;
    fAvailable: boolean;
    fDoc: TCESynMemo;
    fProj: TCEProject;
    procedure killServer;
    procedure terminateClient;
    //
    procedure projNew(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure restartServer;
    procedure addImportFolder(const aFolder: string);
    procedure getComplAtCursor(aList: TStrings);
    procedure getDdocFromCursor(out aComment: string);
    procedure getDeclFromCursor(out aFilename: string; out aPosition: Integer);
    //
    property available: boolean read fAvailable;
  end;

var
  DcdWrapper: TCEDcdWrapper;

implementation

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEDcdWrapper.create(aOwner: TComponent);
const
  clientName = 'dcd-client' + exeExt;
  serverName = 'dcd-server' + exeExt;
begin
  inherited;
  //
  fAvailable := exeInSysPath(clientName) and exeInSysPath(serverName);
  if not fAvailable then
    exit;
  //
  fClient := TProcess.Create(self);
  fClient.Executable := clientName;
  fClient.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
  {$IFNDEF DEBUG}
  fClient.ShowWindow := swoHIDE;
  {$ENDIF}
  //
  fServerWasRunning := AppIsRunning((serverName));
  if not fServerWasRunning then begin
    fServer := TProcess.Create(self);
    fServer.Executable := serverName;
    fServer.Options := [{$IFDEF WINDOWS} poNewConsole{$ENDIF}];
    {$IFNDEF DEBUG}
    fServer.ShowWindow := swoHIDE;
    {$ENDIF}
  end;
  fTempLines := TStringList.Create;

  if (fServer <> nil) then
    fServer.Execute;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCEDcdWrapper.destroy;
begin
  EntitiesConnector.removeObserver(self);
  if fTempLines <> nil then
    fTempLines.Free;
  if fServer <> nil then begin
    if not fServerWasRunning then killServer;
    fServer.Free;
  end;
  fClient.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEDcdWrapper.projNew(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCEDcdWrapper.projChanged(aProject: TCEProject);
var
  i: Integer;
  fold: string;
begin
  if fProj <> aProject then
    exit;
  if fProj = nil then
    exit;
  //
  for i:= 0 to fProj.Sources.Count-1 do
    addImportFolder(extractFilePath(fProj.getAbsoluteSourceName(i)));
  for i := 0 to fProj.currentConfiguration.pathsOptions.importModulePaths.Count-1 do
  begin
    fold := fProj.currentConfiguration.pathsOptions.importModulePaths.Strings[i];
    if DirectoryExists(fold) then
      addImportFolder(fold);
  end;
end;

procedure TCEDcdWrapper.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  fProj := nil;
end;

procedure TCEDcdWrapper.projFocused(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCEDcdWrapper.projCompiling(aProject: TCEProject);
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
  //
  while fClient.Running do;
  fClient.Parameters.Clear;
  fClient.Parameters.Add('--shutdown');
  fClient.Execute;
  {$IFDEF LINUX}
  fClient.Terminate(0);
  fServer.Terminate(0);
  {$ENDIF}
end;

procedure TCEDcdWrapper.restartServer;
begin
  if not fAvailable then exit;
  //
end;

procedure TCEDcdWrapper.addImportFolder(const aFolder: string);
begin
  if not fAvailable then exit;
  //
  while fClient.Running do sleep(1);
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-I' + aFolder);
  fClient.Execute;
end;

procedure TCEDcdWrapper.getComplAtCursor(aList: TStrings);
var
  i, j: NativeInt;
  kind: Char;
  item: string;
  asComp, asTips: boolean;
begin
  if not fAvailable then exit;
  if fDoc = nil then exit;
  //
  fTempLines.Assign(fDoc.Lines);
  fTempLines.SaveToFile(fDoc.tempFilename);
  //
  terminateClient;
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Parameters.Add(fDoc.tempFilename);
  fClient.Execute;
  //
  fTempLines.LoadFromStream(fClient.Output);
  if fTempLines.Count = 0 then exit;
  //
  asComp := fTempLines.Strings[0] = 'identifiers';
  asTips := fTempLines.Strings[0] = 'calltips';
  if asTips then exit;
  //
  if asComp then j := 1 else j := 0;
  aList.Clear;
  for i := j to fTempLines.Count-1 do
  begin
    item := fTempLines.Strings[i];
    kind := item[length(item)];
    setLength(item, length(item)-2);
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
    end;
    aList.Add(item);
  end;
end;

procedure TCEDcdWrapper.getDdocFromCursor(out aComment: string);
var
  i: Integer;
begin
  if not fAvailable then exit;
  if fDoc = nil then exit;
  //
  fTempLines.Assign(fDoc.Lines);
  fTempLines.SaveToFile(fDoc.tempFilename);
  //
  terminateClient;
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-d');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.MouseStart -1));
  fClient.Parameters.Add(fDoc.tempFilename);
  fClient.Execute;
  //
  aComment := '';
  fTempLines.LoadFromStream(fClient.Output);
  for i := 0 to fTempLines.Count-1 do
    aComment += ReplaceStr(fTempLines.Strings[i], '\n', LineEnding);
end;

procedure TCEDcdWrapper.getDeclFromCursor(out aFilename: string; out aPosition: Integer);
var
   i: Integer;
   str, loc: string;
begin
  if not fAvailable then exit;
  if fDoc = nil then exit;
  //
  fTempLines.Assign(fDoc.Lines);
  fTempLines.SaveToFile(fDoc.tempFilename);
  //
  terminateClient;
  fClient.Parameters.Clear;
  fClient.Parameters.Add('-l');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Parameters.Add(fDoc.tempFilename);
  fClient.Execute;
  //
  str := 'a';
  setlength(str, 256);
  i := fClient.Output.Read(str[1], 256);
  setLength(str, i);
  if str <> '' then
  begin
    i := Pos(#9, str);
    if i = -1 then
      exit;
    loc := str[i+1..length(str)];
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
