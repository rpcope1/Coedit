unit ce_common;

{$I ce_defines.inc}

interface

uses

  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Windows, JwaTlHelp32,
  {$ENDIF}
  {$IFDEF LINUX}
  ExtCtrls, FileUtil,
  {$ENDIF}
  dialogs, forms, process, asyncprocess;

const

  DdiagFilter = 'D source|*.d|D interface|*.di|All files|*.*';
  exeExt = {$IFDEF WINDOWS} '.exe' {$ELSE} ''   {$ENDIF};
  objExt = {$IFDEF WINDOWS} '.obj' {$ELSE} '.o' {$ENDIF};
  libExt = {$IFDEF WINDOWS} '.lib' {$ELSE} '.a' {$ENDIF};
  dynExt = {$IFDEF WINDOWS} '.dll' {$ENDIF} {$IFDEF LINUX}'.so'{$ENDIF} {$IFDEF DARWIN}'.dylib'{$ENDIF};

var
  DCompiler: string = 'dmd';

type

  (**
   * Workaround for a TAsyncProcess Linux issue: OnTerminate event not called.
   * An idle timer is started when executing and trigs the event if necessary.
   *)
  TCheckedAsyncProcess = class(TAsyncProcess)
  {$IFDEF LINUX}
  private
    fTimer: TIdleTimer;
    procedure checkTerminated(sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure Execute; override;
  {$ENDIF}
  end;

  (**
   *  TProcess with assign() 'overriden'.
   *)
  TProcessEx = class helper for TProcess
  public
    procedure Assign(aValue: TPersistent);
  end;

  (**
   * CollectionItem used to store a shortcut.
   *)
  TCEPersistentShortcut = class(TCollectionItem)
  private
    fShortcut: TShortCut;
    fActionName: string;
  published
    property shortcut: TShortCut read fShortcut write fShortcut;
    property actionName: string read fActionName write fActionName;
  public
    procedure assign(aValue: TPersistent); override;
  end;

  (**
   * Save a component with a readable aspect.
   *)
  procedure saveCompToTxtFile(const aComp: TComponent; const aFilename: string);

  (**
   * Load a component.
   *)
  procedure loadCompFromTxtFile(const aComp: TComponent; const aFilename: string;
    aPropNotFoundHandler: TPropertyNotFoundEvent = nil; anErrorHandler: TReaderError = nil);

  (**
   * Converts a relative path to an absolute path.
   *)
  function expandFilenameEx(const aBasePath, aFilename: string): string;

  (**
   * Patches the directory separators from a string.
   * This is used to ensure that a project saved on a platform can be loaded
   * on another one.
   *)
  function patchPlateformPath(const aPath: string): string;
  procedure patchPlateformPaths(const sPaths: TStrings);

  (**
   * Patches the file extension from a string.
   * This is used to ensure that a project saved on a platform can be loaded
   * on another one. Note that the ext which are handled are specific to coedit projects.
   *)
  function patchPlateformExt(const aFilename: string): string;

  (**
   * Returns aFilename without its extension.
   *)
  function stripFileExt(const aFilename: string): string;

  (**
   * Ok/Cancel modal dialog
   *)
  function dlgOkCancel(const aMsg: string): TModalResult;

  (**
   * Info message
   *)
  function dlgOkInfo(const aMsg: string): TModalResult;

  (**
   * Error message
   *)
  function dlgOkError(const aMsg: string): TModalResult;

  (**
   * Returns an unique object identifier, based on its heap address.
   *)
  function uniqueObjStr(const aObject: TObject): string;

  (**
   * Reduces a filename if its length is over the threshold defined by charThresh.
   * Even if the result is not usable anymore, it avoids any "visually-overloaded" MRU menu.
   *)
  function shortenPath(const aPath: string; charThresh: Word = 60): string;

  (**
   * Returns the user data dir.
   *)
  function getUserDataPath: string;

  (**
   * Returns the documents and settings folder for Coedit.
   *)
  function getCoeditDocPath: string;

  (**
   * Fills aList with the names of the files located in aPath.
   *)
  procedure listFiles(aList: TStrings; const aPath: string; recursive: boolean = false);

  (**
   * Fills aList with the names of the folders located in aPath.
   *)
  procedure listFolders(aList: TStrings; const aPath: string);

  (**
   * Checks if aPath contains at least one sub-folder.
   *)
  function hasFolder(const aPath: string): boolean;

  (**
   * Fills aList with the system drives.
   *)
  procedure listDrives(aList: TStrings);

  (**
   * If aPath ends with an asterisk then fills aList with the names of the files located in aPath.
   * Returns true if aPath was 'asterisk-ifyed'.
   *)
  function listAsteriskPath(const aPath: string; aList: TStrings; someExts: TStrings = nil): boolean;

  (**
   * Lets the shell open a file
   *)
  function shellOpen(const aFilename: string): boolean;

  (**
   * Returns true if anExeName can be spawn without its full path.
   *)
  function exeInSysPath(anExeName: string): boolean;

  (**
   * Returns the full to anExeName. Works if exeInSysPath().
   *)
  function exeFullName(anExeName: string): string;

  (**
   * Clears then fills aList with aProcess output stream.
   *)
  procedure processOutputToStrings(aProcess: TProcess; var aList: TStringList);

  (**
   * Copy available process output to a stream.
   *)
  procedure processOutputToStream(aProcess: TProcess; output: TMemoryStream);

  (**
   * Terminates and frees aProcess.
   *)
  procedure killProcess(var aProcess: TAsyncProcess);

  procedure killProcess(var aProcess: TCheckedAsyncProcess);

  (**
   * Ensures that the i/o process pipes are not redirected if it waits on exit.
   *)
  procedure ensureNoPipeIfWait(aProcess: TProcess);

  (**
   * Returns true if Exename is running under Windows or Linux
   *)
  function AppIsRunning(const ExeName: string):Boolean;

  (**
   * Returns the length of the line ending in aFilename;
   *)
  function getLineEndingLength(const aFilename: string): byte;

  function getSysLineEndLen: byte;

  (**
   * Returns the common folder of the file names stored in aList
   *)
  function commonFolder(const someFiles: TStringList): string;

  (**
   * Returns true if ext matches a file extension whose type is highlightable
   *)
  function hasDlangSyntax(const ext: string): boolean;

  (**
   * Returns true if ext matches a file extension whose type can be passed as source.
   *)
  function isDlangCompilable(const ext: string): boolean;

  (**
   * Returns true if ext matches a file extension whose type is editable in Coedit
   *)
  function isEditable(const ext: string): boolean;


implementation

{$IFDEF LINUX}
var
  // flag that indicates that migration is to be done on first call to getCoeditData...
  doneLinuxDataMigration: boolean = false;

procedure MigrateOldData;
var
  oldLocation: string;
  newLocation: string;
  err: boolean;
begin
  err := false;
  doneLinuxDataMigration := true;
  oldLocation := sysutils.GetEnvironmentVariable('HOME') +'/Coedit';
  if not DirectoryExists(oldLocation) then exit;
  newLocation := getUserDataPath + 'Coedit';
  try
    try
      CopyDirTree(oldLocation, newLocation,
        [cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime]);
    except
      err := true;
    end;
  finally
    if not err then
      FileUtil.DeleteDirectory(oldLocation, false);
  end;
end;
{$ENDIF}

procedure TCEPersistentShortcut.assign(aValue: TPersistent);
var
  src: TCEPersistentShortcut;
begin
  if aValue is TCEPersistentShortcut then
  begin
    src := TCEPersistentShortcut(Avalue);
    fActionName := src.fActionName;
    fShortcut := src.fShortcut;
  end
  else inherited;
end;

{$IFDEF LINUX}
constructor TCheckedAsyncProcess.Create(aOwner: TComponent);
begin
  inherited;
  fTimer := TIdleTimer.Create(self);
  fTimer.Enabled := false;
  fTimer.Interval := 50;
  fTimer.AutoEnabled := false;
end;

procedure TCheckedAsyncProcess.Execute;
begin
  if OnTerminate <> nil then
    fTimer.Enabled :=true;
  fTimer.OnTimer := @checkTerminated;
  inherited;
end;

procedure TCheckedAsyncProcess.checkTerminated(sender: TObject);
begin
  if Running then exit;
  if OnTerminate = nil then exit;
  fTimer.Enabled:=false;
  OnTerminate(Self);
end;
{$ENDIF}

procedure TProcessEx.Assign(aValue: TPersistent);
var
  src: TProcess;
begin
  if aValue is TProcess then
  begin
    src := TProcess(aValue);
    PipeBufferSize := src.PipeBufferSize;
    Active := src.Active;
    Executable := src.Executable;
    Parameters := src.Parameters;
    ConsoleTitle := src.ConsoleTitle;
    CurrentDirectory := src.CurrentDirectory;
    Desktop := src.Desktop;
    Environment := src.Environment;
    Options := src.Options;
    Priority := src.Priority;
    StartupOptions := src.StartupOptions;
    ShowWindow := src.ShowWindow;
    WindowColumns := src.WindowColumns;
    WindowHeight := src.WindowHeight;
    WindowLeft := src.WindowLeft;
    WindowRows := src.WindowRows;
    WindowTop := src.WindowTop;
    WindowWidth := src.WindowWidth;
    FillAttribute := src.FillAttribute;
    XTermProgram := src.XTermProgram;
  end
  else inherited;
end;

procedure saveCompToTxtFile(const aComp: TComponent; const aFilename: string);
var
  str1, str2: TMemoryStream;
begin
  str1 := TMemoryStream.Create;
  str2 := TMemoryStream.Create;
  try
    str1.WriteComponent(aComp);
    str1.Position := 0;
    ObjectBinaryToText(str1,str2);
    ForceDirectories(ExtractFilePath(aFilename));
    str2.SaveToFile(aFilename);
  finally
    str1.Free;
    str2.Free;
  end;
end;

procedure loadCompFromTxtFile(const aComp: TComponent; const aFilename: string;
  aPropNotFoundHandler: TPropertyNotFoundEvent = nil; anErrorHandler: TReaderError = nil);
var
  str1, str2: TMemoryStream;
  rdr: TReader;
begin
  str1 := TMemoryStream.Create;
  str2 := TMemoryStream.Create;
  try
    str1.LoadFromFile(aFilename);
    str1.Position := 0;
    ObjectTextToBinary(str1, str2);
    str2.Position := 0;
    try
      rdr := TReader.Create(str2, 4096);
      try
        rdr.OnPropertyNotFound := aPropNotFoundHandler;
        rdr.OnError := anErrorHandler;
        rdr.ReadRootComponent(aComp);
      finally
        rdr.Free;
      end;
    except
    end;
  finally
    str1.Free;
    str2.Free;
  end;
end;

function expandFilenameEx(const aBasePath, aFilename: string): string;
var
  curr: string;
begin
  curr := '';
  getDir(0, curr);
  try
    if curr <> aBasePath then
      chDir(aBasePath);
    result := expandFileName(aFilename);
  finally
    chDir(curr);
  end;
end;

function patchPlateformPath(const aPath: string): string;
function patchProc(const src: string; const invalid: char): string;
var
  i: Integer;
  dir: string;
begin
  dir := ExtractFileDrive(src);
  if length(dir) > 0 then
    result := src[length(dir)+1..length(src)]
  else
    result := src;
  i := pos(invalid, result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos(invalid,result);
    until
      i = 0;
  end;
  result := dir + result;
end;
begin
  result := aPath;
  {$IFDEF MSWINDOWS}
  result := patchProc(result, '/');
  {$ENDIF}
  {$IFDEF UNIX}
  result := patchProc(result, '\');
  {$ENDIF}
  {$IFDEF DARWIN}
  result := patchProc(result, '\');
  {$ENDIF}
end;

procedure patchPlateformPaths(const sPaths: TStrings);
var
  i: Integer;
  str: string;
begin
  for i:= 0 to sPaths.Count-1 do
  begin
    str := sPaths.Strings[i];
    sPaths.Strings[i] := patchPlateformPath(str);
  end;
end;

function patchPlateformExt(const aFilename: string): string;
var
  ext, newext: string;
begin
  ext := extractFileExt(aFilename);
  newext := '';
  {$IFDEF MSWINDOWS}
  case ext of
    '.so':    newext := '.dll';
    '.dylib': newext := '.dll';
    '.a':     newext := '.lib';
    '.o':     newext := '.obj';
    else      newext := ext;
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  case ext of
    '.dll':   newext := '.so';
    '.dylib': newext := '.so';
    '.lib':   newext := '.a';
    '.obj':   newext := '.o';
    '.exe':   newext := '';
    else      newext := ext;
  end;
  {$ENDIF}
  {$IFDEF DARWIN}
  case ext of
    '.dll': newext := '.dylib';
    '.so':  newext := '.dylib';
    '.lib': newext := '.a';
    '.obj': newext := '.o';
    '.exe': newext := '';
    else    newext := ext;
  end;
  {$ENDIF}
  result := ChangeFileExt(aFilename, newext);
end;

function stripFileExt(const aFilename: string): string;
begin
  if Pos('.', aFilename) > 1 then
    exit(ChangeFileExt(aFilename, ''))
  else
    exit(aFilename);
end;

function dlgOkCancel(const aMsg: string): TModalResult;
const
  Btns = [mbOK,mbCancel];
begin
  exit( MessageDlg('Coedit', aMsg, mtConfirmation, Btns, ''));
end;

function dlgOkInfo(const aMsg: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', aMsg, mtInformation, Btns, ''));
end;

function dlgOkError(const aMsg: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', aMsg, mtError, Btns, ''));
end;

function uniqueObjStr(const aObject: Tobject): string;
begin
  {$HINTS OFF}{$WARNINGS OFF}
  exit( format('%.8X',[NativeUint(aObject)]));
  {$HINTS ON}{$WARNINGS ON}
end;

function shortenPath(const aPath: string; charThresh: Word = 60): string;
var
  i: NativeInt;
  sepCnt: NativeInt;
  drv: string;
  pth1: string;
begin
  sepCnt := 0;
  if length(aPath) <= charThresh then
    exit(aPath);

  drv := extractFileDrive(aPath);
  i := length(aPath);
  while(i <> length(drv)+1) do
  begin
    Inc(sepCnt, Byte(aPath[i] = directorySeparator));
    if sepCnt = 2 then
      break;
    Dec(i);
  end;
  pth1 := aPath[i..length(aPath)];
  exit( format('%s%s...%s',[drv,directorySeparator,pth1]) );
end;

function getUserDataPath: string;
begin
  {$IFDEF WINDOWS}
  result := sysutils.GetEnvironmentVariable('APPDATA');
  {$ENDIF}
  {$IFDEF LINUX}
  result := sysutils.GetEnvironmentVariable('HOME') + '/.config';
  {$ENDIF}
  {$IFDEF DARWIN}
  result := sysutils.GetEnvironmentVariable('HOME') + '/Library';
  {$ENDIF}
  if not DirectoryExists(result) then
    raise Exception.Create('Coedit failed to retrieve the user data folder');
  if result[length(result)] <> DirectorySeparator then
    result += directorySeparator;
end;

function getCoeditDocPath: string;
begin
  {$IFDEF LINUX}
  if not doneLinuxDataMigration then
    MigrateOldData;
  {$ENDIF}
  result := getUserDataPath + 'Coedit' + directorySeparator;
end;

function isFolder(sr: TSearchRec): boolean;
begin
  result := (sr.Name <> '.') and  (sr.Name <> '..' ) and  (sr.Name <> '' ) and
    (sr.Attr and faDirectory = faDirectory);
end;

procedure listFiles(aList: TStrings; const aPath: string; recursive: boolean = false);
var
  sr: TSearchrec;
procedure tryAdd;
begin
  if sr.Attr and faDirectory <> faDirectory then
    aList.Add(aPath+ directorySeparator + sr.Name);
end;
begin
  if findFirst(aPath + directorySeparator + '*', faAnyFile, sr) = 0 then
  try
    repeat
      tryAdd;
      if recursive then if isFolder(sr) then
        listFiles(aList, aPath + directorySeparator + sr.Name, recursive);
    until
      findNext(sr) <> 0;
  finally
    sysutils.FindClose(sr);
  end;
end;

procedure listFolders(aList: TStrings; const aPath: string);
var
  sr: TSearchrec;
begin
  if findFirst(aPath + '*', faAnyFile, sr) = 0 then
  try
    repeat if isFolder(sr) then
      aList.Add(aPath + sr.Name);
    until findNext(sr) <> 0;
  finally
    sysutils.FindClose(sr);
  end;
end;

function hasFolder(const aPath: string): boolean;
var
  sr: TSearchrec;
  res: boolean;
begin
  res := false;
  if findFirst(aPath + directorySeparator + '*', faDirectory, sr) = 0 then
  try
    repeat if isFolder(sr) then
    begin
      res := true;
      break;
    end;
    until findNext(sr) <> 0;
  finally
    sysutils.FindClose(sr);
  end;
  result := res;
end;

function listAsteriskPath(const aPath: string; aList: TStrings; someExts: TStrings = nil): boolean;
var
  pth, ext, fname: string;
  files: TStringList;
begin
  result := false;
  if aPath = '' then
    exit;
  //
  if aPath[length(aPath)] = '*' then
  begin
    pth := aPath[1..length(aPath)-1];
    if pth[length(pth)] in ['/', '\'] then
      pth := pth[1..length(pth)-1];
    if not directoryExists(pth) then exit(false);
    //
    files := TStringList.Create;
    try
      listFiles(files, pth, true);
      for fname in files do
      begin
        if someExts = nil then
          aList.Add(fname)
        else
        begin
          ext := extractFileExt(fname);
          if someExts.IndexOf(ext) <> -1 then
            aList.Add(fname);
        end;
      end;
    finally
      files.Free;
    end;
    exit(true);
  end;
  exit(false);
end;

procedure listDrives(aList: TStrings);
{$IFDEF WINDOWS}
var
  drv: char;
  ltr, nme: string;
  OldMode : Word;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  setLength(nme, 255);
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    for drv := 'A' to 'Z' do
    begin
      try
        ltr := drv + ':\';
        if not GetVolumeInformation(PChar(ltr), PChar(nme), 255, nil, nil, nil, nil, 0) then
          continue;
        case GetDriveType(PChar(ltr)) of
           DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_REMOTE: aList.Add(ltr);
        end;
      except
        // SEM_FAILCRITICALERRORS: exception is sent to application.
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
  {$ELSE}
  aList.Add('//');
  {$ENDIF}
end;

function shellOpen(const aFilename: string): boolean;
begin
  {$IFDEF WINDOWS}
  result := ShellExecute(0, 'OPEN', PChar(aFilename), nil, nil, SW_SHOW) > 32;
  {$ENDIF}
  {$IFDEF LINUX}
  with TProcess.Create(nil) do
  try
    Executable := 'xdg-open';
    Parameters.Add(aFilename);
    Execute;
  finally
    result := true;
    Free;
  end;
  {$ENDIF}
  {$IFDEF DARWIN}
  with TProcess.Create(nil) do
  try
    Executable := 'open';
    Parameters.Add(aFilename);
    Execute;
  finally
    result := true;
    Free;
  end;
  {$ENDIF}

end;

function exeInSysPath(anExeName: string): boolean;
begin
  exit(exeFullName(anExeName) <> '');
end;

function exeFullName(anExeName: string): string;
var
  ext: string;
  env: string;
begin
  ext := extractFileExt(anExeName);
  if ext <> exeExt then
    anExeName += exeExt;
  if FileExists(anExeName) then
    exit(anExeName)
  else
  begin
    env := sysutils.GetEnvironmentVariable('PATH');
    if Application <> nil then
      env += PathSeparator + ExtractFileDir(ExtractFilePath(application.ExeName));
    exit(ExeSearch(anExeName, env));
  end;
end;

procedure processOutputToStrings(aProcess: TProcess; var aList: TStringList);
var
  str: TMemoryStream;
  sum: Integer;
  cnt: Integer;
  buffSz: Integer;
begin
  if not (poUsePipes in aProcess.Options) then
    exit;
  //
  // note: aList.LoadFromStream() does not work, lines can be split, which breaks message parsing (e.g filename detector).
  //
  {
    Split lines:
    ------------

    The problem comes from TAsynProcess.OnReadData. When the output is read in the
    event, it does not always finish on a full line.

    Resolution:
    -----------

    in TAsynProcess.OnReadData Accumulate avalaible output in a stream.
    Detects last line terminator in the accumation.
    Load TStrings from this stream range.
  }
  sum := 0;
  str := TMemoryStream.Create;
  try
    buffSz := aProcess.PipeBufferSize;
    // temp fix: messages are cut if the TAsyncProcess version is used on simple TProcess.
    if aProcess is TAsyncProcess then begin
      while aProcess.Output.NumBytesAvailable <> 0 do begin
        str.SetSize(sum + buffSz);
        cnt := aProcess.Output.Read((str.Memory + sum)^, buffSz);
        sum += cnt;
      end;
    end else begin
      repeat
        str.SetSize(sum + buffSz);
        cnt := aProcess.Output.Read((str.Memory + sum)^, buffSz);
        sum += cnt;
      until
        cnt = 0;
    end;
    str.Size := sum;
    aList.LoadFromStream(str);
  finally
    str.Free;
  end;
end;

procedure processOutputToStream(aProcess: TProcess; output: TMemoryStream);
var
  sum, cnt: Integer;
const
  buffSz = 2048;
begin
  if not (poUsePipes in aProcess.Options) then
    exit;
  //
  sum := output.Size;
  while aProcess.Output.NumBytesAvailable <> 0 do begin
    output.SetSize(sum + buffSz);
    cnt := aProcess.Output.Read((output.Memory + sum)^, buffSz);
    sum += cnt;
  end;
  output.SetSize(sum);
  output.Position := sum;
end;

procedure killProcess(var aProcess: TAsyncProcess);
begin
  if aProcess = nil then
    exit;
  if aProcess.Running then
    aProcess.Terminate(0);
  aProcess.Free;
  aProcess := nil;
end;

procedure killProcess(var aProcess: TCheckedAsyncProcess);
begin
  if aProcess = nil then
    exit;
  if aProcess.Running then
    aProcess.Terminate(0);
  aProcess.Free;
  aProcess := nil;
end;

procedure ensureNoPipeIfWait(aProcess: TProcess);
begin
  if not (poWaitonExit in aProcess.Options) then
    exit;
  aProcess.Options := aProcess.Options - [poStderrToOutPut, poUsePipes];
end;

function getLineEndingLength(const aFilename: string): byte;
var
  value: char;
  le: string;
begin
  value := #0;
  le := LineEnding;
  result := length(le);
  if not fileExists(aFilename) then
    exit;
  with TMemoryStream.Create do
  try
    LoadFromFile(aFilename);
    while true do
    begin
      if Position = Size then
        exit;
      read(value,1);
      if value = #10 then
        exit(1);
      if value = #13 then
        exit(2);
    end;
  finally
    Free;
  end;
end;

function getSysLineEndLen: byte;
begin
  {$IFDEF WINDOWS}
  exit(2);
  {$ELSE}
  exit(1);
  {$ENDIF}
end;

function countFolder(aFilename: string): integer;
var
  parent: string;
begin
  result := 0;
  while(true) do begin
    parent := ExtractFileDir(aFilename);
    if parent = aFilename then exit;
    aFilename := parent;
    result += 1;
  end;
end;

//TODO-cfeature: make it working with relative paths
function commonFolder(const someFiles: TStringList): string;
var
  i,j,k: integer;
  sink: TStringList;
  dir: string;
  cnt: integer;
begin
  result := '';
  if someFiles.Count = 0 then exit;
  sink := TStringList.Create;
  try
    sink.Assign(someFiles);
    for i := sink.Count-1 downto 0 do
      if (not FileExists(sink.Strings[i])) and (not DirectoryExists(sink.Strings[i])) then
        sink.Delete(i);
    // folders count
    cnt := 256;
    for dir in sink do
    begin
      k := countFolder(dir);
      if k < cnt then
        cnt := k;
    end;
    for i := sink.Count-1 downto 0 do
    begin
      while (countFolder(sink.Strings[i]) <> cnt) do
        sink.Strings[i] := ExtractFileDir(sink.Strings[i]);
    end;
    // common folder
    while(true) do
    begin
      for i := sink.Count-1 downto 0 do
      begin
        dir := ExtractFileDir(sink.Strings[i]);
        j := sink.IndexOf(dir);
        if j = -1 then
          sink.Strings[i] := dir
        else if j <> i then
          sink.Delete(i);
      end;
      if sink.Count = 1 then
        break;
    end;
    result := sink.Strings[0];
  finally
    sink.free;
  end;
end;

{$IFDEF WINDOWS}
function internalAppIsRunning(const ExeName: string): integer;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := 0;
  while integer(ContinueLoop) <> 0 do
    begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeName))) then
      begin
      Inc(Result);
      // SendMessage(Exit-Message) possible?
      end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  CloseHandle(FSnapshotHandle);
end;
{$ENDIF}

{$IFDEF LINUX}
function internalAppIsRunning(const ExeName: string): integer;
var
  proc: TProcess;
  lst: TStringList;
begin
  Result := 0;
  proc := tprocess.Create(nil);
  proc.Executable := 'ps';
  proc.Parameters.Add('-C');
  proc.Parameters.Add(ExeName);
  proc.Options := [poUsePipes, poWaitonexit];
  try
    proc.Execute;
    lst := TStringList.Create;
    try
      lst.LoadFromStream(proc.Output);
      Result := Pos(ExeName, lst.Text);
    finally
      lst.Free;
    end;
  finally
    proc.Free;
  end;
end;
{$ENDIF}

{$IFDEF DARWIN}
function internalAppIsRunning(const ExeName: string): integer;
var
  proc: TProcess;
  lst: TStringList;
begin
  Result := 0;
  proc := tprocess.Create(nil);
  proc.Executable := 'pgrep';
  proc.Parameters.Add(ExeName);
  proc.Options := [poUsePipes, poWaitonexit];
  try
    proc.Execute;
    lst := TStringList.Create;
    try
      lst.LoadFromStream(proc.Output);
      Result := StrToIntDef(Trim(lst.Text), 0);
    finally
      lst.Free;
    end;
  finally
    proc.Free;
  end;
end;
{$ENDIF}

function AppIsRunning(const ExeName: string):Boolean;
begin
  Result:= internalAppIsRunning(ExeName) > 0;
end;

function hasDlangSyntax(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.d', '.di': result := true;
  end;
end;


function isDlangCompilable(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.d', '.di', '.dd', '.obj', '.o', '.a', '.lib': result := true;
  end;
end;

function isEditable(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.d', '.di', '.dd': result := true;
  end;
end;

initialization
  registerClasses([TCEPersistentShortcut]);
end.
