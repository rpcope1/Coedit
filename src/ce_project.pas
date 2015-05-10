unit ce_project;

{$I ce_defines.inc}

interface

uses
  {$IFDEF DEBUG}
  LclProc,
  {$ENDIF}
  Classes, SysUtils, process, strUtils, ce_common, ce_writableComponent,
  ce_dmdwrap, ce_observer;

type

(*******************************************************************************
 * Represents a D project.
 *
 * It includes all the options defined in ce_dmdwrap, organized in
 * a collection to allow multiples configurations.
 *
 * Basically it' s designed to provide the options for the dmd process.
 *)
  TCEProject = class(TWritableLfmTextComponent)
  private
    fOnChange: TNotifyEvent;
    fModified: boolean;
    fRootFolder: string;
    fBasePath: string;
    fLibAliases: TStringList;
    fOptsColl: TCollection;
    fSrcs, fSrcsCop: TStringList;
    fConfIx: Integer;
    fUpdateCount: NativeInt;
    fProjectSubject: TCECustomSubject;
    fRunner: TCheckedAsyncProcess;
    fOutputFilename: string;
    fCanBeRun: boolean;
    procedure updateOutFilename;
    procedure doChanged;
    procedure setLibAliases(const aValue: TStringList);
    procedure subMemberChanged(sender : TObject);
    procedure setOptsColl(const aValue: TCollection);
    procedure setRoot(const aValue: string);
    procedure setSrcs(const aValue: TStringList);
    procedure setConfIx(aValue: Integer);
    function getConfig(const ix: integer): TCompilerConfiguration;
    function getCurrConf: TCompilerConfiguration;
    function runPrePostProcess(const processInfo: TCompileProcOptions): Boolean;
    // passes pre/post/executed project/ outputs as bubles.
    procedure runProcOutput(sender: TObject);
    // passes compilation message as "to be guessed"
    procedure compProcOutput(proc: TProcess);
  protected
    procedure beforeLoad; override;
    procedure afterSave; override;
    procedure afterLoad; override;
    procedure setFilename(const aValue: string); override;
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean); override;
  published
    property RootFolder: string read fRootFolder write setRoot;
    property OptionsCollection: TCollection read fOptsColl write setOptsColl;
    property Sources: TStringList read fSrcs write setSrcs; // 'read' should return a copy to avoid abs/rel errors
    property ConfigurationIndex: Integer read fConfIx write setConfIx;
    property LibraryAliases: TStringList read fLibAliases write setLibAliases;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure beginUpdate;
    procedure endUpdate;
    procedure reset;
    procedure addDefaults;
    function isProjectSource(const aFilename: string): boolean;
    function getAbsoluteSourceName(aIndex: integer): string;
    function getAbsoluteFilename(const aFilename: string): string;
    procedure addSource(const aFilename: string);
    function addConfiguration: TCompilerConfiguration;
    procedure getOpts(const aList: TStrings);
    function runProject(const runArgs: string = ''): Boolean;
    function compileProject: Boolean;
    //
    property configuration[ix: integer]: TCompilerConfiguration read getConfig;
    property currentConfiguration: TCompilerConfiguration read getCurrConf;
    property onChange: TNotifyEvent read fOnChange write fOnChange;
    property modified: Boolean read fModified;
    property canBeRun: Boolean read fCanBeRun;
    property outputFilename: string read fOutputFilename;
  end;

implementation

uses
  ce_interfaces, controls, dialogs, ce_symstring, ce_libman, ce_dcd;

constructor TCEProject.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  //
  fProjectSubject := TCEProjectSubject.create;
  //
  fLibAliases := TStringList.Create;
  fSrcs := TStringList.Create;
  fSrcs.OnChange := @subMemberChanged;
  fSrcsCop := TStringList.Create;
  fOptsColl := TCollection.create(TCompilerConfiguration);
  //
  reset;
  addDefaults;
  subjProjNew(TCEProjectSubject(fProjectSubject), self);
  subjProjChanged(TCEProjectSubject(fProjectSubject), self);
  //
  {$IFDEF LINUX}
  fBasePath := '/';
  {$ENDIF}
  //
  fModified := false;
end;

destructor TCEProject.destroy;
begin
  subjProjClosing(TCEProjectSubject(fProjectSubject), self);
  fProjectSubject.Free;
  //
  fOnChange := nil;
  fLibAliases.Free;
  fSrcs.free;
  fSrcsCop.Free;
  fOptsColl.free;
  killProcess(fRunner);
  inherited;
end;

function TCEProject.addConfiguration: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Add);
  result.onChanged := @subMemberChanged;
end;

procedure TCEProject.setOptsColl(const aValue: TCollection);
var
  i: nativeInt;
begin
  fOptsColl.Assign(aValue);
  for i:= 0 to fOptsColl.Count-1 do
    Configuration[i].onChanged := @subMemberChanged;
end;

procedure TCEProject.addSource(const aFilename: string);
var
  relSrc, absSrc, ext: string;
begin
  ext := ExtractFileExt(aFilename);
  if (dExtList.IndexOf(ext) = -1) and
    (ext <> '.obj') and (ext <> '.o')
      and (ext <> '.lib') and (ext <> '.a') then
        exit;
  for relSrc in fSrcs do
  begin
    absSrc := expandFilenameEx(fBasePath,relsrc);
    if aFilename = absSrc then exit;
  end;
  fSrcs.Add(ExtractRelativepath(fBasePath, aFilename));
end;

procedure TCEProject.setRoot(const aValue: string);
begin
  if fRootFolder = aValue then exit;
  beginUpdate;
  fRootFolder := aValue;
  endUpdate;
end;

procedure TCEProject.setFilename(const aValue: string);
var
  oldAbs, newRel, oldBase: string;
  i: NativeInt;
begin
  if fFilename = aValue then exit;
  //
  beginUpdate;

  fFilename := aValue;
  oldBase := fBasePath;
  fBasePath := extractFilePath(fFilename);
  //
  for i:= 0 to fSrcs.Count-1 do
  begin
    oldAbs := expandFilenameEx(oldBase,fSrcs[i]);
    newRel := ExtractRelativepath(fBasePath, oldAbs);
    fSrcs[i] := newRel;
  end;
  //
  endUpdate;
end;

procedure TCEProject.setLibAliases(const aValue: TStringList);
begin
  beginUpdate;
  fLibAliases.Assign(aValue);
  endUpdate;
end;

procedure TCEProject.setSrcs(const aValue: TStringList);
begin
  beginUpdate;
  fSrcs.Assign(aValue);
  patchPlateformPaths(fSrcs);
  endUpdate;
end;

procedure TCEProject.setConfIx(aValue: Integer);
begin
  beginUpdate;
  if aValue < 0 then aValue := 0;
  if aValue > fOptsColl.Count-1 then aValue := fOptsColl.Count-1;
  fConfIx := aValue;
  endUpdate;
end;

procedure TCEProject.subMemberChanged(sender : TObject);
begin
  beginUpdate;
  fModified := true;
  endUpdate;
end;

procedure TCEProject.beginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TCEProject.endUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount > 0 then
  begin
    {$IFDEF DEBUG}
    DebugLn('project update count > 0');
    {$ENDIF}
    exit;
  end;
  fUpdateCount := 0;
  doChanged;
end;

procedure TCEProject.doChanged;
{$IFDEF DEBUG}
var
  lst: TStringList;
{$ENDIF}
begin
  fModified := true;
  updateOutFilename;
  subjProjChanged(TCEProjectSubject(fProjectSubject), self);
  if assigned(fOnChange) then fOnChange(Self);
  {$IFDEF DEBUG}
  lst := TStringList.Create;
  try
    lst.Add('---------begin----------');
    getOpts(lst);
    lst.Add('---------end-----------');
    DebugLn(lst.Text);
  finally
    lst.Free;
  end;
  {$ENDIF}
end;

function TCEProject.getConfig(const ix: integer): TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Items[ix]);
  result.onChanged := @subMemberChanged;
end;

function TCEProject.getCurrConf: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Items[fConfIx]);
end;

procedure TCEProject.addDefaults;
begin
  with TCompilerConfiguration(fOptsColl.Add) do
  begin
    Name := 'debug';
    debugingOptions.debug := true;
    debugingOptions.codeviewCformat := true;
    outputOptions.boundsCheck := onAlways;
  end;
  with TCompilerConfiguration(fOptsColl.Add) do
  begin
    Name := 'unittest';
    outputOptions.unittest := true;
    outputOptions.boundsCheck := onAlways;
  end;
  with TCompilerConfiguration(fOptsColl.Add) do
  begin
    Name := 'release';
    outputOptions.release := true;
    outputOptions.inlining := true;
    outputOptions.boundsCheck := offAlways;
    outputOptions.optimizations := true;
  end;
end;

procedure TCEProject.reset;
var
  defConf: TCompilerConfiguration;
begin
  beginUpdate;
  fConfIx := 0;
  fOptsColl.Clear;
  defConf := addConfiguration;
  defConf.name := 'default';
  fSrcs.Clear;
  fFilename := '';
  endUpdate;
  fModified := false;
end;

procedure TCEProject.getOpts(const aList: TStrings);
var
  rel, abs: string;
  i: Integer;
  ex_files: TStringList;
  ex_folds: TStringList;
  str: string;
begin
  if fConfIx = -1 then exit;
  ex_files := TStringList.Create;
  ex_folds := TStringList.Create;
  try
    // prepares the exclusions
    for i := 0 to currentConfiguration.pathsOptions.exclusions.Count-1 do
    begin
      str := symbolExpander.get(currentConfiguration.pathsOptions.exclusions.Strings[i]);
      rel := expandFilenameEx(fBasePath, currentConfiguration.pathsOptions.exclusions.Strings[i]);
      if fileExists(str) then
        ex_files.Add(str)
      else if DirectoryExists(str) then
        ex_folds.Add(str);
      if fileExists(rel) then
        ex_files.Add(rel)
      else if DirectoryExists(rel) then
        ex_folds.Add(rel);
    end;
    // sources
    for rel in fSrcs do if rel <> '' then
    begin
      abs := expandFilenameEx(fBasePath, rel);
      if ex_files.IndexOf(abs) = -1 then
        if ex_folds.IndexOf(ExtractFilePath(abs)) = -1
          then aList.Add(abs); // note: process.inc ln 249. double quotes are added if there's a space.
    end;
    // libraries
    LibMan.getLibFiles(fLibAliases, aList);
    LibMan.getLibSources(fLibAliases, aList);
    // config
    TCompilerConfiguration(fOptsColl.Items[fConfIx]).getOpts(aList);
  finally
    ex_files.Free;
    ex_folds.Free;
  end;
end;

function TCEProject.isProjectSource(const aFilename: string): boolean;
var
  i: Integer;
begin
  for i := 0 to fSrcs.Count-1 do
    if getAbsoluteSourceName(i) = aFilename then
      exit(true);
  exit(false);
end;

function TCEProject.getAbsoluteSourceName(aIndex: integer): string;
begin
  if aIndex < 0 then exit('');
  if aIndex > fSrcs.Count-1 then exit('');
  result := expandFileNameEx(fBasePath, fSrcs.Strings[aIndex]);
end;

function TCEProject.getAbsoluteFilename(const aFilename: string): string;
begin
  result := expandFileNameEx(fBasePath, aFilename);
end;

procedure TCEProject.afterSave;
begin
  fModified := false;
  updateOutFilename;
end;

procedure TCEProject.beforeLoad;
begin
  beginUpdate;
  Inherited;
end;

procedure TCEProject.afterLoad;
var
  hasPatched: Boolean;
  // either all the source files have moved or only the project file
  procedure checkMissingAllSources;
  var
    allMissing: boolean;
    dirHint: string;
    newdir: string;
    ini: string;
    src: string;
    i: Integer;
  begin
    if fSrcs.Count = 0 then exit;
    allMissing := true;
    for i:= 0 to fSrcs.Count-1 do
      if fileExists(getAbsoluteSourceName(i)) then
        allMissing := false;
    if not allMissing then exit;
    if dlgOkCancel( 'The project source(s) are all missing. ' + LineEnding +
      'This can be encountered if the project file has been moved from its original location.' + LineEnding + LineEnding +
      'Do you wish to select the new root folder ?') <> mrOk then exit;
    // TODO-cimprovement: use commonFolder() when it'll be compat. with the rel. paths.
    // hint for the common dir
    dirHint := fSrcs.Strings[i];
    while (dirHint[1] = '.') or (dirHint[1] = DirectorySeparator) do
        dirHint := dirHint[2..length(dirHint)];
    ini := extractFilePath(fFilename);
    if not selectDirectory( format('select the folder (that contains "%s")',[dirHint]), ini, newdir) then
      exit;
    for i := 0 to fSrcs.Count-1 do
    begin
      src := fSrcs.Strings[i];
      while (src[1] = '.') or (src[1] = DirectorySeparator) do
        src := src[2..length(src)];
      if fileExists(expandFilenameEx(fBasePath, newdir + DirectorySeparator + src)) then
        fSrcs.Strings[i] := ExtractRelativepath(fBasePath, newdir + DirectorySeparator + src);
      hasPatched := true;
    end;
  end;
  // single sources files are missing
  procedure checkMissingSingleSource;
  var
    oldsrc: string;
    opendlg: TOpenDialog;
    i: Integer;
  begin
    for i:= fSrcs.Count-1 downto 0 do
    begin
      oldsrc := getAbsoluteSourceName(i);
      if fileExists(oldsrc) then continue;
      if dlgOkCancel(format('a particular project source file ("%s") is missing. '
        + LineEnding + 'This happends if a source file has been moved, renamed ' +
        'or deleted.' + LineEnding + LineEnding +
        'Do you wish to select its new location?', [fSrcs[i]])) <> mrOk then exit;
      //
      opendlg := TOpenDialog.Create(nil);
      try
        opendlg.InitialDir := extractFilePath(fFilename);
        opendlg.FileName := fSrcs[i];
        if opendlg.execute then
        begin
          if ExtractFileName(oldsrc) <> ExtractFileName(opendlg.filename) then
            if dlgOkCancel('the filenames are different, replace the old file ?') <> mrOk then
              continue;
            fSrcs[i] := ExtractRelativepath(fBasePath, opendlg.Filename);
            hasPatched := true;
        end else
        begin
          if dlgOkCancel('You have choosen not to update the file, ' +
          'do you wish to remove it from the project ?') <> mrOk then
              continue;
          fSrcs.Delete(i);
          hasPatched := true;
        end;
      finally
        opendlg.Free;
      end;
    end;
  end;
//
begin
  patchPlateformPaths(fSrcs);
  fModified := false;
  hasPatched := false;
  //
  // TODO-cfeature: a modal form with the file list, green checkers and red crosses to indicate the state
  // and some actions to apply to a particular selection: patch root, remove from project, replace, etc...
  checkMissingAllSources;
  checkMissingSingleSource;
  if hasPatched then
  begin
    dlgOkInfo('some source file paths has been patched, some others invalid ' +
    'paths or file may still exist (-of, -od, extraSources, etc)' +
    'but cannot be automatically handled. Note that the modifications have not been saved.');
  end;
  //
  updateOutFilename;
  endUpdate;
  if not hasPatched then fModified := false;
end;

procedure TCEProject.readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean);
//var
  //idt: string;
  //curr: TCompilerConfiguration;
begin
  // continue loading: this method ensures the project compat. in case of drastic changes.

  {curr := self.configuration[OptionsCollection.Count-1];
  if PropName = 'debugIdentifier' then
  begin
    idt := Reader.ReadUnicodeString; // next prop starts one char too late
    if curr.debugingOptions.debugIdentifiers.IndexOf(idt) = -1 then
      curr.debugingOptions.debugIdentifiers.Add(idt);
    Skip := true;
    Handled := true;
  end
  else if PropName = 'versionIdentifier' then
  begin
    idt := Reader.ReadString; // next prop starts one char too late
    if curr.outputOptions.versionIdentifiers.IndexOf(idt) = -1 then
      curr.outputOptions.versionIdentifiers.Add(idt);
    Skip := true;
    Handled := true;
    exit;
  end
  else}
  begin
    Skip := true;
    Handled := false;
  end;
end;

procedure TCEProject.updateOutFilename;
begin
  fOutputFilename := currentConfiguration.pathsOptions.outputFilename;
  // field is specified
  if fOutputFilename <> '' then
  begin
    fOutputFilename := symbolExpander.get(fOutputFilename);
    fOutputFilename := getAbsoluteFilename(fOutputFilename);
    {$IFDEF WINDOWS}
    // field is specified without ext or with a dot in the name.
    // DMD will add the ext. (e.g: "-ofresourced")
    // https://issues.dlang.org/show_bug.cgi?id=13989
    if fileexists(fOutputFilename + exeExt) then
      if currentConfiguration.outputOptions.binaryKind = executable then
        fOutputFilename := fOutputFilename + exeExt;
    {$ENDIF}
  end
  // try to guess
  else if Sources.Count > 0 then
  begin
    // ideally, main() should be searched for, when project binaryKind is executable
    fOutputFilename := extractFilename(Sources.Strings[0]);
    fOutputFilename := stripFileExt(fOutputFilename);
    if FileExists(fileName) then
      fOutputFilename := extractFilePath(fileName) + fOutputFilename
    else
      fOutputFilename := GetTempDir(false) + fOutputFilename;
    // force extension
    case currentConfiguration.outputOptions.binaryKind of
      executable: fOutputFilename := ChangeFileExt(fOutputFilename, exeExt);
      staticlib:  fOutputFilename := ChangeFileExt(fOutputFilename, libExt);
      sharedlib:  fOutputFilename := ChangeFileExt(fOutputFilename, dynExt);
      obj:        fOutputFilename := ChangeFileExt(fOutputFilename, objExt);
    end;
  end;
  //
  fCanBeRun := false;
  if currentConfiguration.outputOptions.binaryKind = executable then
    fCanBeRun := fileExists(fOutputFilename);
end;

function TCEProject.runPrePostProcess(const processInfo: TCompileProcOptions): Boolean;
var
  process: TProcess;
  pname: string;
  i, j: integer;
begin
  pname := symbolExpander.get(processInfo.executable);
  if (not exeInSysPath(pname)) and (pname <> '') then
    exit(false)
  else if (pname = '') then
    exit(true);
  //
  process := TProcess.Create(nil);
  try
    processInfo.setProcess(process);
    process.Executable := pname;
    j := process.Parameters.Count-1;
    for i:= 0 to j do
      process.Parameters.AddText(symbolExpander.get(process.Parameters.Strings[i]));
    for i:= 0 to j do
      process.Parameters.Delete(0);
    if process.CurrentDirectory = '' then
      process.CurrentDirectory := extractFilePath(process.Executable);
    ensureNoPipeIfWait(process);
    process.Execute;
    while process.Running do
    if not (poWaitOnExit in process.Options) then
      if poUsePipes in process.Options then
        runProcOutput(process);
  finally
    result := process.ExitStatus = 0;
    process.Free;
  end;
end;

function TCEProject.compileProject: Boolean;
var
  config: TCompilerConfiguration;
  compilproc: TProcess;
  olddir, prjpath: string;
  prjname: string;
  msgs: ICEMessagesDisplay;
begin
  result := false;
  config := currentConfiguration;
  msgs := getMessageDisplay;
  if config = nil then
  begin
    msgs.message('unexpected project error: no active configuration',
      Self, amcProj, amkErr);
    exit;
  end;
  //
  msgs.clearByData(Self);
  subjProjCompiling(TCEProjectSubject(fProjectSubject), Self);
  //
  if not runPrePostProcess(config.preBuildProcess) then
    msgs.message('project warning: the pre-compilation process has not been properly executed',
      Self, amcProj, amkWarn);
  //
  if (Sources.Count = 0) and (config.pathsOptions.extraSources.Count = 0) then
    exit;
  //
  prjname := shortenPath(filename, 25);
  compilproc := TProcess.Create(nil);
  olddir := '';
  getDir(0, olddir);
  try
    msgs.message('compiling ' + prjname, Self, amcProj, amkInf);
    prjpath := extractFilePath(fileName);
    if directoryExists(prjpath) then
    begin
      chDir(prjpath);
      compilproc.CurrentDirectory := prjpath;
    end;
    compilproc.Executable := DCompiler;
    compilproc.Options := compilproc.Options + [poStderrToOutPut, poUsePipes];
    compilproc.ShowWindow := swoHIDE;
    getOpts(compilproc.Parameters);
    compilproc.Execute;
    while compilProc.Running do
      compProcOutput(compilproc);
    if compilproc.ExitStatus = 0 then begin
      msgs.message(prjname + ' has been successfully compiled', Self, amcProj, amkInf);
      result := true;
    end else
      msgs.message(prjname + ' has not been compiled', Self, amcProj, amkWarn);

    if not runPrePostProcess(config.PostBuildProcess) then
      msgs.message( 'project warning: the post-compilation process has not been properly executed',
        Self, amcProj, amkWarn);

  finally
    updateOutFilename;
    compilproc.Free;
    chDir(olddir);
  end;
end;

function TCEProject.runProject(const runArgs: string = ''): Boolean;
var
  prm: string;
  i: Integer;
begin
  result := false;
  killProcess(fRunner);
  //
  fRunner := TCheckedAsyncProcess.Create(nil); // fRunner can use the input process widget.
  currentConfiguration.runOptions.setProcess(fRunner);
  if runArgs <> '' then
  begin
    prm := '';
    i := 1;
    repeat
      prm := ExtractDelimited(i, runArgs, [' ']);
      prm := symbolExpander.get(prm);
      if prm <> '' then
        fRunner.Parameters.AddText(prm);
      Inc(i);
    until prm = '';
  end;
  //
  if not fileExists(outputFilename) then
  begin
    getMessageDisplay.message('output executable missing: ' + shortenPath(outputFilename, 25),
      Self, amcProj, amkErr);
    exit;
  end;
  //
  fRunner.Executable := outputFilename;
  if fRunner.CurrentDirectory = '' then
    fRunner.CurrentDirectory := extractFilePath(fRunner.Executable);
  if poUsePipes in fRunner.Options then begin
    fRunner.OnReadData := @runProcOutput;
    fRunner.OnTerminate := @runProcOutput;
    getprocInputHandler.addProcess(fRunner);
  end;
  fRunner.Execute;
  //
  result := true;
end;

procedure TCEProject.runProcOutput(sender: TObject);
var
  proc: TProcess;
  lst: TStringList;
  str: string;
  msgs: ICEMessagesDisplay;
begin
  proc := TProcess(sender);
  lst := TStringList.Create;
  msgs := getMessageDisplay;
  try
    processOutputToStrings(proc, lst);
    for str in lst do
      msgs.message(str, Self, amcProj, amkBub);
  finally
    lst.Free;
  end;
  //
  if not proc.Active then
    getprocInputHandler.removeProcess(proc);
end;

procedure TCEProject.compProcOutput(proc: TProcess);
var
  lst: TStringList;
  str: string;
  msgs: ICEMessagesDisplay;
begin
  lst := TStringList.Create;
  msgs := getMessageDisplay;
  try
    processOutputToStrings(proc, lst);
    for str in lst do
      msgs.message(str, Self, amcProj, amkAuto);
  finally
    lst.Free;
  end;
end;

initialization
  RegisterClasses([TCEProject]);
end.
