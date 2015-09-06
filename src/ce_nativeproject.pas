unit ce_nativeproject;

{$I ce_defines.inc}

interface

uses
  {$IFDEF DEBUG}
  LclProc,
  {$ENDIF}
  {$IFNDEF CEBUILD}
  ce_dialogs,
  {$ENDIF}
  Classes, SysUtils, process, strUtils, ce_common, ce_writableComponent,
  ce_dmdwrap, ce_observer, ce_interfaces, ce_processes;

type

(*******************************************************************************
 * Represents a D project.
 *
 * It includes all the options defined in ce_dmdwrap, organized in
 * a collection to allow multiples configurations.
 *
 * Basically it' s designed to provide the options for the dmd process.
 *)
  TCENativeProject = class(TWritableLfmTextComponent, ICECommonProject)
  private
    fOnChange: TNotifyEvent;
    fModified: boolean;
    fRootFolder: string;
    fBasePath: string;
    fLibAliases: TStringList;
    fConfigs: TCollection;
    fSrcs, fSrcsCop: TStringList;
    fConfIx: Integer;
    fUpdateCount: NativeInt;
    fProjectSubject: TCEProjectSubject;
    fRunner: TCEProcess;
    fOutputFilename: string;
    fCanBeRun: boolean;
    fBaseConfig: TCompilerConfiguration;
    procedure updateOutFilename;
    procedure doChanged;
    procedure getBaseConfig;
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
    //
    function getFormat: TCEProjectFormat;
    function getProject: TObject;
  protected
    procedure beforeLoad; override;
    procedure afterSave; override;
    procedure afterLoad; override;
    procedure setFilename(const aValue: string); override;
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean); override;
  published
    property RootFolder: string read fRootFolder write setRoot;
    property OptionsCollection: TCollection read fConfigs write setOptsColl;
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
    function getIfIsSource(const aFilename: string): boolean;
    function getAbsoluteSourceName(aIndex: integer): string;
    function getAbsoluteFilename(const aFilename: string): string;
    procedure addSource(const aFilename: string);
    function addConfiguration: TCompilerConfiguration;
    procedure getOpts(const aList: TStrings);
    function run(const runArgs: string = ''): Boolean;
    function compile: Boolean;
    //
    function getIfModified: boolean;
    function getOutputFilename: string;
    function getConfigurationCount: integer;
    procedure setActiveConfiguration(index: integer);
    function getConfigurationName(index: integer): string;
    function getFilename: string;
    function getBinaryKind: TProjectBinaryKind;
    //
    property configuration[ix: integer]: TCompilerConfiguration read getConfig;
    property currentConfiguration: TCompilerConfiguration read getCurrConf;
    property onChange: TNotifyEvent read fOnChange write fOnChange;
    property modified: Boolean read fModified;
    property canBeRun: Boolean read fCanBeRun;
  end;

  // native project have no ext constraint, this function tells if filename is project
  function isValidNativeProject(const filename: string): boolean;

implementation

uses
  controls, dialogs, ce_symstring, ce_libman, ce_dcd;

constructor TCENativeProject.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  //
  fProjectSubject := TCEProjectSubject.create;
  //
  fLibAliases := TStringList.Create;
  fSrcs := TStringList.Create;
  fSrcs.OnChange := @subMemberChanged;
  fSrcsCop := TStringList.Create;
  fConfigs := TCollection.create(TCompilerConfiguration);
  //
  reset;
  addDefaults;
  subjProjNew(fProjectSubject, self);
  subjProjChanged(fProjectSubject, self);
  //
  {$IFDEF LINUX}
  fBasePath := '/';
  {$ENDIF}
  //
  fModified := false;
end;

destructor TCENativeProject.destroy;
begin
  subjProjClosing(fProjectSubject, self);
  fProjectSubject.Free;
  //
  fOnChange := nil;
  fLibAliases.Free;
  fSrcs.free;
  fSrcsCop.Free;
  fConfigs.free;
  killProcess(fRunner);
  inherited;
end;

function TCENativeProject.getFormat: TCEProjectFormat;
begin
  exit(pfNative);
end;

function TCENativeProject.getProject: TObject;
begin
  exit(Self);
end;

function TCENativeProject.addConfiguration: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fConfigs.Add);
  result.onChanged := @subMemberChanged;
end;

procedure TCENativeProject.setOptsColl(const aValue: TCollection);
var
  i: nativeInt;
begin
  fConfigs.Assign(aValue);
  for i:= 0 to fConfigs.Count-1 do
    Configuration[i].onChanged := @subMemberChanged;
end;

procedure TCENativeProject.addSource(const aFilename: string);
var
  relSrc, absSrc: string;
begin
  if not isDlangCompilable(ExtractFileExt(aFilename)) then
    exit;
  for relSrc in fSrcs do
  begin
    absSrc := expandFilenameEx(fBasePath,relsrc);
    if aFilename = absSrc then exit;
  end;
  fSrcs.Add(ExtractRelativepath(fBasePath, aFilename));
end;

procedure TCENativeProject.setRoot(const aValue: string);
begin
  if fRootFolder = aValue then exit;
  beginUpdate;
  fRootFolder := aValue;
  endUpdate;
end;

procedure TCENativeProject.setFilename(const aValue: string);
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

procedure TCENativeProject.setLibAliases(const aValue: TStringList);
begin
  beginUpdate;
  fLibAliases.Assign(aValue);
  endUpdate;
end;

procedure TCENativeProject.setSrcs(const aValue: TStringList);
begin
  beginUpdate;
  fSrcs.Assign(aValue);
  patchPlateformPaths(fSrcs);
  endUpdate;
end;

procedure TCENativeProject.setConfIx(aValue: Integer);
begin
  beginUpdate;
  if aValue < 0 then aValue := 0;
  if aValue > fConfigs.Count-1 then aValue := fConfigs.Count-1;
  fConfIx := aValue;
  endUpdate;
end;

procedure TCENativeProject.getBaseConfig;
var
  i: integer;
begin
  fBaseConfig := nil;
  for i:= 0 to fConfigs.Count-1 do
    if configuration[i].isBaseConfiguration then
      fBaseConfig := configuration[i];
  for i := 0 to fConfigs.Count-1 do
    if configuration[i].isBaseConfiguration then
      if configuration[i] <> fBaseConfig then
        configuration[i].isBaseConfiguration := false;
end;

procedure TCENativeProject.subMemberChanged(sender : TObject);
begin
  beginUpdate;
  fModified := true;
  endUpdate;
end;

procedure TCENativeProject.beginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TCENativeProject.endUpdate;
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

procedure TCENativeProject.doChanged;
{$IFDEF DEBUG}
var
  lst: TStringList;
{$ENDIF}
begin
  fModified := true;
  updateOutFilename;
  getBaseConfig;
  subjProjChanged(fProjectSubject, self);
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

function TCENativeProject.getConfig(const ix: integer): TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fConfigs.Items[ix]);
  result.onChanged := @subMemberChanged;
end;

function TCENativeProject.getCurrConf: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fConfigs.Items[fConfIx]);
end;

procedure TCENativeProject.addDefaults;
begin
  with TCompilerConfiguration(fConfigs.Add) do
  begin
    Name := 'debug';
    debugingOptions.debug := true;
    debugingOptions.codeviewCformat := true;
    outputOptions.boundsCheck := onAlways;
  end;
  with TCompilerConfiguration(fConfigs.Add) do
  begin
    Name := 'unittest';
    outputOptions.unittest := true;
    outputOptions.boundsCheck := onAlways;
  end;
  with TCompilerConfiguration(fConfigs.Add) do
  begin
    Name := 'release';
    outputOptions.release := true;
    outputOptions.inlining := true;
    outputOptions.boundsCheck := offAlways;
    outputOptions.optimizations := true;
  end;
end;

procedure TCENativeProject.reset;
var
  defConf: TCompilerConfiguration;
begin
  beginUpdate;
  fConfIx := 0;
  fConfigs.Clear;
  defConf := addConfiguration;
  defConf.name := 'default';
  fSrcs.Clear;
  fFilename := '';
  endUpdate;
  fModified := false;
end;

procedure TCENativeProject.getOpts(const aList: TStrings);
var
  rel, abs: string;
  i: Integer;
  ex_files: TStringList;
  ex_folds: TStringList;
  libAliasesPtr: TStringList;
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
    // libraries: an asterisk in list selects all the entries
    libAliasesPtr := fLibAliases;
    if (fLibAliases.Count > 0) and (fLibAliases.Strings[0] = '*') then
      libAliasesPtr := nil;

    {$IFDEF WINDOWS}
    // only link lib file if executable/shared lib
    // OS switch: read more @ http://forum.dlang.org/post/ooekdkwrefposmchekrp@forum.dlang.org
    if (currentConfiguration.outputOptions.binaryKind in [executable, sharedlib]) or
      currentConfiguration.outputOptions.alwaysLinkStaticLibs then
    {$ENDIF}
    LibMan.getLibFiles(libAliasesPtr, aList);

    // but always adds -I<path>
    LibMan.getLibSources(libAliasesPtr, aList);
    // config
    if currentConfiguration.isOverriddenConfiguration then
      currentConfiguration.getOpts(aList, fBaseConfig)
    else
      currentConfiguration.getOpts(aList);
  finally
    ex_files.Free;
    ex_folds.Free;
  end;
end;

function TCENativeProject.getIfIsSource(const aFilename: string): boolean;
var
  i: Integer;
begin
  for i := 0 to fSrcs.Count-1 do
    if getAbsoluteSourceName(i) = aFilename then
      exit(true);
  exit(false);
end;

function TCENativeProject.getAbsoluteSourceName(aIndex: integer): string;
begin
  if aIndex < 0 then exit('');
  if aIndex > fSrcs.Count-1 then exit('');
  result := expandFileNameEx(fBasePath, fSrcs.Strings[aIndex]);
end;

function TCENativeProject.getAbsoluteFilename(const aFilename: string): string;
begin
  result := expandFileNameEx(fBasePath, aFilename);
end;

procedure TCENativeProject.afterSave;
begin
  fModified := false;
  updateOutFilename;
end;

procedure TCENativeProject.beforeLoad;
begin
  beginUpdate;
  Inherited;
end;

procedure TCENativeProject.afterLoad;
var
  hasPatched: Boolean;
  {$IFNDEF CEBUILD}
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
  {$ENDIF}
//
begin
  patchPlateformPaths(fSrcs);
  fModified := false;
  hasPatched := false;
  {$IFNDEF CEBUILD}
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
  {$ENDIF}
  //
  updateOutFilename;
  endUpdate;
  if not hasPatched then fModified := false;
end;

procedure TCENativeProject.readerPropNoFound(Reader: TReader; Instance: TPersistent;
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

procedure TCENativeProject.updateOutFilename;
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

function TCENativeProject.runPrePostProcess(const processInfo: TCompileProcOptions): Boolean;
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
      process.CurrentDirectory := extractFilePath(process.Executable)
    else
      process.CurrentDirectory := symbolExpander.get(process.CurrentDirectory);
    ensureNoPipeIfWait(process);
    process.Execute;
    while process.Running do
      if poUsePipes in process.Options then
        runProcOutput(process);
  finally
    result := process.ExitStatus = 0;
    process.Free;
  end;
end;

function TCENativeProject.compile: Boolean;
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
      self as ICECommonProject, amcProj, amkErr);
    exit;
  end;
  //
  msgs.clearByData(self as ICECommonProject);
  subjProjCompiling(fProjectSubject, Self);
  //
  if not runPrePostProcess(config.preBuildProcess) then
    msgs.message('project warning: the pre-compilation process has not been properly executed',
      self as ICECommonProject, amcProj, amkWarn);
  //
  if (Sources.Count = 0) and (config.pathsOptions.extraSources.Count = 0) then
    exit;
  //
  prjname := shortenPath(filename, 25);
  compilproc := TProcess.Create(nil);
  olddir := '';
  getDir(0, olddir);
  try
    msgs.message('compiling ' + prjname, self as ICECommonProject, amcProj, amkInf);
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
      msgs.message(prjname + ' has been successfully compiled', self as ICECommonProject, amcProj, amkInf);
      result := true;
    end else
      msgs.message(prjname + ' has not been compiled', self as ICECommonProject, amcProj, amkWarn);

    if not runPrePostProcess(config.PostBuildProcess) then
      msgs.message( 'project warning: the post-compilation process has not been properly executed',
        self as ICECommonProject, amcProj, amkWarn);

  finally
    updateOutFilename;
    compilproc.Free;
    chDir(olddir);
  end;
end;

function TCENativeProject.run(const runArgs: string = ''): Boolean;
var
  prm: string;
  i: Integer;
begin
  result := false;
  killProcess(fRunner);
  //
  fRunner := TCEProcess.Create(nil); // fRunner can use the input process widget.
  currentConfiguration.runOptions.setProcess(fRunner);
  if runArgs <> '' then
  begin
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
  if not fileExists(getOutputFilename) then
  begin
    getMessageDisplay.message('output executable missing: ' + shortenPath(getOutputFilename, 25),
      self as ICECommonProject, amcProj, amkErr);
    exit;
  end;
  //
  fRunner.Executable := getOutputFilename;
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

procedure TCENativeProject.runProcOutput(sender: TObject);
var
  lst: TStringList;
  str: string;
  msgs: ICEMessagesDisplay;
begin

  lst := TStringList.Create;
  msgs := getMessageDisplay;
  try
    if (sender is TCEProcess) then
      (sender as TCEProcess).getFullLines(lst)
    else
      processOutputToStrings(TProcess(sender), lst);
    for str in lst do
      msgs.message(str, self as ICECommonProject, amcProj, amkBub);
  finally
    lst.Free;
  end;
  //
  if not TProcess(sender).Active then
    getprocInputHandler.removeProcess(TProcess(sender));
end;

procedure TCENativeProject.compProcOutput(proc: TProcess);
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
      msgs.message(str, self as ICECommonProject, amcProj, amkAuto);
  finally
    lst.Free;
  end;
end;

function TCENativeProject.getIfModified: boolean;
begin
  exit(fModified);
end;

function TCENativeProject.getOutputFilename: string;
begin
  exit(fOutputFilename);
end;

function TCENativeProject.getConfigurationCount: integer;
begin
  exit(fConfigs.Count);
end;

procedure TCENativeProject.setActiveConfiguration(index: integer);
begin
  setConfIx(index);
end;

function TCENativeProject.getConfigurationName(index: integer): string;
begin
  if index > fConfigs.Count -1 then index := fConfigs.Count -1;
  if index < 0 then index := 0;
  result := getConfig(index).name;
end;

function TCENativeProject.getFilename: string;
begin
  exit(fFilename);
end;

function TCENativeProject.getBinaryKind: TProjectBinaryKind;
begin
  exit(currentConfiguration.outputOptions.binaryKind);
end;

function isValidNativeProject(const filename: string): boolean;
var
  maybe: TCENativeProject;
begin
  result := false;
  // avoid the project to notify the observers, current project is not replaced
  EntitiesConnector.beginUpdate;
  maybe := TCENativeProject.create(nil);
  EntitiesConnector.removeSubject(maybe);
  try
    maybe.loadFromFile(filename);
    result := maybe.hasLoaded;
  finally
    maybe.Free;
    EntitiesConnector.endUpdate;
  end;
end;

initialization
  RegisterClasses([TCENativeProject]);
end.
