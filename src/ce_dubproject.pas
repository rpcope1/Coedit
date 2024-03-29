unit ce_dubproject;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, xfpjson, xjsonparser, xjsonscanner, process, strutils, LazFileUtils,
  ce_common, ce_interfaces, ce_observer, ce_dialogs, ce_processes;

type

  TCEDubProject = class(TComponent, ICECommonProject)
  private
    fDubProc: TCEProcess;
    fPreCompilePath: string;
    fPackageName: string;
    fFilename: string;
    fModified: boolean;
    fJSON: TJSONObject;
    fSrcs: TStringList;
    fProjectSubject: TCEProjectSubject;
    fConfigsCount: integer;
    fImportPaths: TStringList;
    fBuildTypes: TStringList;
    fConfigs: TStringList;
    fBuiltTypeIx: integer;
    fConfigIx: integer;
    fBinKind: TProjectBinaryKind;
    fBasePath: string;
    fModificationCount: integer;
    fOutputFileName: string;
    fSaveAsUtf8: boolean;
    fCompiled: boolean;
    //
    procedure doModified;
    procedure updateFields;
    procedure updatePackageNameFromJson;
    procedure udpateConfigsFromJson;
    procedure updateSourcesFromJson;
    procedure updateTargetKindFromJson;
    procedure updateImportPathsFromJson;
    procedure updateOutputNameFromJson;
    function findTargetKindInd(value: TJSONObject): boolean;
    procedure dubProcOutput(proc: TObject);
    procedure dubProcTerminated(proc: TObject);
    function getCurrentCustomConfig: TJSONObject;
    procedure compileOrRun(run: boolean; const runArgs: string = '');
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure beginModification;
    procedure endModification;
    //
    function filename: string;
    function basePath: string;
    procedure loadFromFile(const aFilename: string);
    procedure saveToFile(const aFilename: string);
    //
    function getFormat: TCEProjectFormat;
    function getProject: TObject;
    function modified: boolean;
    function binaryKind: TProjectBinaryKind;
    function getCommandLine: string;
    function outputFilename: string;
    //
    function isSource(const aFilename: string): boolean;
    function sourcesCount: integer;
    function sourceRelative(index: integer): string;
    function sourceAbsolute(index: integer): string;
    function importsPathCount: integer;
    function importPath(index: integer): string;
    //
    function configurationCount: integer;
    function getActiveConfigurationIndex: integer;
    procedure setActiveConfigurationIndex(index: integer);
    function configurationName(index: integer): string;
    //
    procedure compile;
    function compiled: boolean;
    procedure run(const runArgs: string = '');
    function targetUpToDate: boolean;
    //
    property json: TJSONObject read fJSON;
  end;

  // these 9 built types always exist
  TDubBuildType = (plain, debug, release, unittest, docs, ddox, profile, cov, unittestcov);

  // returns true if filename is a valid dub project. Only json format is supported.
  function isValidDubProject(const filename: string): boolean;

  function getDubCompiler: TCECompiler;
  procedure setDubCompiler(value: TCECompiler);

implementation

var

  DubCompiler: TCECompiler = dmd;
  DubCompilerFilename: string = 'dmd';

const

  DubBuiltTypeName: array[TDubBuildType] of string = ('plain', 'debug', 'release',
    'unittest', 'docs', 'ddox', 'profile', 'cov', 'unittest-cov'
  );

  DubDefaultConfigName = '(default config)';


{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCEDubProject.create(aOwner: TComponent);
begin
  inherited;
  fSaveAsUtf8 := true;
  fJSON := TJSONObject.Create();
  fProjectSubject := TCEProjectSubject.Create;
  fBuildTypes := TStringList.Create;
  fConfigs := TStringList.Create;
  fSrcs := TStringList.Create;
  fImportPaths := TStringList.Create;
  //
  subjProjNew(fProjectSubject, self);
  subjProjChanged(fProjectSubject, self);
end;

destructor TCEDubProject.destroy;
begin
  killProcess(fDubProc);
  subjProjClosing(fProjectSubject, self);
  fProjectSubject.free;
  //
  fJSON.Free;
  fBuildTypes.Free;
  fConfigs.Free;
  fSrcs.Free;
  fImportPaths.Free;
  inherited;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICECommonProject: project props ---------------------------------------}
function TCEDubProject.getFormat: TCEProjectFormat;
begin
  exit(pfDub);
end;

function TCEDubProject.getProject: TObject;
begin
  exit(self);
end;

function TCEDubProject.modified: boolean;
begin
  exit(fModified);
end;

function TCEDubProject.filename: string;
begin
  exit(fFilename);
end;

function TCEDubProject.basePath: string;
begin
  exit(fBasePath);
end;

procedure TCEDubProject.loadFromFile(const aFilename: string);
var
  loader: TMemoryStream;
  parser : TJSONParser;
  bom: dword = 0;
begin
  loader := TMemoryStream.Create;
  try
    fBasePath := aFilename.extractFilePath;
    fFilename := aFilename;
    loader.LoadFromFile(fFilename);
    fSaveAsUtf8 := false;
    // skip BOM, this crashes the parser
    loader.Read(bom, 4);
    if (bom and $BFBBEF) = $BFBBEF then
    begin
      loader.Position:= 3;
      fSaveAsUtf8 := true;
    end
    else if (bom = $FFFE0000) or (bom = $FEFF) then
    begin
      // UCS-4 LE/BE not handled by DUB
      loader.clear;
      loader.WriteByte(byte('{'));
      loader.WriteByte(byte('}'));
      loader.Position:= 0;
      fFilename := '';
    end
    else if ((bom and $FEFF) = $FEFF) or ((bom and $FFFE) = $FFFE) then
    begin
      // UCS-2 LE/BE not handled by DUB
      loader.clear;
      loader.WriteByte(byte('{'));
      loader.WriteByte(byte('}'));
      loader.Position:= 0;
      fFilename := '';
    end
    else
      loader.Position:= 0;
    //
    FreeAndNil(fJSON);
    parser := TJSONParser.Create(loader, [joIgnoreTrailingComma, joUTF8]);
    //TODO-cfcl-json: remove etc/fcl-json the day they'll merge and rlz the version with 'Options'
    //TODO-cfcl-json: track possible changes and fixes at http://svn.freepascal.org/cgi-bin/viewvc.cgi/trunk/packages/fcl-json/
    try
      try
        fJSON := parser.Parse as TJSONObject;
      except
        if assigned(fJSON) then
          FreeAndNil(fJSON);
        fFilename := '';
      end;
    finally
      parser.Free;
    end;
  finally
    loader.Free;
    if not assigned(fJSON) then
      fJson := TJSONObject.Create(['name','invalid json']);
    updateFields;
    subjProjChanged(fProjectSubject, self);
    fModified := false;
  end;
end;

procedure TCEDubProject.saveToFile(const aFilename: string);
var
  saver: TMemoryStream;
  str: string;
begin
  saver := TMemoryStream.Create;
  try
    fFilename := aFilename;
    str := fJSON.FormatJSON;
    if fSaveAsUtf8 then
    begin
      saver.WriteDWord($00BFBBEF);
      saver.Position:=saver.Position-1;
    end;
    saver.Write(str[1], str.length);
    saver.SaveToFile(fFilename);
  finally
    saver.Free;
    fModified := false;
  end;
end;

function TCEDubProject.binaryKind: TProjectBinaryKind;
begin
  exit(fBinKind);
end;

function TCEDubProject.getCommandLine: string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add('dub' + exeExt);
    str.Add('build');
    str.Add('--build=' + fBuildTypes[fBuiltTypeIx]);
    if (fConfigs.Count <> 1) and (fConfigs[0] <> DubDefaultConfigName) then
      str.Add('--config=' + fConfigs[fConfigIx]);
    str.Add('--compiler=' + DubCompilerFilename);
    result := str.Text;
  finally
    str.Free;
  end;
end;

function TCEDubProject.outputFilename: string;
begin
  exit(fOutputFileName);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICECommonProject: sources ---------------------------------------------}
function TCEDubProject.isSource(const aFilename: string): boolean;
var
  fname: string;
begin
  fname := aFilename;
  if fname.fileExists then
    fname := ExtractRelativepath(fBasePath, fname);
  result := fSrcs.IndexOf(fname) <> -1;
end;

function TCEDubProject.sourcesCount: integer;
begin
  exit(fSrcs.Count);
end;

function TCEDubProject.sourceRelative(index: integer): string;
begin
  exit(fSrcs[index]);
end;

function TCEDubProject.sourceAbsolute(index: integer): string;
var
  fname: string;
begin
  fname := fSrcs[index];
  if fname.fileExists then
    result := fname
  else
    result := expandFilenameEx(fBasePath, fname);
end;

function TCEDubProject.importsPathCount: integer;
begin
  result := fImportPaths.Count;
end;

function TCEDubProject.importPath(index: integer): string;
begin
  result := expandFilenameEx(fBasePath, fImportPaths[index]);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICECommonProject: configs ---------------------------------------------}
function TCEDubProject.configurationCount: integer;
begin
  exit(fConfigsCount);
end;

function TCEDubProject.getActiveConfigurationIndex: integer;
begin
  exit(fBuiltTypeIx * fConfigs.Count + fConfigIx);
end;

procedure TCEDubProject.setActiveConfigurationIndex(index: integer);
begin
  fBuiltTypeIx := index div fConfigs.Count;
  fConfigIx := index mod fConfigs.Count;
  doModified;
  // DUB does not store an active config
  fModified:=false;
end;

function TCEDubProject.configurationName(index: integer): string;
begin
  result := fBuildTypes[index div fConfigs.Count] + ' - ' +
    fConfigs[index mod fConfigs.Count];
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICECommonProject: actions ---------------------------------------------}
procedure TCEDubProject.dubProcOutput(proc: TObject);
var
  lst: TStringList;
  str: string;
  msgs: ICEMessagesDisplay;
begin
  lst := TStringList.Create;
  msgs := getMessageDisplay;
  try
    fDubProc.getFullLines(lst);
    for str in lst do
      msgs.message(str, self as ICECommonProject, amcProj, amkAuto);
  finally
    lst.Free;
  end;
end;

procedure TCEDubProject.dubProcTerminated(proc: TObject);
var
  msgs: ICEMessagesDisplay;
  prjname: string;
begin
  dubProcOutput(proc);
  msgs := getMessageDisplay;
  prjname := shortenPath(filename);
  fCompiled := fDubProc.ExitStatus = 0;
  if fCompiled then
    msgs.message(prjname + ' has been successfully compiled',
      self as ICECommonProject, amcProj, amkInf)
  else
    msgs.message(prjname + ' has not been compiled',
      self as ICECommonProject, amcProj, amkWarn);
  subjProjCompiled(fProjectSubject, self as ICECommonProject, fCompiled);
  SetCurrentDirUTF8(fPreCompilePath);
end;

procedure TCEDubProject.compileOrRun(run: boolean; const runArgs: string = '');
var
  olddir: string;
  prjname: string;
  msgs: ICEMessagesDisplay;
begin
  msgs := getMessageDisplay;
  if fDubProc.isNotNil and fDubProc.Active then
  begin
    msgs.message('the project is already being compiled',
      self as ICECommonProject, amcProj, amkWarn);
    exit;
  end;
  killProcess(fDubProc);
  fCompiled := false;
  if not fFilename.fileExists then
  begin
    dlgOkInfo('The DUB project must be saved before being compiled or run !');
    exit;
  end;
  msgs.clearByData(Self as ICECommonProject);
  prjname := shortenPath(fFilename);
  fDubProc:= TCEProcess.Create(nil);
  olddir  := GetCurrentDir;
  try
    if not run then
    begin
      subjProjCompiling(fProjectSubject, self as ICECommonProject);
      msgs.message('compiling ' + prjname, self as ICECommonProject, amcProj, amkInf);
      if modified then saveToFile(fFilename);
    end;
    chDir(fFilename.extractFilePath);
    fDubProc.Executable := 'dub' + exeExt;
    fDubProc.Options := fDubProc.Options + [poStderrToOutPut, poUsePipes];
    fDubProc.CurrentDirectory := fFilename.extractFilePath;
    fDubProc.ShowWindow := swoHIDE;
    fDubProc.OnReadData:= @dubProcOutput;
    if not run then
    begin
      fDubProc.Parameters.Add('build');
      fDubProc.OnTerminate:= @dubProcTerminated;
    end
    else
    begin
      fDubProc.Parameters.Add('run');
      fDubProc.OnTerminate:= @dubProcOutput;
    end;
    fDubProc.Parameters.Add('--build=' + fBuildTypes[fBuiltTypeIx]);
    if (fConfigs.Count <> 1) and (fConfigs[0] <> DubDefaultConfigName) then
      fDubProc.Parameters.Add('--config=' + fConfigs[fConfigIx]);
    fDubProc.Parameters.Add('--compiler=' + DubCompilerFilename);
    if run and runArgs.isNotEmpty then
      fDubProc.Parameters.Add('--' + runArgs);
    fDubProc.Execute;
  finally
    SetCurrentDirUTF8(olddir);
  end;
end;

procedure TCEDubProject.compile;
begin
  fPreCompilePath := GetCurrentDirUTF8;
  compileOrRun(false);
end;

function TCEDubProject.compiled: boolean;
begin
  exit(fCompiled);
end;

procedure TCEDubProject.run(const runArgs: string = '');
begin
  compileOrRun(true);
end;

function TCEDubProject.targetUpToDate: boolean;
begin
  // rebuilding is done automatically when the command is 'run'
  result := true;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION JSON to internal fields -----------------------------------------------}
function TCEDubProject.getCurrentCustomConfig: TJSONObject;
var
  item: TJSONData;
  confs: TJSONArray;
begin
  result := nil;
  if fConfigIx = 0 then exit;
  //
  item := fJSON.Find('configurations');
  if item.isNil then exit;
  //
  confs := TJSONArray(item);
  if fConfigIx > confs.Count -1 then exit;
  //
  result := confs.Objects[fConfigIx];
end;

procedure TCEDubProject.updatePackageNameFromJson;
var
  value: TJSONData;
begin
  if not assigned(fJSON) then
    exit;
  value := fJSON.Find('name');
  if value.isNil then fPackageName := ''
  else fPackageName := value.AsString;
end;

procedure TCEDubProject.udpateConfigsFromJson;
var
  i: integer;
  dat: TJSONData;
  arr: TJSONArray = nil;
  item: TJSONObject = nil;
  obj: TJSONObject = nil;
  itemname: string;
begin
  fBuildTypes.Clear;
  fConfigs.Clear;
  if fJSON.isNil then
    exit;
  // the CE interface for dub doesn't make the difference between build type
  //and config, instead each possible combination type + build is generated.
  if fJSON.Find('configurations') <> nil then
  begin
    arr := fJSON.Arrays['configurations'];
    for i:= 0 to arr.Count-1 do
    begin
      item := TJSONObject(arr.Items[i]);
      if item.Find('name').isNil then
        continue;
      fConfigs.Add(item.Strings['name']);
    end;
  end else
  begin
    fConfigs.Add(DubDefaultConfigName);
    // default = what dub set as 'application' or 'library'
    // in this case Coedit will pass only the type to DUB: 'DUB --build=release'
  end;

  fBuildTypes.AddStrings(DubBuiltTypeName);
  dat := fJSON.Find('buildTypes');
  if dat.isNotNil and (dat.JSONType = jtObject) then
  begin
    obj := fJSON.Objects['buildTypes'];
    for i := 0 to obj.Count-1 do
    begin
      itemname := obj.Names[i];
      // defaults build types can be overridden
      if fBuildTypes.IndexOf(itemname) <> -1 then
        continue;
      fBuildTypes.Add(itemname);
    end;
  end;
  fConfigsCount := fConfigs.Count * fBuildTypes.Count;
end;

procedure TCEDubProject.updateSourcesFromJson;
var
  lst: TStringList;
  item: TJSONData;
  conf: TJSONObject;
  arr: TJSONArray;
  i, j: integer;
procedure getExclusion(from: TJSONObject);
var
  i: integer;
begin
  item := from.Find('excludedSourceFiles');
  if item.isNotNil and (item.JSONType = jtArray) then
  begin
    arr := TJSONArray(item);
    for i := 0 to arr.Count-1 do
      lst.Add(patchPlateformPath(arr.Strings[i]));
  end;
end;
procedure tryAddFromFolder(const pth: string);
var
  abs: string;
begin
  if pth.dirExists then
  begin
    lst.Clear;
    listFiles(lst, pth, true);
    for abs in lst do
      if isDlangCompilable(abs.extractFileExt) then
        fSrcs.Add(ExtractRelativepath(fBasePath, abs));
  end;
end;
var
  pth: string;
begin
  fSrcs.Clear;
  if not assigned(fJSON) then
    exit;
  lst := TStringList.Create;
  try
    // auto folders & files
    item := fJSON.Find('mainSourceFile');
    if item.isNotNil then
      fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, item.AsString)));
    tryAddFromFolder(fBasePath + 'src');
    tryAddFromFolder(fBasePath + 'source');
    // custom folders
    item := fJSON.Find('sourcePaths');
    if item.isNotNil then
    begin
      arr := TJSONArray(item);
      for i := 0 to arr.Count-1 do
      begin
        pth := TrimRightSet(arr.Strings[i], ['/','\']);
        if pth.dirExists then
          tryAddFromFolder(pth)
        else
          tryAddFromFolder(fBasePath + pth);
      end;
    end;
    // custom files
    item := fJSON.Find('sourceFiles');
    if item.isNotNil then
    begin
      arr := TJSONArray(item);
      for i := 0 to arr.Count-1 do
        fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, arr.Strings[i])));
    end;
    conf := getCurrentCustomConfig;
    if conf.isNotNil then
    begin
      item := conf.Find('mainSourceFile');
      if item.isNotNil then
        fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, item.AsString)));
      // custom folders in current config
      item := conf.Find('sourcePaths');
      if item.isNotNil then
      begin
        arr := TJSONArray(item);
        for i := 0 to arr.Count-1 do
        begin
          pth := TrimRightSet(arr.Strings[i], ['/','\']);
          if pth.dirExists then
            tryAddFromFolder(pth)
          else
            tryAddFromFolder(fBasePath + pth);
        end;
      end;
      // custom files in current config
      item := conf.Find('sourceFiles');
      if item.isNotNil then
      begin
        arr := TJSONArray(item);
        for i := 0 to arr.Count-1 do
          fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, arr.Strings[i])));
      end;
    end;
    deleteDups(fSrcs);
    // exclusions
    lst.Clear;
    getExclusion(fJSON);
    conf := getCurrentCustomConfig;
    if conf.isNotNil then
      getExclusion(conf);
    for i := fSrcs.Count-1 downto 0 do
      for j := 0 to lst.Count-1 do
        if SameFileName(fSrcs[i], lst[j]) then
          fSrcs.Delete(i);
    // TODO-cDUB: manage exclusions with http://dlang.org/phobos/std_path.html#.globMatch
  finally
    lst.Free;
  end;
end;

function TCEDubProject.findTargetKindInd(value: TJSONObject): boolean;
var
  tt: TJSONData;
begin
  result := true;
  if value.Find('mainSourceFile').isNotNil then
  begin
    fBinKind := executable;
    exit;
  end;
  tt := value.Find('targetType');
  if tt.isNotNil then
  begin
    case tt.AsString of
      'executable': fBinKind := executable;
      'staticLibrary', 'library' : fBinKind := staticlib;
      'dynamicLibrary' : fBinKind := sharedlib;
      'autodetect': result := false;
      else fBinKind := executable;
    end;
  end else result := false;
end;

procedure TCEDubProject.updateTargetKindFromJson;
var
  found: boolean = false;
  conf: TJSONObject;
  src: string;
begin
  fBinKind := executable;
  if fJSON.isNil then exit;
  // note: in Coedit this is only used to known if output can be launched
  found := findTargetKindInd(fJSON);
  conf := getCurrentCustomConfig;
  if conf.isNotNil then
    found := found or findTargetKindInd(conf);
  if not found then
  begin
    for src in fSrcs do
    begin
      if (src = 'source' + DirectorySeparator + 'app.d')
        or (src = 'src' + DirectorySeparator + 'app.d')
        or (src = 'source' + DirectorySeparator + 'main.d')
        or (src = 'src' + DirectorySeparator + 'main.d')
        or (src = 'source' + DirectorySeparator + fPackageName + DirectorySeparator + 'app.d')
        or (src = 'src' + DirectorySeparator + fPackageName + DirectorySeparator + 'app.d')
        or (src = 'source' + DirectorySeparator + fPackageName + DirectorySeparator + 'main.d')
        or (src = 'src' + DirectorySeparator + fPackageName + DirectorySeparator + 'main.d')
      then fBinKind:= executable
      else fBinKind:= staticlib;
    end;
  end;
end;

procedure TCEDubProject.updateImportPathsFromJson;
  procedure addFrom(obj: TJSONObject);
  var
    arr: TJSONArray;
    item: TJSONData;
    i: integer;
  begin
    item := obj.Find('importPaths');
    if assigned(item) then
    begin
      arr := TJSONArray(item);
      for i:= 0 to arr.Count-1 do
        fImportPaths.Add(arr.Strings[i]);
    end;
  end;
  // note: dependencies are added as import to allow DCD completion
  // see TCEDcdWrapper.projChanged()
  procedure addDepsFrom(obj: TJSONObject);
  var
    folds: TStringList;
    deps: TJSONObject;
    item: TJSONData;
    pth: string;
    str: string;
    i,j,k: integer;
  begin
    item := obj.Find('dependencies');
    if assigned(item) then
    begin
      {$IFDEF WINDOWS}
      pth := GetEnvironmentVariable('APPDATA') + '\dub\packages\';
      {$ELSE}
      pth := GetEnvironmentVariable('HOME') + '/.dub/packages/';
      {$ENDIF}
      deps := TJSONObject(item);
      folds := TStringList.Create;
      listFolders(folds, pth);
      try
        // remove semver from folder names
        for i := 0 to folds.Count-1 do
        begin
          str := folds[i];
          k := -1;
          for j := 1 to length(str) do
            if str[j] = '-' then
              k := j;
          if k <> -1 then
            folds[i] := str[1..k-1] + '=' + str[k .. length(str)];
        end;
        // add as import if names match
        for i := 0 to deps.Count-1 do
        begin
          str := pth + deps.Names[i];
          if folds.IndexOfName(str) <> -1 then
          begin
            if (str + folds.Values[str] + DirectorySeparator + 'source').dirExists then
              fImportPaths.Add(str + DirectorySeparator + 'source')
            else if (str + folds.Values[str] + DirectorySeparator + 'src').dirExists then
              fImportPaths.Add(str + DirectorySeparator + 'src');
          end;
        end;
      finally
        folds.Free;
      end;
    end;
  end;
var
  conf: TJSONObject;
begin
  if fJSON.isNil then exit;
  //
  addFrom(fJSON);
  addDepsFrom(fJSON);
  conf := getCurrentCustomConfig;
  if conf.isNotNil then
  begin
    addFrom(conf);
    addDepsFrom(conf);
  end;
end;

procedure TCEDubProject.updateOutputNameFromJson;
var
  conf: TJSONObject;
  item: TJSONData;
  namePart, pathPart: string;
  procedure setFrom(obj: TJSONObject);
  var
    n,p: TJSONData;
  begin
    p := obj.Find('targetPath');
    n := obj.Find('targetName');
    if p.isNotNil then pathPart := p.AsString;
    if n.isNotNil then namePart := n.AsString;
  end;
begin
  fOutputFileName := '';
  if fJSON.isNil then exit;
  item := fJSON.Find('name');
  if item.isNil then exit;

  namePart := item.AsString;
  pathPart := fBasePath;
  setFrom(fJSON);
  conf := getCurrentCustomConfig;
  if conf.isNotNil then
    setFrom(conf);
  pathPart := TrimRightSet(pathPart, ['/','\']);
  {$IFDEF WINDOWS}
  {$ELSE}
  if fBinKind in [staticlib, sharedlib] then
    namePart := 'lib' + namePart;
  {$ENDIF}
  fOutputFileName:= pathPart + DirectorySeparator + namePart;
  patchPlateformPath(fOutputFileName);
  fOutputFileName := expandFilenameEx(fBasePath, fOutputFileName);
  case fBinKind of
    executable: fOutputFileName += exeExt;
    staticlib: fOutputFileName += libExt;
    obj: fOutputFileName += objExt;
    sharedlib: fOutputFileName += dynExt;
  end;
end;

procedure TCEDubProject.updateFields;
begin
  updatePackageNameFromJson;
  udpateConfigsFromJson;
  updateSourcesFromJson;
  updateTargetKindFromJson;
  updateImportPathsFromJson;
  updateOutputNameFromJson;
end;

procedure TCEDubProject.beginModification;
begin
  fModificationCount += 1;
end;

procedure TCEDubProject.endModification;
begin
  fModificationCount -=1;
  if fModificationCount <= 0 then
    doModified;
end;

procedure TCEDubProject.doModified;
begin
  fModificationCount := 0;
  fModified:=true;
  updateFields;
  subjProjChanged(fProjectSubject, self as ICECommonProject);
end;
{$ENDREGION}

{$ENDREGION --------------------------------------------------------------------}

{$REGION Miscellaneous DUB free functions --------------------------------------}
function isValidDubProject(const filename: string): boolean;
var
  maybe: TCEDubProject;
begin
  if (filename.extractFileExt.upperCase <> '.JSON') then
    exit(false);
  result := true;
  // avoid the project to notify the observers, current project is not replaced
  EntitiesConnector.beginUpdate;
  maybe := TCEDubProject.create(nil);
  try
    try
      maybe.loadFromFile(filename);
      if maybe.json.isNil or maybe.filename.isEmpty then
        result := false
      else if maybe.json.Find('name').isNil then
        result := false;
    except
      result := false;
    end;
  finally
    maybe.Free;
    EntitiesConnector.endUpdate;
  end;
end;

function getDubCompiler: TCECompiler;
begin
  exit(DubCompiler);
end;

procedure setDubCompiler(value: TCECompiler);
begin
  case value of
    dmd: DubCompilerFilename := exeFullName('dmd' + exeExt);
    gdc: DubCompilerFilename := exeFullName('gdc' + exeExt);
    ldc: DubCompilerFilename := exeFullName('ldc2' + exeExt);
  end;
  if (not DubCompilerFilename.fileExists) or DubCompilerFilename.isEmpty then
  begin
    value := dmd;
    DubCompilerFilename:= 'dmd' + exeExt;
  end;
  DubCompiler := value;
end;
{$ENDREGION}

initialization
  setDubCompiler(dmd);
end.

