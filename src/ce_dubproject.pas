unit ce_dubproject;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner, process, strutils,
  ce_common, ce_interfaces, ce_observer;

type

  TCEDubProject = class(TComponent, ICECommonProject)
  private
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
    procedure dubProcOutput(proc: TProcess);
    function getCurrentCustomConfig: TJSONObject;
    function compileOrRun(run: boolean; const runArgs: string = ''): boolean;
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
    function compile: boolean;
    function run(const runArgs: string = ''): boolean;
    function targetUpToDate: boolean;
    //
    property json: TJSONObject read fJSON;
    //property sources: TStringList read fSrcs;
  end;

  // these 9 built types always exist
  TDubBuildType = (plain, debug, release, unittest, docs, ddox, profile, cov, unittestcov);

  // returns true if filename is a valid dub project. Only json format is supported.
  function isValidDubProject(const filename: string): boolean;

const

  DubBuiltTypeName: array[TDubBuildType] of string = ('plain', 'debug', 'release',
    'unittest', 'docs', 'ddox', 'profile', 'cov', 'unittest-cov'
  );

implementation

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCEDubProject.create(aOwner: TComponent);
begin
  inherited;
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
begin
  loader := TMemoryStream.Create;
  try
    fBasePath := extractFilePath(aFilename);
    fFilename := aFilename;
    loader.LoadFromFile(fFilename);
    fJSON.Free;
    parser := TJSONParser.Create(loader);
    try
      fJSON := parser.Parse as TJSONObject;
    finally
      parser.Free;
    end;
  finally
    loader.Free;
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
    saver.Write(str[1], length(str));
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
    if fBuiltTypeIx <> 0 then
      str.Add('--build=' + fBuildTypes.Strings[fBuiltTypeIx]);
    if fConfigIx <> 0 then
      str.Add('--config=' + fConfigs.Strings[fConfigIx]);
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
  if fileExists(fname) then
    fname := ExtractRelativepath(fBasePath, fname);
  result := fSrcs.IndexOf(fname) <> -1;
end;

function TCEDubProject.sourcesCount: integer;
begin
  exit(fSrcs.Count);
end;

function TCEDubProject.sourceRelative(index: integer): string;
begin
  exit(fSrcs.Strings[index]);
end;

function TCEDubProject.sourceAbsolute(index: integer): string;
var
  fname: string;
begin
  fname := fSrcs.Strings[index];
  if FileExists(fname) then
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
  result := expandFilenameEx(fBasePath, fImportPaths.Strings[index]);
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
  result := fBuildTypes.Strings[index div fConfigs.Count] + ' - ' +
    fConfigs.Strings[index mod fConfigs.Count];
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICECommonProject: actions ---------------------------------------------}
procedure TCEDubProject.dubProcOutput(proc: TProcess);
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

function TCEDubProject.compileOrRun(run: boolean; const runArgs: string = ''): boolean;
var
  dubproc: TProcess;
  olddir: string;
  prjname: string;
  msgs: ICEMessagesDisplay;
begin
  result := false;
  msgs := getMessageDisplay;
  msgs.clearByData(Self as ICECommonProject);
  prjname := shortenPath(fFilename);
  dubproc := TProcess.Create(nil);
  olddir := GetCurrentDir;
  try
    if not run then
      msgs.message('compiling ' + prjname, self as ICECommonProject, amcProj, amkInf);
    chDir(extractFilePath(fFilename));
    dubproc.Executable := 'dub' + exeExt;
    dubproc.Options := dubproc.Options + [poStderrToOutPut, poUsePipes];
    dubproc.CurrentDirectory := extractFilePath(fFilename);
    dubproc.ShowWindow := swoHIDE;
    if not run then
      dubproc.Parameters.Add('build')
    else
      dubproc.Parameters.Add('run');
    if fBuiltTypeIx <> 0 then
      dubproc.Parameters.Add('--build=' + fBuildTypes.Strings[fBuiltTypeIx]);
    if fConfigIx <> 0 then
      dubproc.Parameters.Add('--config=' + fConfigs.Strings[fConfigIx]);
    if run and (runArgs <> '') then
      dubproc.Parameters.Add('--' + runArgs);
    dubproc.Execute;
    while dubproc.Running do
      dubProcOutput(dubproc);
    if not run then
    begin
      if dubproc.ExitStatus = 0 then begin
        msgs.message(prjname + ' has been successfully compiled', self as ICECommonProject, amcProj, amkInf);
        result := true;
      end else
        msgs.message(prjname + ' has not been compiled', self as ICECommonProject, amcProj, amkWarn);
    end;
  finally
    chDir(olddir);
    dubproc.Free;
  end;
end;

function TCEDubProject.compile: boolean;
begin
  result := compileOrRun(false);
end;

function TCEDubProject.run(const runArgs: string = ''): boolean;
begin
  result := compileOrRun(true);
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
  if not assigned(item) then exit;
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
  value := fJSON.Find('name');
  if not assigned(value) then fPackageName := ''
  else fPackageName := value.AsString;
end;

procedure TCEDubProject.udpateConfigsFromJson;
var
  i: integer;
  arr: TJSONArray = nil;
  item: TJSONObject = nil;
  itemname: string;
begin
  fBuildTypes.Clear;
  fConfigs.Clear;

  // the CE interface for dub doesn't make a difference betwenn build type and config
  // instead, each possible combination type+build is generated.

  if fJSON.Find('configurations') <> nil then
  begin
    arr := fJSON.Arrays['configurations'];
    for i:= 0 to arr.Count-1 do
    begin
      item := TJSONObject(arr.Items[i]);
      fConfigs.Add(item.Strings['name']);
    end;
  end else
  begin
    fConfigs.Add('(dub default)');
    // default = what dub set as 'application' or 'library'
    // in this case Coedit will pass only the type to DUB: 'DUB --build=release'
  end;

  fBuildTypes.AddStrings(DubBuiltTypeName);
  if fJSON.Find('buildTypes') <> nil then
  begin
    arr := fJSON.Arrays['buildTypes'];
    for i := 0 to arr.Count-1 do
    begin
      item := TJSONObject(arr.Items[i]);
      itemname := item.Strings['name'];
      // defaults build types can be overridden
      if fBuildTypes.IndexOf(itemname) <> -1 then continue;
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
  i: integer;

procedure tryAddFromFolder(const pth: string);
var
  abs: string;
begin
  if DirectoryExists(pth) then
  begin
    lst.Clear;
    listFiles(lst, pth, true);
    for abs in lst do
      if isDlangCompilable(extractFileExt(abs)) then
        fSrcs.Add(ExtractRelativepath(fBasePath, abs));
  end;
end;
var
  pth: string;
begin
  fSrcs.Clear;
  lst := TStringList.Create;
  try
    // auto folders & files
    item := fJSON.Find('mainSourceFile');
    if assigned(item) then
      fSrcs.Add(ExtractRelativepath(fBasePath, item.AsString));
    tryAddFromFolder(fBasePath + 'src');
    tryAddFromFolder(fBasePath + 'source');
    // custom folders
    item := fJSON.Find('sourcePaths');
    if assigned(item) then
    begin
      arr := TJSONArray(item);
      for i := 0 to arr.Count-1 do
      begin
        pth := TrimRightSet(arr.Strings[i], ['/','\']);
        if DirectoryExists(pth) then
          tryAddFromFolder(pth)
        else
          tryAddFromFolder(fBasePath + pth);
      end;
    end;
    // custom files
    item := fJSON.Find('sourceFiles');
    if assigned(item) then
    begin
      arr := TJSONArray(item);
      for i := 0 to arr.Count-1 do
        fSrcs.Add(ExtractRelativepath(fBasePath, arr.Strings[i]));
    end;
    conf := getCurrentCustomConfig;
    if assigned(conf) then
    begin
      item := conf.Find('mainSourceFile');
      if assigned(item) then
        fSrcs.Add(ExtractRelativepath(fBasePath, item.AsString));
      // custom folders in current config
      item := conf.Find('sourcePaths');
      if assigned(item) then
      begin
        arr := TJSONArray(item);
        for i := 0 to arr.Count-1 do
        begin
          pth := TrimRightSet(arr.Strings[i], ['/','\']);
          if DirectoryExists(pth) then
            tryAddFromFolder(pth)
          else
            tryAddFromFolder(fBasePath + pth);
        end;
      end;
      // custom files in current config
      item := conf.Find('sourceFiles');
      if assigned(item) then
      begin
        arr := TJSONArray(item);
        for i := 0 to arr.Count-1 do
          fSrcs.Add(ExtractRelativepath(fBasePath, arr.Strings[i]));
      end;
    end;
    //
    deleteDups(fSrcs);
    // TODO-cDUB: manage exclusions from 'excludedSourceFiles' (global + curr conf)
  finally
    lst.Free;
  end;
end;

function TCEDubProject.findTargetKindInd(value: TJSONObject): boolean;
var
  tt: TJSONData;
begin
  result := true;
  if value.Find('mainSourceFile') <> nil then
  begin
    fBinKind := executable;
    exit;
  end;
  tt := value.Find('targetType');
  if tt <> nil then
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
  if not assigned(fJSON) then exit;
  // note: in Coedit this is only used to known if output can be launched
  found := findTargetKindInd(fJSON);
  conf := getCurrentCustomConfig;
  if assigned(conf) then
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
var
  conf: TJSONObject;
begin
  if not assigned(fJSON) then exit;
  //
  addFrom(fJSON);
  conf := getCurrentCustomConfig;
  if assigned(conf) then addFrom(conf);
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
    if assigned(p) then pathPart := p.AsString;
    if assigned(n) then namePart := n.AsString;
  end;
begin
  fOutputFileName := '';
  item := fJSON.Find('name');
  if not assigned(item) then
    exit;
  namePart := item.AsString;
  pathPart := fBasePath;
  setFrom(fJSON);
  conf := getCurrentCustomConfig;
  if assigned(conf) then
    setFrom(conf);
  pathPart := TrimRightSet(pathPart, ['/','\']);
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

function isValidDubProject(const filename: string): boolean;
var
  maybe: TCEDubProject;
begin
  result := true;
  // avoid the project to notify the observers, current project is not replaced
  EntitiesConnector.beginUpdate;
  maybe := TCEDubProject.create(nil);
  try
    try
      maybe.loadFromFile(filename);
      if maybe.json = nil then
        result := false
      else if maybe.json.Find('name') = nil then
        result := false;
    except
      result := false;
    end;
  finally
    maybe.Free;
    EntitiesConnector.endUpdate;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

end.

