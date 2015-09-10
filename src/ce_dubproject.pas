unit ce_dubproject;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner, process,
  ce_common, ce_interfaces, ce_observer ;

type

  TUpdaterTarget = (configs, files);

  TCEDubProject = class(TComponent, ICECommonProject)
  private
    fFilename: string;
    fModified: boolean;
    fJSON: TJSONObject;
    fSrcs: TStringList;
    fProjectSubject: TCEProjectSubject;
    fConfigsCount: integer;
    fBuildTypes: TStringList;
    fConfigs: TStringList;
    fBuiltTypeIx: integer;
    fConfigIx: integer;
    fBinKind: TProjectBinaryKind;
    //
    procedure updateFields;
    procedure udpateConfigsFromJson;
    procedure updateSourcesFromJson;
    procedure updateTargetKindFromJson;
    function findTargetKindIn(value: TJSONObject): boolean;
    procedure dubProcOutput(proc: TProcess);
    //
    function getFormat: TCEProjectFormat;
    function getProject: TObject;
    //
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    function getFilename: string;
    procedure loadFromFile(const aFilename: string);
    procedure saveToFile(const aFilename: string);
    function getIfModified: boolean;
    function getBinaryKind: TProjectBinaryKind;
    function getCommandLine: string;
    //
    function getIfIsSource(const aFilename: string): boolean;
    function getOutputFilename: string;
    //
    function getConfigurationCount: integer;
    procedure setActiveConfiguration(index: integer);
    function getConfigurationName(index: integer): string;
    //
    function compile: boolean;
    function run(const runArgs: string = ''): boolean;
    //
    property json: TJSONObject read fJSON;
  end;

  // these 9 built types always exist
  TDubBuildType = (plain, debug, release, unittest, docs, ddox, profile, cov, unittestcov);

  // returns true iffilename is a valid dub project. Only json format is supported.
  function isValidDubProject(const filename: string): boolean;

const

  DubBuiltTypeName: array[TDubBuildType] of string = ('plain', 'debug', 'release',
    'unittest', 'docs', 'ddox', 'profile', 'cov', 'unittest-cov'
  );

implementation

constructor TCEDubProject.create(aOwner: TComponent);
begin
  inherited;
  fProjectSubject := TCEProjectSubject.Create;
  fBuildTypes := TStringList.Create;
  fConfigs := TStringList.Create;
  fSrcs := TStringList.Create;
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
  inherited;
end;

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

function TCEDubProject.getFormat: TCEProjectFormat;
begin
  exit(pfDub);
end;

function TCEDubProject.getProject: TObject;
begin
  exit(self);
end;

function TCEDubProject.getFilename: string;
begin
  exit(fFilename);
end;

procedure TCEDubProject.udpateConfigsFromJson;
var
  i: integer;
  builtTypes: TJSONArray = nil;
  configs: TJSONArray = nil;
  item: TJSONObject = nil;
  itemname: string;
begin
  fBuildTypes.Clear;
  fConfigs.Clear;

  // configs: builtype0 - config0, builtype0 - config1, ... , builtype0 - configN
  // builtype1 - config0, builtype1 - config1, ... , builtype1 - configN, etc


  if fJSON.Find('configurations') <> nil then
  begin
    configs := fJSON.Arrays['configurations'];
    for i:= 0 to configs.Count-1 do
    begin
      item := TJSONObject(configs.Items[i]);
      fConfigs.Add(item.Strings['name']);
    end;
  end else
  begin
    fConfigs.Add('(dub default)'); // default
    // default = what dub set as 'application' or 'library'
    // in this case a project will pass nothing to DUB: eg DUB --build=release
  end;

  fBuildTypes.AddStrings(DubBuiltTypeName);
  if fJSON.Find('buildTypes') <> nil then
  begin
    builtTypes := fJSON.Arrays['buildTypes'];
    for i := 0 to builtTypes.Count-1 do
    begin
      item := TJSONObject(builtTypes.Items[i]);
      itemname := item.Strings['name'];
      // defaults build types can be overridden
      if fBuildTypes.IndexOf(itemname) <> -1 then continue;
      fBuildTypes.Add(itemname);
    end;
  end;
  fConfigsCount := fConfigs.Count * fBuildTypes.Count;
end;

procedure TCEDubProject.updateSourcesFromJson;
begin
  //TODO-cDUB: update the source files for the current configuration
end;

function TCEDubProject.findTargetKindIn(value: TJSONObject): boolean;
var
  tt: TJSONData;
begin
  result := true;
  tt := value.Find('targetType');
  if tt <> nil then
  begin
    case tt.AsString of
      'executable': fBinKind := executable;
      'staticLibrary' : fBinKind := staticlib;
      'dynamicLibrary' : fBinKind := sharedlib;
      'autodetect': result := false;
      else fBinKind := executable;
    end;
  end else result := false;
end;

procedure TCEDubProject.updateTargetKindFromJson;
var
  guess: boolean = false;
  item: TJSONData;
  confs: TJSONArray;
  i: integer;
begin
  fBinKind := executable;
  if fJSON = nil then exit;

  // actually for a DUB project this is only used to known if output can be
  // ran from the 'project' menu
  guess := not findTargetKindIn(fJSON);
  if fConfigIx <> 0 then
  begin
    item := fJSON.Find('configurations');
    if item <> nil then
    begin
      confs := TJSONArray(item);
      for i := 0 to confs.Count-1 do
        if TJSONObject(confs.Objects[i]).Find('name') <> nil then
          guess := guess and findTargetKindIn(confs.Objects[i]);
    end;
  end;
  if guess then
  begin
    // TODO-cDUB: guess target kind
    // app.d in source ? exe : lib
  end;
end;

procedure TCEDubProject.updateFields;
begin
  udpateConfigsFromJson;
  updateSourcesFromJson;
  updateTargetKindFromJson;
end;

function TCEDubProject.getBinaryKind: TProjectBinaryKind;
begin
  exit(fBinKind);
end;

procedure TCEDubProject.loadFromFile(const aFilename: string);
var
  loader: TMemoryStream;
  parser : TJSONParser;
begin
  loader := TMemoryStream.Create;
  try
    fFilename:= aFilename;
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

function TCEDubProject.getCommandLine: string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add('dub' + exeExt);
    if fBuiltTypeIx <> 0 then
      str.Add('build=' + fBuildTypes.Strings[fBuiltTypeIx]);
    if fConfigIx <> 0 then
      str.Add('config=' + fConfigs.Strings[fConfigIx]);
    result := str.Text;
  finally
    str.Free;
  end;
end;

function TCEDubProject.getIfModified: boolean;
begin
  exit(fModified);
end;

function TCEDubProject.getIfIsSource(const aFilename: string): boolean;
begin
  //TODO-cDUB: implement getIfIsSource
  exit(false);
end;

function TCEDubProject.getOutputFilename: string;
begin
  //TODO-cDUB: implement getOutputFilename
  exit('');
end;

function TCEDubProject.getConfigurationCount: integer;
begin
  exit(fConfigsCount);
end;

procedure TCEDubProject.setActiveConfiguration(index: integer);
begin
  fBuiltTypeIx := index div fConfigs.Count;
  fConfigIx := index mod fConfigs.Count;
  updateSourcesFromJson;
end;

function TCEDubProject.getConfigurationName(index: integer): string;
begin
  result := fBuildTypes.Strings[index div fConfigs.Count] + ' - ' +
    fConfigs.Strings[index mod fConfigs.Count];
end;

function TCEDubProject.compile: boolean;
var
  dubproc: TProcess;
  olddir: string = '';
  prjname: string;
  msgs: ICEMessagesDisplay;
begin
  result := false;
  msgs := getMessageDisplay;
  msgs.clearByData(Self as ICECommonProject);
  prjname := shortenPath(fFilename);
  dubproc := TProcess.Create(nil);
  getDir(0, olddir);
  try
    msgs.message('compiling ' + prjname, self as ICECommonProject, amcProj, amkInf);
    chDir(extractFilePath(fFilename));
    dubproc.Executable := 'dub' + exeExt;
    dubproc.Options := dubproc.Options + [poStderrToOutPut, poUsePipes];
    dubproc.CurrentDirectory := extractFilePath(fFilename);
    dubproc.ShowWindow := swoHIDE;
    if fBuiltTypeIx <> 0 then
      dubproc.Parameters.Add('build=' + fBuildTypes.Strings[fBuiltTypeIx]);
    if fConfigIx <> 0 then
      dubproc.Parameters.Add('config=' + fConfigs.Strings[fConfigIx]);
    dubproc.Execute;
    while dubproc.Running do
      dubProcOutput(dubproc);
    if dubproc.ExitStatus = 0 then begin
      msgs.message(prjname + ' has been successfully compiled', self as ICECommonProject, amcProj, amkInf);
      result := true;
    end else
      msgs.message(prjname + ' has not been compiled', self as ICECommonProject, amcProj, amkWarn);
  finally
    chDir(olddir);
    dubproc.Free;
  end;
end;

function TCEDubProject.run(const runArgs: string = ''): boolean;
begin
  //TODO-cDUB: implement
  result := false;
end;

function isValidDubProject(const filename: string): boolean;
var
  maybe: TCEDubProject;
begin
  result := true;
  // avoid the project to notify the observers, current project is not replaced
  EntitiesConnector.beginUpdate;
  maybe := TCEDubProject.create(nil);
  EntitiesConnector.removeSubject(maybe);
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

end.

