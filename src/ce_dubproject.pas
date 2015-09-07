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
    fJson: TJSONObject;
    fProjectSubject: TCEProjectSubject;
    fConfigsCount: integer;
    fBuildTypes: TStringList;
    fConfigs: TStringList;
    fBuiltTypeIx: integer;
    fConfigIx: integer;
    //
    procedure updateFields;
    procedure udpateConfigsFromJson;
    procedure updateSourcesFromJson;
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
    property json: TJSONObject read fJson;
  end;

  // these 9 built types alway exist
  TDubBuildType = (plain, debug, release, unittest, docs, ddox, profile, cov, unittestcov);

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
  //
  subjProjNew(fProjectSubject, self);
  subjProjChanged(fProjectSubject, self);
end;

destructor TCEDubProject.destroy;
begin
  subjProjClosing(fProjectSubject, self);
  fProjectSubject.free;
  //
  fJSon.Free;
  fBuildTypes.Free;
  fConfigs.Free;
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


  //fConfigs.Add('(dub default)'); // default

  if fJson.Find('configurations') <> nil then
  begin
    configs := fJson.Arrays['configurations'];
    for i:= 0 to configs.Count-1 do
    begin
      item := TJSONObject(configs.Items[i]);
      fConfigs.Add(item.Strings['name']);
    end;
  end else
  begin
    fConfigs.Add('(dub default)'); // default
    // default = what dub set as 'application' or 'library'
    // in the case a project will pass nothing to DUB: eg DUB --build=release
  end;

  fBuildTypes.AddStrings(DubBuiltTypeName);
  if fJson.Find('buildTypes') <> nil then
  begin
    builtTypes := fJson.Arrays['buildTypes'];
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

procedure TCEDubProject.updateFields;
begin
  udpateConfigsFromJson;
  updateSourcesFromJson;
end;

function TCEDubProject.getBinaryKind: TProjectBinaryKind;
begin
  //TODO-cDUB: implement
  exit(executable);
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
    fJSon.Free;
    parser := TJSONParser.Create(loader);
    try
      fJSon := parser.Parse as TJSONObject;
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
    str := fJson.FormatJSON;
    saver.Write(str[1], length(str));
    saver.SaveToFile(fFilename);
  finally
    saver.Free;
    fModified := false;
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
    dubproc.Parameters.Add('build');
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

end.

