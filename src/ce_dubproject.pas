unit ce_dubproject;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner, process,
  ce_common, ce_interfaces, ce_observer ;

type

  TCEDubProject = class(TComponent, ICECommonProject)
  private
    fFilename: string;
    fModified: boolean;
    fJson: TJSONObject;
    fProjectSubject: TCEProjectSubject;
    //
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
  end;

implementation

constructor TCEDubProject.create(aOwner: TComponent);
begin
  inherited;
  fProjectSubject := TCEProjectSubject.Create;
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
    subjProjChanged(fProjectSubject, self);
    try
      fJSon := parser.Parse as TJSONObject;
    finally
      parser.Free;
    end;
  finally
    loader.Free;
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
  //TODO-cDUB: implement
  exit(false);
end;

function TCEDubProject.getOutputFilename: string;
begin
  //TODO-cDUB: implement
  exit('');
end;

function TCEDubProject.getConfigurationCount: integer;
begin
  //TODO-cDUB: implement
  exit(0);
end;

procedure TCEDubProject.setActiveConfiguration(index: integer);
begin
  //TODO-cDUB: implement
end;

function TCEDubProject.getConfigurationName(index: integer): string;
begin
  //TODO-cDUB: implement
  exit('');
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
  msgs.clearByData(Self);
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

