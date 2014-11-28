unit ce_tools;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, process, asyncprocess,
  ce_common, ce_writableComponent, ce_interfaces, ce_observer;

type

  TCEToolItem = class(TCollectionItem)
  private
    fProcess: TCheckedAsyncProcess;
    fExecutable: string;
    fWorkingDir: string;
    fShowWin: TShowWindowOptions;
    fOpts: TProcessOptions;
    fParameters: TStringList;
    fToolAlias: string;
    //fShortcut: string;
    fLogMessager: TCELogMessageSubject;
    procedure setParameters(const aValue: TStringList);
    procedure processOutput(sender: TObject);
  published
    property toolAlias: string read fToolAlias write fToolAlias;
    property options: TProcessOptions read fOpts write fOpts;
    property executable: string read fExecutable write fExecutable;
    property workingDirectory: string read fWorkingDir write fWorkingDir;
    property parameters: TStringList read fParameters write setParameters;
    property showWindows: TShowWindowOptions read fShowWin write fShowWin;
    //property shortcut: string read fShortcut write fShortcut;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
    //
    procedure execute;
  end;

  TCETools = class(TWritableComponent)
  private
    fTools: TCollection;
    function getTool(index: Integer): TCEToolItem;
    procedure setTools(const aValue: TCollection);
  published
    property tools: TCollection read fTools write setTools;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    function addTool: TCEToolItem;
    property tool[index: integer]: TCEToolItem read getTool;
  end;

const
  toolsFname = 'tools.txt';

Var
  CustomTools: TCETools;

implementation

uses
  ce_symstring;

constructor TCEToolItem.create(ACollection: TCollection);
begin
  inherited;
  fToolAlias := format('<tool %d>', [ID]);
  fParameters := TStringList.create;
  fLogMessager := TCELogMessageSubject.create;
end;

destructor TCEToolItem.destroy;
begin
  fParameters.Free;
  fLogMessager.Free;
  killProcess(fProcess);
  inherited;
end;

procedure TCEToolItem.setParameters(const aValue: TStringList);
begin
  fParameters.Assign(aValue);
end;

procedure TCEToolItem.execute;
var
  i: Integer;
begin
  killProcess(fProcess);
  //
  fProcess := TCheckedAsyncProcess.Create(nil);
  fProcess.OnReadData:= @processOutput;
  fProcess.OnTerminate:= @processOutput;
  fProcess.Options := fOpts;
  fProcess.Executable := symbolExpander.get(fExecutable);
  fProcess.ShowWindow := fShowWin;
  fProcess.CurrentDirectory := symbolExpander.get(fWorkingDir);
  fProcess.Parameters.Clear;
  for i:= 0 to fParameters.Count-1 do
      fProcess.Parameters.AddText(symbolExpander.get(fParameters.Strings[i]));
  fProcess.Execute;
end;

procedure TCEToolItem.processOutput(sender: TObject);
var
  lst: TStringList;
  str: string;
begin
  lst := TStringList.Create;
  try
    processOutputToStrings(fProcess, lst);
    for str in lst do
      subjLmFromString(fLogMessager, str, nil, amcMisc, amkAuto);
  finally
    lst.Free;
  end;
end;

constructor TCETools.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fTools := TCollection.Create(TCEToolItem);
  fname := getDocPath + toolsFname;
  if fileExists(fname) then loadFromFile(fname)
end;

destructor TCETools.destroy;
begin
  forceDirectory(getDocPath);
  saveToFile(getDocPath + toolsFname);
  fTools.Free;
  inherited;
end;

procedure TCETools.setTools(const aValue: TCollection);
begin
  fTools.Assign(aValue);
end;

function TCETools.getTool(index: Integer): TCEToolItem;
begin
  result := TCEToolItem(fTools.Items[index]);
end;

function TCETools.addTool: TCEToolItem;
begin
  result := TCEToolItem(fTools.Add);
end;

initialization
  RegisterClasses([TCEToolItem, TCETools]);
  CustomTools := TCETools.create(nil);
finalization
  CustomTools.Free;
end.
