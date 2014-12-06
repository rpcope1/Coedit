unit ce_tools;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, process, menus,
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
    fQueryParams: boolean;
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
    property queryParameters: boolean read fQueryParams write fQueryParams;
    //property shortcut: string read fShortcut write fShortcut;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
    //
    procedure execute;
  end;

  TCETools = class(TWritableComponent, ICEMainMenuProvider)
  private
    fTools: TCollection;
    function getTool(index: Integer): TCEToolItem;
    procedure setTools(const aValue: TCollection);
    //
    procedure menuDeclare(item: TMenuItem);
    procedure menuUpdate(item: TMenuItem);
    procedure executeToolFromMenu(sender: TObject);
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
  ce_symstring, dialogs;

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
  prms: string;
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
  if fQueryParams then
  begin
    prms := '';
    if InputQuery('Parameters', '', prms) then
      if prms <> '' then fProcess.Parameters.DelimitedText := symbolExpander.get(prms);
  end;
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
  if fileExists(fname) then loadFromFile(fname);
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCETools.destroy;
begin
  EntitiesConnector.removeObserver(self);
  //
  forceDirectory(getDocPath);
  saveToFile(getDocPath + toolsFname);
  fTools.Free;
  inherited;
end;

procedure TCETools.executeToolFromMenu(sender: TObject);
begin
  TCEToolItem(TMenuItem(sender).tag).execute;
end;

procedure TCETools.menuDeclare(item: TMenuItem);
var
  i: Integer;
  itm: TMenuItem;
begin
  if tools.Count = 0 then exit;
  //
  item.Caption := 'Custom tools';
  item.Clear;
  for i := 0 to tools.Count-1 do begin
    itm := TMenuItem.Create(item);
    itm.Caption := tool[i].toolAlias;
    itm.tag := ptrInt(tool[i]);
    itm.onClick := @executeToolFromMenu;
    item.add(itm);
  end;
end;

procedure TCETools.menuUpdate(item: TMenuItem);
var
  i: Integer;
begin
  if item = nil then exit;
  if item.Count <> tools.Count then
  begin
    menuDeclare(item);
    exit;
  end;
  for i:= 0 to tools.Count-1 do
  begin
    if ptrInt(tool[i]) <> item.Items[i].Tag then
      item.Items[i].Tag := ptrInt(tool[i]);
    if item.Items[i].Caption <> tool[i].toolAlias then
      item.Items[i].Caption := tool[i].toolAlias;
  end;
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
