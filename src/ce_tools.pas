unit ce_tools;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, process, menus,
  ce_common, ce_writableComponent, ce_interfaces, ce_observer, ce_inspectors;

type

  TCEToolItem = class(TCollectionItem)
  private
    fProcess: TCheckedAsyncProcess;
    fExecutable: TCEFilename;
    fWorkingDir: TCEPathname;
    fShowWin: TShowWindowOptions;
    fOpts: TProcessOptions;
    fParameters: TStringList;
    fToolAlias: string;
    fQueryParams: boolean;
    fChainBefore: TStringList;
    fChainAfter: TStringList;
    //fShortcut: string;
    fLogMessager: TCELogMessageSubject;
    procedure setParameters(aValue: TStringList);
    procedure setChainBefore(aValue: TStringList);
    procedure setChainAfter(aValue: TStringList);
    procedure processOutput(sender: TObject);
    procedure execute;
  published
    property toolAlias: string read fToolAlias write fToolAlias;
    property options: TProcessOptions read fOpts write fOpts;
    property executable: TCEFilename read fExecutable write fExecutable;
    property workingDirectory: TCEPathname read fWorkingDir write fWorkingDir;
    property parameters: TStringList read fParameters write setParameters;
    property showWindows: TShowWindowOptions read fShowWin write fShowWin;
    property queryParameters: boolean read fQueryParams write fQueryParams;
    property chainBefore: TStringList read fChainBefore write setchainBefore;
    property chainAfter: TStringList read fChainAfter write setChainAfter;
    //property shortcut: string read fShortcut write fShortcut;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
  end;

  TCETools = class(TWritableLfmTextComponent, ICEMainMenuProvider)
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
    procedure executeTool(aTool: TCEToolItem); overload;
    procedure executeTool(aToolIndex: Integer); overload;
    property tool[index: integer]: TCEToolItem read getTool; default;
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
  fChainBefore := TStringList.Create;
  fChainAfter := TStringList.Create;
  fLogMessager := TCELogMessageSubject.create;
end;

destructor TCEToolItem.destroy;
begin
  fParameters.Free;
  fChainAfter.Free;
  fChainBefore.Free;
  fLogMessager.Free;
  killProcess(fProcess);
  inherited;
end;

procedure TCEToolItem.setParameters(aValue: TStringList);
begin
  fParameters.Assign(aValue);
end;

procedure TCEToolItem.setChainBefore(aValue: TStringList);
var
  i: Integer;
begin
  fChainBefore.Assign(aValue);
  i := fChainBefore.IndexOf(fToolAlias);
  if i <> -1 then
    fChainBefore.Delete(i);
end;

procedure TCEToolItem.setChainAfter(aValue: TStringList);
var
  i: Integer;
begin
  fChainAfter.Assign(aValue);
  i := fChainAfter.IndexOf(fToolAlias);
  if i <> -1 then
    fChainAfter.Delete(i);
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
  fname := getCoeditDocPath + toolsFname;
  if fileExists(fname) then loadFromFile(fname);
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCETools.destroy;
begin
  EntitiesConnector.removeObserver(self);
  //
  forceDirectory(getCoeditDocPath);
  saveToFile(getCoeditDocPath + toolsFname);
  fTools.Free;
  inherited;
end;

procedure TCETools.executeToolFromMenu(sender: TObject);
begin
  //TCEToolItem(TMenuItem(sender).tag).execute;
  executeTool(TCEToolItem(TMenuItem(sender).tag));
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

procedure TCETools.executeTool(aTool: TCEToolItem);
var
  nme: string;
  chained: TCollectionItem;
begin
  if aTool = nil then exit;
  if not exeInSysPath(aTool.executable) then
    if (aTool.chainAfter.Count = 0) and (aTool.chainBefore.Count = 0) then
      exit;
  for nme in aTool.chainBefore do
    for chained in fTools do
      if TCEToolItem(chained).toolAlias = nme then
        if TCEToolItem(chained).toolAlias <> aTool.toolAlias then
          TCEToolItem(chained).execute;
  if exeInSysPath(aTool.executable) then
    aTool.execute;
  for nme in aTool.chainAfter do
    for chained in fTools do
      if TCEToolItem(chained).toolAlias = nme then
        if TCEToolItem(chained).toolAlias <> aTool.toolAlias then
          TCEToolItem(chained).execute;
end;

procedure TCETools.executeTool(aToolIndex: Integer);
begin
  if aToolIndex < 0 then exit;
  if aToolIndex > fTools.Count-1 then exit;
  //
  executeTool(tool[aToolIndex]);
end;

initialization
  RegisterClasses([TCEToolItem, TCETools]);
  CustomTools := TCETools.create(nil);
finalization
  CustomTools.Free;
end.
