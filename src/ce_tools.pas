unit ce_tools;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, LazFileUtils, process, menus, ce_processes,
  ce_common, ce_writableComponent, ce_interfaces, ce_observer, ce_inspectors,
  ce_synmemo;

type

  TCEToolItems = class;

  TCEToolItem = class(TCollectionItem)
  private
    fToolItems: TCEToolItems;
    fNextToolAlias: string;
    fProcess: TCEProcess;
    fExecutable: TCEFilename;
    fWorkingDir: TCEPathname;
    fShowWin: TShowWindowOptions;
    fOpts: TProcessOptions;
    fParameters: TStringList;
    fToolAlias: string;
    fQueryParams: boolean;
    fClearMessages: boolean;
    fEditorToInput: boolean;
    fOutputToNext: boolean;
    fShortcut: TShortcut;
    fMsgs: ICEMessagesDisplay;
    procedure setParameters(value: TStringList);
    procedure processOutput(sender: TObject);
    procedure setToolAlias(value: string);
    //
    procedure setChainBefore(value: TStringList);
    procedure setChainAfter(value: TStringList);
  published
    property toolAlias: string read fToolAlias write setToolAlias;
    property options: TProcessOptions read fOpts write fOpts;
    property executable: TCEFilename read fExecutable write fExecutable;
    property workingDirectory: TCEPathname read fWorkingDir write fWorkingDir;
    property parameters: TStringList read fParameters write setParameters;
    property showWindows: TShowWindowOptions read fShowWin write fShowWin;
    property queryParameters: boolean read fQueryParams write fQueryParams;
    property clearMessages: boolean read fClearMessages write fClearMessages;
    property editorToInput: boolean read fEditorToInput write fEditorToInput;
    property shortcut: TShortcut read fShortcut write fShortcut;
    property nextToolAlias: string read fNextToolAlias write fNextToolAlias;
    property outputToNext: boolean read fOutputToNext write fOutputToNext;
    //
    property chainBefore: TStringList write setChainBefore stored false; deprecated;
    property chainAfter: TStringList write setChainAfter stored false; deprecated;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
    procedure assign(Source: TPersistent); override;
    //
    procedure execute(previous: TCEToolItem);
    property process: TCEProcess read fProcess;
  end;

  TCEToolItems = class(TCollection)
  public
    function findTool(const value: string): TCEToolItem;
  end;

  TCETools = class(TWritableLfmTextComponent, ICEMainMenuProvider, ICEEditableShortcut, ICEMultiDocObserver)
  private
    fTools: TCEToolItems;
    fShctCount: Integer;
    fDoc: TCESynMemo;
    function getTool(index: Integer): TCEToolItem;
    procedure setTools(value: TCEToolItems);
    //
    procedure menuDeclare(item: TMenuItem);
    procedure menuUpdate(item: TMenuItem);
    procedure executeToolFromMenu(sender: TObject);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    //
    function scedWantFirst: boolean;
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
  published
    property tools: TCEToolItems read fTools write setTools;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    function addTool: TCEToolItem;
    procedure executeTool(aTool: TCEToolItem); overload;
    procedure executeTool(aToolIndex: Integer); overload;
    property tool[index: integer]: TCEToolItem read getTool; default;
  end;

//TODO-crefactor: either set the tools as a service of merge the tools collection& tool editor in a single unit.

var
  CustomTools: TCETools;

implementation

uses
  ce_symstring, dialogs;

const
  toolsFname = 'tools.txt';

{$REGION TCEToolItem -----------------------------------------------------------}
function TCEToolItems.findTool(const value: string): TCEToolItem;
var
  item: TCollectionItem;
begin
  for item in self do
    if TCEToolItem(item).toolAlias = value then
      exit(TCEToolItem(item));
  exit(nil);
end;

constructor TCEToolItem.create(ACollection: TCollection);
begin
  inherited;
  fToolItems  := TCEToolItems(ACollection);
  fToolAlias  := format('<tool %d>', [ID]);
  fParameters := TStringList.create;
end;

destructor TCEToolItem.destroy;
begin
  fParameters.Free;
  ce_processes.killProcess(fProcess);
  inherited;
end;

procedure TCEToolItem.setChainBefore(value: TStringList);
begin
  // kept to reload old setting files. 'chainBefore' is not saved anymore.
end;

procedure TCEToolItem.setChainAfter(value: TStringList);
begin
  // kept to reload old setting files. 'chainAfter' is not saved anymore.
end;

procedure TCEToolItem.assign(Source: TPersistent);
var
  tool: TCEToolItem;
begin
  // only used to clone a tool: so don't copy everything.
  if Source is TCEToolItem then
  begin
    tool := TCEToolItem(Source);
    //
    toolAlias         := tool.toolAlias;
    queryParameters   := tool.queryParameters;
    clearMessages     := tool.clearMessages;
    options           := tool.options;
    executable        := tool.executable;
    workingDirectory  := tool.workingDirectory;
    editorToInput     := tool.editorToInput;
    showWindows       := tool.showWindows;
    parameters.Assign(tool.parameters);
  end
  else inherited;
end;

procedure TCEToolItem.setParameters(value: TStringList);
begin
  fParameters.Assign(value);
end;

procedure TCEToolItem.setToolAlias(value: string);
var
  i: integer = 0;
begin
  while fToolItems.findTool(value) <> nil do
  begin
    value += intToStr(i);
    i += 1;
  end;
  fToolAlias := value;
end;

procedure TCEToolItem.execute(previous: TCEToolItem);
var
  prm: string;
  inp: string;
begin
  ce_processes.killProcess(fProcess);
  //
  if fClearMessages then
    getMessageDisplay(fMsgs).clearByContext(amcMisc);
  //
  fProcess := TCEProcess.Create(nil);
  fProcess.OnReadData:= @processOutput;
  fProcess.OnTerminate:= @processOutput;
  fProcess.Options := fOpts;
  fProcess.Executable := exeFullName(symbolExpander.get(fExecutable));
  fProcess.ShowWindow := fShowWin;
  fProcess.CurrentDirectory := symbolExpander.get(fWorkingDir);
  for prm in fParameters do if not isStringDisabled(prm) then
    fProcess.Parameters.AddText(symbolExpander.get(prm));
  if fQueryParams then
  begin
    prm := '';
    if InputQuery('Parameters', '', prm) then
      if prm <> '' then fProcess.Parameters.AddText(symbolExpander.get(prm));
  end;
  ensureNoPipeIfWait(fProcess);
  //
  if FileExists(fProcess.Executable) then
  begin
    fProcess.Execute;
    if (previous <> nil) and (previous.outputToNext)
      and (poUsePipes in previous.Options) and (poUsePipes in Options) then
    begin
      setLength(inp, previous.process.OutputStack.Size);
      previous.process.OutputStack.Position:=0;
      previous.process.OutputStack.Read(inp[1], length(inp));
      fProcess.Input.Write(inp[1], length(inp));
      fProcess.CloseInput;
    end;
  end;
end;

procedure TCEToolItem.processOutput(sender: TObject);
var
  lst: TStringList;
  str: string;
  nxt: TCEToolItem;
begin
  if ((not fOutputToNext) or (fNextToolAlias = '')) and (poUsePipes in options) then
  begin
    getMessageDisplay(fMsgs);
    lst := TStringList.Create;
    try
      fProcess.getFullLines(lst);
      for str in lst do
        fMsgs.message(str, nil, amcMisc, amkAuto);
    finally
      lst.Free;
    end;
  end;
  if (not fProcess.Running) and (fNextToolAlias <> '') then
  begin
    nxt := fToolItems.findTool(fNextToolAlias);
    if assigned(nxt) then nxt.execute(self);
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCETools.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fTools := TCEToolItems.Create(TCEToolItem);
  fname := getCoeditDocPath + toolsFname;
  if fileExists(fname) then loadFromFile(fname);
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCETools.destroy;
begin
  EntitiesConnector.removeObserver(self);
  //
  ForceDirectoriesUTF8(getCoeditDocPath);
  saveToFile(getCoeditDocPath + toolsFname);
  fTools.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICEMainMenuProvider ---------------------------------------------------}
procedure TCETools.executeToolFromMenu(sender: TObject);
begin
  executeTool(TCEToolItem(TMenuItem(sender).tag));
end;

procedure TCETools.menuDeclare(item: TMenuItem);
var
  i: Integer;
  itm: TMenuItem;
  colitm: TCEToolItem;
begin
  if tools.Count = 0 then exit;
  //
  item.Caption := 'Custom tools';
  item.Clear;
  for i := 0 to tools.Count-1 do
  begin
    colitm := tool[i];
    //
    itm := TMenuItem.Create(item);
    itm.ShortCut:= colitm.shortcut;
    itm.Caption := colitm.toolAlias;
    itm.tag := ptrInt(colitm);
    itm.onClick := @executeToolFromMenu;
    item.add(itm);
  end;
end;

procedure TCETools.menuUpdate(item: TMenuItem);
var
  i: Integer;
  colitm: TCEToolItem;
  mnuitm: TMenuItem;
begin
  if item = nil then exit;
  if item.Count <> tools.Count then
    menuDeclare(item)
  else for i:= 0 to tools.Count-1 do
  begin
    colitm := tool[i];
    mnuitm := item.Items[i];
    //
    if mnuitm.Tag <> ptrInt(colitm) then
      mnuitm.Tag := ptrInt(colitm);
    if mnuitm.Caption <> colitm.toolAlias then
      mnuitm.Caption := colitm.toolAlias;
    if mnuitm.shortcut <> colitm.shortcut then
      mnuitm.shortcut := colitm.shortcut;
  end;
end;
{$ENDREGION}

{$REGION ICEEditableShortcut ---------------------------------------------------}
function TCETools.scedWantFirst: boolean;
begin
  result := fTools.Count > 0;
  fShctCount := 0;
end;

function TCETools.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
begin
  category  := 'Tools';
  identifier:= tool[fShctCount].toolAlias;
  aShortcut := tool[fShctCount].shortcut;
  //
  fShctCount += 1;
  result := fShctCount < fTools.Count;
end;

procedure TCETools.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
var
  i: Integer;
begin
 if category <> 'Tools' then exit;
 //
 for i := 0 to tools.Count-1 do if tool[i].toolAlias = identifier then
 begin
   tool[i].shortcut := aShortcut;
   break;
 end;
end;
{$ENDREGION}

{$REGION ICEMultidocObserver ---------------------------------------------------}
procedure TCETools.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCETools.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCETools.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCETools.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION Tools things ----------------------------------------------------------}
procedure TCETools.setTools(value: TCEToolItems);
begin
  fTools.Assign(value);
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
  txt: string;
begin
  if aTool = nil then exit;
  //
  aTool.execute(nil);
  if aTool.editorToInput and assigned(fDoc) and (poUsePipes in aTool.options)
    and (aTool.fProcess.Input <> nil) then
  begin
    txt := fDoc.Text;
    aTool.fProcess.Input.Write(txt[1], length(txt));
    aTool.fProcess.CloseInput;
  end;
end;

procedure TCETools.executeTool(aToolIndex: Integer);
begin
  if aToolIndex < 0 then exit;
  if aToolIndex > fTools.Count-1 then exit;
  //
  executeTool(tool[aToolIndex]);
end;
{$ENDREGION}

initialization
  RegisterClasses([TCEToolItem, TCETools]);
  CustomTools := TCETools.create(nil);
finalization
  CustomTools.Free;
end.
