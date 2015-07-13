unit ce_symlist;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, ExtCtrls, Menus,
  ComCtrls, ce_widget, jsonparser, process, actnlist, Buttons, Clipbrd, LCLProc,
  ce_common, ce_observer, ce_synmemo, ce_interfaces, ce_writableComponent;

type

  // Enumerates the possible symbol kind. To be kept in sync with the tool declaration.
  TSymbolType = (
      _alias,
      _class,
      _enum,
      _error,
      _function,
      _interface,
      _import,
      _mixin,
      _struct,
      _template,
      _union,
      _variable,
      _warning
  );

  TSymbolCollection = class;

  // Encapsulates a symbol to enable structured serialization
  TSymbol = class(TCollectionItem)
  private
    fline, fCol: nativeUint;
    fName: string;
    fType: TSymbolType;
    fSubs: TSymbolCollection;
    procedure setSubs(aValue: TSymbolCollection);
  published
    property line: nativeUint read fline write fLine;
    property col: nativeUint read fCol write fCol;
    property name: string read fName write fName;
    property symType: TSymbolType read fType write fType;
    property subs: TSymbolCollection read fSubs write setSubs;
  public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
  end;

  // Encapsulates a the sub symbols.
  TSymbolCollection = class(TCollection)
  private
    function getSub(index: Integer): TSymbol;
  public
    constructor create;
    property sub[index: Integer]: TSymbol read getSub; default;
  end;

  // Serializable symbol list
  TSymbolList = class(TComponent)
  private
    fSymbols: TSymbolCollection;
    procedure setSymbols(aValue: TSymbolCollection);
  published
    property symbols: TSymbolCollection read fSymbols write setSymbols;
  public
    constructor create(aOwner: TCOmponent); override;
    destructor destroy; override;
    //
    procedure LoadFromTool(str: TStream);
  end;

  TCESymbolListOptions = class(TWritableLfmTextComponent)
  private
    fAutoRefresh: boolean;
    fRefreshOnChange: boolean;
    fRefreshOnFocus: boolean;
    fShowChildCategories: boolean;
    fAutoRefreshDelay: Integer;
    fSmartFilter: boolean;
    fAutoExpandErrors: boolean;
    fSortSymbols: boolean;
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
    property refreshOnChange: boolean read fRefreshOnChange write fRefreshOnChange;
    property refreshOnFocus: boolean read fRefreshOnFocus write fRefreshOnFocus;
    property showChildCategories: boolean read fShowChildCategories write fShowChildCategories;
    property autoRefreshDelay: Integer read fAutoRefreshDelay write fAutoRefreshDelay;
    property smartFilter: boolean read fSmartFilter write fSmartFilter;
    property autoExpandErrors: boolean read fAutoExpandErrors write fAutoExpandErrors;
    property sortSymbols: boolean read fSortSymbols write fSortSymbols;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  { TCESymbolListWidget }

  TCESymbolListWidget = class(TCEWidget, ICEMultiDocObserver, ICEEditableOptions)
    btnRefresh: TBitBtn;
    imgList: TImageList;
    Panel1: TPanel;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnRefreshClick(Sender: TObject);
    procedure TreeCompare(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer);
    procedure TreeFilterEdit1AfterFilter(Sender: TObject);
    function TreeFilterEdit1FilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure TreeFilterEdit1MouseEnter(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
  private
    fHasToolExe: boolean;
    fOptions: TCESymbolListOptions;
    fSyms: TSymbolList;
    fMsgs: ICEMessagesDisplay;
    fToolProc: TCheckedAsyncProcess;
    fActCopyIdent: TAction;
    fActRefresh: TAction;
    fActRefreshOnChange: TAction;
    fActRefreshOnFocus: TAction;
    fActAutoRefresh: TAction;
    fActSelectInSource: TAction;
    fDoc: TCESynMemo;
    fAutoRefresh: boolean;
    fRefreshOnChange: boolean;
    fRefreshOnFocus: boolean;
    fShowChildCategories: boolean;
    fSmartFilter: boolean;
    fAutoExpandErrors: boolean;
    fSortSymbols: boolean;
    fToolOutput: TMemoryStream;
    ndAlias, ndClass, ndEnum, ndFunc, ndUni: TTreeNode;
    ndImp, ndIntf, ndMix, ndStruct, ndTmp: TTreeNode;
    ndVar, ndWarn, ndErr: TTreeNode;
    procedure TreeDblClick(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAutoRefreshExecute(Sender: TObject);
    procedure actRefreshOnChangeExecute(Sender: TObject);
    procedure actRefreshOnFocusExecute(Sender: TObject);
    procedure actCopyIdentExecute(Sender: TObject);
    procedure updateVisibleCat;
    procedure clearTree;
    procedure expandCurrentDeclaration;
    //
    procedure checkIfHasToolExe;
    procedure callToolProc;
    procedure toolOutputData(sender: TObject);
    procedure toolTerminated(sender: TObject);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  protected
    procedure updateDelayed; override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure SetVisible(Value: boolean); override;
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
    property refreshOnChange: boolean read fRefreshOnChange write fRefreshOnChange;
    property refreshOnFocus: boolean read fRefreshOnFocus write fRefreshOnFocus;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  OptsFname = 'symbollist.txt';
  toolExeName = 'cesyms' + exeExt;

{$REGION Serializable symbols---------------------------------------------------}
constructor TSymbol.create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fSubs := TSymbolCollection.create;
end;

destructor TSymbol.destroy;
begin
  fSubs.Free;
  inherited;
end;

procedure TSymbol.setSubs(aValue: TSymbolCollection);
begin
  fSubs.Assign(aValue);
end;

constructor TSymbolCollection.create;
begin
  inherited create(TSymbol);
end;

function TSymbolCollection.getSub(index: Integer): TSymbol;
begin
  exit(TSymbol(self.Items[index]));
end;

constructor TSymbolList.create(aOwner: TCOmponent);
begin
  inherited;
  fSymbols := TSymbolCollection.create;
end;

destructor TSymbolList.destroy;
begin
  fSymbols.free;
  inherited;
end;

procedure TSymbolList.setSymbols(aValue: TSymbolCollection);
begin
  fSymbols.Assign(aValue);
end;

procedure TSymbolList.LoadFromTool(str: TStream);
var
  bin: TMemoryStream;
begin
  bin := TMemoryStream.Create;
  try
    str.Position:=0;
    try
      ObjectTextToBinary(str, bin);
    except
      exit;
    end;
    bin.Position:=0;
    bin.ReadComponent(self);
  finally
    bin.Free;
  end;
end;
{$ENDREGION}

{$REGION TCESymbolListOptions --------------------------------------------------}
constructor  TCESymbolListOptions.Create(AOwner: TComponent);
begin
  inherited;
  fRefreshOnFocus := true;
  fShowChildCategories := true;
  fAutoExpandErrors := true;
  fSmartFilter := true;
  fSortSymbols := false;
  fAutoRefreshDelay := 1500;
end;

procedure TCESymbolListOptions.Assign(Source: TPersistent);
var
  widg: TCESymbolListWidget;
begin
  if Source is TCESymbolListWidget then
  begin
    widg := TCESymbolListWidget(Source);
    //
    fAutoRefreshDelay     := widg.updaterByDelayDuration;
    fRefreshOnFocus       := widg.fRefreshOnFocus;
    fRefreshOnChange      := widg.fRefreshOnChange;
    fAutoRefresh          := widg.fAutoRefresh;
    fShowChildCategories  := widg.fShowChildCategories;
    fSmartFilter          := widg.fSmartFilter;
    fAutoExpandErrors     := widg.fAutoExpandErrors;
    fSortSymbols          := widg.fSortSymbols;
  end
  else inherited;
end;

procedure TCESymbolListOptions.AssignTo(Dest: TPersistent);
var
  widg: TCESymbolListWidget;
begin
  if Dest is TCESymbolListWidget then
  begin
    widg := TCESymbolListWidget(Dest);
    //
    widg.updaterByDelayDuration := fAutoRefreshDelay;
    widg.fRefreshOnFocus        := fRefreshOnFocus;
    widg.fRefreshOnChange       := fRefreshOnChange;
    widg.fAutoRefresh           := fAutoRefresh;
    widg.fShowChildCategories   := fShowChildCategories;
    widg.fSmartFilter           := fSmartFilter;
    widg.fAutoExpandErrors      := fAutoExpandErrors;
    widg.fSortSymbols           := fSortSymbols;
    //
    widg.fActAutoRefresh.Checked    := fAutoRefresh;
    widg.fActRefreshOnChange.Checked:= fRefreshOnChange;
    widg.fActRefreshOnFocus.Checked := fRefreshOnFocus;
    //
    //if fSortSymbols then
    //  widg.Tree.SortType := stText
    //else
    //  widg.Tree.SortType:= stNone;
  end
  else inherited;
end;
{$ENDREGIOn}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCESymbolListWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
  fname: string;
begin
  fAutoRefresh := false;
  fRefreshOnFocus := true;
  fRefreshOnChange := false;
  checkIfHasToolExe;
  //
  fActCopyIdent := TAction.Create(self);
  fActCopyIdent.OnExecute:=@actCopyIdentExecute;
  fActCopyIdent.Caption := 'Copy identifier';
  fActRefresh := TAction.Create(self);
  fActRefresh.OnExecute := @actRefreshExecute;
  fActRefresh.Caption := 'Refresh';
  fActAutoRefresh := TAction.Create(self);
  fActAutoRefresh.OnExecute := @actAutoRefreshExecute;
  fActAutoRefresh.Caption := 'Auto-refresh';
  fActAutoRefresh.AutoCheck := true;
  fActAutoRefresh.Checked := fAutoRefresh;
  fActRefreshOnChange := TAction.Create(self);
  fActRefreshOnChange.OnExecute := @actRefreshOnChangeExecute;
  fActRefreshOnChange.Caption := 'Refresh on change';
  fActRefreshOnChange.AutoCheck := true;
  fActRefreshOnChange.Checked := fRefreshOnChange;
  fActRefreshOnFocus := TAction.Create(self);
  fActRefreshOnFocus.OnExecute := @actRefreshOnFocusExecute;
  fActRefreshOnFocus.Caption := 'Refresh on focused';
  fActRefreshOnFocus.AutoCheck := true;
  fActRefreshOnFocus.Checked := fRefreshOnFocus;
  fActSelectInSource := TAction.Create(self);
  fActSelectInSource.OnExecute := @TreeDblClick;
  fActSelectInSource.Caption := 'Select in source';
  //
  inherited;
  // allow empty name if owner is nil
  fSyms := TSymbolList.create(nil);
  fToolOutput := TMemoryStream.create;
  //
  fOptions := TCESymbolListOptions.Create(self);
  fOptions.Name:= 'symbolListOptions';
  fname := getCoeditDocPath + OptsFname;
  if FileExists(fname) then
  fOptions.loadFromFile(fname);
  fOptions.AssignTo(self);
  //
  ndAlias   := Tree.Items[0];
  ndClass   := Tree.Items[1];
  ndEnum    := Tree.Items[2];
  ndFunc    := Tree.Items[3];
  ndImp     := Tree.Items[4];
  ndIntf    := Tree.Items[5];
  ndMix     := Tree.Items[6];
  ndStruct  := Tree.Items[7];
  ndTmp     := Tree.Items[8];
  ndUni     := Tree.Items[9];
  ndVar     := Tree.Items[10];
  ndWarn    := Tree.Items[11];
  ndErr     := Tree.Items[12];
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('arrow_update');
    btnRefresh.Glyph.Assign(png);
  finally
    png.Free;
  end;
  //
  Tree.OnDblClick := @TreeDblClick;
  Tree.PopupMenu := contextMenu;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCESymbolListWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  //
  killProcess(fToolProc);
  fToolOutput.free;
  fSyms.Free;
  //
  fOptions.saveToFile(getCoeditDocPath + OptsFname);
  fOptions.Free;
  //
  inherited;
end;

procedure TCESymbolListWidget.SetVisible(Value: boolean);
begin
  inherited;
  checkIfHasToolExe;
  getMessageDisplay(fMsgs);
  if Value then
    callToolProc;
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCESymbolListWidget.contextName: string;
begin
  result := 'Static explorer';
end;

function TCESymbolListWidget.contextActionCount: integer;
begin
  result := 6;
end;

function TCESymbolListWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActSelectInSource);
    1: exit(fActCopyIdent);
    2: exit(fActRefresh);
    3: exit(fActAutoRefresh);
    4: exit(fActRefreshOnChange);
    5: exit(fActRefreshOnFocus);
    else result := nil;
  end;
end;

procedure TCESymbolListWidget.actRefreshExecute(Sender: TObject);
begin
  if Updating then exit;
  callToolProc;
end;

procedure TCESymbolListWidget.actAutoRefreshExecute(Sender: TObject);
begin
  autoRefresh := fActAutoRefresh.Checked;
  fOptions.Assign(self);
end;

procedure TCESymbolListWidget.actRefreshOnChangeExecute(Sender: TObject);
begin
  refreshOnChange := fActRefreshOnChange.Checked;
  fOptions.Assign(self);
end;

procedure TCESymbolListWidget.actRefreshOnFocusExecute(Sender: TObject);
begin
  refreshOnFocus := fActRefreshOnFocus.Checked;
  fOptions.Assign(self);
end;

procedure TCESymbolListWidget.actCopyIdentExecute(Sender: TObject);
begin
  if Tree.Selected = nil then exit;
  Clipboard.AsText:= Tree.Selected.Text;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCESymbolListWidget.optionedWantCategory(): string;
begin
  exit('Symbol list');
end;

function TCESymbolListWidget.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCESymbolListWidget.optionedWantContainer: TPersistent;
begin
  fOptions.Assign(self);
  exit(fOptions);
end;

procedure TCESymbolListWidget.optionedEvent(anEvent: TOptionEditorEvent);
begin
  if anEvent <> oeeAccept then exit;
  fOptions.AssignTo(self);
  callToolProc;
end;

function TCESymbolListWidget.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCESymbolListWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  beginDelayedUpdate;
end;

procedure TCESymbolListWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
  clearTree;
  updateVisibleCat;
end;

procedure TCESymbolListWidget.docFocused(aDoc: TCESynMemo);
begin
  if fDoc = aDoc then exit;
  fDoc := aDoc;
  if not Visible then exit;
  //
  if fAutoRefresh then beginDelayedUpdate
  else if fRefreshOnFocus then callToolProc;
end;

procedure TCESymbolListWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  if not Visible then exit;
  //
  if fAutoRefresh then beginDelayedUpdate
  else if fRefreshOnChange then callToolProc;
  //
  //expandCurrentDeclaration;
end;
{$ENDREGION}

{$REGION Symbol-tree things ----------------------------------------------------}
procedure TCESymbolListWidget.updateDelayed;
begin
  if not fAutoRefresh then exit;
  callToolProc;
end;

procedure TCESymbolListWidget.btnRefreshClick(Sender: TObject);
begin
  fActRefresh.Execute;
end;

procedure TCESymbolListWidget.TreeCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
begin
  Compare := CompareStr(Node1.Text, Node2.text);
end;

procedure TCESymbolListWidget.updateVisibleCat;
begin
  if (fDoc <> nil) then
  begin
    ndAlias.Visible := ndAlias.Count > 0;
    ndClass.Visible := ndClass.Count > 0;
    ndEnum.Visible  := ndEnum.Count > 0;
    ndFunc.Visible  := ndFunc.Count > 0;
    ndImp.Visible   := ndImp.Count > 0;
    ndIntf.Visible  := ndIntf.Count > 0;
    ndMix.Visible   := ndMix.Count > 0;
    ndStruct.Visible:= ndStruct.Count > 0;
    ndTmp.Visible   := ndTmp.Count > 0;
    ndUni.Visible   := ndUni.Count > 0;
    ndVar.Visible   := ndVar.Count > 0;
    ndWarn.Visible  := ndWarn.Count > 0;
    ndErr.Visible   := ndErr.Count > 0;
  end else
  begin
    ndAlias.Visible := true;
    ndClass.Visible := true;
    ndEnum.Visible  := true;
    ndFunc.Visible  := true;
    ndImp.Visible   := true;
    ndIntf.Visible  := true;
    ndMix.Visible   := true;
    ndStruct.Visible:= true;
    ndTmp.Visible   := true;
    ndUni.Visible   := true;
    ndVar.Visible   := true;
    ndWarn.Visible  := true;
    ndErr.Visible   := true;
  end;
end;

procedure TCESymbolListWidget.clearTree;
begin
  ndAlias.DeleteChildren;
  ndClass.DeleteChildren;
  ndEnum.DeleteChildren;
  ndFunc.DeleteChildren;
  ndImp.DeleteChildren;
  ndIntf.DeleteChildren;
  ndMix.DeleteChildren;
  ndStruct.DeleteChildren;
  ndTmp.DeleteChildren;
  ndUni.DeleteChildren;
  ndVar.DeleteChildren;
  ndWarn.DeleteChildren;
  ndErr.DeleteChildren;
end;

procedure TCESymbolListWidget.TreeFilterEdit1AfterFilter(Sender: TObject);
begin
  if TreeFilterEdit1.Filter ='' then
    updateVisibleCat;
end;

function TCESymbolListWidget.TreeFilterEdit1FilterItem(Item: TObject; out
  Done: Boolean): Boolean;
begin
  if not fSmartFilter then exit;
  //
  if TreeFilterEdit1.Filter <> '' then
    tree.FullExpand
  else if tree.Selected = nil then
    tree.FullCollapse
  else tree.MakeSelectionVisible;
  result := false;
end;

procedure TCESymbolListWidget.TreeFilterEdit1MouseEnter(Sender: TObject);
begin
  if not fSmartFilter then exit;
  //
  tree.Selected := nil;
end;

procedure TCESymbolListWidget.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then TreeDblClick(nil);
end;

procedure TCESymbolListWidget.TreeDblClick(Sender: TObject);
var
  line: NativeUint;
begin
  if fDoc = nil then exit;
  if Tree.Selected = nil then exit;
  if Tree.Selected.Data = nil then exit;
  //
  {$HINTS OFF}
  line := NativeUInt(Tree.Selected.Data);
  {$HINTS ON}
  fDoc.CaretY := line;
  fDoc.SelectLine;
end;

procedure TCESymbolListWidget.checkIfHasToolExe;
begin
  fHasToolExe := exeInSysPath(toolExeName);
end;

procedure TCESymbolListWidget.callToolProc;
var
  srcFname: string;
begin
  if not fHasToolExe then exit;
  if fDoc = nil then exit;
  if fDoc.Lines.Count = 0 then exit;
  if not fDoc.isDSource then exit;

  // standard process options
  killProcess(fToolProc);
  fToolProc := TCheckedAsyncProcess.Create(nil);
  fToolProc.ShowWindow := swoHIDE;
  fToolProc.Options := [poUsePipes];
  fToolProc.Executable := exeFullName(toolExeName);
  fToolProc.OnTerminate := @toolTerminated;
  fToolProc.OnReadData  := @toolOutputData;
  fToolProc.CurrentDirectory := ExtractFileDir(Application.ExeName);

  // focused source
  srcFname := fDoc.fileName;
  if not fileExists(srcFname) or (srcFname = fDoc.tempFilename) then
    fDoc.saveTempFile;
  srcFname := fDoc.fileName;
  fToolProc.Parameters.Add(srcFname);

  fToolProc.Execute;
end;

procedure TCESymbolListWidget.toolOutputData(sender: TObject);
begin
  processOutputToStream(TProcess(sender), fToolOutput);
end;

procedure TCESymbolListWidget.toolTerminated(sender: TObject);
//
function getCatNode(node: TTreeNode; stype: TSymbolType ): TTreeNode;
  //
  function newCat(const aCat: string): TTreeNode;
  begin
    result := node.FindNode(aCat);
    if result = nil then result := node.TreeNodes.AddChild(node, aCat);
  end;
  //
begin
  if node = nil then case stype of
    _alias    : exit(ndAlias);
    _class    : exit(ndClass);
    _enum     : exit(ndEnum);
    _function : exit(ndFunc);
    _import   : exit(ndImp);
    _interface: exit(ndIntf);
    _mixin    : exit(ndMix);
    _struct   : exit(ndStruct);
    _template : exit(ndTmp);
    _union    : exit(ndUni);
    _variable : exit(ndVar);
    _warning  : exit(ndWarn);
    _error    : exit(ndErr);
  end else case stype of
    _alias:     exit(newCat('Alias'));
    _class:     exit(newCat('Class'));
    _enum:      exit(newCat('Enum'));
    _function:  exit(newCat('Function'));
    _import:    exit(newCat('Import'));
    _interface: exit(newCat('Interface'));
    _mixin:     exit(newCat('Mixin'));
    _struct:    exit(newCat('Struct'));
    _template:  exit(newCat('Template'));
    _union:     exit(newCat('Union'));
    _variable:  exit(newCat('Variable'));
    _warning:   exit(ndWarn);
    _error:     exit(ndErr);
    end;
end;
//
procedure symbolToTreeNode(origin: TTreenode; sym: TSymbol);
var
  cat: TTreeNode;
  node: TTreeNode;
  i: Integer;
begin
  cat   := getCatNode(origin, sym.symType);
  {$HINTS OFF}
  node  := tree.Items.AddChildObject(cat, sym.name, Pointer(sym.fline));
  {$HINTS ON}
  if not fShowChildCategories then node := nil;
  cat.Visible:=true;
  for i := 0 to sym.subs.Count-1 do
    symbolToTreeNode(node, sym.subs[i]);
end;
//
var
  i: Integer;
begin
  if ndAlias = nil then exit;
  clearTree;
  updateVisibleCat;
  if fDoc = nil then exit;
  //
  processOutputToStream(TProcess(sender), fToolOutput);
  fToolOutput.Position := 0;
  fSyms.LoadFromTool(fToolOutput);
  fToolProc.OnTerminate := nil;
  fToolProc.OnReadData  := nil;
  fToolOutput.Clear;
  //
  tree.BeginUpdate;
  for i := 0 to fSyms.symbols.Count-1 do
    symbolToTreeNode(nil, fSyms.symbols[i]);
  if fAutoExpandErrors then
  begin
    if ndWarn.Visible then
      ndWarn.Expand(true);
    if ndErr.Visible then
      ndErr.Expand(true);
  end;
  if fSortSymbols then
    for i:= 0 to tree.Items.Count-1 do
      if Tree.Items[i].Count > 0 then
        tree.Items[i].CustomSort(nil);

  //expandCurrentDeclaration;
  tree.EndUpdate;
end;

procedure TCESymbolListWidget.expandCurrentDeclaration;
var
  i: integer;
  nearest, target: NativeUint;
  toExpand: TTreeNode;

  procedure look(root: TTreeNode);
  var
    i: integer;
    l: NativeUint;
  begin
    for i := 0 to root.Count-1 do
    begin
      if root.Items[i].Data = nil then
        continue;
      {$HINTS OFF}
      l := NativeUInt(root.Items[i].Data);
      {$HINTS ON}
      if l > target then
        continue;
      if l > nearest then
      begin
        nearest := l;
        toExpand := root.Items[i];
      end;
    end;
  end;

begin
  if fDoc = nil then exit;
  //
  target := fDoc.CaretY;
  toExpand := nil;
  nearest := 0;
  for i := 0 to tree.Items.Count-1 do
    look(tree.Items[i]);
  if toExpand <> nil then
    tree.Selected := toExpand;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
