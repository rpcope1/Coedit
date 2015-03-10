unit ce_symlist;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, ExtCtrls, Menus,
  ComCtrls, ce_widget, jsonparser, process, ActnList, Buttons, Clipbrd, LCLProc,
  ce_common, ce_observer, ce_synmemo, ce_interfaces, ce_writableComponent, EditBtn;

type

  // Enumerates the possible symbol kind. To be kept in sync with the tool declaration.
  TSymbolType = (
    _alias,
    _class,
    _enum,
    _function,
    _interface,
    _import,
    _mixin,
    _struct,
    _template,
    _union,
    _variable
    );

  TSymbolCollection = class;

  // Encapsulates a symbol to enable structured serialization
  TSymbol = class(TCollectionItem)
  private
    fline, fCol: integer;
    fName: string;
    fType: TSymbolType;
    fSubs: TSymbolCollection;
    procedure setSubs(aValue: TSymbolCollection);
  published
    property line: Integer read fline write fLine;
    property col: Integer read fCol write fCol;
    property Name: string read fName write fName;
    property symType: TSymbolType read fType write fType;
    property subs: TSymbolCollection read fSubs write setSubs;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;

  // Encapsulates a ssymbol ub symbols.
  TSymbolCollection = class(TCollection)
  private
    function getSub(index: Integer): TSymbol;
  public
    constructor Create;
    property sub[index: Integer]: TSymbol read getSub; default;
  end;

  // Serializable Symbol list
  TSymbolList = class(TComponent)
  private
    fSymbols: TSymbolCollection;
    procedure setSymbols(aValue: TSymbolCollection);
  published
    property symbols: TSymbolCollection read fSymbols write setSymbols;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
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
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
    property refreshOnChange: boolean read fRefreshOnChange write fRefreshOnChange;
    property refreshOnFocus: boolean read fRefreshOnFocus write fRefreshOnFocus;
    property showChildCategories: boolean read fShowChildCategories write fShowChildCategories;
    property autoRefreshDelay: Integer read fAutoRefreshDelay write fAutoRefreshDelay;
    property smartFilter: boolean read fSmartFilter write fSmartFilter;
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
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeFilterEdit1AfterFilter(Sender: TObject);
    function TreeFilterEdit1FilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure TreeFilterEdit1MouseEnter(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
  private
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
    fToolOutput: TMemoryStream;
    ndAlias, ndClass, ndEnum, ndFunc, ndUni: TTreeNode;
    ndImp, ndIntf, ndMix, ndStruct, ndTmp, ndVar: TTreeNode;
    procedure TreeDblClick(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAutoRefreshExecute(Sender: TObject);
    procedure actRefreshOnChangeExecute(Sender: TObject);
    procedure actRefreshOnFocusExecute(Sender: TObject);
    procedure actCopyIdentExecute(Sender: TObject);
    procedure updateVisibleCat;
    procedure clearTree;
    //
    procedure callToolProc;
    procedure toolOutputData(Sender: TObject);
    procedure toolTerminated(Sender: TObject);
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
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

const
  OptsFname = 'symbollist.txt';

{$REGION Serializable symbols---------------------------------------------------}
constructor TSymbol.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fSubs := TSymbolCollection.Create;
end;

destructor TSymbol.Destroy;
begin
  fSubs.Free;
  inherited;
end;

procedure TSymbol.setSubs(aValue: TSymbolCollection);
begin
  fSubs.Assign(aValue);
end;

constructor TSymbolCollection.Create;
begin
  inherited Create(TSymbol);
end;

function TSymbolCollection.getSub(index: Integer): TSymbol;
begin
  exit(TSymbol(self.Items[index]));
end;

constructor TSymbolList.Create(aOwner: TComponent);
begin
  inherited;
  fSymbols := TSymbolCollection.Create;
end;

destructor TSymbolList.Destroy;
begin
  fSymbols.Free;
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
    str.Position := 0;
    ObjectTextToBinary(str, bin);
    bin.Position := 0;
    bin.ReadComponent(self);
  finally
    bin.Free;
  end;
end;

{$ENDREGION}

{$REGION TCESymbolListOptions --------------------------------------------------}
constructor TCESymbolListOptions.Create(AOwner: TComponent);
begin
  inherited;
  fRefreshOnFocus := True;
  fShowChildCategories := True;
  fSmartFilter := True;
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
    fAutoRefreshDelay := widg.updaterByDelayDuration;
    fRefreshOnFocus := widg.fRefreshOnFocus;
    fRefreshOnChange := widg.fRefreshOnChange;
    fAutoRefresh := widg.fAutoRefresh;
    fShowChildCategories := widg.fShowChildCategories;
    fSmartFilter := widg.fSmartFilter;
  end
  else
    inherited;
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
    widg.fRefreshOnFocus := fRefreshOnFocus;
    widg.fRefreshOnChange := fRefreshOnChange;
    widg.fAutoRefresh := fAutoRefresh;
    widg.fShowChildCategories := fShowChildCategories;
    widg.fSmartFilter := fSmartFilter;
    //
    widg.fActAutoRefresh.Checked := fAutoRefresh;
    widg.fActRefreshOnChange.Checked := fRefreshOnChange;
    widg.fActRefreshOnFocus.Checked := fRefreshOnFocus;
  end
  else
    inherited;
end;

{$ENDREGIOn}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCESymbolListWidget.Create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
  fname: string;
begin
  fAutoRefresh := False;
  fRefreshOnFocus := True;
  fRefreshOnChange := False;
  //
  fActCopyIdent := TAction.Create(self);
  fActCopyIdent.OnExecute := @actCopyIdentExecute;
  fActCopyIdent.Caption := 'Copy identifier';
  fActRefresh := TAction.Create(self);
  fActRefresh.OnExecute := @actRefreshExecute;
  fActRefresh.Caption := 'Refresh';
  fActAutoRefresh := TAction.Create(self);
  fActAutoRefresh.OnExecute := @actAutoRefreshExecute;
  fActAutoRefresh.Caption := 'Auto-refresh';
  fActAutoRefresh.AutoCheck := True;
  fActAutoRefresh.Checked := fAutoRefresh;
  fActRefreshOnChange := TAction.Create(self);
  fActRefreshOnChange.OnExecute := @actRefreshOnChangeExecute;
  fActRefreshOnChange.Caption := 'Refresh on change';
  fActRefreshOnChange.AutoCheck := True;
  fActRefreshOnChange.Checked := fRefreshOnChange;
  fActRefreshOnFocus := TAction.Create(self);
  fActRefreshOnFocus.OnExecute := @actRefreshOnFocusExecute;
  fActRefreshOnFocus.Caption := 'Refresh on focused';
  fActRefreshOnFocus.AutoCheck := True;
  fActRefreshOnFocus.Checked := fRefreshOnFocus;
  fActSelectInSource := TAction.Create(self);
  fActSelectInSource.OnExecute := @TreeDblClick;
  fActSelectInSource.Caption := 'Select in source';
  //
  inherited;
  // allow empty name if owner is nil
  fSyms := TSymbolList.Create(nil);
  fToolOutput := TMemoryStream.Create;
  //
  fOptions := TCESymbolListOptions.Create(self);
  fOptions.Name := 'symbolListOptions';
  fname := getCoeditDocPath + OptsFname;
  if FileExists(fname) then
    fOptions.loadFromFile(fname);
  fOptions.AssignTo(self);
  //
  ndAlias := Tree.Items[0];
  ndClass := Tree.Items[1];
  ndEnum := Tree.Items[2];
  ndFunc := Tree.Items[3];
  ndImp := Tree.Items[4];
  ndIntf := Tree.Items[5];
  ndMix := Tree.Items[6];
  ndStruct := Tree.Items[7];
  ndTmp := Tree.Items[8];
  ndUni := Tree.Items[9];
  ndVar := Tree.Items[10];
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

destructor TCESymbolListWidget.Destroy;
begin
  EntitiesConnector.removeObserver(self);
  //
  killProcess(fToolProc);
  fToolOutput.Free;
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
  getMessageDisplay(fMsgs);
  if Value then
    callToolProc;
end;

{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCESymbolListWidget.contextName: string;
begin
  Result := 'Static explorer';
end;

function TCESymbolListWidget.contextActionCount: integer;
begin
  Result := 6;
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
    else
      Result := nil;
  end;
end;

procedure TCESymbolListWidget.actRefreshExecute(Sender: TObject);
begin
  if Updating then
    exit;
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
  if Tree.Selected = nil then
    exit;
  Clipboard.AsText := Tree.Selected.Text;
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
  if anEvent <> oeeAccept then
    exit;
  fOptions.AssignTo(self);
  callToolProc;
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
  if fDoc <> aDoc then
    exit;
  fDoc := nil;
  clearTree;
  updateVisibleCat;
end;

procedure TCESymbolListWidget.docFocused(aDoc: TCESynMemo);
begin
  if fDoc = aDoc then
    exit;
  fDoc := aDoc;
  if not Visible then
    exit;
  //
  if fAutoRefresh then
    beginDelayedUpdate
  else if fRefreshOnFocus then
    callToolProc;
end;

procedure TCESymbolListWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then
    exit;
  if not Visible then
    exit;
  //
  if fAutoRefresh then
    beginDelayedUpdate
  else if fRefreshOnChange then
    callToolProc;
end;

{$ENDREGION}

{$REGION Symbol-tree things ----------------------------------------------------}
procedure TCESymbolListWidget.updateDelayed;
begin
  if not fAutoRefresh then
    exit;
  callToolProc;
end;

procedure TCESymbolListWidget.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (node.Data <> nil) then
    Dispose(PInt64(node.Data));
end;

procedure TCESymbolListWidget.btnRefreshClick(Sender: TObject);
begin
  fActRefresh.Execute;
end;

procedure TCESymbolListWidget.updateVisibleCat;
begin
  if (fDoc <> nil) then
  begin
    ndAlias.Visible := ndAlias.Count > 0;
    ndClass.Visible := ndClass.Count > 0;
    ndEnum.Visible := ndEnum.Count > 0;
    ndFunc.Visible := ndFunc.Count > 0;
    ndImp.Visible := ndImp.Count > 0;
    ndIntf.Visible := ndIntf.Count > 0;
    ndMix.Visible := ndMix.Count > 0;
    ndStruct.Visible := ndStruct.Count > 0;
    ndTmp.Visible := ndTmp.Count > 0;
    ndUni.Visible := ndUni.Count > 0;
    ndVar.Visible := ndVar.Count > 0;
  end
  else
  begin
    ndAlias.Visible := True;
    ndClass.Visible := True;
    ndEnum.Visible := True;
    ndFunc.Visible := True;
    ndImp.Visible := True;
    ndIntf.Visible := True;
    ndMix.Visible := True;
    ndStruct.Visible := True;
    ndTmp.Visible := True;
    ndUni.Visible := True;
    ndVar.Visible := True;
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
end;

procedure TCESymbolListWidget.TreeFilterEdit1AfterFilter(Sender: TObject);
begin
  if TreeFilterEdit1.Filter = '' then
    updateVisibleCat;
end;

function TCESymbolListWidget.TreeFilterEdit1FilterItem(Item: TObject; out Done: Boolean): Boolean;
begin
  if not fSmartFilter then
    exit;
  //
  if TreeFilterEdit1.Filter <> '' then
    tree.FullExpand
  else if tree.Selected = nil then
    tree.FullCollapse
  else
    tree.MakeSelectionVisible;
  Result := False;
end;

procedure TCESymbolListWidget.TreeFilterEdit1MouseEnter(Sender: TObject);
begin
  if not fSmartFilter then
    exit;
  //
  tree.Selected := nil;
end;

procedure TCESymbolListWidget.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    TreeDblClick(nil);
end;

procedure TCESymbolListWidget.TreeDblClick(Sender: TObject);
var
  line: Int64;
begin
  if fDoc = nil then
    exit;
  if Tree.Selected = nil then
    exit;
  if Tree.Selected.Data = nil then
    exit;
  //
  line := PInt64(Tree.Selected.Data)^;
  fDoc.CaretY := line;
  fDoc.SelectLine;
end;

procedure TCESymbolListWidget.callToolProc;
var
  srcFname: string;
begin
  if fDoc = nil then
    exit;
  if fDoc.Lines.Count = 0 then
    exit;

  // standard process options
  killProcess(fToolProc);
  fToolProc := TCheckedAsyncProcess.Create(nil);
  fToolProc.ShowWindow := swoHIDE;
  fToolProc.Options := [poUsePipes];
  fToolProc.Executable := 'cesyms';
  fToolProc.OnTerminate := @toolTerminated;
  fToolProc.OnReadData := @toolOutputData;
  fToolProc.CurrentDirectory := ExtractFileDir(Application.ExeName);

  // focused source
  srcFname := fDoc.fileName;
  if not fileExists(srcFname) or (srcFname = fDoc.tempFilename) then
    fDoc.saveTempFile;
  srcFname := fDoc.fileName;
  fToolProc.Parameters.Add(srcFname);

  fToolProc.Execute;
end;

procedure TCESymbolListWidget.toolOutputData(Sender: TObject);
begin
  processOutputToStream(TProcess(Sender), fToolOutput);
end;

procedure TCESymbolListWidget.toolTerminated(Sender: TObject);
//
  function getCatNode(node: TTreeNode; stype: TSymbolType): TTreeNode;
  begin
    if node = nil then
      case stype of
        _alias: exit(ndAlias);
        _class: exit(ndClass);
        _enum: exit(ndEnum);
        _function: exit(ndFunc);
        _import: exit(ndImp);
        _interface: exit(ndIntf);
        _mixin: exit(ndMix);
        _struct: exit(ndStruct);
        _template: exit(ndTmp);
        _union: exit(ndUni);
        _variable: exit(ndVar);
      end
    else
      case stype of
        _alias:
        begin
          Result := node.FindNode('Alias');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Alias');
        end;
        _class:
        begin
          Result := node.FindNode('Class');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Class');
        end;
        _enum:
        begin
          Result := node.FindNode('Enum');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Enum');
        end;
        _function:
        begin
          Result := node.FindNode('Function');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Function');
        end;
        _import:
        begin
          Result := node.FindNode('Import');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Import');
        end;
        _interface:
        begin
          Result := node.FindNode('Interface');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Interface');
        end;
        _mixin:
        begin
          Result := node.FindNode('Mixin');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Mixin');
        end;
        _struct:
        begin
          Result := node.FindNode('Struct');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Struct');
        end;
        _template:
        begin
          Result := node.FindNode('Template');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Template');
        end;
        _union:
        begin
          Result := node.FindNode('Union');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Union');
        end;
        _variable:
        begin
          Result := node.FindNode('Variable');
          if Result = nil then
            Result := node.TreeNodes.AddChild(node, 'Variable');
        end;
      end;
  end;
  //
  procedure symbolToTreeNode(origin: TTreenode; sym: TSymbol);
  var
    Data: PInt64;
    cat: TTreeNode;
    node: TTreeNode;
    i: Integer;
  begin
    cat := getCatNode(origin, sym.symType);
    Data := new(PInt64);
    Data^ := sym.fline;
    node := tree.Items.AddChildObject(cat, sym.Name, Data);
    if not fShowChildCategories then
      node := nil;
    cat.Visible := True;
    for i := 0 to sym.subs.Count - 1 do
      symbolToTreeNode(node, sym.subs[i]);
  end;
  //
var
  i: Integer;
begin
  if ndAlias = nil then
    exit;
  clearTree;
  updateVisibleCat;
  if fDoc = nil then
    exit;
  //
  processOutputToStream(TProcess(Sender), fToolOutput);
  fToolOutput.Position := 0;
  fSyms.LoadFromTool(fToolOutput);
  fToolProc.OnTerminate := nil;
  fToolProc.OnReadData := nil;
  fToolOutput.Clear;
  //
  tree.BeginUpdate;
  for i := 0 to fSyms.symbols.Count - 1 do
    symbolToTreeNode(nil, fSyms.symbols[i]);
  tree.EndUpdate;
end;

{$ENDREGION --------------------------------------------------------------------}

end.
