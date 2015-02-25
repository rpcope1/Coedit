unit ce_symlist;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, ExtCtrls, Menus,
  ComCtrls, ce_widget, jsonparser, process, actnlist, Buttons, Clipbrd,
  ce_common, ce_observer, ce_synmemo, ce_interfaces, LCLProc;

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
    property name: string read fName write fName;
    property symType: TSymbolType read fType write fType;
    property subs: TSymbolCollection read fSubs write setSubs;
  public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
  end;

  // Encapsulates a ssymbol ub symbols.
  TSymbolCollection = class(TCollection)
  private
    function getSub(index: Integer): TSymbol;
  public
    constructor create;
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
    constructor create(aOwner: TCOmponent); override;
    destructor destroy; override;
    //
    procedure LoadFromTool(str: TStream);
  end;

  TCESymbolListWidget = class(TCEWidget, ICEMultiDocObserver)
    btnRefresh: TBitBtn;
    imgList: TImageList;
    Panel1: TPanel;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnRefreshClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeFilterEdit1AfterFilter(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
  private
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
    procedure toolOutputData(sender: TObject);
    procedure toolTerminated(sender: TObject);
    //
    procedure optget_AutoRefresh(aWriter: TWriter);
    procedure optset_AutoRefresh(aReader: TReader);
    procedure optget_RefreshOnChange(aWriter: TWriter);
    procedure optset_RefreshOnChange(aReader: TReader);
    procedure optget_RefreshOnFocus(aWriter: TWriter);
    procedure optset_RefreshOnFocus(aReader: TReader);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
  protected
    procedure UpdateByDelay; override;
    //
    procedure sesoptDeclareProperties(aFiler: TFiler); override;
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
    ObjectTextToBinary(str, bin);
    bin.Position:=0;
    bin.ReadComponent(self);
  finally
    bin.Free;
  end;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCESymbolListWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  fAutoRefresh := false;
  fRefreshOnFocus := true;
  fRefreshOnChange := false;
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

{$REGION ICESessionOptionsObserver ---------------------------------------------}
procedure TCESymbolListWidget.optget_AutoRefresh(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fAutoRefresh);
end;

procedure TCESymbolListWidget.optset_AutoRefresh(aReader: TReader);
begin
  fAutoRefresh := aReader.ReadBoolean;
  fActAutoRefresh.Checked := fAutoRefresh;
end;

procedure TCESymbolListWidget.optget_RefreshOnChange(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fRefreshOnChange);
end;

procedure TCESymbolListWidget.optset_RefreshOnChange(aReader: TReader);
begin
  fRefreshOnChange := aReader.ReadBoolean;
  fActRefreshOnChange.Checked := fRefreshOnChange;
end;

procedure TCESymbolListWidget.optget_RefreshOnFocus(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fRefreshOnFocus);
end;

procedure TCESymbolListWidget.optset_RefreshOnFocus(aReader: TReader);
begin
  fRefreshOnFocus := aReader.ReadBoolean;
  fActRefreshOnFocus.Checked := fRefreshOnFocus;
end;

procedure TCESymbolListWidget.sesoptDeclareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_AutoRefresh', @optset_AutoRefresh, @optget_AutoRefresh, true);
  aFiler.DefineProperty(Name + '_RefreshOnChange', @optset_RefreshOnChange, @optget_RefreshOnChange, true);
  aFiler.DefineProperty(Name + '_RefreshOnFocus', @optset_RefreshOnFocus, @optget_RefreshOnFocus, true);
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
  autoRefresh := not autoRefresh;
end;

procedure TCESymbolListWidget.actRefreshOnChangeExecute(Sender: TObject);
begin
  refreshOnChange := not refreshOnChange;
end;

procedure TCESymbolListWidget.actRefreshOnFocusExecute(Sender: TObject);
begin
  refreshOnFocus := not refreshOnFocus;
end;

procedure TCESymbolListWidget.actCopyIdentExecute(Sender: TObject);
begin
  if Tree.Selected = nil then exit;
  Clipboard.AsText:= Tree.Selected.Text;
end;

{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCESymbolListWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  beginUpdateByDelay;
end;

procedure TCESymbolListWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
  clearTree;
  updateVisibleCat;
  beginUpdateByDelay;
end;

procedure TCESymbolListWidget.docFocused(aDoc: TCESynMemo);
begin
  if fDoc = aDoc then exit;
  fDoc := aDoc;
  if not Visible then exit;
  //
  if fAutoRefresh then beginUpdateByDelay
  else if fRefreshOnFocus then callToolProc;
end;

procedure TCESymbolListWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  if not Visible then exit;
  //
  if fAutoRefresh then beginUpdateByDelay
  else if fRefreshOnChange then callToolProc;
end;
{$ENDREGION}

{$REGION Symbol-tree things ----------------------------------------------------}
procedure TCESymbolListWidget.UpdateByDelay;
begin
  if not fAutoRefresh then exit;
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
    ndEnum.Visible  := ndEnum.Count > 0;
    ndFunc.Visible  := ndFunc.Count > 0;
    ndImp.Visible   := ndImp.Count > 0;
    ndIntf.Visible  := ndIntf.Count > 0;
    ndMix.Visible   := ndMix.Count > 0;
    ndStruct.Visible:= ndStruct.Count > 0;
    ndTmp.Visible   := ndTmp.Count > 0;
    ndUni.Visible   := ndUni.Count > 0;
    ndVar.Visible   := ndVar.Count > 0;
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
  if TreeFilterEdit1.Filter ='' then
    updateVisibleCat;
end;

procedure TCESymbolListWidget.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then TreeDblClick(nil);
end;

procedure TCESymbolListWidget.TreeDblClick(Sender: TObject);
var
  line: Int64;
begin
  if fDoc = nil then exit;
  if Tree.Selected = nil then exit;
  if Tree.Selected.Data = nil then exit;
  //
  line := PInt64(Tree.Selected.Data)^;
  fDoc.CaretY := line;
  fDoc.SelectLine;
end;

procedure TCESymbolListWidget.callToolProc;
var
  srcFname: string;
begin
  if fDoc = nil then exit;
  if fDoc.Lines.Count = 0 then exit;

  // standard process options
  killProcess(fToolProc);
  fToolProc := TCheckedAsyncProcess.Create(nil);
  fToolProc.ShowWindow := swoHIDE;
  fToolProc.Options := [poUsePipes];
  fToolProc.Executable := 'cesyms';
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
var
  i: Integer;
//
procedure symbolToTreeNode(sym: TSymbol);
var
  data: PInt64;
  cat: TTreeNode;
  i: Integer;
begin
  case sym.symType of
    _alias    : cat := ndAlias;
    _class    : cat := ndClass;
    _enum     : cat := ndEnum;
    _function : cat := ndFunc;
    _import   : cat := ndImp;
    _interface: cat := ndIntf;
    _mixin    : cat := ndMix;
    _struct   : cat := ndStruct;
    _template : cat := ndTmp;
    _union    : cat := ndUni;
    _variable : cat := ndVar;
  end;
  data := new(PInt64);
  data^ := sym.fline;
  tree.Items.AddChildObject(cat, sym.name, data);
  cat.Visible:=true;
  //
  for i := 0 to sym.subs.Count-1 do
    symbolToTreeNode(sym.subs[i]);
end;
//
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
    symbolToTreeNode(fSyms.symbols[i]);
  tree.EndUpdate;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
