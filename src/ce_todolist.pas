unit ce_todolist;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, ListFilterEdit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, Buttons, StdCtrls, ComCtrls, asyncprocess,
  ce_widget, process, ce_common, ce_interfaces, ce_synmemo, ce_project, ce_symstring;

type

  TTodoContext = (tcNone, tcProject, tcFile);

  // represents a TODO item
  // warning: the props names must be kept in sync with the values set in the tool.
  TTodoItem = class(TCollectionItem)
  private
    fFile: string;
    fLine: string;
    fText: string;
    fPriority: string;
    fAssignee: string;
    fCategory: string;
    fStatus: string;
  published
    property filename:string read fFile     write fFile;
    property line:    string read fLine     write fLine;
    property text:    string read fText     write fText;
    property assignee:string read fAssignee write fAssignee;
    property category:string read fCategory write fCategory;
    property status:  string read fStatus   write fStatus;
    property priority:string read fPriority write fPriority;
  end;

  // encapsulates / makes serializable a collection of TODO item.
  // warning: the class name must be kept in sync with the value set in the tool.
  TTodoItems = class(TComponent)
  private
    fItems: TCollection;
    procedure setItems(aValue: TCollection);
    function getItem(index: Integer): TTodoItem;
    function getCount: integer;
  published
    // warning, "items" must be kept in sync with...
    property items: TCollection read fItems write setItems;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    // str will be set on the tool process output.
    procedure loadFromTxtStream(str: TMemoryStream);
    property count: integer read getCount;
    property item[index: integer]: TTodoItem read getItem; default;
  end;

  { TCETodoListWidget }

  TCETodoListWidget = class(TCEWidget, ICEMultiDocObserver, ICEProjectObserver, ICESessionOptionsObserver)
    btnRefresh: TBitBtn;
    btnGo: TBitBtn;
    lstItems: TListView;
    lstfilter: TListFilterEdit;
    mnuAutoRefresh: TMenuItem;
    Panel1: TPanel;
    procedure btnGoClick(Sender: TObject);
    procedure mnuAutoRefreshClick(Sender: TObject);
  private
    fAutoRefresh: Boolean;
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fToolProcess: TCheckedAsyncProcess;
    fTodos: TTodoItems;
    fMsgs: ICEMessagesDisplay;
    // ICEMultiDocObserver
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    // ICEProjectObserver
    procedure projNew(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
    // TODOlist things
    function getContext: TTodoContext;
    procedure killToolProcess;
    procedure callToolProcess;
    procedure procOutput(sender: TObject);
    procedure procOutputDbg(sender: TObject);
    procedure clearTodoList;
    procedure fillTodoList;
    procedure lstItemsColumnClick(Sender : TObject; Column : TListColumn);
    procedure lstItemsCompare(Sender : TObject; item1, item2: TListItem;Data : Integer; var Compare : Integer);
    procedure lstItemsDoubleClick(sender: TObject);
    procedure btnRefreshClick(sender: TObject);
    procedure filterItems(sender: TObject);
  protected
    procedure SetVisible(Value: boolean); override;
    // ICESessionOptionsObserver
    procedure optset_AutoReafresh(aReader: TReader);
    procedure optget_AutoReafresh(aWriter: TWriter);
    procedure sesoptDeclareProperties(aFiler: TFiler); override;
    procedure sesoptAfterLoad; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

uses
  ce_main, strutils;

const
  ToolExeName = 'cetodo' + exeExt;

{$REGION TTodoItems ------------------------------------------------------------}
constructor TTodoItems.create(aOwner: TComponent);
begin
  inherited;
  fItems := TCollection.Create(TTodoItem);
end;

destructor TTodoItems.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TTodoItems.setItems(aValue: TCollection);
begin
  fItems.Assign(aValue);
end;

function TTodoItems.getItem(index: Integer): TTodoItem;
begin
  result := TTodoItem(fItems.Items[index]);
end;

function TTodoItems.getCount: integer;
begin
  result := fItems.Count;
end;

procedure TTodoItems.loadFromTxtStream(str: TMemoryStream);
var
  bin: TMemoryStream;
begin
  // empty collection ~ length
  if str.Size < 50 then exit;
  //
  try
    bin := TMemoryStream.Create;
    try
      str.Position:=0;
      ObjectTextToBinary(str, bin);
      bin.Position := 0;
      bin.ReadComponent(self);
    finally
      bin.Free;
    end;
  except
    fItems.Clear;
  end;
end;
{$ENDREGIOn}

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCETodoListWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fTodos := TTodoItems.Create(self);
  lstItems.OnDblClick := @lstItemsDoubleClick;
  btnRefresh.OnClick := @btnRefreshClick;
  lstItems.OnColumnClick:= @lstItemsColumnClick;
  lstItems.OnCompare := @lstItemsCompare;
  fAutoRefresh := true;
  mnuAutoRefresh.Checked := true;
  // http://bugs.freepascal.org/view.php?id=27137
  // TODO-cLCL&LAZ-specific: remove comment after next Laz release
  // TODO-cLCL&LAZ-specific, try the new TListViewFilterEdit here.
  lstfilter.OnChange:= @filterItems;
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('arrow_update');
    btnRefresh.Glyph.Assign(png);
    png.LoadFromLazarusResource('arrow_pen');
    btnGo.Glyph.Assign(png);
  finally
    png.Free;
  end;
end;

destructor TCETodoListWidget.destroy;
begin
  killToolProcess;
  inherited;
end;

procedure TCETodoListWidget.SetVisible(Value: boolean);
begin
  inherited;
  if Value then
    callToolProcess;
end;

{$ENDREGION}

{$REGION ICESessionOptionsObserver  --------------------------------------------}
procedure TCETodoListWidget.optset_AutoReafresh(aReader: TReader);
begin
  fAutoRefresh := aReader.ReadBoolean;
end;

procedure TCETodoListWidget.optget_AutoReafresh(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fAutoRefresh);
end;

procedure TCETodoListWidget.sesoptDeclareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_AutoRefresh', @optset_AutoReafresh, @optget_AutoReafresh, true);
end;

procedure TCETodoListWidget.sesoptAfterLoad;
begin
  inherited;
  mnuAutoRefresh.Checked := fAutoRefresh;
end;
{$ENDREGIOn}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCETodoListWidget.docNew(aDoc: TCESynMemo);
begin
end;

procedure TCETodoListWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc = fDoc then exit;
  fDoc := aDoc;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
end;

procedure TCETodoListWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
  callToolProcess;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCETodoListWidget.projNew(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCETodoListWidget.projChanged(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  fProj := nil;
  callToolProcess;
end;

procedure TCETodoListWidget.projFocused(aProject: TCEProject);
begin
  if aProject = fProj then exit;
  fProj := aProject;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.projCompiling(aProject: TCEProject);
begin
end;
{$ENDREGION}

{$REGION Todo list things ------------------------------------------------------}
function TCETodoListWidget.getContext: TTodoContext;
begin
  result := tcNone;
  //
  if ((fProj = nil) and (fDoc = nil)) then exit;
  if ((fProj = nil) and (fDoc <> nil)) then exit(tcFile);
  if ((fProj <> nil) and (fDoc = nil)) then exit(tcProject);
  //
  result := tcFile;
  if not FileExists(fDoc.fileName) then exit;
  if fProj.isProjectSource(fDoc.fileName) then exit(tcProject);
end;

procedure TCETodoListWidget.killToolProcess;
begin
  if fToolProcess = nil then exit;
  //
  fToolProcess.Terminate(0);
  fToolProcess.Free;
  fToolProcess := nil;
end;

procedure TCETodoListWidget.callToolProcess;
var
  ctxt: TTodoContext;
begin
  clearTodoList;
  if not exeInSysPath(ToolExeName) then exit;
  ctxt := getContext;
  if ctxt = tcNone then exit;
  //
  killToolProcess;
  // process parameter
  fToolProcess := TCheckedAsyncProcess.Create(nil);
  fToolProcess.Executable := ToolExeName;
  fToolProcess.Options := [poUsePipes];
  fToolProcess.ShowWindow := swoHIDE;
  fToolProcess.CurrentDirectory := ExtractFileDir(Application.ExeName);

  // Something not quite clear:
  // --------------------------
  // actually the two events can be called, depending
  // on the amount of data in the output.
  // many: OnReadData is called.
  // few: OnTerminate is called.
  fToolProcess.OnTerminate := @procOutput;
  fToolProcess.OnReadData := @procOutput;

  // files passed to the tool argument
  if ctxt = tcProject then fToolProcess.Parameters.AddText(symbolExpander.get('<CPFS>'))
  else fToolProcess.Parameters.Add(symbolExpander.get('<CFF>'));
  //
  fToolProcess.Execute;
end;

procedure TCETodoListWidget.procOutputDbg(sender: TObject);
var
  str: TStringList;
  msg: string;
  ctxt: TTodoContext;
begin
  getMessageDisplay(fMsgs);
  str := TStringList.Create;
  try
    processOutputToStrings(fToolProcess, str);
    ctxt := getContext;
    for msg in str do case ctxt of
      tcNone:   fMsgs.message(msg, nil, amcMisc, amkAuto);
      tcFile:   fMsgs.message(msg, fDoc, amcEdit, amkAuto);
      tcProject:fMsgs.message(msg, fProj, amcProj, amkAuto);
    end;
  finally
    str.Free;
  end;
end;

procedure TCETodoListWidget.procOutput(sender: TObject);
var
  str: TMemoryStream;
  cnt: Integer;
  sum: Integer;
const
  buffSz = 1024;
begin
  sum := 0;
  str := TMemoryStream.Create;
  try
    while fToolProcess.Output.NumBytesAvailable <> 0 do begin
      str.SetSize(sum + buffSz);
      cnt := fToolProcess.Output.Read((str.Memory + sum)^, buffSz);
      sum += cnt;
    end;
    str.SetSize(sum);
    str.Position := 0;
    fTodos.loadFromTxtStream(str);
    fillTodoList;
  finally
    str.Free;
  end;
end;

procedure TCETodoListWidget.clearTodoList;
begin
  lstItems.Clear;
  fTodos.items.Clear;
end;

procedure TCETodoListWidget.fillTodoList;
var
  i: integer;
  src: TTodoItem;
  trg: TListItem;
  flt: string;
begin
  lstItems.Clear;
  lstItems.Column[1].Visible:=false;
  lstItems.Column[2].Visible:=false;
  lstItems.Column[3].Visible:=false;
  lstItems.Column[4].Visible:=false;
  flt := lstfilter.Text;
  for i:= 0 to fTodos.count -1  do begin
    src := fTodos[i];
    trg := lstItems.Items.Add;
    trg.Data := src;
    trg.Caption := src.text;
    trg.SubItems.Add(src.category);
    trg.SubItems.Add(src.assignee);
    trg.SubItems.Add(src.status);
    trg.SubItems.Add(src.priority);
    //
    if flt <> '' then if flt <> '(filter)' then
      if not AnsiContainsText(src.text,flt)     then
      if not AnsiContainsText(src.category,flt) then
      if not AnsiContainsText(src.assignee,flt) then
      if not AnsiContainsText(src.status,flt)   then
      if not AnsiContainsText(src.priority,flt) then
    begin
      lstItems.Items.Delete(trg.Index);
      continue;
    end;
    //
    if src.category <> '' then lstItems.Column[1].Visible := true;
    if src.assignee <> '' then lstItems.Column[2].Visible := true;
    if src.status <> '' then lstItems.Column[3].Visible := true;
    if src.priority <> '' then lstItems.Column[4].Visible := true;
  end;
end;

procedure TCETodoListWidget.btnGoClick(Sender: TObject);
begin
  lstItemsDoubleClick(nil);
end;

procedure TCETodoListWidget.mnuAutoRefreshClick(Sender: TObject);
begin
  fAutoRefresh := mnuAutoRefresh.Checked;
end;

procedure TCETodoListWidget.lstItemsDoubleClick(sender: TObject);
var
  itm: TTodoItem;
  fname: string;
  ln: string;
begin
  if lstItems.Selected = nil then exit;
  if lstItems.Selected.Data = nil then exit;
  // the collection will be cleared if a file is opened
  // docFocused->callToolProcess->fTodos....clear
  // so line and filename must be copied
  itm := TTodoItem(lstItems.Selected.Data);
  fname := itm.filename;
  ln := itm.line;
  CEMainForm.openFile(fname);
  //
  if fDoc = nil then exit;
  fDoc.CaretY := strToInt(ln);
  fDoc.SelectLine;
end;

procedure TCETodoListWidget.lstItemsColumnClick(Sender : TObject; Column :
  TListColumn);
var
  curr: TListItem;
begin
  if lstItems.Selected = nil then exit;
  curr := lstItems.Selected;
  //
  if lstItems.SortDirection = sdAscending then
    lstItems.SortDirection := sdDescending
  else lstItems.SortDirection := sdAscending;
  lstItems.SortColumn := Column.Index;
  lstItems.Selected := nil;
  lstItems.Selected := curr;
  lstItems.Update;
end;

procedure TCETodoListWidget.lstItemsCompare(Sender : TObject; item1, item2:
  TListItem;Data : Integer; var Compare : Integer);
var
  txt1, txt2: string;
  col: Integer;
begin
  txt1 := '';
  txt2 := '';
  col  := lstItems.SortColumn;
  if col = 0 then
  begin
    txt1 := item1.Caption;
    txt2 := item2.Caption;
  end else
  begin
    if col < item1.SubItems.Count then txt1 := item1.SubItems.Strings[col];
    if col < item2.SubItems.Count then txt2 := item2.SubItems.Strings[col];
  end;
  Compare := AnsiCompareStr(txt1, txt2);
  if lstItems.SortDirection = sdDescending then Compare := -Compare;
end;

procedure TCETodoListWidget.btnRefreshClick(sender: TObject);
begin
  callToolProcess;
end;

procedure TCETodoListWidget.filterItems(sender: TObject);
begin
  fillTodoList
end;

{$ENDREGION}
end.

