unit ce_todolist;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls,
  strutils, Graphics, Dialogs, ExtCtrls, Menus, Buttons, ComCtrls,
  ce_widget, process, ce_common, ce_interfaces, ce_synmemo, ce_processes,
  ce_nativeproject, ce_symstring, ce_writableComponent, ce_observer, ce_sharedres;

type

  TTodoColumn = (filename, line, text, priority, assignee, category, status);
  TTodoColumns = set of TTodoColumn;

  TCETodoOptions = class(TWritableLfmTextComponent)
  private
    fAutoRefresh: boolean;
    fSingleClick: boolean;
    fColumns: TTodoColumns;
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
    property singleClickSelect: boolean read fSingleClick write fSingleClick;
    property columns: TTodoColumns read fColumns write fColumns;
  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Src: TPersistent); override;
  end;

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
    property filename: string read fFile write fFile;
    property line: string read fLine write fLine;
    property Text: string read fText write fText;
    property assignee: string read fAssignee write fAssignee;
    property category: string read fCategory write fCategory;
    property status: string read fStatus write fStatus;
    property priority: string read fPriority write fPriority;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // str is the output stream of the tool process.
    procedure loadFromTxtStream(str: TMemoryStream);
    property Count: integer read getCount;
    property item[index: integer]: TTodoItem read getItem; default;
  end;

  { TCETodoListWidget }

  TCETodoListWidget = class(TCEWidget, ICEMultiDocObserver, ICEProjectObserver, ICEEditableOptions)
    btnRefresh: TBitBtn;
    btnGo: TBitBtn;
    lstItems: TListView;
    lstfilter: TListFilterEdit;
    mnuAutoRefresh: TMenuItem;
    Panel1: TPanel;
    procedure handleListClick(Sender: TObject);
    procedure mnuAutoRefreshClick(Sender: TObject);
  private
    fAutoRefresh: Boolean;
    fSingleClick: Boolean;
    fColumns: TTodoColumns;
    fProj: ICECommonProject;
    fDoc: TCESynMemo;
    fToolProc: TCEProcess;
    fTodos: TTodoItems;
    fMsgs: ICEMessagesDisplay;
    fOptions: TCETodoOptions;
    // ICEMultiDocObserver
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    // ICEProjectObserver
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    // ICEEditableOptions
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    // TODOlist things
    function getContext: TTodoContext;
    procedure killToolProcess;
    procedure callToolProcess;
    procedure toolTerminated(Sender: TObject);
    procedure procOutputDbg(Sender: TObject);
    procedure clearTodoList;
    procedure fillTodoList;
    procedure lstItemsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lstItemsCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
    procedure btnRefreshClick(Sender: TObject);
    procedure filterItems(Sender: TObject);
    procedure setSingleClick(aValue: boolean);
    procedure setAutoRefresh(aValue: boolean);
    procedure setColumns(aValue: TTodoColumns);
    procedure refreshVisibleColumns;
  protected
    procedure SetVisible(Value: boolean); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    //
    property singleClickSelect: boolean read fSingleClick write setSingleClick;
    property autoRefresh: boolean read fAutoRefresh write setAutoRefresh;
    property columns: TTodoColumns read fColumns write setColumns;
  end;

implementation

{$R *.lfm}

const
  ToolExeName = 'cetodo' + exeExt;
  OptFname = 'todolist.txt';

{$REGION TTodoItems ------------------------------------------------------------}
constructor TTodoItems.Create(aOwner: TComponent);
begin
  inherited;
  fItems := TCollection.Create(TTodoItem);
end;

destructor TTodoItems.Destroy;
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
  Result := TTodoItem(fItems.Items[index]);
end;

function TTodoItems.getCount: integer;
begin
  Result := fItems.Count;
end;

procedure TTodoItems.loadFromTxtStream(str: TMemoryStream);
var
  bin: TMemoryStream;
begin
  // empty collection ~ length
  if str.Size < 50 then
    exit;
  //
  try
    bin := TMemoryStream.Create;
    try
      str.Position := 0;
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
constructor TCETodoListWidget.Create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  //
  columns:= [TTodoColumn.filename .. TTodoColumn.line];
  fOptions := TCETodoOptions.Create(self);
  fOptions.autoRefresh := True;
  fOptions.Name := 'todolistOptions';
  //
  fTodos := TTodoItems.Create(self);
  lstItems.OnDblClick := @handleListClick;
  btnRefresh.OnClick := @btnRefreshClick;
  lstItems.OnColumnClick := @lstItemsColumnClick;
  lstItems.OnCompare := @lstItemsCompare;
  fAutoRefresh := True;
  fSingleClick := False;
  mnuAutoRefresh.Checked := True;
  lstfilter.OnChange := @filterItems;
  btnGo.OnClick := @handleListClick;
  //
  AssignPng(btnRefresh, 'arrow_update');
  AssignPng(btnGo, 'arrow_pen');
  //
  fname := getCoeditDocPath + OptFname;
  if FileExists(fname) then
    fOptions.loadFromFile(fname);
  fOptions.AssignTo(self);
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCETodoListWidget.Destroy;
begin
  fOptions.saveToFile(getCoeditDocPath + OptFname);
  killToolProcess;
  inherited;
end;

procedure TCETodoListWidget.SetVisible(Value: boolean);
begin
  inherited;
  if Value and fAutoRefresh then
    callToolProcess;
  refreshVisibleColumns;
end;

{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
procedure TCETodoOptions.AssignTo(Dest: TPersistent);
var
  widg: TCETodoListWidget;
begin
  if Dest is TCETodoListWidget then
  begin
    widg := TCETodoListWidget(Dest);
    widg.singleClickSelect := fSingleClick;
    widg.autoRefresh := fAutoRefresh;
    widg.columns := fColumns;
  end
  else
    inherited;
end;

procedure TCETodoOptions.Assign(Src: TPersistent);
var
  widg: TCETodoListWidget;
begin
  if Src is TCETodoListWidget then
  begin
    widg := TCETodoListWidget(Src);
    fSingleClick := widg.singleClickSelect;
    fAutoRefresh := widg.autoRefresh;
    fColumns:=widg.columns;
  end
  else
    inherited;
end;

function TCETodoListWidget.optionedWantCategory(): string;
begin
  exit('Todo list');
end;

function TCETodoListWidget.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCETodoListWidget.optionedWantContainer: TPersistent;
begin
  fOptions.Assign(self);
  exit(fOptions);
end;

procedure TCETodoListWidget.optionedEvent(anEvent: TOptionEditorEvent);
begin
  if anEvent <> oeeAccept then
    exit;
  fOptions.AssignTo(self);
end;

function TCETodoListWidget.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCETodoListWidget.docNew(aDoc: TCESynMemo);
begin
end;

procedure TCETodoListWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc = fDoc then
    exit;
  fDoc := aDoc;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCETodoListWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then
    exit;
  fDoc := nil;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCETodoListWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCETodoListWidget.projChanged(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  fProj := nil;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.projFocused(aProject: ICECommonProject);
begin
  if aProject = fProj then
    exit;
  fProj := aProject;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.projCompiling(aProject: ICECommonProject);
begin
end;
{$ENDREGION}

{$REGION Todo list things ------------------------------------------------------}
function TCETodoListWidget.getContext: TTodoContext;
begin
  if ((fProj = nil) and (fDoc = nil)) then
    exit(tcNone);
  if ((fProj = nil) and (fDoc <> nil)) then
    exit(tcFile);
  if ((fProj <> nil) and (fDoc = nil)) then
    exit(tcProject);
  //
  if fProj.isSource(fDoc.fileName) then
    exit(tcProject)
  else
    exit(tcFile);
end;

procedure TCETodoListWidget.killToolProcess;
begin
  if fToolProc = nil then
    exit;
  //
  fToolProc.Terminate(0);
  fToolProc.Free;
  fToolProc := nil;
end;

procedure TCETodoListWidget.callToolProcess;
var
  ctxt: TTodoContext;
begin
  clearTodoList;
  if not exeInSysPath(ToolExeName) then
    exit;
  ctxt := getContext;
  if ctxt = tcNone then
    exit;
  //
  killToolProcess;
  // process parameter
  fToolProc := TCEProcess.Create(nil);
  fToolProc.Executable := exeFullName(ToolExeName);
  fToolProc.Options := [poUsePipes];
  fToolProc.ShowWindow := swoHIDE;
  fToolProc.CurrentDirectory := ExtractFileDir(Application.ExeName);
  fToolProc.OnTerminate := @toolTerminated;

  // files passed to the tool argument
  if ctxt = tcProject then
    fToolProc.Parameters.AddText(symbolExpander.get('<CPFS>'))
  else
    fToolProc.Parameters.Add(symbolExpander.get('<CFF>'));
  //
  fToolProc.Execute;
end;

procedure TCETodoListWidget.procOutputDbg(Sender: TObject);
var
  str: TStringList;
  msg: string;
  ctxt: TTodoContext;
begin
  getMessageDisplay(fMsgs);
  str := TStringList.Create;
  try
    processOutputToStrings(fToolProc, str);
    ctxt := getContext;
    for msg in str do
      case ctxt of
        tcNone: fMsgs.message(msg, nil, amcMisc, amkAuto);
        tcFile: fMsgs.message(msg, fDoc, amcEdit, amkAuto);
        tcProject: fMsgs.message(msg, fProj, amcProj, amkAuto);
      end;
  finally
    str.Free;
  end;
end;

procedure TCETodoListWidget.toolTerminated(Sender: TObject);
begin
  //TODO-cbugfix: UTF chars in TODO comments bug either in the widget or the tool, symptom: empty todo list, conditions: to determine.

  fToolProc.OutputStack.Position := 0;
  fTodos.loadFromTxtStream(fToolProc.OutputStack);
  fillTodoList;
  fToolProc.OnTerminate := nil;
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
  lstItems.Column[1].Visible := False;
  lstItems.Column[2].Visible := False;
  lstItems.Column[3].Visible := False;
  lstItems.Column[4].Visible := False;
  flt := lstfilter.Text;
  for i := 0 to fTodos.Count - 1 do
  begin
    src := fTodos[i];
    trg := lstItems.Items.Add;
    trg.Data := src;
    trg.Caption := src.Text;
    trg.SubItems.Add(src.category);
    trg.SubItems.Add(src.assignee);
    trg.SubItems.Add(src.status);
    trg.SubItems.Add(src.priority);
    trg.SubItems.Add(shortenPath(src.filename, 25));
    //
    if flt <> '' then
      if flt <> '(filter)' then
        if not AnsiContainsText(src.Text, flt) then
          if not AnsiContainsText(src.category, flt) then
            if not AnsiContainsText(src.assignee, flt) then
              if not AnsiContainsText(src.status, flt) then
                if not AnsiContainsText(src.priority, flt) then
                begin
                  lstItems.Items.Delete(trg.Index);
                  continue;
                end;
    //
    if src.category <> '' then
      lstItems.Column[1].Visible := True;
    if src.assignee <> '' then
      lstItems.Column[2].Visible := True;
    if src.status <> '' then
      lstItems.Column[3].Visible := True;
    if src.priority <> '' then
      lstItems.Column[4].Visible := True;
  end;
end;

procedure TCETodoListWidget.handleListClick(Sender: TObject);
var
  itm: TTodoItem;
  fname, ln: string;
begin
  if lstItems.Selected = nil then
    exit;
  if lstItems.Selected.Data = nil then
    exit;
  // the collection will be cleared if a file is opened
  // docFocused->callToolProcess->fTodos....clear
  // so line and filename must be copied
  itm := TTodoItem(lstItems.Selected.Data);
  fname := itm.filename;
  ln := itm.line;
  getMultiDocHandler.openDocument(fname);
  //
  if fDoc = nil then
    exit;
  fDoc.CaretY := StrToInt(ln);
  fDoc.SelectLine;
end;

procedure TCETodoListWidget.mnuAutoRefreshClick(Sender: TObject);
begin
  autoRefresh := mnuAutoRefresh.Checked;
  fOptions.autoRefresh := autoRefresh;
end;

procedure TCETodoListWidget.lstItemsColumnClick(Sender: TObject; Column: TListColumn);
var
  curr: TListItem;
begin
  if lstItems.Selected = nil then
    exit;
  curr := lstItems.Selected;
  //
  if lstItems.SortDirection = sdAscending then
    lstItems.SortDirection := sdDescending
  else
    lstItems.SortDirection := sdAscending;
  lstItems.SortColumn := Column.Index;
  lstItems.Selected := nil;
  lstItems.Selected := curr;
  lstItems.Update;
end;

procedure TCETodoListWidget.lstItemsCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
var
  txt1: string = '';
  txt2: string = '';
  col: Integer;
begin
  col := lstItems.SortColumn;
  if col = 0 then
  begin
    txt1 := item1.Caption;
    txt2 := item2.Caption;
  end
  else
  begin
    if col < item1.SubItems.Count then
      txt1 := item1.SubItems.Strings[col];
    if col < item2.SubItems.Count then
      txt2 := item2.SubItems.Strings[col];
  end;
  Compare := AnsiCompareStr(txt1, txt2);
  if lstItems.SortDirection = sdDescending then
    Compare := -Compare;
end;

procedure TCETodoListWidget.btnRefreshClick(Sender: TObject);
begin
  callToolProcess;
end;

procedure TCETodoListWidget.filterItems(Sender: TObject);
begin
  fillTodoList;
end;

procedure TCETodoListWidget.setSingleClick(aValue: boolean);
begin
  fSingleClick := aValue;
  if fSingleClick then
  begin
    lstItems.OnClick := @handleListClick;
    lstItems.OnDblClick := nil;
  end
  else
  begin
    lstItems.OnClick := nil;
    lstItems.OnDblClick := @handleListClick;
  end;
end;

procedure TCETodoListWidget.setAutoRefresh(aValue: boolean);
begin
  fAutoRefresh := aValue;
  mnuAutoRefresh.Checked := aValue;
  if fAutoRefresh then
    callToolProcess;
end;

procedure TCETodoListWidget.setColumns(aValue: TTodoColumns);
begin
  fColumns := aValue;
  refreshVisibleColumns;
end;

procedure TCETodoListWidget.refreshVisibleColumns;
begin
  if lstItems = nil then exit;
  if lstItems.Columns = nil then exit;
  if lstItems.ColumnCount <> 6 then exit;
  //
  lstItems.Column[1].Visible := TTodoColumn.category in fColumns ;
  lstItems.Column[2].Visible := TTodoColumn.assignee in fColumns ;
  lstItems.Column[3].Visible := TTodoColumn.status in fColumns ;
  lstItems.Column[4].Visible := TTodoColumn.priority in fColumns ;
  lstItems.Column[5].Visible := TTodoColumn.filename in fColumns ;
end;
{$ENDREGION}

end.
