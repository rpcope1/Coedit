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

  TCETodoListWidget = class(TCEWidget, ICEMultiDocObserver, ICEProjectObserver)
    btnRefresh: TBitBtn;
    lstItems: TListView;
    lstfilter: TListFilterEdit;
    Panel1: TPanel;
  private
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fToolProcess: TCheckedAsyncProcess;
    fTodos: TTodoItems;
    fLogMessager: TCELogMessageSubject;
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
    procedure procTerminated(sender: TObject);
    procedure procOutput(sender: TObject);
    procedure clearTodoList;
    procedure fillTodoList;
    procedure lstItemsDoubleClick(sender: TObject);
    procedure btnRefreshClick(sender: TObject);
    procedure filterItems(sender: TObject);
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
  fLogMessager := TCELogMessageSubject.create;
  lstItems.OnDblClick := @lstItemsDoubleClick;
  btnRefresh.OnClick := @btnRefreshClick;

  // http://bugs.freepascal.org/view.php?id=27137
  // TODO-cCleanup: remove comment after next Laz release
  // TODO-cfeature, try the new TListViewFilterEdit here.
  lstfilter.OnChange:= @filterItems;
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('arrow_update');
    btnRefresh.Glyph.Assign(png);
  finally
    png.Free;
  end;
end;

destructor TCETodoListWidget.destroy;
begin
  killToolProcess;
  fLogMessager.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCETodoListWidget.docNew(aDoc: TCESynMemo);
begin
end;

procedure TCETodoListWidget.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
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
  fProj := aProject;
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
  if (fDoc = nil) and (fProj = nil)then exit;
  //
  killToolProcess;
  // process parameter
  fToolProcess := TCheckedAsyncProcess.Create(nil);
  fToolProcess.Executable := ToolExeName;
  fToolProcess.Options := [poUsePipes, poStderrToOutPut];
  fToolProcess.ShowWindow := swoHIDE;
  fToolProcess.OnTerminate := @procTerminated;
  fToolProcess.OnReadData := @procOutput;
  // files passed to the tool argument
  if ctxt = tcProject then fToolProcess.Parameters.Add(symbolExpander.get('<CPFS>'))
  else fToolProcess.Parameters.AddText(symbolExpander.get('<CFF>'));
  //
  fToolProcess.Execute;
end;

procedure TCETodoListWidget.procOutput(sender: TObject);
var
  str: TStringList;
  msg: string;
  ctxt: TTodoContext;
begin
  subjLmFromString(fLogMessager, 'called even if nothing in output', fProj, amcProj, amkAuto);
  str := TStringList.Create;
  try
    processOutputToStrings(TAsyncProcess(fToolProcess), str);
    ctxt := getContext;
    for msg in str do case ctxt of
      tcNone:   subjLmFromString(fLogMessager, msg, nil, amcMisc, amkAuto);
      tcFile:   subjLmFromString(fLogMessager, msg, fDoc, amcEdit, amkAuto);
      tcProject:subjLmFromString(fLogMessager, msg, fProj, amcProj, amkAuto);
    end;
  finally
    str.Free;
  end;
  fToolProcess.Input.WriteByte($0A);
end;

procedure TCETodoListWidget.procTerminated(sender: TObject);
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

