unit ce_messages;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  lcltype, ce_widget, ActnList, Menus, clipbrd, AnchorDocking, TreeFilterEdit,
  Buttons, math,ce_writableComponent, ce_common, ce_synmemo, GraphType,
  ce_dlangutils, ce_interfaces, ce_observer, ce_symstring;

type

  (**
   * the struct linked to a log message. allow to be filtered.
   *)
  PMessageData = ^TMessageData;
  TMessageData = record
    ctxt: TCEAppMessageCtxt;
    data: Pointer;
  end;

  TCEMessagesOptions = class(TWritableLfmTextComponent)
  private
    fFastDisplay: boolean;
    fMaxCount: Integer;
    fAutoSelect: boolean;
    fSingleClick: boolean;
    fFont: TFont;
    fMsgColors: array[TCEAppMessageKind] of TColor;
    procedure setFont(aValue: TFont);
  published
    property fastDisplay: boolean read fFastDisplay write fFastDisplay;
    property maxMessageCount: integer read fMaxCount write fMaxCount;
    property autoSelect: boolean read fAutoSelect write fAutoSelect;
    property singleMessageClick: boolean read fSingleClick write fSingleClick;
    property font: TFont read fFont write setFont;
    property colorBuble: TColor read fMsgColors[amkBub] write fMsgColors[amkBub];
    property colorInfo: TColor read fMsgColors[amkInf] write fMsgColors[amkInf];
    property colorHint: TColor read fMsgColors[amkHint] write fMsgColors[amkHint];
    property colorWarning: TColor read fMsgColors[amkWarn] write fMsgColors[amkWarn];
    property colorError: TColor read fMsgColors[amkErr] write fMsgColors[amkErr];
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  // grants access to some protected fields that should be actually public (scoll position needed for custom draw !)
  TTreeHack = class(TTreeView)
  end;

  { TCEMessagesWidget }

  TCEMessagesWidget = class(TCEWidget, ICEEditableOptions, ICEMultiDocObserver, ICEProjectObserver, ICEMessagesDisplay)
    btnClearCat: TBitBtn;
    imgList: TImageList;
    List: TTreeView;
    selCtxt: TToolBar;
    btnSelAll: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    btnSelMisc: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    btnSelEdit: TToolButton;
    ToolButton4: TToolButton;
    btnSelProj: TToolButton;
    ToolButton8: TToolButton;
    btnSelApp: TToolButton;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure ListCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fMsgColors: array[TCEAppMessageKind] of TColor;
    fActAutoSel: TAction;
    fActClearAll: TAction;
    fActClearCurCat: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fMaxMessCnt: Integer;
    fProj: ICECommonProject;
    fDoc: TCESynMemo;
    fCtxt: TCEAppMessageCtxt;
    fAutoSelect: boolean;
    fSingleClick: boolean;
    fastDisplay: boolean;
    fOptions: TCEMessagesOptions;
    fOptionsBackup: TCEMessagesOptions;
    fBtns: array[TCEAppMessageCtxt] of TToolButton;
    procedure filterMessages(aCtxt: TCEAppMessageCtxt);
    procedure clearOutOfRangeMessg;
    procedure actAutoSelExecute(Sender: TObject);
    procedure actClearCurCatExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
    procedure actCopyMsgExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
    procedure setMaxMessageCount(aValue: Integer);
    procedure setAutoSelectCategory(aValue: boolean);
    procedure setSingleMessageClick(aValue: boolean);
    procedure listDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCtxtClick(Sender: TObject);
    function iconIndex(aKind: TCEAppMessageKind): Integer;
    procedure handleMessageClick(Sender: TObject);
    //
    procedure setColorError(aValue: TColor);
    procedure setColorInfo(aValue: TColor);
    procedure setColorHint(aValue: TColor);
    procedure setColorBuble(aValue: TColor);
    procedure setColorWarning(aValue: TColor);
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
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
    //
    function singleServiceName: string;
    procedure message(const aValue: string; aData: Pointer; aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
    procedure clearbyContext(aCtxt: TCEAppMessageCtxt);
    procedure clearbyData(aData: Pointer);
    procedure scrollToBack;
  protected
    procedure updateLoop; override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    property maxMessageCount: Integer     read fMaxMessCnt  write setMaxMessageCount;
    property autoSelectCategory: boolean  read fAutoSelect  write setAutoSelectCategory;
    property singleMessageClick: boolean  read fSingleClick write setSingleMessageClick;
    //
    property colorBuble: TColor   read fMsgColors[amkBub]   write setColorBuble;
    property colorInfo: TColor    read fMsgColors[amkInf]   write setColorInfo;
    property colorHint: TColor    read fMsgColors[amkHint]  write setColorHint;
    property colorWarning: TColor read fMsgColors[amkWarn]  write setColorWarning;
    property colorError: TColor   read fMsgColors[amkErr]   write setColorError;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  function guessMessageKind(const aMessg: string): TCEAppMessageKind;
  function getLineFromMessage(const aMessage: string): TPoint;
  function openFileFromDmdMessage(const aMessage: string): boolean;

implementation
{$R *.lfm}

const
  optname = 'messages.txt';
  minColor = $232323;

{$REGION TCEMessagesOptions ----------------------------------------------------}
constructor TCEMessagesOptions.Create(AOwner: TComponent);
begin
  inherited;
  fFont := TFont.Create;
end;

destructor TCEMessagesOptions.destroy;
begin
  fFont.Free;
  inherited;
end;

procedure TCEMessagesOptions.setFont(aValue: TFont);
begin
  fFont.Assign(aValue);
end;

procedure TCEMessagesOptions.assign(Source: TPersistent);
var
  widg : TCEMessagesWidget;
  opts : TCEMessagesOptions;
begin
  if Source is TCEMessagesOptions then
  begin
    opts := TCEMessagesOptions(Source);
    fFont.BeginUpdate;
    fFont.Assign(opts.font);
    fMaxCount := opts.fMaxCount;
    fAutoSelect := opts.fAutoSelect;
    fSingleClick := opts.fSingleClick;
    fFastDisplay := opts.fFastDisplay;
    fMsgColors := opts.fMsgColors;
    fFont.EndUpdate;
  end
  else if Source is TCEMessagesWidget then
  begin
    widg := TCEMessagesWidget(Source);
    fFont.Assign(widg.List.Font);
    fMaxCount := widg.fMaxMessCnt;
    fAutoSelect := widg.fAutoSelect;
    fSingleClick := widg.fSingleClick;
    fFastDisplay := widg.fastDisplay;
    fMsgColors := widg.fMsgColors;
  end
  else inherited;
end;

procedure TCEMessagesOptions.AssignTo(Dest: TPersistent);
var
  widg : TCEMessagesWidget;
begin
  if Dest is TCEMessagesWidget then
  begin
    widg := TCEMessagesWidget(Dest);
    widg.List.Font.Assign(fFont);
    widg.maxMessageCount := fMaxCount;
    widg.autoSelectCategory := fAutoSelect;
    widg.singleMessageClick := fSingleClick;
    widg.fastDisplay:= fFastDisplay;
    widg.fMsgColors := fMsgColors;
  end
  else inherited;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMessagesWidget.create(aOwner: TComponent);
var
  fname: string;
  png: TPortableNetworkGraphic;
begin
  fMaxMessCnt := 500;
  fCtxt := amcAll;
  //
  fActAutoSel := TAction.Create(self);
  fActAutoSel.Caption := 'Auto select message category';
  fActAutoSel.AutoCheck := true;
  fActAutoSel.OnExecute := @actAutoSelExecute;
  fActClearAll := TAction.Create(self);
  fActClearAll.OnExecute := @actClearAllExecute;
  fActClearAll.caption := 'Clear all messages';
  fActClearCurCat := TAction.Create(self);
  fActClearCurCat.OnExecute := @actClearCurCatExecute;
  fActClearCurCat.caption := 'Clear filtered messages';
  fActCopyMsg := TAction.Create(self);
  fActCopyMsg.OnExecute := @actCopyMsgExecute;
  fActCopyMsg.Caption := 'Copy message(s)';
  fActSelAll := TAction.Create(self);
  fActSelAll.OnExecute := @actSelAllExecute;
  fActSelAll.Caption := 'Select all';
  fActSaveMsg := TAction.Create(self);
  fActSaveMsg.OnExecute := @actSaveMsgExecute;
  fActSaveMsg.caption := 'Save selected message(s) to...';
  //
  inherited;
  //
  fMsgColors[amkBub]  := clDefault;
  fMsgColors[amkHint] := clDefault;
  fMsgColors[amkInf]  := clDefault;
  fMsgColors[amkErr]  := clDefault;
  fMsgColors[amkWarn] := clDefault;
  //
  updaterByLoopInterval := 12;
  fOptions := TCEMessagesOptions.Create(Self);
  fOptions.assign(self);
  fOptions.Name:= 'messageOptions';
  fOptionsBackup := TCEMessagesOptions.Create(Self);
  //
  List.PopupMenu := contextMenu;
  List.OnDeletion := @ListDeletion;
  List.OnDblClick := @handleMessageClick;
  //
  btnSelProj.OnClick  := @selCtxtClick;
  btnSelMisc.OnClick  := @selCtxtClick;
  btnSelEdit.OnClick  := @selCtxtClick;
  btnSelApp.OnClick   := @selCtxtClick;
  btnSelAll.OnClick   := @selCtxtClick;
  fBtns[amcAll] := btnSelAll;
  fBtns[amcApp] := btnSelApp;
  fBtns[amcEdit]:= btnSelEdit;
  fBtns[amcMisc]:= btnSelMisc;
  fBtns[amcProj]:= btnSelProj;
  //
  btnClearCat.OnClick := @actClearCurCatExecute;
  png:= TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('clean');
    btnClearCat.Glyph.Assign(png);
  finally
    png.free;
  end;
  //
  fname := getCoeditDocPath + optname;
  if fileExists(fname) then
  begin
    fOptions.loadFromFile(fname);
    fOptions.AssignTo(self);
  end;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
end;

destructor TCEMessagesWidget.destroy;
begin
  fOptions.saveToFile(getCoeditDocPath + optname);
  EntitiesConnector.removeObserver(self);
  Inherited;
end;

procedure TCEMessagesWidget.listDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data <> nil then
    Dispose(PMessageData(Node.Data));
end;

procedure TCEMessagesWidget.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  case Key of
    VK_BACK, VK_DELETE:
    begin
      if List.SelectionCount > 0 then
      begin
      for i := List.Items.Count-1 downto 0 do
        if List.Items[i].MultiSelected then
          List.Items.Delete(List.Items[i]);
      end
      else clearbyContext(amcAll);
    end;
    VK_UP, VK_DOWN:
      if fOptions.singleMessageClick then handleMessageClick(nil);
    VK_RETURN:
      handleMessageClick(nil);
  end;
end;

procedure TCEMessagesWidget.selCtxtClick(Sender: TObject);
var
  btn: TToolButton;
  i: Integer;
begin
  if sender = nil then
    exit;
  //
  fCtxt := amcAll;
  btn := TToolButton(Sender);
  for i := 0 to selCtxt.ButtonCount-1 do
    selCtxt.Buttons[i].Down := selCtxt.Buttons[i] = btn;
  if btn = btnSelAll  then
    fCtxt := amcAll
  else if btn = btnSelEdit then
    fCtxt := amcEdit
  else if btn = btnSelProj then
    fCtxt := amcProj
  else if btn = btnSelApp then
    fCtxt := amcApp
  else if btn = btnSelMisc then
    fCtxt := amcMisc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.setMaxMessageCount(aValue: Integer);
begin
  if aValue < 5 then
    aValue := 5;
  if fMaxMessCnt = aValue then
    exit;
  fMaxMessCnt := aValue;
  clearOutOfRangeMessg;
end;

procedure TCEMessagesWidget.setAutoSelectCategory(aValue: boolean);
begin
  fAutoSelect := aValue;
  fActAutoSel.Checked:= fAutoSelect;
end;

procedure TCEMessagesWidget.setSingleMessageClick(aValue: boolean);
begin
  fSingleClick := aValue;
  if fSingleClick then
  begin
    List.OnClick := @handleMessageClick;
    List.OnDblClick:= nil;
  end else begin
    List.OnClick := nil;
    List.OnDblClick:= @handleMessageClick;
  end;
end;

procedure TCEMessagesWidget.setColorError(aValue: TColor);
begin
  fMsgColors[amkErr] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorInfo(aValue: TColor);
begin
  fMsgColors[amkInf] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorHint(aValue: TColor);
begin
  fMsgColors[amkHint] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorBuble(aValue: TColor);
begin
  fMsgColors[amkBub] := max(aValue, minColor);
  List.Invalidate;
end;

procedure TCEMessagesWidget.setColorWarning(aValue: TColor);
begin
  fMsgColors[amkWarn] := aValue;
  List.Invalidate;
end;

procedure TCEMessagesWidget.ListCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  x: integer;
  rc: TRect;
begin
  rc := node.DisplayRect(false);
  x := rc.Left + 2 - TTreeHack(list).ScrolledLeft;
  // warning: the cast may become wrong if the enum is modified.
  Sender.Canvas.Brush.Color := fMsgColors[TCEAppMessageKind(node.ImageIndex + 1)];
  if node.Selected then
  begin
    Sender.Canvas.DrawFocusRect(rc);
    Sender.Canvas.Brush.Color := Sender.Canvas.Brush.Color - minColor;
  end;
  Sender.Canvas.FillRect(rc);
  list.Images.Draw(sender.Canvas, x, (rc.Top + rc.Bottom - list.Images.Height) div 2,
    node.ImageIndex, Node.NodeEffect);
  x += list.Images.Width + 5;
  Sender.Canvas.TextOut(x, rc.Top, node.Text);
  DefaultDraw := false;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCEMessagesWidget.optionedWantCategory(): string;
begin
  exit('Messages');
end;

function TCEMessagesWidget.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEMessagesWidget.optionedWantContainer: TPersistent;
begin
  fOptions.assign(self);
  fOptionsBackup.assign(self);
  exit(fOptions);
end;

procedure TCEMessagesWidget.optionedEvent(anEvent: TOptionEditorEvent);
begin
  case anEvent of
    oeeAccept, oeeSelectCat:
      fOptionsBackup.assign(fOptions);
    oeeCancel:
      fOptions.assign(fOptionsBackup);
  end;
  fOptions.AssignTo(self);
  List.Invalidate;
end;

function TCEMessagesWidget.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TCEMessagesWidget.contextActionCount: integer;
begin
  result := 6;
end;

function TCEMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActAutoSel;
    1: result := fActClearAll;
    2: result := fActClearCurCat;
    3: result := fActCopyMsg;
    4: result := fActSelAll;
    5: result := fActSaveMsg;
    else result := nil;
  end;
end;

procedure TCEMessagesWidget.actAutoSelExecute(Sender: TObject);
begin
  fAutoSelect := fActAutoSel.Checked;
end;

procedure TCEMessagesWidget.actClearAllExecute(Sender: TObject);
begin
  clearbyContext(amcAll);
end;

procedure TCEMessagesWidget.actClearCurCatExecute(Sender: TObject);
begin
  case fCtxt of
    amcAll, amcApp, amcMisc :
      clearbyContext(fCtxt);
    amcEdit: if fDoc <> nil then
      clearbyData(fDoc);
    amcProj: if fProj <> nil then
      clearbyData(fProj);
  end;
end;

procedure TCEMessagesWidget.actCopyMsgExecute(Sender: TObject);
var
  i: Integer;
  str: string = '';
begin
  for i := 0 to List.Items.Count-1 do
    if List.Items[i].MultiSelected then
      str += List.Items[i].Text + LineEnding;
  Clipboard.AsText := str;
end;

procedure TCEMessagesWidget.actSelAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to List.Items.Count-1 do
    if List.Items[i].Visible then
      List.Items[i].MultiSelected := true;
end;

procedure TCEMessagesWidget.actSaveMsgExecute(Sender: TObject);
var
  lst: TStringList;
  itm: TtreeNode;
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      lst := TStringList.Create;
      try
        for itm in List.Items do
          lst.Add(itm.Text);
        lst.SaveToFile(filename);
      finally
        lst.Free;
      end;
    end;
  finally
    free;
  end;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEMessagesWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  //
  clearbyData(fProj);
  fProj := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projFocused(aProject: ICECommonProject);
begin
  if fProj = aProject then exit;
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projChanged(aProject: ICECommonProject);
begin
end;

procedure TCEMessagesWidget.projCompiling(aProject: ICECommonProject);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEMessagesWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docClosing(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  clearbyData(fDoc);
  fDoc := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docFocused(aDoc: TCESynMemo);
begin
  if fDoc = aDoc then exit;
  fDoc := aDoc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docChanged(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;
{$ENDREGION}

{$REGION ICEMessagesDisplay ----------------------------------------------------}
function TCEMessagesWidget.singleServiceName: string;
begin
  exit('ICEMessagesDisplay');
end;

procedure TCEMessagesWidget.message(const aValue: string; aData: Pointer;
  aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
var
  dt: PMessageData;
  item: TTreeNode;
begin
  showWidget;
  if aKind = amkAuto then
    aKind := guessMessageKind(aValue);
  dt := new(PMessageData);
  dt^.data := aData;
  dt^.ctxt := aCtxt;
  if fAutoSelect then if fCtxt <> aCtxt then
    fBtns[aCtxt].Click;
  if fastDisplay then
    IncLoopUpdate;
  item := List.Items.Add(nil, aValue);
  item.Data := dt;
  item.ImageIndex := iconIndex(aKind);
  item.SelectedIndex := item.ImageIndex;
  if not fastDisplay then
  begin
    clearOutOfRangeMessg;
    scrollToBack;
    TTreeHack(list).scrolledLeft := 0;
    List.Update;
    filterMessages(fCtxt);
  end;
end;

procedure TCEMessagesWidget.clearByContext(aCtxt: TCEAppMessageCtxt);
var
  i: Integer;
  msgdt: PMessageData;
begin
  list.BeginUpdate;
  if aCtxt = amcAll then
    List.Items.Clear
  else for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if msgdt^.ctxt = aCtxt then
      List.Items.Delete(List.Items[i]);
  end;
  list.EndUpdate;
end;

procedure TCEMessagesWidget.clearByData(aData: Pointer);
var
  i: Integer;
  msgdt: PMessageData;
begin
  if aData = nil then
    exit;
  list.BeginUpdate;
  for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if (msgdt^.data = aData) then
      List.Items.Delete(List.Items[i]);
  end;
  list.EndUpdate;
end;
{$ENDREGION}

{$REGION Messages --------------------------------------------------------------}
procedure TCEMessagesWidget.updateLoop;
begin
  clearOutOfRangeMessg;
  scrollToBack;
  List.Update;
  filterMessages(fCtxt);
end;

function TCEMessagesWidget.iconIndex(aKind: TCEAppMessageKind): Integer;
begin
  case aKind of
    amkBub:  exit(0);
    amkInf:  exit(1);
    amkHint: exit(2);
    amkWarn: exit(3);
    amkErr:  exit(4);
    else exit(0);
  end;
end;

procedure TCEMessagesWidget.clearOutOfRangeMessg;
begin
  list.BeginUpdate;
  while List.Items.Count > fMaxMessCnt do
    List.Items.Delete(List.Items.GetFirstNode);
  list.EndUpdate;
end;

procedure TCEMessagesWidget.scrollToBack;
begin
  if not Visible then
    exit;
  if List.BottomItem <> nil then
    List.BottomItem.MakeVisible;
end;

procedure TCEMessagesWidget.handleMessageClick(Sender: TObject);
var
  pos: TPoint;
  msg: string;
begin
  if List.Selected = nil then
    exit;
  msg := List.Selected.Text;
  if not openFileFromDmdMessage(msg) then
    exit;
  // from here, since a doc has the focus, List.Selected is nil
  pos := getLineFromMessage(msg);
  if fDoc = nil then
    exit;
  fDoc.CaretXY := pos;
  fDoc.SelectLine;
end;

procedure TCEMessagesWidget.filterMessages(aCtxt: TCEAppMessageCtxt);
var
  msgdt: PMessageData;
  itm: TTreeNode;
  i: Integer;
begin
  if updating then
    exit;
  List.BeginUpdate;
  for i := 0 to List.Items.Count-1 do
  begin
    itm := List.Items[i];
    Itm.Visible := false;
    Itm.Selected := false;
    msgdt := PMessageData(itm.Data);
    if aCtxt = amcAll then
      Itm.Visible := true
    else case msgdt^.ctxt of
      amcEdit: itm.Visible := (fDoc  = TCESynMemo(msgdt^.data)) and (aCtxt = amcEdit);
      amcProj: itm.Visible := (fProj = ICECommonProject(msgdt^.data)) and (aCtxt = amcProj);
      amcApp:  itm.Visible := aCtxt = amcApp;
      amcMisc: itm.Visible := aCtxt = amcMisc;
    end;
  end;
  list.EndUpdate;
end;

function guessMessageKind(const aMessg: string): TCEAppMessageKind;
var
  pos: Integer = 1;
  idt: string = '';
function checkIdent: TCEAppMessageKind;
begin
  case idt of
    'ERROR', 'error', 'Error', 'Invalid', 'invalid',
    'exception', 'Exception', 'illegal', 'Illegal',
    'fatal', 'Fatal', 'Critical', 'critical', 'errorlevel':
      exit(amkErr);
    'Warning', 'warning', 'caution', 'Caution', 'warn', 'Warn':
      exit(amkWarn);
    'Hint', 'hint', 'Tip', 'tip', 'advice', 'Advice',
    'suggestion', 'Suggestion':
      exit(amkHint);
    'Information', 'information':
      exit(amkInf);
    else
      exit(amkBub);
  end;
end;
begin
  result := amkBub;
  while(true) do
  begin
    if pos > length(aMessg) then
      exit;
    if aMessg[pos] in [#0..#32, ',', ':', ';'] then
    begin
      Inc(pos);
      result := checkIdent;
      if result <> amkBub then
        exit;
      idt := '';
      continue;
    end;
    if not (aMessg[pos] in ['a'..'z', 'A'..'Z']) then
    begin
      Inc(pos);
      result := checkIdent;
      if result <> amkBub then exit;
      idt := '';
      continue;
    end;
    idt += aMessg[pos];
    Inc(pos);
  end;
end;

function getLineFromMessage(const aMessage: string): TPoint;
var
  i, j: Integer;
  ident: string = '';
begin
  result.x := 0;
  result.y := 0;
  i := 1;
  while (true) do
  begin
    if i > length(aMessage) then exit;
    if aMessage[i] = '(' then
    begin
      inc(i);
      if i > length(aMessage) then exit;
      while( isNumber(aMessage[i]) or (aMessage[i] = ',') or (aMessage[i] = ':')) do
      begin
        ident += aMessage[i];
        inc(i);
        if i > length(aMessage) then exit;
      end;
      if aMessage[i] = ')' then
      begin
        j := Pos(',', ident);
        if j = 0 then j := Pos(':', ident);
        if j = 0 then
          result.y := strToIntDef(ident, -1)
        else
        begin
          result.y := strToIntDef(ident[1..j-1], -1);
          result.x := strToIntDef(ident[j+1..length(ident)], -1);
        end;
        exit;
      end;
    end;
    inc(i);
  end;
end;

function openFileFromDmdMessage(const aMessage: string): boolean;
var
  i: integer = 0;
  ident: string = '';
  absName: string;
begin
  result := false;
  while (true) do
  begin
    inc(i);
    if i > length(aMessage) then
      exit;
    // '(': line will be indicated after fname
    // -mixin: dmd, error in mixin(token string) '<fname>-mixinXX<index>('
    if isEditable(extractFileExt(ident)) and ((aMessage[i] = '(') or
      ((aMessage[i] = '-') and (i < length(aMessage)-5)
        and (aMessage[i..i+5] = '-mixin'))) then
    begin
      // absolute fname
      if fileExists(ident) then
      begin
        getMultiDocHandler.openDocument(ident);
        exit(true);
      end;
      // relative fname if project file is the base path to a rel. fname
      absName := ExpandFileName(ident);
      if fileExists(absName) then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
      // if fname relative to native project path or project filed 'root'
      absName := expandFilenameEx(symbolExpander.get('<CPP>') + DirectorySeparator, ident);
      if fileExists(absName) then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
      absName := expandFilenameEx(symbolExpander.get('<CPR>') + DirectorySeparator, ident);
      if fileExists(absName) then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
    end
    // <assertion failure messg>@<filename>
    else if aMessage[i] = '@' then
      ident := ''
    else
      ident += aMessage[i];
  end;
end;
{$ENDREGION}

end.
