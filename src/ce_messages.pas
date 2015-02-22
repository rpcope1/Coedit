unit ce_messages;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  lcltype, ce_widget, ActnList, Menus, clipbrd, AnchorDocking, Buttons, ce_writableComponent,
  ce_common, ce_project, ce_synmemo, ce_dlangutils, ce_interfaces, ce_observer;

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
    fMaxCount: Integer;
    fAutoSelect: boolean;
    fSingleClick: boolean;
    fFont: TFont;
    procedure setFont(aValue: TFont);
  published
    property maxMessageCount: integer read fMaxCount write fMaxCount;
    property autoSelect: boolean read fAutoSelect write fAutoSelect;
    property singleMessageClick: boolean read fSingleClick write fSingleClick;
    property font: TFont read fFont write setFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  TCEMessagesWidget = class(TCEWidget, ICEEditableOptions, ICEMultiDocObserver, ICEProjectObserver, ICEMessagesDisplay)
    btnClearCat: TBitBtn;
    imgList: TImageList;
    List: TTreeView;
    selCtxt: TToolBar;
    btnSelAll: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    btnSelMisc: TToolButton;
    ToolButton2: TToolButton;
    btnSelEdit: TToolButton;
    ToolButton4: TToolButton;
    btnSelProj: TToolButton;
    ToolButton8: TToolButton;
    btnSelApp: TToolButton;
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fActAutoSel: TAction;
    fActClearAll: TAction;
    fActClearCurCat: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fMaxMessCnt: Integer;
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fCtxt: TCEAppMessageCtxt;
    fAutoSelect: boolean;
    fSingleClick: boolean;
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
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
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
    //
    function singleServiceName: string;
    procedure message(const aValue: string; aData: Pointer; aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
    procedure clearbyContext(aCtxt: TCEAppMessageCtxt);
    procedure clearbyData(aData: Pointer);
  protected
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    property maxMessageCount: Integer read fMaxMessCnt write setMaxMessageCount;
    property autoSelectCategory: boolean read fAutoSelect write setAutoSelectCategory;
    property singleMessageClick: boolean read fSingleClick write setSingleMessageClick;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure scrollToBack;
  end;

  function guessMessageKind(const aMessg: string): TCEAppMessageKind;
  function getLineFromMessage(const aMessage: string): TPoint;
  function openFileFromDmdMessage(const aMessage: string): boolean;

implementation
{$R *.lfm}

const
  optname = 'messages.txt';

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
    fFont.EndUpdate;
  end
  else if Source is TCEMessagesWidget then
  begin
    widg := TCEMessagesWidget(Source);
    fFont.Assign(widg.List.Font);
    fMaxCount := widg.fMaxMessCnt;
    fAutoSelect := widg.fAutoSelect;
    fSingleClick := widg.fSingleClick;
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
  end
  else inherited;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMessagesWidget.create(aOwner: TComponent);
var
  fname: string;
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
  fOptions := TCEMessagesOptions.Create(Self);
  fOptions.assign(self);
  fOptions.Name:= 'messageOptions';
  fOptionsBackup := TCEMessagesOptions.Create(Self);
  //
  List.PopupMenu := contextMenu;
  List.OnDeletion := @ListDeletion;
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
  i: NativeInt;
begin
  if Key in [VK_BACK, VK_DELETE] then
  begin
    if List.SelectionCount > 0 then
    begin
    for i := List.Items.Count-1 downto 0 do
      if List.Items[i].MultiSelected then
        List.Items.Delete(List.Items[i]);
    end
    else clearbyContext(amcAll);
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
    oeeAccept:
      fOptionsBackup.assign(fOptions);
    oeeCancel:
      fOptions.assign(fOptionsBackup);
  end;
  fOptions.AssignTo(self);
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
  i: NativeInt;
  str: string;
begin
  str := '';
  for i := 0 to List.Items.Count-1 do
      if List.Items[i].MultiSelected then
        str += List.Items[i].Text + LineEnding;
  Clipboard.AsText := str;
end;

procedure TCEMessagesWidget.actSelAllExecute(Sender: TObject);
var
  i: NativeInt;
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
procedure TCEMessagesWidget.projNew(aProject: TCEProject);
begin
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then
    exit;
  //
  clearbyData(aProject);
  fProj := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projFocused(aProject: TCEProject);
begin
  if fProj = aProject then exit;
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projChanged(aProject: TCEProject);
begin
end;

procedure TCEMessagesWidget.projCompiling(aProject: TCEProject);
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
  if aKind = amkAuto then
    aKind := guessMessageKind(aValue);
  dt := new(PMessageData);
  dt^.data := aData;
  dt^.ctxt := aCtxt;
  if fAutoSelect then if fCtxt <> aCtxt then
    fBtns[aCtxt].Click;
  item := List.Items.Add(nil, aValue);
  item.Data := dt;
  item.ImageIndex := iconIndex(aKind);
  item.SelectedIndex := item.ImageIndex;
  clearOutOfRangeMessg;
  scrollToBack;
  Application.ProcessMessages;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.clearByContext(aCtxt: TCEAppMessageCtxt);
var
  i: Integer;
  msgdt: PMessageData;
begin
  if aCtxt = amcAll then
    List.Items.Clear
  else for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if msgdt^.ctxt = aCtxt then
      List.Items.Delete(List.Items[i]);
  end;
end;

procedure TCEMessagesWidget.clearByData(aData: Pointer);
var
  i: Integer;
  msgdt: PMessageData;
begin
  if aData = nil then
    exit;
  for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if (msgdt^.data = aData) then
      List.Items.Delete(List.Items[i]);
  end;
end;
{$ENDREGION}

{$REGION Messages --------------------------------------------------------------}
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
  while List.Items.Count > fMaxMessCnt do
    List.Items.Delete(List.Items.GetFirstNode);
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
  i: NativeInt;
begin
  if updating then
    exit;
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
      amcProj: itm.Visible := (fProj = TCEProject(msgdt^.data)) and (aCtxt = amcProj);
      amcApp:  itm.Visible := aCtxt = amcApp;
      amcMisc: itm.Visible := aCtxt = amcMisc;
    end;
  end;
end;

function guessMessageKind(const aMessg: string): TCEAppMessageKind;
var
  pos: Nativeint;
  idt: string;
function checkIdent: TCEAppMessageKind;
begin
  case idt of
    'ERROR', 'error', 'Error', 'Invalid', 'invalid',
    'exception', 'Exception', 'illegal', 'Illegal',
    'fatal', 'Fatal', 'Critical', 'critical':
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
  idt := '';
  pos := 1;
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
  i, j: NativeInt;
  ident: string;
begin
  result.x := 0;
  result.y := 0;
  ident := '';
  i := 1;
  while (true) do
  begin
    if i > length(aMessage) then exit;
    if aMessage[i] = '.' then
    begin
      inc(i);
      if i > length(aMessage) then exit;
      if aMessage[i] = 'd' then
      begin
        inc(i);
        if i > length(aMessage) then exit;
        if aMessage[i] = 'i' then
          inc(i);
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
      end;
    end;
    inc(i);
  end;
end;

function openFileFromDmdMessage(const aMessage: string): boolean;
var
  i: NativeInt;
  ident: string;
  ext: string;
begin
  ident := '';
  i := 0;
  result := false;
  while(true) do
  begin
    inc(i);
    if i > length(aMessage) then
      exit;
    if aMessage[i] = '(' then
    begin
      if not fileExists(ident) then
        exit;
      ext := extractFileExt(ident);
      if dExtList.IndexOf(ext) = -1 then
        exit;
      getMultiDocHandler.openDocument(ident);
      result := true;
    end;
    ident += aMessage[i];
  end;
end;
{$ENDREGION}

end.
