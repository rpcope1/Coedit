unit ce_messages;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  lcltype, ce_widget, ActnList, Menus, clipbrd, AnchorDocking, process, asyncprocess,
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

  { TCEMessagesWidget }
  TCEMessagesWidget = class(TCEWidget, ICEMultiDocObserver, ICEProjectObserver, ICELogMessageObserver)
    imgList: TImageList;
    List: TTreeView;
    selCtxt: TToolBar;
    btnSelAll: TToolButton;
    ToolButton10: TToolButton;
    btnSelMisc: TToolButton;
    ToolButton2: TToolButton;
    btnSelEdit: TToolButton;
    ToolButton4: TToolButton;
    btnSelProj: TToolButton;
    ToolButton8: TToolButton;
    btnSelApp: TToolButton;
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fActClearAll: TAction;
    fActClearEdi: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fMaxMessCnt: Integer;
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fCtxt: TCEAppMessageCtxt;
    procedure filterMessages(aCtxt: TCEAppMessageCtxt);
    procedure clearOutOfRangeMessg;
    procedure actClearEdiExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
    procedure actCopyMsgExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
    procedure setMaxMessageCount(aValue: Integer);
    procedure listDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCtxtClick(Sender: TObject);
    function iconIndex(aKind: TCEAppMessageKind): Integer;
    //
    procedure optset_MaxMessageCount(aReader: TReader);
    procedure optget_MaxMessageCount(awriter: TWriter);
  published
    property maxMessageCount: Integer read fMaxMessCnt write setMaxMessageCount default 125;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure scrollToBack;
    //
    procedure sesoptDeclareProperties(aFiler: TFiler); override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure projNew(const aProject: TCEProject);
    procedure projClosing(const aProject: TCEProject);
    procedure projFocused(const aProject: TCEProject);
    procedure projChanged(const aProject: TCEProject);
    //
    procedure docNew(const aDoc: TCESynMemo);
    procedure docClosing(const aDoc: TCESynMemo);
    procedure docFocused(const aDoc: TCESynMemo);
    procedure docChanged(const aDoc: TCESynMemo);
    //
    procedure lmFromString(const aValue: string; aData: Pointer; aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
    procedure lmClearbyContext(aCtxt: TCEAppMessageCtxt);
    procedure lmClearbyData(aData: Pointer);
  end;

  TMessageKind = (msgkUnknown, msgkInfo, msgkHint, msgkWarn, msgkError);

  function semanticMsgAna2(const aMessg: string): TCEAppMessageKind;
  function getLineFromDmdMessage(const aMessage: string): TPoint;
  function openFileFromDmdMessage(const aMessage: string): boolean;

implementation
{$R *.lfm}

uses
  ce_main;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMessagesWidget.create(aOwner: TComponent);
begin
  fMaxMessCnt := 125;
  fCtxt := amcAll;
  //
  fActClearAll := TAction.Create(self);
  fActClearAll.OnExecute := @actClearAllExecute;
  fActClearAll.caption := 'Clear all messages';
  fActClearEdi := TAction.Create(self);
  fActClearEdi.OnExecute := @actClearEdiExecute;
  fActClearEdi.caption := 'Clear editor messages';
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
  List.PopupMenu := contextMenu;
  List.OnDeletion := @ListDeletion;
  //
  btnSelProj.OnClick  := @selCtxtClick;
  btnSelMisc.OnClick  := @selCtxtClick;
  btnSelEdit.OnClick  := @selCtxtClick;
  btnSelApp.OnClick   := @selCtxtClick;
  btnSelAll.OnClick   := @selCtxtClick;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.endUpdate;
end;

destructor TCEMessagesWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  Inherited;
end;

procedure TCEMessagesWidget.listDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data <> nil then
    Dispose( PMessageData(Node.Data));
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
    else lmClearbyContext(amcAll);
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
{$ENDREGION}

{$REGION ICESessionOptionsObserver ------------------------------------------------------}
procedure TCEMessagesWidget.setMaxMessageCount(aValue: Integer);
begin
  if aValue < 10 then aValue := 10;
  if aValue > 1023 then aValue := 1023;
  if fMaxMessCnt = aValue then exit;
  fMaxMessCnt := aValue;
  clearOutOfRangeMessg;
end;

procedure TCEMessagesWidget.optset_MaxMessageCount(aReader: TReader);
begin
  maxMessageCount := aReader.ReadInteger;
end;

procedure TCEMessagesWidget.optget_MaxMessageCount(aWriter: TWriter);
begin
  aWriter.WriteInteger(fMaxMessCnt);
end;

procedure TCEMessagesWidget.sesoptDeclareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_MaxMessageCount', @optset_MaxMessageCount, @optget_MaxMessageCount, true);
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TCEMessagesWidget.contextActionCount: integer;
begin
  result := 5;
end;

function TCEMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActClearAll;
    1: result := fActClearEdi;
    2: result := fActCopyMsg;
    3: result := fActSelAll;
    4: result := fActSaveMsg;
    else result := nil;
  end;
end;

procedure TCEMessagesWidget.actClearAllExecute(Sender: TObject);
begin
  lmClearbyContext(amcAll);
end;

procedure TCEMessagesWidget.actClearEdiExecute(Sender: TObject);
begin
  lmClearbyData(@fDoc);
end;

procedure TCEMessagesWidget.actCopyMsgExecute(Sender: TObject);
var
  i: NativeInt;
  str: string;
begin
  str := '';
  for i := 0 to List.Items.Count-1 do if List.Items[i].MultiSelected then
    str += List.Items[i].Text + LineEnding;
  Clipboard.AsText := str;
end;

procedure TCEMessagesWidget.actSelAllExecute(Sender: TObject);
var
  i: NativeInt;
begin
  for i := 0 to List.Items.Count-1 do
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
procedure TCEMessagesWidget.projNew(const aProject: TCEProject);
begin
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projClosing(const aProject: TCEProject);
begin
  if fProj <> aProject then
    exit;
  //
  lmClearByData(aProject);
  fProj := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projFocused(const aProject: TCEProject);
begin
  fProj := aProject;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.projChanged(const aProject: TCEProject);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEMessagesWidget.docNew(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docClosing(const aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  lmClearbyData(@fDoc);
  fDoc := nil;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docFocused(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  filterMessages(fCtxt);
end;

procedure TCEMessagesWidget.docChanged(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;
{$ENDREGION}

{$REGION ICELogMessageObserver -------------------------------------------------}
procedure TCEMessagesWidget.lmFromString(const aValue: string; aData: Pointer;
  aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
var
  dt: PMessageData;
   item: TTreeNode;
begin
  if aKind = amkAuto then
    aKind := semanticMsgAna2(aValue);
  dt := new(PMessageData);
  dt^.data := aData;
  dt^.ctxt := aCtxt;
  item := List.Items.Add(nil, aValue);
  item.Data := dt;
  item.ImageIndex := iconIndex(aKind);
  item.SelectedIndex := item.ImageIndex;
  clearOutOfRangeMessg;
  scrollToBack;
  Application.ProcessMessages;
end;

procedure TCEMessagesWidget.lmClearByContext(aCtxt: TCEAppMessageCtxt);
var
  i: Integer;
  msgdt: PMessageData;
begin
  if aCtxt = amcAll then
  begin
    List.Items.Clear;
    exit;
  end;
  for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if msgdt^.ctxt = aCtxt then
      List.Items.Delete(List.Items[i]);
  end;
end;

procedure TCEMessagesWidget.lmClearByData(aData: Pointer);
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
  if not Visible then exit;
  if List.BottomItem <> nil then
    List.BottomItem.MakeVisible;
end;

procedure TCEMessagesWidget.ListDblClick(Sender: TObject);
var
  pos: TPoint;
  msg: string;
begin
  if List.Selected = nil then exit;
  msg := List.Selected.Text;
  if not openFileFromDmdMessage(msg) then exit;
  // from here since a doc has the focus,  List.Selected is nil
  pos := getLineFromDmdMessage(msg);
  if fDoc = nil then exit;
  fDoc.CaretXY := pos;
  fDoc.SelectLine;
end;

procedure TCEMessagesWidget.filterMessages(aCtxt: TCEAppMessageCtxt);
var
  msgdt: PMessageData;
  itm: TTreeNode;
  i: NativeInt;
begin
  if updating then exit;
  for i := 0 to List.Items.Count-1 do
  begin
    itm := List.Items[i];
    Itm.Visible := false;
    msgdt := PMessageData(itm.Data);
    if aCtxt = amcAll then
    begin
      Itm.Visible := true;
      continue;
    end
    else case msgdt^.ctxt of
      amcEdit: itm.Visible := (fDoc  = TCESynMemo(msgdt^.data)) and (aCtxt = amcEdit);
      amcProj: itm.Visible := (fProj = TCEProject(msgdt^.data)) and (aCtxt = amcProj);
      amcApp:  itm.Visible := aCtxt = amcApp;
      amcMisc: itm.Visible := aCtxt = amcMisc;
    end;
  end;
end;

function semanticMsgAna2(const aMessg: string): TCEAppMessageKind;
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
    'Warning', 'warning', 'caution', 'Caution':
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
    if pos > length(aMessg) then exit;
    if aMessg[pos] in [#0..#32] then
    begin
      Inc(pos);
      result := checkIdent;
      if result <> amkBub then exit;
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

function getLineFromDmdMessage(const aMessage: string): TPoint;
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
        if aMessage[i] = '(' then
        begin
          inc(i);
          if i > length(aMessage) then exit;
          while( isNumber(aMessage[i]) or (aMessage[i] = ',')) do
          begin
            ident += aMessage[i];
            inc(i);
            if i > length(aMessage) then exit;
          end;
          if aMessage[i] = ')' then
          begin
            j := Pos(',', ident);
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
    if i > length(aMessage) then exit;
    if aMessage[i] = '(' then
    begin
      if not fileExists(ident) then exit;
      ext := extractFileExt(ident);
      if not (ext = '.d') or (ext = '.di') then exit;
      CEMainForm.openFile(ident);
      result := true;
    end;
    ident += aMessage[i];
  end;
end;
{$ENDREGION}

end.
