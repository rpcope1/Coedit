unit ce_editor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, lcltype,
  Graphics, SynEditKeyCmds, ComCtrls, SynEditHighlighter, ExtCtrls, Menus,
  SynMacroRecorder, SynPluginSyncroEdit, SynEdit, SynHighlighterMulti, ce_dialogs,
  ce_widget, ce_interfaces, ce_synmemo, ce_dlang, ce_common, ce_dcd, ce_observer;

type

  // this descendant propagates the Visible property to the children.
  // this fix the bug described in commit c1a0ed2799390d788b1d1e435eb8dc1ed3369ce7
  TCEEditorPage = class(TTabSheet)
  protected
    procedure SetVisible(Value: Boolean); override;
  end;

  { TCEEditorWidget }

  TCEEditorWidget = class(TCEWidget, ICEMultiDocObserver, ICEMultiDocHandler)
    mnuedCopy: TMenuItem;
    mnuedCut: TMenuItem;
    mnuedPaste: TMenuItem;
    MenuItem4: TMenuItem;
    mnuedUndo: TMenuItem;
    mnuedRedo: TMenuItem;
    MenuItem7: TMenuItem;
    mnuedJum2Decl: TMenuItem;
    PageControl: TExtendedNotebook;
    macRecorder: TSynMacroRecorder;
    editorStatus: TStatusBar;
    mnuEditor: TPopupMenu;
    procedure mnuedCopyClick(Sender: TObject);
    procedure mnuedCutClick(Sender: TObject);
    procedure mnuEditorPopup(Sender: TObject);
    procedure mnuedPasteClick(Sender: TObject);
    procedure mnuedUndoClick(Sender: TObject);
    procedure mnuedRedoClick(Sender: TObject);
    procedure mnuedJum2DeclClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  protected
    procedure updateDelayed; override;
    procedure updateImperative; override;
  private
    fKeyChanged: boolean;
    fDoc: TCESynMemo;
    fTokList: TLexTokenList;
    fErrList: TLexErrorList;
    fModStart: boolean;
    fLastCommand: TSynEditorCommand;
    {$IFDEF LINUX}
    procedure pageCloseBtnClick(Sender: TObject);
    {$ENDIF}
    procedure lexFindToken(const aToken: PLexToken; out doStop: boolean);
    procedure memoKeyPress(Sender: TObject; var Key: char);
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoCtrlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure getSymbolLoc;
    procedure focusedEditorChanged;
    procedure memoCmdProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    //
    function SingleServiceName: string;
    function documentCount: Integer;
    function getDocument(index: Integer): TCESynMemo;
    function findDocument(aFilename: string): TCESynMemo;
    procedure openDocument(aFilename: string);
    function closeDocument(index: Integer): boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function closeQuery: boolean; override;
  end;

implementation
{$R *.lfm}

procedure TCEEditorPage.SetVisible(Value: Boolean);
var
  i: integer;
begin
  inherited;
  for i := 0 to ControlCount-1 do
    Controls[i].Visible:= Value;
end;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEEditorWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  //
  fTokList := TLexTokenList.Create;
  fErrList := TLexErrorList.Create;
  {$IFDEF LINUX}
  PageControl.OnCloseTabClicked := @pageCloseBtnClick;
  {$ENDIF}
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('copy');
    mnuedCopy.Bitmap.Assign(png);
    png.LoadFromLazarusResource('cut');
    mnuedCut.Bitmap.Assign(png);
    png.LoadFromLazarusResource('paste');
    mnuedPaste.Bitmap.Assign(png);
    png.LoadFromLazarusResource('arrow_undo');
    mnuedUndo.Bitmap.Assign(png);
    png.LoadFromLazarusResource('arrow_redo');
    mnuedRedo.Bitmap.Assign(png);
    png.LoadFromLazarusResource('arrow_shoe');
    mnuedJum2Decl.Bitmap.Assign(png);
  finally
    png.Free;
  end;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
end;

destructor TCEEditorWidget.destroy;
var
  i: integer;
begin
  EntitiesConnector.removeObserver(self);
  for i := PageControl.PageCount-1 downto 0 do
    if PageControl.Page[i].ControlCount > 0 then
      if (PageControl.Page[i].Controls[0] is TCESynMemo) then
        PageControl.Page[i].Controls[0].Free;
  fTokList.Free;
  fErrList.Free;
  inherited;
end;

function TCEEditorWidget.closeQuery: boolean;
begin
  result := inherited;
  result := result and (Parent = nil);
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEEditorWidget.docNew(aDoc: TCESynMemo);
var
  sheet: TCEEditorPage;
begin
  sheet := TCEEditorPage.Create(self);
  sheet.PageControl := PageControl;
  //
  aDoc.Align := alClient;
  aDoc.Parent := sheet;
  //
  aDoc.OnKeyDown := @memoKeyDown;
  aDoc.OnKeyUp := @memoKeyUp;
  aDoc.OnKeyPress := @memoKeyPress;
  aDoc.OnMouseDown := @memoMouseDown;
  aDoc.OnMouseMove := @memoMouseMove;
  aDoc.OnClickLink := @memoCtrlClick;
  aDoc.OnCommandProcessed:= @memoCmdProcessed;
  //
  fDoc := aDoc;
  pageControl.ActivePage := sheet;
  focusedEditorChanged;
  beginDelayedUpdate;
  updateImperative;
end;

procedure TCEEditorWidget.docClosing(aDoc: TCESynMemo);
var
  sheet: TWinControl;
begin
  if aDoc = nil then
    exit;
  sheet := aDoc.Parent;
  aDoc.Parent := nil;
  if aDoc = fDoc then
    fDoc := nil;
  if sheet <> nil then sheet.Free;
  updateImperative;
end;

procedure TCEEditorWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc = fDoc then exit;
  fDoc := aDoc;
  focusedEditorChanged;
  beginDelayedUpdate;
  updateImperative;
end;

procedure TCEEditorWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fKeyChanged := true;
  beginDelayedUpdate;
  updateImperative;
end;
{$ENDREGION}

{$REGION ICEMultiDocHandler ----------------------------------------------------}
function TCEEditorWidget.SingleServiceName: string;
begin
  exit('ICEMultiDocHandler');
end;

function TCEEditorWidget.documentCount: Integer;
begin
  exit(PageControl.PageCount);
end;

function TCEEditorWidget.getDocument(index: Integer): TCESynMemo;
begin
  exit(TCESynMemo(pageControl.Pages[index].Controls[0]));
end;

function TCEEditorWidget.findDocument(aFilename: string): TCESynMemo;
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount-1 do
  begin
    result := getDocument(i);
    if result.fileName = aFilename then
      exit;
  end;
  result := nil;
end;

procedure TCEEditorWidget.openDocument(aFilename: string);
var
  doc: TCESynMemo;
begin
  doc := findDocument(aFilename);
  if doc <> nil then begin
    PageControl.ActivePage := TTabSheet(doc.Parent);
    exit;
  end;
  doc := TCESynMemo.Create(nil);
  fDoc.loadFromFile(aFilename);
end;

function TCEEditorWidget.closeDocument(index: Integer): boolean;
var
  doc: TCESynMemo;
begin
  doc := getDocument(index);
  if doc.modified then if dlgOkCancel(format(
    'The latest "%s" modifications are not saved, continue ?',
    [shortenPath(doc.fileName,25)])) = mrCancel then exit(false);
  doc.Free;
  result := true;
end;
{$ENDREGION}

{$REGION PageControl/Editor things ---------------------------------------------}
{$IFDEF LINUX}
procedure TCEEditorWidget.pageCloseBtnClick(Sender: TObject);
begin
  if fDoc <> nil then fDoc.Free;
end;
{$ENDIF}

procedure TCEEditorWidget.focusedEditorChanged;
begin
  macRecorder.Clear;
  if fDoc = nil then exit;
  //
  macRecorder.Editor:= fDoc;
  fDoc.PopupMenu := mnuEditor;
  if (pageControl.ActivePage.Caption = '') then
  begin
    fKeyChanged := true;
    beginDelayedUpdate;
  end;
end;

procedure TCEEditorWidget.PageControlChange(Sender: TObject);
begin
  updateImperative;
end;

procedure TCEEditorWidget.memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_CLEAR,VK_RETURN,VK_BACK : fKeyChanged := true;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: updateImperative;
  end;
  if fKeyChanged then
    beginDelayedUpdate
  else if (Key = VK_UP) and (shift = [ssShift,ssCtrl]) then
    getSymbolLoc;
end;

procedure TCEEditorWidget.memoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case fLastCommand of
    ecSelectionStart..ecSelectionEnd: updateImperative;
  end;
end;

procedure TCEEditorWidget.memoCmdProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  fLastCommand := Command;
end;

procedure TCEEditorWidget.memoKeyPress(Sender: TObject; var Key: char);
begin
  fKeyChanged := true;
  beginDelayedUpdate;
end;

procedure TCEEditorWidget.memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  beginDelayedUpdate;
  updateImperative;
end;

procedure TCEEditorWidget.memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then exit;
  beginDelayedUpdate;
end;

procedure TCEEditorWidget.memoCtrlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  getSymbolLoc;
end;

procedure TCEEditorWidget.getSymbolLoc;
var
  srcpos, i, sum, linelen: Integer;
  fname: string;
  len: byte;
begin
  if not DcdWrapper.available then exit;
  //
  DcdWrapper.getDeclFromCursor(fname, srcpos);
  if fname <> fDoc.fileName then if fileExists(fname) then
    openDocument(fname);
  if srcpos <> -1 then
  begin
    sum := 0;
    len := getLineEndingLength(fDoc.fileName);
    for i := 0 to fDoc.Lines.Count-1 do
    begin
      linelen := length(fDoc.Lines.Strings[i]);
      if sum + linelen + len > srcpos then
      begin
        fDoc.CaretY := i + 1;
        fDoc.CaretX := srcpos - sum + len;
        fDoc.SelectWord;
        fDoc.EnsureCursorPosVisible;
        break;
      end;
      sum += linelen;
      sum += len;
    end;
  end;
end;

procedure TCEEditorWidget.updateImperative;
const
  modstr: array[boolean] of string = ('...', 'MODIFIED');
begin
  if fDoc = nil then begin
    editorStatus.Panels[0].Text := '';
    editorStatus.Panels[1].Text := '';
    editorStatus.Panels[2].Text := '';
  end else begin
    editorStatus.Panels[0].Text := format('%d : %d | %d', [fDoc.CaretY, fDoc.CaretX, fDoc.SelEnd - fDoc.SelStart]);
    editorStatus.Panels[1].Text := modstr[fDoc.modified];
    editorStatus.Panels[2].Text := fDoc.fileName;
  end;
end;

procedure TCEEditorWidget.lexFindToken(const aToken: PLexToken; out doStop: boolean);
begin
  if aToken^.kind = ltkKeyword then
    if aToken^.data = 'module' then
      fModStart := true;
  if fModStart then if aToken^.kind = ltkSymbol then
    if aToken^.data = ';' then begin
      doStop := true;
      fModStart := false;
    end;
end;

procedure TCEEditorWidget.updateDelayed;
var
  md: string;
begin
  if fDoc = nil then exit;
  updateImperative;
  if not fKeyChanged then exit;
  //
  fKeyChanged := false;
  if fDoc.Lines.Count = 0 then exit;
  //
  lex(fDoc.Lines.Text, fTokList, @lexFindToken);
  md := '';
  if fDoc.isDSource then
    md := getModuleName(fTokList);
  if md = '' then md := extractFileName(fDoc.fileName);
  pageControl.ActivePage.Caption := md;
  //
  fTokList.Clear;
  fErrList.Clear;
  // when a widget saves a temp file & syncro mode is on:
  // - editor is saved
  // - gutter is updated (green bar indicating a saved block)
  // - syncroedit icon is hidden
  if fDoc.syncroEdit.Active then
    fDoc.Refresh;
end;
{$ENDREGION}

{$REGION Editor context menu ---------------------------------------------------}
procedure TCEEditorWidget.mnuedCopyClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecCopy, '', nil);
end;

procedure TCEEditorWidget.mnuedCutClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecCut, '', nil);
end;

procedure TCEEditorWidget.mnuedPasteClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecPaste, '', nil);
end;

procedure TCEEditorWidget.mnuedUndoClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecUndo, '', nil);
end;

procedure TCEEditorWidget.mnuedRedoClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecRedo, '', nil);
end;

procedure TCEEditorWidget.mnuedJum2DeclClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  getSymbolLoc;
end;

procedure TCEEditorWidget.mnuEditorPopup(Sender: TObject);
begin
  if fDoc = nil then exit;
  //
  mnuedCut.Enabled:=fDOc.SelAvail;
  mnuedPaste.Enabled:=fDoc.CanPaste;
  mnuedCopy.Enabled:=fDoc.SelAvail;
  mnuedUndo.Enabled:=fDoc.CanUndo;
  mnuedRedo.Enabled:=fDoc.CanRedo;
  mnuedJum2Decl.Enabled:=fDoc.isDSource;
end;
{$ENDREGION}
end.
