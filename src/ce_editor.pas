unit ce_editor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, lcltype, Graphics, SynEditKeyCmds,
  ComCtrls, SynEditHighlighter, ExtCtrls, Menus, SynMacroRecorder,
  SynPluginSyncroEdit, SynEdit, SynHighlighterMulti, ce_dialogs,
  ce_widget, ce_interfaces, ce_synmemo, ce_dlang, ce_common, ce_dcd, ce_observer,
  ce_sharedres, ce_controls;

type

  { TCEEditorWidget }

  TCEEditorWidget = class(TCEWidget, ICEMultiDocObserver, ICEMultiDocHandler, ICEProjectObserver)
    mnuedCallTip: TMenuItem;
    mnuedDdoc: TMenuItem;
    mnuedCopy: TMenuItem;
    mnuedCut: TMenuItem;
    mnuedPaste: TMenuItem;
    MenuItem4: TMenuItem;
    mnuedUndo: TMenuItem;
    mnuedRedo: TMenuItem;
    MenuItem7: TMenuItem;
    mnuedJum2Decl: TMenuItem;
    macRecorder: TSynMacroRecorder;
    editorStatus: TStatusBar;
    mnuEditor: TPopupMenu;
    procedure mnuedCallTipClick(Sender: TObject);
    procedure mnuedCopyClick(Sender: TObject);
    procedure mnuedCutClick(Sender: TObject);
    procedure mnuedDdocClick(Sender: TObject);
    procedure mnuEditorPopup(Sender: TObject);
    procedure mnuedPasteClick(Sender: TObject);
    procedure mnuedUndoClick(Sender: TObject);
    procedure mnuedRedoClick(Sender: TObject);
    procedure mnuedJum2DeclClick(Sender: TObject);
    procedure PageControlChanged(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  protected
    procedure updateDelayed; override;
    procedure updateImperative; override;
  private
    pageControl: TCEPageControl;
    fKeyChanged: boolean;
    fDoc: TCESynMemo;
    fProj: ICECommonProject;
    fTokList: TLexTokenList;
    fErrList: TLexErrorList;
    fModStart: boolean;
    fLastCommand: TSynEditorCommand;
    procedure pageBtnAddCLick(Sender: TObject);
    procedure pageCloseBtnClick(Sender: TObject);
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
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
    function SingleServiceName: string;
    function documentCount: Integer;
    function getDocument(index: Integer): TCESynMemo;
    function findDocument(aFilename: string): TCESynMemo;
    procedure openDocument(aFilename: string);
    function closeDocument(index: Integer): boolean;
    function closeDocument(doc: TCESynMemo): boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function closeQuery: boolean; override;
  end;

implementation
{$R *.lfm}

uses
  ce_lcldragdrop;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEEditorWidget.create(aOwner: TComponent);
begin
  inherited;
  //
  pageControl := TCEPageControl.Create(self);
  pageControl.Parent := Content;
  pageControl.align := alClient;
  pageControl.onChanged:= @PageControlChanged;
  pageControl.onChanging:=@PageControlChanging;
  pageControl.closeButton.OnClick:=@pageCloseBtnClick;
  pageControl.addButton.OnClick:=@pageBtnAddCLick;
  pageControl.OnDragDrop:= @ddHandler.DragDrop;
  pageControl.OnDragOver:= @ddHandler.DragOver;
  AssignPng(pageControl.moveLeftButton, 'go_previous');
  AssignPng(pageControl.moveRightButton, 'go_next');
  AssignPng(pageControl.addButton, 'document_add');
  AssignPng(pageControl.closeButton, 'document_delete');
  AssignPng(pageControl.splitButton, 'splitter');

  fTokList := TLexTokenList.Create;
  fErrList := TLexErrorList.Create;
  //
  AssignPng(mnuedCopy.Bitmap, 'copy');
  AssignPng(mnuedCut.Bitmap, 'cut');
  AssignPng(mnuedPaste.Bitmap, 'paste');
  AssignPng(mnuedUndo.Bitmap, 'arrow_undo');
  AssignPng(mnuedRedo.Bitmap, 'arrow_redo');
  AssignPng(mnuedJum2Decl.Bitmap, 'arrow_shoe');
  AssignPng(mnuedCopy.Bitmap, 'copy');
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
    if PageControl.Pages[i].ControlCount > 0 then
      if (PageControl.Pages[i].Controls[0] is TCESynMemo) then
        PageControl.Pages[i].Controls[0].Free;
  fTokList.Free;
  fErrList.Free;
  inherited;
end;

function TCEEditorWidget.closeQuery: boolean;
begin
  result := inherited and Parent.isNil;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEEditorWidget.docNew(aDoc: TCESynMemo);
var
  pge: TCEPage;
begin
  pge := pageControl.addPage;
  //
  aDoc.Align := alClient;
  aDoc.Parent := pge;
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
  focusedEditorChanged;
  beginDelayedUpdate;
  updateImperative;
end;

procedure TCEEditorWidget.docClosing(aDoc: TCESynMemo);
begin
  if aDoc.isNil then
    exit;
  aDoc.Parent := nil;
  if aDoc = fDoc then
    fDoc := nil;
  updateImperative;
  pageControl.deletePage(pageControl.pageIndex);
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

{$REGION ICECommonProject ------------------------------------------------------}
procedure TCEEditorWidget.projNew(aProject: ICECommonProject);
begin
end;

procedure TCEEditorWidget.projChanged(aProject: ICECommonProject);
begin
end;

procedure TCEEditorWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj = aProject then
    fProj := nil;
end;

procedure TCEEditorWidget.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEEditorWidget.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCEEditorWidget.projCompiled(aProject: ICECommonProject; success: boolean);
begin
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
  if doc.isNotNil then begin
    PageControl.currentPage := TCEPage(doc.Parent);
    exit;
  end;
  doc := TCESynMemo.Create(nil);
  fDoc.loadFromFile(aFilename);
  if assigned(fProj) and (fProj.filename = fDoc.fileName) then
  begin
    if fProj.getFormat = pfNative then
      fDoc.Highlighter := LfmSyn
    else
      fDoc.Highlighter := JsSyn;
  end;
end;

function TCEEditorWidget.closeDocument(index: Integer): boolean;
var
  doc: TCESynMemo;
begin
  doc := getDocument(index);
  if doc.isNil then exit(false);
  if (doc.modified or (doc.fileName = doc.tempFilename)) and
    (dlgFileChangeClose(doc.fileName) = mrCancel) then exit(false);
  pageControl.pageIndex:=index;
  doc.Free;
  result := true;
end;

function TCEEditorWidget.closeDocument(doc: TCESynMemo): boolean;
var
  page: TCEPage = nil;
begin
  page := TCEPage(doc.Parent);
  if page.isNil then
    exit(false);
  exit(closeDocument(page.index));
end;
{$ENDREGION}

{$REGION PageControl/Editor things ---------------------------------------------}
procedure TCEEditorWidget.pageCloseBtnClick(Sender: TObject);
begin
  closeDocument(PageControl.PageIndex);
end;

procedure TCEEditorWidget.pageBtnAddCLick(Sender: TObject);
begin
  TCESynMemo.Create(nil);
  pageControl.currentPage.Caption:='<new document>';
end;

procedure TCEEditorWidget.focusedEditorChanged;
begin
  if fDoc.isNil then exit;
  //
  macRecorder.Editor:= fDoc;
  fDoc.PopupMenu := mnuEditor;
  fDoc.hideCallTips;
  fDoc.hideDDocs;
  if (pageControl.currentPage.Caption = '') then
  begin
    fKeyChanged := true;
    beginDelayedUpdate;
  end;
end;

procedure TCEEditorWidget.PageControlChanged(Sender: TObject);
begin
  if fDoc.isNil then exit;
  fDoc.hideCallTips;
  fDoc.hideDDocs;
end;

procedure TCEEditorWidget.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if fDoc.isNil then exit;
  fDoc.hideCallTips;
  fDoc.hideDDocs;
end;

procedure TCEEditorWidget.memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_CLEAR,VK_RETURN,VK_BACK : fKeyChanged := true;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
    begin
      if Shift <> [ssCtrl, ssAlt, ssShift] then
        updateImperative
      else begin
        if Key = VK_LEFT then pageControl.pageIndex := pageControl.pageIndex -1
        else if Key = VK_RIGHT then pageControl.pageIndex := pageControl.pageIndex +1;
      end;
    end;
  end;
  if fKeyChanged then
    beginDelayedUpdate;
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
  //
  case Command of
    ecJumpToDeclaration:
      getSymbolLoc;
    ecRecordMacro:
    begin
      if macRecorder.State = msStopped then
        macRecorder.RecordMacro(fDoc)
      else
        macRecorder.Stop;
      updateImperative;
    end;
    ecPlayMacro:
    begin
      macRecorder.Stop;
      macRecorder.PlaybackMacro(fDoc);
      updateImperative;
    end;
  end;
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
  page: TCEPage;
  srcpos, i, sum, linelen: Integer;
  fname: string;
  len: byte;
begin
  if not DcdWrapper.available then exit;
  //
  DcdWrapper.getDeclFromCursor(fname, srcpos);
  if (fname <> fDoc.fileName) and fname.fileExists then
  begin
    page := pageControl.splitPage;
    if assigned(page) then
    begin
      fDoc := TCESynMemo(page.Controls[0]);
      if fDoc.fileName <> fname then
        openDocument(fname);
    end
    else openDocument(fname);
  end;
  if srcpos <> -1 then
  begin
    sum := 0;
    len := getLineEndingLength(fDoc.fileName);
    for i := 0 to fDoc.Lines.Count-1 do
    begin
      linelen := fDoc.Lines.Strings[i].length;
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
var
  md: string = '';
begin
  if fDoc = nil then begin
    editorStatus.Panels[0].Text := '';
    editorStatus.Panels[1].Text := '';
    editorStatus.Panels[2].Text := '';
    editorStatus.Panels[3].Text := '';
    editorStatus.Panels[4].Text := '';
  end else begin
    editorStatus.Panels[0].Text := format('%d : %d | %d', [fDoc.CaretY, fDoc.CaretX, fDoc.SelEnd - fDoc.SelStart]);
    editorStatus.Panels[1].Text := modstr[fDoc.modified];
    if macRecorder.State = msRecording then
      editorStatus.Panels[2].Text := 'recording macro'
    else if macRecorder.IsEmpty then
      editorStatus.Panels[2].Text := 'no macro'
    else
      editorStatus.Panels[2].Text := 'macro ready';
    if fDoc.ReadOnly then
    begin
      editorStatus.Panels[3].Width:= 120;
      editorStatus.Panels[3].Text := '(read-only)';
    end else
      editorStatus.Panels[3].Width:= 0;
    editorStatus.Panels[4].Text := fDoc.fileName;
    if Visible and (pageControl.currentPage <> nil) and ((pageControl.currentPage.Caption = '') or
      (pageControl.currentPage.Caption = '<new document>')) then
    begin
      if fDoc.isDSource then
      begin
        lex(fDoc.Lines.Text, fTokList, @lexFindToken);
        md := getModuleName(fTokList);
        fTokList.Clear;
        fErrList.Clear;
      end;
      if md.isEmpty then md := fDoc.fileName.extractFileName;
      pageControl.currentPage.Caption := md;
    end;
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
  md := '';
  if fDoc.isDSource then
  begin
    lex(fDoc.Lines.Text, fTokList, @lexFindToken);
    md := getModuleName(fTokList);
    fTokList.Clear;
    fErrList.Clear;
  end;
  if md.isEmpty then
    md := fDoc.fileName.extractFileName;
  pageControl.currentPage.Caption := md;
end;
{$ENDREGION}

{$REGION Editor context menu ---------------------------------------------------}
procedure TCEEditorWidget.mnuedCopyClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecCopy, '', nil);
end;

procedure TCEEditorWidget.mnuedCallTipClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  mnuEditor.Close;
  fDoc.hideDDocs;
  fDoc.showCallTips;
end;

procedure TCEEditorWidget.mnuedCutClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  fDoc.ExecuteCommand(ecCut, '', nil);
end;

procedure TCEEditorWidget.mnuedDdocClick(Sender: TObject);
begin
  if fDoc = nil then exit;
  mnuEditor.Close;
  fDoc.hideCallTips;
  fDoc.showDDocs;
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
