unit ce_editor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, lcltype,
  Graphics, SynEditKeyCmds, ComCtrls, SynEditHighlighter, ExtCtrls, Menus,
  SynMacroRecorder, SynPluginSyncroEdit, SynEdit, SynCompletion, ce_widget,
  ce_interfaces, ce_synmemo, ce_dlang, ce_common, ce_dcd, ce_observer;

type

  // this descendant propagates the Visible property to the children.
  // this fix the bug described in commit c1a0ed2799390d788b1d1e435eb8dc1ed3369ce7
  TCEEditorPage = class(TTabSheet)
  protected
    procedure SetVisible(Value: Boolean); override;
  end;

  TCEEditorWidget = class(TCEWidget, ICEMultiDocObserver, ICEMultiDocHandler)
    imgList: TImageList;
    PageControl: TExtendedNotebook;
    macRecorder: TSynMacroRecorder;
    editorStatus: TStatusBar;
    completion: TSynCompletion;
    procedure completionCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure completionExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  protected
    procedure UpdateByDelay; override;
    procedure UpdateByEvent; override;
  private
    fKeyChanged: boolean;
    fDoc: TCESynMemo;
    // TODO-cbugfix: syncro-edit partially broken, undetermined condition
    // TODO-cbugfix: syncro-edit icon hidden after deletion, if doc is saved (as temp file, by the static explorer)
    fSyncEdit: TSynPluginSyncroEdit;
    tokLst: TLexTokenList;
    errLst: TLexErrorList;
    fModStart: boolean;
    {$IFDEF LINUX}
    procedure pageCloseBtnClick(Sender: TObject);
    {$ENDIF}
    function completionItemPaint(const AKey: string; ACanvas: TCanvas;X, Y: integer; Selected: boolean; Index: integer): boolean;
    procedure lexFindToken(const aToken: PLexToken; out doStop: boolean);
    procedure memoKeyPress(Sender: TObject; var Key: char);
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoCtrlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure getCompletionList;
    procedure getSymbolLoc;
    procedure focusedEditorChanged;
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
  bmp: TBitmap;
begin
  inherited;
  //
  tokLst := TLexTokenList.Create;
  errLst := TLexErrorList.Create;
  //
  completion.OnPaintItem := @completionItemPaint;
  fSyncEdit := TSynPluginSyncroEdit.Create(self);
  //TODO-cLCL&LAZ-specific: activate this after next Laz release
  //fSyncEdit.CaseSensitive:=true;
  bmp := TBitmap.Create;
  try
    imgList.GetBitmap(0, bmp);
    fSyncEdit.GutterGlyph.Assign(bmp);
  finally
    bmp.Free;
  end;
  //
  {$IFDEF LINUX}
  PageControl.OnCloseTabClicked := @pageCloseBtnClick;
  {$ENDIF}
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
  tokLst.Free;
  errLst.Free;
  inherited;
end;

function TCEEditorWidget.completionItemPaint(const AKey: string; ACanvas: TCanvas;X, Y: integer; Selected: boolean; Index: integer): boolean;
var
  lft, rgt: string;
  len: Integer;
begin
  // warning: '20' depends on ce_dcd, case knd of, string literals length
  result := true;
  lft := AKey[1 .. length(AKey)-20];
  rgt := AKey[length(AKey)-19 .. length(AKey)];
  ACanvas.Font.Style := [fsBold];
  len := ACanvas.TextExtent(lft).cx;
  ACanvas.TextOut(2 + X , Y, lft);
  ACanvas.Font.Style := [fsItalic];
  ACanvas.TextOut(2 + X + len + 2, Y, rgt);
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
  aDoc.OnKeyUp := @memoKeyDown;
  aDoc.OnKeyPress := @memoKeyPress;
  aDoc.OnMouseDown := @memoMouseDown;
  aDoc.OnMouseMove := @memoMouseMove;
  aDoc.OnClickLink := @memoCtrlClick;
  //
  fDoc := aDoc;
  pageControl.ActivePage := sheet;
  focusedEditorChanged;
  beginUpdateByDelay;
  UpdateByEvent;
end;

procedure TCEEditorWidget.docClosing(aDoc: TCESynMemo);
var
  sheet: TWinControl;
begin
  if fDoc <> aDoc then exit;
  sheet := fDoc.Parent;
  fDoc.Parent := nil;
  fDoc := nil;
  if sheet <> nil then sheet.Free;
  UpdateByEvent;
end;

procedure TCEEditorWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc = fDoc then exit;
  fDoc := aDoc;
  focusedEditorChanged;
  beginUpdateByDelay;
  UpdateByEvent;
end;

procedure TCEEditorWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fKeyChanged := true;
  beginUpdateByDelay;
  UpdateByEvent;
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
    'The latest "%s" mofifications are not saved, continue ?',
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
  if fDoc = nil then exit;
  //
  macRecorder.Editor := fDoc;
  fSyncEdit.Editor := fDoc;
  completion.Editor := fDoc;
  if (pageControl.ActivePage.Caption = '') then
  begin
    fKeyChanged := true;
    beginUpdateByDelay;
  end;
end;

procedure TCEEditorWidget.PageControlChange(Sender: TObject);
begin
  UpdateByEvent;
end;

procedure TCEEditorWidget.completionExecute(Sender: TObject);
begin
  getCompletionList;
  completion.TheForm.Font.Size := fDoc.Font.Size;
end;

procedure TCEEditorWidget.completionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  // warning: '20' depends on ce_dcd, case knd of, string literals length
  Value := Value[1..length(Value)-20];
end;

procedure TCEEditorWidget.memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdateByEvent;
  case Byte(Key) of
    VK_CLEAR,VK_RETURN,VK_BACK : fKeyChanged := true;
  end;
  if fKeyChanged then
    beginUpdateByDelay;
  //
  if (Key = VK_UP) and (shift = [ssShift,ssCtrl]) then
    getSymbolLoc;
end;

procedure TCEEditorWidget.memoKeyPress(Sender: TObject; var Key: char);
begin
  fKeyChanged := true;
  beginUpdateByDelay;
end;

procedure TCEEditorWidget.memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  beginUpdateByDelay;
  UpdateByEvent;
end;

procedure TCEEditorWidget.memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then exit;
  beginUpdateByDelay;
end;

procedure TCEEditorWidget.memoCtrlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  getSymbolLoc;
end;

procedure TCEEditorWidget.getSymbolLoc;
var
  srcpos, i, sum: Integer;
  fname: string;
  lel: byte;
begin
  if not DcdWrapper.available then exit;
  //
  DcdWrapper.getDeclFromCursor(fname, srcpos);
  if fname <> fDoc.fileName then if fileExists(fname) then
    openDocument(fname);
  if srcpos <> -1 then
  begin
    //fDoc.SelStart := srcpos;
    //fDoc.SelectWord;

    // note: SelStart only matches the srcpos if the target file has the same line ending
    // as the operating system: the pos has to be found manually.
    sum := 0;
    lel := getLineEndingLength(fDoc.fileName);
    for i := 0 to fDoc.Lines.Count-1 do
    begin
      sum += length(fDoc.Lines.Strings[i]);
      sum += lel;
      //TODO-cenhancement: find declaration, determine column accurately.
      if sum >= srcpos then
      begin
        fDoc.CaretY := i+1;
        break;
      end;
    end;
  end;
end;

procedure TCEEditorWidget.getCompletionList;
begin
  if not DcdWrapper.available then exit;
  //
  completion.Position := 0;
  completion.ItemList.Clear;
  DcdWrapper.getComplAtCursor(completion.ItemList);
end;

procedure TCEEditorWidget.UpdateByEvent;
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

procedure TCEEditorWidget.UpdateByDelay;
var
  md: string;
begin
  if fDoc = nil then exit;
  UpdateByEvent;
  if not fKeyChanged then exit;
  //
  fKeyChanged := false;
  if fDoc.Lines.Count = 0 then exit;
  //
  lex(fDoc.Lines.Text, tokLst, @lexFindToken);
  md := '';
  if fDoc.isDSource then
    md := getModuleName(tokLst);
  if md = '' then md := extractFileName(fDoc.fileName);
  pageControl.ActivePage.Caption := md;
  //
  tokLst.Clear;
  errLst.Clear;
end;
{$ENDREGION}

end.
