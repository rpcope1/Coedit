unit ce_editor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, lcltype,
  Graphics, SynEditKeyCmds, ComCtrls, SynEditHighlighter, ExtCtrls, Menus,
  SynMacroRecorder, SynPluginSyncroEdit, SynEdit, SynCompletion, ce_widget,
  ce_interfaces, ce_synmemo, ce_dlang, ce_common, ce_dcd, ce_observer;

type

  { TCEEditorWidget }
  TCEEditorWidget = class(TCEWidget, ICEMultiDocObserver)
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
    // http://bugs.freepascal.org/view.php?id=26329
    fSyncEdit: TSynPluginSyncroEdit;
    tokLst: TLexTokenList;
    errLst: TLexErrorList;
    fModStart: boolean;
    procedure lexFindToken(const aToken: PLexToken; out doStop: boolean);
    procedure memoKeyPress(Sender: TObject; var Key: char);
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoChange(Sender: TObject);
    procedure memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function getEditor(index: NativeInt): TCESynMemo;
    function getEditorCount: NativeInt;
    function getEditorIndex: NativeInt;
    procedure getCompletionList;
    procedure getSymbolLoc;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure addEditor;
    procedure removeEditor(const aIndex: NativeInt);
    procedure focusedEditorChanged;
    function getEditorHint: string;
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    //
    property editor[index: NativeInt]: TCESynMemo read getEditor;
    property editorCount: NativeInt read getEditorCount;
    property editorIndex: NativeInt read getEditorIndex;
  end;

implementation
{$R *.lfm}

uses
  ce_main;

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
  fSyncEdit := TSynPluginSyncroEdit.Create(self);
  bmp := TBitmap.Create;
  try
    imgList.GetBitmap(0,bmp);
    fSyncEdit.GutterGlyph.Assign(bmp);
  finally
    bmp.Free;
  end;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.endUpdate;
end;

destructor TCEEditorWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  tokLst.Free;
  errLst.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEEditorWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEEditorWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
end;

procedure TCEEditorWidget.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  focusedEditorChanged;
end;

procedure TCEEditorWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fKeyChanged := true;
  beginUpdateByDelay;
  UpdateByEvent;
end;
{$ENDREGION}

{$REGION PageControl/Editor things ---------------------------------------------}
function TCEEditorWidget.getEditorCount: NativeInt;
begin
  result := pageControl.PageCount;
end;

function TCEEditorWidget.getEditorIndex: NativeInt;
begin
  if pageControl.PageCount > 0 then
    result := pageControl.PageIndex
  else
    result := -1;
end;

function TCEEditorWidget.getEditor(index: NativeInt): TCESynMemo;
begin
  result := TCESynMemo(pageControl.Pages[index].Controls[0]);
end;

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
end;

procedure TCEEditorWidget.completionExecute(Sender: TObject);
begin
  getCompletionList;
end;

procedure TCEEditorWidget.completionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  // warning: '20' depends on ce_dcd, case knd of, string literals length
  Value := Value[1..length(Value)-20];
end;

procedure TCEEditorWidget.addEditor;
var
  sheet: TTabSheet;
  memo: TCESynMemo;
begin
  sheet := pageControl.AddTabSheet;
  memo  := TCESynMemo.Create(sheet);
  //
  memo.Align := alClient;
  memo.Parent := sheet;
  //
  memo.OnKeyDown := @memoKeyDown;
  memo.OnKeyUp := @memoKeyDown;
  memo.OnKeyPress := @memoKeyPress;
  memo.OnMouseDown := @memoMouseDown;
  memo.OnMouseMove := @memoMouseMove;
  //
  pageControl.ActivePage := sheet;
end;

procedure TCEEditorWidget.removeEditor(const aIndex: NativeInt);
begin
  editor[aIndex].OnChange:= nil;
  pageControl.Pages[aIndex].Free;
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
  //
  beginUpdateByDelay;
  UpdateByEvent;
end;

procedure TCEEditorWidget.memoChange(Sender: TObject);
begin
end;

procedure TCEEditorWidget.getSymbolLoc;
var
  str: TMemoryStream;
  srcpos: Integer;
  ftempname, fname: string;
begin
  if not dcdOn then exit;
  if fDoc = nil then exit;
  //
  str := TMemoryStream.Create;
  try
    ftempname := fDoc.tempFilename;
    fDoc.Lines.SaveToStream(str);
    str.SaveToFile(ftempname);
    fname := ftempname;
    srcpos := fDoc.SelStart;
    if srcpos > 0 then srcpos += -1;
    if fDoc.GetWordAtRowCol(fDoc.LogicalCaretXY) <> '' then
      ce_dcd.getSymbolLoc(fname, srcpos);
    if fname <> ftempname then if fileExists(fname) then
      CEMainForm.openFile(fname);
    if srcpos <> -1 then
      fDoc.SelStart := srcpos; // fDoc probably not be updated
  finally
    str.Free;
  end;
end;

procedure TCEEditorWidget.getCompletionList;
var
  str: TMemoryStream;
  srcpos: NativeInt;
  fname: string;
begin
  if not dcdOn then exit;
  if fDoc = nil then exit;
  //
  str := TMemoryStream.Create;
  try
    completion.Position := 0; // previous index could cause an error here.
    fname := fDoc.tempFilename;
    fDoc.Lines.SaveToStream(str);
    str.SaveToFile(fname);
    srcpos := fDoc.SelStart;
    if srcpos > 0 then srcpos += -1;
    completion.ItemList.Clear;
    ce_dcd.getCompletion(fname, srcpos, completion.ItemList);
  finally
    str.Free;
  end;
end;

function TCEEditorWidget.getEditorHint: string;
var
  str: TMemoryStream;
  lst: TStringList;
  srcpos: NativeInt;
  fname: string;
begin
  result := '';
  if not dcdOn then exit;
  if fDoc = nil then exit;
  //
  str := TMemoryStream.Create;
  lst := TStringList.Create;
  try
    fname := fDoc.tempFilename;
    fDoc.Lines.SaveToStream(str);
    str.SaveToFile(fname);
    srcpos := fDoc.SelStart;
    if srcpos > 0 then srcpos += -1;
    if fDoc.GetWordAtRowCol(fDoc.LogicalCaretXY) <> '' then
      ce_dcd.getHint(fname, srcpos, lst);
    result := lst.Text;
  finally
    str.Free;
    lst.Free;
  end;
end;

procedure TCEEditorWidget.UpdateByEvent;
const
  modstr: array[boolean] of string = ('...', 'MODIFIED');
begin
  if fDoc = nil then exit;
  //
  editorStatus.Panels[0].Text := format('%d : %d | %d', [fDoc.CaretY, fDoc.CaretX,
    fDoc.SelEnd - fDoc.SelStart]);
  editorStatus.Panels[1].Text := modstr[fDoc.modified];
  editorStatus.Panels[2].Text := fDoc.fileName;
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
  //dt: PMessageItemData;
  //err: TLexError;
  md: string;
begin
  if fDoc = nil then exit;
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
