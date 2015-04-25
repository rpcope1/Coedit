unit ce_synmemo;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, controls,lcltype, Forms, graphics, ExtCtrls, crc, SynPluginSyncroEdit,
  SynEditKeyCmds, LazSynEditText, SynEditHighlighter, SynEdit, SynHighlighterLFM,
  SynEditMouseCmds, SynEditFoldedView, ce_common, ce_observer, ce_writableComponent,
  ce_d2syn, ce_txtsyn;

type

  TCESynMemo = class;

  // Simple THintWindow descendant allowing the font size to be in sync with the editor.
  TCEEditorHintWindow = class(THintWindow)
  public
    class var FontSize: Integer;
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
      AData: Pointer): TRect; override;
  end;

  // Stores the state of a particular source code folding.
  TCEFoldCache = class(TCollectionItem)
  private
    fCollapsed: boolean;
    fLineIndex: Integer;
    fNestedIndex: Integer;
  published
    property isCollapsed: boolean read fCollapsed write fCollapsed;
    property lineIndex: Integer read fLineIndex write fLineIndex;
    property nestedIndex: Integer read fNestedIndex write fNestedIndex;
  end;

  // Stores the state of a document between two cessions.
  TCESynMemoCache = class(TWritableLfmTextComponent)
  private
    fMemo: TCESynMemo;
    fFolds: TCollection;
    fCaretPosition: Integer;
    fSelectionEnd: Integer;
    fFontSize: Integer;
    fSourceFilename: string;
    procedure setFolds(someFolds: TCollection);
  published
    property caretPosition: Integer read fCaretPosition write fCaretPosition;
    property sourceFilename: string read fSourceFilename write fSourceFilename;
    property folds: TCollection read fFolds write setFolds;
    property selectionEnd: Integer read fSelectionEnd write fSelectionEnd;
    property fontSize: Integer read fFontSize write fFontSize;
  public
    constructor create(aComponent: TComponent); override;
    destructor destroy; override;
    //
    procedure beforeSave; override;
    procedure afterLoad; override;
    procedure save;
    procedure load;
  end;

  // buffer of caret positions allowing to jump quickly to the most recent locations.
  TCESynMemoPositions = class
  private
    fPos: Integer;
    fMax: Integer;
    fList: TFPList;
    fMemo: TCustomSynEdit;
  public
    constructor create(aMemo: TCustomSynEdit);
    destructor destroy; override;
    procedure store;
    procedure back;
    procedure next;
  end;

  TCESynMemo = class(TSynEdit)
  private
    fFilename: string;
    fModified: boolean;
    fFileDate: double;
    fIsDSource: boolean;
    fIsTxtFile: boolean;
    fIsConfig: boolean;
    fIdentifier: string;
    fTempFileName: string;
    fMultiDocSubject: TCECustomSubject;
    fDefaultFontSize: Integer;
    fPositions: TCESynMemoPositions;
    fMousePos: TPoint;
    fCallTipWin: TCEEditorHintWindow;
    fDDocWin: TCEEditorHintWindow;
    fHintTimer: TIdleTimer;
    fCanShowHint: boolean;
    fOldMousePos: TPoint;
    fSyncEdit: TSynPluginSyncroEdit;
    function getMouseFileBytePos: Integer;
    procedure changeNotify(Sender: TObject);
    procedure identifierToD2Syn;
    procedure saveCache;
    procedure loadCache;
    procedure setDefaultFontSize(aValue: Integer);
    procedure getCallTips;
    procedure HintTimerEvent(sender: TObject);
    procedure InitHintWins;
  protected
    procedure MouseLeave; override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetHighlighter(const Value: TSynCustomHighlighter); override;
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  published
    property defaultFontSize: Integer read fDefaultFontSize write setDefaultFontSize;
  public
    constructor Create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure setFocus; override;
    //
    procedure checkFileDate;
    procedure loadFromFile(const aFilename: string);
    procedure saveToFile(const aFilename: string);
    procedure save;
    procedure saveTempFile;
    //
    property Identifier: string read fIdentifier;
    property fileName: string read fFilename;
    property modified: boolean read fModified;
    property tempFilename: string read fTempFileName;
    //
    property syncroEdit: TSynPluginSyncroEdit read fSyncEdit;
    property isDSource: boolean read fIsDSource;
    property isProjectSource: boolean read fIsConfig;
    property TextView;
    //
    property MouseStart: Integer read getMouseFileBytePos;
  end;

var
  D2Syn: TSynD2Syn;
  LfmSyn: TSynLfmSyn;
  TxtSyn: TSynTxtSyn;

implementation

uses
  ce_interfaces, ce_staticmacro, ce_dcd, SynEditHighlighterFoldBase;

function TCEEditorHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect;
begin
  Font.Size:= FontSize;
  result := inherited CalcHintRect(MaxWidth, AHint, AData);
end;

{$REGION TCESynMemoCache -------------------------------------------------------}
constructor TCESynMemoCache.create(aComponent: TComponent);
begin
  inherited create(nil);
  if (aComponent is TCESynMemo) then
  	fMemo := TCESynMemo(aComponent);
  fFolds := TCollection.Create(TCEFoldCache);
end;

destructor TCESynMemoCache.destroy;
begin
  fFolds.Free;
  inherited;
end;

procedure TCESynMemoCache.setFolds(someFolds: TCollection);
begin
  fFolds.Assign(someFolds);
end;

procedure TCESynMemoCache.beforeSave;
var
  i, start, prev: Integer;
  itm : TCEFoldCache;
begin
  if fMemo = nil then exit;
  //
  fCaretPosition := fMemo.SelStart;
  fSourceFilename := fMemo.fileName;
  fSelectionEnd := fMemo.SelEnd;
  fFontSize := fMemo.Font.Size;
  TCEEditorHintWindow.FontSize := fMemo.Font.Size;
  //
  // TODO-cEditor Cache: >nested< folding persistence
  // cf. other ways: http://forum.lazarus.freepascal.org/index.php?topic=26748.msg164722#msg164722
  prev := fMemo.Lines.Count-1;
  for i := fMemo.Lines.Count-1 downto 0 do
  begin
    // - CollapsedLineForFoldAtLine() does not handle the sub-folding.
    // - TextView visibility is increased so this is not the standard way of getting the infos.
    start := fMemo.TextView.CollapsedLineForFoldAtLine(i);
    if start = -1 then
      continue;
    if start = prev then
      continue;
    prev := start;
    itm := TCEFoldCache(fFolds.Add);
    itm.isCollapsed := true;
    itm.fLineIndex := start;
  end;
end;

procedure TCESynMemoCache.afterLoad;
var
  i: integer;
  itm : TCEFoldCache;
begin
  if fMemo = nil then exit;
  //
  if fFontSize > 0 then
    fMemo.Font.Size := fFontSize;
  // Currently collisions are not handled.
  if fMemo.fileName <> fSourceFilename then exit;
  //
  for i := 0 to fFolds.Count-1 do
  begin
    itm := TCEFoldCache(fFolds.Items[i]);
    if not itm.isCollapsed then
      continue;
    fMemo.TextView.FoldAtLine(itm.lineIndex-1);
  end;
  //
  fMemo.SelStart := fCaretPosition;
  fMemo.SelEnd := fSelectionEnd;
end;

{$IFDEF DEBUG}{$R-}{$ENDIF}
procedure TCESynMemoCache.save;
var
  fname: string;
  tempn: string;
  chksm: Cardinal;
begin
  tempn := fMemo.fileName;
  if tempn = fMemo.tempFilename then exit;
  if not fileExists(tempn) then exit;
  //
  fname := getCoeditDocPath + 'editorcache' + DirectorySeparator;
  ForceDirectories(fname);
  chksm := crc32(0, nil, 0);
  chksm := crc32(chksm, @tempn[1], length(tempn));
  fname := fname + format('%.8X.txt', [chksm]);
  saveToFile(fname);
end;

procedure TCESynMemoCache.load;
var
  fname: string;
  tempn: string;
  chksm: Cardinal;
begin
  tempn := fMemo.fileName;
  if not fileExists(tempn) then exit;
  //
  fname := getCoeditDocPath + 'editorcache' + DirectorySeparator;
  chksm := crc32(0, nil, 0);
  chksm := crc32(chksm, @tempn[1], length(tempn));
  fname := fname + format('%.8X.txt', [chksm]);
  //
  if not fileExists(fname) then exit;
  loadFromFile(fname);
end;
{$IFDEF DEBUG}{$R+}{$ENDIF}
{$ENDREGION}

{$REGION TCESynMemoPositions ---------------------------------------------------}
constructor TCESynMemoPositions.create(aMemo: TCustomSynEdit);
begin
  fList := TFPList.Create;
  fMax  := 20;
  fMemo := aMemo;
  fPos  := -1;
end;

destructor TCESynMemoPositions.destroy;
begin
  fList.Free;
  inherited;
end;

procedure TCESynMemoPositions.back;
begin
  Inc(fPos);
  {$HINTS OFF}
  if fPos < fList.Count then
    fMemo.CaretY := NativeInt(fList.Items[fPos])
  {$HINTS ON}
  else Dec(fPos);
end;

procedure TCESynMemoPositions.next;
begin
  Dec(fPos);
  {$HINTS OFF}
  if fPos > -1 then
    fMemo.CaretY := NativeInt(fList.Items[fPos])
  {$HINTS ON}
  else Inc(fPos);
end;

procedure TCESynMemoPositions.store;
var
  delta: NativeInt;
const
  thresh = 6;
begin
  fPos := 0;
  {$HINTS OFF}{$WARNINGS OFF}
  if fList.Count > 0 then
  begin
    delta := fMemo.CaretY - NativeInt(fList.Items[fPos]);
    if (delta > -thresh) and (delta < thresh) then exit;
  end;
  fList.Insert(0, Pointer(NativeInt(fMemo.CaretY)));
  {$HINTS ON}{$WARNINGS ON}
  while fList.Count > fMax do
    fList.Delete(fList.Count-1);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION TCESynMemo ------------------------------------------------------------}
constructor TCESynMemo.Create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  //
  fDefaultFontSize := 10;
  SetDefaultKeystrokes; // not called in inherited if owner = nil !
  //
  ShowHint :=false;
  InitHintWins;
  fHintTimer := TIdleTimer.Create(self);
  fHintTimer.AutoEnabled:=true;
  fHintTimer.Interval := 200;
  fHintTimer.OnTimer := @HintTimerEvent;
  //
  Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf := 5;
  Gutter.LineNumberPart.MarkupInfo.Foreground := clGray;
  Gutter.SeparatorPart.LineOffset := 1;
  Gutter.SeparatorPart.LineWidth := 1;
  Gutter.SeparatorPart.MarkupInfo.Foreground := clGray;
  Gutter.CodeFoldPart.MarkupInfo.Foreground := clGray;
  BracketMatchColor.Foreground:=clRed;
  //
  fSyncEdit := TSynPluginSyncroEdit.Create(self);
  fSyncEdit.Editor := self;
  fSyncEdit.CaseSensitive := true;
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('link_edit');
    fSyncEdit.GutterGlyph.Assign(png);
  finally
    png.Free;
  end;
  //
  MouseLinkColor.Style:= [fsUnderline];
  with MouseActions.Add do begin
    Command := emcMouseLink;
    shift := [ssCtrl];
    ShiftMask := [ssCtrl];
  end;
  //
  Highlighter := D2Syn;
  Highlighter.ResetRange;
  //
  fTempFileName := GetTempDir(false) + 'temp_' + uniqueObjStr(self) + '.d';
  fFilename := '<new document>';
  fModified := false;
  TextBuffer.AddNotifyHandler(senrUndoRedoAdded, @changeNotify);
  //
  fPositions := TCESynMemoPositions.create(self);
  fMultiDocSubject := TCEMultiDocSubject.create;
  //
  subjDocNew(TCEMultiDocSubject(fMultiDocSubject), self);
end;

destructor TCESynMemo.destroy;
begin
  saveCache;
  //
  subjDocClosing(TCEMultiDocSubject(fMultiDocSubject), self);
  fMultiDocSubject.Free;
  fPositions.Free;
  //
  if fileExists(fTempFileName) then
    sysutils.DeleteFile(fTempFileName);
  //
  inherited;
end;

procedure TCESynMemo.setDefaultFontSize(aValue: Integer);
var
  old: Integer;
begin
  old := Font.Size;
  if aValue < 5 then aValue := 5;
  fDefaultFontSize:= aValue;
  if Font.Size = old then
    Font.Size := fDefaultFontSize;
end;

procedure TCESynMemo.setFocus;
begin
  inherited;
  checkFileDate;
  if (Highlighter <> nil) then
  begin
    LineTextChanged(nil, 0, Lines.Count);
    Highlighter.ScanRanges;
  end;
  identifierToD2Syn;
  subjDocFocused(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.SetVisible(Value: Boolean);
begin
  inherited;
  if Value then
    setFocus
  else begin
    fDDocWin.Hide;
    fCallTipWin.Hide;
  end;
end;

procedure TCESynMemo.InitHintWins;
begin
  if fCallTipWin = nil then begin
    fCallTipWin := TCEEditorHintWindow.Create(self);
    fCallTipWin.Color := clInfoBk + $01010100;
    fCallTipWin.Font.Color:= clInfoText;
  end;
  if fDDocWin = nil then begin
    fDDocWin := TCEEditorHintWindow.Create(self);
    fDDocWin.Color := clInfoBk + $01010100;
    fDDocWin.Font.Color:= clInfoText;
  end;
end;

procedure TCESynMemo.getCallTips();
var
  str: string;
  pnt: TPoint;
begin
  DcdWrapper.getCallTip(str);
  if str <> '' then
  begin
    pnt := ClientToScreen(point(CaretXPix, CaretYPix));
    fCallTipWin.FontSize := Font.Size;
	  fCallTipWin.HintRect := fCallTipWin.CalcHintRect(0, str, nil);
    fCallTipWin.OffsetHintRect(pnt, Font.Size * 2);
	  fCallTipWin.ActivateHint(str);
  end;
end;

procedure TCESynMemo.HintTimerEvent(sender: TObject);
var
  str: string;
begin
  if not Visible then exit;
  if not isDSource then exit;
  //
  if not fCanShowHint then exit;
  fCanShowHint := false;
  DcdWrapper.getDdocFromCursor(str);
  //
  if (length(str) > 0) then
    if str[1] = #13 then
      str := str[2..length(str)];
  if (length(str) > 0) then
    if str[1] = #10 then
      str := str[2..length(str)];
  //
  if str <> '' then
  begin
    fDDocWin.FontSize := Font.Size;
    fDDocWin.HintRect := fDDocWin.CalcHintRect(0, str, nil);
    fDDocWin.OffsetHintRect(mouse.CursorPos, Font.Size);
    fDDocWin.ActivateHint(fDDocWin.HintRect, str);
  end;
end;

procedure TCESynMemo.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  inherited;
  fIsDSource := Highlighter = D2Syn;
  fIsConfig  := Highlighter = LfmSyn;
  fIsTxtFile := Highlighter = TxtSyn;
end;

procedure TCESynMemo.identifierToD2Syn;
begin
  fIdentifier := GetWordAtRowCol(LogicalCaretXY);
  if fIsDSource then
    D2Syn.CurrentIdentifier := fIdentifier
  else if fIsTxtFile then
    TxtSyn.CurrIdent := fIdentifier;
end;

procedure TCESynMemo.changeNotify(Sender: TObject);
begin
  identifierToD2Syn;
  fModified := true;
  fPositions.store;
  subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.loadFromFile(const aFilename: string);
var
  ext: string;
begin
  ext := extractFileExt(aFilename);
  if dExtList.IndexOf(ext) = -1 then
    Highlighter := TxtSyn;
  Lines.LoadFromFile(aFilename);
  fFilename := aFilename;
  FileAge(fFilename, fFileDate);
  //
  loadCache;
  //
  fModified := false;
  if Showing then setFocus;
  subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.saveToFile(const aFilename: string);
var
  ext: string;
begin
  Lines.SaveToFile(aFilename);
  fFilename := aFilename;
  ext := extractFileExt(aFilename);
  if dExtList.IndexOf(ext) <> -1 then
    Highlighter := D2Syn;
  FileAge(fFilename, fFileDate);
  fModified := false;
  if fFilename <> fTempFileName then
    subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.save;
begin
  Lines.SaveToFile(fFilename);
  FileAge(fFilename, fFileDate);
  fModified := false;
  if fFilename <> fTempFileName then
    subjDocChanged(TCEMultiDocSubject(fMultiDocSubject), self);
end;

procedure TCESynMemo.saveTempFile;
begin
  saveToFile(fTempFileName);
  fModified := false;
end;

procedure TCESynMemo.saveCache;
var
  cache: TCESynMemoCache;
begin
  cache := TCESynMemoCache.create(self);
  try
    cache.save;
  finally
    cache.free;
  end;
end;

procedure TCESynMemo.loadCache;
var
  cache: TCESynMemoCache;
begin
  cache := TCESynMemoCache.create(self);
  try
    cache.load;
  finally
    cache.free;
  end;
end;

procedure TCESynMemo.checkFileDate;
var
  newDate: double;
  str: TStringList;
begin
  if fFilename = fTempFileName then exit;
  if not FileAge(fFilename, newDate) then exit;
  if fFileDate = newDate then exit;
  if fFileDate <> 0.0 then
  begin
    if dlgOkCancel(format('"%s" has been modified by another program, load the new version ?',
      [shortenPath(fFilename, 25)])) = mrOk then
    begin
      str := TStringList.Create;
      try
        str.LoadFromFile(fFilename);
        DoCopyToClipboard(str.Text);
        ClearAll;
        PasteFromClipboard;
        fModified := true;
      finally
        str.Free;
      end;
    end;
  end;
  fFileDate := newDate;
end;

procedure TCESynMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  identifierToD2Syn;
  if not (Shift = [ssCtrl]) then exit;
  //
  case Key of
    VK_ADD: if Font.Size < 50 then Font.Size := Font.Size + 1;
    VK_SUBTRACT: if Font.Size > 3 then Font.Size := Font.Size - 1;
    VK_DECIMAL: Font.Size := fDefaultFontSize;
  end;
  TCEEditorHintWindow.FontSize := Font.Size;
  UpdateCursor;
  fCanShowHint:=false;
  fDDocWin.Hide;
end;

procedure TCESynMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_PRIOR, VK_NEXT, Vk_UP] then
    fPositions.store;
  inherited;
  //
  if StaticEditorMacro.automatic then
    StaticEditorMacro.Execute;
end;

procedure TCESynMemo.UTF8KeyPress(var Key: TUTF8Char);
var
  c: TUTF8Char;
begin
  c := Key;
  inherited;
  case c of
    '(': getCallTips;
    ')': fCallTipWin.Hide;
  end;
end;

function TCESynMemo.getMouseFileBytePos: Integer;
var
  i, len, llen: Integer;
begin
  result := 0;
  if fMousePos.y-1 > Lines.Count-1 then exit;
  llen := length(Lines.Strings[fMousePos.y-1]);
  if fMousePos.X > llen  then exit;
  //
  // something note really clear:
  // TCEEditorWidget.getSymbolLoc works when using the line ending of the file.
  // TCESynMemo.getMouseFileBytePos works when using the line ending from the system.
  len := getSysLineEndLen;
  for i:= 0 to fMousePos.y-2 do
    result += length(Lines.Strings[i]) + len;
  result += fMousePos.x;
end;

procedure TCESynMemo.MouseLeave;
begin
  fDDocWin.Hide;
  fCallTipWin.Hide;
end;

procedure TCESynMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dX, dY: Integer;
begin
  fDDocWin.Hide;
  fCallTipWin.Hide;
  inherited;
  dx := X - fOldMousePos.x;
  dy := Y - fOldMousePos.y;
  fCanShowHint:=false;
  if (shift = []) then if
    ((dx < 0) and (dx > -5) or (dx > 0) and (dx < 5)) or
      ((dy < 0) and (dy > -5) or (dy > 0) and (dy < 5)) then
        fCanShowHint:=true;
  fOldMousePos := Point(X, Y);
  fMousePos := PixelsToRowColumn(fOldMousePos);
  if ssLeft in Shift then
    identifierToD2Syn;
end;

procedure TCESynMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
  inherited;
  identifierToD2Syn;
  fCanShowHint := false;
  fDDocWin.Hide;
  fCallTipWin.Hide;
end;

procedure TCESynMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
  inherited;
  case Button of
    mbMiddle: if (Shift = [ssCtrl]) then
      Font.Size := fDefaultFontSize;
    // TODO-cfeature: Linux only, mouse action for src position buffer.
    mbExtra1: fPositions.back;
    mbExtra2: fPositions.next;
    mbLeft:   fPositions.store;
  end;
end;

function TCESynMemo.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  fCanShowHint:=false;
  fHintTimer.Enabled:=false;
  //SynEdit bug: begin scroll, press CTRL still while. Wrong cursor size sometimes.
end;
{$ENDREGION --------------------------------------------------------------------}

initialization
  D2Syn := TSynD2Syn.create(nil);
  LfmSyn := TSynLFMSyn.Create(nil);
  TxtSyn := TSynTxtSyn.create(nil);
  //
  LfmSyn.KeyAttri.Foreground := clNavy;
  LfmSyn.KeyAttri.Style := [fsBold];
  LfmSyn.NumberAttri.Foreground := clMaroon;
  LfmSyn.StringAttri.Foreground := clBlue;
  //
  TCEEditorHintWindow.FontSize := 10;
finalization
  D2Syn.Free;
  LfmSyn.Free;
  TxtSyn.Free;
end.
