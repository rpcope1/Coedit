unit ce_editoroptions;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynEditMouseCmds, SynEditMiscClasses,
  SynEditKeyCmds, Menus, LCLProc,
  ce_interfaces, ce_observer, ce_common, ce_writableComponent, ce_synmemo,
  ce_d2syn, ce_txtsyn;

type

  (**
   * Container for the editor and highlither options.
   * The base class is also used to backup the settings
   * to allow a to preview and restore the settings when rejected.
   *
   * note: when adding a new property, the default value must be set in
   * the constructor according to the default value of the member binded
   * to the property.
   *)
  TCEEditorOptionsBase = class(TWritableLfmTextComponent)
  private
    // note this is how a TComponent can be edited in
    // a basic TTIGrid: in the ctor create the component
    // but expose it as a published TPersistent.
    fD2Syn: TPersistent;
    fTxtSyn: TPersistent;
    //
    fShortCuts: TCollection;
    //
    fSelCol: TSynSelectedColor;
    fFoldedColor: TSynSelectedColor;
    fMouseLinkColor: TSynSelectedColor;
    fBracketMatchColor: TSynSelectedColor;
    fFont: TFont;
    //
    fHintDelay: Integer;
    fAutoDotDelay: Integer;
    fTabWidth: Integer;
    fBlockIdent: Integer;
    fLineSpacing: Integer;
    fCharSpacing: Integer;
    fRightEdge: Integer;
    fBackground: TColor;
    fRightEdgeColor: TColor;
    fOptions1: TSynEditorOptions;
    fOptions2: TSynEditorOptions2;
    fMouseOptions: TSynEditorMouseOptions;
    fCompletionMenuCaseCare: boolean;
    //
    procedure setFont(aValue: TFont);
    procedure setSelCol(aValue: TSynSelectedColor);
    procedure setFoldedColor(aValue: TSynSelectedColor);
    procedure setMouseLinkColor(aValue: TSynSelectedColor);
    procedure setBracketMatchColor(aValue: TSynSelectedColor);
    procedure setD2Syn(aValue: TPersistent);
    procedure setTxtSyn(aValue: TPersistent);
    procedure setShortcuts(aValue: TCollection);
    procedure setHintDelay(aValue: Integer);
    procedure setAutoDotDelay(aValue: Integer);
  published
    property completionMenuCaseCare: boolean read fCompletionMenuCaseCare write fCompletionMenuCaseCare;
    property autoDotDelay: integer read fAutoDotDelay write SetautoDotDelay;
    property hintDelay: Integer read fHintDelay write setHintDelay;
    property bracketMatchColor: TSynSelectedColor read fBracketMatchColor write setBracketMatchColor;
    property mouseLinkColor: TSynSelectedColor read fMouseLinkColor write setMouseLinkColor;
    property selectedColor: TSynSelectedColor read fSelCol write setSelCol;
    property foldedColor: TSynSelectedColor read fFoldedColor write setFoldedColor;
    property background: TColor read fBackground write fBackground default clWhite;
    property tabulationWidth: Integer read fTabWidth write fTabWidth default 4;
    property blockIdentation: Integer read fBlockIdent write fBlockIdent default 4;
    property lineSpacing: Integer read fLineSpacing write fLineSpacing default 0;
    property characterSpacing: Integer read fCharSpacing write fCharSpacing default 0;
    property rightEdge: Integer read fRightEdge write fRightEdge default 80;
    property rightEdgeColor: TColor read fRightEdgeColor write fRightEdgeColor default clSilver;
    property font: TFont read fFont write setFont;
    property options1: TSynEditorOptions read fOptions1 write fOptions1;
    property options2: TSynEditorOptions2 read fOptions2 write fOptions2;
    property mouseOptions: TSynEditorMouseOptions read fMouseOptions write fMouseOptions;
    property D2Highlighter: TPersistent read fD2Syn write setD2Syn;
    property TxtHighlighter: TPersistent read fTxtSyn write setTxtSyn;
    property shortcuts: TCollection read fShortCuts write setShortcuts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure Assign(src: TPersistent); override;
  end;

  (**
   * Manages and exposes all the editor and highligther options to an TCEOptionsEditor.
   * It's also responsible to give the current options to a new editor.
   *)
  TCEEditorOptions = class(TCEEditorOptionsBase, ICEEditableOptions, ICEMultiDocObserver, ICEEDitableShortcut)
  private
    fBackup: TCEEditorOptionsBase;
    fShortcutCount: Integer;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    //
    function scedWantFirst: boolean;
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
    //
    procedure applyChangesFromSelf;
    procedure applyChangeToEditor(anEditor: TCESynMemo);
  protected
    procedure afterLoad; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

const
  edoptFname = 'editor.txt';

var
  EditorOptions: TCEEditorOptions;

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCEEditorOptionsBase.Create(AOwner: TComponent);
var
  i: integer;
  shc: TCEPersistentShortcut;
  ed: TSynEdit;
begin
  inherited;
  //
  fFont := TFont.Create;
  fFont.Name := 'Courier New';
  fFont.Quality := fqProof;
  fFont.Pitch := fpFixed;
  fFont.Size := 10;
  //
  fD2Syn := TSynD2Syn.Create(self);
  fD2Syn.Assign(D2Syn);
  fTxtSyn := TSynTxtSyn.Create(self);
  fTxtSyn.Assign(TxtSyn);
  //
  fHintDelay:=200;
  fAutoDotDelay:=200;
  fSelCol := TSynSelectedColor.Create;
  fFoldedColor := TSynSelectedColor.Create;
  fMouseLinkColor := TSynSelectedColor.Create;
  fBracketMatchColor := TSynSelectedColor.Create;
  //
  // note: default values come from TSynEditFoldedView ctor.
  fFoldedColor.Background := clNone;
  fFoldedColor.Foreground := clDkGray;
  fFoldedColor.FrameColor := clDkGray;
  //
  fMouseLinkColor.Style := [fsUnderline, fsBold];
  fMouseLinkColor.StyleMask := [];
  fMouseLinkColor.Foreground := clNone;
  fMouseLinkColor.Background := clNone;
  //
  fBracketMatchColor.Foreground := clRed;
  fBracketMatchColor.Background := clNone;
  //
  rightEdge := 80;
  tabulationWidth := 4;
  blockIdentation := 4;
  fBackground := clWhite;
  fRightEdgeColor := clSilver;
  //
  options1 :=
    [eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoTabsToSpaces,
    eoDragDropEditing, eoShowCtrlMouseLinks, eoEnhanceHomeKey, eoTabIndent];
  options2 :=
    [eoEnhanceEndKey, eoFoldedCopyPaste, eoOverwriteBlock];
  //
  mouseOptions := MouseOptions +
    [emAltSetsColumnMode, emDragDropEditing, emCtrlWheelZoom, emShowCtrlMouseLinks];
  //
  fShortCuts := TCollection.Create(TCEPersistentShortcut);
  ed := TSynEdit.Create(nil);
  try
    // note: cant use a TCESynMemo because it'd be added to the EntitiesConnector
    SetDefaultCoeditKeystrokes(ed);
    for i:= 0 to ed.Keystrokes.Count-1 do
    begin
      shc := TCEPersistentShortcut(fShortCuts.Add);
      shc.actionName:= EditorCommandToCodeString(ed.Keystrokes.Items[i].Command);
      shc.shortcut  := ed.Keystrokes.Items[i].ShortCut;
    end;
  finally
    ed.free;
  end;
end;

destructor TCEEditorOptionsBase.Destroy;
begin
  fFont.Free;
  fSelCol.Free;
  fShortCuts.Free;
  fFoldedColor.Free;
  fMouseLinkColor.Free;
  fBracketMatchColor.Free;
  inherited;
end;

procedure TCEEditorOptionsBase.Assign(src: TPersistent);
var
  srcopt: TCEEditorOptionsBase;
begin
  if (src is TCEEditorOptionsBase) then
  begin
    srcopt := TCEEditorOptionsBase(src);
    //
    fCompletionMenuCaseCare:=srcopt.fCompletionMenuCaseCare;
    fAutoDotDelay:=srcopt.fAutoDotDelay;
    fHintDelay:=srcopt.fHintDelay;
    fFont.Assign(srcopt.fFont);
    fSelCol.Assign(srcopt.fSelCol);
    fFoldedColor.Assign(srcopt.fFoldedColor);
    fMouseLinkColor.Assign(srcopt.fMouseLinkColor);
    fBracketMatchColor.Assign(srcopt.fBracketMatchColor);
    fD2Syn.Assign(srcopt.fD2Syn);
    fTxtSyn.Assign(srcopt.fTxtSyn);
    background := srcopt.background;
    tabulationWidth := srcopt.tabulationWidth;
    blockIdentation := srcopt.blockIdentation;
    lineSpacing := srcopt.lineSpacing;
    characterSpacing := srcopt.characterSpacing;
    options1 := srcopt.options1;
    options2 := srcopt.options2;
    mouseOptions := srcopt.mouseOptions;
    rightEdge := srcopt.rightEdge;
    rightEdgeColor := srcopt.rightEdgeColor;
    fShortCuts.Assign(srcopt.fShortCuts);
  end
  else
    inherited;
end;

procedure TCEEditorOptionsBase.setHintDelay(aValue: Integer);
begin
  if aValue > 2000 then aValue := 2000
  else if aValue < 20 then aValue := 20;
  fHintDelay:=aValue;
end;

procedure TCEEditorOptionsBase.setAutoDotDelay(aValue: Integer);
begin
  if aValue > 2000 then aValue := 2000
  else if aValue < 0 then aValue := 0;
  fAutoDotDelay:=aValue;
end;

procedure TCEEditorOptionsBase.setShortcuts(aValue: TCollection);
begin
  fShortCuts.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setFont(aValue: TFont);
begin
  fFont.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setSelCol(aValue: TSynSelectedColor);
begin
  fSelCol.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setFoldedColor(aValue: TSynSelectedColor);
begin
  fFoldedColor.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setMouseLinkColor(aValue: TSynSelectedColor);
begin
  fMouseLinkColor.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setBracketMatchColor(aValue: TSynSelectedColor);
begin
  fBracketMatchColor.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setD2Syn(aValue: TPersistent);
begin
  D2Syn.Assign(aValue);
end;

procedure TCEEditorOptionsBase.setTxtSyn(aValue: TPersistent);
begin
  TxtSyn.Assign(aValue);
end;

constructor TCEEditorOptions.Create(AOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fBackup := TCEEditorOptionsBase.Create(self);
  EntitiesConnector.addObserver(self);
  //
  fname := getCoeditDocPath + edoptFname;
  if fileExists(fname) then
    loadFromFile(fname);
end;

destructor TCEEditorOptions.Destroy;
begin
  saveToFile(getCoeditDocPath + edoptFname);
  //
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEEditorOptions.afterLoad;
begin
  inherited;
  D2Syn.Assign(fD2Syn);
  TxtSyn.Assign(fTxtSyn);
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ----------------------------------------------------}
procedure TCEEditorOptions.docNew(aDoc: TCESynMemo);
begin
  applyChangeToEditor(aDoc);
end;

procedure TCEEditorOptions.docFocused(aDoc: TCESynMemo);
begin
end;

procedure TCEEditorOptions.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCEEditorOptions.docClosing(aDoc: TCESynMemo);
begin
end;
{$ENDREGION}

{$REGION ICEEDitableShortcut ---------------------------------------------------}
function TCEEditorOptions.scedWantFirst: boolean;
begin
  result := fShortCuts.Count > 0;
  fShortcutCount := 0;
end;

function TCEEditorOptions.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
var
  shrct: TCEPersistentShortcut;
begin
  shrct     := TCEPersistentShortcut(fShortCuts.Items[fShortcutCount]);
  category  := 'Code editor';
  identifier:= shrct.actionName;
  // SynEdit shortcuts start with 'ec'
  if length(identifier) > 2 then
    identifier := identifier[3..length(identifier)];
  aShortcut := shrct.shortcut;
  //
  fShortcutCount += 1;
  result := fShortcutCount < fShortCuts.Count;
end;

procedure TCEEditorOptions.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
var
  i: Integer;
  shc: TCEPersistentShortcut;
begin
  if category <> 'Code editor' then exit;
  //
  for i:= 0 to fShortCuts.Count-1 do
  begin
    shc := TCEPersistentShortcut(fShortCuts.Items[i]);
    if length(shc.actionName) > 2 then
    begin
      if shc.actionName[3..length(shc.actionName)] <> identifier then
        continue;
    end else if shc.actionName <> identifier then
      continue;
    shc.shortcut:= aShortcut;
    break;
  end;
  // note: shortcut modifications are not reversible,
  // they are sent from another option editor.
  applyChangesFromSelf;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCEEditorOptions.optionedWantCategory(): string;
begin
  exit('Editor');
end;

function TCEEditorOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TCEEditorOptions.optionedWantContainer: TPersistent;
begin
  fD2Syn := D2Syn;
  fTxtSyn := TxtSyn;
  fBackup.Assign(self);
  fBackup.fD2Syn.Assign(D2Syn);
  fBackup.fTxtSyn.Assign(TxtSyn);
  exit(self);
end;

procedure TCEEditorOptions.optionedEvent(anEvent: TOptionEditorEvent);
begin
  // restores
  if anEvent = oeeCancel then
  begin
    self.Assign(fBackup);
    D2Syn.Assign(fBackup.fD2Syn);
    TxtSyn.Assign(fBackup.fTxtSyn);
  end;
  // apply, if change/accept event
  // to get a live preview
  if anEvent <> oeeSelectCat then
    applyChangesFromSelf;
  // new backup values based on accepted values.
  if anEvent = oeeAccept then
  begin
    fBackup.Assign(self);
    fBackup.fD2Syn.Assign(D2Syn);
    fBackup.fTxtSyn.Assign(TxtSyn);
  end;
end;

{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
procedure TCEEditorOptions.applyChangesFromSelf;
var
  multied: ICEMultiDocHandler;
  i: Integer;
begin
  multied := getMultiDocHandler;
  for i := 0 to multied.documentCount - 1 do
    applyChangeToEditor(multied.document[i]);
end;

procedure TCEEditorOptions.applyChangeToEditor(anEditor: TCESynMemo);
var
  i, j: Integer;
  shc: TCEPersistentShortcut;
  kst: TSynEditKeyStroke;
begin
  anEditor.completionMenuCaseCare:=fCompletionMenuCaseCare;
  anEditor.autoDotDelay:=fAutoDotDelay;
  anEditor.hintDelay:=fHintDelay;
  anEditor.defaultFontSize := font.Size;
  anEditor.Font.Assign(font);
  anEditor.SelectedColor.Assign(fSelCol);
  anEditor.FoldedCodeColor.Assign(fFoldedColor);
  anEditor.MouseLinkColor.Assign(fMouseLinkColor);
  anEditor.BracketMatchColor.Assign(fBracketMatchColor);
  anEditor.TabWidth := tabulationWidth;
  anEditor.BlockIndent := blockIdentation;
  anEditor.ExtraLineSpacing := lineSpacing;
  anEditor.ExtraCharSpacing := characterSpacing;
  anEditor.Options := options1;
  anEditor.Options2 := options2;
  anEditor.MouseOptions := mouseOptions;
  anEditor.Color := background;
  anEditor.RightEdge := rightEdge;
  anEditor.RightEdgeColor := rightEdgeColor;
  for i := 0 to anEditor.Keystrokes.Count-1 do
  begin
    kst := anEditor.Keystrokes.Items[i];
    for j := 0 to fShortCuts.Count-1 do
    begin
      shc := TCEPersistentShortcut(fShortCuts.Items[j]);
      if shc.actionName = EditorCommandToCodeString(kst.Command) then
      begin
        try
          // if anEditor.Keystrokes.FindShortcut();
          // try to find, if not match cur action, set to 0
          kst.ShortCut := shc.shortcut;
        except
          kst.ShortCut := 0;
          shc.shortcut := 0;
          // TODO-cfeaure: manage shortcuts conflicts
          // either here or in the shortcut editor.
          // by default and if a conflict exists synedit will raise an exception here.
        end;
        break;
      end;
    end;
  end;
end;

{$ENDREGION}


initialization
  EditorOptions := TCEEditorOptions.Create(nil);

finalization
  EditorOptions.Free;
end.
