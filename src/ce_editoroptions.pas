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
    fSelAttribs: TSynSelectedColor;
    fFoldedColor: TSynSelectedColor;
    fMouseLinkAttribs: TSynSelectedColor;
    fBracketMatchAttribs: TSynSelectedColor;
    fIdentifierMarkup: TSynSelectedColor;
    fFont: TFont;
    //
    fDDocDelay: Integer;
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
    fCompletionMenuWidth: integer;
    fCompletionMenuLines: Byte;
    //
    procedure setFont(value: TFont);
    procedure setSelCol(value: TSynSelectedColor);
    procedure setFoldedColor(value: TSynSelectedColor);
    procedure setMouseLinkColor(value: TSynSelectedColor);
    procedure setBracketMatchColor(value: TSynSelectedColor);
    procedure SetIdentifierMarkup(value: TSynSelectedColor);
    procedure setD2Syn(value: TPersistent);
    procedure setTxtSyn(value: TPersistent);
    procedure setShortcuts(value: TCollection);
    procedure setDDocDelay(value: Integer);
    procedure setAutoDotDelay(value: Integer);
    procedure setCompletionMenuLines(value: byte);
  published
    property completionMenuCaseCare: boolean read fCompletionMenuCaseCare write fCompletionMenuCaseCare;
    property completionMenuWidth: integer read fCompletionMenuWidth write fCompletionMenuWidth;
    property completionMenuLines: byte read fCompletionMenuLines write setCompletionMenuLines;
    property autoDotDelay: integer read fAutoDotDelay write SetautoDotDelay;
    property hintDelay: Integer read fDDocDelay write setDDocDelay stored false; deprecated;
    property ddocDelay: Integer read fDDocDelay write setDDocDelay;
    property bracketMatch: TSynSelectedColor read fBracketMatchAttribs write setBracketMatchColor;
    property mouseLink: TSynSelectedColor read fMouseLinkAttribs write setMouseLinkColor;
    property selection: TSynSelectedColor read fSelAttribs write setSelCol;
    property folding: TSynSelectedColor read fFoldedColor write setFoldedColor;
    property identifierMatch: TSynSelectedColor read fIdentifierMarkup write SetIdentifierMarkup;
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
    property highlighterDlang: TPersistent read fD2Syn write setD2Syn;
    property highlighterGeneric: TPersistent read fTxtSyn write setTxtSyn;
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
    function optionedOptionsModified: boolean;
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
  fDDocDelay:=200;
  fAutoDotDelay:=200;
  fSelAttribs := TSynSelectedColor.Create;
  fFoldedColor := TSynSelectedColor.Create;
  fMouseLinkAttribs := TSynSelectedColor.Create;
  fBracketMatchAttribs := TSynSelectedColor.Create;
  fIdentifierMarkup := TSynSelectedColor.Create;
  //
  // note: default values come from TSynEditFoldedView ctor.
  fFoldedColor.Background := clNone;
  fFoldedColor.Foreground := clDkGray;
  fFoldedColor.FrameColor := clDkGray;
  //
  fMouseLinkAttribs.Style := [fsUnderline, fsBold];
  fMouseLinkAttribs.StyleMask := [];
  fMouseLinkAttribs.Foreground := clNone;
  fMouseLinkAttribs.Background := clNone;
  //
  fBracketMatchAttribs.Foreground := clRed;
  fBracketMatchAttribs.Background := clNone;
  //
  fIdentifierMarkup.Foreground:= clNone;
  fIdentifierMarkup.Background:= clSilver;
  //
  fCompletionMenuWidth:= 160;
  fCompletionMenuLines:= 15;
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
  fSelAttribs.Free;
  fShortCuts.Free;
  fFoldedColor.Free;
  fMouseLinkAttribs.Free;
  fBracketMatchAttribs.Free;
  fIdentifierMarkup.Free;
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
    fCompletionMenuWidth:=srcopt.fCompletionMenuWidth;
    fCompletionMenuLines:=srcopt.fCompletionMenuLines;
    fCompletionMenuCaseCare:=srcopt.fCompletionMenuCaseCare;
    fAutoDotDelay:=srcopt.fAutoDotDelay;
    fDDocDelay:=srcopt.fDDocDelay;
    fFont.Assign(srcopt.fFont);
    fSelAttribs.Assign(srcopt.fSelAttribs);
    fFoldedColor.Assign(srcopt.fFoldedColor);
    fMouseLinkAttribs.Assign(srcopt.fMouseLinkAttribs);
    fBracketMatchAttribs.Assign(srcopt.fBracketMatchAttribs);
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

procedure TCEEditorOptionsBase.setDDocDelay(value: Integer);
begin
  if value > 2000 then value := 2000
  else if value < 20 then value := 20;
  fDDocDelay:=value;
end;

procedure TCEEditorOptionsBase.setAutoDotDelay(value: Integer);
begin
  if value > 2000 then value := 2000
  else if value < 0 then value := 0;
  fAutoDotDelay:=value;
end;

procedure TCEEditorOptionsBase.setCompletionMenuLines(value: byte);
begin
  if value < 5 then value := 5
  else if value > 64 then value := 64;
  fCompletionMenuLines := value;
end;

procedure TCEEditorOptionsBase.setShortcuts(value: TCollection);
begin
  fShortCuts.Assign(value);
end;

procedure TCEEditorOptionsBase.setFont(value: TFont);
begin
  fFont.Assign(value);
end;

procedure TCEEditorOptionsBase.setSelCol(value: TSynSelectedColor);
begin
  fSelAttribs.Assign(value);
end;

procedure TCEEditorOptionsBase.setFoldedColor(value: TSynSelectedColor);
begin
  fFoldedColor.Assign(value);
end;

procedure TCEEditorOptionsBase.setMouseLinkColor(value: TSynSelectedColor);
begin
  fMouseLinkAttribs.Assign(value);
end;

procedure TCEEditorOptionsBase.setBracketMatchColor(value: TSynSelectedColor);
begin
  fBracketMatchAttribs.Assign(value);
end;

procedure TCEEditorOptionsBase.SetIdentifierMarkup(value: TSynSelectedColor);
begin
  fIdentifierMarkup.Assign(value);
end;

procedure TCEEditorOptionsBase.setD2Syn(value: TPersistent);
begin
  D2Syn.Assign(value);
end;

procedure TCEEditorOptionsBase.setTxtSyn(value: TPersistent);
begin
  TxtSyn.Assign(value);
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
var
  ed: TSynEdit;
  shc: TCEPersistentShortcut;
  i,j: integer;
  exists: boolean;
begin
  inherited;
  D2Syn.Assign(fD2Syn);
  TxtSyn.Assign(fTxtSyn);
  //
  ed := TSynEdit.Create(nil);
  try
    SetDefaultCoeditKeystrokes(ed);
    // new version with more shortcuts
    for i:= 0 to ed.Keystrokes.Count-1 do
    begin
      exists := false;
      for j := 0 to fShortcuts.count-1 do
      begin
        if TCEPersistentShortcut(fShortCuts.Items[j]).actionName <>
          EditorCommandToCodeString(ed.Keystrokes.Items[i].Command) then
            continue;
        exists := true;
        break;
      end;
      if exists then
        continue;
      shc := TCEPersistentShortcut(fShortCuts.Add);
      shc.actionName := EditorCommandToCodeString(ed.Keystrokes.Items[i].Command);
      shc.shortcut := ed.Keystrokes.Items[i].ShortCut;
    end;
    // new version wih less shortcuts
    for j := fShortcuts.count-1 downto 0 do
    begin
      exists := false;
      for i:= 0 to ed.Keystrokes.Count-1 do
      begin
        if TCEPersistentShortcut(fShortCuts.Items[j]).actionName <>
          EditorCommandToCodeString(ed.Keystrokes.Items[i].Command) then
            continue;
        exists := true;
        break;
      end;
      if exists then
        continue;
      fShortCuts.Delete(j);
    end;
  finally
    ed.free;
  end;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
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
  fCompletionMenuWidth := aDoc.completionMenu.TheForm.Width;
  fCompletionMenuLines := aDoc.completionMenu.LinesInWindow;
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
  fShortcutCount -= 1;
  // TODO: modifies interface so that the target knows when the last
  // item has been sent.
  if fShortcutCount = 0 then
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

function TCEEditorOptions.optionedOptionsModified: boolean;
begin
  exit(false);
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
  i, j, k: Integer;
  shc: TCEPersistentShortcut;
  kst: TSynEditKeyStroke;
  dup: boolean;
  savedSize: integer;
begin
  anEditor.D2Highlighter.Assign(D2Syn);
  anEditor.TxtHighlighter.Assign(TxtSyn);

  anEditor.autoDotDelay:=fAutoDotDelay;
  anEditor.ddocDelay:=fDDocDelay;

  savedSize := anEditor.Font.Size;
  anEditor.defaultFontSize := font.Size;
  anEditor.Font.Assign(font);
  anEditor.Font.Size := savedSize;

  anEditor.completionMenu.TheForm.Width   := fCompletionMenuWidth;
  anEditor.completionMenu.LinesInWindow   := fCompletionMenuLines;
  anEditor.completionMenu.CaseSensitive   := fCompletionMenuCaseCare;

  anEditor.SelectedColor.Assign(fSelAttribs);
  anEditor.FoldedCodeColor.Assign(fFoldedColor);
  anEditor.MouseLinkColor.Assign(fMouseLinkAttribs);
  anEditor.BracketMatchColor.Assign(fBracketMatchAttribs);
  anEditor.HighlightAllColor.Assign(fIdentifierMarkup);
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
      dup := false;
      shc := TCEPersistentShortcut(fShortCuts.Items[j]);
      if shc.actionName = EditorCommandToCodeString(kst.Command) then
      begin
        try
          for k := 0 to i-1 do
            if anEditor.Keystrokes.Items[k].shortCut = shc.shortcut then
              if shc.shortCut <> 0 then
                dup := true;
          if not dup then
            kst.shortCut := shc.shortcut;
        except
          kst.shortCut := 0;
          shc.shortcut := 0;
          // in case of conflict synedit raises an exception.
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
