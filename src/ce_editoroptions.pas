unit ce_editoroptions;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynEditMouseCmds,
  ce_interfaces, ce_observer, ce_common, ce_writableComponent, ce_synmemo,
  ce_d2syn, ce_txtsyn;

type

  TDHighligthOptions = class(TPersistent)

  end;

  TTxtHighligthOptions = class(TPersistent)

  end;

  (**
   * Container for the editor and highlither options.
   * The base class is also used to backup settings
   * to allow settings to be previewed in live and restored
   * when not accepted.
   *)
  TCEEditorOptionsBase = class(TWritableLfmTextComponent)
  private
    fDHlOptions: TPersistent;
    fTxtHlOptions: TPersistent;
    fFont: TFont;
    //
    fTabWidth: Integer;
    fBlockIdent: Integer;
    fLineSpacing: Integer;
    fCharSpacing: Integer;
    fOptions1: TSynEditorOptions;
    fOptions2: TSynEditorOptions2;
    fMouseOptions: TSynEditorMouseOptions;
    //
    procedure setFont(aFont: TFont);
  published
    property tabulationWidth: Integer read fTabWidth write fTabWidth;
    property blockIdentation: Integer read fBlockIdent write fBlockIdent;
    property lineSpacing: Integer read fLineSpacing write fLineSpacing;
    property characterSpacing: Integer read fCharSpacing write fCharSpacing;
    property font: TFont read fFont write setFont;
    property options1: TSynEditorOptions read fOptions1 write fOptions1;
    property options2: TSynEditorOptions2 read fOptions2 write fOptions2;
    property mouseOptions: TSynEditorMouseOptions read fMouseOptions write fMouseOptions;

    property D_colorizer: TPersistent read fDHlOptions;
    property TXT_colorizer: TPersistent read fTxtHlOptions;
  public
    constructor Create(AOwner: TComponent); override;
    //
    procedure assign(src: TPersistent); override;
  end;

  (**
   * Manages and exposes all the editor and highligther options to an TCEOptionsEditor.
   *)
  TCEEditorOptions = class(TCEEditorOptionsBase, ICEEditableOptions, ICEMultiDocObserver)
  private
    fBackup: TCEEditorOptionsBase;
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
    procedure applyChangesFromSelf;
    procedure applyChangeToEditor(anEditor: TCESynMemo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  EditorOptions: TCEEditorOptions;

implementation

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCEEditorOptionsBase.Create(AOwner: TComponent);
begin
  inherited;
  fDHlOptions := D2Syn;
  fTxtHlOptions := TxtSyn;
  //
  fFont := TFont.Create;
  fFont.Size := 10;
  fFont.Name := 'Courier New';
  fFont.Quality := fqProof;
  fFont.Pitch := fpFixed;
  fFont.Size:= 10;
  //
  tabulationWidth := 4;
  blockIdentation := 4;
  //
  options1 :=
    [eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoTabsToSpaces,
    eoDragDropEditing, eoShowCtrlMouseLinks, eoEnhanceHomeKey, eoTabIndent];
  options2 :=
    [eoEnhanceEndKey, eoFoldedCopyPaste, eoOverwriteBlock];
  //
  mouseOptions := MouseOptions +
    [emAltSetsColumnMode, emDragDropEditing, emCtrlWheelZoom, emShowCtrlMouseLinks];
end;

constructor TCEEditorOptions.Create(AOwner: TComponent);
begin
  inherited;
  fBackup := TCEEditorOptionsBase.Create(self);
  EntitiesConnector.addObserver(self);
end;

destructor TCEEditorOptions.Destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEEditorOptionsBase.assign(src: TPersistent);
var
  srcopt: TCEEditorOptionsBase;
begin
  if (src is TCEEditorOptionsBase) then
  begin
    srcopt := TCEEditorOptionsBase(src);
    //
    font.Assign(srcopt.font);
    tabulationWidth := srcopt.tabulationWidth;
    blockIdentation := srcopt.blockIdentation;
    lineSpacing     := srcopt.lineSpacing;
    characterSpacing:= srcopt.characterSpacing;
    options1        := srcopt.options1;
    options2        := srcopt.options2;
    mouseOptions    := srcopt.mouseOptions;
  end
  else inherited;
end;

procedure TCEEditorOptionsBase.setFont(aFont: TFont);
begin
  fFont.Assign(aFont);
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

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCEEditorOptions.optionedWantCategory(): string;
begin
  exit('Editor');
end;

function TCEEditorOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekAbstract);
end;

function TCEEditorOptions.optionedWantContainer: TPersistent;
begin
  fBackup.Assign(self);
  exit(self);
end;

procedure TCEEditorOptions.optionedEvent(anEvent: TOptionEditorEvent);
begin
  // restores
  if anEvent = oeeCancel then
    self.assign(fBackup);
  // apply
  applyChangesFromSelf;
  // new backup values based on accepted values.
  if anEvent = oeeAccept then
    fBackup.assign(self);
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
procedure TCEEditorOptions.applyChangesFromSelf;
var
  multied: ICEMultiDocHandler;
  i: Integer;
begin
  // editors
  multied := getMultiDocHandler;
  for i := 0 to multied.documentCount-1 do
    applyChangeToEditor(multied.document[i]);
  // highlighter(s)
  // ...
end;

procedure TCEEditorOptions.applyChangeToEditor(anEditor: TCESynMemo);
begin
  anEditor.Font.Assign(font);
  anEditor.TabWidth := tabulationWidth;
  anEditor.BlockIndent := blockIdentation;
  anEditor.ExtraLineSpacing := lineSpacing;
  anEditor.ExtraCharSpacing := characterSpacing;
  anEditor.Options := options1;
  anEditor.Options2 := options2;
  anEditor.MouseOptions := mouseOptions;
end;
{$ENDREGION}


initialization
  EditorOptions := TCEEditorOptions.Create(nil);
finalization
  EditorOptions.Free;
end.

