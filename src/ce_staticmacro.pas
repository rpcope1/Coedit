unit ce_staticmacro;

{$I ce_defines.inc}

interface

uses
  Classes, Sysutils, SynEdit, SynEditAutoComplete,
  ce_interfaces, ce_writableComponent, ce_synmemo;

type

  (**
   * TCEStaticEditorMacro is used to insert static macros (parameter-less code snippets)
   * in an editor. A macro begins with the dollar symbol and ends with an alphanum.
   *
   * The dollar symbol is used as starter because it's usually accessible without
   * modifier: no CTRL, no ALT, no SHIFT.
   * Erroneous insertion is avoided because in D '$' is either followed
   * by a symbol: '$-1', '$]' or by a blank '$ ]'
   *
   * Shift + SPACE works automatically on the right editor (ICEMultiDocObserver)
   * Automatic insertion is handled in TCESynMemo.KeyUp()
   *)
  TCEStaticEditorMacro = class(TWritableComponent, ICEMultiDocObserver)
  private
    fCompletor: TSynEditAutoComplete;
    fMacros: TStringList;
    fDoc: TCESynMemo;
    fAutomatic: boolean;
    procedure sanitize;
    procedure addDefaults;
    procedure updateCompletor;
    procedure setMacros(aValue: TStringList);
    // ICEMultiDocObserver
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
  published
    // list of string with the format $<..>alnum=<..>
    property macros: TStringList read fMacros write setMacros;
    property automatic: boolean read fAutomatic write fAutomatic default true;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    // execute using the editor
    procedure Execute; overload;
    // execute in aEditor, according to aToken
    procedure Execute(aEditor: TCustomSynEdit; const aToken: string); overload;
  end;

const
  macFname = 'staticMacros.txt';

  defMacros: array[0..5] of string = (
  '$a=auto',
  '$c=class {}',
  '$s=struct {}',
  '$ut=unittest{}',
  '$fo=for(auto i = 0; ; )',
  '$fe=foreach(elem; )'
  );

var
  StaticEditorMacro: TCEStaticEditorMacro = nil;

implementation

uses
  ce_observer, ce_common;

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TCEStaticEditorMacro.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fAutomatic := true;
  fCompletor := TSynEditAutoComplete.Create(self);
  fMacros := TStringList.Create;
  fMacros.Delimiter := '=';
  //
  fname := getDocPath + macFname;
  if fileExists(fname) then loadFromFile(fname);
  addDefaults;
  //
  sanitize;
  updateCompletor;
  //
  EntitiesConnector.addObserver(Self);
end;

destructor TCEStaticEditorMacro.destroy;
begin
  saveToFile(getDocPath + macFname);
  EntitiesConnector.removeObserver(Self);
  //
  fMacros.Free;
  inherited;
end;

procedure TCEStaticEditorMacro.setMacros(aValue: TStringList);
begin
  fMacros.Assign(aValue);
  addDefaults;
  sanitize;
  updateCompletor;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEStaticEditorMacro.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEStaticEditorMacro.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEStaticEditorMacro.docChanged(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then
    exit;
end;

procedure TCEStaticEditorMacro.docClosing(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then
    exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION Macros things ---------------------------------------------------------}
procedure TCEStaticEditorMacro.sanitize;
var
  i: Integer;
  text: string;
  macro: string;
begin
  for i := fMacros.Count-1 downto 0 do
  begin
    text := fMacros.Strings[i];
    if length(text) >= 4 then
      if text[1] = '$' then
        if Pos('=', text) > 2 then
        begin
          macro := fMacros.Names[i];
          if (macro[length(macro)] in ['a'..'z', 'A'..'Z', '0'..'9']) then
            continue;
        end;
    fMacros.Delete(i);
  end;
end;

procedure TCEStaticEditorMacro.addDefaults;
var
  i: Integer;
begin
  for i := 0 to high(defMacros) do
    if fMacros.IndexOf(defMacros[i]) = -1 then
      fMacros.Add(defMacros[i]);
end;

procedure TCEStaticEditorMacro.updateCompletor;
var
  i: Integer;
  tok, val: string;
begin
  fCompletor.AutoCompleteList.Clear;
  for i := 0 to fMacros.Count-1 do
  begin
    tok := fMacros.Names[i];
    val := fMacros.ValueFromIndex[i];
    fCompletor.AutoCompleteList.Add(tok);
    fCompletor.AutoCompleteList.Add('=' + val);
  end;
end;

procedure TCEStaticEditorMacro.Execute;
begin
  if fDoc <> nil then
    fCompletor.ExecuteCompletion(fDoc.Identifier, fDoc);
end;

procedure TCEStaticEditorMacro.Execute(aEditor: TCustomSynEdit; const aToken: string);
begin
  if aEditor <> nil then
    fCompletor.ExecuteCompletion(aToken, aEditor);
end;
{$ENDREGION}

initialization
  StaticEditorMacro := TCEStaticEditorMacro.create(nil);
finalization
  StaticEditorMacro.Free;;
end.

