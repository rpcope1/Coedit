unit ce_shortcutseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Menus, Graphics,
  ExtCtrls, LCLProc, ComCtrls, StdCtrls, Buttons, LCLType,
  ce_observer, ce_interfaces, ce_common, ce_writableComponent;

type

  TShortcutItem = class(TCollectionItem)
  private
    fIdentifier: string;
    fData: TShortcut;
    fDeclarator: ICEEditableShortCut;
    property declarator: ICEEditableShortCut read fDeclarator write fDeclarator;
  published
    property identifier: string read fIdentifier write fIdentifier;
    property data: TShortcut read fData write fData;
  public
    function combination: string;
    procedure assign(aValue: TPersistent); override;
  end;

  TShortCutCollection = class(TWritableLfmTextComponent)
  private
    fItems: TCollection;
    procedure setItems(aValue: TCollection);
    function getCount: Integer;
    function getItem(index: Integer): TShortcutItem;
  published
    property items: TCollection read fItems write setItems;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    //
    function findIdentifier(const identifier: string): boolean;
    function findShortcut(aShortcut: Word): boolean;
    //
    property count: Integer read getCount;
    property item[index: Integer]: TShortcutItem read getItem; default;
  end;

  { TCEShortcutEditor }

  TCEShortcutEditor = class(TFrame, ICEEditableOptions)
    btnClear: TSpeedButton;
    shortcutCatcher: TEdit;
    Panel1: TPanel;
    fltItems: TTreeFilterEdit;
    Panel2: TPanel;
    schrtText: TStaticText;
    btnActivate: TSpeedButton;
    tree: TTreeView;
    procedure btnActivateClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure LabeledEdit1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure shortcutCatcherExit(Sender: TObject);
    procedure shortcutCatcherMouseLeave(Sender: TObject);
    procedure treeSelectionChanged(Sender: TObject);
  private
    fObservers: TCEEditableShortCutSubject;
    fShortcuts: TShortCutCollection;
    fBackup: TShortCutCollection;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    //
    function findCategory(const aName: string; aData: Pointer): TTreeNode;
    function sortCategories(Cat1, Cat2: TTreeNode): integer;
    procedure updateFromObservers;
    procedure updateEditCtrls;
  protected
    procedure UpdateShowing; override;
  public
    constructor create(TheOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

var
  CEShortcutEditor: TCEShortcutEditor;

{$REGION TShortCutCollection ---------------------------------------------------}
function TShortcutItem.combination: string;
begin
  result := ShortCutToText(fData);
end;

procedure TShortcutItem.assign(aValue: TPersistent);
var
  src: TShortcutItem;
begin
  if aValue is TShortcutItem then
  begin
    src := TShortcutItem(aValue);
    fData:= src.fData;
    fIdentifier:= src.fIdentifier;
    fDeclarator := src.fDeclarator;
  end
  else inherited;
end;

constructor TShortCutCollection.create(AOwner: TComponent);
begin
  inherited;
  fItems := TCollection.Create(TShortcutItem);
end;

destructor TShortCutCollection.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TShortCutCollection.assign(aValue: TPersistent);
begin
  if aValue is TShortCutCollection then
    fItems.Assign(TShortCutCollection(aValue).fItems)
  else
    inherited;
end;

procedure TShortCutCollection.setItems(aValue: TCollection);
begin
  fItems.Assign(aValue);
end;

function TShortCutCollection.getCount: Integer;
begin
  exit(fItems.Count);
end;

function TShortCutCollection.getItem(index: Integer): TShortcutItem;
begin
  exit(TShortcutItem(fItems.Items[index]));
end;

function TShortCutCollection.findIdentifier(const identifier: string): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to count-1 do
    if item[i].identifier = identifier then
      exit(true);
end;

function TShortCutCollection.findShortcut(aShortcut: Word): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to count-1 do
    if item[i].data = aShortcut then
      exit(true);
end;
{$ENDREGION}

{$REGION Standard Comp/Object things -------------------------------------------}
constructor TCEShortcutEditor.create(TheOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fObservers := TCEEditableShortCutSubject.create;
  fShortcuts := TShortCutCollection.create(self);
  fBackup := TShortCutCollection.create(self);
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('clean');
    btnClear.Glyph.Assign(png);
  finally
    png.free;
  end;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCEShortcutEditor.destroy;
begin
  fObservers.Free;
  inherited;
end;

procedure TCEShortcutEditor.UpdateShowing;
var
  png : TPortableNetworkGraphic;
begin
  inherited;
  if not visible then exit;
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('keyboard_pencil');
    btnActivate.Glyph.Assign(png);
  finally
    png.free;
  end;
end;
{$ENDREGION}

{$REGION ICEEditableOptions ----------------------------------------------------}
function TCEShortcutEditor.optionedWantCategory(): string;
begin
  exit('Shortcuts');
end;

function TCEShortcutEditor.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TCEShortcutEditor.optionedWantContainer: TPersistent;
begin
  updateFromObservers;
  exit(self);
end;

procedure TCEShortcutEditor.optionedEvent(anEvent: TOptionEditorEvent);
begin
  case anEvent of
    oeeSelectCat: updateFromObservers;
    //TODO-cfeature: cancel modifications using fBackup
  end;
end;
{$ENDREGION}

{$REGION shortcut editor things ------------------------------------------------}
procedure TCEShortcutEditor.treeSelectionChanged(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TCEShortcutEditor.shortcutCatcherExit(Sender: TObject);
begin
  shortcutCatcher.Enabled := false;
  updateEditCtrls;
end;

procedure TCEShortcutEditor.shortcutCatcherMouseLeave(Sender: TObject);
begin
  shortcutCatcher.Enabled := false;
  updateEditCtrls;
end;

procedure TCEShortcutEditor.btnActivateClick(Sender: TObject);
begin
  if tree.Selected = nil then exit;
  if tree.Selected.Level = 0 then exit;
  if tree.Selected.Data = nil then exit;
  //
  shortcutCatcher.Enabled := not shortcutCatcher.Enabled;
end;

procedure TCEShortcutEditor.btnClearClick(Sender: TObject);
begin
  if tree.Selected = nil then exit;
  if tree.Selected.Level = 0 then exit;
  if tree.Selected.Data = nil then exit;
  //
  TShortcutItem(tree.Selected.Data).data := 0;
  TShortcutItem(tree.Selected.Data).declarator.scedSendItem(
    tree.Selected.Parent.Text,
    tree.Selected.Text, 0);
  //
  updateEditCtrls;
end;

procedure TCEShortcutEditor.LabeledEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  sh: TShortCut;
begin
  if tree.Selected = nil then exit;
  if tree.Selected.Level = 0 then exit;
  if tree.Selected.Data = nil then exit;
  //
  if Key = VK_RETURN then
    shortcutCatcher.Enabled := false
  else
  begin
    sh := Shortcut(Key, Shift);
    TShortcutItem(tree.Selected.Data).data := sh;
    TShortcutItem(tree.Selected.Data).declarator.scedSendItem(
      tree.Selected.Parent.Text,
      tree.Selected.Text, sh );
  end;
  //
  updateEditCtrls;
end;

procedure TCEShortcutEditor.updateEditCtrls;
begin
  schrtText.Caption := '';
  //
  if tree.Selected = nil then exit;
  if tree.Selected.Level = 0 then exit;
  if tree.Selected.Data = nil then exit;
  //
  schrtText.Caption := TShortcutItem(tree.Selected.Data).combination;
  shortcutCatcher.Text := '';
end;

function TCEShortcutEditor.findCategory(const aName: string; aData: Pointer): TTreeNode;
var
  i: Integer;
begin
  result := nil;
  for i:= 0 to tree.Items.Count-1 do
    if tree.Items[i].Text = aName then
      if tree.Items[i].Data = aData then
        exit(tree.Items[i]);
end;

function TCEShortcutEditor.sortCategories(Cat1, Cat2: TTreeNode): integer;
begin
  result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TCEShortcutEditor.updateFromObservers;
var
  i: Integer;
  obs: ICEEditableShortCut;
  cat: string;
  sht: word;
  idt: string;
  itm: TShortcutItem;
procedure addItem();
var
  prt: TTreeNode;
begin
  // root category
  if cat = '' then exit;
  if idt = '' then exit;
  prt := findCategory(cat, obs);
  if prt = nil then
    prt := tree.Items.AddObject(nil, cat, obs);
  // item as child
  itm := TShortcutItem(fShortcuts.items.Add);
  itm.identifier := idt;
  itm.data:= sht;
  itm.declarator := obs;
  tree.Items.AddChildObject(prt, idt, itm);
  cat := '';
  idt := '';
end;
begin
  tree.Items.Clear;
  fShortcuts.items.Clear;
  fBackup.items.Clear;
  cat := '';
  idt := '';
  for i:= 0 to fObservers.observersCount-1 do
  begin
    obs := fObservers.observers[i] as ICEEditableShortCut;
    if obs.scedWantFirst then
    begin
      while obs.scedWantNext(cat, idt, sht) do
        addItem();
      addItem();
    end;
  end;
  tree.Items.SortTopLevelNodes(@sortCategories);
  fBackup.Assign(fShortcuts);
end;
{$ENDREGION}

initialization
  CEShortcutEditor := TCEShortcutEditor.Create(nil);
finalization
  CEShortcutEditor.Free;
end.

