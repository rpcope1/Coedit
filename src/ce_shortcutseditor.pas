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
  published
    property identifier: string read fIdentifier write fIdentifier;
    property data: TShortcut read fData write fData;
  public
    function combination: string;
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
    //
    function findIdentifier(const identifier: string): boolean;
    function findShortcut(aShortcut: Word): boolean;
    //
    property count: Integer read getCount;
    property item[index: Integer]: TShortcutItem read getItem; default;
  end;

  TCEShortcutEditor = class(TFrame, ICEEditableOptions)
    shortcutCatcher: TEdit;
    Panel1: TPanel;
    fltItems: TTreeFilterEdit;
    Panel2: TPanel;
    schrtText: TStaticText;
    btnActivate: TSpeedButton;
    tree: TTreeView;
    procedure btnActivateClick(Sender: TObject);
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
begin
  inherited;
  fObservers := TCEEditableShortCutSubject.create;
  fShortcuts := TShortCutCollection.create(self);
  fBackup := TShortCutCollection.create(self);
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
  // TODO-cfeature: pass new shortcut to observer
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

procedure TCEShortcutEditor.LabeledEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if tree.Selected = nil then exit;
  if tree.Selected.Level = 0 then exit;
  if tree.Selected.Data = nil then exit;
  //
  if Key = VK_RETURN then shortcutCatcher.Enabled := false
  else TShortcutItem(tree.Selected.Data).data := Shortcut(Key, Shift);
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

procedure TCEShortcutEditor.updateFromObservers;
var
  i: Integer;
  obs: ICEEditableShortCut;
  cat: string;
  prt: TTreeNode;
  sht: word;
  idt: string;
  itm: TShortcutItem;
begin
  tree.Items.Clear;
  fShortcuts.items.Clear;
  fBackup.items.Clear;
  cat := '';
  idt := '';
  for i:= 0 to fObservers.observersCount-1 do
  begin
    obs := fObservers.observers[i] as ICEEditableShortCut;
    if obs.scedWantFirst then while obs.scedWantNext(cat, idt, sht) do
    begin
      // root category
      if cat = '' then
        continue;
      if idt = '' then
        continue;
      prt := findCategory(cat, obs);
      if prt = nil then
        prt := tree.Items.AddObject(nil, cat, obs);
      // item as child
      itm := TShortcutItem(fShortcuts.items.Add);
      itm.identifier := idt;
      itm.data:= sht;
      tree.Items.AddChildObject(prt, idt, itm);
      cat := '';
      idt := '';
    end;
  end;
end;
{$ENDREGION}

initialization
  CEShortcutEditor := TCEShortcutEditor.Create(nil);
finalization
  CEShortcutEditor.Free;
end.

