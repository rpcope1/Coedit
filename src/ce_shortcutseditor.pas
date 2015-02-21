unit ce_shortcutseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Menus,
  ExtCtrls, LCLProc, ComCtrls,
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
    fCollection: TCollection;
    procedure setCollection(aValue: TCollection);
    function getCount: Integer;
    function getShortcut(index: Integer): TShortcutItem;
  published
    property items: TCollection read fCollection write setCollection;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    //
    function findIdentifier(const identifier: string): boolean;
    function findShortcut(aShortcut: Word): boolean;
    //
    property count: Integer read getCount;
    property item[index: Integer]: TShortcutItem read getShortcut; default;
  end;

  TCEShortcutEditor = class(TFrame, ICEEditableOptions)
    Panel1: TPanel;
    fltItems: TTreeFilterEdit;
    Panel2: TPanel;
    tree: TTreeView;
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
  fCollection := TCollection.Create(TShortcutItem);
end;

destructor TShortCutCollection.destroy;
begin
  fCollection.Free;
  inherited;
end;

procedure TShortCutCollection.setCollection(aValue: TCollection);
begin
  fCollection.Assign(aValue);
end;

function TShortCutCollection.getCount: Integer;
begin
  exit(fCollection.Count);
end;

function TShortCutCollection.getShortcut(index: Integer): TShortcutItem;
begin
  exit(TShortcutItem(fCollection.Items[index]));
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
  // todo
end;
{$ENDREGION}

{$REGION shortcut editor things ------------------------------------------------}
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
      prt := findCategory(cat, obs);
      if prt = nil then
        prt := tree.Items.AddObject(nil, cat, obs);
      if idt = '' then
        continue;
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

