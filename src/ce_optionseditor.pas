unit ce_optionseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, ExtCtrls,
  Menus, ComCtrls, Buttons, ce_common, ce_widget, ce_interfaces,
  ce_observer, PropEdits, ObjectInspector;

type

  // store the information about the obsever
  // exposing some editable options.
  PCategoryData = ^TCategoryData;

  TCategoryData = record
    kind: TOptionEditorKind;
    container: TPersistent;
    observer: ICEEditableOptions;
  end;

  //TODO-cbugfix: linux only, a conversion error is raised after a color's  been edited using the dialog color.

  { TCEOptionEditorWidget }

  TCEOptionEditorWidget = class(TCEWidget)
    btnCancel: TSpeedButton;
    btnAccept: TSpeedButton;
    pnlEd: TPanel;
    pnlBody: TPanel;
    pnlFooter: TPanel;
    Splitter1: TSplitter;
    inspector: TTIPropertyGrid;
    selCat: TTreeView;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure inspectorEditorFilter(Sender: TObject; aEditor: TPropertyEditor; var aShow: boolean);
    procedure inspectorModified(Sender: TObject);
    procedure selCatDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCatSelectionChanged(Sender: TObject);
  protected
    procedure UpdateShowing; override;
  private
    fEdOptsSubj: TCEEditableOptionsSubject;
    procedure updateCategories;
    function sortCategories(Cat1, Cat2: TTreeNode): integer;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEOptionEditorWidget.Create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fDockable := False;
  fModal := True;
  fEdOptsSubj := TCEEditableOptionsSubject.Create;
  inspector.CheckboxForBoolean := True;
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('cancel');
    btnCancel.Glyph.Assign(png);
    png.LoadFromLazarusResource('accept');
    btnAccept.Glyph.Assign(png);
  finally
    png.Free;
  end;
end;

destructor TCEOptionEditorWidget.Destroy;
begin
  fEdOptsSubj.Free;
  inherited;
end;

procedure TCEOptionEditorWidget.UpdateShowing;
begin
  inherited;
  if Visible then
    updateCategories;
end;

{$ENDREGION}

{$REGION Option editor things --------------------------------------------------}
procedure TCEOptionEditorWidget.updateCategories;
var
  i: Integer;
  dt: PCategoryData;
  ed: ICEEditableOptions;
begin
  inspector.TIObject := nil;
  selCat.Items.Clear;
  for i := 0 to fEdOptsSubj.observersCount - 1 do
  begin
    dt := new(PCategoryData);
    ed := fEdOptsSubj.observers[i] as ICEEditableOptions;
    selCat.Items.AddObject(nil, ed.optionedWantCategory, dt);
    dt^.container := ed.optionedWantContainer;
    dt^.kind := ed.optionedWantEditorKind;
    dt^.observer := ed;
  end;
  selCat.Items.SortTopLevelNodes(@sortCategories);
end;

function TCEOptionEditorWidget.sortCategories(Cat1, Cat2: TTreeNode): integer;
begin
  Result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TCEOptionEditorWidget.selCatDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data <> nil then
    Dispose(PCategoryData(node.Data));
end;

procedure TCEOptionEditorWidget.selCatSelectionChanged(Sender: TObject);
var
  dt: PCategoryData;
begin
  // remove either the control, the form or the inspector
  // being used as editor.
  inspector.TIObject := nil;
  if pnlEd.ControlCount > 0 then
    pnlEd.Controls[0].Parent := nil;
  //
  if selCat.Selected = nil then
    exit;
  if selCat.Selected.Data = nil then
    exit;
  //
  dt := PCategoryData(selCat.Selected.Data);
  if dt^.container = nil then
    exit;
  case dt^.kind of
    oekControl:
    begin
      TWinControl(dt^.container).Parent := pnlEd;
      TWinControl(dt^.container).Align := alClient;
    end;
    oekForm:
    begin
      TCustomForm(dt^.container).Parent := pnlEd;
      TCustomForm(dt^.container).Align := alClient;
      TCustomForm(dt^.container).BorderIcons := [];
      TCustomForm(dt^.container).BorderStyle := bsNone;
    end;
    oekGeneric:
    begin
      inspector.Parent := pnlEd;
      inspector.Align := alClient;
      inspector.TIObject := dt^.container;
    end;
  end;
end;

procedure TCEOptionEditorWidget.inspectorModified(Sender: TObject);
begin
  if selCat.Selected = nil then
    exit;
  if selcat.Selected.Data = nil then
    exit;
  //
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeChange);
end;

procedure TCEOptionEditorWidget.btnCancelClick(Sender: TObject);
begin
  if selCat.Selected = nil then
    exit;
  if selcat.Selected.Data = nil then
    exit;
  //
  if inspector.Parent <> nil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeCancel);
end;

procedure TCEOptionEditorWidget.inspectorEditorFilter(Sender: TObject; aEditor: TPropertyEditor; var aShow: boolean);
begin
  if aEditor.GetComponent(0) is TComponent then
  begin
    if aEditor.GetPropInfo^.Name = 'Tag' then
      aSHow := False;
    if aEditor.GetPropInfo^.Name = 'Name' then
      aSHow := False;
  end;
end;

procedure TCEOptionEditorWidget.btnAcceptClick(Sender: TObject);
begin
  if selCat.Selected = nil then
    exit;
  if selcat.Selected.Data = nil then
    exit;
  //
  if inspector.Parent <> nil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeAccept);
end;

{$ENDREGION}

end.
