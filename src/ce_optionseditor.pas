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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure inspectorEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure inspectorModified(Sender: TObject);
    procedure selCatChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure selCatDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCatSelectionChanged(Sender: TObject);
  protected
    procedure UpdateShowing; override;
  private
    fCatChanged: boolean;
    fEdOptsSubj: TCEEditableOptionsSubject;
    procedure updateCategories;
    function allowCategoryChange: boolean;
    function sortCategories(Cat1, Cat2: TTreeNode): integer;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  msg_mod = 'The current category modifications are not validated, discard them and continue ?';

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEOptionEditorWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fDockable := false;
  fModal:= true;
  fEdOptsSubj := TCEEditableOptionsSubject.create;
  inspector.CheckboxForBoolean := true;
  inspector.PropertyEditorHook.AddHandlerModified(@inspectorModified);
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

destructor TCEOptionEditorWidget.destroy;
begin
  fCatChanged := false;
  fEdOptsSubj.Free;
  inherited;
end;

procedure TCEOptionEditorWidget.UpdateShowing;
begin
  inherited;
  if Visible then updateCategories;
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
  for i:= 0 to fEdOptsSubj.observersCount-1 do
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
  result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TCEOptionEditorWidget.selCatDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data <> nil then
    Dispose(PCategoryData(node.Data));
end;

function TCEOptionEditorWidget.allowCategoryChange: boolean;
var
  dt: PCategoryData;
begin
  result := true;
  if selCat.Selected = nil then exit;
  if selCat.Selected.Data = nil then exit;
  // accept/cancel is relative to a single category
  dt := PCategoryData(selCat.Selected.Data);
  // generic editor, changes are tracked directly here
  if dt^.kind = oekGeneric then
  begin
    if fCatChanged then
    begin
      result := dlgOkCancel(msg_mod) = mrOk;
      fCatChanged := not result;
      if result then btnCancelClick(nil);
    end;
  // custom editor, changes are notified by optionedOptionsModified()
  end else
  begin
    dt := PCategoryData(selCat.Selected.Data);
    if dt^.container = nil then exit;
    if dt^.observer.optionedOptionsModified() then
    begin
      result := dlgOkCancel(msg_mod) = mrOk;
      if result then btnCancelClick(nil);
    end;
  end;
end;

procedure TCEOptionEditorWidget.selCatChanging(Sender: TObject;Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := allowCategoryChange;
end;

procedure TCEOptionEditorWidget.selCatSelectionChanged(Sender: TObject);
var
  dt: PCategoryData;
begin
  // remove either the control, the form or the inspector used as editor.
  inspector.TIObject := nil;
  if pnlEd.ControlCount > 0 then
    pnlEd.Controls[0].Parent := nil;
  //
  if selCat.Selected = nil then exit;
  if selCat.Selected.Data = nil then exit;
  //
  dt := PCategoryData(selCat.Selected.Data);
  if dt^.container = nil then exit;
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
        TCustomForm(dt^.container).BorderIcons:= [];
        TCustomForm(dt^.container).BorderStyle:= bsNone;
      end;
    oekGeneric:
      begin
        inspector.Parent := pnlEd;
        inspector.Align := alClient;
        inspector.TIObject := dt^.container;
      end;
  end;
  //
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeSelectCat);
end;

procedure TCEOptionEditorWidget.inspectorModified(Sender: TObject);
begin
  if selCat.Selected = nil then exit;
  if selcat.Selected.Data = nil then exit;
  //
  fCatChanged := true;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeChange);
end;

procedure TCEOptionEditorWidget.btnCancelClick(Sender: TObject);
begin
  if selCat.Selected = nil then exit;
  if selcat.Selected.Data = nil then exit;
  //
  fCatChanged := false;
  if inspector.Parent <> nil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeCancel);
end;

procedure TCEOptionEditorWidget.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  canClose := allowCategoryChange;
end;

procedure TCEOptionEditorWidget.inspectorEditorFilter(Sender: TObject;aEditor:
  TPropertyEditor; var aShow: boolean);
begin
  if aEditor.GetComponent(0) is TComponent then
  begin
    if aEditor.GetPropInfo^.Name = 'Tag' then
      aShow := false
    else if aEditor.GetPropInfo^.Name = 'Name' then
      aShow := false
    else if aEditor.GetPropInfo^.PropType = TypeInfo(TCollection) then
      aShow := false;
  end;
end;

procedure TCEOptionEditorWidget.btnAcceptClick(Sender: TObject);
begin
  if selCat.Selected = nil then exit;
  if selcat.Selected.Data = nil then exit;
  //
  fCatChanged := false;
  if inspector.Parent <> nil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeAccept);
end;
{$ENDREGION}

end.

