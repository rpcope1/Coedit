unit ce_optionseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, ExtCtrls,
  Menus, ComCtrls, Buttons, PropEdits, ObjectInspector, ce_sharedres,
  ce_common, ce_widget, ce_interfaces, ce_observer, ce_dialogs;

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
    fUpdatingCat: boolean;
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
begin
  inherited;
  fIsDockable := false;
  fIsModal:= true;
  fEdOptsSubj := TCEEditableOptionsSubject.create;
  inspector.CheckboxForBoolean := true;
  inspector.PropertyEditorHook.AddHandlerModified(@inspectorModified);
  //
  AssignPng(btnCancel, 'cancel');
  AssignPng(btnAccept, 'accept');
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
  sel: string = '';
begin
  if selCat.Selected.isNotNil then
    sel := selCat.Selected.Text;
  fUpdatingCat := true;
  inspector.TIObject := nil;
  selCat.BeginUpdate;
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
  for i := 0 to selCat.Items.Count-1 do
    if SelCat.Items.Item[i].Text = sel then
      SelCat.Selected := SelCat.Items.Item[i];
  selCat.EndUpdate;
  fUpdatingCat := false;
end;

function TCEOptionEditorWidget.sortCategories(Cat1, Cat2: TTreeNode): integer;
begin
  result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TCEOptionEditorWidget.selCatDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data.isNotNil then
    Dispose(PCategoryData(node.Data));
end;

function TCEOptionEditorWidget.allowCategoryChange: boolean;
var
  dt: PCategoryData;
begin
  result := true;
  if fUpdatingCat then exit;
  if csDestroying in ComponentState then exit;
  if selCat.Selected.isNil then exit;
  if selCat.Selected.Data.isNil then exit;
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
    if dt^.container.isNil then exit;
    if dt^.observer = nil then exit;
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
  if selCat.Selected.isNil then exit;
  if selCat.Selected.Data.isNil then exit;
  //
  dt := PCategoryData(selCat.Selected.Data);
  if dt^.container.isNil then exit;
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
  if selCat.Selected.isNil then exit;
  if selcat.Selected.Data.isNil then exit;
  //
  fCatChanged := true;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeChange);
end;

procedure TCEOptionEditorWidget.btnCancelClick(Sender: TObject);
begin
  if selCat.Selected.isNil then exit;
  if selcat.Selected.Data.isNil then exit;
  //
  fCatChanged := false;
  if inspector.Parent.isNotNil then
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
var
  nme: string;
  len: integer;
begin
  if aEditor.GetComponent(0) is TComponent then
  begin
    nme := aEditor.GetPropInfo^.Name;
    len := nme.length;
    // TODO-cbugfix: filtering does not work on sub componenets 'e.g D2HL options)
    if (len > 2) and (nme[len - 2 .. len] = 'Tag') then
      aShow := false
    else if (len > 3) and (nme[len - 3 .. len] = 'Name') then
      aShow := false
    else if aEditor.GetPropInfo^.PropType = TypeInfo(TCollection) then
      aShow := false;
  end;
end;

procedure TCEOptionEditorWidget.btnAcceptClick(Sender: TObject);
begin
  if selCat.Selected.isNil then exit;
  if selcat.Selected.Data.isNil then exit;
  //
  fCatChanged := false;
  if inspector.Parent.isNotNil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeAccept);
end;
{$ENDREGION}

end.

