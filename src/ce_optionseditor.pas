unit ce_optionseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, ExtCtrls,
  Menus, ComCtrls, StdCtrls, Buttons, ce_common, ce_widget, ce_interfaces,
  ce_observer;

type

  PCategoryData = ^TCategoryData;
  TCategoryData = record
    kind: TOptionEditorKind;
    container: TPersistent;
    observer: ICEEditableOptions;
  end;

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
    procedure inspectorModified(Sender: TObject);
    procedure selCatDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCatSelectionChanged(Sender: TObject);
  protected
    procedure UpdateShowing; override;
  private
    fEdOptsSubj: TCEEditableOptionsSubject;
    procedure updateCategories;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEOptionEditorWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fDockable := false;
  fEdOptsSubj := TCEEditableOptionsSubject.create;
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
  inspector.TIObject := nil;
  if pnlEd.ControlCount > 0 then
    pnlEd.Controls[0].Parent := nil;
  if selCat.Selected = nil then exit;
  if selCat.Selected.Data = nil then exit;
  //
  dt := PCategoryData(selCat.Selected.Data);
  if dt^.container = nil then exit;
  case dt^.kind of
    oekForm:
      begin
        TForm(dt^.container).Parent := pnlEd;
        TForm(dt^.container).Align := alClient;
        TForm(dt^.container).BorderStyle:= bsNone;
      end;
    oekAbstract:
      begin
        inspector.Parent := pnlEd;
        inspector.Align := alClient;
        inspector.TIObject := dt^.container;
      end;
  end;
end;

procedure TCEOptionEditorWidget.inspectorModified(Sender: TObject);
begin
  if selCat.Selected = nil then exit;
  if selcat.Selected.Data = nil then exit;
  //
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeChange);
end;

procedure TCEOptionEditorWidget.btnCancelClick(Sender: TObject);
begin
  if selCat.Selected = nil then exit;
  if selcat.Selected.Data = nil then exit;
  //
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeCancel);
  //
  // if generic editor then
  //  refresh displayed value since the provider may have updated the options container
end;

procedure TCEOptionEditorWidget.btnAcceptClick(Sender: TObject);
begin
  if selCat.Selected = nil then exit;
  if selcat.Selected.Data = nil then exit;
  //
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeAccept);
end;

{$ENDREGION}

end.

