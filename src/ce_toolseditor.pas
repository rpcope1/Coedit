unit ce_toolseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, Buttons, StdCtrls, ce_widget, ce_tools;

type

  { TCEToolsEditorWidget }
  TCEToolsEditorWidget = class(TCEWidget)
    BtnAddTool: TBitBtn;
    btnRemTool: TBitBtn;
    btnRun: TBitBtn;
    lstTools: TListBox;
    Panel1: TPanel;
    Splitter1: TSplitter;
    propsEd: TTIPropertyGrid;
    procedure BtnAddToolClick(Sender: TObject);
    procedure btnRemToolClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure lstToolsSelectionChange(Sender: TObject; User: boolean);
    procedure propsEdModified(Sender: TObject);
  private
    procedure DataToGui;
    procedure updateNames;
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

constructor TCEToolsEditorWidget.create(aOwner: TComponent);
begin
  inherited;
  DataToGui;
end;

procedure TCEToolsEditorWidget.updateNames;
var
  i: Integer;
begin
  for i := 0 to CustomTools.tools.Count-1 do
    lstTools.Items.Strings[i] := CustomTools.tool[i].toolAlias;
end;

procedure TCEToolsEditorWidget.DataToGui;
var
  i: integer;
begin
  propsEd.TIObject := nil;
  propsEd.ItemIndex := -1;
  lstTools.Clear;
  //
  for i := 0 to CustomTools.tools.Count-1 do
    lstTools.AddItem(CustomTools.tool[i].toolAlias, nil);
  if lstTools.Count > 0 then
    lstTools.ItemIndex := 0;
end;

procedure TCEToolsEditorWidget.lstToolsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if lstTools.ItemIndex = -1 then
    exit;
  propsEd.TIObject := CustomTools.tool[lstTools.ItemIndex];
end;

procedure TCEToolsEditorWidget.propsEdModified(Sender: TObject);
begin
  if propsEd.ItemIndex = -1 then
    exit;
  if propsEd.Rows[propsEd.ItemIndex].Name = 'toolAlias' then
    updateNames;
end;

procedure TCEToolsEditorWidget.BtnAddToolClick(Sender: TObject);
begin
  CustomTools.addTool;
  DataToGui;
end;

procedure TCEToolsEditorWidget.btnRemToolClick(Sender: TObject);
begin
  if lstTools.ItemIndex = -1 then
      exit;
  CustomTools.tools.Delete(lstTools.ItemIndex);
  DataToGui;
end;

procedure TCEToolsEditorWidget.btnRunClick(Sender: TObject);
begin
  if lstTools.ItemIndex = -1 then
    exit;
  CustomTools.tool[lstTools.ItemIndex].execute;
end;

end.

