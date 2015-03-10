unit ce_toolseditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, Buttons, StdCtrls, ce_widget, ce_tools;

type
  TCEToolsEditorWidget = class(TCEWidget)
    BtnAddTool: TBitBtn;
    btnMoveDown: TBitBtn;
    btnMoveUp: TBitBtn;
    btnRemTool: TBitBtn;
    btnRun: TBitBtn;
    lstTools: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    propsEd: TTIPropertyGrid;
    procedure BtnAddToolClick(Sender: TObject);
    procedure btnRemToolClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure lstToolsDblClick(Sender: TObject);
    procedure lstToolsSelectionChange(Sender: TObject; User: boolean);
    procedure propsEdModified(Sender: TObject);
  private
    procedure executeSelectedTool;
    procedure clearInspector;
    procedure rebuildToolList;
    procedure updateToolList;
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

constructor TCEToolsEditorWidget.Create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  propsEd.CheckboxForBoolean := True;
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('arrow_up');
    btnMoveUp.Glyph.Assign(png);
    png.LoadFromLazarusResource('arrow_down');
    btnMoveDown.Glyph.Assign(png);
    png.LoadFromLazarusResource('application_add');
    BtnAddTool.Glyph.Assign(png);
    png.LoadFromLazarusResource('application_delete');
    btnRemTool.Glyph.Assign(png);
    png.LoadFromLazarusResource('application_flash');
    btnRun.Glyph.Assign(png);
  finally
    png.Free;
  end;
  rebuildToolList;
end;

procedure TCEToolsEditorWidget.clearInspector;
begin
  propsEd.TIObject := nil;
  propsEd.ItemIndex := -1;
end;

procedure TCEToolsEditorWidget.rebuildToolList;
var
  i: integer;
begin
  clearInspector;
  lstTools.Clear;
  //
  for i := 0 to CustomTools.tools.Count - 1 do
    lstTools.AddItem(CustomTools[i].toolAlias, nil);
  if lstTools.Count > 0 then
    lstTools.ItemIndex := 0;
end;

procedure TCEToolsEditorWidget.updateToolList;
var
  i: Integer;
begin
  for i := 0 to CustomTools.tools.Count - 1 do
    lstTools.Items.Strings[i] := CustomTools[i].toolAlias;
end;

procedure TCEToolsEditorWidget.lstToolsSelectionChange(Sender: TObject; User: boolean);
begin
  if lstTools.ItemIndex = -1 then
    exit;
  propsEd.TIObject := CustomTools[lstTools.ItemIndex];
end;

procedure TCEToolsEditorWidget.propsEdModified(Sender: TObject);
begin
  if propsEd.ItemIndex = -1 then
    exit;
  if propsEd.Rows[propsEd.ItemIndex].Name = 'toolAlias' then
    updateToolList;
end;

procedure TCEToolsEditorWidget.BtnAddToolClick(Sender: TObject);
begin
  CustomTools.addTool;
  rebuildToolList;
end;

procedure TCEToolsEditorWidget.btnRemToolClick(Sender: TObject);
begin
  if lstTools.ItemIndex = -1 then
    exit;
  clearInspector;
  CustomTools.tools.Delete(lstTools.ItemIndex);
  rebuildToolList;
end;

procedure TCEToolsEditorWidget.btnMoveUpClick(Sender: TObject);
begin
  if lstTools.ItemIndex = -1 then
    exit;
  if lstTools.ItemIndex = 0 then
    exit;
  //
  CustomTools.tools.Exchange(lstTools.ItemIndex, lstTools.ItemIndex - 1);
  lstTools.ItemIndex := lstTools.ItemIndex - 1;
  updateToolList;
end;

procedure TCEToolsEditorWidget.btnMoveDownClick(Sender: TObject);
begin
  if lstTools.ItemIndex = -1 then
    exit;
  if lstTools.ItemIndex = lstTools.Items.Count - 1 then
    exit;
  //
  CustomTools.tools.Exchange(lstTools.ItemIndex, lstTools.ItemIndex + 1);
  lstTools.ItemIndex := lstTools.ItemIndex + 1;
  updateToolList;
end;

procedure TCEToolsEditorWidget.executeSelectedTool;
begin
  if lstTools.ItemIndex = -1 then
    exit;
  CustomTools.executeTool(lstTools.ItemIndex);
end;

procedure TCEToolsEditorWidget.btnRunClick(Sender: TObject);
begin
  executeSelectedTool;
end;

procedure TCEToolsEditorWidget.lstToolsDblClick(Sender: TObject);
begin
  executeSelectedTool;
end;

end.
