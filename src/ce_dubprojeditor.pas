unit ce_dubprojeditor;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, Buttons, ComCtrls, jsonparser, fpjson,
  ce_widget, ce_common, ce_interfaces, ce_observer, ce_dubproject;

type

 { TCEDubProjectEditorWidget }

  TCEDubProjectEditorWidget = class(TCEWidget, ICEProjectObserver)
    btnAddProp: TSpeedButton;
    btnAddProp1: TSpeedButton;
    btnDelProp: TSpeedButton;
    btnDelProp1: TSpeedButton;
    edValue: TMemo;
    PageControl1: TPageControl;
    pnlToolBar: TPanel;
    pnlToolBar1: TPanel;
    propTree: TTreeView;
    treeInspect: TTreeView;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure propTreeSelectionChanged(Sender: TObject);
  private
    fSelectedNode: TTreeNode;
    fProj: TCEDubProject;
    fNodeSources: TTreeNode;
    fNodeConfig: TTreeNode;
    procedure updateEditor;
    procedure updateInspector;
    procedure updateValueEditor;
    procedure setJsonValueFromEditor;
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    //
  protected
    procedure SetVisible(Value: boolean); override;
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

{$REGION  Standard Comp/Obj ----------------------------------------------------}
constructor TCEDubProjectEditorWidget.create(aOwner: TComponent);
begin
  inherited;
  fNodeSources := treeInspect.Items[0];
  fNodeConfig := treeInspect.Items[1];
end;

procedure TCEDubProjectEditorWidget.SetVisible(Value: boolean);
begin
  inherited;
  if not Value then exit;
  //
  updateEditor;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEDubProjectEditorWidget.projNew(aProject: ICECommonProject);
begin
  fProj := nil;
  enabled := false;
  if aProject.getFormat <> pfDub then
    exit;
  enabled := true;
  fProj := TCEDubProject(aProject.getProject);
  //
end;

procedure TCEDubProjectEditorWidget.projChanged(aProject: ICECommonProject);
begin
  if fProj = nil then
    exit;
  if aProject.getProject <> fProj then
    exit;
  if not Visible then
    exit;

  updateEditor;
  updateInspector;
end;

procedure TCEDubProjectEditorWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj = nil then
    exit;
  if aProject.getProject <> fProj then
    exit;
  fProj := nil;
  //
  updateEditor;
  updateInspector;
  enabled := false;
end;

procedure TCEDubProjectEditorWidget.projFocused(aProject: ICECommonProject);
begin
  fProj := nil;
  enabled := false;
  if aProject.getFormat <> pfDub then
    exit;
  fProj := TCEDubProject(aProject.getProject);
  enabled := true;
  if not Visible then
    exit;

  updateEditor;
  updateInspector;
end;

procedure TCEDubProjectEditorWidget.projCompiling(aProject: ICECommonProject);
begin
end;
{$ENDREGION}

{$REGION Editor ----------------------------------------------------------------}
procedure TCEDubProjectEditorWidget.propTreeSelectionChanged(Sender: TObject);
begin
  fSelectedNode := nil;
  if propTree.Selected = nil then exit;
  //
  fSelectedNode := propTree.Selected;
  updateValueEditor;
end;

procedure TCEDubProjectEditorWidget.setJsonValueFromEditor;
var
  dat: TJSONData;
  vFloat: TJSONFloat;
  vInt: integer;
  vInt64: int64;
  vBool: boolean;
begin
  if fSelectedNode = nil then exit;
  if fSelectedNode.Data = nil then exit;
  //
  dat := TJSONData(fSelectedNode.Data);
  case dat.JSONType of
    jtNumber:
      case TJSONNumber(dat).NumberType of
        ntFloat:
          if TryStrToFloat(edValue.Text, vFloat) then
            dat.AsFloat := vFloat;
        ntInt64:
          if TryStrToInt64(edValue.Text, vInt64) then
            dat.AsInt64 := vInt64;
        ntInteger:
          if TryStrToInt(edValue.Text, vInt) then
            dat.AsInteger := vInt;
      end;
     jtBoolean:
      if TryStrToBool(edValue.Text, vBool) then
        dat.AsBoolean := vBool;
      jtString:
        dat.AsString := edValue.Text;
  end;
end;

procedure TCEDubProjectEditorWidget.updateValueEditor;
var
  dat: TJSONData;
begin
  edValue.Clear;
  if fSelectedNode = nil then exit;
  if fSelectedNode.Data = nil then exit;
  //
  dat := TJSONData(fSelectedNode.Data);
  case dat.JSONType of
    jtNumber:
      case TJSONNumber(dat).NumberType of
        ntFloat:
          edValue.Text := FloatToStr(dat.AsFloat);
        ntInt64:
          edValue.Text := IntToStr(dat.AsInt64);
        ntInteger:
          edValue.Text := IntToStr(dat.AsInteger);
      end;
    jtBoolean:
      edValue.Text := BoolToStr(dat.AsBoolean);
    jtString:
      edValue.Text := dat.AsString;
  end;
end;

procedure TCEDubProjectEditorWidget.updateEditor;

  procedure addPropsFrom(node: TTreeNode; data: TJSONData);
  var
    i: integer;
    c: TTreeNode;
  begin
    if data.JSONType = jtObject then for i := 0 to data.Count-1 do
    begin
      c := node.TreeNodes.AddChildObject(node, TJSONObject(data).Names[i],
        TJSONObject(data).Items[i]);
      case TJSONObject(data).Items[i].JSONType of
       jtObject, jtArray:
         addPropsFrom(c, TJSONObject(data).Items[i]);
      end;
    end else if data.JSONType = jtArray then for i := 0 to data.Count-1 do
    begin
      c := node.TreeNodes.AddChildObject(node, format('item %d',[i]),
        TJSONArray(data).Items[i]);
      case TJSONArray(data).Items[i].JSONType of
       jtObject, jtArray:
         addPropsFrom(c, TJSONArray(data).Items[i]);
      end;
    end;
  end;

begin
  propTree.Items.Clear;
  if (fProj = nil) or (fProj.json = nil) then
    exit;
  //
  propTree.BeginUpdate;
  addPropsFrom(propTree.Items.Add(nil, 'project'), fProj.json);
  propTree.EndUpdate;
end;
{$ENDREGION}

{$REGION Inspector -------------------------------------------------------------}
procedure TCEDubProjectEditorWidget.updateInspector;
var
  i: integer;
begin
  if (fNodeConfig = nil) or (fNodeSources = nil) then
    exit;
  //
  fNodeConfig.DeleteChildren;
  fNodeSources.DeleteChildren;
  //
  if (fProj = nil) then
    exit;
  //
  for i:= 0 to fProj.getConfigurationCount -1 do
    treeInspect.Items.AddChild(fNodeConfig, fProj.getConfigurationName(i));
  for i := 0 to fProj.sources.count-1 do
    treeInspect.Items.AddChild(fNodeSources, fProj.sources.strings[i]);
end;
{$ENDREGION}

end.

