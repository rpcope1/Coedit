unit ce_projinspect;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics, actnlist,
  Dialogs, ExtCtrls, ComCtrls, Menus, Buttons, lcltype, ce_project, ce_interfaces,
  ce_common, ce_widget, ce_observer;

type

  { TCEProjectInspectWidget }
  TCEProjectInspectWidget = class(TCEWidget, ICEProjectObserver)
    btnRemFold: TSpeedButton;
    imgList: TImageList;
    Panel1: TPanel;
    btnAddFile: TSpeedButton;
    btnProjOpts: TSpeedButton;
    btnAddFold: TSpeedButton;
    btnRemFile: TSpeedButton;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnAddFoldClick(Sender: TObject);
    procedure btnRemFileClick(Sender: TObject);
    procedure btnRemFoldClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeSelectionChanged(Sender: TObject);
  protected
    procedure UpdateByEvent; override;
  private
    fActOpenFile: TAction;
    fActSelConf: TAction;
    fProject: TCEProject;
    fFileNode, fConfNode: TTreeNode;
    fImpsNode, fInclNode: TTreeNode;
    fXtraNode: TTreeNode;
    procedure actUpdate(sender: TObject);
    procedure TreeDblClick(sender: TObject);
    procedure actOpenFileExecute(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  end;

implementation
{$R *.lfm}

uses
  ce_main, ce_symstring;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEProjectInspectWidget.create(aOwner: TComponent);
begin
  fActOpenFile := TAction.Create(self);
  fActOpenFile.Caption := 'Open file in editor';
  fActOpenFile.OnExecute := @actOpenFileExecute;
  fActSelConf := TAction.Create(self);
  fActSelConf.Caption := 'Select configuration';
  fActSelConf.OnExecute := @actOpenFileExecute;
  fActSelConf.OnUpdate := @actUpdate;
  //
  inherited;
  Tree.OnDblClick := @TreeDblClick;
  fFileNode := Tree.Items[0];
  fConfNode := Tree.Items[1];
  fImpsNode := Tree.Items[2];
  fInclNode := Tree.Items[3];
  fXtraNode := Tree.Items[4];
  //
  Tree.PopupMenu := contextMenu;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.endUpdate;
end;

destructor TCEProjectInspectWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEProjectInspectWidget.contextName: string;
begin
  exit('Inspector');
end;

function TCEProjectInspectWidget.contextActionCount: integer;
begin
  exit(2);
end;

function TCEProjectInspectWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActOpenFile);
    1: exit(fActSelConf);
    else exit(nil);
  end;
end;

procedure TCEProjectInspectWidget.actOpenFileExecute(sender: TObject);
begin
  TreeDblClick(sender);
end;
{$ENDREGION}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCEProjectInspectWidget.projNew(aProject: TCEProject);
begin
  fProject := aProject;
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.projClosing(aProject: TCEProject);
begin
  if fProject <> aProject then
    exit;
  fProject := nil;
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.projFocused(aProject: TCEProject);
begin
  fProject := aProject;
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.projChanged(aProject: TCEProject);
begin
  if fProject <> aProject then
    exit;
  UpdateByEvent;
end;
{$ENDREGION}

{$REGION Insêctor things -------------------------------------------------------}
procedure TCEProjectInspectWidget.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreeDblClick(nil);
end;

procedure TCEProjectInspectWidget.TreeSelectionChanged(Sender: TObject);
begin
  actUpdate(sender);
end;

procedure TCEProjectInspectWidget.TreeDblClick(sender: TObject);
var
  fname: string;
  i: NativeInt;
begin
  if fProject = nil then exit;
  if Tree.Selected = nil then exit;
  //
  if Tree.Selected.Parent = fFileNode then
  begin
    fname := Tree.Selected.Text;
    i := fProject.Sources.IndexOf(fname);
    if i > -1 then
    begin
      fname := fProject.getAbsoluteSourceName(i);
      if fileExists(fname) then
        CEMainForm.openFile(fname);
    end;
  end
  else if Tree.Selected.Parent = fConfNode then
  begin
    i := Tree.Selected.Index;
    fProject.ConfigurationIndex := i;
    UpdateByEvent;
  end;
end;

procedure TCEProjectInspectWidget.actUpdate(sender: TObject);
begin
  fActSelConf.Enabled := false;
  fActOpenFile.Enabled := false;
  if Tree.Selected = nil then exit;
  fActSelConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActOpenFile.Enabled := Tree.Selected.Parent = fFileNode;
end;

procedure TCEProjectInspectWidget.btnAddFileClick(Sender: TObject);
begin
  if fProject = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    filter := DdiagFilter;
    if execute then
      fProject.addSource(filename);
  finally
    free;
  end;
end;

procedure TCEProjectInspectWidget.btnAddFoldClick(Sender: TObject);
var
  dir, fname, ext: string;
  lst: TStringList;
  i: NativeInt;
begin
  if fProject = nil then exit;
  //
  if fileExists(fProject.fileName) then
    dir := extractFilePath(fProject.fileName)
  else dir := '';
  if selectDirectory('sources', dir, dir, true, 0) then
  begin
    lst := TStringList.Create;
    try
      listFiles(lst, dir, true);
      for i := 0 to lst.Count-1 do
      begin
        fname := lst.Strings[i];
        ext := extractFileExt(fname);
        if dExtList.IndexOf(ext) <> -1 then
          fProject.addSource(fname);
      end;
    finally
      lst.Free;
    end;
  end;
end;

procedure TCEProjectInspectWidget.btnRemFoldClick(Sender: TObject);
var
  dir, fname: string;
  i: Integer;
begin
  if fProject = nil then exit;
  if Tree.Selected = nil then exit;
  if Tree.Selected.Parent <> fFileNode then exit;
  //
  fname := Tree.Selected.Text;
  i := fProject.Sources.IndexOf(fname);
  if i = -1 then exit;
  fname := fProject.getAbsoluteSourceName(i);
  dir := extractFilePath(fname);
  if not DirectoryExists(dir) then exit;
  //
  for i:= fProject.Sources.Count-1 downto 0 do
    if extractFilePath(fProject.getAbsoluteSourceName(i)) = dir then
      fProject.Sources.Delete(i);
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.btnRemFileClick(Sender: TObject);
var
  fname: string;
  i: NativeInt;
begin
  if fProject = nil then exit;
  if Tree.Selected = nil then exit;
  //
  if Tree.Selected.Parent = fFileNode then
  begin
    fname := Tree.Selected.Text;
    i := fProject.Sources.IndexOf(fname);
    if i > -1 then fProject.Sources.Delete(i);
    UpdateByEvent;
  end;
end;

procedure TCEProjectInspectWidget.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  fname: string;
begin
  CEMainForm.FormDropFiles(Sender, Filenames);
  if fProject = nil then exit;
  for fname in Filenames do fProject.addSource(fname);
end;

procedure TCEProjectInspectWidget.UpdateByEvent;
var
  src, fold, conf: string;
  lst: TStringList;
  itm: TTreeNode;
  i: NativeInt;
begin
  fConfNode.DeleteChildren;
  fFileNode.DeleteChildren;
  fImpsNode.DeleteChildren;
  fInclNode.DeleteChildren;
  fXtraNode.DeleteChildren;
  if fProject = nil then exit;
  // display main sources
  for src in fProject.Sources do
   begin
     itm := Tree.Items.AddChild(fFileNode, src);
     itm.ImageIndex := 2;
     itm.SelectedIndex := 2;
   end;
  // display configurations
  for i := 0 to fProject.OptionsCollection.Count-1 do
  begin
    conf := fProject.configuration[i].name;
    if i = fProject.ConfigurationIndex then conf += ' (active)';
    itm := Tree.Items.AddChild(fConfNode, conf);
    itm.ImageIndex := 3;
    itm.SelectedIndex:= 3;
  end;
  // display Imports (-J)
  for fold in FProject.currentConfiguration.pathsOptions.Imports do
  begin
    if fold = '' then
      continue;
    itm := Tree.Items.AddChild(fImpsNode, shortenPath(symbolExpander.get(fold)));
    itm.ImageIndex := 5;
    itm.SelectedIndex := 5;
  end;
  fImpsNode.Collapse(false);
  // display Includes (-I)
  for fold in FProject.currentConfiguration.pathsOptions.Includes do
  begin
    if fold = '' then
      continue;
    itm := Tree.Items.AddChild(fInclNode, shortenPath(symbolExpander.get(fold)));
    itm.ImageIndex := 5;
    itm.SelectedIndex := 5;
  end;
  fInclNode.Collapse(false);
  // display extra sources (external .lib, *.a, *.d)
  for src in FProject.currentConfiguration.pathsOptions.extraSources do
  begin
    if src = '' then
      continue;
    lst := TStringList.Create;
    try
      if listAsteriskPath(src, lst) then for src in lst do begin
        itm := Tree.Items.AddChild(fXtraNode, shortenPath(symbolExpander.get(src)));
        itm.ImageIndex := 2;
        itm.SelectedIndex := 2;
      end else begin
        itm := Tree.Items.AddChild(fXtraNode, shortenPath(symbolExpander.get(src)));
        itm.ImageIndex := 2;
        itm.SelectedIndex := 2;
      end;
    finally
      lst.Free;
    end;
  end;
  fXtraNode.Collapse(false);
end;
{$ENDREGION --------------------------------------------------------------------}

end.
