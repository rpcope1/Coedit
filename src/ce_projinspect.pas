unit ce_projinspect;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, actnlist,
  Dialogs, ExtCtrls, ComCtrls, Menus, Buttons, lcltype, ce_nativeproject, ce_interfaces,
  ce_common, ce_widget, ce_observer, ce_dialogs, ce_sharedres;

type

  { TCEProjectInspectWidget }

  TCEProjectInspectWidget = class(TCEWidget, ICEProjectObserver)
    btnRemFold: TSpeedButton;
    imgList: TImageList;
    pnlToolBar: TPanel;
    btnAddFile: TSpeedButton;
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
    procedure updateImperative; override;
    procedure updateDelayed; override;
    procedure SetVisible(Value: boolean); override;
  private
    fActOpenFile: TAction;
    fActSelConf: TAction;
    fProject: TCENativeProject;
    fFileNode, fConfNode: TTreeNode;
    fImpsNode, fInclNode: TTreeNode;
    fXtraNode: TTreeNode;
    fLastFileOrFolder: string;
    procedure actUpdate(sender: TObject);
    procedure TreeDblClick(sender: TObject);
    procedure actOpenFileExecute(sender: TObject);
    //
    procedure projNew(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
  protected
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

uses
  ce_symstring;

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
  //
  AssignPng(btnAddFile, 'document_add');
  AssignPng(btnRemFile, 'document_delete');
  AssignPng(btnAddFold, 'folder_add');
  AssignPng(btnRemFold, 'folder_delete');
  //
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
end;

destructor TCEProjectInspectWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEProjectInspectWidget.SetVisible(Value: boolean);
begin
  inherited;
  if Value then updateImperative;
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
procedure TCEProjectInspectWidget.projNew(aProject: ICECommonProject);
begin
  fProject := nil;
  enabled := false;
  fLastFileOrFolder := '';
  if aProject.getFormat <> pfNative then
    exit;
  enabled := true;
  //
  fProject := TCENativeProject(aProject.getProject);
  if Visible then updateImperative;
end;

procedure TCEProjectInspectWidget.projClosing(aProject: ICECommonProject);
begin
  if fProject.isNil then exit;
  if fProject <> aProject.getProject then
    exit;
  fProject := nil;
  fLastFileOrFolder := '';
  enabled := false;
  updateImperative;
end;

procedure TCEProjectInspectWidget.projFocused(aProject: ICECommonProject);
begin
  fProject := nil;
  enabled := false;
  fLastFileOrFolder := '';
  if aProject.getFormat <> pfNative then
    exit;
  enabled := true;
  //
  fProject := TCENativeProject(aProject.getProject);
  if Visible then beginDelayedUpdate;
end;

procedure TCEProjectInspectWidget.projChanged(aProject: ICECommonProject);
begin
  if fProject.isNil then exit;
  if fProject <> aProject.getProject then
    exit;
  if Visible then beginDelayedUpdate;
end;

procedure TCEProjectInspectWidget.projCompiling(aProject: ICECommonProject);
begin
end;
{$ENDREGION}

{$REGION Inspector things -------------------------------------------------------}
procedure TCEProjectInspectWidget.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreeDblClick(nil);
end;

procedure TCEProjectInspectWidget.TreeSelectionChanged(Sender: TObject);
begin
  actUpdate(sender);
  if fProject.isNil or Tree.Selected.isNil then
    exit;
  if (Tree.Selected.Parent = fFileNode) then
    fLastFileOrFolder := expandFilenameEx(fProject.basePath,tree.Selected.Text)
  else
    fLastFileOrFolder := tree.Selected.Text;
end;

procedure TCEProjectInspectWidget.TreeDblClick(sender: TObject);
var
  fname: string;
  i: NativeInt;
begin
  if fProject.isNil or Tree.Selected.isNil then
    exit;
  //
  if (Tree.Selected.Parent = fFileNode) or (Tree.Selected.Parent = fXtraNode) then
  begin
    fname := Tree.Selected.Text;
    i := fProject.Sources.IndexOf(fname);
    if i > -1 then
      fname := fProject.sourceAbsolute(i);
    if isEditable(ExtractFileExt(fname)) and fileExists(fname) then
      getMultiDocHandler.openDocument(fname);
  end
  else if Tree.Selected.Parent = fConfNode then
  begin
    i := Tree.Selected.Index;
    fProject.ConfigurationIndex := i;
    beginDelayedUpdate;
  end;
end;

procedure TCEProjectInspectWidget.actUpdate(sender: TObject);
begin
  fActSelConf.Enabled := false;
  fActOpenFile.Enabled := false;
  if Tree.Selected.isNil then exit;
  fActSelConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActOpenFile.Enabled := Tree.Selected.Parent = fFileNode;
end;

procedure TCEProjectInspectWidget.btnAddFileClick(Sender: TObject);
begin
  if fProject.isNil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    if FileExists(fLastFileOrFolder) then
      InitialDir := ExtractFilePath(fLastFileOrFolder)
    else if DirectoryExists(fLastFileOrFolder) then
      InitialDir := fLastFileOrFolder;
    filter := DdiagFilter;
    if execute then
    begin
      fProject.beginUpdate;
      fProject.addSource(filename);
      fProject.endUpdate;
    end;
  finally
    free;
  end;
end;

procedure TCEProjectInspectWidget.btnAddFoldClick(Sender: TObject);
var
  dir, fname: string;
  lst: TStringList;
  i: NativeInt;
begin
  if fProject.isNil then exit;
  //
  dir := '';
  if FileExists(fLastFileOrFolder) then
    dir := extractFilePath(fLastFileOrFolder)
  else if DirectoryExists(fLastFileOrFolder) then
    dir := fLastFileOrFolder
  else if fileExists(fProject.fileName) then
    dir := extractFilePath(fProject.fileName);
  if selectDirectory('sources', dir, dir, true, 0) then
  begin
    fProject.beginUpdate;
    lst := TStringList.Create;
    try
      listFiles(lst, dir, true);
      for i := 0 to lst.Count-1 do
      begin
        fname := lst.Strings[i];
        if isDlangCompilable(extractFileExt(fname)) then
          fProject.addSource(fname);
      end;
    finally
      lst.Free;
      fProject.endUpdate;
    end;
  end;
end;

procedure TCEProjectInspectWidget.btnRemFoldClick(Sender: TObject);
var
  dir, fname: string;
  i: Integer;
begin
  if fProject.isNil or Tree.Selected.isNil then
    exit;
  if Tree.Selected.Parent <> fFileNode then exit;
  //
  fname := Tree.Selected.Text;
  i := fProject.Sources.IndexOf(fname);
  if i = -1 then exit;
  fname := fProject.sourceAbsolute(i);
  dir := extractFilePath(fname);
  if not DirectoryExists(dir) then exit;
  //
  fProject.beginUpdate;
  for i:= fProject.Sources.Count-1 downto 0 do
    if extractFilePath(fProject.sourceAbsolute(i)) = dir then
      fProject.Sources.Delete(i);
  fProject.endUpdate;
end;

procedure TCEProjectInspectWidget.btnRemFileClick(Sender: TObject);
var
  fname: string;
  i: NativeInt;
begin
  if fProject.isNil or Tree.Selected.isNil then
    exit;
  //
  if Tree.Selected.Parent = fFileNode then
  begin
    fname := Tree.Selected.Text;
    i := fProject.Sources.IndexOf(fname);
    if i > -1 then
    begin
      fProject.beginUpdate;
      fProject.Sources.Delete(i);
      fProject.endUpdate;
    end;
  end;
end;

procedure TCEProjectInspectWidget.FormDropFiles(Sender: TObject; const FileNames: array of String);
procedure addFile(const aFilename: string);
var
  ext: string;
begin
  ext := ExtractFileExt(aFilename);
  if not isDlangCompilable(ext) then
    exit;
  fProject.addSource(aFilename);
  if isEditable(ext) then
    getMultiDocHandler.openDocument(aFilename);
end;
var
  fname, direntry: string;
  lst: TStringList;
begin
  if fProject.isNil then exit;
  lst := TStringList.Create;
  fProject.beginUpdate;
  try for fname in Filenames do
    if FileExists(fname) then
      addFile(fname)
    else if DirectoryExists(fname) then
    begin
      lst.Clear;
      listFiles(lst, fname, true);
      for direntry in lst do
        addFile(dirEntry);
    end;
  finally
    fProject.endUpdate;
    lst.Free;
  end;
end;

procedure TCEProjectInspectWidget.updateDelayed;
begin
  updateImperative;
end;

procedure TCEProjectInspectWidget.updateImperative;
var
  src, fold, conf, str: string;
  lst: TStringList;
  itm: TTreeNode;
  i: NativeInt;
begin
  fConfNode.DeleteChildren;
  fFileNode.DeleteChildren;
  fImpsNode.DeleteChildren;
  fInclNode.DeleteChildren;
  fXtraNode.DeleteChildren;
  //
  if fProject.isNil then
    exit;
  //
  Tree.BeginUpdate;
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
    if i = fProject.ConfigurationIndex then
    begin
      itm.ImageIndex := 7;
      itm.SelectedIndex:= 7;
    end
    else
    begin
      itm.ImageIndex := 3;
      itm.SelectedIndex:= 3;
    end;
  end;
  // display Imports (-J)
  for str in FProject.currentConfiguration.pathsOptions.importStringPaths do
  begin
    if str.isEmpty then
      continue;
    fold := expandFilenameEx(fProject.basePath, str);
    fold := symbolExpander.get(fold);
    itm := Tree.Items.AddChild(fImpsNode, fold);
    itm.ImageIndex := 5;
    itm.SelectedIndex := 5;
  end;
  fImpsNode.Collapse(false);
  // display Includes (-I)
  for str in FProject.currentConfiguration.pathsOptions.importModulePaths do
  begin
    if str.isEmpty then
      continue;
    fold := expandFilenameEx(fProject.basePath, str);
    fold := symbolExpander.get(fold);
    itm := Tree.Items.AddChild(fInclNode, fold);
    itm.ImageIndex := 5;
    itm.SelectedIndex := 5;
  end;
  fInclNode.Collapse(false);
  // display extra sources (external .lib, *.a, *.d)
  for str in FProject.currentConfiguration.pathsOptions.extraSources do
  begin
    if str.isEmpty then
      continue;
    src := expandFilenameEx(fProject.basePath, str);
    src := symbolExpander.get(src);
    lst := TStringList.Create;
    try
      if listAsteriskPath(src, lst) then for src in lst do
      begin
        itm := Tree.Items.AddChild(fXtraNode, src);
        itm.ImageIndex := 2;
        itm.SelectedIndex := 2;
      end else
      begin
        itm := Tree.Items.AddChild(fXtraNode, src);
        itm.ImageIndex := 2;
        itm.SelectedIndex := 2;
      end;
    finally
      lst.Free;
    end;
  end;
  fXtraNode.Collapse(false);
  Tree.EndUpdate;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
