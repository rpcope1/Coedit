unit ce_projconf;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, RTTICtrls, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus, Buttons, rttiutils, typinfo,
  PropEdits, ObjectInspector, ce_dmdwrap, ce_project, ce_widget, ce_interfaces,
  ce_observer;

type

  { TCEProjectConfigurationWidget }

  TCEProjectConfigurationWidget = class(TCEWidget, ICEProjectObserver)
    btnSyncEdit: TSpeedButton;
    imgList: TImageList;
    Panel2: TPanel;
    selConf: TComboBox;
    Panel1: TPanel;
    btnAddConf: TSpeedButton;
    btnDelConf: TSpeedButton;
    btnCloneConf: TSpeedButton;
    Splitter1: TSplitter;
    inspector: TTIPropertyGrid;
    Tree: TTreeView;
    procedure btnAddConfClick(Sender: TObject);
    procedure btnDelConfClick(Sender: TObject);
    procedure btnCloneCurrClick(Sender: TObject);
    procedure btnSyncEditClick(Sender: TObject);
    procedure inspectorModified(Sender: TObject);
    procedure selConfChange(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure GridFilter(Sender: TObject; aEditor: TPropertyEditor;var aShow: boolean);
  private
    fProj: TCEProject;
    fSyncroMode: boolean;
    fSyncroPropValue: string;
    function getGridTarget: TPersistent;
    procedure setSyncroMode(aValue: boolean);
    function syncroSetPropAsString(const ASection, Item, Default: string): string;
    procedure syncroGetPropAsString(const ASection, Item, Value: string);
    property syncroMode: boolean read fSyncroMode write setSyncroMode;
    //
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
  protected
    procedure updateImperative; override;
    procedure SetVisible(Value: boolean); override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEProjectConfigurationWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('cog_add');
    btnAddConf.Glyph.Assign(png);
    png.LoadFromLazarusResource('cog_delete');
    btnDelConf.Glyph.Assign(png);
    png.LoadFromLazarusResource('cog_go');
    btnCloneConf.Glyph.Assign(png);
    png.LoadFromLazarusResource('link_break');
    btnSyncEdit.Glyph.Assign(png);
  finally
    png.Free;
  end;
  Tree.Selected := Tree.Items.GetLastNode;
  inspector.OnEditorFilter := @GridFilter;
  inspector.CheckboxForBoolean := true;
  //TODO-cfeature: project inspector synchro-mode for dialog-based editors
  // currently the event OnModified is only called for simple properties.
  // and it misses in ObjectINspector.pp TOICustomPropertyGrid.DoCallEdit();
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCEProjectConfigurationWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEProjectConfigurationWidget.SetVisible(Value: boolean);
begin
  inherited;
  if Visible then updateImperative;
end;

{$ENDREGION --------------------------------------------------------------------}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEProjectConfigurationWidget.projNew(aProject: TCEProject);
begin
  beginImperativeUpdate;
  fProj := aProject;
  if Visible then updateImperative;
  syncroMode := false;
end;

procedure TCEProjectConfigurationWidget.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then
    exit;
  inspector.TIObject := nil;
  inspector.ItemIndex := -1;
  self.selConf.Clear;
  syncroMode := false;
  fProj := nil;
end;

procedure TCEProjectConfigurationWidget.projChanged(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  fProj := aProject;
  if Visible then updateImperative;
end;

procedure TCEProjectConfigurationWidget.projFocused(aProject: TCEProject);
begin
  fProj := aProject;
  if Visible then updateImperative;
end;

procedure TCEProjectConfigurationWidget.projCompiling(aProject: TCEProject);
begin
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION config. things --------------------------------------------------------}
procedure TCEProjectConfigurationWidget.selConfChange(Sender: TObject);
begin
  if fProj = nil then exit;
  if Updating then exit;
  if selConf.ItemIndex = -1 then exit;
  //
  beginImperativeUpdate;
  fProj.ConfigurationIndex := selConf.ItemIndex;
  endImperativeUpdate;
end;

procedure TCEProjectConfigurationWidget.TreeChange(Sender: TObject;
  Node: TTreeNode);
begin
  inspector.TIObject := getGridTarget;
end;

procedure TCEProjectConfigurationWidget.setSyncroMode(aValue: boolean);
var
  png: TPortableNetworkGraphic;
begin
  if fSyncroMode = aValue then exit;
  //
  fSyncroMode := aValue;
  png := TPortableNetworkGraphic.Create;
  try
    if fSyncroMode then png.LoadFromLazarusResource('link')
    else png.LoadFromLazarusResource('link_break');
    btnSyncEdit.Glyph.Assign(png);
  finally
    png.Free;
  end;
end;

function TCEProjectConfigurationWidget.syncroSetPropAsString(const ASection, Item, Default: string): string;
begin
  result := fSyncroPropValue;
end;

procedure TCEProjectConfigurationWidget.syncroGetPropAsString(const ASection, Item, Value: string);
begin
 fSyncroPropValue := Value;
end;

procedure TCEProjectConfigurationWidget.inspectorModified(Sender: TObject);
var
  propstr: string;
  src_list, trg_list: rttiutils.TPropInfoList;
  src_prop, trg_prop: PPropInfo;
  storage: rttiutils.TPropsStorage;
  trg_obj: TPersistent;
  i: Integer;
begin
  if fProj = nil then exit;
  if not fSyncroMode then exit;
  if inspector.TIObject = nil then exit;
  if inspector.ItemIndex = -1 then exit;
  //
  storage := nil;
  src_prop:= nil;
  trg_prop:= nil;
  trg_obj := nil;
  propstr := inspector.PropertyPath(inspector.ItemIndex);
  storage := rttiutils.TPropsStorage.Create;
  storage.OnReadString := @syncroSetPropAsString;
  storage.OnWriteString := @syncroGetPropAsString;
  src_list:= rttiutils.TPropInfoList.Create(getGridTarget, tkAny);
  fProj.beginUpdate;
  try
    src_prop := src_list.Find(propstr);
    if src_prop = nil then exit;
    storage.AObject := getGridTarget;
    storage.StoreAnyProperty(src_prop);
    for i:= 0 to fProj.OptionsCollection.Count-1 do
    begin
      // skip current config
      if i = fProj.ConfigurationIndex then continue;
      // find target persistent
      if inspector.TIObject = fProj.currentConfiguration.messagesOptions then
        trg_obj := fProj.configuration[i].messagesOptions else
      if inspector.TIObject = fProj.currentConfiguration.debugingOptions then
        trg_obj := fProj.configuration[i].debugingOptions else
      if inspector.TIObject = fProj.currentConfiguration.documentationOptions then
        trg_obj := fProj.configuration[i].documentationOptions else
      if inspector.TIObject = fProj.currentConfiguration.outputOptions then
        trg_obj := fProj.configuration[i].outputOptions else
      if inspector.TIObject = fProj.currentConfiguration.otherOptions then
        trg_obj := fProj.configuration[i].otherOptions else
      if inspector.TIObject = fProj.currentConfiguration.pathsOptions then
         trg_obj := fProj.configuration[i].pathsOptions else
      if inspector.TIObject = fProj.currentConfiguration.preBuildProcess then
        trg_obj := fProj.configuration[i].preBuildProcess else
      if inspector.TIObject = fProj.currentConfiguration.postBuildProcess then
        trg_obj := fProj.configuration[i].postBuildProcess else
      if inspector.TIObject = fProj.currentConfiguration.runOptions then
         trg_obj := fProj.configuration[i].runOptions
      else continue;
      // find target property
      storage.AObject := trg_obj;
      trg_list := rttiutils.TPropInfoList.Create(trg_obj, tkAny);
      try
        trg_prop := trg_list.Find(propstr);
        if trg_prop <> nil then
          storage.LoadAnyProperty(trg_prop);
      finally
        trg_list.Free;
        trg_prop := nil;
      end;
    end;
  finally
    storage.free;
    src_list.free;
    fProj.endUpdate;
    fSyncroPropValue := '';
  end;
end;

procedure TCEProjectConfigurationWidget.btnAddConfClick(Sender: TObject);
var
  nme: string;
  cfg: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  nme := '';
  beginImperativeUpdate;
  cfg := fProj.addConfiguration;
  // note: Cancel is actually related to the conf. name not to the add operation.
  if InputQuery('Configuration name', '', nme) then cfg.name := nme;
  fProj.ConfigurationIndex := cfg.Index;
  endImperativeUpdate;
end;

procedure TCEProjectConfigurationWidget.btnDelConfClick(Sender: TObject);
begin
  if fProj = nil then exit;
  if fProj.OptionsCollection.Count = 1 then exit;
  //
  beginImperativeUpdate;
  inspector.TIObject := nil;
  inspector.Clear;
  Invalidate;
  fProj.OptionsCollection.Delete(selConf.ItemIndex);
  fProj.ConfigurationIndex := 0;
  endImperativeUpdate;
end;

procedure TCEProjectConfigurationWidget.btnCloneCurrClick(Sender: TObject);
var
  nme: string;
  trg,src: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  nme := '';
  beginImperativeUpdate;
  fProj.beginUpdate;
  src := fProj.currentConfiguration;
  trg := fProj.addConfiguration;
  trg.assign(src);
  if InputQuery('Configuration name', '', nme) then trg.name := nme;
  fProj.ConfigurationIndex := trg.Index;
  fProj.endUpdate;
  endImperativeUpdate;
end;

procedure TCEProjectConfigurationWidget.btnSyncEditClick(Sender: TObject);
begin
  if fProj = nil then exit;
  syncroMode := not syncroMode;
end;

procedure TCEProjectConfigurationWidget.GridFilter(Sender: TObject; aEditor: TPropertyEditor;
  var aShow: boolean);
begin
  if fProj = nil then exit;

  // filter TComponent things.
  if getGridTarget = fProj then
  begin
    if aEditor.GetName = 'Name' then
      aShow := false
    else if aEditor.GetName = 'Tag' then
      aShow := false
    else  if aEditor.ClassType = TCollectionPropertyEditor then
      aShow := false;
  end;
  // deprecated field
  if getGridTarget = fProj.currentConfiguration.pathsOptions  then
  begin
    if aEditor.GetName = 'Sources' then
      aShow := false
    else if aEditor.GetName = 'includes' then
      aShow := false
    else if aEditor.GetName = 'imports' then
      aShow := false;
  end;
  if getGridTarget = fProj.currentConfiguration.outputOptions  then
    if aEditor.GetName = 'noBoundsCheck' then
      aShow := false;
  if getGridTarget = fProj.currentConfiguration.debugingOptions then
  begin
    if aEditor.GetName = 'addCInformations' then
      aShow := false
    else if aEditor.GetName = 'addDInformations' then
      aShow := false;
  end;
end;

function TCEProjectConfigurationWidget.getGridTarget: TPersistent;
begin
  if fProj = nil then exit(nil);
  if fProj.ConfigurationIndex = -1 then exit(nil);
  if Tree.Selected = nil then exit(nil);
  // Warning: TTreeNode.StateIndex is usually made for the images...it's not a tag
  case Tree.Selected.StateIndex of
    1: exit( fProj );
    2: exit( fProj.currentConfiguration.messagesOptions );
    3: exit( fProj.currentConfiguration.debugingOptions );
    4: exit( fProj.currentConfiguration.documentationOptions );
    5: exit( fProj.currentConfiguration.outputOptions );
    6: exit( fProj.currentConfiguration.otherOptions );
    7: exit( fProj.currentConfiguration.pathsOptions );
    8: exit( fProj.currentConfiguration.preBuildProcess );
    9: exit( fProj.currentConfiguration.postBuildProcess );
    10:exit( fProj.currentConfiguration.runOptions );
    11:exit( fProj.currentConfiguration );
    else result := nil;
  end;
end;

procedure TCEProjectConfigurationWidget.updateImperative;
var
  i: NativeInt;
begin
  selConf.ItemIndex:= -1;
  selConf.Clear;
  if fProj = nil then exit;
  //
  for i:= 0 to fProj.OptionsCollection.Count-1 do
    selConf.Items.Add(fProj.configuration[i].name);
  selConf.ItemIndex := fProj.ConfigurationIndex;
  inspector.TIObject := getGridTarget;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
