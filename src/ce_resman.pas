unit ce_resman;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, Buttons, StdCtrls, ce_widget, ce_writableComponent, fpjson,
  ce_interfaces, ce_project, ce_synmemo, ce_common, process, fpjsonrtti, fpjsondataset;

type


  TResourceType = (aFile, aFolder);
  TResourceFormat = (bytes, utf8, base16, base64);

  TResourceItem = class(TCollectionItem)
  private
    fResType: TResourceType;
    fIDentifier: string;
    fName: string;
    fFormat: TResourceFormat;
    fMetadata: string;
  published
    property resourceType: TResourceType read fResType write fResType stored true;
    property identifier: string read fIDentifier write fIDentifier stored true;
    property name: string read fName write fName stored true;
    property format: TResourceFormat read fFormat write fFormat stored true;
    property metadata: string read fMetadata write fMetadata stored true;
  end;

  (**
   * Represents a resource script. The resource script use the
   * JSON format for a better compatibility with the tool.
   *)
  TResourceItems = class(TWritableComponent)
  private
    fItems: TCollection;
    procedure setItems(aValue: TCollection);
  published
    property items: TCollection read fItems write setItems;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    // overides the component streaming methods to use JSON instead of lfm
    procedure saveToFile(const aFilename: string); override;
    procedure loadFromFile(const aFilename: string); override;
  end;

  { TCEResmanWidget }
  TCEResmanWidget = class(TCEWidget, ICEProjectObserver, ICEMultiDocObserver)
    BtnAddItem: TBitBtn;
    btnRemItem: TBitBtn;
    lstItems: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    propsEd: TTIPropertyGrid;
    Splitter1: TSplitter;
    procedure BtnAddItemClick(Sender: TObject);
    procedure btnRemItemClick(Sender: TObject);
    procedure lstItemsSelectionChange(Sender: TObject; User: boolean);
    procedure propsEdModified(Sender: TObject);
  private
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fResourceItems: TResourceItems;
    // try to load the json resource script for the current doc
    procedure loadDocResJson;
    // try to save the json resource script for the current doc
    procedure saveDocResJson;
    procedure refreshItemList;
    procedure updateIdentifierList;
    procedure genResources;
    //
    procedure projNew(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

{$REGION TResourceItems --------------------------------------------------------}
constructor TResourceItems.Create(aOwner: TCOmponent);
begin
  inherited;
  fItems := TCollection.Create(TResourceItem);
end;

destructor TResourceItems.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TResourceItems.saveToFile(const aFilename: string);
var
  json_streamer: TJSONStreamer;
  json_string: TJSONStringType;
  str: TMemoryStream;
begin
  fHasSaved := true;
  beforeSave;
  try
    json_streamer := TJSONStreamer.Create(nil);
    str := TMemoryStream.Create;
    try
      json_string := json_streamer.ObjectToJSONString(self);
      str.Write(json_string[1], length(json_string));
      str.SaveToFile(aFilename);
    finally
      json_streamer.Free;
      str.Free;
    end;
  except
    fHasSaved := false;
  end;
  setFilename(aFilename);
  afterSave;
end;

procedure TResourceItems.loadFromFile(const aFilename: string);
var
  json_destreamer: TJSONDeStreamer;
  json_string: TJSONStringType;
  str: TMemoryStream;
begin
  fHasLoaded := true;
  beforeLoad;
  setFilename(aFilename);
  json_destreamer := TJSONDeStreamer.Create(nil);
  str := TMemoryStream.Create;
  try
    str.LoadFromFile(aFilename);
    setLength(json_string, str.Size);
    str.Read(json_string[1], str.Size);
    json_destreamer.JSONToObject(json_string, self);
  finally
    json_destreamer.Free;
    str.Free;
  end;
  afterLoad;
end;

procedure TResourceItems.setItems(aValue: TCollection);
begin
  fItems.Assign(aValue);
end;
{$ENDREGION}

constructor TCEResmanWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fResourceItems := TResourceItems.create(self);
  //
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromLazarusResource('package_add');
    BtnAddItem.Glyph.Assign(png);
    png.LoadFromLazarusResource('package_delete');
    btnRemItem.Glyph.Assign(png);
  finally
    png.Free;
  end;
end;

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEResmanWidget.projNew(aProject: TCEProject);
begin
  fProj := aProject;
  loadDocResJson;
end;

procedure TCEResmanWidget.projChanged(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  loadDocResJson;
end;

procedure TCEResmanWidget.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  fProj := nil;
  propsEd.TIObject := nil;
  propsEd.ItemIndex := -1;
  fResourceItems.Items.Clear;
  refreshItemList;
end;

procedure TCEResmanWidget.projFocused(aProject: TCEProject);
begin
  fProj := aProject;
  loadDocResJson;
end;

procedure TCEResmanWidget.projCompiling(aProject: TCEProject);
begin
  if fProj <> aProject then exit;
  saveDocResJson;
  genResources;
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEResmanWidget.docNew(aDoc: TCESynMemo);
begin
end;

procedure TCEResmanWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
end;

procedure TCEResmanWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  //
  saveDocResJson;
  fDoc := nil;
  refreshItemList;
end;

procedure TCEResmanWidget.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  loadDocResJson;
end;
{$ENDREGION}

{$REGION Resources things -----------------------------------------------------}
procedure TCEResmanWidget.lstItemsSelectionChange(Sender: TObject; User: boolean);
begin
  if lstItems.Count = 0 then exit;
  if lstItems.ItemIndex = -1 then exit;
  //
  propsEd.TIObject := TPersistent(lstItems.Items.Objects[lstItems.ItemIndex]);
end;

procedure TCEResmanWidget.propsEdModified(Sender: TObject);
begin
  if propsEd.ItemIndex = -1 then
    exit;
  if propsEd.Rows[propsEd.ItemIndex].Name = 'identifier' then
    updateIdentifierList;
  saveDocResJson;
end;

procedure TCEResmanWidget.BtnAddItemClick(Sender: TObject);
var
  item: TResourceItem;
begin
  item := TResourceItem(fResourceItems.items.Add);
  item.identifier := format('<id_for_item %d>' ,[item.ID]);
  refreshItemList;
  saveDocResJson;
end;

procedure TCEResmanWidget.btnRemItemClick(Sender: TObject);
begin
  if lstItems.ItemIndex = -1 then
    exit;
  propsEd.TIObject := nil;
  propsEd.ItemIndex := -1;
  fResourceItems.items.Delete(lstItems.ItemIndex);
  refreshItemList;
  saveDocResJson;
end;

procedure TCEResmanWidget.loadDocResJson;
var
  fname: string;
begin
  if fDoc = nil then exit;
  if fProj = nil then exit;
  if not fProj.isProjectSource(fDoc.fileName) then exit;
  //
  fname := stripFileExt(fDoc.fileName) + '.resman';
  propsEd.TIObject := nil;
  propsEd.ItemIndex := -1;
  fResourceItems.Items.Clear;
  if fileExists(fname) then
    fResourceItems.loadFromFile(fname);
  refreshItemList;
end;

procedure TCEResmanWidget.saveDocResJson;
var
  fname: string;
begin
  if fDoc = nil then exit;
  if fProj = nil then exit;
  if not fProj.isProjectSource(fDoc.fileName) then exit;
  //
  fname := stripFileExt(fDoc.fileName) + '.resman';
  if fResourceItems.Items.Count = 0 then exit;
  //
  fResourceItems.saveToFile(fname);
end;

procedure TCEResmanWidget.genResources;
var
  proc: TProcess;
  fname: string;
  i: Integer;
begin
  if fProj = nil then exit;
  if not exeInSysPath('resman' + exeExt) then exit;
  //
  proc := Tprocess.Create(nil);
  try
    proc.Executable:= 'resman' + exeExt;
    //proc.Options := [poUsePipes, poStderrToOutPut];
    //proc.ShowWindow := swoHIDE;
    proc.Parameters.Add('-v');
    for i := 0 to fProj.Sources.Count-1 do
    begin
      fname := fProj.getAbsoluteSourceName(i);
      fname := stripFileExt(fname) + '.resman';
      if not FileExists(fname) then continue;
      proc.Parameters.Add(fname);
    end;
      proc.Execute;
      while proc.Running do;
      // output to project message...
  finally
    proc.Free;
  end;
end;

procedure TCEResmanWidget.refreshItemList;
var
  i: Integer;
  item: TResourceItem;
begin
  propsEd.TIObject := nil;
  propsEd.ItemIndex := -1;
  lstItems.Items.Clear;
  for i:= 0 to fResourceItems.items.Count-1 do
  begin
    item := TResourceItem(fResourceItems.items.Items[i]);
    lstItems.Items.AddObject(item.identifier, item);
  end;
  if lstItems.Count > 0 then
    lstItems.ItemIndex := 0;
end;

procedure TCEResmanWidget.updateIdentifierList;
var
  i: Integer;
  item: TResourceItem;
begin
  for i:= 0 to fResourceItems.items.Count-1 do
  begin
    item := TResourceItem(fResourceItems.items.Items[i]);
    lstItems.Items.Strings[i] := item.identifier;
  end;
end;
{$ENDREGION}

end.

