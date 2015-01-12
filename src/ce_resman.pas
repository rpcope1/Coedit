unit ce_resman;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, Buttons, StdCtrls, ce_widget, ce_writableComponent, fpjson,
  ce_interfaces, ce_project, ce_synmemo, ce_common, process, fpjsonrtti, fpjsondataset;

type


  TResourceType = (aFile, aFolder);
  TResourceFormat = (bytes, utf8, base16, base64, z85, e7F);

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
  TResources = class(TWritableComponent)
  private
    fResources: TCollection;
    procedure setResources(aValue: TCollection);
    function getResource(index: Integer): TResourceItem;
  published
    property resources: TCollection read fResources write setResources;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    // overides the component streaming methods to use JSON instead of lfm
    procedure saveToFile(const aFilename: string); override;
    procedure loadFromFile(const aFilename: string); override;
    property resource[index: Integer]: TResourceItem read getResource; default;
  end;

  { TCEResmanWidget }
  TCEResmanWidget = class(TCEWidget, ICEProjectObserver, ICEMultiDocObserver)
    BtnAddItem: TBitBtn;
    btnRemItem: TBitBtn;
    lstRes: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    inspector: TTIPropertyGrid;
    Splitter1: TSplitter;
    procedure BtnAddItemClick(Sender: TObject);
    procedure btnRemItemClick(Sender: TObject);
    procedure lstResSelectionChange(Sender: TObject; User: boolean);
    procedure inspectorModified(Sender: TObject);
  private
    fProj: TCEProject;
    fDoc: TCESynMemo;
    fResourceItems: TResources;
    fLogMessager: TCELogMessageSubject;
    // try to load the json resource script for the current doc
    procedure loadDocResJson;
    // try to save the json resource script for the current doc
    procedure saveDocResJson;
    procedure clearInspector;
    procedure rebuildResList;
    procedure updateResList;
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
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

{$REGION TResources ------------------------------------------------------------}
constructor TResources.Create(aOwner: TCOmponent);
begin
  inherited;
  fResources := TCollection.Create(TResourceItem);
end;

destructor TResources.destroy;
begin
  fResources.Free;
  inherited;
end;

function TResources.getResource(index: Integer): TResourceItem;
begin
  result := TResourceItem(fResources.Items[index]);
end;

procedure TResources.saveToFile(const aFilename: string);
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

procedure TResources.loadFromFile(const aFilename: string);
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

procedure TResources.setResources(aValue: TCollection);
begin
  fResources.Assign(aValue);
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEResmanWidget.create(aOwner: TComponent);
var
  png: TPortableNetworkGraphic;
begin
  inherited;
  fLogMessager := TCELogMessageSubject.create;
  fResourceItems := TResources.create(self);
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

destructor TCEResmanWidget.destroy;
begin
  fLogMessager.Free;
  inherited;
end;
{$ENDREGION}

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
  inspector.TIObject := nil;
  inspector.ItemIndex := -1;
  fResourceItems.resources.Clear;
  rebuildResList;
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
  rebuildResList;
end;

procedure TCEResmanWidget.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  loadDocResJson;
end;
{$ENDREGION}

{$REGION Resources things -----------------------------------------------------}
procedure TCEResmanWidget.lstResSelectionChange(Sender: TObject; User: boolean);
begin
  if lstRes.Count = 0 then exit;
  if lstRes.ItemIndex = -1 then exit;
  //
  inspector.TIObject := fResourceItems[lstRes.ItemIndex];
end;

procedure TCEResmanWidget.inspectorModified(Sender: TObject);
begin
  if inspector.ItemIndex = -1 then
    exit;
  if inspector.Rows[inspector.ItemIndex].Name = 'identifier' then
    updateResList;
  saveDocResJson;
end;

procedure TCEResmanWidget.BtnAddItemClick(Sender: TObject);
var
  item: TResourceItem;
begin
  item := TResourceItem(fResourceItems.resources.Add);
  item.identifier := format('<id_for_item %d>' ,[item.ID]);
  rebuildResList;
  saveDocResJson;
end;

procedure TCEResmanWidget.btnRemItemClick(Sender: TObject);
begin
  if lstRes.ItemIndex = -1 then
    exit;
  inspector.TIObject := nil;
  inspector.ItemIndex := -1;
  fResourceItems.resources.Delete(lstRes.ItemIndex);
  rebuildResList;
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
  inspector.TIObject := nil;
  inspector.ItemIndex := -1;
  fResourceItems.resources.Clear;
  if fileExists(fname) then
    fResourceItems.loadFromFile(fname);
  rebuildResList;
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
  if fResourceItems.resources.Count = 0 then exit;
  //
  fResourceItems.saveToFile(fname);
end;

procedure TCEResmanWidget.genResources;
var
  proc: TProcess;
  str: TStringList;
  fname, msg: string;
  i: Integer;
begin
  if fProj = nil then exit;
  if not exeInSysPath('resman' + exeExt) then exit;
  //
  proc := TProcess.Create(nil);
  str := TStringList.Create;
  try
    proc.Executable:= 'resman' + exeExt;
    proc.Options := [poUsePipes, poStderrToOutPut];
    proc.ShowWindow := swoHIDE;
    proc.Parameters.Add('-v');
    for i := 0 to fProj.Sources.Count-1 do
    begin
      fname := fProj.getAbsoluteSourceName(i);
      fname := stripFileExt(fname) + '.resman';
      if not FileExists(fname) then continue;
      proc.Parameters.Add(fname);
    end;
      msg := 'generating the resources...';
      subjLmFromString(fLogMessager, msg, fProj, amcProj, amkInf);
      proc.Execute;
      while proc.Running do begin
        processOutputToStrings(proc, str);
        for msg in str do
          subjLmFromString(fLogMessager, msg, fProj, amcProj, amkAuto);
      end;
  finally
    proc.Free;
    str.Free;
  end;
end;

procedure TCEResmanWidget.clearInspector;
begin
  inspector.TIObject := nil;
  inspector.ItemIndex := -1;
end;

procedure TCEResmanWidget.rebuildResList;
var
  i: Integer;
begin
  clearInspector;
  lstRes.Items.Clear;
  //
  for i:= 0 to fResourceItems.resources.Count-1 do
    lstRes.AddItem(fResourceItems[i].identifier, nil);
  if lstRes.Count > 0 then
    lstRes.ItemIndex := 0;
end;

procedure TCEResmanWidget.updateResList;
var
  i: Integer;
begin
  for i:= 0 to fResourceItems.resources.Count-1 do
    lstRes.Items.Strings[i] := fResourceItems[i].identifier;
end;
{$ENDREGION}

end.

