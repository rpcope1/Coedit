unit ce_staticexplorer;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, ExtCtrls, Menus,
  ComCtrls, ce_widget, jsonparser, fpjson, process, actnlist, Buttons, Clipbrd,
  ce_common, ce_project, ce_observer, ce_synmemo, ce_interfaces;

type

  { TCEStaticExplorerWidget }
  TCEStaticExplorerWidget = class(TCEWidget, ICEProjectObserver, ICEMultiDocObserver)
    btnRefresh: TBitBtn;
    imgList: TImageList;
    Panel1: TPanel;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnRefreshClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeFilterEdit1AfterFilter(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
  private
    fDmdProc: TCheckedAsyncProcess;
    fLogMessager: TCELogMessageSubject;
    fActCopyIdent: TAction;
    fActRefresh: TAction;
    fActRefreshOnChange: TAction;
    fActRefreshOnFocus: TAction;
    fActAutoRefresh: TAction;
    fActSelectInSource: TAction;
    fDoc: TCESynMemo;
    fProj: TCEProject;
    fAutoRefresh: boolean;
    fRefreshOnChange: boolean;
    fRefreshOnFocus: boolean;
    fJsonFname: string;
    ndAlias, ndClass, ndEnum, ndFunc: TTreeNode;
    ndImp, ndIntf, ndMix, ndStruct, ndTmp, ndVar: TTreeNode;
    procedure TreeDblClick(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAutoRefreshExecute(Sender: TObject);
    procedure actRefreshOnChangeExecute(Sender: TObject);
    procedure actRefreshOnFocusExecute(Sender: TObject);
    procedure actCopyIdentExecute(Sender: TObject);
    procedure updateVisibleCat;
    //
    procedure produceJsonInfo;
    procedure jsonInfoProduced(sender: TObject);
    //
    procedure optget_AutoRefresh(aWriter: TWriter);
    procedure optset_AutoRefresh(aReader: TReader);
    procedure optget_RefreshOnChange(aWriter: TWriter);
    procedure optset_RefreshOnChange(aReader: TReader);
    procedure optget_RefreshOnFocus(aWriter: TWriter);
    procedure optset_RefreshOnFocus(aReader: TReader);
  protected
    procedure UpdateByDelay; override;
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
    property refreshOnChange: boolean read fRefreshOnChange write fRefreshOnChange;
    property refreshOnFocus: boolean read fRefreshOnFocus write fRefreshOnFocus;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    //
    procedure sesoptDeclareProperties(aFiler: TFiler); override;
  end;

implementation
{$R *.lfm}

uses ce_libman, ce_symstring;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEStaticExplorerWidget.create(aOwner: TComponent);
begin
  fLogMessager := TCELogMessageSubject.create;
  fAutoRefresh := false;
  fRefreshOnFocus := true;
  fRefreshOnChange := false;

  fActCopyIdent := TAction.Create(self);
  fActCopyIdent.OnExecute:=@actCopyIdentExecute;
  fActCopyIdent.Caption := 'Copy identifer';
  fActRefresh := TAction.Create(self);
  fActRefresh.OnExecute := @actRefreshExecute;
  fActRefresh.Caption := 'Refresh';
  fActAutoRefresh := TAction.Create(self);
  fActAutoRefresh.OnExecute := @actAutoRefreshExecute;
  fActAutoRefresh.Caption := 'Auto-refresh';
  fActAutoRefresh.AutoCheck := true;
  fActAutoRefresh.Checked := fAutoRefresh;
  fActRefreshOnChange := TAction.Create(self);
  fActRefreshOnChange.OnExecute := @actRefreshOnChangeExecute;
  fActRefreshOnChange.Caption := 'Refresh on change';
  fActRefreshOnChange.AutoCheck := true;
  fActRefreshOnChange.Checked := fRefreshOnChange;
  fActRefreshOnFocus := TAction.Create(self);
  fActRefreshOnFocus.OnExecute := @actRefreshOnFocusExecute;
  fActRefreshOnFocus.Caption := 'Refresh on focused';
  fActRefreshOnFocus.AutoCheck := true;
  fActRefreshOnFocus.Checked := fRefreshOnFocus;
  fActSelectInSource := TAction.Create(self);
  fActSelectInSource.OnExecute := @TreeDblClick;
  fActSelectInSource.Caption := 'Select in source';
  //
  inherited;
  //
  ndAlias   := Tree.Items[0];
  ndClass   := Tree.Items[1];
  ndEnum    := Tree.Items[2];
  ndFunc    := Tree.Items[3];
  ndImp     := Tree.Items[4];
  ndIntf    := Tree.Items[5];
  ndMix     := Tree.Items[6];
  ndStruct  := Tree.Items[7];
  ndTmp     := Tree.Items[8];
  ndVar     := Tree.Items[9];
  //
  Tree.OnDblClick := @TreeDblClick;
  Tree.PopupMenu := contextMenu;
  //
  EntitiesConnector.addObserver(self);
  EntitiesConnector.endUpdate;
end;

destructor TCEStaticExplorerWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  //
  killProcess(fDmdProc);
  fLogMessager.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICESessionOptionsObserver ---------------------------------------------}
procedure TCEStaticExplorerWidget.optget_AutoRefresh(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fAutoRefresh);
end;

procedure TCEStaticExplorerWidget.optset_AutoRefresh(aReader: TReader);
begin
  fAutoRefresh := aReader.ReadBoolean;
  fActAutoRefresh.Checked := fAutoRefresh;
end;

procedure TCEStaticExplorerWidget.optget_RefreshOnChange(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fRefreshOnChange);
end;

procedure TCEStaticExplorerWidget.optset_RefreshOnChange(aReader: TReader);
begin
  fRefreshOnChange := aReader.ReadBoolean;
  fActRefreshOnChange.Checked := fRefreshOnChange;
end;

procedure TCEStaticExplorerWidget.optget_RefreshOnFocus(aWriter: TWriter);
begin
  aWriter.WriteBoolean(fRefreshOnFocus);
end;

procedure TCEStaticExplorerWidget.optset_RefreshOnFocus(aReader: TReader);
begin
  fRefreshOnFocus := aReader.ReadBoolean;
  fActRefreshOnFocus.Checked := fRefreshOnFocus;
end;

procedure TCEStaticExplorerWidget.sesoptDeclareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_AutoRefresh', @optset_AutoRefresh, @optget_AutoRefresh, true);
  aFiler.DefineProperty(Name + '_RefreshOnChange', @optset_RefreshOnChange, @optget_RefreshOnChange, true);
  aFiler.DefineProperty(Name + '_RefreshOnFocus', @optset_RefreshOnFocus, @optget_RefreshOnFocus, true);
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEStaticExplorerWidget.contextName: string;
begin
  result := 'Static explorer';
end;

function TCEStaticExplorerWidget.contextActionCount: integer;
begin
  result := 6;
end;

function TCEStaticExplorerWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActSelectInSource);
    1: exit(fActCopyIdent);
    2: exit(fActRefresh);
    3: exit(fActAutoRefresh);
    4: exit(fActRefreshOnChange);
    5: exit(fActRefreshOnFocus);
    else result := nil;
  end;
end;

procedure TCEStaticExplorerWidget.actRefreshExecute(Sender: TObject);
begin
  if Updating then exit;
  produceJsonInfo;
end;

procedure TCEStaticExplorerWidget.actAutoRefreshExecute(Sender: TObject);
begin
  autoRefresh := not autoRefresh;
end;

procedure TCEStaticExplorerWidget.actRefreshOnChangeExecute(Sender: TObject);
begin
  refreshOnChange := not refreshOnChange;
end;

procedure TCEStaticExplorerWidget.actRefreshOnFocusExecute(Sender: TObject);
begin
  refreshOnFocus := not refreshOnFocus;
end;

procedure TCEStaticExplorerWidget.actCopyIdentExecute(Sender: TObject);
begin
  if Tree.Selected = nil then exit;
  Clipboard.AsText:= Tree.Selected.Text;
end;

{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEStaticExplorerWidget.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  beginUpdateByDelay;
end;

procedure TCEStaticExplorerWidget.docClosing(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
  beginUpdateByDelay;
end;

procedure TCEStaticExplorerWidget.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  if fAutoRefresh then beginUpdateByDelay
  else if fRefreshOnFocus then produceJsonInfo;
end;

procedure TCEStaticExplorerWidget.docChanged(aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  if fAutoRefresh then
    beginUpdateByDelay
  else if fRefreshOnChange then
    produceJsonInfo;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEStaticExplorerWidget.projNew(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCEStaticExplorerWidget.projClosing(aProject: TCEProject);
begin
  if fProj <> aProject then
    exit;
  fProj := nil;
end;

procedure TCEStaticExplorerWidget.projFocused(aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCEStaticExplorerWidget.projChanged(aProject: TCEProject);
begin
end;
{$ENDREGION}

{$REGION Symbol-tree things ----------------------------------------------------}
procedure TCEStaticExplorerWidget.UpdateByDelay;
begin
  if not fAutoRefresh then exit;
  produceJsonInfo;
end;

procedure TCEStaticExplorerWidget.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (node.Data <> nil) then
    Dispose(PInt64(node.Data));
end;

procedure TCEStaticExplorerWidget.btnRefreshClick(Sender: TObject);
begin
  fActRefresh.Execute;
end;

procedure TCEStaticExplorerWidget.updateVisibleCat;
begin
  ndAlias.Visible := ndAlias.Count > 0;
  ndClass.Visible := ndClass.Count > 0;
  ndEnum.Visible := ndEnum.Count > 0;
  ndFunc.Visible := ndFunc.Count > 0;
  ndImp.Visible := ndImp.Count > 0;
  ndIntf.Visible := ndIntf.Count > 0;
  ndMix.Visible := ndMix.Count > 0;
  ndStruct.Visible := ndStruct.Count > 0;
  ndTmp.Visible := ndTmp.Count > 0;
  ndVar.Visible := ndVar.Count > 0;
end;

procedure TCEStaticExplorerWidget.TreeFilterEdit1AfterFilter(Sender: TObject);
begin
  if TreeFilterEdit1.Filter ='' then
    updateVisibleCat;
end;

procedure TCEStaticExplorerWidget.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then TreeDblClick(nil);
end;

procedure TCEStaticExplorerWidget.TreeDblClick(Sender: TObject);
var
  line: Int64;
begin
  if fDoc = nil then exit;
  if Tree.Selected = nil then exit;
  if Tree.Selected.Data = nil then exit;
  //
  line := PInt64(Tree.Selected.Data)^;
  fDoc.CaretY := line;
  fDoc.SelectLine;
end;

procedure TCEStaticExplorerWidget.produceJsonInfo;
var
  srcFname, itm: string;
  tempSrc: TStringList;
  i: Integer;
begin
  if fDoc = nil then exit;
  if fDoc.Lines.Count = 0 then exit;

  // standard process options
  killProcess(fDmdProc);
  fDmdProc := TCheckedAsyncProcess.Create(nil);
  fDmdProc.ShowWindow := swoHIDE;
  fDmdProc.Options := [];
  fDmdProc.Executable := 'dmd';
  fDmdProc.OnTerminate := @jsonInfoProduced;

  // focused source
  fJsonFname := fDoc.tempFilename + '.json';
  srcFname := fDoc.fileName;
  if not fileExists(srcFname) then begin
    tempSrc := TStringList.Create;
    try
      srcFname := fDoc.tempFilename;
      tempSrc.Assign(fDoc.Lines);
      tempSrc.SaveToFile(srcFname);
    finally
      tempSrc.Free;
    end;
  end;
  fDmdProc.Parameters.Add(srcFname);

  // other project sources, -I, -J
  if fProj <> nil then begin
    fDmdProc.CurrentDirectory := extractFilePath(fProj.fileName);
    for i := 0 to fProj.Sources.Count-1 do begin
      itm := fProj.getAbsoluteSourceName(i);
      if srcFname <> itm then fDmdProc.Parameters.Add(itm);
    end;
    for itm in fProj.currentConfiguration.pathsOptions.Includes do
      fDmdProc.Parameters.Add('-I' + symbolExpander.get(itm));
    for itm in fProj.currentConfiguration.pathsOptions.Imports do
      fDmdProc.Parameters.Add('-J' + symbolExpander.get(itm));
  end;

  //adds the libman entries
  LibMan.getLibFiles(nil, fDmdProc.Parameters);
  LibMan.getLibSources(nil, fDmdProc.Parameters);

  // option to produce the Json file.
  fDmdProc.Parameters.Add('-c');
  fDmdProc.Parameters.Add('-o-');
  fDmdProc.Parameters.Add('-X');
  fDmdProc.Parameters.Add('-Xf' + fJsonFname);

  fDmdProc.Execute;
end;

procedure TCEStaticExplorerWidget.jsonInfoProduced(sender: TObject);
var
  str: TMemoryStream;
  prs: TJsonParser;
  dat: TJsonData;
  memb: TJsonData;
  ndCat: TTreeNode;
  ln: PInt64;
  nme, knd: string;
  i: NativeInt;

  // recursively display members, without master categories.
  procedure digMembers(const srcDt: TJsonData; const srcNd: TTreeNode);
  var
    _memb: TJsonData;
    _ln: PInt64;
    _nme: string;
    _i: NativeInt;
    _nd: TTreeNode;
  begin
    _memb := srcDt.FindPath('members');
    if _memb <> nil then for _i := 0 to _memb.Count-1 do
    begin
      _ln   := new(PInt64);
      _ln^  := _memb.Items[_i].GetPath('line').AsInt64;
      _nme  := _memb.Items[_i].GetPath('name').AsString;
      _nd   := Tree.Items.AddChildObject(srcNd, _nme, _ln);
      digMembers(_memb.Items[_i], _nd);
    end;
  end;

begin
  if ndAlias = nil then exit;

  // clear the tree
  ndAlias.DeleteChildren;
  ndClass.DeleteChildren;
  ndEnum.DeleteChildren;
  ndFunc.DeleteChildren;
  ndImp.DeleteChildren;
  ndIntf.DeleteChildren;
  ndMix.DeleteChildren;
  ndStruct.DeleteChildren;
  ndTmp.DeleteChildren;
  ndVar.DeleteChildren;
  updateVisibleCat;

  if not FileExists(fJsonFname) then exit;

  // load json
  str := TMemoryStream.Create;
  try
    str.LoadFromFile(fJsonFname);
    str.Position := 0;
    prs := TJsonParser.Create(str);
    try
      dat := prs.Parse;
    finally
      prs.Free;
    end;
  finally
    str.Free;
    DeleteFile(fJsonFname);
  end;

  // update tree
  try
    memb := dat.items[0].FindPath('members');
    if memb <> nil then for i := 0 to memb.Count-1 do
    begin

      ndcat := nil;
      // categories
      ln  := new(PInt64);
      ln^ := memb.Items[i].GetPath('line').AsInt64;
      nme := memb.Items[i].GetPath('name').AsString;

      knd := memb.Items[i].GetPath('kind').AsString;
      case knd of
        'alias'     :ndCat := Tree.Items.AddChildObject(ndAlias, nme, ln);
        'class'     :ndCat := Tree.Items.AddChildObject(ndClass, nme, ln);
        'enum'      :ndCat := Tree.Items.AddChildObject(ndEnum, nme, ln);
        'function'  :ndCat := Tree.Items.AddChildObject(ndFunc, nme, ln);
        'import', 'static import':
                     ndCat := Tree.Items.AddChildObject(ndImp, nme, ln);
        'interface' :ndCat := Tree.Items.AddChildObject(ndIntf, nme, ln);
        'mixin'     :ndCat := Tree.Items.AddChildObject(ndMix, nme, ln);
        'struct'    :ndCat := Tree.Items.AddChildObject(ndStruct, nme, ln);
        'template'  :ndCat := Tree.Items.AddChildObject(ndTmp, nme, ln);
        'variable'  :ndCat := Tree.Items.AddChildObject(ndVar, nme, ln);
        else subjLmFromString(fLogMessager, 'static explorer does not handle this kind: '
          + knd, nil, amcApp, amkWarn);
      end;

      if ndCat = nil then
      begin
        {$IFDEF DEBUG}
        writeln(memb.Items[i].GetPath('kind').AsString);
        {$ENDIF}
        continue;
      end;

      ndCat.Parent.Visible := true;

      //recursive
      digMembers(memb.Items[i], ndCat);

    end;
  finally
    if dat <> nil then
    begin
      dat.Clear;
      dat.Free;
    end;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
