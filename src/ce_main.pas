unit ce_main;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, SynHighlighterLFM, Forms,
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls, process, XMLPropStorage, dynlibs, SynExportHTML,
  ce_common, ce_dmdwrap, ce_project, ce_dcd, ce_plugin, ce_synmemo, ce_widget,
  ce_messages, ce_interfaces, ce_editor, ce_projinspect, ce_projconf, ce_search,
  ce_staticexplorer, ce_miniexplorer, ce_libman, ce_libmaneditor,
  ce_observer, ce_writableComponent, ce_toolseditor, ce_procinput, ce_cdbcmd;

type

  // TODO-cfeature: options
  // TODO-cwidget: options editor

  { TCEMainForm }
  TCEMainForm = class(TForm, ICEMultiDocObserver, ICESessionOptionsObserver)
    actFileCompAndRun: TAction;
    actFileSaveAll: TAction;
    actFileClose: TAction;
    actFileAddToProj: TAction;
    actFileNewRun: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSaveAs: TAction;
    actFileSave: TAction;
    actFileCompAndRunWithArgs: TAction;
    actEdFind: TAction;
    actEdFindNext: TAction;
    actFileOpenContFold: TAction;
    actFileHtmlExport: TAction;
    actLayoutSave: TAction;
    actProjOpenContFold: TAction;
    actProjOptView: TAction;
    actProjSource: TAction;
    actProjRun: TAction;
    actProjRunWithArgs: TAction;
    actProjCompile: TAction;
    actProjCompileAndRun: TAction;
    actProjCompAndRunWithArgs: TAction;
    actProjClose: TAction;
    actProjOpts: TAction;
    actProjNew: TAction;
    actProjOpen: TAction;
    actProjSave: TAction;
    actProjSaveAs: TAction;
    actEdMacPlay: TAction;
    actEdMacStartStop: TAction;
    actEdCut: TAction;
    actEdRedo: TAction;
    actEdUndo: TAction;
    actEdPaste: TAction;
    actEdCopy: TAction;
    actEdIndent: TAction;
    actEdUnIndent: TAction;
    Actions: TActionList;
    ApplicationProperties1: TApplicationProperties;
    imgList: TImageList;
    mainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    mnuLayout: TMenuItem;
    mnuItemMruFile: TMenuItem;
    mnuItemMruProj: TMenuItem;
    mnuItemWin: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure actEdFindExecute(Sender: TObject);
    procedure actEdFindNextExecute(Sender: TObject);
    procedure actFileAddToProjExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCompAndRunExecute(Sender: TObject);
    procedure actFileCompAndRunWithArgsExecute(Sender: TObject);
    procedure actFileHtmlExportExecute(Sender: TObject);
    procedure actFileOpenContFoldExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actEdIndentExecute(Sender: TObject);
    procedure actLayoutSaveExecute(Sender: TObject);
    procedure actProjCompAndRunWithArgsExecute(Sender: TObject);
    procedure actProjCompileAndRunExecute(Sender: TObject);
    procedure actProjCompileExecute(Sender: TObject);
    procedure actEdCopyExecute(Sender: TObject);
    procedure actEdCutExecute(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actEdMacPlayExecute(Sender: TObject);
    procedure actEdMacStartStopExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actProjNewExecute(Sender: TObject);
    procedure actFileNewRunExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actProjOpenContFoldExecute(Sender: TObject);
    procedure actProjOpenExecute(Sender: TObject);
    procedure actEdPasteExecute(Sender: TObject);
    procedure actProjCloseExecute(Sender: TObject);
    procedure actProjOptsExecute(Sender: TObject);
    procedure actEdRedoExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actProjOptViewExecute(Sender: TObject);
    procedure actProjRunExecute(Sender: TObject);
    procedure actProjRunWithArgsExecute(Sender: TObject);
    procedure actProjSaveAsExecute(Sender: TObject);
    procedure actProjSaveExecute(Sender: TObject);
    procedure actEdUndoExecute(Sender: TObject);
    procedure actProjSourceExecute(Sender: TObject);
    procedure actEdUnIndentExecute(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private

    fDoc: TCESynMemo;
    fUpdateCount: NativeInt;
    fProject: TCEProject;
    fPlugList: TCEPlugDescriptorList;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectInspectWidget;
    fPrjCfWidg: TCEProjectConfigurationWidget;
    fStExpWidg: TCEStaticExplorerWidget;
    fFindWidg:  TCESearchWidget;
    fExplWidg: TCEMiniExplorerWidget;
    fLibMWidg: TCELibManEditorWidget;
    fTlsEdWidg: TCEToolsEditorWidget;
    fProjMru: TMruFileList;
    fFileMru: TMruFileList;
    fPrInpWidg: TCEProcInputWidget;
    fInitialized: boolean;
    fRunnableSw: string;
    {$IFDEF WIN32}
    fCdbWidg: TCECdbWidget;
    {$ENDIF}

    fRunProc: TCheckedAsyncProcess;

    fLogMessager: TCELogMessageSubject;
    fMainMenuSubj: TCEMainMenuSubject;
    procedure updateMainMenuProviders;

    // ICEMultiDocObserver
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);

    // ICESessionOptionsObserver
    procedure sesoptBeforeSave;
    procedure sesoptDeclareProperties(aFiler: TFiler);
    procedure sesoptAfterLoad;
    procedure optget_FileMRUItems(aWriter: TWriter);
    procedure optset_FileMRUItems(aReader: TReader);
    procedure optget_FileMRULimit(aWriter: TWriter);
    procedure optset_FileMRULimit(aReader: TReader);
    procedure optget_ProjMRUItems(aWriter: TWriter);
    procedure optset_ProjMRUItems(aReader: TReader);
    procedure optget_ProjMRULimit(aWriter: TWriter);
    procedure optset_ProjMRULimit(aReader: TReader);
    procedure optset_RunnableSw(aReader: TReader);
    procedure optget_RunnableSw(aWriter: Twriter);

    //Init - Fina
    procedure getCMdParams;
    procedure checkCompilo;
    procedure InitMRUs;
    procedure InitWidgets;
    procedure InitPlugins;
    procedure InitDocking;
    procedure InitSettings;
    procedure SaveSettings;
    procedure LoadDocking;
    procedure SaveDocking;
    procedure KillPlugs;
    procedure FreeRunnableProc;

    // widget interfaces subroutines
    procedure checkWidgetActions(const aWidget: TCEWidget);
    procedure widgetShowFromAction(sender: TObject);

    // run & exec sub routines
    procedure asyncprocOutput(sender: TObject);
    procedure asyncprocTerminate(sender: TObject);
    procedure compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');

    // file sub routines
    procedure newFile;
    function findFile(const aFilename: string): NativeInt;
    procedure saveFile(const edIndex: NativeInt);
    procedure saveFileAs(const edIndex: NativeInt; const aFilename: string);

    // project sub routines
    procedure saveProjSource(const aEditor: TCESynMemo);
    procedure newProj;
    procedure saveProj;
    procedure saveProjAs(const aFilename: string);
    procedure openProj(const aFilename: string);
    procedure closeProj;
    procedure addSource(const aFilename: string);

    // mru
    procedure mruChange(Sender: TObject);
    procedure mruFileItemClick(Sender: TObject);
    procedure mruProjItemClick(Sender: TObject);
    procedure mruClearClick(Sender: TObject);

    // layout
    procedure layoutMnuItemClick(sender: TObject);
    procedure layoutLoadFromFile(const aFilename: string);
    procedure layoutSaveToFile(const aFilename: string);
    procedure layoutUpdateMenu;

  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    //
    procedure openFile(const aFilename: string);
    //
    property processInput: TCEProcInputWidget read fPrInpWidg;
  end;

  procedure PlugDispatchToHost(aPlugin: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;

var
  CEMainForm: TCEMainForm;

implementation
{$R *.lfm}

uses
  SynMacroRecorder, ce_options, ce_symstring;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMainForm.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  fLogMessager := TCELogMessageSubject.create;
  fMainMenuSubj:= TCEMainMenuSubject.create;
  //
  EntitiesConnector.addObserver(self);
  //
  InitMRUs;
  InitWidgets;
  InitDocking;
  InitSettings;
  layoutUpdateMenu;
  //
  newProj;
  checkCompilo;
  getCMdParams;
  //
  updateMainMenuProviders;
  EntitiesConnector.forceUpdate;
  fInitialized := true;
end;

procedure TCEMainForm.checkCompilo;
const
  msg = 'Coedit recquires DMD or DUB to be setup on this system' + LineEnding +
    'If DMD is setup please add it to the system PATH variable before using Coedit';
begin
  if exeInSysPath('dmd') or exeInSysPath('dub') then
    exit;
  ce_common.dlgOkError(msg);
  close;
end;

procedure TCEMainForm.getCMdParams;
var
  value: string;
  str: TStringList;
begin
  if application.ParamCount > 0 then
  begin
    value := application.Params[1];
    if value <> '' then
    begin
      str := TStringList.Create;
      try
        str.DelimitedText := value;
        for value in str do
        begin
          if fileExists(value) then
            openFile(value);
        end;
      finally
        str.Free;
      end;
    end;
  end;
  value := application.GetOptionValue('plugs');
  if value <> 'OFF' then
    InitPlugins;
  value := application.GetOptionValue('p', 'project');
  if (value <> '') and fileExists(value) then
    openProj(value);
  value := application.GetOptionValue('f', 'files');
  if value <> '' then
  begin
    str := TStringList.Create;
    try
      str.DelimitedText := value;
      for value in str do
      begin
        if fileExists(value) then
          openFile(value);
      end;
    finally
      str.Free;
    end;
  end;
end;

procedure TCEMainForm.InitMRUs;
begin
  fProjMru := TMruFileList.Create;
  fFileMru := TMruFileList.Create;
  fProjMru.objectTag := mnuItemMruProj;
  fFileMru.objectTag := mnuItemMruFile;
  fProjMru.OnChange := @mruChange;
  fFileMru.OnChange := @mruChange;
end;

procedure TCEMainForm.InitPlugins;
var
  pth: string;
  fname: string;
  i: NativeInt;
  lst: TStringList;
  hdl: TLibHandle;
  plg: PPlugDescriptor;
begin
  fPlugList := TCEPlugDescriptorList.Create;
  pth := extractFilePath(application.ExeName) + 'plugins';
  lst := TStringList.Create;
  try
    listFiles(lst, pth, false);
    for i := 0 to lst.Count-1 do
    begin
      fname := lst.Strings[i];
      if extractFileExt(fname) <> '.' + SharedSuffix then
        continue;
      hdl := LoadLibrary(fname);
      if hdl = NilHandle then
        continue;

      plg := new(PPlugDescriptor);
      plg^.Handle := hdl;
      plg^.HostCreatePlug   := THostCreatePlug(GetProcAddress(hdl, 'createPlug'));
      plg^.HostDestroyPlug  := THostDestroyPlug(GetProcAddress(hdl, 'destroyPlug'));
      plg^.HostDispatchToPlug := THostDispatchToPlug(GetProcAddress(hdl, 'dispatchToPlug'));
      if plg^.HostCreatePlug <> nil then
        plg^.Plugin := plg^.HostCreatePlug(@PlugDispatchToHost);

      if (plg^.HostCreatePlug = nil) or (plg^.HostDestroyPlug = nil) or
        (plg^.HostDispatchToPlug = nil) then
      begin
        Dispose(plg);
        {$IFDEF RELEASE}
        FreeLibrary(Hdl);
        {$ENDIF}
        continue;
      end;
      fPlugList.addPlugin(plg);
    end;
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.InitWidgets;
var
  widg: TCEWidget;
  act: TAction;
  itm: TMenuItem;
begin
  fWidgList := TCEWidgetList.Create;
  fMesgWidg := TCEMessagesWidget.create(self);
  fEditWidg := TCEEditorWidget.create(self);
  fProjWidg := TCEProjectInspectWidget.create(self);
  fPrjCfWidg:= TCEProjectConfigurationWidget.create(self);
  fStExpWidg:= TCEStaticExplorerWidget.create(self);
  fFindWidg := TCESearchWidget.create(self);
  fExplWidg := TCEMiniExplorerWidget.create(self);
  fLibMWidg := TCELibManEditorWidget.create(self);
  fTlsEdWidg:= TCEToolsEditorWidget.create(self);
  fPrInpWidg:= TCEProcInputWidget.create(self);

  {$IFDEF WIN32}
  fCdbWidg  := TCECdbWidget.create(self);
  {$ENDIF}

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);
  fWidgList.addWidget(@fPrjCfWidg);
  fWidgList.addWidget(@fStExpWidg);
  fWidgList.addWidget(@fFindWidg);
  fWidgList.addWidget(@fExplWidg);
  fWidgList.addWidget(@fLibMWidg);
  fWidgList.addWidget(@fTlsEdWidg);
  fWidgList.addWidget(@fPrInpWidg);

  {$IFDEF WIN32}
  fWidgList.addWidget(@fCdbWidg);
  {$ENDIF}

  for widg in fWidgList do
  begin
    act := TAction.Create(self);
    act.Category := 'Window';
    act.Caption := widg.Caption;
    act.OnExecute := @widgetShowFromAction;
    act.Tag := ptrInt(widg);
    act.ImageIndex := 25;
    itm := TMenuItem.Create(self);
    itm.Action := act;
    itm.Tag := ptrInt(widg);
    mnuItemWin.Add(itm);
  end;
end;

procedure TCEMainForm.InitDocking;
var
  i: NativeInt;
  aManager: TAnchorDockManager;
begin
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);
  DockMaster.OnShowOptions := @ShowAnchorDockOptions;
  DockMaster.HeaderStyle := adhsPoints;
  DockMaster.HideHeaderCaptionFloatingControl := true;

  if DockManager is TAnchorDockManager then begin
    aManager:=TAnchorDockManager(DockManager);
    aManager.PreferredSiteSizeAsSiteMinimum:=false;
  end;
  Height := 0;

  for i := 0 to fWidgList.Count-1 do
  begin
    DockMaster.MakeDockable(fWidgList.widget[i],true);
    DockMaster.GetAnchorSite(fWidgList.widget[i]).Header.HeaderPosition := adlhpTop;
  end;

  DockMaster.ManualDock(DockMaster.GetAnchorSite(fEditWidg), DockMaster.GetSite(Self), alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fMesgWidg), DockMaster.GetSite(Self), alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fStExpWidg), DockMaster.GetSite(Self), alLeft);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fFindWidg),
    DockMaster.GetAnchorSite(fStExpWidg), alBottom, fStExpWidg);
  width := width - fProjWidg.Width;
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fProjWidg), DockMaster.GetSite(Self), alRight);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fPrjCfWidg),
    DockMaster.GetAnchorSite(fProjWidg), alBottom, fProjWidg);
  DockMaster.GetAnchorSite(fEditWidg).Header.HeaderPosition := adlhpTop;

  DockMaster.GetAnchorSite(fExplWidg).Close;
  DockMaster.GetAnchorSite(fLibMWidg).Close;
  DockMaster.GetAnchorSite(fTlsEdWidg).Close;
  LoadDocking;
end;

procedure TCEMainForm.InitSettings;
var
  fname1: string;
  fname2: string;
  opts: TCEOptions;
begin
  fname1 := getCoeditDocPath + 'options2.txt';
  fname2 := getCoeditDocPath + 'options2.bak';
  opts := TCEOptions.create(nil);
  try
    if fileExists(fname1) then
    begin
      opts.loadFromFile(fname1);
      if opts.hasLoaded then
      begin
        if fileExists(fname2) then
           sysutils.deleteFile(fname2);
        if not fileExists(fname2) then
          fileutil.copyFile(fname1, fname2, false);
      end;
    end;
  finally
    opts.Free;
  end;
end;

procedure TCEMainForm.SaveSettings;
var
  opts: TCEOptions;
begin
  if not fInitialized then
    exit;
  opts := TCEOptions.create(nil);
  try
    forceDirectory(getCoeditDocPath);
    opts.saveToFile(getCoeditDocPath + 'options2.txt');
  finally
    opts.Free;
  end;
end;

procedure TCEMainForm.SaveDocking;
var
  xcfg: TXMLConfigStorage;
  i: NativeInt;
begin
  if not fInitialized then
    exit;
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  for i:= 0 to fWidgList.Count-1 do
  begin
    DockMaster.GetAnchorSite(fWidgList.widget[i]).Show;
    DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState := wsNormal;
  end;
  if not Visible then exit;
  //
  forceDirectory(getCoeditDocPath);
  xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'docking.xml',false);
  try
    DockMaster.SaveLayoutToConfig(xcfg);
    xcfg.WriteToDisk;
  finally
    xcfg.Free;
  end;
  //
  xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'dockingopts.xml',false);
  try
    DockMaster.SaveSettingsToConfig(xcfg);
    xcfg.WriteToDisk;
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.LoadDocking;
var
  xcfg: TXMLConfigStorage;
  str: TMemoryStream;
begin
  if fileExists(getCoeditDocPath + 'docking.xml') then
  begin
    xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'docking.xml', true);
    try
      try
        DockMaster.LoadLayoutFromConfig(xcfg, false);
      except
        exit;
      end;
      str := TMemoryStream.Create;
      try
        xcfg.SaveToStream(str);
        str.saveToFile(getCoeditDocPath + 'docking.bak')
      finally
        str.Free;
      end;
    finally
      xcfg.Free;
    end;
  end;
  if fileExists(getCoeditDocPath + 'dockingopts.xml') then
  begin
    xcfg := TXMLConfigStorage.Create(getCoeditDocPath + 'dockingopts.xml', true);
    try
      try
        DockMaster.LoadSettingsFromConfig(xcfg);
      except
        exit;
      end;
      str := TMemoryStream.Create;
      try
        xcfg.SaveToStream(str);
        str.saveToFile(getCoeditDocPath + 'dockingopts.bak')
      finally
        str.Free;
      end;
    finally
      xcfg.Free;
    end;
  end;
end;

procedure TCEMainForm.KillPlugs;
var
  descr: TPlugDescriptor;
  i: NativeInt;
begin
  if fPlugList = nil then exit;
  for i := 0 to fPlugList.Count-1 do
  begin
    descr := fPlugList.plugin[i];
    descr.HostDestroyPlug(descr.Plugin);
    {$IFDEF RELEASE}
    FreeLibrary(descr.Handle);
    {$ENDIF}
  end;
  while fPlugList.Count <> 0 do
  begin
    Dispose(PPlugDescriptor(fPlugList.Items[fPlugList.Count-1]));
    fPlugList.Delete(fPlugList.Count-1);
  end;
  fPlugList.Free;
end;

procedure TCEMainForm.FreeRunnableProc;
var
  fname: string;
begin
  if fRunProc = nil then
    exit;
  //
  fname := fRunProc.Executable;
  if ExtractFileDir(fname) <> GetTempDir(false) then
    exit;
  killProcess(fRunProc);
  if fileExists(fname) then
    sysutils.DeleteFile(fname);
end;

destructor TCEMainForm.destroy;
begin
  SaveSettings;
  //
  KillPlugs;
  //
  fWidgList.Free;
  fProjMru.Free;
  fFileMru.Free;
  fProject.Free;
  FreeRunnableProc;
  //
  fLogMessager.Free;
  fMainMenuSubj.Free;
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMainForm.UpdateDockCaption(Exclude: TControl = nil);
begin
  // otherwise dockmaster puts the widget list.
  Caption := 'Coedit';
end;

procedure TCEMainForm.ApplicationProperties1Exception(Sender: TObject;E: Exception);
begin
  if fMesgWidg = nil then
    ce_common.dlgOkError(E.Message)
  else
    fMesgWidg.lmFromString(E.Message, nil, amcApp, amkErr);
end;

procedure TCEMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: NativeInt;
  ed: TCESynMemo;
begin
  canClose := false;
  if fProject <> nil then if fProject.modified then
    if ce_common.dlgOkCancel('last project modifications are not saved, quit anyway ?')
        <> mrOK then exit;
  for i := 0 to fEditWidg.editorCount-1 do
  begin
    ed := fEditWidg.editor[i];
    if ed.modified then if ce_common.dlgOkCancel(format
      ('last "%s" modifications are not saved, quit anyway ?',
        [shortenPath(ed.fileName, 25)])) <> mrOK then exit;
  end;
  canClose := true;
  // saving doesnt work when csDestroying in comp.state.
  SaveDocking;
end;

procedure TCEMainForm.ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  hasEd: boolean;
  hasProj: boolean;
begin
  Handled := true;
  {$IFDEF LINUX}
  // fixes the error raised when the update is called after docClosing ()
  // looks like a syncro error, needs more investigation.
  Application.DisableIdleHandler;
  {$ENDIF}
  if fUpdateCount > 0 then exit;
  Inc(fUpdateCount);
  try
    HasEd := fDoc <> nil;
    if hasEd then
    begin
      actEdCopy.Enabled := fDoc.SelAvail and fDoc.Focused;
      actEdCut.Enabled := fDoc.SelAvail and fDoc.Focused;
      actEdPaste.Enabled := fDoc.CanPaste and fDoc.Focused;
      actEdUndo.Enabled := fDoc.CanUndo;
      actEdRedo.Enabled := fDoc.CanRedo;
      //
      actFileCompAndRun.Enabled := fDoc.isDSource;
      actFileCompAndRunWithArgs.Enabled := fDoc.isDSource;
      //
      actEdMacPlay.Enabled := true;
      actEdMacStartStop.Enabled := true;
      actEdIndent.Enabled := true;
      actEdUnIndent.Enabled := true;
      //
      actFileSave.Enabled := true;
      actFileSaveAs.Enabled := true;
      actFileClose.Enabled := true;
      actFileSaveAll.Enabled := true;
      actFileOpenContFold.Enabled := true;
      actFileHtmlExport.Enabled := true;
    end
    else begin
      actEdCopy.Enabled := false;
      actEdCut.Enabled := false ;
      actEdPaste.Enabled := false;
      actEdUndo.Enabled := false;
      actEdRedo.Enabled := false;
      actEdMacPlay.Enabled := false;
      actEdMacStartStop.Enabled := false;
      actEdIndent.Enabled := false;
      actEdUnIndent.Enabled := false;
      //
      actFileCompAndRun.Enabled := false;
      actFileCompAndRunWithArgs.Enabled := false;
      actFileSave.Enabled := false;
      actFileSaveAs.Enabled := false;
      actFileClose.Enabled := false;
      actFileSaveAll.Enabled := false;
      actFileOpenContFold.Enabled := false;
      actFileHtmlExport.Enabled := false;
    end;
    hasProj := fProject <> nil;
    actProjSave.Enabled := hasProj;
    actProjSaveAs.Enabled := hasProj;
    actProjOpts.Enabled := hasProj;
    actProjClose.Enabled := hasProj;
    actProjCompile.Enabled := hasProj;
    actProjCompileAndRun.Enabled := hasProj;
    actProjCompAndRunWithArgs.Enabled := hasProj;
    actProjOptView.Enabled := hasProj;
    actProjOpenContFold.Enabled := hasProj;
    if hasProj then
    begin
      actProjSource.Enabled := fileExists(fProject.Filename);
      actProjRun.Enabled := fProject.canBeRun;
      actProjRunWithArgs.Enabled := fProject.canBeRun;
    end;
    actFileAddToProj.Enabled := hasEd and hasProj;
    //
    updateMainMenuProviders;
  finally
    Dec(fUpdateCount);
  end;
end;

procedure TCEMainForm.updateMainMenuProviders;
var
  i, j: Integer;
  itm: TMenuItem;
  doneUpdate: boolean;
begin
  doneUpdate := false;
  for j := 0 to fMainMenuSubj.observersCount-1 do
  begin
    // try to update existing entry.
    for i := 0 to mainMenu.Items.Count-1 do
      if PtrInt(fMainMenuSubj.observers[j]) = mainMenu.Items[i].Tag then
      begin
        (fMainMenuSubj.observers[j] as ICEMainMenuProvider).menuUpdate(mainMenu.Items[i]);
        doneUpdate := true;
        break;
      end;
    if doneUpdate then
      continue;
    // otherwise propose to create a new entry
    itm := TMenuItem.Create(Self);
    (fMainMenuSubj.observers[j] as ICEMainMenuProvider).menuDeclare(itm);
    itm.Tag:= PtrInt(fMainMenuSubj.observers[j]);
    case itm.Count > 0 of
      true: mainMenu.Items.Add(itm);
      false: itm.Free;
    end;
  end;
end;

procedure TCEMainForm.checkWidgetActions(const aWidget: TCEWidget);
var
  tlt: string;
  cnt, i: NativeInt;
  prt, itm: TMenuItem;
begin
  tlt := aWidget.contextName;
  if tlt = '' then exit;
  cnt := aWidget.contextActionCount;
  if cnt = 0 then exit;
  //
  prt := TMenuItem.Create(self);
  prt.Caption := tlt;
  mainMenu.Items.Add(prt);
  for i := 0 to cnt-1 do
  begin
    itm := TMenuItem.Create(prt);
    itm.Action := aWidget.contextAction(i);
    prt.Add(itm);
  end;
end;

procedure TCEMainForm.mruChange(Sender: TObject);
var
  srcLst: TMruFileList;
  trgMnu: TMenuItem;
  itm: TMenuItem;
  fname: string;
  clickTrg: TNotifyEvent;
  i: NativeInt;
begin
  srcLst := TMruFileList(Sender);
  if srcLst = nil then exit;
  trgMnu := TMenuItem(srcLst.objectTag);
  if trgMnu = nil then exit;

  if fUpdateCount > 0 then exit;
  Inc(fUpdateCount);
  try
    if srcLst = fFileMru then
      clickTrg := @mruFileItemClick
    else if srcLst = fProjMru then
      clickTrg := @mruProjItemClick;

    trgMnu.Clear;

    for i:= 0 to srcLst.Count-1 do
    begin
      fname := srcLst.Strings[i];
      itm := TMenuItem.Create(trgMnu);
      itm.Hint := fname;
      itm.Caption := shortenPath(fname, 50);
      itm.OnClick := clickTrg;
      trgMnu.Add(itm);
    end;

    trgMnu.AddSeparator;
    itm := TMenuItem.Create(trgMnu);
    itm.Caption := 'Clear';
    itm.OnClick := @mruClearClick;
    itm.Tag := PtrInt(srcLst);
    trgMnu.Add(itm);

  finally
    Dec(fUpdateCount);
  end;
end;

procedure TCEMainForm.mruClearClick(Sender: TObject);
var
  srcLst: TMruFileList;
begin
  srcLst := TMruFileList(TmenuItem(Sender).Tag);
  if srcLst = nil then exit;
  //
  srcLst.Clear;
end;

procedure TCEMainForm.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  CanShow := true;
  //if fDoc <> nil then if fDoc.Focused then
  //begin
  //  DcdWrapper.getDdocFromCursor(HintStr);
  //  CanShow := HintStr <> '';
  //end;
end;
{$ENDREGION}

{$REGION ICEMultiDocMonitor ----------------------------------------------------}
procedure TCEMainForm.docNew(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEMainForm.docClosing(aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  fDoc := nil;
end;

procedure TCEMainForm.docFocused(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEMainForm.docChanged(aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;
{$ENDREGION}

{$REGION file ------------------------------------------------------------------}
procedure TCEMainForm.actFileHtmlExportExecute(Sender: TObject);
var
  exp: TSynExporterHTML;
begin
  if fDoc = nil then
    exit;
  exp := TSynExporterHTML.Create(nil);
  try
    with TOpenDialog.Create(nil) do
    try
      if Execute then begin
        exp.Highlighter := fDoc.Highlighter;
        exp.Title := fDoc.fileName;
        exp.ExportAsText:=true;
        exp.ExportAll(fDoc.Lines);
        exp.SaveToFile(filename);
      end;
    finally
      Free;
    end;
  finally
    exp.Free;
  end;
end;

procedure TCEMainForm.newFile;
begin
  if fEditWidg = nil then exit;
  fEditWidg.addEditor;
end;

function TCEMainForm.findFile(const aFilename: string): NativeInt;
var
  i: NativeInt;
begin
  result := -1;
  if fEditWidg = nil then exit;
  for i := 0 to fEditWidg.editorCount-1 do begin
    if fEditWidg.editor[i].fileName = aFilename then exit(i);
    if fEditWidg.editor[i].tempFilename = aFilename then exit(i);
  end;
end;

procedure TCEMainForm.openFile(const aFilename: string);
var
  i: NativeInt;
begin
  if fEditWidg = nil then exit;
  //
  i := findFile(aFilename);
  if i > -1 then
  begin
    fEditWidg.PageControl.PageIndex := i;
    exit;
  end;
  i := fEditWidg.editorCount;
  fEditWidg.addEditor;
  fEditWidg.editor[i].loadFromFile(aFilename);
  fEditWidg.focusedEditorChanged;
  fFileMru.Insert(0, aFilename);
end;

procedure TCEMainForm.saveFile(const edIndex: NativeInt);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if edIndex >= fEditWidg.editorCount then exit;
  //
  if fEditWidg.editor[edIndex].Highlighter = LfmSyn then
  begin
    saveProjSource(fEditWidg.editor[edIndex]);
    exit;
  end;
  //
  str := fEditWidg.editor[edIndex].fileName;
  if str = '' then exit;
  fEditWidg.editor[edIndex].save;
end;

procedure TCEMainForm.saveFileAs(const edIndex: NativeInt; const aFilename: string);
begin
  if fEditWidg = nil then exit;
  if edIndex < 0 then exit;
  if edIndex >= fEditWidg.editorCount then exit;
  //
  fEditWidg.editor[edIndex].saveToFile(aFilename);
  fFileMru.Insert(0, aFilename);
end;

procedure TCEMainForm.mruFileItemClick(Sender: TObject);
begin
  openFile(TMenuItem(Sender).Hint);
end;

procedure TCEMainForm.actFileOpenExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    filter := DdiagFilter;
    if execute then
    begin
      openFile(filename);
    end;
  finally
    free;
  end;
end;

procedure TCEMainForm.actProjOpenContFoldExecute(Sender: TObject);
begin
  if fProject = nil then exit;
  if not fileExists(fProject.fileName) then exit;
  //
  DockMaster.GetAnchorSite(fExplWidg).Show;
  fExplWidg.expandPath(extractFilePath(fProject.fileName));
end;

procedure TCEMainForm.actFileNewExecute(Sender: TObject);
begin
  newFile;
end;

procedure TCEMainForm.actFileNewRunExecute(Sender: TObject);
begin
  newFile;
  fDoc.Text :=
  'module runnable;' + LineEnding +
  LineEnding +
  'import std.stdio;' + LineEnding +
  LineEnding +
  'void main(string[] args)' + LineEnding +
  '{' + LineEnding +
  '    // this file can be directly executed using menu file/compile & run' + LineEnding +
  '    // phobos and libman imports are allowed' + LineEnding +
  '    writeln("hello runnable module");' + LineEnding +
  '}';
end;

procedure TCEMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  with TSaveDialog.Create(nil) do
  try
    Filter := DdiagFilter;
    if execute then
      saveFileAs(fEditWidg.editorIndex, filename);
  finally
    free;
  end;
end;

procedure TCEMainForm.actFileSaveExecute(Sender: TObject);
var
  str: string;
begin
  if fDoc = nil then exit;
  //
  str := fDoc.fileName;
  if (str <> fDoc.tempFilename) and (fileExists(str)) then
    saveFile(fEditWidg.editorIndex)
  else actFileSaveAs.Execute;
end;

procedure TCEMainForm.actFileAddToProjExecute(Sender: TObject);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  if fEditWidg.editor[fEditWidg.editorIndex].isProjectSource
    then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  if fileExists(str) then fProject.addSource(str)
  else dlgOkInfo('the file has not been added to the project because it does not exist');
end;

procedure TCEMainForm.actFileCloseExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  if fDoc.modified then if dlgOkCancel(
      'The latest mdofifications are not saved, continue ?') = mrCancel
      then exit;
  //
  fEditWidg.removeEditor(fEditWidg.editorIndex);
end;

procedure TCEMainForm.actFileSaveAllExecute(Sender: TObject);
var
  i: NativeInt;
begin
  for i:= 0 to fEditWidg.editorCount-1 do
    saveFile(i);
end;

procedure TCEMainForm.FormDropFiles(Sender: TObject;const FileNames: array of String);
var
  i: NativeInt;
begin
  for i:= low(FileNames) to high(FileNames) do
    openFile(FileNames[i]);
end;
{$ENDREGION}

{$REGION edit ------------------------------------------------------------------}
procedure TCEMainForm.actEdCopyExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.CopyToClipboard;
end;

procedure TCEMainForm.actEdCutExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.CutToClipboard;
end;

procedure TCEMainForm.actEdPasteExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.PasteFromClipboard;
end;

procedure TCEMainForm.actEdUndoExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.Undo;
end;

procedure TCEMainForm.actEdRedoExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.Redo;
end;

procedure TCEMainForm.actEdMacPlayExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fEditWidg.macRecorder.PlaybackMacro(fDoc);
end;

procedure TCEMainForm.actEdMacStartStopExecute(Sender: TObject);
begin
  if assigned(fDoc) then
  begin
    if fEditWidg.macRecorder.State = msRecording then
      fEditWidg.macRecorder.Stop
    else fEditWidg.macRecorder.RecordMacro(fDoc);
  end;
end;

procedure TCEMainForm.actEdIndentExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.ExecuteCommand(ecBlockIndent, '', nil);
end;

procedure TCEMainForm.actEdUnIndentExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.ExecuteCommand(ecBlockUnIndent, '', nil);
end;

procedure TCEMainForm.actEdFindExecute(Sender: TObject);
var
  win: TAnchorDockHostSite;
  str: string;
begin
  win := DockMaster.GetAnchorSite(fFindWidg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
  if fDoc = nil then exit;
  //
  if fDoc.SelAvail then
    str := fDoc.SelText
  else str := fDoc.Identifier;
  ffindwidg.cbToFind.Text := str;
  ffindwidg.cbToFindChange(nil);
end;

procedure TCEMainForm.actEdFindNextExecute(Sender: TObject);
begin
  ffindwidg.actFindNextExecute(nil);
end;
{$ENDREGION}

{$REGION run -------------------------------------------------------------------}
procedure TCEMainForm.asyncprocOutput(sender: TObject);
var
  proc: TProcess;
  lst: TStringList;
  str: string;
begin
  proc := TProcess(sender);
  lst := TStringList.Create;
  try
    processOutputToStrings(proc, lst);
    if proc = fRunProc then for str in lst do
      subjLmFromString(fLogMessager, str, fDoc, amcEdit, amkBub)
    else if proc.Executable = DCompiler then
      for str in lst do
        subjLmFromString(fLogMessager, str, fDoc, amcEdit, amkAuto);
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.asyncprocTerminate(sender: TObject);
var
  proc: TProcess;
  lst: TStringList;
  str: string;
begin
  proc := TProcess(sender);
  lst := TStringList.Create;
  try
    processOutputToStrings(proc, lst);
    // runnable module
    if proc = fRunProc then
    begin
      for str in lst do
        subjLmFromString(fLogMessager, str, fDoc, amcEdit, amkBub);
    end;
  finally
    lst.Free;
  end;
  if proc = fPrInpWidg.process then
    fPrInpWidg.process := nil;
end;

procedure TCEMainForm.compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
var
  editor: TCESynMemo;
  dmdproc: TProcess;
  fname, fBasename: string;
begin

  FreeRunnableProc;
  fRunProc := TCheckedAsyncProcess.Create(nil);
  fRunProc.Options := [poStderrToOutPut, poUsePipes];
  fRunProc.ShowWindow := swoHIDE;
  fRunProc.OnReadData := @asyncprocOutput;
  fRunProc.OnTerminate:= @asyncprocTerminate;

  dmdproc := TProcess.Create(nil);
  editor  := fEditWidg.editor[edIndex];
  try

    subjLmClearByData(fLogMessager, editor);
    subjLmFromString(fLogMessager, 'compiling ' + shortenPath(editor.fileName, 25),
      editor, amcEdit, amkInf);

    if fileExists(editor.fileName) then begin
      editor.save;
      fname := editor.fileName;
    end else begin
      editor.saveTempFile;
      fname := editor.tempFilename;
    end;
    fBasename := stripFileExt(fname);

    if fRunnableSw = '' then
      fRunnableSw := '-vcolumns'#13'-w'#13'-wi';
    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options := [poStdErrToOutput, poUsePipes];
    dmdproc.Executable := DCompiler;
    dmdproc.Parameters.Add(fname);
    dmdproc.Parameters.AddText(fRunnableSw);
    dmdproc.Parameters.Add('-of' + fBasename + exeExt);
    LibMan.getLibFiles(nil, dmdproc.Parameters);
    LibMan.getLibSources(nil, dmdproc.Parameters);
    dmdproc.Execute;
    while dmdproc.Running do asyncprocOutput(dmdProc);

    if (dmdProc.ExitStatus = 0) then
    begin
      subjLmFromString(fLogMessager, shortenPath(editor.fileName, 25)
        + ' successfully compiled', editor, amcEdit, amkInf);

      fRunProc.CurrentDirectory := extractFilePath(fRunProc.Executable);
      if runArgs <> '' then
        fRunProc.Parameters.DelimitedText := symbolExpander.get(runArgs);
      fRunProc.Executable := fBasename + exeExt;
      fPrInpWidg.process := fRunProc;
      fRunProc.Execute;
      sysutils.DeleteFile(fBasename + objExt);
    end
    else begin
      subjLmFromString(fLogMessager, shortenPath(editor.fileName,25)
        + ' has not been compiled', editor, amcEdit, amkErr);
    end;

  finally
    dmdproc.Free;
  end;
end;

procedure TCEMainForm.actFileCompAndRunExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  compileAndRunFile(fEditWidg.editorIndex);
end;

procedure TCEMainForm.actFileCompAndRunWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  runargs := '';
  if InputQuery('Execution arguments', '', runargs) then
    compileAndRunFile(fEditWidg.editorIndex, runargs);
end;

procedure TCEMainForm.actFileOpenContFoldExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  if not fileExists(fDoc.fileName) then exit;
  //
  DockMaster.GetAnchorSite(fExplWidg).Show;
  fExplWidg.expandPath(extractFilePath(fDoc.fileName));
end;

procedure TCEMainForm.actProjCompileExecute(Sender: TObject);
begin
  fProject.compileProject;
end;

procedure TCEMainForm.actProjCompileAndRunExecute(Sender: TObject);
begin
  if fProject.compileProject then
    fProject.runProject;
end;

procedure TCEMainForm.actProjCompAndRunWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  if not fProject.compileProject then
    exit;
  runargs := '';
  if InputQuery('Execution arguments', '', runargs) then
    fProject.runProject(runargs);
end;

procedure TCEMainForm.actProjRunExecute(Sender: TObject);
var
  i: Integer;
  dt: double;
label
  _rbld,
  _run;
begin
  if fProject.currentConfiguration.outputOptions.binaryKind <> executable then
  begin
    // TODO-cfeature: define an alternative exe name for shared lib:
    // e.g: the dll produced by the proj. is the input filename of an host app.
    dlgOkInfo('Non executable projects cant be run');
    exit;
  end;
  if not fileExists(fProject.outputFilename) then
  begin
    if dlgOkCancel('The project output is missing, build ?') <> mrOK then
      exit;
    goto _rbld;
  end;
  dt := fileAge(fProject.outputFilename);
  for i := 0 to fProject.Sources.Count-1 do
  begin
    if fileAge(fProject.getAbsoluteSourceName(i)) > dt then
      if dlgOkCancel('The project sources have changed since last build, rebuild ?') = mrOK then
        goto _rbld
      else
        break;
  end;
  goto _run;
  _rbld:
    fProject.compileProject;
  _run:
    if fileExists(fProject.outputFilename) then
      fProject.runProject;
end;

procedure TCEMainForm.actProjRunWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  runargs := '';
  if InputQuery('Execution arguments', '', runargs) then
    fProject.runProject(runargs);
end;
{$ENDREGION}

{$REGION view ------------------------------------------------------------------}
procedure TCEMainForm.widgetShowFromAction(sender: TObject);
var
  widg: TCEWidget;
  win: TControl;
begin
  widg := TCEWidget( TComponent(sender).tag );
  if widg = nil then exit;
  win := DockMaster.GetAnchorSite(widg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
end;

procedure TCEMainForm.layoutLoadFromFile(const aFilename: string);
var
  xcfg: TXMLConfigStorage;
begin
  if not fileExists(aFilename) then
    exit;
  //
  xcfg := TXMLConfigStorage.Create(aFilename, true);
  try
    DockMaster.LoadLayoutFromConfig(xcfg, false);
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.layoutSaveToFile(const aFilename: string);
var
  xcfg: TXMLConfigStorage;
  i: NativeInt;
begin
  // TODO-cbugfix: possible loading AV, xml saved after undocking some widgets, xml file abnormal size.
  for i:= 0 to fWidgList.Count-1 do
  begin
    DockMaster.GetAnchorSite(fWidgList.widget[i]).Show;
    DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState := wsNormal;
  end;
  //
  forceDirectory(extractFilePath(aFilename));
  xcfg := TXMLConfigStorage.Create(aFilename, false);
  try
    DockMaster.SaveLayoutToConfig(xcfg);
    xcfg.WriteToDisk;
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.layoutUpdateMenu;
var
  lst: TStringList;
  itm: TMenuItem;
  i: NativeInt;
begin
  mnuLayout.Clear;
  //
  itm := TMenuItem.Create(self);
  itm.Action := actLayoutSave;
  mnuLayout.Add(itm);
  mnuLayout.AddSeparator;
  //
  lst := TStringList.Create;
  try
    listFiles(lst, getCoeditDocPath + 'layouts' + DirectorySeparator);
    for i := 0 to lst.Count-1 do
    begin
      itm := TMenuItem.Create(self);
      itm.Caption := extractFileName(lst.Strings[i]);
      itm.Caption := stripFileExt(itm.Caption);
      itm.OnClick := @layoutMnuItemClick;
      itm.ImageIndex := 32;
      mnuLayout.Add(itm);
    end;
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.layoutMnuItemClick(sender: TObject);
begin
  layoutLoadFromFile(getCoeditDocPath + 'layouts' + DirectorySeparator +
    TMenuItem(sender).Caption + '.xml');
end;

procedure TCEMainForm.actLayoutSaveExecute(Sender: TObject);
var
  fname: string;
begin
  fname := '';
  if not InputQuery('New layout name', '', fname) then
    exit;
  //
  fname := extractFileName(fname);
  if extractFileExt(fname) <> '.xml' then
    fname += '.xml';
  layoutSaveToFile(getCoeditDocPath + 'layouts' + DirectorySeparator + fname);
  layoutUpdateMenu;
end;
{$ENDREGION}

{$REGION project ---------------------------------------------------------------}
procedure TCEMainForm.saveProjSource(const aEditor: TCESynMemo);
begin
  if fProject = nil then exit;
  if fProject.fileName <> aEditor.fileName then exit;
  //
  aEditor.saveToFile(fProject.fileName);
  openProj(fProject.fileName);
end;

procedure TCEMainForm.closeProj;
begin
  fProject.Free;
  fProject := nil;
end;

procedure TCEMainForm.newProj;
begin
  fProject := TCEProject.Create(nil);
  fProject.Name := 'CurrentProject';
end;

procedure TCEMainForm.saveProj;
begin
  fProject.saveToFile(fProject.fileName);
end;

procedure TCEMainForm.saveProjAs(const aFilename: string);
begin
  fProject.fileName := aFilename;
  fProject.saveToFile(fProject.fileName);
  fProjMru.Insert(0,fProject.fileName);
end;

procedure TCEMainForm.openProj(const aFilename: string);
begin
  closeProj;
  newProj;
  fProject.loadFromFile(aFilename);
  fProjMru.Insert(0,aFilename);
end;

procedure TCEMainForm.mruProjItemClick(Sender: TObject);
begin
  if fProject <> nil then if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;
  openProj(TMenuItem(Sender).Hint);
end;

procedure TCEMainForm.actProjNewExecute(Sender: TObject);
begin
  if fProject <> nil then if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;
  closeProj;
  newProj;
end;

procedure TCEMainForm.actProjCloseExecute(Sender: TObject);
begin
  if fProject = nil then exit;
  if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;

  closeProj;
end;

procedure TCEMainForm.addSource(const aFilename: string);
begin
  if fProject.Sources.IndexOf(aFilename) >= 0 then exit;
  fProject.addSource(aFilename);
end;

procedure TCEMainForm.actProjSaveAsExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then saveProjAs(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjSaveExecute(Sender: TObject);
begin
  if fProject.fileName <> '' then saveProj
  else actProjSaveAs.Execute;
end;

procedure TCEMainForm.actProjOpenExecute(Sender: TObject);
begin
  if fProject <> nil then if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;
  with TOpenDialog.Create(nil) do
  try
    if execute then openProj(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjOptsExecute(Sender: TObject);
var
  win: TControl;
begin
  win := DockMaster.GetAnchorSite(fPrjCfWidg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
end;

procedure TCEMainForm.actProjSourceExecute(Sender: TObject);
begin
  if fProject = nil then exit;
  if not fileExists(fProject.fileName) then exit;
  //
  openFile(fProject.fileName);
  fDoc.Highlighter := LfmSyn;
end;

procedure TCEMainForm.actProjOptViewExecute(Sender: TObject);
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    fProject.getOpts(lst);
    dlgOkInfo(lst.Text);
  finally
    lst.Free;
  end;
end;
{$ENDREGION}

{$REGION ICESessionOptionsObserver ----------------------------------------------------}
procedure TCEMainForm.sesoptBeforeSave;
begin
end;

procedure TCEMainForm.sesoptDeclareProperties(aFiler: TFiler);
begin
  aFiler.DefineProperty('Menu_FileMRU_Items', @optset_FileMRUItems, @optget_FileMRUItems, true);
  aFiler.DefineProperty('Menu_FileMRU_Limit', @optset_FileMRULimit, @optget_FileMRULimit, true);
  aFiler.DefineProperty('Menu_ProjMRU_Items', @optset_ProjMRUItems, @optget_ProjMRUItems, true);
  aFiler.DefineProperty('Menu_ProjMRU_Limit', @optset_ProjMRULimit, @optget_ProjMRULimit, true);
  //
  aFiler.DefineProperty('Runnable_Switches', @optset_RunnableSw, @optget_RunnableSw, true);
end;

procedure TCEMainForm.sesoptAfterLoad;
begin
end;

procedure TCEMainForm.optget_FileMRUItems(aWriter: TWriter);
begin
  aWriter.WriteString(fFileMru.DelimitedText);
end;

procedure TCEMainForm.optset_FileMRUItems(aReader: TReader);
begin
  fFileMru.DelimitedText := aReader.ReadString;
end;

procedure TCEMainForm.optget_FileMRULimit(aWriter: TWriter);
begin
  aWriter.WriteInteger(fFileMru.maxCount);
end;

procedure TCEMainForm.optset_FileMRULimit(aReader: TReader);
begin
  fFileMru.maxCount := aReader.ReadInteger;
end;

procedure TCEMainForm.optget_ProjMRUItems(aWriter: TWriter);
begin
  aWriter.WriteString(fProjMru.DelimitedText);
end;

procedure TCEMainForm.optset_ProjMRUItems(aReader: TReader);
begin
  fProjMru.DelimitedText := aReader.ReadString;
end;

procedure TCEMainForm.optget_ProjMRULimit(aWriter: TWriter);
begin
  aWriter.WriteInteger(fProjMru.maxCount);
end;

procedure TCEMainForm.optset_ProjMRULimit(aReader: TReader);
begin
  fProjMru.maxCount := aReader.ReadInteger;
end;

procedure TCEMainForm.optset_RunnableSw(aReader: TReader);
begin
  fRunnableSw := aReader.ReadString;
end;

procedure TCEMainForm.optget_RunnableSw(aWriter: Twriter);
begin
  aWriter.WriteString(fRunnableSw);
end;
{$ENDREGION}

procedure PlugDispatchToHost(aPlugin: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;
//var
  //ctxt: NativeUint;
  //oper: NativeUint;
begin

  if opCode = HELLO_PLUGIN then begin
      dlgOkInfo('Hello plugin');
      exit;
  end;
{
  ctxt := opCode and $0F000000;
  oper := opCode and $000FFFFF;

  case ctxt of
    CTXT_MSGS:
      case oper of
        //DT_ERR:  CEMainForm.MessageWidget.addCeErr(PChar(data1));
        //DT_INF:  CEMainForm.MessageWidget.addCeInf(PChar(data1));
        //DT_WARN: CEMainForm.MessageWidget.addCeWarn(PChar(data1));
        //else CEMainForm.MessageWidget.addCeWarn('unsupported dispatcher opCode');
      end;
    CTXT_DLGS:
      case oper of
        DT_ERR: dlgOkError(PChar(data1));
        DT_INF: dlgOkInfo(PChar(data1));
        DT_WARN: dlgOkInfo(PChar(data1));
        //else CEMainForm.MessageWidget.addCeWarn('unsupported dispatcher opCode');
      end;
    //else CEMainForm.MessageWidget.addCeWarn('unsupported dispatcher opCode');
  end;}

end;

initialization
  RegisterClasses([TCEOptions]);
end.
