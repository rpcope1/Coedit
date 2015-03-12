unit ce_main;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, SynHighlighterLFM, Forms,
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls, process, XMLPropStorage, SynExportHTML,
  ce_common, ce_dmdwrap, ce_project, ce_dcd, ce_synmemo, ce_writableComponent,
  ce_widget, ce_messages, ce_interfaces, ce_editor, ce_projinspect, ce_projconf,
  ce_search, ce_miniexplorer, ce_libman, ce_libmaneditor, ce_todolist, ce_observer,
  ce_toolseditor, ce_procinput, ce_optionseditor,{$IFDEF WIN32} ce_cdbcmd,{$ENDIF}
  ce_symlist, ce_mru;

type

  // TODO-cfeature: options

  { TCEMainForm }

  TCEMainForm = class(TForm, ICEMultiDocObserver, ICESessionOptionsObserver, ICEEditableShortCut)
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
    actFileUnittest: TAction;
    actFileCompileAndRunOut: TAction;
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
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
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
    procedure actFileCompileAndRunOutExecute(Sender: TObject);
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
    procedure actFileUnittestExecute(Sender: TObject);
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private

    fDoc: TCESynMemo;
    fActionHandler: TCEActionProviderSubject;
    fMultidoc: ICEMultiDocHandler;
    fScCollectCount: Integer;
    fUpdateCount: NativeInt;
    fProject: TCEProject;
    fProjMru: TCEMRUProjectList;
    fFileMru: TCEMRUDocumentList;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectInspectWidget;
    fPrjCfWidg: TCEProjectConfigurationWidget;
    fFindWidg:  TCESearchWidget;
    fExplWidg: TCEMiniExplorerWidget;
    fLibMWidg: TCELibManEditorWidget;
    fTlsEdWidg: TCEToolsEditorWidget;
    fPrInpWidg: TCEProcInputWidget;
    fTodolWidg: TCETodoListWidget;
    fOptEdWidg: TCEOptionEditorWidget;
    fSymlWidg: TCESymbolListWidget;
    {$IFDEF WIN32}
    fCdbWidg: TCECdbWidget;
    {$ENDIF}

    fInitialized: boolean;
    fRunnableSw: string;
    fRunProc: TCheckedAsyncProcess;
    fMsgs: ICEMessagesDisplay;
    fMainMenuSubj: TCEMainMenuSubject;
    procedure updateMainMenuProviders;

    // action provider handling;
    procedure clearActProviderEntries;
    procedure collectedActProviderEntries;

    // ICEMultiDocObserver
    procedure docNew(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);

    // ICEEditableShortcut
    function scedWantFirst: boolean;
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);

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
    procedure InitDocking;
    procedure InitSettings;
    procedure SaveSettings;
    procedure LoadDocking;
    procedure SaveDocking;
    procedure FreeRunnableProc;

    // widget interfaces subroutines
    procedure widgetShowFromAction(sender: TObject);

    // run & exec sub routines
    procedure asyncprocOutput(sender: TObject);
    procedure asyncprocTerminate(sender: TObject);
    procedure compileAndRunFile(unittest: boolean = false; redirect: boolean = true;
      const runArgs: string = '');

    // file sub routines
    procedure newFile;
    procedure saveFile(aDocument: TCESynMemo);
    procedure openFile(const aFilename: string);

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
  end;

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
  fMainMenuSubj := TCEMainMenuSubject.create;
  fActionHandler := TCEActionProviderSubject.create;
  //
  EntitiesConnector.addObserver(self);
  //
  InitMRUs;
  InitWidgets;
  InitDocking;
  InitSettings;
  layoutUpdateMenu;
  fMultidoc := getMultiDocHandler;
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
  lst: TStringList;
  hdl: THandle;
  str: string;
begin
  if application.ParamCount > 0 then
  begin
    value := application.Params[1];
    if value <> '' then
    begin
      setLength(str, 6);
      lst := TStringList.Create;
      try
        lst.DelimitedText := value;
        for value in lst do
        begin
          if not fileExists(value) then
            continue;
          hdl := FileOpen(value, fmOpenRead);
          if hdl = 0 then
            continue;
          FileRead(hdl, str[1], length(str));
          FileClose(hdl);
          if str = 'object' then
            openProj(value)
          else
            openFile(value);
        end;
      finally
        lst.Free;
      end;
    end;
  end;
  value := application.GetOptionValue('p', 'project');
  if (value <> '') and fileExists(value) then
    openProj(value);
  value := application.GetOptionValue('f', 'files');
  if value <> '' then
  begin
    lst := TStringList.Create;
    try
      lst.DelimitedText := value;
      for value in lst do
      begin
        if fileExists(value) then
          openFile(value);
      end;
    finally
      lst.Free;
    end;
  end;
end;

procedure TCEMainForm.InitMRUs;
begin
  fProjMru := TCEMRUProjectList.Create;
  fFileMru := TCEMRUDocumentList.Create;
  fProjMru.objectTag := mnuItemMruProj;
  fFileMru.objectTag := mnuItemMruFile;
  fProjMru.OnChange := @mruChange;
  fFileMru.OnChange := @mruChange;
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
  fFindWidg := TCESearchWidget.create(self);
  fExplWidg := TCEMiniExplorerWidget.create(self);
  fLibMWidg := TCELibManEditorWidget.create(self);
  fTlsEdWidg:= TCEToolsEditorWidget.create(self);
  fPrInpWidg:= TCEProcInputWidget.create(self);
  fTodolWidg:= TCETodoListWidget.create(self);
  fOptEdWidg:= TCEOptionEditorWidget.create(self);
  fSymlWidg := TCESymbolListWidget.create(self);

  getMessageDisplay(fMsgs);

  {$IFDEF WIN32}
  fCdbWidg  := TCECdbWidget.create(self);
  {$ENDIF}

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);
  fWidgList.addWidget(@fPrjCfWidg);
  fWidgList.addWidget(@fFindWidg);
  fWidgList.addWidget(@fExplWidg);
  fWidgList.addWidget(@fLibMWidg);
  fWidgList.addWidget(@fTlsEdWidg);
  fWidgList.addWidget(@fPrInpWidg);
  fWidgList.addWidget(@fTodolWidg);
  fWidgList.addWidget(@fOptEdWidg);
  fWidgList.addWidget(@fSymlWidg);

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
  i: Integer;
  widg: TCEWidget;
  aManager: TAnchorDockManager;
begin
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);
  DockMaster.OnShowOptions := @ShowAnchorDockOptions;
  DockMaster.HeaderStyle := adhsPoints;
  DockMaster.HideHeaderCaptionFloatingControl := true;

  // this is a fix copied from Laz, seems to force the space between the menu and the UI stay 0.
  if DockManager is TAnchorDockManager then begin
    aManager:=TAnchorDockManager(DockManager);
    aManager.PreferredSiteSizeAsSiteMinimum:=false;
  end;

  // makes widget dockable
  for i := 0 to fWidgList.Count-1 do
  begin
    widg := fWidgList.widget[i];
    if not widg.isDockable then continue;
    DockMaster.MakeDockable(widg, true);
    DockMaster.GetAnchorSite(widg).Header.HeaderPosition := adlhpTop;
  end;

  // load existing or default docking
  if FileExists(getCoeditDocPath + 'docking.xml') then LoadDocking
  else begin
    Height := 0;
    // center
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fEditWidg), DockMaster.GetSite(Self), alBottom);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fMesgWidg), DockMaster.GetSite(fEditWidg), alBottom);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fPrInpWidg), DockMaster.GetSite(fMesgWidg), alBottom);
    // left
    DockMaster.GetAnchorSite(fSymlWidg).Width := 200;
    DockMaster.GetAnchorSite(fFindWidg).Width := 200;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fSymlWidg), DockMaster.GetSite(fEditWidg), alLeft);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fFindWidg), DockMaster.GetAnchorSite(fSymlWidg), alBottom, fSymlWidg);
    // right
    DockMaster.GetAnchorSite(fProjWidg).Width := 250;
    DockMaster.GetAnchorSite(fPrjCfWidg).Width := 250;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fProjWidg), DockMaster.GetSite(fEditWidg), alRight);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(fPrjCfWidg), DockMaster.GetAnchorSite(fProjWidg), alBottom, fProjWidg);
    // close remaining and header to top
    for i := 0 to fWidgList.Count-1 do
    begin
      widg := fWidgList.widget[i];
      if not widg.isDockable then continue;
      DockMaster.GetAnchorSite(widg).Header.HeaderPosition := adlhpTop;
      if not DockMaster.GetAnchorSite(widg).HasParent then
        DockMaster.GetAnchorSite(widg).Close
    end;
  end;
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
  if not fInitialized then exit;
  if not Visible then exit;
  //
  if WindowState = wsMinimized then WindowState := wsNormal;
  // does not save minimized/undocked windows to prevent bugs
  for i:= 0 to fWidgList.Count-1 do
  begin
    if not fWidgList.widget[i].isDockable then continue;
    if DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState = wsMinimized then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close
    else if not DockMaster.GetAnchorSite(fWidgList.widget[i]).HasParent then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close;
  end;
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
  fWidgList.Free;
  fProjMru.Free;
  fFileMru.Free;
  fProject.Free;
  FreeRunnableProc;
  //
  fMainMenuSubj.Free;
  fActionHandler.Free;
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
    fMsgs.message(E.Message, nil, amcApp, amkErr);
end;

procedure TCEMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  canClose := false;
  if fProject <> nil then if fProject.modified then
    if ce_common.dlgOkCancel(
      'last project modifications are not saved, quit anyway ?') <> mrOK then
          exit;
  for i := fMultidoc.documentCount-1 downto 0 do
    if not fMultidoc.closeDocument(i) then exit;
  canClose := true;

  // saving doesnt work when csDestroying in comp.state (in Free)
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


  clearActProviderEntries;
  collectedActProviderEntries;

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
      actFileUnittest.Enabled := fDoc.isDSource;
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
      actFileUnittest.Enabled := false;
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

procedure TCEMainForm.mruChange(Sender: TObject);
var
  srcLst: TCEMruFileList;
  trgMnu: TMenuItem;
  itm: TMenuItem;
  fname: string;
  clickTrg: TNotifyEvent;
  i: NativeInt;
begin
  srcLst := TCEMruFileList(Sender);
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
  srcLst: TCEMruFileList;
begin
  srcLst := TCEMruFileList(TmenuItem(Sender).Tag);
  if srcLst = nil then exit;
  //
  srcLst.Clear;
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

{$REGION ICEEditableShortCut ---------------------------------------------------}
function TCEMainForm.scedWantFirst: boolean;
begin
  fScCollectCount := 0;
  result := true;
end;

function TCEMainForm.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
var
  act: TCustomAction;
begin
  act := TCustomAction(Actions.Actions[fScCollectCount]);
  category := act.Category;
  identifier := act.Caption;
  aShortcut := act.ShortCut;
  //
  fScCollectCount += 1;
  result := fScCollectCount < actions.ActionCount;
end;

procedure TCEMainForm.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
begin

end;
{$ENDREGION}

{$REGION TCEActionProviderHandler ----------------------------------------------}
procedure TCEMainForm.clearActProviderEntries;
var
  prov: ICEActionProvider;
  act: TContainedAction;
  i, j: Integer;
begin
  for i:= 0 to fActionHandler.observersCount-1 do
  begin
    prov := fActionHandler[i] as ICEActionProvider;
    if not prov.actHandlerWantRecollect then continue;
    //
    for j := Actions.ActionCount-1 downto 0 do
    begin
      act := Actions.Actions[j];
      if act.Owner = Self then continue;
      if act.Tag <> PtrInt(prov) then continue;
      //
      act.ActionList := nil;
    end;
  end;
end;

procedure TCEMainForm.collectedActProviderEntries;
var
  prov: ICEActionProvider;
  act: TCustomAction;
  cat: string;
  i: Integer;
  procedure addAction;
  begin
    act.ActionList := Actions;
    act.Tag := ptrInt(prov);
    act.Category := cat;
    //
    act := nil;
    cat := '';
  end;

begin
  for i:= 0 to fActionHandler.observersCount-1 do
  begin
    prov := fActionHandler[i] as ICEActionProvider;
    if not prov.actHandlerWantFirst then continue;
    //
    act := nil;
    cat := '';
    while prov.actHandlerWantNext(cat, act) do
      addAction;
    addAction;
  end;
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
  TCESynMemo.Create(nil);
end;

procedure TCEMainForm.openFile(const aFilename: string);
begin
  fMultidoc.openDocument(aFilename);
end;

procedure TCEMainForm.saveFile(aDocument: TCESynMemo);
begin
  if aDocument.Highlighter = LfmSyn then
    saveProjSource(aDocument)
  else if aDocument.fileName <> '' then
    aDocument.save;
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
  fDoc.setFocus;
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
  fDoc.setFocus;
end;

procedure TCEMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  //
  with TSaveDialog.Create(nil) do
  try
    Filter := DdiagFilter;
    if execute then
      fDoc.saveToFile(filename);
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
    saveFile(fDoc)
  else actFileSaveAs.Execute;
end;

procedure TCEMainForm.actFileAddToProjExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  if fDoc.isProjectSource then exit;
  if fProject = nil then exit;
  //
  if fileExists(fDoc.fileName) then
    fProject.addSource(fDoc.fileName)
  else dlgOkInfo('the file has not been added to the project because it does not exist');
end;

procedure TCEMainForm.actFileCloseExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  if fDoc.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?') = mrCancel
      then exit;
  //
  fDoc.Free;
end;

procedure TCEMainForm.actFileSaveAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to fMultidoc.documentCount-1 do
    saveFile(fMultidoc.document[i]);
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
      fMsgs.message(str, fDoc, amcEdit, amkBub)
    else if proc.Executable = DCompiler then
      for str in lst do
        fMsgs.message(str, fDoc, amcEdit, amkAuto);
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.asyncprocTerminate(sender: TObject);
var
  proc: TProcess;
  lst: TStringList;
  str: string;
  inph: TObject;
begin
  proc := TProcess(sender);
  lst := TStringList.Create;
  try
    processOutputToStrings(proc, lst);
    // runnable module
    if proc = fRunProc then
    begin
      for str in lst do
        fMsgs.message(str, fDoc, amcEdit, amkBub);
    end;
  finally
    lst.Free;
  end;
  //if proc = fPrInpWidg.process then
    //fPrInpWidg.process := nil;

  inph := EntitiesConnector.getSingleService('ICEProcInputHandler');
  if (inph <> nil) then (inph as ICEProcInputHandler).removeProcess(proc);
end;

procedure TCEMainForm.compileAndRunFile(unittest: boolean = false; redirect: boolean = true;
	const runArgs: string = '');
var
  dmdproc: TProcess;
  fname: string;
begin

  FreeRunnableProc;
  if fDoc = nil then exit;

  fRunProc := TCheckedAsyncProcess.Create(nil);
  if redirect then
  begin
  	fRunProc.Options := [poStderrToOutPut, poUsePipes];
  	fRunProc.ShowWindow := swoHIDE;
  	fRunProc.OnReadData := @asyncprocOutput;
  	fRunProc.OnTerminate:= @asyncprocTerminate;
  end else
  begin
    {$IFDEF LINUX}
    fRunProc.Options := fRunProc.Options + [poNewConsole];
    {$ENDIF}
  end;

  dmdproc := TProcess.Create(nil);
  try

    fMsgs.clearByData(fDoc);
    fMsgs.message('compiling ' + shortenPath(fDoc.fileName, 25), fDoc, amcEdit, amkInf);

    if fileExists(fDoc.fileName) then fDoc.save
    else fDoc.saveTempFile;
    fname := stripFileExt(fDoc.fileName);

    if fRunnableSw = '' then
      fRunnableSw := '-vcolumns'#13'-w'#13'-wi';
    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options := [poStdErrToOutput, poUsePipes];
    dmdproc.Executable := DCompiler;
    dmdproc.Parameters.Add(fDoc.fileName);
    dmdproc.Parameters.AddText(fRunnableSw);
    if unittest then
    begin
      dmdproc.Parameters.Add('-main');
      dmdproc.Parameters.Add('-unittest');
    end
    else dmdproc.Parameters.Add('-version=runnable_module');
    dmdproc.Parameters.Add('-of' + fname + exeExt);
    LibMan.getLibFiles(nil, dmdproc.Parameters);
    LibMan.getLibSources(nil, dmdproc.Parameters);
    dmdproc.Execute;
    while dmdproc.Running do asyncprocOutput(dmdProc);

    if (dmdProc.ExitStatus = 0) then
    begin
      fMsgs.message(shortenPath(fDoc.fileName, 25) + ' successfully compiled',
        fDoc, amcEdit, amkInf);
      fRunProc.CurrentDirectory := extractFilePath(fRunProc.Executable);
      if runArgs <> '' then
        fRunProc.Parameters.DelimitedText := symbolExpander.get(runArgs);
      fRunProc.Executable := fname + exeExt;
      if redirect then
      	getprocInputHandler.addProcess(fRunProc);
      fRunProc.Execute;
      sysutils.DeleteFile(fname + objExt);
    end
    else begin
      fMsgs.message(shortenPath(fDoc.fileName,25) + ' has not been compiled',
        fDoc, amcEdit, amkErr);
    end;

  finally
    dmdproc.Free;
  end;
end;

procedure TCEMainForm.actFileUnittestExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  compileAndRunFile(true);
end;

procedure TCEMainForm.actFileCompAndRunExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  compileAndRunFile(false);
end;

procedure TCEMainForm.actFileCompileAndRunOutExecute(Sender: TObject);
begin
  if fDoc = nil then exit;
  compileAndRunFile(false, false);
end;

procedure TCEMainForm.actFileCompAndRunWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  if fDoc = nil then exit;
  runargs := '';
  if InputQuery('Execution arguments', '', runargs) then
    compileAndRunFile(false, true, runargs);
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
  //
  if widg.isDockable then
  begin
    win := DockMaster.GetAnchorSite(widg);
    win.Show;
    win.BringToFront;
  end
  else begin
    if widg.isModal then widg.ShowModal else
    begin
      widg.Show;
      widg.BringToFront;
    end;
  end;
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
  // TODO-cbugfix: possible loading AV, xml saved after undocking some widgets, xml file abnormal size, seems to be related to Anchordocking itself, not its usage.
  for i:= 0 to fWidgList.Count-1 do
  begin
    if not fWidgList.widget[i].isDockable then continue;
    if DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState = wsMinimized then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close
    else if not DockMaster.GetAnchorSite(fWidgList.widget[i]).HasParent then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).Close;
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
end;

procedure TCEMainForm.openProj(const aFilename: string);
begin
  closeProj;
  newProj;
  fProject.loadFromFile(aFilename);
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

{$REGION ICESessionOptionsObserver ---------------------------------------------}
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

initialization
  RegisterClasses([TCEOptions]);
end.
