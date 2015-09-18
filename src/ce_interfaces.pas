unit ce_interfaces;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, actnList, menus, process,
  ce_synmemo, ce_observer;

type

  // describes the project kind. Used as a hint to cast ICECommonProject.getProject()
  TCEProjectFormat = (pfNative, pfDub);

  // describes the binary kind produces when compiling a project
  TProjectBinaryKind = (executable, staticlib, sharedlib, obj);

  (**
   * Common project interface.
   *
   * Each project format has its own dedicated editors.
   * A few common properties allow some generic operations whatever is the format.
   *)
  ICECommonProject = interface
  ['ICECommonProject']

    // general properties ------------------------------------------------------

      // indicates the project format
      function getFormat: TCEProjectFormat;
      // returns an untyped object that can be casted using getFormat()
      function getProject: TObject;
      // returns the project filename
      function filename: string;
      // loads project from filename
      procedure loadFromFile(const aFilename: string);
      // saves project to filename
      procedure saveToFile(const aFilename: string);
      // indicates of the project is modified (should be saved or not)
      function modified: boolean;
      // returns the base path used to solve relative locations
      function basePath: string;
      // returns the name of the file produced when a project is compiled
      function outputFilename: string;
      // returns the binary kind produced according to the current configuration
      function binaryKind: TProjectBinaryKind;
      // returns what's gonna be executed in background for this config
      function getCommandLine: string;

    // configs -----------------------------------------------------------------

      // returns the count of configuration
      function configurationCount: integer;
      // sets the active configuration
      procedure setActiveConfiguration(index: integer);
      // returns the name of the index-th configuration
      function configurationName(index: integer): string;

    // project sources ---------------------------------------------------------

      // returns the count of source files for the current config
      function sourcesCount: integer;
      // returns the source absolute filename.
      function sourceAbsolute(index: integer): string;
      // returns the source relative filename.
      function sourceRelative(index: integer): string;
      // returns true if aFilename is a project source.
      function isSource(const aFilename: string): boolean;
      // returns the count of import paths for the current config
      function importsPathCount: integer;
      // returns the import absolute path
      function importPath(index: integer): string;

    // sub routines for the actions --------------------------------------------

      // tries to compile and returns true if it does
      function compile: boolean;
      // tries to un the project output and returns true if it did
      function run(const runArgs: string = ''): boolean;
      // returns true if the target has not to be recompiled
      function targetUpToDate: boolean;

  end;



  (**
   * An implementer declares some actions on demand.
   *)
  ICEContextualActions = interface
  ['ICEContextualActions']
    // declares a context name for the actions
    function contextName: string;
    // action count, called before contextAction()
    function contextActionCount: integer;
    // declares actions, called in loop, from 0 to contextActionCount-1
    function contextAction(index: integer): TAction;
  end;



  (**
   * An implementer is informed about the current file(s).
   *)
  ICEMultiDocObserver = interface
  ['ICEMultiDocObserver']
    // aDoc has been created (empty, runnable, project source, ...).
    procedure docNew(aDoc: TCESynMemo);
    // aDoc is the document being edited.
    procedure docFocused(aDoc: TCESynMemo);
    // aDoc content has just been modified (edited, saved).
    procedure docChanged(aDoc: TCESynMemo);
    // aDoc is about to be closed.
    procedure docClosing(aDoc: TCESynMemo);
  end;
  (**
   * An implementer informs some ICEMultiDocObserver about the current file(s)
   *)
  TCEMultiDocSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer is informed about the current project(s).
   *)
  ICEProjectObserver = interface
  ['ICEProjectObserver']
    // aProject has been created/opened
    procedure projNew(aProject: ICECommonProject);
    // aProject has been modified: switches, source name, ...
    procedure projChanged(aProject: ICECommonProject);
    // aProject is about to be closed.
    procedure projClosing(aProject: ICECommonProject);
    // not called yet: aProject is always the same
    procedure projFocused(aProject: ICECommonProject);
    // aProject is about to be compiled
    procedure projCompiling(aProject: ICECommonProject);
  end;
  (**
   * An implementer informs some ICEProjectObserver about the current project(s)
   *)
  TCEProjectSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer can add a main menu entry.
   *)
  ICEMainMenuProvider = interface
  ['ICEMainMenuProvider']
    // item is a new mainMenu entry. item must be filled with the sub-items to be added.
    procedure menuDeclare(item: TMenuItem);
    // item is the mainMenu entry declared previously. the sub items can be updated, deleted.
    procedure menuUpdate(item: TMenuItem);
  end;
  (**
   * An implementer collects and updates its observers menus.
   *)
  TCEMainMenuSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer declares some actions which have their own main menu entry and
   * whose shortcuts are automatically handled
   *)
  ICEActionProvider = interface
  ['ICEActionProvider']
    // the action handler will clear the references to the actions collected previously and start collecting if result.
    function actHandlerWantRecollect: boolean;
    // the action handler starts to collect the actions if result.
    function actHandlerWantFirst: boolean;
    // the handler continue collecting action if result.
    function actHandlerWantNext(out category: string; out action: TCustomAction): boolean;
    // the handler update the state of a particular action.
    procedure actHandleUpdater(action: TCustomAction);
  end;
  (**
   * An implementer handles its observers actions.
   *)
  TCEActionProviderSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer can expose some customizable shortcuts to be edited in a dedicated widget.
   *)
  ICEEditableShortCut = interface
  ['ICEEditableShortCut']
    // a TCEEditableShortCutSubject will start to collect shortcuts if result
    function scedWantFirst: boolean;
    // a TCEEditableShortCutSubject collects the information on the shortcuts while result
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    // a TCEEditableShortCutSubject sends the possibly modified shortcut
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
  end;
  (**
   * An implementer manages its observers shortcuts.
   *)
  TCEEditableShortCutSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  // the option editor uses this value as a hint to cast and display an option container.
  TOptionEditorKind = (oekGeneric, oekForm, oekControl);
  // event generated by the option editor and passed to an ICEEditableOptions.
  // the oeeChange event only happends if the container is oekGeneric.
  TOptionEditorEvent = (oeeCancel, oeeAccept, oeeChange, oeeSelectCat);
  (**
   * An implementer can expose some options to be edited in a dedicated widget.
   *)
  ICEEditableOptions = interface
  ['ICEEditableOptions']
    // the widget wants the category.
    function optionedWantCategory(): string;
    // the widget wants to know if the options will use a generic editor or a custom form.
    function optionedWantEditorKind: TOptionEditorKind;
    // the widget wants the custom option editor TCustomForm, TWinControl or the TPersistent containing the options.
    function optionedWantContainer: TPersistent;
    // the option editor informs that something has happened.
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    // the option editor wants to know if an editor allows another category to be displayed (not called for oekGeneric).
    function optionedOptionsModified: boolean;
  end;
  (**
   * An implementer displays its observers editable options.
   *)
  TCEEditableOptionsSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  /// describes the message kind, 'amkAuto' implies that an ICELogMessageObserver guess the kind.
  TCEAppMessageKind = (amkAuto, amkBub, amkInf, amkHint, amkWarn, amkErr);
  /// describes the message context. Used by a ICELogMessageObserver to filter the messages.
  TCEAppMessageCtxt = (amcAll, amcEdit, amcProj, amcApp, amcMisc);

  (**
   * Single service provided by the messages widget.
   *)
  ICEMessagesDisplay = interface(ICESingleService)
    // displays a message
    procedure message(const aValue: string; aData: Pointer; aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
    // clears the messages related to the context aCtxt.
    procedure clearByContext(aCtxt: TCEAppMessageCtxt);
    // clears the messages related to the data aData.
    procedure clearByData(aData: Pointer);
  end;



  (**
   * Single service provided by the process-input widget.
   *)
  ICEProcInputHandler = interface(ICESingleService)
    // add an entry to the list of process which can receive an user input.
    procedure addProcess(aProcess: TProcess);
    // removes an entry.
    procedure removeProcess(aProcess: TProcess);
    // indicates the current process
    function process: TProcess;
  end;



  (**
   * Single service related to the documents as a collection.
   *)
  ICEMultiDocHandler = interface(ICESingleService)
    // returns the count of opened document
    function documentCount: Integer;
    // returns the index-th document
    function getDocument(index: Integer): TCESynMemo;
    // returns true if the document matching aFielanme is already opened.
    function findDocument(aFilename: string): TCESynMemo;
    // open or set the focus on the document matching aFilename
    procedure openDocument(aFilename: string);
    // close the index-th document
    function closeDocument(index: Integer): boolean;
    // conveniance property
    property document[index: integer]: TCESynMemo read getDocument;
  end;



  (**
   * Single service provided by the library manager
   * In both cases, if someAliases is empty then all the available entries are passed.
   *)
  ICELibraryInformer = interface(ICESingleService)
    // fills aList with the filenames of the static libraries matching to someAliases content.
    procedure getLibsFiles(someAliases: TStrings; aList: TStrings);
    // fills aList with the path to static libraries sources matching to someAliases content.
    procedure getLibsPaths(someAliases: TStrings; aList: TStrings);
    // fills aList with all the available libraries aliases.
    procedure getLibsAliases(aList: TStrings);
  end;



  (**
   * Single service that allows objects with a short life-time
   * to get the project information.
   *)
  //ICEProjectInfos = interface(ICESingleService)
  //  function getCurrentProjectInterface: ICECommonProject;
  //end;


{
  subject primitives:

  A subject cannot necessarly provides all the informations the observers expect.
  It can compose using the following "primitives".
}

  (**
   * TCEMultiDocSubject primitives.
   *)
  procedure subjDocNew(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);      {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjDocClosing(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);  {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjDocFocused(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);  {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjDocChanged(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);  {$IFDEF RELEASE}inline;{$ENDIF}

  (**
   * TCEProjectSubject primitives.
   *)
  procedure subjProjNew(aSubject: TCEProjectSubject; aProj: ICECommonProject);     {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjClosing(aSubject: TCEProjectSubject; aProj: ICECommonProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjFocused(aSubject: TCEProjectSubject; aProj: ICECommonProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjChanged(aSubject: TCEProjectSubject; aProj: ICECommonProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjCompiling(aSubject: TCEProjectSubject; aProj: ICECommonProject);{$IFDEF RELEASE}inline;{$ENDIF}


{
  Service getters:

  The first overload assign the variable only when not yet set, the second is
  designed for a punctual usage, for example if a widget needs the service in
  a single and rarely called method.
}

  function getMessageDisplay(var obj: ICEMessagesDisplay): ICEMessagesDisplay; overload;
  function getMessageDisplay: ICEMessagesDisplay; overload;

  function getprocInputHandler(var obj: ICEProcInputHandler): ICEProcInputHandler; overload;
  function getprocInputHandler: ICEProcInputHandler; overload;

  function getMultiDocHandler(var obj: ICEMultiDocHandler): ICEMultiDocHandler; overload;
  function getMultiDocHandler: ICEMultiDocHandler; overload;

  function getLibraryInformer(var obj: ICELibraryInformer): ICELibraryInformer; overload;
  function getLibraryInformer: ICELibraryInformer; overload;

implementation

{$REGION TCEMultiDocSubject ----------------------------------------------------}
function TCEMultiDocSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEMultiDocObserver);
end;

procedure subjDocNew(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docNew(aDoc);
end;

procedure subjDocClosing(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docClosing(aDoc);
end;

procedure subjDocFocused(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docFocused(aDoc);
end;

procedure subjDocChanged(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docChanged(aDoc);
end;
{$ENDREGION}

{$REGION TCEProjectSubject -----------------------------------------------------}
function TCEProjectSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEProjectObserver);
end;

procedure subjProjNew(aSubject: TCEProjectSubject; aProj: ICECommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).ProjNew(aProj);
end;

procedure subjProjClosing(aSubject: TCEProjectSubject; aProj: ICECommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projClosing(aProj);
end;

procedure subjProjFocused(aSubject: TCEProjectSubject; aProj: ICECommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projFocused(aProj);
end;

procedure subjProjChanged(aSubject: TCEProjectSubject; aProj: ICECommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projChanged(aProj);
end;

procedure subjProjCompiling(aSubject: TCEProjectSubject; aProj: ICECommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projCompiling(aProj);
end;
{$ENDREGION}

{$REGION Misc subjects ---------------------------------------------------------}
function TCEMainMenuSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEMainMenuProvider);
end;

function TCEEditableShortCutSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEEditableShortCut);
end;

function TCEEditableOptionsSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEEditableOptions);
end;

function TCEActionProviderSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEActionProvider);
end;
{$ENDREGION}

{$REGION ICESingleService getters ----------------------------------------------}
function getMessageDisplay(var obj: ICEMessagesDisplay): ICEMessagesDisplay;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('ICEMessagesDisplay') as ICEMessagesDisplay;
  exit(obj);
end;

function getMessageDisplay: ICEMessagesDisplay;
begin
  exit(EntitiesConnector.getSingleService('ICEMessagesDisplay') as ICEMessagesDisplay);
end;

function getprocInputHandler(var obj: ICEProcInputHandler): ICEProcInputHandler;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('ICEProcInputHandler') as ICEProcInputHandler;
  exit(obj);
end;

function getprocInputHandler: ICEProcInputHandler;
begin
  exit(EntitiesConnector.getSingleService('ICEProcInputHandler') as ICEProcInputHandler);
end;

function getMultiDocHandler(var obj: ICEMultiDocHandler): ICEMultiDocHandler;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('ICEMultiDocHandler') as ICEMultiDocHandler;
  exit(obj);
end;

function getMultiDocHandler: ICEMultiDocHandler;
begin
  exit(EntitiesConnector.getSingleService('ICEMultiDocHandler') as ICEMultiDocHandler);
end;

function getLibraryInformer(var obj: ICELibraryInformer): ICELibraryInformer;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('ICELibraryInformer') as ICELibraryInformer;
  exit(obj);
end;

function getLibraryInformer: ICELibraryInformer;
begin
  exit(EntitiesConnector.getSingleService('ICELibraryInformer') as ICELibraryInformer);
end;
{$ENDREGION}

end.
