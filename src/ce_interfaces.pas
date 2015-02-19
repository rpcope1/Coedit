unit ce_interfaces;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, actnList, menus, process,
  ce_synmemo, ce_project, ce_observer;

type

  (**
   * An implementer can save and load some stuffs on application start/quit
   *)
  ICESessionOptionsObserver = interface
  ['ICESessionOptionsObserver']
    // persistent things are about to be saved.
    procedure sesoptBeforeSave;
    // persistent things can be declared to aFiler.
    procedure sesoptDeclareProperties(aFiler: TFiler);
    // persistent things have just been reloaded.
    procedure sesoptAfterLoad;
  end;
  (**
   * An implementer gets and gives back some things
   *)
  TCESessionOptionsSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
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
    procedure projNew(aProject: TCEProject);
    // aProject has been modified: switches, source name, ...
    procedure projChanged(aProject: TCEProject);
    // aProject is about to be closed.
    procedure projClosing(aProject: TCEProject);
    // not called yet: aProject is always the same
    procedure projFocused(aProject: TCEProject);
    // aProject is about to be compiled
    procedure projCompiling(aProject: TCEProject);
  end;
  (**
   * An implementer informs some ICEProjectObserver about the current project(s)
   *)
  TCEProjectSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer can add a mainmenu entry.
   *)
  ICEMainMenuProvider = interface
  ['ICEMainMenuProvider']
    // item is a new mainMenu entry. item must be filled with the sub-items to be added.
    procedure menuDeclare(item: TMenuItem);
    // item is the mainMenu entry declared previously. the sub items can be updated, deleted.
    procedure menuUpdate(item: TMenuItem);
  end;
  (**
   * An implementer agregates its observers menus.
   *)
  TCEMainMenuSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer can expose some customizable shortcuts to be edited in a dedicated widget.
   *)
  ICEEditableShortCut = interface
  ['ICEEditableShortCut']
    // a TCEEditableShortCutSubject queries the editable shortcuts count.
    procedure scGetCount(out aValue: Integer);
    // a TCEEditableShortCutSubject queries the shortcut category name.
    procedure scGetCategory(out aValue: string);
    // a TCEEditableShortCutSubject queries the state of the index-th shortcut.
    procedure scGetItem(index: Integer; out aName: string; out aShortcut: Word);
    // a TCEEditableShortCutSubject sends the possibly modified assignation of the index-th shortcut.
    procedure scSetItem(index: Integer; const aCategory, aName: string; aShortcut: Word);
  end;
  (**
   * An implementer manages its observers shortcuts.
   *)
  TCEEditableShortCutSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  TOptionEditorKind = (oekGeneric, oekForm);
  TOptionEditorEvent = (oeeCancel, oeeAccept, oeeChange);
  (**
   * An implementer can expose some options to be edited in a dedicated widget.
   *)
  ICEEditableOptions = interface
  ['ICEEditableOptions']
    // the widget wants the category
    function optionedWantCategory(): string;
    // the widget wants to know if the options will use a generic editor or a custom form
    function optionedWantEditorKind: TOptionEditorKind;
    // the widget wants the custom option editor form or the TPersistent containing the options
    function optionedWantContainer: TPersistent;
    // the option editor informs that something has happened
    procedure optionedEvent(anEvent: TOptionEditorEvent);
  end;
  (**
   * An implementer displays its observers editable options.
   *)
  TCEEditableOptionsSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  /// describes the message kind, when Auto implies that a ICELogMessageObserver guess the kind.
  TCEAppMessageKind = (amkAuto, amkBub, amkInf, amkHint, amkWarn, amkErr);
  /// describes the message context. Used by a ICELogMessageObserver to filter the messages.
  TCEAppMessageCtxt = (amcAll, amcEdit, amcProj, amcApp, amcMisc);

  (**
   * Single service given by the messages widget.
   *)
  ICEMessagesDisplay = interface(ICESingleService)
    // display a message
    procedure message(const aValue: string; aData: Pointer; aCtxt: TCEAppMessageCtxt; aKind: TCEAppMessageKind);
    // clear the messages related to the context aCtxt.
    procedure clearByContext(aCtxt: TCEAppMessageCtxt);
    // clear the messages related to the data aData.
    procedure clearByData(aData: Pointer);
  end;



  (**
   * Single service given by the process-input widget.
   *)
  ICEProcInputHandler = interface(ICESingleService)
    // add an entry to the list of process which can receive an user input
    procedure addProcess(aProcess: TProcess);
    // remove an entry
    procedure removeProcess(aProcess: TProcess);
  end;



  (**
   * Single service related to the collection of document
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


{
  subject Primitives:

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
  procedure subjProjNew(aSubject: TCEProjectSubject; aProj: TCEProject);     {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjClosing(aSubject: TCEProjectSubject; aProj: TCEProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjFocused(aSubject: TCEProjectSubject; aProj: TCEProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjChanged(aSubject: TCEProjectSubject; aProj: TCEProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjCompiling(aSubject: TCEProjectSubject; aProj: TCEProject);{$IFDEF RELEASE}inline;{$ENDIF}
  (**
   * TCESessionOptionsSubject primitives.
   *)
  procedure subjSesOptsBeforeSave(aSubject: TCESessionOptionsSubject);                       {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjSesOptsDeclareProperties(aSubject: TCESessionOptionsSubject; aFiler: TFiler);{$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjSesOptsAfterLoad(aSubject: TCESessionOptionsSubject);                        {$IFDEF RELEASE}inline;{$ENDIF}


{
  Service getters:

  Lazily get the interface of a service when needed or for a punctual usage.
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

procedure subjProjNew(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).ProjNew(aProj);
end;

procedure subjProjClosing(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projClosing(aProj);
end;

procedure subjProjFocused(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projFocused(aProj);
end;

procedure subjProjChanged(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projChanged(aProj);
end;

procedure subjProjCompiling(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projCompiling(aProj);
end;
{$ENDREGION}

{$REGION TCESessionOptionsSubject ----------------------------------------------}
function TCESessionOptionsSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICESessionOptionsObserver);
end;

procedure subjSesOptsBeforeSave(aSubject: TCESessionOptionsSubject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICESessionOptionsObserver).sesoptBeforeSave;
end;

procedure subjSesOptsDeclareProperties(aSubject: TCESessionOptionsSubject; aFiler: TFiler);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICESessionOptionsObserver).sesoptDeclareProperties(aFiler);
end;

procedure subjSesOptsAfterLoad(aSubject: TCESessionOptionsSubject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICESessionOptionsObserver).sesoptAfterLoad;
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
{$ENDREGION}

end.
