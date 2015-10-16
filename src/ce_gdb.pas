unit ce_gdb;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, Buttons, ComCtrls, StdCtrls, process, ce_common,
  ce_interfaces, ce_widget, ce_processes, ce_observer, ce_synmemo, ce_sharedres;

type

  {$IFDEF CPU64}
  TCpuRegs = (rax);
  {$ENDIF}

  {$IFDEF CPU32}
  TCpuRegs = (eax);
  {$ENDIF}

  //TODO-cDebugging: write a parser for the DBG/MI output messages


  TGDBMI_Frame = record
    level: integer;
    func: string;
    adrress: ptruint;
    fname: string;  // named "file"
    line: integer;
    from: string;
  end;

  TGDBMI_Breakpoint = record
    number: integer;
    tpe: string;        // named "type"
    catchtype: string;  // named "catch-type"
    disp: string;       // "del" | "keep"
    enabled: boolean;   // "y" | "n"
    addr: ptrUint;      // hex | <PENDING> | <MULTIPLE>
    func: string;
    filename: string;
    fullname: string;
    line: integer;
    at: string;
    pending: string;    // value is the command passed to set the BP
    evaluateby: string; // named "evaluate-by" , host | target
    thread: ptrUint;
    task: string;
    cond: string;
    ignore: integer;
    enable: integer;
    traceframeusage: string;// named "traceframe-usage"
    statictraceid: string;  // named "static-tracepoint-marker-string-id"
    mask: string;
    pass: integer;
    originloc: string; // named "original-location"
    times: integer;
    installed: boolean; // "y" | "n" , only for trace points
    what: string;
  end;

  TGDBMI_Thread = record
    id: ptrUint;
    targetid: string; // named "target-id"
    details: string;
    state: string;    // running | stopped
    core: integer;
  end;


  { TCEGdbWidget }
  TCEGdbWidget = class(TCEWidget, ICEProjectObserver, ICEMultiDocObserver)
    btnSendCom: TBitBtn;
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    btnCont: TBitBtn;
    Edit1: TEdit;
    lstfilter: TListFilterEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TreeView1: TTreeView;
    procedure btnContClick(Sender: TObject);
    procedure btnSendComClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fProj: ICECommonProject;
    fLog: TStringList;
    fFileLineBrks: TStringList;
    fDocHandler: ICEMultiDocHandler;
    fMsg: ICEMessagesDisplay;
    fGdb: TCEProcess;
    fRegs: array[TCpuRegs] of ptrUint;
    //
    procedure startDebugging;
    procedure killGdb;
    procedure updateFileLineBrks;
    procedure editorModBrk(sender: TCESynMemo; line: integer; modification: TBreakPointModification);
    // GDB output processors
    procedure processInfoRegs(sender: TObject);
    procedure processInfoStack(sender: TObject);
    procedure processSilently(sender: TObject);
    procedure gdbOutput(sender: TObject);
    // GDB commands & actions
    procedure gdbCommand(aCommand: string; outputCatcher: TNotifyEvent = nil);
    procedure infoRegs;
    procedure infoStack;

    //
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
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

{$REGION Common/standard comp --------------------------------------------------}
constructor TCEGdbWidget.create(aOwner: TComponent);
begin
  inherited;
  EntitiesConnector.addObserver(self);
  fDocHandler:= getMultiDocHandler;
  fMsg:= getMessageDisplay;
  fFileLineBrks:= TStringList.Create;
  fLog := TStringList.Create;
  //
  AssignPng(btnSendCom, 'accept');
end;

destructor TCEGdbWidget.destroy;
begin
  fFileLineBrks.Free;
  fLog.Free;
  killGdb;
  EntitiesConnector.removeObserver(self);
  inherited;
end;
{$ENDREGION}

{$REGION ICEProjectObserver ----------------------------------------------------}
procedure TCEGdbWidget.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEGdbWidget.projChanged(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
end;

procedure TCEGdbWidget.projClosing(aProject: ICECommonProject);
begin
  if fProj <> aProject then
    exit;
  fProj := nil;
end;

procedure TCEGdbWidget.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TCEGdbWidget.projCompiling(aProject: ICECommonProject);
begin
end;
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCEGdbWidget.docNew(aDoc: TCESynMemo);
begin
  if aDoc.isDSource then
    aDoc.onBreakpointModify := @editorModBrk;
end;

procedure TCEGdbWidget.docFocused(aDoc: TCESynMemo);
begin
  if aDoc.isDSource then
    aDoc.onBreakpointModify := @editorModBrk;
end;

procedure TCEGdbWidget.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCEGdbWidget.docClosing(aDoc: TCESynMemo);
begin
end;
{$ENDREGION}

{$REGION Unsorted Debugging things ---------------------------------------------}
procedure TCEGdbWidget.killGdb;
begin
  if not assigned(fGdb) then
    exit;
  if fGdb.Running then
    fGdb.Terminate(0);
  FreeAndNil(fGdb);
end;

procedure TCEGdbWidget.updateFileLineBrks;
var
  i,j: integer;
  doc: TCESynMemo;
  nme: string;
begin
  fFileLineBrks.Clear;
  if fDocHandler = nil then exit;
  //
  for i:= 0 to fDocHandler.documentCount-1 do
  begin
    doc := fDocHandler.document[i];
    if not doc.isDSource then
      continue;
    nme := doc.fileName;
    if not FileExists(nme) then
      continue;
    {$WARNINGS OFF}
    for j := 0 to doc.breakPointsCount-1 do
      fFileLineBrks.AddObject(nme, TObject(pointer(doc.BreakPointLine(j))));
    {$WARNINGS ON}
  end;
end;

procedure TCEGdbWidget.editorModBrk(sender: TCESynMemo; line: integer; modification: TBreakPointModification);
var
  str: string;
  nme: string;
const
  cmd: array[TBreakPointModification] of string = ('break ', 'clear ');
begin
  // set only breakpoint in live, while debugging
  // note: only works if execution is paused (breakpoint)
  // and not inside a loop (for ex. with sleep).
  if fGdb = nil then exit;
  if not fGdb.Running then exit;
  nme := sender.fileName;
  if not FileExists(nme) then exit;
  //
  str := cmd[modification] + nme + ':' + intToStr(line);
  fGdb.Suspend;
  gdbCommand(str);
  fGdb.Resume;
end;

procedure TCEGdbWidget.startDebugging;
var
  str: string;
  i: integer;
begin
  // protect
  if fProj = nil then exit;
  if fProj.binaryKind <> executable then exit;
  str := fProj.outputFilename;
  if not FileExists(str) then exit;
  // gdb process
  killGdb;
  fGdb := TCEProcess.create(nil);
  fGdb.Executable:= 'gdb' + exeExt;
  fgdb.Options:= [poUsePipes, poStderrToOutPut];
  fgdb.Parameters.Add(str);
  fGdb.OnReadData:= @gdbOutput;
  fGdb.OnTerminate:= @gdbOutput;
  fgdb.execute;
  // file:line breakpoints
  updateFileLineBrks;
  for i:= 0 to fFileLineBrks.Count-1 do
  begin
    str := 'break ' + fFileLineBrks.Strings[i] + ':' + intToStr(PtrUInt(fFileLineBrks.Objects[i])) + #10;
    fGdb.Input.Write(str[1], length(str));
  end;
  // break on druntime exceptions heper + throw'
  fGdb.OnReadData := @processSilently;
  gdbCommand('break onAssertError');
  gdbCommand('break onAssertErrorMsg');
  gdbCommand('break onUnittestErrorMsg');
  gdbCommand('break onRangeError');
  gdbCommand('break onFinalizeError');
  gdbCommand('break onHiddenFuncError');
  gdbCommand('break onOutOfMemoryError');
  gdbCommand('break onInvalidMemoryOperationError');
  gdbCommand('break onSwitchError');
  gdbCommand('break onUnicodeError');
  gdbCommand('break _d_throwc');
  fGdb.OnReadData := @gdbOutput;
  // launch
  gdbCommand('run');
end;
{$ENDREGION}

{$REGIOn GDB output processors -------------------------------------------------}
procedure TCEGdbWidget.gdbOutput(sender: TObject);
var
  str: string;
begin
  if fMsg = nil then
    exit;
  fLog.Clear;
  fGdb.getFullLines(fLog);
  for str in fLog do
    fMsg.message(str, nil, amcMisc, amkAuto);
end;

procedure TCEGdbWidget.processSilently(sender: TObject);
begin
  fGdb.OutputStack.Clear;
  fGdb.OnReadData:=@gdbOutput;
end;

procedure TCEGdbWidget.processInfoRegs(sender: TObject);
begin
  try
    //
  finally
    fGdb.OnReadData:=@gdbOutput;
  end;
end;

procedure TCEGdbWidget.processInfoStack(sender: TObject);
begin
  try
    //
  finally
    fGdb.OnReadData:=@gdbOutput;
  end;
end;
{$ENDREGION}

{$REGIOn GDB commands & actions ------------------------------------------------}
procedure TCEGdbWidget.gdbCommand(aCommand: string; outputCatcher: TNotifyEvent = nil);
begin
  if fGdb = nil then exit;
  if not fGdb.Running then exit;
  //
  aCommand += #10;
  if assigned(outputCatcher) then
    fGdb.OnReadData := outputCatcher;
  fGdb.Input.Write(aCommand[1], length(aCommand));
end;

procedure TCEGdbWidget.infoRegs;
begin
  gdbCommand('info registers', @processInfoRegs);
end;

procedure TCEGdbWidget.infoStack;
begin
  gdbCommand('info stack', @processInfoStack);
end;

procedure TCEGdbWidget.btnStartClick(Sender: TObject);
begin
  startDebugging;
end;

procedure TCEGdbWidget.btnContClick(Sender: TObject);
begin
  gdbCommand('continue');
end;

procedure TCEGdbWidget.btnStopClick(Sender: TObject);
begin
  gdbCommand('kill');
  killGdb;
end;

procedure TCEGdbWidget.btnSendComClick(Sender: TObject);
begin
  gdbCommand(edit1.Text);
  edit1.Text := '';
end;

procedure TCEGdbWidget.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> byte(#13) then exit;
  gdbCommand(edit1.Text);
  edit1.Text := '';
end;
{$ENDREGION}

end.

