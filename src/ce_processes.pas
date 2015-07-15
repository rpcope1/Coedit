unit ce_processes;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, process, asyncprocess;

type

  {
    The common process runner used in Coedit.

    This class solves several issues encountered when using TProcess and TAsyncProcess:

    -   OnTerminate event is never called under Linux.
        Here a timer perdiodically check the process and call the event accordingly.
    -   TAsyncProcess.OnReadData event is not usable to read full output lines.
        Here the output is accumulated in a TMemoryStream which allows to keep data
        at the left of an unterminated line when a buffer is available.

    The member Output is not usable anymore. Instead:

    -   getFullLines() can be used  in OnReadData or after the execution to fill
        a string list.
    -   OutputStack can be used to read the raw output. It allows to seek, which
        overcomes another limitation of the basic process classes.
  }
  TCEProcess = class(TASyncProcess)
  private
    fRealOnTerminate: TNotifyEvent;
    fRealOnReadData: TNotifyEvent;
    fOutputStack: TMemoryStream;
    fTerminateChecker: TIdleTimer;
    fDoneTerminated: boolean;
    procedure checkTerminated(sender: TObject);
    procedure setOnTerminate(value: TNotifyEvent);
    procedure setOnReadData(value: TNotifyEvent);
    procedure internalDoOnReadData(sender: TObject);
    procedure internalDoOnTerminate(sender: TObject);
  published
    property OnTerminate write setOnTerminate;
    property OnReadData write setOnReadData;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure execute; override;
    // reads TProcess.OUtput in OutputStack
    procedure fillOutputStack;
    // fills list with the full lines contained in OutputStack
    procedure getFullLines(list: TStrings; consume: boolean = true);
    // access to a flexible copy of TProcess.Output
    property OutputStack: TMemoryStream read fOutputStack;
  end;

  procedure killProcess(var proc: TCEProcess);

implementation

procedure killProcess(var proc: TCEProcess);
begin
  if proc = nil then
    exit;
  if proc.Running then
    proc.Terminate(0);
  proc.Free;
  proc := nil;
end;

constructor TCEProcess.create(aOwner: TComponent);
begin
  inherited;
  FOutputStack := TMemoryStream.Create;
  FTerminateChecker := TIdleTimer.Create(nil);
  FTerminateChecker.Interval := 50;
  fTerminateChecker.OnTimer := @checkTerminated;
  fTerminateChecker.Enabled := false;
  fTerminateChecker.AutoEnabled:= true;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
end;

destructor TCEProcess.destroy;
begin
  FTerminateChecker.Free;
  FOutputStack.Free;
  inherited;
end;

procedure TCEProcess.Execute;
begin
  fOutputStack.Clear;
  fDoneTerminated := false;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
  fTerminateChecker.Enabled := true;
  inherited;
end;

procedure TCEProcess.fillOutputStack;
var
  sum, cnt: Integer;
begin
  if not (poUsePipes in Options) then
    exit;

  sum := fOutputStack.Size;
  while (Output <> nil) and (NumBytesAvailable > 0) do
  begin
    fOutputStack.SetSize(sum + 1024);
    cnt := Output.Read((fOutputStack.Memory + sum)^, 1024);
    sum += cnt;
  end;

  fOutputStack.SetSize(sum);
end;

procedure TCEProcess.getFullLines(list: TStrings; consume: boolean = true);
var
  stored: Integer;
  lastTerm: Integer;
  toread: Integer;
  buff: Byte = 0;
  str: TMemoryStream;
begin
  if not Running then
  begin
    list.LoadFromStream(fOutputStack);
    if consume then
      fOutputStack.Clear;
  end else
  begin
    lastTerm := fOutputStack.Position;
    stored := fOutputStack.Position;
    while fOutputStack.Read(buff, 1) = 1 do
      if buff = 10 then lastTerm := fOutputStack.Position;
    fOutputStack.Position := stored;
    if lastTerm <> stored then
    begin
      str := TMemoryStream.Create;
      try
        toread := lastTerm - stored;
        str.SetSize(toRead);
        fOutputStack.Read(str.Memory^, toread);
        list.LoadFromStream(str);
      finally
        str.Free;
      end;
    end;
  end;
end;

procedure TCEProcess.setOnTerminate(value: TNotifyEvent);
begin
  fRealOnTerminate := value;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
end;

procedure TCEProcess.setOnReadData(value: TNotifyEvent);
begin
  fRealOnReadData := value;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
end;

procedure TCEProcess.internalDoOnReadData(sender: TObject);
begin
  fillOutputStack;
  if fRealOnReadData <> nil then
    fRealOnReadData(self);
end;

procedure TCEProcess.internalDoOnTerminate(sender: TObject);
begin
  fTerminateChecker.Enabled := false;
  if fDoneTerminated then exit;
  fDoneTerminated := true;
  //
  fillOutputStack;
  if fRealOnTerminate <> nil then
    fRealOnTerminate(self);
end;

procedure TCEProcess.checkTerminated(sender: TObject);
begin
  if Running then
    exit;
  fTerminateChecker.Enabled := false;
  internalDoOnTerminate(self);
end;

end.

