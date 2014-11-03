unit ce_gdbcmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, process,
  Menus, StdCtrls, ce_widget, ce_project, ce_interfaces, ce_observer, asyncprocess;

type

  { TCEWidget1 }

  TCEWidget1 = class(TCEWidget, ICEProjectObserver)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    IdleTimer1: TIdleTimer;
    lstGdbOut: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure IdleTimer1Timer(Sender: TObject);
  private
    fGdbProc: TProcess;
    fProject: TCEProject;
    procedure gdbReadData(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure projNew(const aProject: TCEProject);
    procedure projClosing(const aProject: TCEProject);
    procedure projFocused(const aProject: TCEProject);
    procedure projChanged(const aProject: TCEProject);
  end;

implementation
{$R *.lfm}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEWidget1.create(aOwner: TComponent);
begin
  inherited;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCEWidget1.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCEWidget1.projNew(const aProject: TCEProject);
begin
  fProject := aProject;
end;

procedure TCEWidget1.projClosing(const aProject: TCEProject);
begin
  fProject := nil;
end;

procedure TCEWidget1.projFocused(const aProject: TCEProject);
begin
  fProject := aProject;
end;

procedure TCEWidget1.projChanged(const aProject: TCEProject);
begin
  fProject := aProject;
end;
{$ENDREGION --------------------------------------------------------------------}


procedure TCEWidget1.Button1Click(Sender: TObject);
var
  outname: string;
begin
  if fProject = nil then exit;
  outname := fProject.outputFilename;
  if not fileExists(outname) then exit;
  //
  fGdbProc := TProcess.create(nil);
  fGdbProc.Executable := 'cdb';
  fGdbProc.Parameters.Add('-c');
  fGdbProc.Parameters.Add('"l+*;.lines"');
  fGdbProc.Parameters.Add(outname);
  fGdbProc.CurrentDirectory := extractFilePath(outname);
  fGdbProc.Options := fGdbProc.Options + [poStderrToOutPut, poUsePipes];
  //fGdbProc.OnReadData := @gdbReadData;

  fGdbProc.Execute;
end;

procedure TCEWidget1.Button2Click(Sender: TObject);
begin
  if fGdbProc = nil then exit;
  fGdbProc.Terminate(0);
  fGdbProc.Free;
  fGdbProc := nil;
end;

procedure TCEWidget1.Edit1KeyPress(Sender: TObject; var Key: char);
var
  inp: string;
begin
  if fGdbProc <> nil then
    if Key = LineEnding[1] then
    begin
      inp := Edit1.Text + LineEnding;
      fGdbProc.Input.Write( inp[1], length(inp) );
    end;
  Edit1.Text := '';
end;

procedure TCEWidget1.IdleTimer1Timer(Sender: TObject);
begin
  gdbReadData(nil);
end;

procedure TCEWidget1.gdbReadData(sender: TObject);
var
  str: TMemoryStream;
  lst: TStringList;
  cnt: Integer;
  sum: Integer;
begin

  if fGdbProc = nil then exit;

  cnt := 0;
  sum := 0;
  str := TMemoryStream.Create;
  lst := TStringList.Create;


  while fGdbProc.Output.NumBytesAvailable <> 0 do
  begin
    str.Size := str.Size + 1024;
    cnt := fGdbProc.Output.Read((str.Memory + sum)^, 1024);
    sum += cnt;
  end;
  //  cnt = 0;

  str.Size := sum;
  lst.LoadFromStream(str);
  lstGdbOut.Lines.AddStrings(lst);
  lstGdbOut.VertScrollBar.Position := lstGdbOut.Lines.Count * 53;

  lst.Free;
  str.Free;
end;



end.

