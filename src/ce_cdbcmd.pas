unit ce_cdbcmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  process, Menus, StdCtrls, ce_widget, ce_project, ce_interfaces, ce_observer,
  asyncprocess, ComCtrls, Buttons, ce_common;

type

  { TCECdbWidget }
  TCECdbWidget = class(TCEWidget, ICEProjectObserver)
    btnGo: TSpeedButton;
    btnStep: TSpeedButton;
    btnDisasm: TSpeedButton;
    btnStop: TSpeedButton;
    btnStart: TSpeedButton;
    txtCdbCmd: TEdit;
    lstCdbOut: TListView;
    Panel1: TPanel;
    procedure btnDisasmClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure txtCdbCmdKeyPress(Sender: TObject; var Key: char);
  private
    fCdbProc: TAsyncProcess;
    fProject: TCEProject;
    procedure cdbOutput(sender: TObject);
    procedure cdbTerminate(sender: TObject);
    procedure cdbOutputToGui;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
  end;

implementation
{$R *.lfm}

uses
  ce_main;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCECdbWidget.create(aOwner: TComponent);
begin
  inherited;
  Enabled := exeInSysPath('cdb');
  if Enabled then
  begin
    EntitiesConnector.addObserver(self);
    EntitiesConnector.endUpdate;
  end;
end;

destructor TCECdbWidget.destroy;
begin
  if Enabled then begin
    killProcess(fCdbProc);
    EntitiesConnector.removeObserver(self);
  end;
  inherited;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCECdbWidget.projNew(aProject: TCEProject);
begin
  fProject := aProject;
end;

procedure TCECdbWidget.projClosing(aProject: TCEProject);
begin
  if fProject <> aProject then
    exit;
  fProject := nil;
end;

procedure TCECdbWidget.projFocused(aProject: TCEProject);
begin
  fProject := aProject;
end;

procedure TCECdbWidget.projChanged(aProject: TCEProject);
begin
end;
{$ENDREGION --------------------------------------------------------------------}

procedure TCECdbWidget.btnStartClick(Sender: TObject);
var
  outname: string;
begin
  lstCdbOut.Clear;
  if fProject = nil then
    exit;
  outname := fProject.outputFilename;
  if not fileExists(outname) then
    exit;
  //
  killProcess(fCdbProc);
  fCdbProc := TAsyncProcess.create(nil);
  fCdbProc.Executable := 'cdb';
  fCdbProc.Parameters.Add('-c');
  fCdbProc.Parameters.Add('"l+*;.lines"');
  fCdbProc.Parameters.Add(outname);
  fCdbProc.CurrentDirectory := extractFilePath(outname);
  fCdbProc.Options := [poNoConsole, poStderrToOutPut, poUsePipes];
  fCdbProc.OnReadData := @cdbOutput;
  fCdbProc.OnTerminate := @cdbTerminate;
  //
  fCdbProc.Execute;
end;

procedure TCECdbWidget.btnStepClick(Sender: TObject);
const
  cmd = 'p'#13#10;
begin
  if fCdbProc = nil then
    exit;
  fCdbProc.Input.Write(cmd[1], length(cmd));
end;

procedure TCECdbWidget.btnGoClick(Sender: TObject);
const
  cmd = 'g'#13#10;
begin
  if fCdbProc = nil then
    exit;
  fCdbProc.Input.Write(cmd[1], length(cmd));
end;

procedure TCECdbWidget.btnDisasmClick(Sender: TObject);
const
  cmd = 'u'#13#10;
begin
  if fCdbProc = nil then
    exit;
  fCdbProc.Input.Write(cmd[1], length(cmd));
end;

procedure TCECdbWidget.btnStopClick(Sender: TObject);
const
  cmd = 'q'#13#10;
begin
  if fCdbProc <> nil
    then
    fCdbProc.Input.Write(cmd[1], length(cmd));
  killProcess(fCdbProc);
end;

procedure TCECdbWidget.txtCdbCmdKeyPress(Sender: TObject; var Key: char);
var
  inp: string;
begin
  if (fCdbProc = nil) or (key <> #13) then
    exit;
  //
  inp := CEMainForm.expandSymbolicString(txtCdbCmd.Text) + LineEnding;
  fCdbProc.Input.Write(inp[1], length(inp));
  //
  inp := lstCdbOut.Items.Item[lstCdbOut.Items.Count-1].Caption;
  inp += CEMainForm.expandSymbolicString(txtCdbCmd.Text);
  lstCdbOut.Items.Item[lstCdbOut.Items.Count-1].Caption := inp;
  //
  txtCdbCmd.Text := '';
end;

procedure TCECdbWidget.cdbOutputToGui;
var
  lst: TStringList;
  cnt: Integer;
begin
  if fCdbProc = nil then
    exit;
  //
  lst := TStringList.Create;
  try
    processOutputToStrings(fCdbProc, lst);
    for cnt := 0 to lst.Count-1 do
      lstCdbOut.AddItem(lst.Strings[cnt], nil);
    lstCdbOut.Items[lstCdbOut.Items.Count-1].MakeVisible(true);
  finally
    lst.Free;
  end;
end;

procedure TCECdbWidget.cdbOutput(sender: TObject);
begin
  cdbOutputToGui;
end;

procedure TCECdbWidget.cdbTerminate(sender: TObject);
begin
  cdbOutputToGui;
  killProcess(fCdbProc);
end;

end.

