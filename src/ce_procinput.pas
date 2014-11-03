unit ce_procinput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ce_widget, process, ce_common;

type

  { TCEProcInputWidget }
  TCEProcInputWidget = class(TCEWidget)
    btnSend: TButton;
    txtInp: TEdit;
    txtExeName: TStaticText;
    procedure btnSendClick(Sender: TObject);
    procedure txtInpKeyPress(Sender: TObject; var Key: char);
  private
    fProc: TProcess;
    procedure sendInput;
    procedure setProc(const aValue: TProcess);
  public
    property process: TProcess read fProc write setProc;
  end;

implementation
{$R *.lfm}

procedure TCEProcInputWidget.setProc(const aValue: TProcess);
begin
  txtExeName.Caption := 'no process';
  fProc := nil;
  if aValue = nil then
    exit;
  if not (poUsePipes in aValue.Options) then
    exit;
  fProc := aValue;
  txtExeName.Caption := shortenPath(fProc.Executable);
end;

procedure TCEProcInputWidget.sendInput;
var
  inp: string;
begin
  inp := txtInp.Text + lineEnding;
  fProc.Input.Write(inp[1], length(inp));
  txtInp.Text := '';
end;

procedure TCEProcInputWidget.txtInpKeyPress(Sender: TObject; var Key: char);
begin
  if fProc = nil then
    exit;
  if key <> #13 then
    exit;
  sendInput;
end;

procedure TCEProcInputWidget.btnSendClick(Sender: TObject);
begin
  if fProc = nil then
    exit;
  sendInput;
end;



end.

