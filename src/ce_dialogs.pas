unit ce_dialogs;

{$I ce_defines.inc}

interface

uses
  classes, sysutils, forms, dialogs;


(**
 * Ok/Cancel modal dialog
 *)
function dlgOkCancel(const aMsg: string): TModalResult;

(**
 * Info message
 *)
function dlgOkInfo(const aMsg: string): TModalResult;

(**
 * Error message
 *)
function dlgOkError(const aMsg: string): TModalResult;


const
  DdiagFilter = 'D source|*.d|D interface|*.di|All files|*.*';

implementation


function dlgOkCancel(const aMsg: string): TModalResult;
const
  Btns = [mbOK,mbCancel];
begin
  exit( MessageDlg('Coedit', aMsg, mtConfirmation, Btns, ''));
end;

function dlgOkInfo(const aMsg: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', aMsg, mtInformation, Btns, ''));
end;

function dlgOkError(const aMsg: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', aMsg, mtError, Btns, ''));
end;

end.

