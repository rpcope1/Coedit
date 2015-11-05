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

(**
 * close aFilename Ok/Cancel.
 *)
function dlgFileChangeClose(const aFilename: string): TModalResult;

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

function dlgFileChangeClose(const aFilename: string): TModalResult;
const
  fmt = '"%s" latest modifications are not saved.'#13#10#13#10'Close it without saving ?';
begin
  exit(dlgOkCancel(format(fmt, [aFilename])));
end;

end.

