program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols, ce_sharedres,
  ce_observer, ce_libman, ce_tools, ce_dcd, ce_main, ce_writableComponent,
  ce_symstring, ce_staticmacro, ce_inspectors, ce_editoroptions, ce_dockoptions,
  ce_shortcutseditor, ce_mru, ce_processes, ce_dubproject, ce_dialogs, 
  ce_dubprojeditor, ce_controls, ce_dfmt, ce_lcldragdrop;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCEMainForm, CEMainForm);
  Application.Run;
end.

