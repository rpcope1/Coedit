program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols, ce_observer, ce_libman,
  ce_tools, ce_dcd, ce_main, ce_writableComponent, ce_options, ce_symstring,
  ce_staticmacro, ce_icons, ce_dubwrap;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCEMainForm, CEMainForm);
  Application.Run;
end.

