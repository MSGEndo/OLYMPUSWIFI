program Olympus_ImageSave;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //{$IFDEF UseCThreads}    // NB:  On Linux, not Windows, there is a custom compiler setting -dUsecThreads which allows cThreads to be used
  cthreads,                 //      when this IFDEF is present.  Seems unnecessary to complicate things -just dont have the IFDEF and if
  //{$ENDIF}               //       dont want to use cThreads then take it out of the uses clause.  Duh!
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, Olympus_ImageSave1, OlympusShare;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.Run;
end.

