program Valistus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, v_Constants, CastaliaPasLex, CastaliaPasLexTypes,
  CastaliaSimplePasPar, CastaliaSimplePasParTypes,
  v_ideCodeParser, v_ideCodeInsight, v_MiscFunctions, v_AutoCompleteForm;

{$R Valistus.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

