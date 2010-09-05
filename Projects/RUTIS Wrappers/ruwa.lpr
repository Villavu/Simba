program ruwa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, v_MiscFunctions, CastaliaPasLex, CastaliaPasLexTypes,
  CastaliaSimplePasPar, CastaliaSimplePasParTypes,
  v_Constants, v_ideCodeParser;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

