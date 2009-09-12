program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, Window, files, MufasaTypes, Client, TestUnit, finder,
  MMLThread, mmlpsthread;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  Application.Title:='Mufasa Stand Alone';
  {$I project1.lrs}
  Application.Initialize;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

