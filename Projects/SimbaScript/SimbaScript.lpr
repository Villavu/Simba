program SimbaScript;

{$mode objfpc}{$H+}

{$IFOPT D+}
  {$DEFINE HAS_DEBUG_INFO}
{$ENDIF}

uses
  {$IFDEF LINUX}
  cthreads, cmem,
  {$ENDIF}
  sysutils, classes, interfaces, forms, lazLoggerbase, lazcontrols,
  simbascript.script, simba.script_common;

{$R *.res}

var
  i: Int32;

begin
  ExitCode := SCRIPT_ERROR;

  try
    RequireDerivedFormResource := True;

    Application.CaptureExceptions := False;
    Application.Scaled := True;
    Application.Initialize();

    Application.QueueAsyncCall(@Script.Initialize, 0);

    if Application.HasOption('dump') then
    begin
      Application.QueueAsyncCall(@Script.Dump, 0);
      Application.QueueAsyncCall(@Script.Terminate, 0);
    end;

    if Application.HasOption('compile') then
    begin
      Application.QueueAsyncCall(@Script.Compile, 0);
      Application.QueueAsyncCall(@Script.Terminate, 0);
    end;

    if Application.HasOption('run') then
    begin
      Application.QueueAsyncCall(@Script.Compile, 0);
      Application.QueueAsyncCall(@Script.Run, 0);
    end;

    Application.Run();

    ExitCode := SCRIPT_SUCCESS;
  except
    on E: Exception do
    begin
      WriteLn(StringOfChar('-', 80));
      WriteLn(' ');
      WriteLn('Exception: ', E.Message);
      WriteLn('Exception Class: ', E.ClassName);

      {$IFDEF HAS_DEBUG_INFO}
      WriteLn(' ');
      DumpExceptionBackTrace();
      {$ENDIF}

      WriteLn(' ');
      WriteLn('  Parameters:');
      for i := 1 to Application.ParamCount + 1 do
        WriteLn('  ' + Application.Params[i]);

      WriteLn(StringOfChar('-', 80));
    end;
  end;
end.

