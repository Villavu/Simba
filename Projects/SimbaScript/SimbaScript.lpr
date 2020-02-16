program SimbaScript;

{$mode objfpc}{$H+}

{$IFOPT D+}
  {$DEFINE HAS_DEBUG_INFO}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  sysutils, classes, interfaces, forms, lazLoggerbase, lazcontrols,
  simbascript.script, simba.script_common;

{$R *.res}

type
  TObjectHelper = class helper for TObject
    procedure Execute(Data: PtrInt);
  end;

var
  i: Int32;
  obj: TObject;

procedure TObjectHelper.Execute(Data: PtrInt);
begin
  Script := TSimbaScript.Create();
  if Application.HasOption('compile') then
    Script.CompileOnly := True;
  if Application.HasOption('dump') then
    Script.DumpOnly := True;
  Script.Start();
end;

begin
  ExitCode := SCRIPT_ERROR;

  try
    RequireDerivedFormResource := True;

    Application.CaptureExceptions := False;
    Application.Scaled := True;
    Application.Initialize();

    Application.QueueAsyncCall(@obj.Execute, 0);
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

