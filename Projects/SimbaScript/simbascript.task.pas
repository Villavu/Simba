unit simbascript.task;

{$mode objfpc}{$H+}

{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

interface

uses
  classes, sysutils;

type
  TSimbaScriptTask = class(TThread)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  forms
  {$IFDEF WINDOWS},
  windows
  {$ENDIF}
  {$IFDEF DARWIN},
  cocoaint
  {$ENDIF};

type
  TApplicationHelper = class helper for TApplication
    procedure Terminate;
  end;

procedure TApplicationHelper.Terminate;
begin
  inherited Terminate();

  if (WakeMainThread <> nil) then
    WakeMainThread(Self);

  // MacOS needs extra help
  {$IFDEF DARWIN}
  CocoaWidgetSet.NSApp.Terminate(nil);
  {$ENDIF}
end;

constructor TSimbaScriptTask.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
end;

destructor TSimbaScriptTask.Destroy;
var
  PID: UInt32;
begin
  if (FatalException <> nil) then
    WriteLn(Exception(FatalException).Message);

  inherited Destroy();

  {$IFDEF WINDOWS}
  GetWindowThreadProcessId(GetConsoleWindow(), PID);
  if (PID = GetCurrentProcessID()) then
  begin
    WriteLn('');
    WriteLn('Press enter to exit');

    ReadLn();
  end;
  {$ENDIF}

  TThread.Synchronize(TThread.CurrentThread, @Application.Terminate);
end;

end.

