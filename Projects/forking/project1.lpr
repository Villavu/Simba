program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Process
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;

  Processes: array of TProcess;
  i, c, a:integer;

begin

  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  a := 5;
  SetLength(Processes, a);
  for i := 0 to a - 1 do
  begin
    Processes[i] := TProcess.Create(Self);
    {$WARNING SET THIS PATH}
    Processes[i].CommandLine := 'C:/mufasa/Projects/MufasaTests/project1.exe';
    //Processes[i].ApplicationName := 'ForkingTest: ' + inttostr(i);
    Processes[i].Execute;
    Writeln('Process ' + inttostr(i) + ', has ID ' + inttostr(Processes[i].ProcessID));
  end;

  sleep(10000);
  while true do
  begin
    c := 0;
    for i := 0 to a - 1 do
    begin
      if not Processes[i].Running then
        c := c + 1;
    end;
    if c = a then
      break;
    sleep(1000);
  end;

  for i := 0 to a - 1 do
  begin
    Processes[i].Free;
  end;
  { add

  end;
  your program here }

  // stop program loop
  writeln('done');
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

