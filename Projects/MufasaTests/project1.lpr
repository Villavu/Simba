program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Forms,Interfaces,
  Client

  { you can add units after this };

type

  { MufasaTests }

  MufasaTests = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ MufasaTests }

procedure MufasaTests.DoRun;

var
  ErrorMsg: String;
  C: TClient;
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

  { add your program here }
  C := TClient.Create;
  writeln('wat');



  C.Free;


  // stop program loop
  Terminate;
end;

constructor MufasaTests.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor MufasaTests.Destroy;
begin
  inherited Destroy;
end;

procedure MufasaTests.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: MufasaTests;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  Application:=MufasaTests.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

