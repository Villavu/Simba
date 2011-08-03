program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  interfaces, // to prevent linking errors. :|

  client, MufasaTypes,

  lclintf;

type

  { TMMLBenchmark }

  TMMLBenchmark = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  bmp: Integer;

  MMLClient: TClient;
{ TMMLBenchmark }

procedure init;

begin
  MMLClient := TClient.Create('');

  bmp := MMLClient.MBitmaps.CreateBMP(0, 0);
  MMLClient.MBitmaps.GetBMP(bmp).LoadFromFile('/home/merlijn/Programs/simba/wall.bmp');
  MMLClient.IOManager.SetTarget(MMLClient.MBitmaps[bmp]);
end;

procedure free;
begin
  MMLClient.IOManager.SetDesktop;
  MMLClient.MBitmaps.FreeBMP(bmp);
  MMLClient.Free;
end;

procedure Benchmark;

var
  t, t2, t3: PtrUInt;
  i, c, w, h: integer;
  col, tol: integer;

  dummytpa: TPointArray;

begin
  init;
  t := gettickcount;
  MMLClient.IOManager.GetDimensions(w, h);

  col := 16777215; tol := 30;

  for c := 0 to 2 do // benchmark all CTS'es
  begin
    MMLClient.MFinder.SetToleranceSpeed(c);
    t2 := gettickcount;
    for i := 0 to 100 do
    begin
      MMLClient.MFinder.FindColorsTolerance(dummytpa, col, 0, 0, w-1,h-1, tol);
    end;
    t3 := gettickcount;
    writeln(format('FindColorsTolerance'+#10+
      'Time spent: %d ms (per %f ms), matches: %d, CTS: %d, Colour: %d, Tolerance %d',
                         [t3-t2, (t3-t2) / 100.0, length(dummytpa), c, col, tol]));
  end;

  free;
end;

procedure TMMLBenchmark.DoRun;
var
  ErrorMsg: String;
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

  Benchmark;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TMMLBenchmark.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMMLBenchmark.Destroy;
begin
  inherited Destroy;
end;

procedure TMMLBenchmark.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMMLBenchmark;

{$R *.res}

begin
  Application:=TMMLBenchmark.Create(nil);
  Application.Run;
  Application.Free;
end.

