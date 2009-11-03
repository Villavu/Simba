program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Forms,Interfaces,
  LCLIntf,
  Client,
  bitmaps,x


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
  Time: DWord;
  C: TClient;
  I{, W, H, X, Y}: Integer;
  bmp: TMufasaBitmap;

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
  {$WARNING Change This Path!}
  C.MOCR.InitTOCR('/home/merlijn/Programs/mufasa/Fonts/');
  //C.MOCR.InitTOCR('/home/merlijn/Programs/mufasa/ben/');

 {C.MWindow.GetDimensions(W, H);
  Time := GetTickCount;
  for i := 0 to 100 do
    C.MFinder.FindColor(X, Y, 0, 0, 0, W - 1, H - 1);
  writeln('Time: ' + IntToStr(GetTickCount - Time));

  if C.MFinder.FindColor(X, Y, 0, 0, 0, W - 1, H - 1) then
  begin
    C.MInput.SetMousePos(X, Y);
    writeln('found!');
  end else
  begin
    writeln('not found!');
  end;    }


  bmp := TMufasaBitmap.Create;

  {$WARNING Change This Path!}
 //bmp.LoadFromFile('/home/merlijn/Programs/mufasa/UpText/text5.bmp');
  //bmp.LoadFromFile('/home/merlijn/Programs/mufasa/output.bmp');

  writeln(inttostr(bmp.Width) + ', ' + inttostr(bmp.height));
  C.MWindow.SetTarget(x.TWindow(94371927));
  //C.MWindow.SetTarget(bmp);

 { Time := GetTickCount;
  for i := 0 to 100 do
    C.MOCR.GetUpTextAt(0,0);
  writeln('Time: ' + FloatToStr(((GetTickCount - Time) / (i + 1)))); }
  writeln(C.MOCR.GetUpTextAt(7,7));

  C.Free;
  //bmp.OnDestroy:=nil;
  //bmp.Free;


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

