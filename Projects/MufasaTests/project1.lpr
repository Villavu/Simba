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
  bitmaps,x ,mufasatypes,dtm,dtmutil


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
  I: Integer;
  dtm: pdtm;
  p:tpointarray;
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

  bmp := TMufasaBitmap.Create;
  bmp.SetSize(10,10);
  FillChar(bmp.FData[0],sizeof(trgb32)*100, 0);
  bmp.FastSetPixel(8,8,255);
  bmp.FastSetPixel(9,9,255);
  bmp.FastSetPixel(7,7,255);
  bmp.FastSetPixel(9,8,255);
  bmp.FastSetPixel(8,9,255);
  C.MWindow.SetTarget(bmp);


  initdtm(dtm, 3);
  dtm.p[0] := Point(2, 2);
  dtm.p[1] := Point(-3, -3);
  dtm.p[2] := Point(0, 0);
  dtm.c[0] := 255;
  dtm.asz[1] := 0;
  dtm.ash[1] := dtm_Rectangle;

  setlength(p, 0);

  C.MFinder.FindDTMs(dtm, p, 0, 0, 9, 9);
  for i := 0 to high(p) do
    writeln(format('%d: (%d, %d)', [i, p[i].x, p[i].y]));




  //bmp.OnDestroy:=nil;
  bmp.Free;
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


{  {$WARNING Change This Path!}
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
{  bmp.LoadFromFile('/home/merlijn/Programs/mufasa/UpText/text1.bmp');
  //bmp.LoadFromFile('/home/merlijn/Programs/mufasa/output.bmp');

  writeln(inttostr(bmp.Width) + ', ' + inttostr(bmp.height));
  {C.MWindow.SetTarget(x.TWindow(94371927));   }
  C.MWindow.SetTarget(bmp);

  Time := GetTickCount;
  for i := 0 to 100 do
    C.MOCR.GetUpTextAt(0,0);
  writeln('Time: ' + FloatToStr(((GetTickCount - Time) / (i + 1))));
  writeln(C.MOCR.GetUpTextAt(0,0));    }

  //C.MInput.ClickMouse(5,5, mouse_Left);
  sleep(2000);
  C.MInput.SendText('a');       }
