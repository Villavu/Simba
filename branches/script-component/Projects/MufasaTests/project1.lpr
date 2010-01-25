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
  bitmaps,{$IFDEF LINUX}x,{$ENDIF}mufasatypes,dtm,dtmutil, ocrutil


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

const
    CW = 800;
    CH = 600;

function randomdtm(a: integer): pdtm;
var
   i: integer;
begin
  initdtm(result, a);
  for i := 1 to result.l - 1 do
  begin
    result.p[i] := point(random(30) - 15, random(30) - 15);
    result.c[i] := 0;
    result.t[i] := random(255);
    result.asz [i] := random(5);
    result.ash[i] := 0;
    {writeln(format('dtm: (%d, %d) c: %d, t: %d, asz: %d', [result.p[i].x,
            result.p[i].y,  result.c[i], result.t[i], result.asz[i]])); }
    result.bp[i] := false;// random(10) = 1;
  end;
  result.c[0] := 255;
  PrintpDTM(result);
end;

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
  C := TClient.Create('.');
  C.MOCR.InitTOCR('/home/merlijn/Programs/mufasa/Fonts/');

  bmp := C.MOCR.TextToFontBitmap('Danke schon', 'SmallChars');

  bmp.SaveToFile('/tmp/wat.bmp');

{  bmp := TMufasaBitmap.Create;
  bmp.SetSize(CW,CH);
  Writeln(Format('Client W/H: %d, %d', [CW, CH]));
  FillChar(bmp.FData[0],sizeof(trgb32)*CW*CH, 0);
  Randomize;
 for i := 0 to 500 do
    bmp.fastsetpixel(random(CW), random(CH), 255);    }
 { bmp.FastSetPixel(8,8,255);
  bmp.FastSetPixel(9,9,255);
  bmp.FastSetPixel(7,7,255);
  bmp.FastSetPixel(9,8,255);
  bmp.FastSetPixel(8,9,255);            }
//  C.MWindow.SetTarget(bmp);


 { initdtm(dtm, 5);
  dtm.p[0] := Point(2, 2);
  dtm.p[1] := Point(-3, -3);
  dtm.p[2] := Point(0, 0);
  dtm.p[3] := Point(1, 1);
  dtm.p[4] := Point(3, 3);
  dtm.c[0] :=  255;
  dtm.t[0] :=  0;
  dtm.asz[1] := 1;
  dtm.ash[1] := dtm_Rectangle;  }

 { dtm := randomdtm(10);

 // setlength(p, 1);
 C.MFinder.SetToleranceSpeed(1);

  time := GetTickCount;
  for i := 0 to 100 do
  begin
    setlength(p,0);
    C.MFinder.FindDTMs(dtm, p, 0, 0,CW-1, CH-1, 0);
  end;
  writeln(inttostr(gettickcount - time) + 'ms');
  writeln(inttostr(length(p))+ ' points found');
  setlength(p,0);

  PrintpDTM(tdtmtopDTM(pDTMToTDTM(dtm)));     }

  {for i := 0 to high(p) do
    writeln(format('%d: (%d, %d)', [i, p[i].x, p[i].y]));     }


  //bmp.OnDestroy:=nil;
  //bmp.Free;
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
