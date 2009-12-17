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
  bitmaps,{x ,}mufasatypes,dtm,dtmutil, ocrutil ,graphics ,colour_conv,math


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
    writeln(format('dtm: (%d, %d) c: %d, t: %d, asz: %d', [result.p[i].x,
            result.p[i].y,  result.c[i], result.t[i], result.asz[i]]));
  end;
  result.c[0] := 255;
end;

procedure MufasaTests.DoRun;

const
    ocr_Limit_High = 192;
    ocr_Limit_Low = 65;

var
  ErrorMsg: String;
  Time: DWord;
  C: TClient;
  I, w, h,x,y: Integer;
  dtm: pdtm;
  p:tpointarray;
  bmp, bmprs: TMufasaBitmap;
  cyan, itemc:integer;
  r,g,b:integer;
  t:Dword;

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
  cyan := rgbtocolor(0,255,255);


  bmprs := TMufasaBitmap.Create;
  bmprs.LoadFromFile('/home/merlijn/Programs/mufasa/pics/uptext7.png');
  C := TClient.Create;
  C.MWindow.SetTarget(bmprs);
  C.MWindow.GetDimensions(w, h);

  bmp := TMufasaBitmap.Create;
  bmp.CopyClientToBitmap(C.MWindow, True, 0, 0, 450, 50);

  t:=gettickcount;

  for y := 0 to bmp.Height - 1 do
    for x := 0 to bmp.Width - 1 do
    begin
      colortorgb(bmp.fastgetpixel(x,y),r,g,b);
      // the abs(g-b) < 15 seems to help heaps when taking out crap points
      if (r > ocr_Limit_High) and (g > ocr_Limit_High) and (b > ocr_Limit_High){ and (abs(g-b) < 15)} then
      begin
        bmp.fastsetpixel(x,y,clwhite);
        continue;
      end;
      if (r < ocr_Limit_Low) and (g > ocr_Limit_High) and (b > ocr_Limit_High) then
      begin
        bmp.fastsetpixel(x,y,cyan);
        continue;
      end;
      if (r < ocr_Limit_Low) and (g > ocr_Limit_High) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,rgbtocolor(0,255,0));
        continue;
      end;
      if(r > ocr_Limit_High) and (g > 100) and (g < ocr_Limit_High) and (b > 30) and (b < 90) then
      begin
        bmp.fastsetpixel(x,y,rgbtocolor(255,127,0));
        continue;
      end;
      if(r > ocr_Limit_High) and (g > ocr_Limit_High) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,rgbtocolor(255,255,0));
        continue;
      end;
      // better use g < 40 than ocr_Limit_Low imo
      if (r > ocr_Limit_High) and (g < ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,rgbtocolor(255,0,0));
        continue;
      end;

      bmp.fastsetpixel(x,y,0);
    end;
    writeln(inttostr(gettickcount-t));
  {
  bmp.Posterize(130); // ~ 3
 // bmp.Contrast(3);

  for y := 0 to bmp.Height - 1 do
    for x := 0 to bmp.Width - 1 do
    begin
      if bmp.FastGetPixel(x, y) = clWhite then
        continue;
      //cyan
      if bmp.FastGetPixel(x, y) = rgbtocolor(0,255,255) then
        continue;
      //green
      if bmp.FastGetPixel(x, y) = rgbtocolor(0,255,0) then
        continue;

      //item // TODO -> 5.bmp = not OK
      if bmp.FastGetPixel(x, y) = rgbtocolor(255,130,0) then
        continue;

      //yellow, interact
      if bmp.FastGetPixel(x, y) = rgbtocolor(255,255,0) then
        continue;

      bmp.fastsetpixel(x,y,0);
    end;
                }




  bmp.SaveToFile('/tmp/output.bmp');
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
