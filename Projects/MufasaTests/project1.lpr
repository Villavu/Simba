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
    ocr_Limit_High = 191;
    ocr_Limit_Low = 65;

    ocr_White = 16777215;
    ocr_Green = 65280;
    ocr_Red = 255;
    ocr_Yellow = 65535;
    ocr_Blue = 16776960;
    ocr_ItemC = 16744447;

    ocr_Purple = 8388736;

var
  ErrorMsg: String;
  Time: DWord;
  C: TClient;
  I, w, h,x,y: Integer;
  dtm: pdtm;
  p:tpointarray;
  bmp, bmprs: TMufasaBitmap;
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


  { clOlive = false point }
  { clSilver = false shadow }
  { clLime = false shadow}

  { add your program here }


  bmprs := TMufasaBitmap.Create;
  bmprs.LoadFromFile('/home/merlijn/Programs/mufasa/pics/16.bmp');
  C := TClient.Create;
  C.MWindow.SetTarget(bmprs);
  C.MWindow.GetDimensions(w, h);

  writeln(inttostr(clpurple));

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
        bmp.fastsetpixel(x,y,ocr_White);
        continue;
      end;
      if (r < ocr_Limit_Low) and (g > ocr_Limit_High) and (b > ocr_Limit_High) then
      begin
        bmp.fastsetpixel(x,y,ocr_Blue);
        continue;
      end;
      if (r < ocr_Limit_Low) and (g > ocr_Limit_High) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Green);
        continue;
      end;

      // false results with fire
      if(r > ocr_Limit_High) and (g > 100) and (g < ocr_Limit_High) and (b > 40) and (b < 90) then
      begin
        bmp.fastsetpixel(x,y,ocr_ItemC);
        continue;
      end;
      if(r > ocr_Limit_High) and (g > ocr_Limit_High) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Yellow);
        continue;
      end;
      // better use g < 40 than ocr_Limit_Low imo
      if (r > ocr_Limit_High) and (g < ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Red);
        continue;
      end;

      if (r < ocr_Limit_Low) and (g < ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.FastSetPixel(x,y, ocr_Purple);
        continue;
      end;

      bmp.fastsetpixel(x,y,0);
    end;

    // increase height by 1, so our algo works better. (shadow)
    bmp.SetSize(Bmp.Width, Bmp.Height+1);
    for x := 0 to bmp.width -1 do
      bmp.fastsetpixel(x,bmp.height-1,0);

   for y := 0 to bmp.Height - 2 do
     for x := 0 to bmp.Width - 2 do
     begin
       if bmp.fastgetpixel(x,y) = clPurple then
         continue;
       if bmp.fastgetpixel(x,y) = clBlack then
         continue;
       if (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x+1,y+1)) and (bmp.fastgetpixel(x+1,y+1) <> clpurple) then
         bmp.fastsetpixel(x,y,{clAqua}0);
     end;

     { Optional - remove false shadow }
   for y := bmp.Height - 1 downto 1 do
     for x := bmp.Width - 1 downto 1 do
     begin
       if bmp.fastgetpixel(x,y) <> clPurple then
         continue;
       if bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x-1,y-1) then
       begin
         bmp.fastsetpixel(x,y,clSilver);
         continue;
       end;
       if bmp.fastgetpixel(x-1,y-1) = 0 then
         bmp.fastsetpixel(x,y,clLime);
     end;

   { remove bad points }
   for y := bmp.Height - 2 downto 1 do
     for x := bmp.Width - 2 downto 1 do
     begin
       if bmp.fastgetpixel(x,y) = clPurple then
         continue;
       if bmp.fastgetpixel(x,y) = clBlack then
         continue;
       if (bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x+1,y+1) ) then
         continue;

       if bmp.fastgetpixel(x+1,y+1) <> clPurple then
       begin
         bmp.fastsetpixel(x,y,clOlive);
         continue;
       end;
     end;

   { Dangerous removes all pixels that had no pixels on x-1 or x+1}
 {  for y := 0 to bmp.Height - 2 do
     for x := 1 to bmp.Width - 2 do
     begin
       if bmp.fastgetpixel(x,y) = clBlack then continue;
       if bmp.fastgetpixel(x,y) = clPurple then continue;
       if bmp.fastgetpixel(x,y) = clOlive then continue;
       if bmp.fastgetpixel(x,y) = clSilver then continue;
       if bmp.fastgetpixel(x,y) = clLime then continue;
       if (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x+1,y) )  and
          (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x-1,y) ) then
          bmp.fastsetpixel(x,y,clFuchsia);
     end;                                }

    writeln(inttostr(gettickcount-t));




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
