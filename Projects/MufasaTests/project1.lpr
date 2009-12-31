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
  bitmaps,{x ,}mufasatypes,dtm,dtmutil, ocrutil ,graphics ,colour_conv,math,
  updater


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

var
   aTime: dword;

function myChange: boolean;
begin
  if gettickcount-atime > 1000 then
    result := true
  else
    result := false;
end;

procedure MufasaTests.DoRun;


{const
    ocr_Limit_High = 191;
    ocr_Limit_Low = 65;

    ocr_White = 16777215;
    ocr_Green = 65280;
    ocr_Red = 255;
    ocr_Yellow = 65535;
    ocr_Blue = 16776960;
    ocr_ItemC = 16744447;

    ocr_Purple = 8388736;   }

var
  ErrorMsg: String;
{  Time: DWord;
  C: TClient;
  I, w, h,x,y: Integer;
  dtm: pdtm;
  p:tpointarray;
  bmp, bmprs: TMufasaBitmap;
  tbmp: TBitmap;
  r,g,b:integer;
  t:Dword;    }
  up: TMMLFileDownloader;

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

  atime:=gettickcount;

  up := TMMLFileDownloader.Create;
  up.FileURL:='http://www.villavu.com/pics/desktop.png';
  up.ReplacementFile:='test.png';
  up.OnBeat:=@myChange;
  try
    up.DownloadAndSave;
    up.Replace;
  finally
    writeln(inttostr(gettickcount-atime));
    up.Free;
  end;




  { clOlive = false point }
  { clSilver = false shadow }
  { clLime = false shadow}

  { add your program here }

{  tbmp:=TBitmap.Create;
  tbmp.LoadFromFile('/home/merlijn/Programs/mufasa/pics/16.bmp');

  bmprs := TMufasaBitmap.Create;
  bmprs.SetSize(10,10);

  bmprs.LoadFromRawImage(tbmp.RawImage);
  tbmp.Free;
  tbmp := bmprs.ToTBitmap;
                                  }
 { for y := 0 to tbmp.Height -1 do
    for x := 0 to tbmp.width -1 do
    begin
      writeln(format('(%d, %d) = %d , %d', [x,y,tbmp.Canvas.pixels[x,y],bmprs.FastGetPixel(x,y)]));
      colortorgb(tbmp.Canvas.pixels[x,y],r,g,b);
      writeln(format('%d,%d,%d', [r,g,b]));
      colortorgb(bmprs.FastGetPixel(x,y),r,g,b);
      writeln(format('%d,%d,%d', [r,g,b]));
    end;     }
  //bmprs.LoadFromFile('/home/merlijn/Programs/mufasa/pics/16.bmp');
  {C := TClient.Create;
  C.MWindow.SetTarget(bmprs);    }


 { bmp.SaveToFile('/tmp/output.bmp');
  tbmp.SaveToFile('/tmp/output2.bmp');
    //bmp.OnDestroy:=nil;
  bmp.Free;
  tbmp.Free;  }
 // C.Free;

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
