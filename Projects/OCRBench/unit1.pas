unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Client, MufasaTypes, Bitmaps, ocr, windowselector,window;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitmapButton: TButton;
    FShadow: TCheckBox;
    PathButton: TButton;
    OCRButton: TButton;
    Image1: TImage;
    OCRFileOpen: TOpenDialog;
    ClientButton: TToggleBox;
    UpCharsDialog: TSelectDirectoryDialog;
    procedure BitmapButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OCRButtonClick(Sender: TObject);
    procedure PathButtonClick(Sender: TObject);
    procedure SelectClient(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    BitmapPath: String;
    FontPath: String;

    CliW: TMWindow;
    UseClient: Boolean;
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  lclintf, lcltype;

{ TForm1 }

procedure TForm1.OCRButtonClick(Sender: TObject);
Var
   C: TClient;
   bmp: TMufasaBitmap;
   x,y: integer;
   s: string;
   Shadow: boolean;
   t: dword;

begin
  if not FileExists(BitmapPath) and not UseClient then
  begin
    MessageBox(0,pchar('You did not set a valid bitmap'), Pchar('Bitmap Error'),
                    MB_OK);
    if OCRFileOpen.Execute then
      BitmapPath := OCRFileOpen.FileName;
    Exit;
  end;
  if not DirectoryExists(FontPath) then
  begin
    MessageBox(0,pchar('You did not set a FontPath' ), Pchar('Path Error'),
                    MB_OK);
    if UpCharsDialog.Execute then
      FontPath := UpCharsDialog.FileName;
    Exit;
  end;

  Form1.Image1.Canvas.Brush.Color := 0;
  Form1.Image1.Canvas.Rectangle(0, 0, Form1.Image1.Canvas.Width,  Form1.Image1.Canvas.Height);

  // create and init client
  C := TClient.Create;
  bmp := TMufasaBitmap.Create;
  if UseClient then
    C.MWindow.SetWindow(CliW)
  else
  begin
    bmp.LoadFromFile(BitmapPath);
    C.MWindow.SetTarget(bmp);
  end;

  Shadow :=FShadow.Checked;

  // DS + .. + DS because InitOCR wants the directory of the Fonts, not UpChars
  // only.
  C.MOCR.InitTOCR(FontPath + DS, Shadow);


  t:=gettickcount;

  s := C.MOCR.GetUpTextAtEx(7, 7, Shadow);

  writeln(inttostr(gettickcount-t));


  // write to debugbmp
  {$IFDEF OCRDEBUG}
  for y := 0 to C.MOCR.debugbmp.Height - 1 do
    for x := 0 to C.MOCR.debugbmp.Width -1 do
      Form1.Image1.Canvas.Pixels[x,y] := C.MOCR.debugbmp.FastGetPixel(x,y);
  // print ocr'ed text

  Form1.Image1.Canvas.Font.Color:=clRed;
  Form1.Image1.Canvas.TextOut(0, C.MOCR.debugbmp.Height, s);

  C.MOCR.debugbmp.Free;
  {$ELSE}
  Form1.Image1.Canvas.Font.Color:=clRed;
  Form1.Image1.Canvas.TextOut(0, 0, s);
  {$ENDIF}
  {$IFDEF OCRDEBUG}
  Form1.Image1.Picture.SaveToFile(OCRDebugPath + 'ocrbench.bmp');
  {$ENDIF}

  bmp.Free;
  C.Free;
  Application.ProcessMessages;
end;

procedure TForm1.BitmapButtonClick(Sender: TObject);
begin
  if OCRFileOpen.Execute then
  begin
    BitmapPath := OCRFileOpen.FileName;
    UseClient:=False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UseClient := False;
  {BitmapPath := '/home/merlijn/Programs/mufasa/pics/uptext4.bmp'; }
  FontPath := '/home/merlijn/Programs/mufasa/Fonts/';
end;

procedure TForm1.PathButtonClick(Sender: TObject);
begin
  if UpCharsDialog.Execute then
    FontPath := UpCharsDialog.FileName;
end;

procedure TForm1.SelectClient(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

Var
   WS: TMWindowSelector;
begin
  UseClient := True;
  if not assigned(CliW) then
    CliW := TMWindow.Create;
  WS := TMWindowSelector.Create(CliW);
  CliW.SetTarget(WS.Drag);
end;

initialization
  {$I unit1.lrs}

end.

