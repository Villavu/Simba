unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Client, MufasaTypes, Bitmaps;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitmapButton: TButton;
    FShadow: TCheckBox;
    PathButton: TButton;
    OCRButton: TButton;
    Image1: TImage;
    OCRFileOpen: TOpenDialog;
    UpCharsDialog: TSelectDirectoryDialog;
    procedure BitmapButtonClick(Sender: TObject);
    procedure OCRButtonClick(Sender: TObject);
    procedure PathButtonClick(Sender: TObject);
  private
    BitmapPath: String;
    UpTextPath: String;
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

begin
  if not FileExists(BitmapPath) then
  begin
    MessageBox(0,pchar('You did not set a valid bitmap'), Pchar('Bitmap Error'),
                    MB_OK);
    if OCRFileOpen.Execute then
      BitmapPath := OCRFileOpen.FileName;
    Exit;
  end;
  if not DirectoryExists(UpTextPath) then
  begin
    MessageBox(0,pchar('You did not set a UpText Path' ), Pchar('Path Error'),
                    MB_OK);
    if UpCharsDialog.Execute then
      UpTextPath := UpCharsDialog.FileName;
    Exit;
  end;

  Form1.Image1.Canvas.Brush.Color := 0;
  Form1.Image1.Canvas.Rectangle(0, 0, Form1.Image1.Canvas.Width,  Form1.Image1.Canvas.Height);

  C := TClient.Create;
  bmp := TMufasaBitmap.Create;
  bmp.LoadFromFile(BitmapPath);
  C.MWindow.SetTarget(bmp);

  Shadow :=FShadow.Checked;

  C.MOCR.InitTOCR(UpTextPath + DS + '..' + DS, Shadow);
  s := C.MOCR.GetUpTextAt(7,7, Shadow);

  for y := 0 to C.MOCR.debugbmp.Height - 1 do
    for x := 0 to C.MOCR.debugbmp.Width -1 do
      Form1.Image1.Canvas.Pixels[x,y] := C.MOCR.debugbmp.FastGetPixel(x,y);
  Form1.Image1.Canvas.Font.Color:=clRed;
  Form1.Image1.Canvas.TextOut(0, C.MOCR.debugbmp.Height, s);

  C.Free;
end;

procedure TForm1.BitmapButtonClick(Sender: TObject);
begin
  if OCRFileOpen.Execute then
    BitmapPath := OCRFileOpen.FileName;
end;

procedure TForm1.PathButtonClick(Sender: TObject);
begin
  if UpCharsDialog.Execute then
    UpTextPath := UpCharsDialog.FileName;
end;

initialization
  {$I unit1.lrs}

end.

