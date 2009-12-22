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
    SplitLabel: TLabel;
    SplitEdit: TEdit;
    FShadow: TCheckBox;
    PathButton: TButton;
    OCRButton: TButton;
    Image1: TImage;
    OCRFileOpen: TOpenDialog;
    UpCharsDialog: TSelectDirectoryDialog;
    procedure BitmapButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FShadowChange(Sender: TObject);
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
   Spacing: Integer;

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

  // create and init client
  C := TClient.Create;
  bmp := TMufasaBitmap.Create;
  bmp.LoadFromFile(BitmapPath);
  C.MWindow.SetTarget(bmp);

  Shadow :=FShadow.Checked;

  try
    Spacing := StrToInt(Form1.SplitEdit.Text);
  except
    if shadow then
    begin
      MessageBox(0,pchar('Spacing could not be parsed.' +
      'Defaulting to 2' ), Pchar('Space Error'), MB_OK);
      Spacing := 2;
    end
    else
    begin
      MessageBox(0,pchar('Spacing could not be parsed.' +
      'Defaulting to 1' ), Pchar('Space Error'), MB_OK);
      Spacing := 1;
    end;
  end;

  // DS + .. + DS because InitOCR wants the directory of the Fonts, not UpChars
  // only.
  C.MOCR.InitTOCR(UpTextPath + DS + '..' + DS, Shadow);
  s := C.MOCR.GetUpTextAtEx(7,7, Shadow, Spacing);

  // write to debugbmp
  for y := 0 to C.MOCR.debugbmp.Height - 1 do
    for x := 0 to C.MOCR.debugbmp.Width -1 do
      Form1.Image1.Canvas.Pixels[x,y] := C.MOCR.debugbmp.FastGetPixel(x,y);

  // print ocr'ed text
  Form1.Image1.Canvas.Font.Color:=clRed;
  Form1.Image1.Canvas.TextOut(0, C.MOCR.debugbmp.Height, s);

  Form1.Image1.Picture.SaveToFile('/tmp/ocrbench.bmp');

  C.Free;
end;

procedure TForm1.BitmapButtonClick(Sender: TObject);
begin
  if OCRFileOpen.Execute then
    BitmapPath := OCRFileOpen.FileName;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BitmapPath := '/home/merlijn/Programs/mufasa/pics/uptext2.bmp';
  UpTextPath := '/home/merlijn/Programs/mufasa/Fonts/UpChars';
end;

procedure TForm1.FShadowChange(Sender: TObject);
begin
  if Form1.FShadow.Checked then
    Form1.SplitEdit.Text:='2'
  else
    Form1.SplitEdit.Text:='1';
end;

procedure TForm1.PathButtonClick(Sender: TObject);
begin
  if UpCharsDialog.Execute then
    UpTextPath := UpCharsDialog.FileName;
end;

initialization
  {$I unit1.lrs}

end.

