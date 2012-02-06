unit ocrtoolunit;

{$mode objfpc}{$H+}

//{$DEFINE OCRDEBUG}


{
  Features:
    - Change Bitmap
    - Set Target (Client)
    - Set Font Directory
    - Change filter(s)(?)
    - Change coords to start at
    - Output text in TEdit?
    - Interactive / show individual step.
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,

  // Custom
  Client, MufasaTypes, mufasabase, Bitmaps, ocr, windowselector, iomanager
  {$IFDEF MSWINDOWS} ,os_windows {$ENDIF}
  {$IFDEF LINUX} ,os_linux {$ENDIF}     ;

type

  { TForm1 }

  TForm1 = class(TForm)
    XEdit: TEdit;
    YEdit: TEdit;
    Label1: TLabel;
    ShadowCheckbox: TCheckBox;
    OCRButton: TButton;
    UpCharsDialog: TSelectDirectoryDialog;
    SetFontButton: TButton;
    OCRFileOpen: TOpenDialog;
    SetBitmapButton: TButton;
    SetClientButton: TButton;
    EditFiltersButton: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure OCRButtonClick(Sender: TObject);
    procedure SetClientButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFontButtonClick(Sender: TObject);
    procedure SetBitmapButtonClick(Sender: TObject);
    function RunChecks: Boolean;
  private
    { private declarations }
    BitmapPath: String;
    FontPath: String;
    XXX, YYY: Integer;

    CliW: TIOManager;
    UseClient: Boolean;

  public
    { public declarations }
  end; 

var
  Form1: TForm1;

implementation
uses
    lclintf, lcltype    ;

{$R *.lfm}

{ TForm1 }

function TForm1.RunChecks: boolean;

begin
  XXX := StrToIntDef(XEdit.Text, -1);
  if XXX = -1 then
    Exit(False);
  YYY := StrToIntDef(YEdit.Text, -1);
  if YYY = -1 then
    Exit(False);
  Result := True;
end;

procedure CleanseBitmap(bmp: TMufasaBitmap; w, h: Integer);

var
   x, y, ow, oh: Integer;
begin
  ow := bmp.width; oh := bmp.height;
  if (ow < w) or (oh < h) then
  begin
    bmp.SetSize(w, h);
    for x := ow to w - 1 do
      for y := oh to h - 1  do
      begin
        bmp.FastSetPixel(x, y, clWhite);
      end;
  end;

end;

procedure TForm1.OCRButtonClick(Sender: TObject);
Var
   C: TClient;
   bmp: TMufasaBitmap;
   x,y, ii: integer;
   s: string;
   Shadow: boolean;
   t: dword;

begin
  if not RunChecks then
    exit;

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
  C := TClient.Create('');
  bmp := TMufasaBitmap.Create;
  if UseClient then
    C.IOManager.SetTarget(TWindow(CliW.GetImageTarget).GetNativeWindow())
  else
  begin
    bmp.LoadFromFile(BitmapPath);
      CleanseBitmap(bmp, 401 + XXX, 21 + YYY);
    C.IOManager.SetTarget(bmp);
  end;

  Shadow := ShadowCheckbox.Checked;

  // DS + .. + DS because InitOCR wants the directory of the Fonts, not UpChars
  // only.
  C.MOCR.InitTOCR(FontPath + DS);
  //C.MOCR.SetFonts(C.MOCR.GetFonts);


  t:=gettickcount;

  s := C.MOCR.GetUpTextAtEx(XXX, YYY, Shadow);

  writeln(inttostr(gettickcount-t));

 { for  ii := 1 to length(s) do
    writeln('Text found: ' + s[ii]);     }

  writeln('Text: ' + s);


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

  bmp.OnDestroy:=nil;
  bmp.Free;
  C.Free;
  Application.ProcessMessages;

end;

procedure TForm1.SetClientButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
   WS: TMWindowSelector;
begin
  UseClient := True;
  if not assigned(CliW) then
    CliW := TIOManager.Create;
  WS := TMWindowSelector.Create(CliW);
  CliW.SetTarget(WS.Drag);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BitmapPath := '/home/merlijn/Programs/simba/uptext11.png';
  FontPath := '/home/merlijn/Programs/simba/Fonts/';
end;

procedure TForm1.SetFontButtonClick(Sender: TObject);
begin
  if UpCharsDialog.Execute then
    FontPath := UpCharsDialog.FileName;
end;

procedure TForm1.SetBitmapButtonClick(Sender: TObject);
begin
  if OCRFileOpen.Execute then
  begin
    BitmapPath := OCRFileOpen.FileName;
    UseClient:=False;
  end;
end;


end.

