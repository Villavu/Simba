unit bitmapconv;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, FileUtil, bitmaps, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, ExtDlgs, ExtCtrls;

type

  { TBitmapConvForm }

  TBitmapConvForm = class(TForm)
    GroupBox: TGroupBox;
    ToStringButton: TButton;
    OpenButton: TButton;
    PadOutput: TCheckBox;
    ImagePreview: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    procedure OpenButtonClick(Sender: TObject);
    procedure ToStringButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    dispPic : TMufasaBitmap;
    { public declarations }
  end; 

var
  BitmapConvForm: TBitmapConvForm;

implementation
uses
  SimbaUnit;
const
  BmpSizeTxt = '(%dx%d)';
{$R *.lfm}

{ TBitmapConvForm }

procedure TBitmapConvForm.OpenButtonClick(Sender: TObject);
var
  x : TMufasaBitmap;
begin
  if OpenPictureDialog.Execute then
  begin
    try
      ImagePreview.Picture.LoadFromFile(OpenPictureDialog.FileName);
      GroupBox.Caption:= Format(BmpSizeTxt,[ImagePreview.Picture.Width,ImagePreview.Picture.Height]);
      x := TMufasaBitmap.Create;
      x.LoadFromFile(OpenPictureDialog.FileName);
      if dispPic <> nil then
        dispPic.Free;
      dispPic := x;
    except
      SimbaForm._WriteLn('ERROR loading the file: ' + OpenPictureDialog.FileName);
      ImagePreview.Picture := nil;
      if dispPic <> nil then
        FreeAndNil(dispPic);
    end;
  end;
end;

procedure TBitmapConvForm.ToStringButtonClick(Sender: TObject);
var
  str : string;
  strend : string;
  len : integer;
begin
  if dispPic <> nil then
  begin
    str := '  Bmp := BitmapFromString('+
           inttostr(disppic.Width)+ ', ' + inttostr(disppic.height) +', '#39 + dispPic.ToString;
    strend :=  #39 +');';
    len := length(str);
    if PadOutput.Checked then
      while Len > 65 do
      begin
        SimbaForm._WriteLn(Copy(str,1,62) + #39 + ' +');
        delete(str,1,62);
        str := StringOfChar(' ',8) + #39 + str;
        len := length(str);
      end;
    SimbaForm._WriteLn(str + strend);
  end;
end;

end.

