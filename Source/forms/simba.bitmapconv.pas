{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmapconv;

{$i simba.inc}

interface

uses
  Classes, SysUtils, FileUtil, simba.bitmap, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtDlgs, ExtCtrls;

type
  TSimbaBitmapConversionForm = class(TForm)
    ClipboardButton: TButton;
    GroupBox: TGroupBox;
    ToStringButton: TButton;
    OpenButton: TButton;
    PadOutput: TCheckBox;
    ImagePreview: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    procedure ClipboardButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure ToStringButtonClick(Sender: TObject);
  private
  public
    dispPic : TMufasaBitmap;
  end; 

var
  SimbaBitmapConversionForm: TSimbaBitmapConversionForm;

implementation

{$R *.lfm}

uses
  clipbrd, lclintf, lcltype,
  simba.debugform;

const
  BmpSizeTxt = '(%dx%d)';

procedure TSimbaBitmapConversionForm.OpenButtonClick(Sender: TObject);
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
      ImagePreview.Picture := nil;
      if dispPic <> nil then
        FreeAndNil(dispPic);
    end;
  end;
end;

procedure TSimbaBitmapConversionForm.ClipboardButtonClick(Sender: TObject);
begin
  if (Clipboard.HasPictureFormat()) then
  try
    ImagePreview.Picture.Bitmap.LoadFromClipboardFormat(CF_Bitmap);
    GroupBox.Caption := Format(BmpSizeTxt, [ImagePreview.Picture.Width, ImagePreview.Picture.Height]);

    if Assigned(dispPic) then
      FreeAndNil(dispPic);

    dispPic := TMufasaBitmap.Create();
    dispPic.LoadFromTBitmap(ImagePreview.Picture.Bitmap);
  except
    on E: Exception do
    begin
      MessageDlg('Error Loading', 'Cannot load bitmap from clipboard. ' + LineEnding + E.Message, mtError, [mbOK], 0);
      ImagePreview.Picture.Assign(nil);
    end;
  end else
    MessageDlg('Invalid Clipboard?', 'Cannot create bitmap from clipboard.', mtError, [mbOK], 0);
end;

procedure TSimbaBitmapConversionForm.ToStringButtonClick(Sender: TObject);
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
        SimbaDebugForm.Add(Copy(str,1,62) + #39 + ' +');
        delete(str,1,62);
        str := StringOfChar(' ',8) + #39 + str;
        len := length(str);
      end;
    SimbaDebugForm.Add(str + strend);
  end;
end;

end.

