{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmaptostringform;

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
  public
    dispPic: TSimbaImage;
  end; 

var
  SimbaBitmapConversionForm: TSimbaBitmapConversionForm;

implementation

{$R *.lfm}

uses
  clipbrd, lclintf, lcltype,
  simba.mufasatypes;

const
  BmpSizeTxt = '(%dx%d)';

procedure TSimbaBitmapConversionForm.OpenButtonClick(Sender: TObject);
var
  x : TSimbaImage;
begin
  if OpenPictureDialog.Execute then
  begin
    try
      ImagePreview.Picture.LoadFromFile(OpenPictureDialog.FileName);
      GroupBox.Caption:= Format(BmpSizeTxt,[ImagePreview.Picture.Width,ImagePreview.Picture.Height]);
      x := TSimbaImage.Create;
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

    dispPic := TSimbaImage.Create();
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
const
  PAD_WIDTH = 65;
var
  Str: String;
  Builder: TStringList;
  Offset: Integer;
begin
  if (dispPic <> nil) then
  begin
    Str := dispPic.SaveToString();

    Builder := TStringList.Create();

    if PadOutput.Checked then
    begin
      Builder := TStringList.Create();
      Builder.Add('  BMP := TSimbaImage.CreateFromString(%d, %d,', [dispPic.Width, dispPic.Height]);

      Offset := 1;
      while (Offset < Length(Str) - PAD_WIDTH) do
      begin
        Builder.Add('    ' + #39 + Copy(Str, Offset, PAD_WIDTH) + #39 + ' +');

        Inc(Offset, PAD_WIDTH);
      end;
      Builder.Add('    ' + #39 + Copy(Str, Offset) + #39 + ');');
    end else
      Builder.Add(Format('  BMP := TSimbaImage.CreateFromString(%d, %d, %s);', [dispPic.Width, dispPic.Height, QuotedStr(Str)]));

    SimbaDebugLn([EDebugLn.FOCUS], Builder.Text);

    try
      Clipboard.AsText := Builder.Text;
    except
    end;

    Builder.Free();
  end;
end;

end.

