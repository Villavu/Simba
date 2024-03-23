{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_imagestring;

{$i simba.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs, ExtCtrls, ClipBrd;

type
  TSimbaImageStringForm = class(TForm)
    ClipboardButton: TButton;
    GroupBox: TGroupBox;
    ToStringButton: TButton;
    OpenButton: TButton;
    PadOutput: TCheckBox;
    ImagePreview: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    procedure ClipboardButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure OpenButtonClick(Sender: TObject);
    procedure ToStringButtonClick(Sender: TObject);
  end; 

var
  SimbaImageStringForm: TSimbaImageStringForm;

implementation

{$R *.lfm}

uses
  simba.base, simba.image, simba.image_lazbridge, simba.containers;

procedure TSimbaImageStringForm.OpenButtonClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute() then
  try
    ImagePreview.Picture.LoadFromFile(OpenPictureDialog.FileName);

    GroupBox.Caption := Format('(%d,%d)', [ImagePreview.Picture.Width, ImagePreview.Picture.Height]);
  except
    ImagePreview.Picture.Clear();
  end;
end;

procedure TSimbaImageStringForm.ClipboardButtonClick(Sender: TObject);
begin
  if (Clipboard.HasPictureFormat()) then
  try
    ImagePreview.Picture.Bitmap.LoadFromClipboardFormat(CF_Bitmap);

    GroupBox.Caption := Format('(%d,%d)', [ImagePreview.Picture.Width, ImagePreview.Picture.Height]);
  except
    ImagePreview.Picture.Clear();
  end;
end;

procedure TSimbaImageStringForm.FormCreate(Sender: TObject);
begin
  Width := Scale96ToScreen(500);
  Height := Scale96ToScreen(300);
end;

procedure TSimbaImageStringForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if (Length(FileNames) > 0) then
  try
    ImagePreview.Picture.LoadFromFile(FileNames[0]);

    GroupBox.Caption := Format('(%d,%d)', [ImagePreview.Picture.Width, ImagePreview.Picture.Height]);
  except
    ImagePreview.Picture.Clear();
  end;
end;

procedure TSimbaImageStringForm.ToStringButtonClick(Sender: TObject);
const
  PAD_WIDTH = 65;
var
  ImageString: String;
  Builder: TSimbaStringBuilder;
begin
  if Assigned(ImagePreview.Picture.Bitmap) and (ImagePreview.Picture.Bitmap.Width > 0) and (ImagePreview.Picture.Bitmap.Height > 0) then
  begin
    with LazImage_ToSimbaImage(ImagePreview.Picture.Bitmap) do
    try
      ImageString := SaveToString();
    finally
      Free();
    end;

    if PadOutput.Checked then
    begin
      Builder.Append('Image := TImage.CreateFromString(');
      Builder.AppendLine();

      while (ImageString <> '') do
      begin
        Builder.AppendLine('  ' + #39 + Copy(ImageString, 1, PAD_WIDTH) + #39 + ' +');

        Delete(ImageString, 1, PAD_WIDTH);
      end;

      ImageString := Copy(Builder.Str, 1, Builder.Count - Length(LineEnding) - 2) + ');';
    end else
      ImageString := 'Image := TImage.CreateFromString(' + #39 + ImageString + #39 + ');';

    try
      Clipboard.AsText := ImageString;
    except
    end;

    DebugLn([EDebugLn.FOCUS], ImageString);
  end;
end;

end.

