{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Bitmap Conversion form for Simba
}
unit bitmapconv;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, FileUtil, bitmaps, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, ExtDlgs, ExtCtrls;

type

  { TBitmapConvForm }

  TBitmapConvForm = class(TForm)
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
    { private declarations }
  public
    dispPic : TMufasaBitmap;
    { public declarations }
  end; 

var
  BitmapConvForm: TBitmapConvForm;

implementation
uses
  SimbaUnit, clipbrd, LCLIntf, LCLType;
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
      formWritelnEx('ERROR loading the file: ' + OpenPictureDialog.FileName);
      ImagePreview.Picture := nil;
      if dispPic <> nil then
        FreeAndNil(dispPic);
    end;
  end;
end;

procedure TBitmapConvForm.ClipboardButtonClick(Sender: TObject);
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
        formWritelnEx(Copy(str,1,62) + #39 + ' +');
        delete(str,1,62);
        str := StringOfChar(' ',8) + #39 + str;
        len := length(str);
      end;
    formWritelnEx(str + strend);
  end;
end;

end.

