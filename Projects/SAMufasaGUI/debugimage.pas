{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

    Image debug window for Mufasa Macro Library
}
unit debugimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, bitmaps;

type

  { TDebugImgForm }

  TDebugImgForm = class(TForm)
    DrawImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    DispSize : TPoint;
    ToDrawBmp: TMufasaBitmap;//The bitmap we should draw!
    GetDbgBmp : TMufasaBitmap;
    procedure BlackDebugImage;
    procedure DrawBitmap;
    procedure GetDebugImage;
    procedure ShowDebugImgForm; //Uses the global var for w/h
    { public declarations }
  end;


var
  DebugImgForm: TDebugImgForm;

implementation

uses
  MufasaTypes, math,windowutil,graphtype, IntfGraphics,TestUnit,lclintf,colour_conv,InterfaceBase;
{ TDebugImgForm }

procedure TDebugImgForm.FormCreate(Sender: TObject);
begin
  BlackDebugImage;
end;

procedure TDebugImgForm.FormHide(Sender: TObject);
begin
  Form1.MenuItemDebugImage.Checked := False;
end;

procedure TDebugImgForm.FormResize(Sender: TObject);
begin
  DrawImage.Picture.Graphic.Width := DrawImage.Width;
  DrawImage.Picture.Graphic.Height := DrawImage.Height;
  BlackDebugImage;
end;

procedure TDebugImgForm.BlackDebugImage;
begin
  DrawImage.Canvas.Brush.Color:= clBlack;
  DrawImage.Canvas.Pen.Color:= clBlack;
  DrawImage.Canvas.Rectangle(0,0,DrawImage.Width,DrawImage.Height);
end;

procedure TDebugImgForm.DrawBitmap;
var
  rawImage : TRawImage;
  Bitmap : Graphics.TBitmap;
begin
  if ToDrawBmp = nil then
    raise Exception.Create('ERROR in TDebugImgForm.DrawBitmap: ToDrawBmp = nil');
  ArrDataToRawImage(ToDrawBmp.FData,Point(ToDrawBmp.width,ToDrawBmp.height),RawImage);
  Bitmap := Graphics.TBitmap.Create;
  Bitmap.LoadFromRawImage(Rawimage,false);
  DrawImage.Canvas.Draw(0,0,Bitmap);
  Bitmap.Free;
end;

procedure TDebugImgForm.GetDebugImage;
begin;
  GetDbgBmp.LoadFromRawImage(DrawImage.Picture.Bitmap.RawImage);
end;

procedure TDebugImgForm.ShowDebugImgForm;
begin
  Show;
  if (DispSize.x <> Width) or (DispSize.y  <> height) then
  begin;
    Width := DispSize.x;
    Height := DispSize.y;
  end;
end;

initialization
  {$I debugimage.lrs}

end.

