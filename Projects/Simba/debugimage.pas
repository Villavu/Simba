{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

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
    procedure DrawImageResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { private declarations }
  public
    procedure BlackDebugImage;
    procedure DrawBitmap(ToDrawBmp: TMufasaBitmap);
    procedure GetDebugImage(GetDbgBmp : TMufasaBitmap);
    procedure ShowDebugImgForm(DispSize : TPoint);
    { public declarations }
  end;


var
  DebugImgForm: TDebugImgForm;

implementation

uses
  MufasaTypes, math, graphtype, IntfGraphics,SimbaUnit,lclintf,colour_conv,InterfaceBase;
{ TDebugImgForm }

procedure TDebugImgForm.FormCreate(Sender: TObject);
begin
  BlackDebugImage;
end;

procedure TDebugImgForm.DrawImageResize(Sender: TObject);
begin
  DrawImage.Picture.Graphic.Width := DrawImage.Width;
  DrawImage.Picture.Graphic.Height := DrawImage.Height;
  BlackDebugImage;
end;

procedure TDebugImgForm.FormHide(Sender: TObject);
begin
  SimbaForm.MenuItemDebugImage.Checked := False;
end;

procedure TDebugImgForm.BlackDebugImage;
begin
  DrawImage.Canvas.Brush.Color:= clBlack;
  DrawImage.Canvas.Pen.Color:= clBlack;
  DrawImage.Canvas.Rectangle(0,0,DrawImage.Width,DrawImage.Height);
  DrawImage.Repaint;
end;

procedure TDebugImgForm.DrawBitmap(ToDrawBmp : TMufasaBitmap);
var
  Bitmap : Graphics.TBitmap;
begin
  if ToDrawBmp = nil then
    raise Exception.Create('ERROR in TDebugImgForm.DrawBitmap: ToDrawBmp = nil');
  Bitmap := ToDrawBmp.ToTBitmap;
  DrawImage.Canvas.Draw(0,0,Bitmap);
  DrawImage.Repaint;
  Bitmap.Free;
end;

procedure TDebugImgForm.GetDebugImage(GetDbgBmp : TMufasaBitmap);
begin;
  GetDbgBmp.LoadFromRawImage(DrawImage.Picture.Bitmap.RawImage);
end;

procedure TDebugImgForm.ShowDebugImgForm(DispSize : TPoint);
begin
  if not Visible then
    show;
  if (DispSize.x <> Width) or (DispSize.y  <> height) then
  begin;
    Width := DispSize.x;
    Height := DispSize.y;
  end;
  FormStyle := fsStayOnTop;
end;

initialization
  {$R *.lfm}

end.

