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

    Colourpicker for the Mufasa Macro Library
}

unit colourpicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf,LCLType,InterfaceBase,Forms,Controls,ExtCtrls,
  Graphics,
  Window

  {$IFNDEF PICKER_CLIENT}
    {$IFDEF LINUX}
    ,x
    {$ENDIF}
  {$ENDIF}
  ;

type
  TPickEvent = procedure (Sender: TObject; Color, X, Y: Integer);

  TMColorPicker = class(TObject)
        constructor Create(aWindow: TMWindow);
        destructor Destroy; override;

        procedure Pick(Out C, X, Y: Integer);

        procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer );
       procedure TimorTimer(Sender: TObject);
       Procedure ColorPickDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  public
        // Will give us CopyClientToBitmap
        Window: TMWindow;
        ColourHistory: TList; // for colour history

        Form : TForm;
        Image: TImage;
        Timor : TTimer;
        Bitmap : Graphics.TBitmap;
        Note : Graphics.TBitmap;
        Brush : TBrush;
        Text : string;
        FPickEvent : TPickEvent;

        oldx, oldy, Color, colorx, colory: Integer;

        TheChangedEvent,TheChangingEvent : TNotifyEvent;
        NoteHandle, BitmapHandle, ImageHandle : HDC;
  public
    property OnPick: TPickEvent read FPickEvent write FPickEvent;
  end;


implementation

constructor TMColorPicker.Create(aWindow: TMWindow);
begin
  Self.Window := aWindow;

end;

destructor TMColorPicker.Destroy;
begin

end;

procedure TMColorPicker.Pick(Out C, X, Y: Integer);
var
   w, h: integer;
   bmp: TBitmap;

   {$IFNDEF PICKER_CLIENT}
     {$IFDEF LINUX}
     OldWindow: TWindow;
     {$ENDIF}
   {$ENDIF}

begin
  Form := TForm.Create(Application.MainForm);
   {$IFNDEF PICKER_CLIENT}
     {$IFDEF LINUX}
     OldWindow := Window.CurWindow;
     Window.SetTarget(Window.DesktopWindow);
     {$ENDIF}
   {$ENDIF}
  w := 0;
  h := 0;
  Window.GetDimensions(w, h);

  Form.Width := w;
  Form.Height := h;
  Form.Top := 0;
  Form.left := 0;
  Form.WindowState := wsmaximized;
  Form.BorderStyle:= bsNone;

  Image := TImage.Create(Form);
  Image.Parent := Form;
  Image.left := 0;
  image.Width := 0;
  Image.width := Form.Width - 1;
  Image.Height := Form.Height - 1;
  Image.Cursor:= crCross;
  Image.OnMouseDown:= @ColorPickDown;
  Image.OnMouseMove:=@ImageMouseMove;
  Image.Canvas.Brush.Color := 14811135;
  Bitmap := Graphics.TBitmap.create;
  Bitmap.width := Form.Width;
  Bitmap.Height := Form.Height;
  Note := Graphics.TBitmap.create;
  Note.Canvas.Brush.Color := 14811135;
  Note.Width := 148;
  Note.Height := 33;
  Note.Canvas.Rectangle(0, 0, 147, 33);
  Note.Canvas.Rectangle(89, 3, 115, 29);
  Note.Canvas.Pen.Style:= psClear;

  bmp := Window.CopyClientToBitmap(0, 0, w, h);
  BitBlt(Image.Canvas.Handle, 0,0,w,h, bmp.Canvas.Handle,0,0,SRCCOPY);
  BitBlt(Bitmap.Canvas.Handle, 0,0,w,h, bmp.Canvas.Handle,0,0,SRCCOPY);
  bmp.Free;

  ImageHandle:= Image.Canvas.Handle;
  BitmapHandle:= Bitmap.Canvas.Handle;
  NoteHandle:= Note.Canvas.Handle;
  TheChangedEvent := Image.Canvas.OnChange;
  TheChangingEvent := Image.Canvas.OnChanging;

  Brush := Image.Canvas.Brush;
  Timor := TTimer.Create(Form);

  Timor.OnTimer:= @TimorTimer;
  Timor.Interval:= 50;
  Timor.Enabled:= False;

  Form.ShowModal;

  // add x to history here.
  c := Color;
  x := Colorx;
  y := Colory;

  {$IFNDEF PICKER_CLIENT}
    {$IFDEF LINUX}
    Window.SetTarget(OldWindow);
    {$ENDIF}
  {$ENDIF}

  Note.Free;
  Bitmap.Free;
  Timor.Free;
  Image.Free;
  Form.Free;
end;

procedure TMColorPicker.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Timor.Enabled:= True;
end;

procedure TMColorPicker.TimorTimer(Sender: TObject);
var
  TempPoint : TPoint;
begin
//  GetCursorPos(TempPoint);
  TempPoint := Mouse.CursorPos;

  { If a form cannot be fully set to 0,0 }
  TempPoint.X := TempPoint.X - Form.Left;
  TempPoint.Y := TempPoint.Y - Form.Top;

  BitBlt(ImageHandle, oldx + 5, oldy + 5,147,33,BitmapHandle,oldx + 5,oldy + 5,SRCCOPY);
  Color := WidgetSet.DCGetPixel(ImageHandle, TempPoint.X, TempPoint.Y);
  Rectangle(NoteHandle,1,1,85,32);
//  Text:='Pos: ' + inttostr(TempPoint.x - Client.Rect.Left) + ','  + inttostr(TempPoint.y - Client.Rect.Bottom);
  Text:='Pos: ' + inttostr(TempPoint.x) + ','  + inttostr(TempPoint.y);
  ExtTextOut(NoteHandle, 5, 3,0,nil,pchar(text),length(text),nil);
  Text := 'Color: ' +  inttostr(Color);
  ExtTextOut(NoteHandle, 5, 15,0,nil,pchar(text),length(text),nil);
  BitBlt( ImageHandle, TempPoint.x + 5, TempPoint.y + 5,147,33,NoteHandle,0,0,SRCCOPY);
  Brush.Color := Color;
  Image.Canvas.Rectangle(TempPoint.x + 123, TempPoint.y + 8, tempPoint.x + 149, temppoint.y + 34);
//  Rectangle(ImageHandle,TempPoint.x + 123, TempPoint.y + 8, tempPoint.x + 149, temppoint.y + 34);
  TheChangingEvent(Sender);
  StretchBlt(ImageHandle,TempPoint.x + 95, TempPoint.y + 9, 24,24, BitmapHandle, TempPoint.x - 1, TempPoint.y-1,3,3, SRCCOPY);
  TheChangedEvent(Sender);
  Oldx := TempPoint.x;
  Oldy := TempPoint.y;
  Timor.Enabled:= False;
end;

procedure TMColorPicker.ColorPickDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin;
  Color:=  WidgetSet.DCGetPixel(Image.Canvas.Handle,x,y);
  Self.Colorx := x;
  Self.Colory := y;
  Timor.enabled := false;
  if OnPick <> nil then
    Onpick(Sender,Color,x,y);
  Form.Close;
end;

end.

