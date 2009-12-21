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
  Window,MufasaTypes

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

        procedure ImageMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure ImageInfoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
       Procedure ColorPickDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  public
        // Will give us CopyClientToBitmap
        Window: TMWindow;

        { Form components }
        ScreenForm, InfoForm : TForm;
        ImageMain, ImageInfo: TImage;
        Text : string;
        FPickEvent : TPickEvent;

        { Some temp vars }
        oldx, oldy, Color, colorx, colory: Integer;
//        targetleft,targettop : integer;

        TheChangedEvent,TheChangingEvent : TNotifyEvent;

        { Handles }
        InfoHandle, ImageHandle : HDC;
  public
    property OnPick: TPickEvent read FPickEvent write FPickEvent;
  end;


implementation

constructor TMColorPicker.Create(aWindow: TMWindow);
begin
  inherited Create;
  Self.Window := aWindow;
end;

destructor TMColorPicker.Destroy;
begin

  inherited Destroy;
end;

var
  closed: Boolean;

procedure TMColorPicker.Pick(Out C, X, Y: Integer);
var
   w, h: integer;
   box : TBox;

   {$IFNDEF PICKER_CLIENT}
     {$IFDEF LINUX}
     OldWindow: TWindow;
     {$ELSE}
     OldWindow: HWND;
     {$ENDIF}
   {$ENDIF}

begin
  { We create a Form, with the client image on it. }
  ScreenForm := TForm.Create(Application.MainForm);
  InfoForm := TForm.Create(ScreenForm);

{  if Window.GetDimensionBox(box) then
  begin;
    targetleft := box.x1;
    targettop := box.y1;
  end else
  begin;
    targetleft := 0;
    targettop := 0;
  end; }
   {$IFNDEF PICKER_CLIENT}
     {$IFDEF LINUX}
     OldWindow := Window.CurWindow;
     {$ELSE}
     OldWindow := Window.TargetHandle;
     {$ENDIF}
     Window.SetDesktop;
   {$ENDIF}
  w := 0;
  h := 0;
  Window.GetDimensions(w, h);

  ScreenForm.Caption := 'SimbaColourPicker';
  ScreenForm.Width := w;
  ScreenForm.Height := h;
  ScreenForm.Top := 0;
  ScreenForm.left := 0;
  ScreenForm.WindowState := wsmaximized;
  ScreenForm.BorderStyle:= bsNone;
  ScreenForm.FormStyle := fsStayOnTop;

  InfoForm.Width := 173;
  InfoForm.Height := 33;
  InfoForm.BorderStyle := bsNone;
  InfoForm.FormStyle := fsStayOnTop;
  InfoForm.Left := Mouse.CursorPos.X + 5;
  InfoForm.Top := Mouse.CursorPos.Y - 16;

  ImageInfo := TImage.Create(InfoForm);
  ImageInfo.Parent := InfoForm;
  ImageInfo.Left := 0;
  ImageInfo.Top := 0;
  ImageInfo.Width := 173;
  ImageInfo.Height := 33;
  ImageInfo.Cursor := crCross;
  ImageInfo.OnMouseMove := @ImageInfoMouseMove;
  ImageInfo.Canvas.Brush.Color := 14811135;
  ImageInfo.Canvas.Rectangle(0, 0, 173, 33);
  ImageInfo.Canvas.Rectangle(114, 3, 140, 29);
  ImageInfo.Canvas.Rectangle(142, 3, 168, 29);
  ImageInfo.Canvas.Pen.Style := psClear;

  ImageMain := TImage.Create(ScreenForm);
  ImageMain.Parent := ScreenForm;
  ImageMain.left := 0;
  ImageMain.top := 0;
  ImageMain.width := ScreenForm.Width;
  ImageMain.Height := ScreenForm.Height;
  ImageMain.Cursor:= crCross;
  ImageMain.OnMouseDown:= @ColorPickDown;
  ImageMain.OnMouseMove:=@ImageMainMouseMove;

  ImageMain.Picture.Bitmap := Window.CopyClientToBitmap(0, 0, w - 1, h - 1);

  ImageHandle:= ImageMain.Canvas.Handle;
  InfoHandle:= ImageInfo.Canvas.Handle;
  TheChangedEvent := ImageMain.Canvas.OnChange;
  TheChangingEvent := ImageMain.Canvas.OnChanging;

  ScreenForm.Show;
  InfoForm.Show;

  closed := False;

  while not Closed do //CBA to do this a better way...
  begin
    sleep(1);
    Application.ProcessMessages;
  end;

  // add x to history here.
  c := Color;
  x := Colorx;
  y := Colory;

  {$IFNDEF PICKER_CLIENT}
    {$IFDEF LINUX}
    Window.SetTarget(OldWindow);
    {$ELSE}
    Window.SetTarget(OldWindow, w_Window);
    {$ENDIF}
  {$ENDIF}

  ImageMain.Free;
  ImageInfo.Free;
  InfoForm.Free;
  ScreenForm.Free;
end;

procedure TMColorPicker.ImageMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempPoint : TPoint;
  Data : TRetData;
  R : TRect;
  px, py : Integer;
begin
  InfoForm.Left := Mouse.CursorPos.X + 5;
  InfoForm.Top := Mouse.CursorPos.Y - 16;

  TempPoint := Point(x, y);

  { If a form cannot be fully set to 0,0 }
  TempPoint.X := TempPoint.X - ScreenForm.Left;
  TempPoint.Y := TempPoint.Y - ScreenForm.Top;
  Color := WidgetSet.DCGetPixel(ImageHandle, X, Y);
  ImageInfo.Canvas.Brush.Color := Color;
  ImageInfo.Canvas.Rectangle(143, 4, 168, 29);
  SetBkColor(InfoHandle, 14811135);
  Text := 'Pos: ' + inttostr(x) + ','  + inttostr(y);
  R := Rect(5, 6, 114, 18);
  ExtTextOut(InfoHandle, 5, 3, ETO_OPAQUE, @R, pchar(text), length(text), nil);
  Text := 'Color: ' +  inttostr(Color);
  R := Rect(5, 18, 114, 28);
  ExtTextOut(InfoHandle, 5, 15, ETO_OPAQUE, @R, pchar(text), length(text), nil);
  for px := -1 to 1 do
    for py := -1 to 1 do
    begin
      ImageInfo.Canvas.Brush.Color := WidgetSet.DCGetPixel(ImageHandle, x + px, y + py);
      ImageInfo.Canvas.Rectangle((px + 1) * 8 + 115, (py + 1) * 8 + 4, (px + 1) * 8 + 124, (py + 1) * 8 + 13);
    end;
  Oldx := TempPoint.x;
  Oldy := TempPoint.y;
end;

procedure TMColorPicker.ImageInfoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  InfoForm.Top := Mouse.CursorPos.Y - 16;
  InfoForm.Left := Mouse.CursorPos.X + 5;
end;

procedure TMColorPicker.ColorPickDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin;
  Color:=  WidgetSet.DCGetPixel(ImageMain.Canvas.Handle,x,y);
  Self.Colorx := x;
  Self.Colory := y;
  if OnPick <> nil then
    Onpick(Sender,Color,Colorx,Colory);
  InfoForm.Close;
  ScreenForm.Close;
  closed := True;
end;

end.

