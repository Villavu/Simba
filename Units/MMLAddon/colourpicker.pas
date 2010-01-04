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
  Window,MufasaTypes, colourhistory,bitmaps,input

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
       Procedure ColorPickUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  public
        // Will give us CopyClientToBitmap
        Window: TMWindow;

        // Created and freed in Pick.
        Input: TMInput;

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
   SS : TShiftState;
   p : TPoint;

   bmp: TMufasaBitmap;
   Desktop : TMWindow;


begin
  { Disable both of the color pick buttons }
  w := 0;
  h := 0;
  if not Self.Window.TargetValid then
    self.Window.SetDesktop;

  Desktop := TMWindow.Create;
  Desktop.SetDesktop;
  Input := TMInput.Create(Self.Window);
  Desktop.GetDimensions(w, h);

  Application.MainForm.Enabled := False;
  ColourHistoryForm.Enabled := False;

  { Create a form that will hold the client image and a form that will show cursor and color data }
  ScreenForm := TForm.Create(Application.MainForm);
  InfoForm := TForm.Create(ScreenForm);




  { Initialize the form that will hold the client image }
  ScreenForm.Caption := 'SimbaColourPicker';
  { Set the form's dimensions to match that of the screen }
  ScreenForm.Width := w;
  ScreenForm.Height := h;
  ScreenForm.Top := 0;
  ScreenForm.left := 0;
  ScreenForm.WindowState := wsmaximized;
  ScreenForm.BorderStyle:= bsNone;
  ScreenForm.FormStyle := fsStayOnTop;

  { Initialize the form that will hold the cursor and color info }
  InfoForm.Width := 173;
  InfoForm.Height := 33;
  InfoForm.BorderStyle := bsNone;
  InfoForm.FormStyle := fsStayOnTop;
  InfoForm.Left := Mouse.CursorPos.X + 5;
  InfoForm.Top := Mouse.CursorPos.Y - 15;

  { Initialize the image that will hold the cursor and color info }
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

  { Initialize the image that will hold the client image }
  ImageMain := TImage.Create(ScreenForm);
  ImageMain.Parent := ScreenForm;
  ImageMain.left := 0;
  ImageMain.top := 0;
  ImageMain.width := ScreenForm.Width;
  ImageMain.Height := ScreenForm.Height;
  ImageMain.Cursor:= crCross;
  ImageMain.OnMouseUp:= @ColorPickUp;
  ImageMain.OnMouseMove:=@ImageMainMouseMove;

  { Copy the client to ImageMain }
  bmp:=TMufasaBitmap.Create;
  bmp.CopyClientToBitmap(Desktop, true, 0, 0, w-1, h-1);
  ImageMain.Picture.Bitmap.Free;
  ImageMain.Picture.Bitmap := bmp.ToTBitmap;
  bmp.Free;

  { Set up handles and events }
  ImageHandle:= ImageMain.Canvas.Handle;
  InfoHandle:= ImageInfo.Canvas.Handle;
  TheChangedEvent := ImageMain.Canvas.OnChange;
  TheChangingEvent := ImageMain.Canvas.OnChanging;
  { Show the forms }
  ScreenForm.Show;
  InfoForm.Show;

  { Display the data on the info form }
  p := ImageMain.ScreenToClient(Mouse.CursorPos);
  ImageMainMouseMove(nil, SS, p.x, p.y);

  closed := False;

  { Wait while the forms are still open }
  while not Closed do
  begin
    sleep(1);
    Application.ProcessMessages;
  end;

  // add x to history here.
  c := Color;
  x := Colorx;
  y := Colory;

  { Free forms and images }
  ImageMain.Free;
  ImageInfo.Free;
  InfoForm.Free;
  ScreenForm.Free;

  Input.Free;
  Desktop.free;

  { Re-enable the color pick buttons }
  Application.MainForm.Enabled := True;
  ColourHistoryForm.Enabled := True;
end;

procedure TMColorPicker.ImageMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempPoint : TPoint;
  Data : TRetData;
  R : TRect;
  px, py : Integer;
  MouseX, MouseY: Integer;
begin
  { Move the info form }
  Input.GetMousePos(MouseX, MouseY);
  InfoForm.Left := Mouse.CursorPos.X + 5;
  InfoForm.Top := Mouse.CursorPos.Y - 15;

  TempPoint := Point(x, y);

  { If a form cannot be fully set to 0,0 }
  TempPoint.X := TempPoint.X - ScreenForm.Left;
  TempPoint.Y := TempPoint.Y - ScreenForm.Top;

  { Get the pixel that the cursor is currently on }
  Color := WidgetSet.DCGetPixel(ImageHandle, X, Y);

  { Draw the current pixel to the right color box }
  ImageInfo.Canvas.Brush.Color := Color;
  ImageInfo.Canvas.Rectangle(143, 4, 168, 29);

  { Draw the cursor and color info }
  SetBkColor(InfoHandle, 14811135);
  Text := Format('Pos: %d, %d', [MouseX, MouseY]);
  R := Rect(5, 6, 114, 18);
  ExtTextOut(InfoHandle, 5, 3, ETO_OPAQUE, @R, pchar(text), length(text), nil);
  Text := Format('Color: %d', [Color]);
  R := Rect(5, 18, 114, 28);
  ExtTextOut(InfoHandle, 5, 15, ETO_OPAQUE, @R, pchar(text), length(text), nil);

  { Draw the left, slightly zoomed out, color box }
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
  { Move the info form }
  InfoForm.Top := Mouse.CursorPos.Y - 15;
  InfoForm.Left := Mouse.CursorPos.X + 5;
end;

procedure TMColorPicker.ColorPickUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin;
  { Set the coordinates and color that the user cliked on }
  Color:=  WidgetSet.DCGetPixel(ImageMain.Canvas.Handle,x,y);
  Self.Colorx := x;
  Self.Colory := y;

  if OnPick <> nil then
    Onpick(Sender,Color,Colorx,Colory);

  { Close the forms }
  InfoForm.Close;
  ScreenForm.Close;

  { Tell Pick() that we are done }
  closed := True;
end;

end.

