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

    WindowSelector for the Mufasa Macro Library
}

unit windowselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ctypes,
  {$IFDEF MSWINDOWS} os_windows, {$ENDIF}
  {$IFDEF LINUX} os_linux, {$ENDIF}
  windowutil,
  controls,
  graphics,
  forms,
  {$IFNDEF MSWINDOWS}x, xlib,xatom
  {$ELSE}
  windows
  {$ENDIF}

  ;

type
    TMWindowSelector = class(TObject)
          constructor Create(manager: TIOManager);
          destructor Destroy; override;

          function Drag: TNativeWindow;


    public
          LastPick: TNativeWindow;
          manager: TIOManager;

    end;


implementation


constructor TMWindowSelector.Create(manager: TIOManager);
begin
  inherited create;

  self.manager := manager;

end;


destructor TMWindowSelector.Destroy;
begin

  inherited;
end;

{$IFDEF LINUX}
function TMWindowSelector.Drag: TNativeWindow;
var
  Tempwindow : x.TWindow;
  root : x.TWindow;
  subwindow : x.TWindow;
  x_root, y_root : cint;
  xmask : cuint;
  x, y : cint;
  Old_Handler : TXErrorHandler;

  window_opacity: TAtom;
  opacity_75: culong;
  opacity_100: culong;

begin
  Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

  Result := 0;

  window_opacity:=XInternAtom(manager.display,PChar('_NET_WM_WINDOW_OPACITY'), False);
  opacity_75 := cuint($ffffffff * 0.75);
  opacity_100 := cuint($ffffffff);

  repeat
    // get pointer pos + current window we are at.
    XQueryPointer(manager.display, manager.desktop, @root,
                @Tempwindow, @x_root, @y_root,
                @x, @y, @xmask);
    subwindow:= Tempwindow;

    while subwindow <> 0 do
    begin
        Tempwindow := subwindow;
        XQueryPointer(manager.display, Tempwindow, @root,
                 @subwindow, @x_root, @y_root,
                 @x, @y, @xmask);
    end;



    if Result <> Tempwindow then
    begin
      writeln('Making ' + inttostr(tempwindow) + ' transparent');
      XChangeProperty(manager.display, tempwindow, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_75, 1);

      writeln('Resetting ' + inttostr(Result));
      if result <> 0 then
        XChangeProperty(manager.display, Result, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_100, 1);
      WriteLn('Changing Window from: ' +  Inttostr(result) +' to: ' + IntToStr(Tempwindow));
     // XChangeProperty(Window.XDisplay, tempwindow, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_50, 1);

      Result := Tempwindow;
      LastPick:= TempWindow;
    end;
    XFlush(manager.display);
    Sleep(16);

    //if we are selecting for a long time, we must still process other messages
    Application.ProcessMessages;

  until (xmask and Button1Mask) = 0;

  XChangeProperty(manager.display, Result, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_100, 1);
  XFlush(manager.display);

  XSetErrorHandler(Old_handler);
end;

{$ELSE}

function TMWindowSelector.Drag: TNativeWindow;
var
  TargetRect: TRect;
  DC: HDC;
  OldPen, Pen: hPen;
  OldBrush : hBrush;
  BrushHandle : THandle;
  Cursor : TCursor;
  TempHandle : Hwnd;
  Handle : Hwnd;
begin;
  Pen := CreatePen(PS_SOLID, GetSystemMetrics(SM_CXBORDER)*5, clred);
  BrushHandle := GetStockObject(Null_Brush);
  Cursor:= Screen.Cursor;
  Screen.Cursor:= crCross;
  TempHandle := GetDesktopWindow;
  while GetAsyncKeyState(VK_LBUTTON) <> 0 do
  begin;
    Handle:= WindowFromPoint(Mouse.CursorPos);
    if Handle <> TempHandle then
    begin;
      if TempHandle <> 0 then
      begin;
        Invalidaterect(temphandle, nil, true);
        UpdateWindow(temphandle);
        {$IFDEF MSWINDOWS}
        RedrawWindow(TempHandle, nil, 0, RDW_Frame or RDW_Invalidate or RDW_Updatenow or RDW_Allchildren);
        {$ENDIF}
      end;
      if Handle <> 0 then
      begin;
        GetWindowRect(Handle, TargetRect);
        DC := Windows.GetWindowDC(Handle);
        OldPen := SelectObject(DC, Pen);
        OldBrush := SelectObject(DC, BrushHandle);
        Rectangle(DC, 0, 0, TargetRect.Right - TargetRect.Left, TargetRect.Bottom - TargetRect.Top);
        SelectObject(DC, OldBrush);
        SelectObject(DC, OldPen);
        ReleaseDC(Handle, DC);
      end;
      TempHandle  := Handle;
    end;
    Application.ProcessMessages;
    Sleep(64);
  end;
  Result := TempHandle;
  LastPick:= TempHandle;
  Screen.Cursor:= cursor;
  Invalidaterect(temphandle, nil, true);
  UpdateWindow(temphandle);
  RedrawWindow(TempHandle, nil, 0, RDW_Frame or RDW_Invalidate or RDW_Updatenow or RDW_Allchildren);
  DeleteObject(Pen);
end;
{$ENDIF}



end.

