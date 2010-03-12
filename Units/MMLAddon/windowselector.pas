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
  {$IFDEF MSWINDOWS} os_windows, {$ENDIF}
  {$IFDEF LINUX} os_linux,  ctypes, {$ENDIF}
  controls,
  graphics,
  forms,
  extctrls,
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
          haspicked: boolean;
          manager: TIOManager;

    end;


implementation


constructor TMWindowSelector.Create(manager: TIOManager);
begin
  inherited create;
  haspicked:= false;
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
  //Old_Handler : TXErrorHandler;

  window_opacity: TAtom;
  opacity_75: culong;
  opacity_100: culong;

begin
  //Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);

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
      mDebugLn('Making ' + inttostr(tempwindow) + ' transparent');
      XChangeProperty(manager.display, tempwindow, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_75, 1);

      mDebugLn('Resetting ' + inttostr(Result));
      if result <> 0 then
        XChangeProperty(manager.display, Result, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_100, 1);
      mDebugLn('Changing Window from: ' +  Inttostr(result) +' to: ' + IntToStr(Tempwindow));
     // XChangeProperty(Window.XDisplay, tempwindow, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_50, 1);

      Result := Tempwindow;
      LastPick:= TempWindow;
      haspicked:= true;
    end;
    XFlush(manager.display);
    Sleep(16);

    //if we are selecting for a long time, we must still process other messages
    Application.ProcessMessages;

  until (xmask and Button1Mask) = 0;

  XChangeProperty(manager.display, Result, window_opacity, XA_CARDINAL, 32, PropModeReplace, @opacity_100, 1);
  XFlush(manager.display);

  //XSetErrorHandler(Old_handler);
end;

{$ELSE}

function TMWindowSelector.Drag: TNativeWindow;
var
  TargetRect: TRect;
  Region : HRGN;
  Cursor : TCursor;
  TempHandle : Hwnd;
  Handle : Hwnd;
  DragForm : TForm;
  EdgeForm : TForm;
  Style : DWord;
  W,H: integer;
const
  EdgeSize =4;
  WindowCol = clred;
begin;
  Cursor:= Screen.Cursor;
  Screen.Cursor:= crCross;
  TempHandle := GetDesktopWindow;
  EdgeForm := TForm.Create(nil);
  EdgeForm.Color:= WindowCol;
  EdgeForm.BorderStyle:= bsNone;
  EdgeForm.Show;

  DragForm := TForm.Create(nil);
  DragForm.Color:= WindowCol;
  DragForm.BorderStyle:= bsNone;
  DragForm.Show;

  Style := GetWindowLong(DragForm.Handle, GWL_EXSTYLE);
  SetWindowLong(DragForm.Handle, GWL_EXSTYLE, Style or WS_EX_LAYERED or WS_EX_TRANSPARENT);
  SetLayeredWindowAttributes(DragForm.Handle, 0, 100, LWA_ALPHA);

  while GetAsyncKeyState(VK_LBUTTON) <> 0 do
  begin;
    Handle:= WindowFromPoint(Mouse.CursorPos);
    if (Handle <> TempHandle) and (Handle <> EdgeForm.Handle) then
    begin;
      GetWindowRect(Handle, TargetRect);
      W :=TargetRect.Right - TargetRect.Left+1;
      H :=TargetRect.Bottom - TargetRect.Top+1;
      DragForm.SetBounds(TargetRect.Left,TargetRect.top,W,H);//Draw the transparent form

      SetWindowRgn(EdgeForm.Handle,0,false);//Delete the old region
      Region := CreateRectRgn(0,0,w-1,h-1); //Create a full region, of the whole form
      CombineRgn(Region,Region,CreateRectRgn(EdgeSize,EdgeSize,w-1-(edgesize),h-1-(edgesize)),RGN_XOR); //Combine a the 2 regions (of the full form and one without the edges)
      SetWindowRgn(edgeform.Handle,Region,true);//Set the only-edge-region!
      EdgeForm.SetBounds(TargetRect.Left,TargetRect.top,W,H);//Move the form etc..
      TempHandle  := Handle;
    end;
    Application.ProcessMessages;
    Sleep(30);
  end;
  Result := TempHandle;
  LastPick:= TempHandle;
  haspicked:= true;
  Screen.Cursor:= cursor;
  DragForm.Hide;
  DragForm.Free;
  EdgeForm.Hide;
  EdgeForm.Free;
end;
{$ENDIF}



end.

