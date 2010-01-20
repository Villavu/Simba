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

    Mouse.inc for the Mufasa Macro Library
}

const
  ps_mouse_right = 0;
  ps_mouse_left = 1;
  ps_mouse_middle = 2;
procedure MoveMouse(x, y: integer);
begin
  CurrThread.Client.IOManager.SetMousePos(X, Y);
end;

procedure GetMousePos(out x, y: integer);
begin
   CurrThread.Client.IOManager.GetMousePos(X, Y);
end;
function ConvIntClickType(Int : Integer) : TClickType;inline;
begin;
  case int of
    ps_mouse_right : result := mouse_Right;
    ps_mouse_left : result := mouse_left;
    ps_mouse_middle: result := mouse_middle;
  else
    raise exception.CreateFMT('Unknown Clicktype (%d) passed.',[int]);
  end;
end;

procedure HoldMouse(x, y: integer; clickType: integer);
begin
   CurrThread.Client.IOManager.SetMousePos(x, y);
   CurrThread.Client.IOManager.MouseButtonAction(x, y, ConvIntClickType(clickType), mouse_Down);
end;

procedure ReleaseMouse(x, y: integer; clickType: integer);
begin
   CurrThread.Client.IOManager.SetMousePos(x, y);
   CurrThread.Client.IOManager.MouseButtonAction(x, y, ConvIntClickType(clickType), mouse_Up);
end;

procedure ClickMouse(x, y: integer; clickType: integer);
begin
   CurrThread.Client.IOManager.ClickMouse(x, y, ConvIntClickType(clickType));
end;
