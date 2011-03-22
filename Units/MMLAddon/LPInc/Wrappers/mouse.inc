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

    mouse.inc for the Mufasa Macro Library
}

procedure lpMoveMouse(Params: PParamArray);
begin
  CurrThread.Client.IOManager.MoveMouse(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure lpScrollMouse(Params: PParamArray);
begin
  CurrThread.Client.IOManager.ScrollMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure lpGetMousePos(Params: PParamArray);
begin
   CurrThread.Client.IOManager.GetMousePos(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure lpHoldMouse(Params: PParamArray);
begin
   CurrThread.Client.IOManager.HoldMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, ConvIntClickType(PInt32(Params^[2])^));
end;

procedure lpReleaseMouse(Params: PParamArray);
begin
   CurrThread.Client.IOManager.ReleaseMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, ConvIntClickType(PInt32(Params^[2])^));
end;

procedure lpClickMouse(Params: PParamArray);
begin
   CurrThread.Client.IOManager.ClickMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, ConvIntClickType(PInt32(Params^[2])^));
end;

procedure lpIsMouseButtonDown(Params: PParamArray; Result: Pointer);
begin
  PBool(Result)^ := CurrThread.Client.IOManager.IsMouseButtonDown(ConvIntClickType(PInt32(Params^[0])^));
end;