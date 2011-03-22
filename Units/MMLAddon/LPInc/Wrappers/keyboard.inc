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

    keyboard.inc for the Mufasa Macro Library
}

procedure lpKeyDown(Params: PParamArray);
begin
  CurrThread.Client.IOManager.KeyDown(PWord(Params^[0])^);
end;

procedure lpKeyUp(Params: PParamArray);
begin
  CurrThread.Client.IOManager.KeyUp(PWord(Params^[0])^);
end;

procedure lpPressKey(Params: PParamArray);
begin
  CurrThread.Client.IOManager.PressKey(PWord(Params^[0])^);
end;

procedure lpSendKeys(Params: PParamArray);
begin
  CurrThread.Client.IOManager.SendText(PString(Params^[0])^);
end;

procedure lpisKeyDown(Params: PParamArray; Result: Pointer);
begin
  PBool(Result)^ := CurrThread.Client.IOManager.isKeyDown(PWord(Params^[0])^);
end;

procedure lpGetKeyCode(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := CurrThread.Client.IOManager.GetKeyCode(PChar(Params^[0])^);
end;
