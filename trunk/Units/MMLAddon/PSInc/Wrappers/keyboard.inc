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

    Keyboard.inc for the Mufasa Macro Library
}

procedure KeyDown(key: Word); extdecl;
begin
  CurrThread.Client.IOManager.KeyDown(key);
end;

procedure KeyUp(key: Word); extdecl;
begin
  CurrThread.Client.IOManager.KeyUp(key);
end;

procedure SendKeys(s: string); extdecl;
begin
  CurrThread.Client.IOManager.SendText(s);
end;

procedure PressKey(key: Word); extdecl;
begin
  CurrThread.Client.IOManager.PressKey(key);
end;

function isKeyDown(key: Word): boolean; extdecl;
begin
  Result := CurrThread.Client.IOManager.isKeyDown(key);
end;

function GetKeyCode(c :char) : integer; extdecl;
begin
  result := CurrThread.Client.IOManager.GetKeyCode(c);
end;
