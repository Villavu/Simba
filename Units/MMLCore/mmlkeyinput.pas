unit MMLKeyInput;

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

    MMLKeyInput class for Keyboard input in MML.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XKeyInput;

type
  TMMLKeyInput = class(TXKeyInput)
    public
       { Override these two methods,
         as the original class calls ProcessMessages;
       }
       procedure Down(Key: Word);
       procedure Up(Key: Word);
end;


implementation
uses LCLType;

procedure TMMLKeyInput.Down(Key: Word);
begin
  DoDown(Key);
end;

procedure TMMLKeyInput.Up(Key: Word);
begin
  DoUp(Key);
end;

end.

