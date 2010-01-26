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

    window.inc for the Mufasa Macro Library
}

procedure SetDesktopAsClient;
begin;
  CurrThread.Client.IOManager.SetDesktop;
end;

function SetTargetArray(P: Integer; w, h: integer): integer;
begin
  result:= CurrThread.Client.IOManager.SetTarget(PRGB32(P), classes.point(w,h));
end;

function SetTargetBitmap(bitmap: Integer): integer;
begin;
  result:= CurrThread.Client.IOManager.SetTarget(CurrThread.Client.MBitmaps[Bitmap]);
end;

function SetEIOSTarget(name: string; args: Variant): integer;
begin
  result:= CurrThread.Client.IOManager.SetTarget(name, @args);
end;

procedure SetImageTarget(idx: integer);
begin
  CurrThread.Client.IOManager.SetImageTarget(idx);
end;

procedure SetKeyMouseTarget(idx: integer);
begin
  CurrThread.Client.IOManager.SetKeyMouseTarget(idx);
end;

function GetImageTarget: integer;
begin
  CurrThread.Client.IOManager.GetImageTarget(result);
end;

function GetKeyMouseTarget: integer;
begin
  CurrThread.Client.IOManager.GetKeyMouseTarget(result);
end;

procedure FreeTarget(idx: integer);
begin
  CurrThread.Client.IOManager.FreeTarget(idx);
end;

procedure GetClientDimensions(out w, h: integer);
begin
  CurrThread.Client.IOManager.GetDimensions(w, h);
end;

function Freeze: boolean;
begin
  CurrThread.Client.IOManager.SetFrozen(true);
  result := true; //dunno what the result is supposed to mean
end;

function Unfreeze: boolean;
begin
  CurrThread.Client.IOManager.SetFrozen(false);
  result := true; //dunno what the result is supposed to mean
end;

procedure ActivateClient;
begin
  CurrThread.Client.IOManager.ActivateClient;
end;

function IsTargetValid: boolean;
begin
  result:= CurrThread.Client.IOManager.TargetValid;
end;
