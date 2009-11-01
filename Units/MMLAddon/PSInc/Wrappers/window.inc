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
  CurrThread.Client.MWindow.SetDesktop;
end;

procedure SetTargetBitmap(bitmap: Integer);
begin;
  With CurrThread.Client do
    MWindow.SetTarget(MBitmaps.Bmp[Bitmap]);
end;

procedure GetClientDimensions(var w, h: integer);
begin
  CurrThread.Client.MWindow.GetDimensions(w, h);
end;

function Freeze: boolean;
begin
  result := CurrThread.Client.MWindow.Freeze();
end;

function Unfreeze: boolean;
begin
  result := CurrThread.Client.MWindow.Unfreeze;
end;

procedure SetTargetArray(P: Integer; Size: TPoint);
begin
  CurrThread.Client.MWindow.SetTarget(PRGB32(P), Size);
end;
