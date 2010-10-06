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

procedure ps_SetDesktopAsClient; extdecl;
begin;
  CurrThread.Client.IOManager.SetDesktop;
end;

function ps_SetTargetArray(P: Integer; w, h: integer): integer; extdecl;
begin
  result:= CurrThread.Client.IOManager.SetTarget(PRGB32(P), classes.point(w,h));
end;

function ps_SetTargetBitmap(bitmap: Integer): integer; extdecl;
begin;
  result:= CurrThread.Client.IOManager.SetTarget(CurrThread.Client.MBitmaps[Bitmap]);
end;

function ps_SetEIOSTarget(name: string; args: Variant): integer; extdecl;
begin
  result:= CurrThread.Client.IOManager.SetTarget(name, @args);
end;

procedure ps_SetImageTarget(idx: integer); extdecl;
begin
  CurrThread.Client.IOManager.SetImageTarget(idx);
end;

procedure ps_SetKeyMouseTarget(idx: integer); extdecl;
begin
  CurrThread.Client.IOManager.SetKeyMouseTarget(idx);
end;

function ps_GetImageTarget: integer; extdecl;
begin
  CurrThread.Client.IOManager.GetImageTarget(result);
end;

function ps_GetKeyMouseTarget: integer; extdecl;
begin
  CurrThread.Client.IOManager.GetKeyMouseTarget(result);
end;

function ps_ExportImageTarget : TTarget_Exported;extdecl;
begin;
  result := CurrThread.Client.IOManager.ExportImageTarget;
end;

function ps_ExportKeyMouseTarget : TTarget_Exported; extdecl;
begin;
  result := CurrThread.Client.IOManager.ExportKeyMouseTarget;
end;

procedure ps_FreeTarget(idx: integer); extdecl;
begin
  CurrThread.Client.IOManager.FreeTarget(idx);
end;

procedure ps_GetClientDimensions(var w, h: integer); extdecl;
begin
  CurrThread.Client.IOManager.GetDimensions(w, h);
end;

procedure ps_GetClientPosition(var left, top: integer); extdecl;
begin
  CurrThread.Client.IOManager.GetPosition(left, top);
end;

function ps_Freeze: boolean; extdecl;
begin
  CurrThread.Client.IOManager.SetFrozen(true);
  result := true; //dunno what the result is supposed to mean
end;

function ps_Unfreeze: boolean; extdecl;
begin
  CurrThread.Client.IOManager.SetFrozen(false);
  result := true; //dunno what the result is supposed to mean
end;

procedure ps_ActivateClient; extdecl;
begin
  CurrThread.Client.IOManager.ActivateClient;
end;

function ps_IsTargetValid: boolean; extdecl;
begin
  result:= CurrThread.Client.IOManager.TargetValid;
end;
