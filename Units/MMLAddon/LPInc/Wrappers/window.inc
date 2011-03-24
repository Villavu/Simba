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

procedure lpSetDesktopAsClient(Params: PParamArray);
begin;
  CurrThread.Client.IOManager.SetDesktop;
end;

procedure lpSetTargetArray(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.IOManager.SetTarget(PRGB32(PLongInt(Params^[0])^), classes.point(PLongInt(Params^[1])^, PLongInt(Params^[2])^));
end;

procedure lpSetTargetBitmap(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.IOManager.SetTarget(CurrThread.Client.MBitmaps[PLongInt(Params^[0])^]);
end;

procedure lpSetEIOSTarget(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.IOManager.SetTarget(PlpString(Params^[0])^, @PVariant(Params^[1])^);
end;

procedure lpSetImageTarget(Params: PParamArray);
begin
  CurrThread.Client.IOManager.SetImageTarget(PLongInt(Params^[0])^);
end;

procedure lpSetKeyMouseTarget(Params: PParamArray);
begin
  CurrThread.Client.IOManager.SetKeyMouseTarget(PLongInt(Params^[0])^);
end;

procedure lpGetImageTarget(Params: PParamArray; Result: Pointer);
begin
  CurrThread.Client.IOManager.GetImageTarget(PLongInt(Result)^);
end;

procedure lpGetKeyMouseTarget(Params: PParamArray; Result: Pointer);
begin
  CurrThread.Client.IOManager.GetKeyMouseTarget(PLongInt(Result)^);
end;

procedure lpExportImageTarget(Params: PParamArray; Result: Pointer);
begin;
  PTarget_Exported(Result)^ := CurrThread.Client.IOManager.ExportImageTarget;
end;

procedure lpExportKeyMouseTarget(Params: PParamArray; Result: Pointer);
begin;
  PTarget_Exported(Result)^ := CurrThread.Client.IOManager.ExportKeyMouseTarget;
end;

procedure lpFreeTarget(Params: PParamArray);
begin
  CurrThread.Client.IOManager.FreeTarget(PLongInt(Params^[0])^);
end;

procedure lpGetClientDimensions(Params: PParamArray);
begin
  CurrThread.Client.IOManager.GetDimensions(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure lpGetClientPosition(Params: PParamArray);
begin
  CurrThread.Client.IOManager.GetPosition(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure lpFreeze(Params: PParamArray; Result: Pointer);
begin
  CurrThread.Client.IOManager.SetFrozen(True);
  PBoolean(Result)^ := True; //dunno what the result is supposed to mean
end;

procedure lpUnfreeze(Params: PParamArray; Result: Pointer);
begin
  CurrThread.Client.IOManager.SetFrozen(False);
  PBoolean(Result)^ := True; //dunno what the result is supposed to mean
end;

procedure lpActivateClient(Params: PParamArray);
begin
  CurrThread.Client.IOManager.ActivateClient;
end;

procedure lpIsTargetValid(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.IOManager.TargetValid;
end;
