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

    settings.inc for the Mufasa Macro Library
}

procedure lpSetSettingValue(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PBool(Result)^ := CurrThread.Sett.SetKeyValue(PlpString(Params^[0])^, PlpString(Params^[1])^)
  else
    PBool(Result)^ := False;
end;

procedure lpGetSettingValue(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PString(Result)^ := CurrThread.Sett.GetKeyValue(PlpString(Params^[0])^)
  else
    PString(Result)^ :='';
end;

procedure lpGetSettingValueDef(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PString(Result)^ := CurrThread.Sett.GetKeyValueDef(PlpString(Params^[0])^, PlpString(Params^[1])^)
  else
    PString(Result)^ :='';
end;

procedure lpListSettings(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PBool(Result)^ := CurrThread.Sett.ListKeys(PlpString(Params^[0])^, PStringArray(Params^[1])^)
  else
    PBool(Result)^ := False;
end;

procedure lpKeyIsSetting(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PBool(Result)^ := CurrThread.Sett.IsKey(PlpString(Params^[0])^)
  else
    PBool(Result)^ := False;
end;

procedure lpKeyIsDirectory(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PBool(Result)^ := CurrThread.Sett.IsDirectory(PlpString(Params^[0])^)
  else
    PBool(Result)^ := False;
end;

procedure lpDeleteSetting(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PBool(Result)^ := CurrThread.Sett.DeleteKey(PlpString(Params^[0])^)
  else
    PBool(Result)^ := False;
end;

procedure lpDeleteSubSettings(Params: PParamArray; Result: Pointer);
begin
  if (CurrThread.Sett <> nil) then
    PBool(Result)^ := CurrThread.Sett.DeleteSubKeys(PlpString(Params^[0])^)
  else
    PBool(Result)^ := False;
end;
