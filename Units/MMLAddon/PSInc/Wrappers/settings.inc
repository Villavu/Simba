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

    Settings.inc for the Mufasa Macro Library
}

function SettingsIsKey(KeyName: String): Boolean;
begin
  if CurrThread.Sett <> nil then
    result:= CurrThread.Sett.IsKey(KeyName)
  else
    result:=false;
end;

function SettingsIsDirectory(KeyName: String): Boolean;
begin
  if CurrThread.Sett <> nil then
    result:= CurrThread.Sett.IsDirectory(KeyName)
  else
    result := false;
end;

function SettingsGetKeyValue(KeyName: String): String;
begin
  if CurrThread.Sett <> nil then
    result:= CurrThread.Sett.GetKeyValue(KeyName)
  else
    result:='';
end;

function SettingsGetSetDefaultKeyValue(KeyName, defVal: String): String;
begin
  if CurrThread.Sett <> nil then
    result:= CurrThread.Sett.GetSetDefaultKeyValue(KeyName, defVal)
  else
    result:='';
end;

procedure SettingsListKeys(KeyName: String; var KeyReturn: TStringArray);
begin
  if CurrThread.Sett <> nil then
    KeyReturn := CurrThread.Sett.ListKeys(KeyName)
  else
    setlength(KeyReturn, 0);
end;

function SettingsDeleteKey(KeyName: String): Boolean;
begin
  if CurrThread.Sett <> nil then
    Result := CurrThread.Sett.DeleteKey(KeyName)
  else
    result := False;
end;

function SettingsDeleteSubKeys(KeyName: String): Boolean;
begin
    if CurrThread.Sett <> nil then
    Result := CurrThread.Sett.DeleteSubKeys(KeyName)
  else
    result := False;
end;
