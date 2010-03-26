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

    Settings Sandbox class for Mufasa Macro Library
}
unit settingssandbox;

{$mode objfpc}

interface

uses
  Classes, SysUtils, settings, strutils, mufasatypes;

type
  TMMLSettingsSandbox = class(TObject)
      public
            constructor Create(sett: TMMLSettings);
            destructor Destroy; override;

            function IsKey(KeyName: String): Boolean;
            function IsDirectory(KeyName: String): Boolean;
            function GetKeyValue(KeyName: String): String;
            function GetSetDefaultKeyValue(KeyName, defVal: String): String;
            function ListKeys(KeyName: String): TStringArray;

            function DeleteKey(KeyName: String): Boolean;
            function DeleteSubKeys(KeyName: String): Boolean;
      public
            function GetPrefix: String;
            procedure SetPrefix(s: String);
      private
            ST: TMMLSettings;
            Prefix: String;

  end;

implementation

constructor TMMLSettingsSandbox.Create(sett: TMMLSettings);
begin
  inherited;
  Self.ST := sett;
end;

destructor TMMLSettingsSandbox.Destroy;

begin
  { Don't free the settings object. ;-) }
  Self.ST := nil;
  inherited;
end;

function TMMLSettingsSandbox.GetPrefix: String;
begin
  result := Prefix;
end;

procedure TMMLSettingsSandbox.SetPrefix(s: String);
begin
  Prefix := s;
end;

function TMMLSettingsSandbox.ListKeys(KeyName: String): TStringArray;
begin
  exit(ST.ListKeys(Prefix + KeyName))
end;

function TMMLSettingsSandbox.GetKeyValue(KeyName: String): String;
begin
  exit(ST.GetKeyValue(Prefix + KeyName))
end;

function TMMLSettingsSandbox.GetSetDefaultKeyValue(KeyName, defVal: String): String;
begin
  exit(ST.GetSetDefaultKeyValue(Prefix + KeyName, defVal))
end;

function TMMLSettingsSandbox.IsKey(KeyName: String): Boolean;
begin
  exit(ST.IsKey(Prefix + KeyName))
end;

function TMMLSettingsSandbox.IsDirectory(KeyName: String): Boolean;
begin
  exit(ST.IsDirectory(Prefix + KeyName))
end;

function TMMLSettingsSandbox.DeleteKey(KeyName: String): Boolean;
begin
  exit(ST.DeleteKey(Prefix + KeyName));
end;

function TMMLSettingsSandbox.DeleteSubKeys(KeyName: String): Boolean;
begin
  exit(ST.DeleteSubKeys(Prefix + KeyName));
end;

end.

