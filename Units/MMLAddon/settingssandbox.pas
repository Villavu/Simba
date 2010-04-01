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

  { TMMLSettingsSandbox }

  TMMLSettingsSandbox = class(TObject)
  private
    ST: TMMLSettings;
    FPrefix: String;
    function GetPrefix: String;
    procedure SetPrefix(s: String);
  public
    constructor Create(sett: TMMLSettings);
    destructor Destroy; override;

    function IsKey(KeyName: String): Boolean;
    function IsDirectory(KeyName: String): Boolean;
    function SetKeyValue(Keyname : string; Value : string) : boolean;
    function GetKeyValue(KeyName: String): String;
    function GetKeyValueDef(KeyName, defVal: String): String;
    function ListKeys(KeyName: String; out Keys : TStringArray): boolean;
    function DeleteKey(KeyName: String): Boolean;
    function DeleteSubKeys(KeyName: String): Boolean;
    property prefix : string read GetPrefix write SetPrefix;
  end;

implementation

constructor TMMLSettingsSandbox.Create(sett: TMMLSettings);
begin
  inherited Create;
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
  result := FPrefix;
end;

procedure TMMLSettingsSandbox.SetPrefix(s: String);
begin
  FPrefix := s;
end;

function TMMLSettingsSandbox.ListKeys(KeyName: String; out Keys :TStringArray): boolean;
begin
  exit(ST.ListKeys(Prefix + KeyName,keys))
end;

function TMMLSettingsSandbox.GetKeyValue(KeyName: String): String;
begin
  exit(ST.GetKeyValue(Prefix + KeyName))
end;

function TMMLSettingsSandbox.GetKeyValueDef(KeyName, defVal: String): String;
begin
  exit(ST.GetKeyValueDef(Prefix + KeyName, defVal))
end;

function TMMLSettingsSandbox.IsKey(KeyName: String): Boolean;
begin
  exit(ST.IsKey(Prefix + KeyName))
end;

function TMMLSettingsSandbox.IsDirectory(KeyName: String): Boolean;
begin
  exit(ST.IsDirectory(Prefix + KeyName))
end;

function TMMLSettingsSandbox.SetKeyValue(Keyname: string; Value: string
  ): boolean;
begin
  Writeln(KeyName);
  Writeln(Value);
  exit(ST.SetKeyValue(prefix + keyname,value,true));
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

