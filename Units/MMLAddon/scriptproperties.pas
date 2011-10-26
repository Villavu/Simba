{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

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

    ScriptProperties for the Mufasa Macro Library
}
unit scriptproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,MufasaTypes,mufasabase;
type

    { TScriptProperties }

    TSP_Property = (
      SP_WriteTimeStamp, //Writes the time infront of a writeln
      SP_OnTerminate
    );
    PSP_Property = ^TSP_Property;
    TSP_Properties = set of TSP_Property;


    TScriptProperties = class(TObject)
    private
      FProperties : TSP_Properties;
      FOnTerminateProcs : TStringList;
      function HasTimeStamp: boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function GetProperty(name: ansistring; var Prop : TSP_Property): boolean;
      function GetProp(name: ansistring; var Value : TVariantArray) : Boolean;overload;
      function GetProp(Prop : TSP_Property; var Value : TVariantArray) : Boolean;overload;
      function SetProp(Name: ansistring; Value: TVariantArray): Boolean;overload;
      function SetProp(Prop : TSP_Property; Value: TVariantArray): Boolean;overload;
    public
      property Properties : TSP_Properties read FProperties;
      property WriteTimeStamp : boolean read HasTimeStamp;
      property OnTerminateProcs : TStringList read FOnTerminateProcs;
    end;


implementation

function TScriptProperties.HasTimeStamp: boolean;
begin
  result := (SP_WriteTimeStamp in FProperties);
end;

constructor TScriptProperties.Create;
begin
  inherited;

  FProperties := [];
  FOnTerminateProcs := TStringList.Create;
  FOnTerminateProcs.CaseSensitive:= false;
  FOnTerminateProcs.Duplicates:= dupIgnore;
  { set default values }
end;

destructor TScriptProperties.Destroy;
begin
  FOnTerminateProcs.Free;
  inherited Destroy;
end;

function TScriptProperties.GetProperty(name: ansistring; var Prop : TSP_Property): boolean;
const
  Names : array[TSP_Property] of ansistring = ('writetimestamp','onterminate');
var
  i : integer;
begin
  Result := false;
  for i := 0 to length(names)- 1 do
    if lowercase(name) = Names[TSP_Property(i)] then
    begin
      Prop := (TSP_Property(i));
      Exit(true);
    end;
end;

function TScriptProperties.GetProp(name: ansistring; var Value: TVariantArray): Boolean;
var
  Prop : TSP_Property;
begin
  Result := false;
  if GetProperty(name,prop) then
    Result := (GetProp(Prop,value));
end;

function TScriptProperties.GetProp(Prop: TSP_Property; var Value: TVariantArray
  ): Boolean;
var
  i : integer;
begin
  Result := true;
  Setlength(value,0);
  case Prop of
    SP_WriteTimeStamp : begin SetLength(Value,1); Value[0] := BoolToStr(Prop in FProperties,true); end;
    SP_OnTerminate :
      begin
        if not (Prop in FProperties) then
          exit;
        setlength(value,FOnTerminateProcs.Count);
        for i := 0 to high(Value) do
          value[i] := FOnTerminateProcs[i];
        result := true;
      end;
  end;
end;

function TScriptProperties.SetProp(Name: ansistring; Value: TVariantArray): Boolean;
var
  Prop : TSP_Property;
begin
  Result := false;
  if GetProperty(name,prop) then
    Result := (SetProp(Prop,value));
end;

function TScriptProperties.SetProp(Prop: TSP_Property; Value: TVariantArray): Boolean;
var
  i : integer;
begin
  result := false;
  if Length(value) < 1 then
  begin;
    mDebugLn('SetProp passed a TVarArray with a length of 0' );
    exit;
  end;
  case Prop of
    SP_WriteTimeStamp : begin
                          if length(Value) <> 1 then
                          begin
                            mDebugLn('SP_WriteTimeStamp only needs 1 value in the array');
                            exit;
                          end;
                          try
                            if Value[0] = True then
                              FProperties := FProperties + [Prop]
                            else
                              FProperties := FProperties - [Prop];
                          except
                            mDebugLn('Could not convert your value passed to SetProp');
                          end;
                       end;
    SP_OnTerminate :
      begin
        FOnTerminateProcs.Clear;
        for i := 0 to high(value) do
          FOnTerminateProcs.Add(Value[i]);
        FProperties := FProperties + [prop];
        Result := True;
      end;
  end;
end;

end.

