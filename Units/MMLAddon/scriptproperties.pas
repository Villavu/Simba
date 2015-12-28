{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

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
      FOnTerminateProcsSkip : TBooleanArray;
      function HasTimeStamp: boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function GetProp(Prop : TSP_Property; var Value : TVariantArray) : Boolean;overload;
      function AddProp(Prop : TSP_Property; Value: Variant; skipIfUserTerminated: Boolean = false): Boolean;
      function DeleteOnTerminateProcs(Value: Variant): Boolean;
      procedure ClearOnTerminateProcs;
    public
      property Properties : TSP_Properties read FProperties;
      property WriteTimeStamp : boolean read HasTimeStamp;
      property OnTerminateProcs : TStringList read FOnTerminateProcs;
      property OnTerminateProcsSkip : TBooleanArray read FOnTerminateProcsSkip;
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
end;

destructor TScriptProperties.Destroy;
begin
  FOnTerminateProcs.Free;
  inherited Destroy;
end;

function TScriptProperties.GetProp(Prop: TSP_Property; var Value: TVariantArray): Boolean;
var
  i : integer;
begin
  Result := true;
  Setlength(value,0);
  case Prop of
    SP_WriteTimeStamp :
    begin
      SetLength(Value,1);
      Value[0] := BoolToStr(Prop in FProperties,true);
    end;
    SP_OnTerminate :
    begin
      if not (Prop in FProperties) then
        exit(false);
      setlength(value,FOnTerminateProcs.Count);
      for i := 0 to high(Value) do
        value[i] := FOnTerminateProcs[i];
    end;
  end;
end;

function TScriptProperties.AddProp(Prop: TSP_Property; Value: Variant; skipIfUserTerminated: Boolean = false): Boolean;
begin
  result := True;
  case Prop of
    SP_WriteTimeStamp :
      try
        if Value = True then
          FProperties := FProperties + [Prop]
        else
          FProperties := FProperties - [Prop];
      except
        mDebugLn('Could not convert your value passed to SetProp');
        Exit(False);
      end;

    SP_OnTerminate :
    begin
      if FOnTerminateProcs.IndexOf(Value) > -1 then
        Exit;
      FOnTerminateProcs.Add(Value);
      SetLength(FOnTerminateProcsSkip, FOnTerminateProcs.Count);
      FOnTerminateProcsSkip[FOnTerminateProcs.Count - 1] := skipIfUserTerminated;
      FProperties := FProperties + [prop];
    end;
  end;
end;

function TScriptProperties.DeleteOnTerminateProcs(Value: Variant): Boolean;
var
  index, i: Integer;
begin
  Result := True;
  index := FOnTerminateProcs.IndexOf(value);
  if index = -1 then
    Exit(False);
  FOnTerminateProcs.Delete(index);

  for i := index to FOnTerminateProcs.Count - 1 do
    FOnTerminateProcsSkip[i + 1] := FOnTerminateProcsSkip[i];
  SetLength(FOnTerminateProcsSkip, FOnTerminateProcs.Count);
end;

procedure TScriptProperties.ClearOnTerminateProcs;
begin
  FOnTerminateProcs.Clear;
  SetLength(FOnTerminateProcsSkip, 0);
end;

end.

