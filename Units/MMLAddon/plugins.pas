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

    Plugins Class for the Mufasa Macro Library
}

unit plugins;

{
   Problems with SMART; you cannot free the plugin when smart is open..
   Therefore, loading & free-ing plugins per script run is not an option.
   Assigning a TMPlugin per Tab might be a do-able solution, but will still cope with the SMART Problems..
   So the question is: Plugins Per Tab,Per Run or Global?
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, libloader;

const
  cv_StdCall = 0; //StdCall
  cv_Register = 1; //Register
  
type
  TPasScriptType = record
    TypeName, TypeDef: string;
  end;
  
  TMPluginMethod = record
    FuncPtr: pointer;
    FuncStr: string;
    FuncConv: integer;
  end;

  TMPlugin = record
    Methods: array of TMPluginMethod;
    MethodLen: integer;
    Types: array of TPasScriptType;
    TypesLen: integer;
  end;
  TMPluginArray = array of TMPlugin;



  { TMPlugins }

  TMPlugins = class (TGenericLoader)
    private
      Plugins: TMPluginArray;
      NumPlugins: integer;
    protected
      function InitPlugin(plugin: TLibHandle): boolean; override;
    public
      property MPlugins: TMPluginArray read Plugins;
      property Count: integer read NumPlugins;
  end;

implementation

uses
  MufasaTypes,FileUtil;

{ TMPlugins }

function TMPlugins.InitPlugin(Plugin: TLibHandle): boolean;
var
  GetFuncCount: function: integer; stdcall;
  GetFuncInfo: function(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): integer; stdcall;
  GetFuncConv: function(x: integer): integer; stdcall;
  GetTypeCount: function: integer; stdcall;
  GetTypeInfo: function(x: Integer; var sType, sTypeDef: string): integer; stdcall;
  PD: PChar;
  pntr: Pointer;
  ArrC, I: integer;
begin
  Pointer(GetFuncCount) := GetProcAddress(Plugin, PChar('GetFunctionCount'));
  if (GetFuncCount = nil) then
  begin
    Result := False;
    Exit;
  end;
  
  Pointer(GetFuncInfo) := GetProcAddress(Plugin, PChar('GetFunctionInfo'));
  if (GetFuncInfo = nil) then
  begin
    Result := False;
    Exit;
  end;
  
  Pointer(GetFuncConv) := GetProcAddress(Plugin, PChar('GetFunctionCallingConv'));
  
  SetLength(Plugins, NumPlugins + 1);
  
  Pointer(GetTypeCount) := GetProcAddress(Plugin, PChar('GetTypeCount'));
  if (not (GetTypeCount = nil)) then
  begin
    Pointer(GetTypeInfo) := GetProcAddress(Plugin, PChar('GetTypeInfo'));
    if (not (GetTypeInfo = nil)) then
    begin
      ArrC := GetTypeCount();
      Plugins[NumPlugins].TypesLen := ArrC;
      SetLength(Plugins[NumPlugins].Types, ArrC);
      for I := 0 to ArrC - 1 do
      begin
        if (GetTypeInfo(I, Plugins[NumPlugins].Types[I].TypeName, Plugins[NumPlugins].Types[I].TypeDef) < 0) then
        begin
          Plugins[NumPlugins].Types[I].TypeName := '';
          Plugins[NumPlugins].Types[I].TypeDef := '';
        end;
      end;
    end;
  end;

  ArrC := GetFuncCount();
  Plugins[NumPlugins].MethodLen := ArrC;
  SetLength(Plugins[NumPlugins].Methods, ArrC);
  
  PD := StrAlloc(255);
  for I := 0 to ArrC - 1 do
  begin;
    if (GetFuncInfo(I, pntr, PD) < 0) then
      Continue;
    Plugins[NumPlugins].Methods[I].FuncPtr := pntr;
    Plugins[NumPlugins].Methods[I].FuncStr := PD;

    if (GetFuncConv <> nil) then
      Plugins[NumPlugins].Methods[I].FuncConv := GetFuncConv(I)
    else
      Plugins[NumPlugins].Methods[I].FuncConv := cv_stdcall;
  end;
  StrDispose(PD);  
  Inc(NumPlugins);
  Result := True;
end;

end.

