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

type
  TMPluginMethod = record
    FuncPtr : pointer;
    FuncStr : string;
  end;

  TMPlugin = record
    Methods : Array of TMPluginMethod;
    MethodLen : integer;
  end;
  TMPluginArray = array of TMPlugin;



  { TMPlugins }

  TMPlugins = class (TGenericLoader)
    private
      Plugins : TMPluginArray;
      NumPlugins : integer;
    protected
      function InitPlugin(plugin: TLibHandle): boolean; override;
    public
      property MPlugins : TMPluginArray read Plugins;
      property Count : integer read NumPlugins;
  end;

implementation

uses
  MufasaTypes,FileUtil;

{ TMPlugins }

function TMPlugins.InitPlugin(plugin: TLibHandle): boolean;
var
  pntrArrc     :  function : integer; stdcall;
  GetFuncInfo  :  function (x: Integer; var ProcAddr: Pointer; var ProcDef: PChar) : Integer; stdcall;
  GetTypeCount :  function : Integer; stdcall;
  GetTypeInfo  :  function (x: Integer; var sType, sTypeDef: string): Integer; stdcall;
  PD : PChar;
  pntr : Pointer;
  arrc, ii : integer;
begin
  Pointer(pntrArrc) := GetProcAddress(plugin, PChar('GetFunctionCount'));
  if @pntrArrc = nil then begin result:= false; exit; end;
  Pointer(GetFuncInfo) := GetProcAddress(plugin, PChar('GetFunctionInfo'));
  if @GetFuncInfo = nil then begin result:= false; exit; end;
  arrc := pntrArrc();
  SetLength(Plugins,NumPlugins+1);
  Plugins[NumPlugins].MethodLen := Arrc;
  SetLength(Plugins[NumPlugins].Methods, ArrC);
  pd := StrAlloc(255);
  for ii := 0 to ArrC-1 do
  begin;
    if (GetFuncInfo(ii, pntr, pd) < 0) then
      Continue;
    Plugins[NumPlugins].Methods[ii].FuncPtr := pntr;
    Plugins[NumPlugins].Methods[ii].FuncStr := pd;
  end;
  StrDispose(pd);
  inc(NumPlugins);
  result:= true;
end;

end.

