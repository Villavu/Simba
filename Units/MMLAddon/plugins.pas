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

    Plugins Class for the Mufasa Macro Library
}

unit plugins;

{$mode objfpc}{$H+}

interface
{$MACRO ON}
{$define callconv:=
    {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
    {$IFDEF LINUX}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
}

uses
  Classes, SysUtils, dynlibs, libloader;

const
  // stdcall
  cv_StdCall = 0;

  // register call
  cv_Register = 1;

  // default (cdecl where supported) otherwise native platform convention
  cv_default = 2;

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
    MemMgrSet: boolean;
    ABI: Integer;
    Handle: TLibHandle;
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
      procedure FreePlugins; override;
      procedure FreePlugin(plugin: TMPlugin);
  end;

implementation

uses
  MufasaTypes, FileUtil;

{ TMPlugins }
function TMPlugins.InitPlugin(Plugin: TLibHandle): boolean;
var
  GetFuncCount_std: function: integer; stdcall;
  GetFuncInfo_std: function(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): integer; stdcall;
  GetFuncConv_std: function(x: integer): integer; stdcall;

  GetTypeCount_std: function: integer; stdcall;

  SetPluginMemManager_std: procedure(MemMgr : TMemoryManager); stdcall;
  OnAttach_std: procedure(info: Pointer); stdcall;

  // ABI = 0
  GetTypeInfo0: function(x: Integer; var sType, sTypeDef: string): integer; stdcall;

  // ABI = 1
  GetTypeInfo1: function(x: Integer; var sType, sTypeDef: PChar): integer; stdcall;

  // ABI = 2
  GetPluginABIVersion: function: Integer; callconv
  GetTypeInfo: function(x: Integer; var sType: PChar; var sTypeDef: PChar): integer; callconv
  GetTypeCount: function: integer; callconv
  GetFuncCount: function: integer; callconv
  GetFuncInfo: function(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): integer; callconv
  SetPluginMemManager: procedure(MemMgr : TMemoryManager); callconv
  OnAttach: procedure(info: Pointer); callconv

  PD, PD2: PChar;
  pntr: Pointer;
  ArrC, I: integer;

  // Memory manager. We use this to retrieve our own memory manager and pass it
  // to the plugin to simplify sharing of memory with FPC plugins.
  MemMgr : TMemoryManager;

  // Plugin ABI version. Requires due to ABI changes and backwards
  // compatibility.
  PluginVersion: Integer;

  // Strings for ABI = 0
  a, b: String;
begin
  Result := False;

  SetLength(Plugins, NumPlugins + 1);

  // Query ABI. Oldest is 0 (where GetPluginABIVersion is not exported)
  Pointer(GetPluginABIVersion) := GetProcAddress(Plugin, PChar('GetPluginABIVersion'));
  if Assigned(GetPluginABIVersion) then
    PluginVersion := GetPluginABIVersion()
  else
    PluginVersion := 0;

  Writeln('Plugin Version:', PluginVersion);

  Plugins[NumPlugins].ABI := PluginVersion;
  Plugins[NumPlugins].handle := Plugin;

  if PluginVersion < 2 then
  begin
    Pointer(SetPluginMemManager_std) := GetProcAddress(Plugin, PChar('SetPluginMemManager'));
    if (Assigned(SetPluginMemManager_std)) then
    begin
      Plugins[NumPlugins].MemMgrSet := True;
      GetMemoryManager(MemMgr);
      SetPluginMemManager_std(MemMgr);
    end;

    Pointer(OnAttach_std) := GetProcAddress(Plugin, PChar('OnAttach'));
    if Assigned(OnAttach_std) then
    begin
      OnAttach_std(nil);
    end;
  end else
  begin
    Pointer(SetPluginMemManager) := GetProcAddress(Plugin, PChar('SetPluginMemManager'));
    if (Assigned(SetPluginMemManager)) then
    begin
      Plugins[NumPlugins].MemMgrSet := True;
      GetMemoryManager(MemMgr);
      SetPluginMemManager(MemMgr);
    end;
    Pointer(OnAttach) := GetProcAddress(Plugin, PChar('OnAttach'));
    if Assigned(OnAttach) then
    begin
      OnAttach(nil);
    end;
  end;

  if PluginVersion < 2 then
  begin
      Pointer(GetTypeCount_std) := GetProcAddress(Plugin, PChar('GetTypeCount'));
      if (Assigned(GetTypeCount_std)) then
      begin
        case PluginVersion of
          0:
            begin
              Pointer(GetTypeInfo0) := GetProcAddress(Plugin, PChar('GetTypeInfo'));
              if Assigned(GetTypeInfo0) then
              begin
                ArrC := GetTypeCount_std();

                Plugins[NumPlugins].TypesLen := ArrC;
                SetLength(Plugins[NumPlugins].Types, ArrC);

                for I := 0 to ArrC - 1 do
                begin
                  if (GetTypeInfo0(I, a, b) >= 0) then
                  begin
                    Plugins[NumPlugins].Types[I].TypeName := Copy(a, 1, Length(a));
                    Plugins[NumPlugins].Types[I].TypeDef := Copy(b, 1, Length(b));
                    writeln('Loading: ', Plugins[NumPlugins].Types[I].TypeName);
                  end
                  else
                  begin
                    Plugins[NumPlugins].Types[I].TypeName := '';
                    Plugins[NumPlugins].Types[I].TypeDef := '';
                  end;
                end;
              end;
            end;
          1:
            begin
              Pointer(GetTypeInfo1) := GetProcAddress(Plugin, PChar('GetTypeInfo'));
              if Assigned(GetTypeInfo1) then
              begin
                ArrC := GetTypeCount_std();
                Plugins[NumPlugins].TypesLen := ArrC;
                SetLength(Plugins[NumPlugins].Types, ArrC);

                PD := StrAlloc(1024);
                PD2 := StrAlloc(1024);

                for I := 0 to ArrC - 1 do
                begin
                  if (GetTypeInfo1(I, PD, PD2) >= 0) then
                  begin
                    Plugins[NumPlugins].Types[I].TypeName := PD;
                    Plugins[NumPlugins].Types[I].TypeDef := PD2;
                    writeln('Loading: ', Plugins[NumPlugins].Types[I].TypeName);
                  end
                  else
                  begin
                    Plugins[NumPlugins].Types[I].TypeName := '';
                    Plugins[NumPlugins].Types[I].TypeDef := '';
                  end;
                end;

                StrDispose(PD);
                StrDispose(PD2);
              end;
            end;
        end;
      end;
  end else
  begin
    Pointer(GetTypeCount) := GetProcAddress(Plugin, PChar('GetTypeCount'));
    if (Assigned(GetTypeCount)) then
    begin
      Pointer(GetTypeInfo) := GetProcAddress(Plugin, PChar('GetTypeInfo'));
      if Assigned(GetTypeInfo) then
      begin
        ArrC := GetTypeCount();
        Plugins[NumPlugins].TypesLen := ArrC;
        SetLength(Plugins[NumPlugins].Types, ArrC);

        PD := StrAlloc(1024);
        PD2 := StrAlloc(1024);

        for I := 0 to ArrC - 1 do
        begin
          if (GetTypeInfo(I, PD, PD2) >= 0) then
          begin
            Plugins[NumPlugins].Types[I].TypeName := PD;
            Plugins[NumPlugins].Types[I].TypeDef := PD2;
            writeln('Loading: ', Plugins[NumPlugins].Types[I].TypeName);
          end
          else
          begin
            Plugins[NumPlugins].Types[I].TypeName := '';
            Plugins[NumPlugins].Types[I].TypeDef := '';
          end;
        end;

        StrDispose(PD);
        StrDispose(PD2);
      end;
    end;
  end;

  if PluginVersion < 2 then
  begin
    Pointer(GetFuncCount_std) := GetProcAddress(Plugin, PChar('GetFunctionCount'));
    if (Assigned(GetFuncCount_std)) then
    begin
      Pointer(GetFuncInfo_std) := GetProcAddress(Plugin, PChar('GetFunctionInfo'));

      if (Assigned(GetFuncInfo_std)) then
      begin
        Pointer(GetFuncConv_std) := GetProcAddress(Plugin, PChar('GetFunctionCallingConv'));

        ArrC := GetFuncCount_std();
        Plugins[NumPlugins].MethodLen := ArrC;
        SetLength(Plugins[NumPlugins].Methods, ArrC);

        PD := StrAlloc(1024);

        for I := 0 to ArrC - 1 do
        begin;
          if (GetFuncInfo_std(I, pntr, PD) < 0) then
            Continue;

          Plugins[NumPlugins].Methods[I].FuncPtr := pntr;
          Plugins[NumPlugins].Methods[I].FuncStr := PD;
          writeln('Loading: ', Plugins[NumPlugins].Methods[I].FuncStr);

          if (Assigned(GetFuncConv_std)) then
            Plugins[NumPlugins].Methods[I].FuncConv := GetFuncConv_std(I);
        end;

        StrDispose(PD);
      end;
    end;
  end else
  begin
    Pointer(GetFuncCount) := GetProcAddress(Plugin, PChar('GetFunctionCount'));
    if (Assigned(GetFuncCount)) then
    begin
      Pointer(GetFuncInfo) := GetProcAddress(Plugin, PChar('GetFunctionInfo'));

      if (Assigned(GetFuncInfo)) then
      begin
        ArrC := GetFuncCount();
        Plugins[NumPlugins].MethodLen := ArrC;
        SetLength(Plugins[NumPlugins].Methods, ArrC);

        PD := StrAlloc(1024);

        for I := 0 to ArrC - 1 do
        begin;
          if (GetFuncInfo(I, pntr, PD) < 0) then
            Continue;

          writeln(pd);
          Plugins[NumPlugins].Methods[I].FuncPtr := pntr;
          Plugins[NumPlugins].Methods[I].FuncStr := PD;
          Plugins[NumPlugins].Methods[I].FuncConv := cv_default;

          writeln('Loading: ', Plugins[NumPlugins].Methods[I].FuncStr);
        end;

        StrDispose(PD);
      end;
    end;
  end;

  Inc(NumPlugins);
  Result := True;
end;


procedure TMPlugins.FreePlugins;
var
  I : integer;
begin
  for i := 0 to NumPlugins - 1 do
  begin
    writeln(format('Freeing plugin[%d]',[i]));
    FreePlugin(Plugins[i]);
  end;
  SetLength(Plugins ,0);
  numPlugins := 0;
end;

procedure TMPlugins.FreePlugin(plugin: TMPlugin);
var
    OnDetach_std: procedure(); stdcall;
    OnDetach: procedure(); callconv
begin
  if (plugin.handle > 0) then
  try
    if plugin.ABI < 2 then
    begin
      Pointer(OnDetach_std) := GetProcAddress(
          plugin.handle, 'OnDetach');
      if Assigned(OnDetach_std) then
      begin
        writeln('Calling OnDetach');
        OnDetach_std();
      end;
    end else
    begin
      Pointer(OnDetach) := GetProcAddress(
          plugin.handle, 'OnDetach');
      if Assigned(OnDetach) then
      begin
        writeln('Calling OnDetach');
        OnDetach();
      end;
    end;

    FreeLibrary(plugin.handle);
  except
  end;

end;

end.

