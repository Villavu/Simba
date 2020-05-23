unit simba.pluginparser;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.codeinsight;

function ParsePlugin(Plugin: String; SearchPaths: array of String): TCodeInsight;

implementation

uses
  dynlibs,
  simba.main, simba.files;

function ParsePlugin(Plugin: String; SearchPaths: array of String): TCodeInsight;
var
  Path, Script: String;
  Lib: TLibHandle;
  Name, Str, Header: PChar;
  Index: Int32;
  Address: Pointer;
var
  GetFunctionInfo: function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
  GetFunctionCount: function: Int32; cdecl;
  GetTypeInfo: function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
  GetTypeCount: function: Int32; cdecl;
  GetCode: procedure(var Code: PChar); cdecl;
  GetCodeLength: function: Int32; cdecl;
begin
  Result := nil;

  Lib := NilHandle;
  Path := FindPlugin(Plugin, SearchPaths);

  if Path <> '' then
  try
    WriteLn('Parsing plugin: ', Path);

    Lib := LoadLibrary(Path);
    if (Lib <> NilHandle) then
    begin
      Pointer(GetFunctionInfo) := GetProcedureAddress(Lib, 'GetFunctionInfo');
      Pointer(GetFunctionCount) := GetProcedureAddress(Lib, 'GetFunctionCount');
      Pointer(GetTypeInfo) := GetProcedureAddress(Lib, 'GetTypeInfo');
      Pointer(GetTypeCount) := GetProcedureAddress(Lib, 'GetTypeCount');
      Pointer(GetCode) := GetProcedureAddress(Lib, 'GetCode');
      Pointer(GetCodeLength) := GetProcedureAddress(Lib, 'GetCodeLength');

      Script := '';

      if (Pointer(GetTypeCount) <> nil) and (Pointer(GetTypeInfo) <> nil) then
      begin
        Name := StrAlloc(2048);
        Str := StrAlloc(2048);

        try
          for Index := 0 to GetTypeCount() - 1 do
          begin
            GetTypeInfo(Index, Name, Str);

            Script := Script + 'type ' + Name + ' = ' + Str;
            if (not Script.EndsWith(';')) then
              Script := Script + ';';
          end;
        finally
          StrDispose(Name);
          StrDispose(Str);
        end;
      end;

      if (Pointer(GetFunctionCount) <> nil) and (Pointer(GetFunctionInfo) <> nil) then
      begin
        Header := StrAlloc(2048);

        try
          for Index := 0 to GetFunctionCount() - 1 do
          begin
            GetFunctionInfo(Index, Address, Header);

            Script := Script + Header;

            // Remove native, and add trailing semicolon if needed
            if (not Script.EndsWith(';')) then
              Script := Script + ';';
            if Script.EndsWith('native;', True) then
              Script.Remove(Length(Script) - Length('native;'), $FFFFFF);
            if (not Script.EndsWith(';')) then
              Script := Script + ';';

            Script := Script + 'begin end;';
          end;
        finally
          StrDispose(Header);
        end;
      end;

      if (Pointer(GetCodeLength) <> nil) and (Pointer(GetCode) <> nil) then
      begin
        Str := StrAlloc(GetCodeLength() + 1);

        try
          GetCode(Str);

          Script := Script + Str;
        finally
          StrDispose(Str);
        end;
      end;

      Result := TCodeInsight.Create();
      Result.FileName := Plugin;
      Result.OnMessage := @SimbaForm.OnCCMessage;
      Result.Run(Script);
    end;
  except
    on E: Exception do
      WriteLn('Error parsing plugin "' + Plugin + '" : ' + E.Message);
  end;

  if Lib <> NilHandle then
    FreeLibrary(Lib);
end;

end.

