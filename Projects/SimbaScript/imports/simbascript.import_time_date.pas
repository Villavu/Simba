unit script_import_time_date;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports,lpcompiler, lptypes, mmisc;

procedure Lape_ConvertTime(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ConvertTime(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_ConvertTime64(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ConvertTime64(PUInt64(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^);
end;

procedure Lape_GetSystemTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := GetTickCount64();
end;

procedure Lape_Import_Time_Date(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('procedure ConvertTime(Time: Int32; var h, m, s: Int32);', @Lape_ConvertTime, Data);
    addGlobalMethod('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Int32);', @Lape_ConvertTime64, Data);
    addGlobalMethod('function GetSystemTime: UInt64;', @Lape_GetSystemTime, Data);
  end;
end;

initialization
  ScriptImports.Add('Time & Date', @Lape_Import_Time_Date);

end.

