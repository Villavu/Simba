unit simbascript.import_time_date;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Time_Date(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.misc;

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

procedure Lape_GetTimeRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := GetTickCount64() - TSimbaScript(Params^[0]).StartTime;
end;

procedure Lape_Import_Time_Date(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Time & Date';

    addGlobalMethod('procedure ConvertTime(Time: Int32; var h, m, s: Int32);', @Lape_ConvertTime, Data);
    addGlobalMethod('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Int32);', @Lape_ConvertTime64, Data);
    addGlobalMethod('function GetSystemTime: UInt64;', @Lape_GetSystemTime, Data);
    addGlobalMethod('function GetTimeRunning: UInt64;', @Lape_GetTimeRunning, Data);
  end;
end;

end.

