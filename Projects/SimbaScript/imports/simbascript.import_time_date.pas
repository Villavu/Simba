unit simbascript.import_time_date;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Time_Date(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  dateutils, LazSysUtils,
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

procedure Lape_NowUTC(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := NowUTC();
end;

procedure Lape_UnixTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := DateTimeToUnix(NowUTC());
end;

procedure Lape_UnixToDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := UnixToDateTime(PInt64(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_DateTimeToUnix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := DateTimeToUnix(PDateTime(Params^[1])^, PBoolean(Params^[2])^);
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
    addGlobalMethod('function NowUTC: TDateTime;', @Lape_NowUTC, Data);
    addGlobalMethod('function UnixTime: Int64;', @Lape_UnixTime, Data);
    addGlobalMethod('function DateTimeToUnix(const Value: TDateTime; IsUTC: Boolean = True): Int64;', @Lape_DateTimeToUnix, Data);
    addGlobalMethod('function UnixToDateTime(const Value: Int64; ReturnUTC: Boolean = True): TDateTime;', @Lape_UnixToDateTime, Data);
  end;
end;

end.

