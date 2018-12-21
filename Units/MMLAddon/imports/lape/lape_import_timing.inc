'var HoursPerDay: Int32 = 24;' + LineEnding +
'var MinsPerHour: Int32 = 60;' + LineEnding +
'var SecsPerMin: Int32 = 60;' + LineEnding +
'var MSecsPerSec: Int32 = 1000;' + LineEnding +
'var MinsPerDay: Int32 = 1440;' + LineEnding +
'var SecsPerDay: Int32 = 86400;' + LineEnding +
'var MSecsPerDay: Int32 = 86400000;' + LineEnding +
'var DateDelta: Int32 = 693594;' + LineEnding +
'type TDateTime = type Double;' + LineEnding +
'function EncodeDate(Year, Month, Day: UInt16): TDateTime; begin end;' + LineEnding +
'function EncodeTime(Hour, Min, Sec, MSec: UInt16): TDateTime; begin end;' + LineEnding +
'procedure DecodeDate(DateTime: TDateTime; var Year, Month, Day: UInt16); begin end;' + LineEnding +
'function DecodeDateFully(DateTime: TDateTime; var Year, Month, Day, DOW: UInt16): Boolean; begin end;' + LineEnding +
'procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: UInt16); begin end;' + LineEnding +
'function DateTimeToStr(const DateTime: TDateTime): string; begin end;' + LineEnding +
'function DateToStr(const DateTime: TDateTime): string; begin end;' + LineEnding +
'function TimeToStr(const DateTime: TDateTime): string; begin end;' + LineEnding +
'function Date: TDateTime; begin end;' + LineEnding +
'function Time: TDateTime; begin end;' + LineEnding +
'function Now: TDateTime; begin end;' + LineEnding +
'function GetTickCount: UInt64; begin end;' + LineEnding +
'procedure ReplaceTime(var DateTime: TDateTime; NewTime: TDateTime); begin end;' + LineEnding +
'procedure ReplaceDate(var DateTime: TDateTime; NewDate: TDateTime); begin end;' + LineEnding +
'function FormatDateTime(Format: string; DateTime: TDateTime): string; begin end;' + LineEnding +
'function StrToDate(s: string): TDateTime; begin end;' + LineEnding +
'function StrToDateDef(s: string; Default: TDateTime): TDateTime; begin end;' + LineEnding +
'function StrToTime(s: string): TDateTime; begin end;' + LineEnding +
'function StrToTimeDef(s: string; Default: TDateTime): TDateTime; begin end;' + LineEnding +
'function StrToDateTime(s: string): TDateTime; begin end;' + LineEnding +
'function StrToDateTimeDef(s: string; Default: TDateTime): TDateTime; begin end;' + LineEnding;
