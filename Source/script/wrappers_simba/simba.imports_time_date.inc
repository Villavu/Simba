pushSection('Date & Time');

addGlobalFunc('procedure ConvertTime(Time: Integer; var h, m, s: Integer)', @_LapeConvertTime);
addGlobalFunc('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Integer)', @_LapeConvertTime64);
addGlobalFunc('function GetTimeRunning: UInt64', @_LapeGetTimeRunning);
addGlobalFunc('function PerformanceTimer: Double;', @_LapePerformanceTimer);
addGlobalFunc('function FormatMilliseconds(Milliseconds: Int64; Format: String): String; overload;', @_LapeFormatMillisecondsI);
addGlobalFunc('function FormatMilliseconds(Milliseconds: Double; Format: String): String; overload;', @_LapeFormatMillisecondsF);

popSection();

