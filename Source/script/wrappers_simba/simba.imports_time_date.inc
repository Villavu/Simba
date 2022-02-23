pushSection('Date & Time');

addGlobalFunc('procedure ConvertTime(Time: Int32; var h, m, s: Int32)', @_LapeConvertTime);
addGlobalFunc('procedure ConvertTime64(Time: UInt64; var y, m, w, d, h, min, s: Int32)', @_LapeConvertTime64);
addGlobalFunc('function GetSystemTime: UInt64', @_LapeGetSystemTime);
addGlobalFunc('function GetTimeRunning: UInt64', @_LapeGetTimeRunning);
addGlobalFunc('function HighResolutionTime: Double;', @_LapeHighResolutionTime);

popSection();

