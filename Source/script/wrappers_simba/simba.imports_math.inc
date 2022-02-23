pushSection('Math');

addGlobalFunc('function GaussMatrix(N: Integer; Sigma: Extended): TExtendedMatrix', @_LapeGaussMatrix);
addGlobalFunc('function Distance(const X1, Y1, X2, Y2: Integer): Integer; overload', @_LapeDistance);
addGlobalFunc('function Distance(const P1, P2: TPoint): Integer; overload', @_LapeDistanceEx);
addGlobalFunc('function RandomRange(const Min, Max: Integer): Integer', @_LapeRandomRange);
addGlobalFunc('function FixD(Degrees: Extended): Extended', @_LapeFixD);
addGlobalFunc('function LogN(base, x: Extended): Extended', @_LapeLogn);
addGlobalFunc('function Sar(num: Integer; shift: byte): Integer', @_LapeSar);
addGlobalFunc('function Ror(num: UInt32; shift: byte): UInt32', @_LapeRor);
addGlobalFunc('function Rol(num: UInt32; shift: byte): UInt32', @_LapeRol);
addGlobalFunc('function Radians(e: Extended): Extended', @_LapeRadians);
addGlobalFunc('function Degrees(e: Extended): Extended', @_LapeDegrees);
addGlobalFunc('function Log10(f: Extended): Extended', @_LapeLog10);
addGlobalFunc('function FixRad(rad: Extended): Extended', @_LapeFixRad);
addGlobalFunc('function NextPower2(const n: Integer): Integer', @_LapeNextPower2);

addGlobalFunc('function MinA(const Arr: TIntegerArray): Integer; overload', @_LapeMinA);
addGlobalFunc('function MaxA(const Arr: TIntegerArray): Integer; overload', @_LapeMaxA);

addGlobalFunc('function MinA(const Arr: TExtendedArray): Extended; overload', @_LapeMinAF);
addGlobalFunc('function MaxA(const Arr: TExtendedArray): Extended; overload', @_LapeMaxAF);

addGlobalFunc('function Modulo(const X, Y: Integer): Integer; overload', @_LapeModulo);
addGlobalFunc('function Modulo(const X, Y: Extended): Extended; overload', @_LapeModuloF);

addGlobalFunc('function Mode(const Arr: TIntegerArray): Integer', @_LapeMode);

addGlobalFunc('function Sum(const Arr: TIntegerArray): Int64; overload', @_LapeSum);
addGlobalFunc('function Sum(const Arr: TExtendedArray): Extended; overload', @_LapeSumF);

addGlobalFunc('function Average(const Arr: TIntegerArray): Int64; overload', @_LapeAverage);
addGlobalFunc('function Average(const Arr: TExtendedArray): Extended; overload', @_LapeAverageF);

popSection();

