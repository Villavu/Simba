{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Base types / methods that are used throughout Simba.
}
unit simba.base;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics;

{$PUSH}
{$SCOPEDENUMS ON}
type
  EKeyCode = (
    UNKNOWN             = 0,
    LBUTTON             = 1,
    RBUTTON             = 2,
    CANCEL              = 3,
    MBUTTON             = 4,
    XBUTTON1            = 5,
    XBUTTON2            = 6,
    BACK                = 8,
    TAB                 = 9,
    CLEAR               = 12,
    RETURN              = 13,
    SHIFT               = 16,
    CONTROL             = 17,
    MENU                = 18,
    PAUSE               = 19,
    CAPITAL             = 20,
    ESCAPE              = 27,
    CONVERT             = 28,
    NONCONVERT          = 29,
    ACCEPT              = 30,
    MODECHANGE          = 31,
    SPACE               = 32,
    PRIOR               = 33,
    NEXT                = 34,
    END_KEY             = 35,
    HOME                = 36,
    LEFT                = 37,
    UP                  = 38,
    RIGHT               = 39,
    DOWN                = 40,
    SELECT              = 41,
    PRINT               = 42,
    EXECUTE             = 43,
    SNAPSHOT            = 44,
    INSERT              = 45,
    DELETE              = 46,
    HELP                = 47,
    NUM_0               = 48,
    NUM_1               = 49,
    NUM_2               = 50,
    NUM_3               = 51,
    NUM_4               = 52,
    NUM_5               = 53,
    NUM_6               = 54,
    NUM_7               = 55,
    NUM_8               = 56,
    NUM_9               = 57,
    A                   = 65,
    B                   = 66,
    C                   = 67,
    D                   = 68,
    E                   = 69,
    F                   = 70,
    G                   = 71,
    H                   = 72,
    I                   = 73,
    J                   = 74,
    K                   = 75,
    L                   = 76,
    M                   = 77,
    N                   = 78,
    O                   = 79,
    P                   = 80,
    Q                   = 81,
    R                   = 82,
    S                   = 83,
    T                   = 84,
    U                   = 85,
    V                   = 86,
    W                   = 87,
    X                   = 88,
    Y                   = 89,
    Z                   = 90,
    LWIN                = 91,
    RWIN                = 92,
    APPS                = 93,
    SLEEP               = 95,
    NUMPAD_0            = 96,
    NUMPAD_1            = 97,
    NUMPAD_2            = 98,
    NUMPAD_3            = 99,
    NUMPAD_4            = 100,
    NUMPAD_5            = 101,
    NUMPAD_6            = 102,
    NUMPAD_7            = 103,
    NUMPAD_8            = 104,
    NUMPAD_9            = 105,
    MULTIPLY            = 106,
    ADD                 = 107,
    SEPARATOR           = 108,
    SUBTRACT            = 109,
    DECIMAL             = 110,
    DIVIDE              = 111,
    F1                  = 112,
    F2                  = 113,
    F3                  = 114,
    F4                  = 115,
    F5                  = 116,
    F6                  = 117,
    F7                  = 118,
    F8                  = 119,
    F9                  = 120,
    F10                 = 121,
    F11                 = 122,
    F12                 = 123,
    F13                 = 124,
    F14                 = 125,
    F15                 = 126,
    F16                 = 127,
    F17                 = 128,
    F18                 = 129,
    F19                 = 130,
    F20                 = 131,
    F21                 = 132,
    F22                 = 133,
    F23                 = 134,
    F24                 = 135,
    NUMLOCK             = 144,
    SCROLL              = 145,
    LSHIFT              = 160,
    RSHIFT              = 161,
    LCONTROL            = 162,
    RCONTROL            = 163,
    LMENU               = 164,
    RMENU               = 165,
    BROWSER_BACK        = 166,
    BROWSER_FORWARD     = 167,
    BROWSER_REFRESH     = 168,
    BROWSER_STOP        = 169,
    BROWSER_SEARCH      = 170,
    BROWSER_FAVORITES   = 171,
    BROWSER_HOME        = 172,
    VOLUME_MUTE         = 173,
    VOLUME_DOWN         = 174,
    VOLUME_UP           = 175,
    MEDIA_NEXT_TRACK    = 176,
    MEDIA_PREV_TRACK    = 177,
    MEDIA_STOP          = 178,
    MEDIA_PLAY_PAUSE    = 179,
    LAUNCH_MAIL         = 180,
    LAUNCH_MEDIA_SELECT = 181,
    LAUNCH_APP1         = 182,
    LAUNCH_APP2         = 183,
    OEM_1               = 186,
    OEM_PLUS            = 187,
    OEM_COMMA           = 188,
    OEM_MINUS           = 189,
    OEM_PERIOD          = 190,
    OEM_2               = 191,
    OEM_3               = 192,
    OEM_4               = 219,
    OEM_5               = 220,
    OEM_6               = 221,
    OEM_7               = 222,
    OEM_8               = 223,
    OEM_102             = 226,
    PROCESSKEY          = 231,
    ATTN                = 246,
    CRSEL               = 247,
    EXSEL               = 248,
    EREOF               = 249,
    PLAY                = 250,
    ZOOM                = 251
  );

  EMouseButton = (
    LEFT,
    RIGHT,
    MIDDLE,
    SCROLL_UP,
    SCROLL_DOWN
  );

  PKeyCode = ^EKeyCode;
  PMouseButton = ^EMouseButton;

  ESimbaProcessType = (UNKNOWN, IDE, SCRIPT, SCRIPT_WITH_COMMUNICATION);
  ESimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING, STATE_NONE);

  ESimbaCommunicationMessage = (
    SIMBA_TITLE, SIMBA_PID, SIMBA_TARGET_PID, SIMBA_TARGET_WINDOW,
    SCRIPT, SCRIPT_ERROR, SCRIPT_STATE_CHANGE,
    TRAY_NOTIFICATION,
    DEBUGIMAGE_UPDATE, DEBUGIMAGE_MAXSIZE, DEBUGIMAGE_SHOW, DEBUGIMAGE_HIDE, DEBUGIMAGE_DISPLAY, DEBUGIMAGE_DISPLAY_XY
  );
{$POP}

  // Colors
  TColorRGB = record
    R,G,B: Byte;
  end;

  TColorXYZ = record
    X,Y,Z: Single;
  end;

  TColorLAB = record
    L,A,B: Single;
  end;

  TColorLCH = record
    L,C,H: Single;
  end;

  TColorHSV = record
    H,S,V: Single;
  end;

  TColorHSL = record
    H,S,L: Single;
  end;

  TColorBGR = packed record
    B,G,R: Byte;
  end;
  PColorBGR = ^TColorBGR;

  TColorARGB = packed record
  case Byte of
    0: (A, R, G, B: Byte);
    1: (AsInteger: UInt32);
  end;

  TColorBGRA = packed record
  case Byte of
    0: (B, G, R, A: Byte);
    1: (AsInteger: UInt32);
  end;

  TColorRGBA = packed record
  case Byte of
    0: (R,G,B,A: Byte);
    1: (AsInteger: UInt32);
  end;

  PColorARGB = ^TColorARGB;
  PColorBGRA = ^TColorBGRA;
  PColorRGB = ^TColorRGB;
  PColorXYZ = ^TColorXYZ;
  PColorLAB = ^TColorLAB;
  PColorLCH = ^TColorLCH;
  PColorHSV = ^TColorHSV;
  PColorHSL = ^TColorHSL;

  TColorArray = array of TColor;
  PColorArray = ^TColorArray;

  TWindowHandle = type UInt64;
  TWindowHandleArray = array of TWindowHandle;
  PWindowHandle = ^TWindowHandle;
  PWindowHandleArray = ^TWindowHandleArray;

  PStringArray = ^TStringArray;

  TPointF = record X, Y: Double; end;
  TPointFArray = array of TPointF;

  PPoint = ^TPoint;
  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;
  P2DPointArray = ^T2DPointArray;
  T2DPointArray = array of TPointArray;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array of Integer;

  PIntegerMatrix = ^TIntegerMatrix;
  TIntegerMatrix = array of TIntegerArray;

  PByteArray = ^TByteArray;
  TByteArray = array of Byte;

  PByteMatrix = ^TByteMatrix;
  TByteMatrix = array of TByteArray;

  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array of Boolean;

  PBooleanMatrix = ^TBooleanMatrix;
  TBooleanMatrix = array of TBooleanArray;

  PSingleArray  = ^TSingleArray;
  TSingleArray  = array of Single;
  PSingleMatrix = ^TSingleMatrix;
  TSingleMatrix = array of TSingleArray;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array of Double;

  PDoubleMatrix = ^TDoubleMatrix;
  TDoubleMatrix = array of TDoubleArray;

  TInt64Array = array of Int64;
  PInt64Array = ^TInt64Array;

  EComparator = (__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__);
  PComparator = ^EComparator;

  // Box
  TBox = record
  case Integer of
    0: (X1, Y1, X2, Y2: Integer);
    1: (TopLeft, BottomRight: TPoint);
  end;
  TBoxArray = array of TBox;

  PBox = ^TBox;
  PBoxArray = ^TBoxArray;

type
  TQuad = record
    Top: TPoint;
    Right: TPoint;
    Bottom: TPoint;
    Left: TPoint;
  end;
  TQuadArray = array of TQuad;

  PQuad = ^TQuad;
  PQuadArray = ^TQuadArray;

  TCircle = record
    X: Integer;
    Y: Integer;
    Radius: Integer;
  end;
  TCircleArray = array of TCircle;

  PCircle = ^TCircle;
  PCircleArray = ^TCircleArray;

{$PUSH}
{$SCOPEDENUMS ON}
type
  EDebugLn = (CLEAR, YELLOW, RED, GREEN, FOCUS);
  EDebugLnFlags = set of EDebugLn;
{$POP}

var
  OnDebugLn: procedure(const S: String) of object = nil;

procedure Debug(const Msg: String); overload;
procedure Debug(const Msg: String; Args: array of const); overload;
procedure DebugLn(const Msg: String); overload;
procedure DebugLn(const Msg: String; Args: array of const); overload;
procedure DebugLn(const Flags: EDebugLnFlags; const Msg: String); overload;
procedure DebugLn(const Flags: EDebugLnFlags; const Msg: String; Args: array of const); overload;

function FlagsToString(const Flags: EDebugLnFlags): String;
function FlagsFromString(var Str: String): EDebugLnFlags;

function InRange(const AValue, AMin, AMax: Integer): Boolean; overload;
function InRange(const AValue, AMin, AMax: Int64): Boolean; overload;
function InRange(const AValue, AMin, AMax: Single): Boolean; overload;
function InRange(const AValue, AMin, AMax: Double): Boolean; overload;

function Min(const A, B: Int64): Int64; overload;
function Max(const A, B: Int64): Int64; overload;
function Min(const A, B: Single): Single; overload;
function Max(const A, B: Single): Single; overload;
function Min(const A, B: Double): Double; overload;
function Max(const A, B: Double): Double; overload;

procedure Swap(var A, B: Byte); overload;
procedure Swap(var A, B: Integer); overload;
procedure Swap(var A, B: Single); overload;
procedure Swap(var A, B: Double); overload;
procedure Swap(var A, B: Pointer); overload;
procedure Swap(var A, B: TPoint); overload;
procedure Swap(var A, B: TColorBGRA); overload;

function IfThen(const Val: Boolean; const IfTrue, IfFalse: String): String overload;
function IfThen(const Val: Boolean; const IfTrue, IfFalse: Int64): Int64; overload;
function IfThen(const Val: Boolean; const IfTrue, IfFalse: Boolean): Boolean; overload;

// Generic helpers
generic function Min<_T>(const A, B: _T): _T;
generic function Max<_T>(const A, B: _T): _T;
generic procedure Swap<_T>(var A, B: _T);
generic procedure MoveElement<_T>(var Arr: specialize TArray<_T>; AFrom, ATo: Integer);
generic function MinA<_T>(const Arr: specialize TArray<_T>): _T;
generic function MaxA<_T>(const Arr: specialize TArray<_T>): _T;
generic function Sum<_T, _R>(var AValues: specialize TArray<_T>): _R;
generic procedure Reverse<_T>(var Arr: specialize TArray<_T>);
generic function Reversed<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
generic function Equals<_T>(const A, B: specialize TArray<_T>): Boolean;
generic function IndexOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
generic function IndicesOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;

type
  TBooleanHelper = type helper for Boolean
    function ToString: String;
  end;

type
  ESimbaException = class(Exception);

procedure SimbaException(Message: String; Args: array of const); overload;
procedure SimbaException(Message: String); overload;

// Writable const
const
  SimbaProcessType: ESimbaProcessType = ESimbaProcessType.UNKNOWN;

implementation

uses
  Math, TypInfo;

procedure Debug(const Msg: String);
begin
  if Assigned(OnDebugLn) then
  begin
    OnDebugLn(Msg);
    Exit;
  end;

  {$I-}
  Write(Msg);
  {$I+}
end;

procedure Debug(const Msg: String; Args: array of const);
begin
  Debug(Format(Msg, Args));
end;

procedure DebugLn(const Msg: String);
begin
  if Assigned(OnDebugLn) then
  begin
    OnDebugLn(Msg);
    Exit;
  end;

  {$I-}
  WriteLn(Msg);
  Flush(Output);
  {$I+}
end;

procedure DebugLn(const Msg: String; Args: array of const);
begin
  DebugLn(Format(Msg, Args));
end;

procedure DebugLn(const Flags: EDebugLnFlags; const Msg: String);
begin
  DebugLn(FlagsToString(Flags) + Msg);
end;

procedure DebugLn(const Flags: EDebugLnFlags; const Msg: String; Args: array of const);
begin
  DebugLn(FlagsToString(Flags) + Format(Msg, Args));
end;

procedure SimbaException(Message: String; Args: array of const);
begin
  raise ESimbaException.CreateFmt(Message, Args);
end;

procedure SimbaException(Message: String);
begin
  raise ESimbaException.Create(Message);
end;

const
  DebugLnFlagsHeader       = String(#0#0);
  DebugLnFlagsHeaderLength = Length(DebugLnFlagsHeader) + 6;

function FlagsToString(const Flags: EDebugLnFlags): String; inline;
begin
  Result := DebugLnFlagsHeader + IntToHex(Integer(Flags), 6);
end;

function FlagsFromString(var Str: String): EDebugLnFlags;

  function HexToInt(P: PChar): Integer; inline;
  var
    N, I: Integer;
    Val: Char;
  begin
    Result := 0;

    for I := 1 to 6 do
    begin
      Val := P^;
      case Val of
        '0'..'9': N := Ord(Val) - (Ord('0'));
        'a'..'f': N := Ord(Val) - (Ord('a') - 10);
        'A'..'F': N := Ord(Val) - (Ord('A') - 10);
        else
          Exit(0);
      end;
      Inc(P);

      Result := Result*16+N;
    end;
  end;

begin
  if (Length(Str) >= DebugLnFlagsHeaderLength) and (Str[1] = DebugLnFlagsHeader[1]) and (Str[2] = DebugLnFlagsHeader[2]) then
  begin
    Result := EDebugLnFlags(HexToInt(@Str[3]));

    Delete(Str, 1, DebugLnFlagsHeaderLength);
  end else
    Result := [];
end;

function InRange(const AValue, AMin, AMax: Integer): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Int64): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Single): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Double): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function Min(const A, B: Int64): Int64;
begin
  Result := specialize Min<Int64>(A, B);
end;

function Max(const A, B: Int64): Int64;
begin
  Result := specialize Max<Int64>(A, B);
end;

function Min(const A, B: Single): Single;
begin
  Result := specialize Min<Single>(A, B);
end;

function Max(const A, B: Single): Single;
begin
  Result := specialize Max<Single>(A, B);
end;

function Min(const A, B: Double): Double;
begin
  Result := specialize Min<Double>(A, B);
end;

function Max(const A, B: Double): Double;
begin
  Result := specialize Max<Double>(A, B);
end;

procedure Swap(var A, B: Byte);
begin
  specialize Swap<Byte>(A, B);
end;

procedure Swap(var A, B: Integer);
begin
  specialize Swap<Integer>(A, B);
end;

procedure Swap(var A, B: Single);
begin
  specialize Swap<Single>(A, B);
end;

procedure Swap(var A, B: Double);
begin
  specialize Swap<Double>(A, B);
end;

procedure Swap(var A, B: Pointer);
begin
  specialize Swap<Pointer>(A, B);
end;

procedure Swap(var A, B: TPoint);
begin
  specialize Swap<TPoint>(A, B);
end;

procedure Swap(var A, B: TColorBGRA);
begin
  specialize Swap<TColorBGRA>(A, B);
end;

function IfThen(const Val: Boolean; const IfTrue, IfFalse: String): String;
begin
  Result := specialize IfThen<String>(Val, IfTrue, IfFalse);
end;

function IfThen(const Val: Boolean; const IfTrue, IfFalse: Int64): Int64;
begin
  Result := specialize IfThen<Int64>(Val, IfTrue, IfFalse);
end;

function IfThen(const Val: Boolean; const IfTrue, IfFalse: Boolean): Boolean;
begin
  Result := specialize IfThen<Boolean>(Val, IfTrue, IfFalse);
end;

generic function Min<_T>(const A, B: _T): _T;
begin
  if (A < B) then
    Result := A
  else
    Result := B;
end;

generic function Max<_T>(const A, B: _T): _T;
begin
  if (A > B) then
    Result := A
  else
    Result := B;
end;

generic procedure Swap<_T>(var A, B: _T);
var
  C: _T;
begin
  C := A;

  A := B;
  B := C;
end;

generic procedure MoveElement<_T>(var Arr: specialize TArray<_T>; AFrom, ATo: Integer);
var
  Item: _T;
begin
  if (Length(Arr) = 0) then
    SimbaException('MoveElement: Empty array');
  AFrom := EnsureRange(AFrom, Low(Arr), High(Arr));
  ATo := EnsureRange(ATo, Low(Arr), High(Arr));
  Item := Arr[AFrom];

  if (ATo > AFrom) then
    Move(Arr[AFrom + 1], Arr[AFrom], (ATo - AFrom) * SizeOf(_T))
  else
    Move(Arr[ATo], Arr[ATo + 1], (AFrom - ATo) * SizeOf(_T));

  Move(Item, Arr[ATo], SizeOf(_T));
end;

generic function Sum<_T, _R>(var AValues: specialize TArray<_T>): _R;
var
  I: Integer;
begin
  Result := Default(_R);
  for I := 0 to High(AValues) do
    Result := Result + AValues[I];
end;

generic procedure Reverse<_T>(var Arr: specialize TArray<_T>);
var
  tmp: _T;
  Lo,Hi: ^_T;
begin
  if Length(Arr) > 0 then
  begin
    Lo := @Arr[0];
    Hi := @Arr[High(Arr)];
    while (PtrUInt(Lo) < PtrUInt(Hi)) do
    begin
      tmp := Hi^;
      Hi^ := Lo^;
      Lo^ := tmp;
      Dec(Hi);
      Inc(Lo);
    end;
  end;
end;

generic function Reversed<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
var
  Lo: PtrUInt;
  r, p: ^_T;
begin
  SetLength(Result, Length(Arr));

  if Length(Arr) > 0 then
  begin
    p := @Arr[High(Arr)];
    r := @Result[0];

    Lo := PtrUInt(@Arr[0]);
    while (Lo <= PtrUInt(p)) do
    begin
      r^ := p^;
      Dec(p);
      Inc(r);
    end;
  end;
end;

generic function Equals<_T>(const A, B: specialize TArray<_T>): Boolean;
var
  VarType: (OTHER, SINGLE, DOUBLE);

  function IsEquals(constref A, B: _T): Boolean; inline;
  begin
    case VarType of
      SINGLE: Result := SameValue(PSingle(@A)^, PSingle(@B)^);
      DOUBLE: Result := SameValue(PDouble(@A)^, PDouble(@B)^);
      OTHER:  Result := (A = B);
    end;
  end;

var
  I: Integer;
begin
  if (Length(A) <> Length(B)) then
    Exit(False);
  if (Length(A) = 0) and (Length(B) = 0) then
    Exit(True);

  case GetTypeData(TypeInfo(_T))^.FloatType of
    ftSingle: VarType := SINGLE;
    ftDouble: VarType := DOUBLE;
    else
      VarType := OTHER;
  end;

  for I := 0 to High(A) do
    if not IsEquals(A[I], B[I]) then
      Exit(False);

  Result := True;
end;

generic function IndexOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
var
  VarType: (OTHER, SINGLE, DOUBLE);

  function IsEquals(constref A, B: _T): Boolean; inline;
  begin
    case VarType of
      SINGLE: Result := SameValue(PSingle(@A)^, PSingle(@B)^);
      DOUBLE: Result := SameValue(PDouble(@A)^, PDouble(@B)^);
      OTHER:  Result := (A = B);
    end;
  end;

var
  I: Integer;
begin
  Result := -1;
  if (Length(Arr) = 0) then
    Exit;

  case GetTypeData(TypeInfo(_T))^.FloatType of
    ftSingle: VarType := SINGLE;
    ftDouble: VarType := DOUBLE;
    else
      VarType := OTHER;
  end;

  for I := 0 to High(Arr) do
    if IsEquals(Arr[I], Item) then
      Exit(I);
end;

generic function IndicesOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;
var
  VarType: (OTHER, SINGLE, DOUBLE);

  function IsEquals(constref A, B: _T): Boolean; inline;
  begin
    case VarType of
      SINGLE: Result := SameValue(PSingle(@A)^, PSingle(@B)^);
      DOUBLE: Result := SameValue(PDouble(@A)^, PDouble(@B)^);
      OTHER:  Result := (A = B);
    end;
  end;

var
  I, Count: Integer;
begin
  Result := [];
  if (Length(Arr) = 0) then
    Exit;

  case GetTypeData(TypeInfo(_T))^.FloatType of
    ftSingle: VarType := SINGLE;
    ftDouble: VarType := DOUBLE;
    else
      VarType := OTHER;
  end;

  SetLength(Result, 8);
  Count := 0;

  for I := 0 to High(Arr) do
    if IsEquals(Arr[I], Item) then
    begin
      if (Count >= Length(Result)) then
        SetLength(Result, Length(Result) * 2);
      Result[Count] := I;
      Inc(Count);
    end;

  SetLength(Result, Count);
end;

generic function MinA<_T>(const Arr: specialize TArray<_T>): _T;
var
  I: Integer;
begin
  if (Length(Arr) = 0) then
    Result := Default(_T)
  else
    Result := Arr[0];

  for I := 1 to High(Arr) do
    if (Arr[I] < Result) then
      Result := Arr[I];
end;

generic function MaxA<_T>(const Arr: specialize TArray<_T>): _T;
var
  I: Integer;
begin
  if (Length(Arr) = 0) then
    Result := Default(_T)
  else
    Result := Arr[0];

  for I := 1 to High(Arr) do
    if (Arr[I] > Result) then
      Result := Arr[I];
end;

function TBooleanHelper.ToString: String;
begin
  if Self then
    Result := 'True'
  else
    Result := 'False';
end;

end.

