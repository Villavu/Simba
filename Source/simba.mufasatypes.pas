{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.mufasatypes;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, fpjson;

{$PUSH}
{$SCOPEDENUMS ON}

type
  KeyCode = (
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

  MouseButton = (
    LEFT,
    RIGHT,
    MIDDLE,
    SCROLL_UP,
    SCROLL_DOWN
  );

  PKeyCode = ^KeyCode;
  PMouseButton = ^MouseButton;

  ESimbaProcessType = (UNKNOWN, IDE, SCRIPT, SCRIPT_WITH_COMMUNICATION);
  ESimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING, STATE_NONE);

  ESimbaCommunicationMessage = (
    STATUS, DISGUSE,
    SIMBA_PID, SIMBA_TARGET_PID, SIMBA_TARGET_WINDOW,
    SCRIPT_ERROR, SCRIPT_STATE_CHANGE,
    TRAY_NOTIFICATION,
    DEBUGIMAGE_UPDATE,
    DEBUGIMAGE_MOVETO, DEBUGIMAGE_MAXSIZE, DEBUGIMAGE_SHOW, DEBUGIMAGE_HIDE,
    DEBUGIMAGE_DISPLAY, DEBUGIMAGE_DISPLAY_XY,
    DEBUG_METHOD_NAME, DEBUG_EVENTS
  );

  PSimbaScriptDebuggerEvent = ^TSimbaScriptDebuggerEvent;
  TSimbaScriptDebuggerEvent = packed record
    Method: Int16;
    Depth: Int16;
    Exception: Boolean;
  end;
  TSimbaScriptDebuggerEvents = array of TSimbaScriptDebuggerEvent;

{$POP}

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

  TColorARGB = packed record
  case Byte of
    0: (A, R, G, B: Byte);
    1: (AsInteger: Integer);
  end;
  PColorARGB = ^TColorARGB;

  TColorBGRA = packed record
  case Byte of
    0: (B, G, R, A: Byte);
    1: (AsInteger: Integer);
  end;
  TColorBGRAArray = array of TColorBGRA;

  PColorBGRA = ^TColorBGRA;
  PColorBGRAArray = array of PColorBGRA;

  PColorRGB = ^TColorRGB;
  PColorXYZ = ^TColorXYZ;
  PColorLAB = ^TColorLAB;
  PColorLCH = ^TColorLCH;
  PColorHSV = ^TColorHSV;
  PColorHSL = ^TColorHSL;

  TColorArray = array of TColor;

  TRetData = record
    Ptr: PColorBGRA;
    IncPtrWith: Integer;
    RowLen: Integer;
  end;
  PRetData = ^TRetData;

  PWindowHandle = ^TWindowHandle;
  PWindowHandleArray = ^TWindowHandleArray;

  TWindowHandle = type UInt64;
  TWindowHandleArray = array of TWindowHandle;

type
  PTMFormula = ^ETMFormula;
  ETMFormula = (
    TM_CCORR,
    TM_CCORR_NORMED,
    TM_CCOEFF,
    TM_CCOEFF_NORMED,
    TM_SQDIFF,
    TM_SQDIFF_NORMED
  );

  PStringArray = ^TStringArray;

  PPoint = ^TPoint;
  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;
  P2DPointArray = ^T2DPointArray;
  T2DPointArray = array of TPointArray;

  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

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

  PBox = ^TBox;
  TBox = record
  case Integer of
    0: (X1, Y1, X2, Y2: Integer);
    1: (TopLeft, BottomRight: TPoint);
  end;
  PBoxArray = ^TBoxArray;
  TBoxArray = array of TBox;

  {$DEFINE HEADER}
    {$i generics.inc}
    {$i quad.inc}
    {$i box.inc}
    {$i boxarray.inc}
    {$i point.inc}
    {$i integermatrix.inc}
    {$i singlematrix.inc}
    {$i matrix.inc}
    {$i string.inc}
  {$UNDEF HEADER}

{$PUSH}
{$SCOPEDENUMS ON}
type
  EDebugLn = (
    CLEAR,
    YELLOW,
    RED,
    GREEN,
    FOCUS
  );
  EDebugLnFlags = set of EDebugLn;
{$POP}

var
  DoSimbaDebugLn: procedure(const S: String) of object;

procedure DebugLn(const Msg: String); overload;
procedure DebugLn(const Msg: String; Args: array of const); overload;

procedure SimbaDebugLn(const Flags: EDebugLnFlags; const Msg: String); overload;
procedure SimbaDebugLn(const Flags: EDebugLnFlags; const Msg: TStringArray); overload;

function FlagsToString(const Flags: EDebugLnFlags): String; inline;
function FlagsFromString(var Str: String): EDebugLnFlags; inline;

procedure AssertMainThread(const Method: String);

function Min(const A, B: Integer): Integer; inline; overload;
function Max(const A, B: Integer): Integer; inline; overload;
function Min(const A, B: Int64): Int64; inline; overload;
function Max(const A, B: Int64): Int64; inline; overload;
function Min(const A, B: Single): Single; inline; overload;
function Max(const A, B: Single): Single; inline; overload;
function Min(const A, B: Double): Double; inline; overload;
function Max(const A, B: Double): Double; inline; overload;

procedure Swap(var A, B: Byte); overload;
procedure Swap(var A, B: Integer); overload;
procedure Swap(var A, B: Single); overload;
procedure Swap(var A, B: Double); overload;
procedure Swap(var A, B: TPoint); overload;
procedure Swap(var A, B: Pointer); overload;
procedure Swap(var A, B: TColorBGRA); overload;

type
  TProc       = procedure of object;
  TProcArray  = array of TProc;

  TNestedProc      = procedure is nested;
  TNestedProcArray = array of TNestedProc;

procedure Sync(Method: TProc); overload;
procedure Sync(Method: TNestedProc); overload;

function Threaded(Method: TNestedProc): TThread; overload;
function Threaded(Method: TProc): TThread; overload;
procedure Threaded(Methods: TProcArray; Interval: Integer = 0); overload;

procedure ThreadedAndForget(Method: TNestedProc);

// Writable const
const
  SimbaProcessType: ESimbaProcessType = ESimbaProcessType.UNKNOWN;

implementation

uses
  math, forms, lazloggerbase, uregexpr, strutils, jsonparser, jsonscanner,
  simba.math, simba.overallocatearray, simba.geometry, simba.heaparray,
  simba.algo_sort, simba.tpa, simba.random;

{$DEFINE BODY}
  {$i generics.inc}
  {$i quad.inc}
  {$i box.inc}
  {$i boxarray.inc}
  {$i point.inc}
  {$i integermatrix.inc}
  {$i singlematrix.inc}
  {$i matrix.inc}
  {$i string.inc}
{$UNDEF BODY}

procedure DebugLn(const Msg: String);
begin
  DebugLogger.DebugLn(Msg);
end;

procedure DebugLn(const Msg: String; Args: array of const);
begin
  DebugLogger.DebugLn(Msg, Args);
end;

procedure AssertMainThread(const Method: String);
begin
  if (GetCurrentThreadID() <> MainThreadID) then
    raise Exception('Not called on main thread: ' + Method);
end;

procedure SimbaDebugLn(const Msg: String);
begin
  DebugLn(Msg);
end;

procedure SimbaDebugLn(const Msg: String; Args: array of const);
begin
  DebugLn(Msg.Format(Args));
end;

const
  DebugLnFlagsHeader       = String(#0#0);
  DebugLnFlagsHeaderLength = Length(DebugLnFlagsHeader) + 6;

function FlagsToString(const Flags: EDebugLnFlags): String;
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

procedure SimbaDebugLn(const Flags: EDebugLnFlags; const Msg: String);
begin
  DoSimbaDebugLn(FlagsToString(Flags) + Msg);
end;

procedure SimbaDebugLn(const Flags: EDebugLnFlags; const Msg: TStringArray);
begin
  DoSimbaDebugLn(FlagsToString(Flags) + LineEnding.Join(Msg));
end;

function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Int64): Int64;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Int64): Int64;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Double): Double;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
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

procedure Swap(var A, B: TPoint);
begin
  specialize Swap<TPoint>(A, B);
end;

procedure Swap(var A, B: Pointer);
begin
  specialize Swap<Pointer>(A, B);
end;

procedure Swap(var A, B: TColorBGRA);
begin
  specialize Swap<TColorBGRA>(A, B);
end;

type
  TSyncObject = object
    Proc: TProc;
    NestedProc: TNestedProc;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
begin
  try
    if Assigned(Proc)       then Proc();
    if Assigned(NestedProc) then NestedProc();
  except
    on E: Exception do
      DebugLn('Sync :: ' + E.Message);
  end;
end;

procedure Sync(Method: TProc);
var
  SyncObject: TSyncObject;
begin
  if (GetCurrentThreadID() <> MainThreadID) then
  begin
    SyncObject.Proc       := Method;
    SyncObject.NestedProc := nil;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

procedure Sync(Method: TNestedProc);
var
  SyncObject: TSyncObject;
begin
  if (GetCurrentThreadID() <> MainThreadID) then
  begin
    SyncObject.Proc       := nil;
    SyncObject.NestedProc := Method;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TProc;
    FNestedProc: TNestedProc;

    procedure DoTerminated(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create(Proc: TNestedProc; Forget: Boolean); reintroduce;
    constructor Create(Proc: TProc; Forget: Boolean); reintroduce;
  end;

procedure TThreaded.DoTerminated(Sender: TObject);
begin
  Flush(Output);
end;

procedure TThreaded.Execute;
begin
  if Assigned(FNestedProc) then FNestedProc();
  if Assigned(FProc)       then FProc();
end;

constructor TThreaded.Create(Proc: TNestedProc; Forget: Boolean);
begin
  inherited Create(False, 512*512);

  if Forget then
  begin
    FreeOnTerminate := True;
    OnTerminate := @DoTerminated;
  end;

  FNestedProc := Proc;
end;

constructor TThreaded.Create(Proc: TProc; Forget: Boolean);
begin
  inherited Create(False, 512*512);

  if Forget then
  begin
    FreeOnTerminate := True;
    OnTerminate := @DoTerminated;
  end;

  FProc := Proc;
end;

function Threaded(Method: TNestedProc): TThread;
begin
  Result := TThreaded.Create(Method, False);
end;

function Threaded(Method: TProc): TThread;
begin
  Result := TThreaded.Create(Method, False);
end;

procedure Threaded(Methods: TProcArray; Interval: Integer);
var
  Threads: array of TThread;
  I: Integer;
begin
  SetLength(Threads, Length(Methods));
  for I := 0 to High(Threads) do
  begin
    Threads[I] := TThreaded.Create(Methods[I], False);
    if (Interval > 0) then
      Sleep(Interval);
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor();
    Threads[I].Free();
  end;
end;

procedure ThreadedAndForget(Method: TNestedProc);
begin
  TThreaded.Create(Method, True);
end;

initialization
  DoSimbaDebugLn := @DebugLogger.DebugLn;

end.

