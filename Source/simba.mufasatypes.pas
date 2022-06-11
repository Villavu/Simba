{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.mufasatypes;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics;

type
  {$SCOPEDENUMS ON}
  ESimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING, STATE_NONE);

  ESimbaCommunicationMessage = (
    STATUS, DISGUSE,
    SIMBA_PID, SIMBA_TARGET_PID, SIMBA_TARGET_WINDOW,
    SCRIPT_ERROR, SCRIPT_STATE_CHANGE,
    BALLOON_HINT,
    DEBUGIMAGE_ZOOM, DEBUGIMAGE_MOVETO, DEBUGIMAGE_MAXSIZE, DEBUGIMAGE_DRAW, DEBUGIMAGE_SHOW, DEBUGIMAGE_HIDE,
    DEBUGIMAGE_DISPLAY, DEBUGIMAGE_CLEAR,
    DEBUG_METHOD_NAME, DEBUG_EVENTS
  );

  PSimbaScriptDebuggerEvent = ^TSimbaScriptDebuggerEvent;
  TSimbaScriptDebuggerEvent = packed record
    Method: Int16;
    Depth: Int16;
    Exception: Boolean;
  end;
  TSimbaScriptDebuggerEvents = array of TSimbaScriptDebuggerEvent;
  {$SCOPEDENUMS OFF}

  TRGB32 = packed record
    function ToString: String;

    function Equals(const Other: TRGB32): Boolean; inline;
    function EqualsIgnoreAlpha(const Other: TRGB32): Boolean; inline;

    case Byte of
      0: (B, G, R, A: Byte);
      1: (AsInteger: Integer);
  end;

  PRGB32 = ^TRGB32;
  PRGB32Array = ^TRGB32Array;
  TRGB32Array = array of TRGB32;

  PPRGB32 = ^PRGB32;
  PPRGB32Array = ^TPRGB32Array;
  TPRGB32Array = array of PRGB32;

  TRetData = record
    Ptr: PRGB32;
    IncPtrWith: Integer;
    RowLen: Integer;
  end;
  PRetData = ^TRetData;

  PWindowHandle = ^TWindowHandle;
  PWindowHandleArray = ^TWindowHandleArray;

  TWindowHandle = type PtrUInt;
  TWindowHandleArray = array of TWindowHandle;

const
  NullReturnData: TRetData = (Ptr: nil; IncPtrWith: -1; RowLen: -1);

operator =(Left, Right: TRetData): Boolean; inline;

type
  TClickType = (
    MOUSE_RIGHT,
    MOUSE_LEFT,
    MOUSE_MIDDLE,
    MOUSE_EXTRA_1,
    MOUSE_EXTRA_2,
    MOUSE_SCROLL_UP,
    MOUSE_SCROLL_DOWN
  );
  PClickType = ^TClickType;

  TMousePress = (MOUSE_DOWN, MOUSE_UP);

  PStringArray = ^TStringArray;
  TStringArray = array of String;
  P2DStringArray = ^T2DStringArray;
  T2DStringArray = array of TStringArray;

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

  PExtendedArray = ^TExtendedArray;
  TExtendedArray = array of Extended;
  P2DExtendedArray = ^T2DExtendedArray;
  T2DExtendedArray = array of TExtendedArray;
  PExtendedMatrix = ^TExtendedMatrix;
  TExtendedMatrix = array of TExtendedArray;

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

  TComplex = packed record
    Re, Im: Single;
  end;

  TComplexArray   = array of TComplex;
  T2DComplexArray = array of TComplexArray;

  TComplexMatrix = T2DComplexArray;

  EComparator = (__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__);
  PComparator = ^EComparator;

  { File types }
  TMufasaFile = record
    Path: String;
    FS: TFileStream;
    BytesRead, Mode: Integer;
  end;
  TMufasaFilesArray = array of TMufasaFile;

  PBox = ^TBox;
  TBox = record
    X1, Y1, X2, Y2: Integer;
  end;
  PBoxArray = ^TBoxArray;
  TBoxArray = array of TBox;

  TBoxHelper = record Helper for TBox
  protected
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    class function Create(const X1, Y1, X2, Y2: Integer): TBox; static;

    function Area: Integer;
    function Expand(Amount: Integer): TBox;
    function Contains(X, Y, Width, Height: Integer): Boolean; overload;
    function Contains(X, Y: Integer): Boolean; overload;

    procedure Clip(Other: TBox);

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  function Box(X1, Y1, X2, Y2: Integer): TBox;
  function Box(Mid: TPoint; XRad, YRad: Integer): TBox;

const
  TMDTMPointSize = 5*SizeOf(integer)+Sizeof(boolean);
type
  TMDTMPoint = record //TMufasaDTMPoint
    x,y,c,t,asz : integer;
    bp : boolean;
  end;

  PMDTMPoint = ^TMDTMPoint; //PointerMufasaDTMPoint
  TMDTMPointArray = array of TMDTMPoint; //TMufasaDTMPointArray

  TSDTMPointDef = record
    x, y, Color, Tolerance, AreaSize, AreaShape: integer;
  end;

  TSDTMPointDefArray = array of TSDTMPointDef;

  PSDTM = ^TSDTM;
  TSDTM = record
    MainPoint: TSDTMPointDef;
    SubPoints: TSDTMPointDefArray;
  end;

  TOCRFilterData = packed record
      _type: integer;
      is_text_color: boolean;

      r_low,r_high,g_low,g_high,b_low,b_high,set_col: integer;

      ref_color,tol,cts: integer;
  end;

  POCRFilterData = ^TOCRFilterData;

  TOcrFilterDataArray = array of TOCRFilterData;
  POcrFilterDataArray = ^TOCRFilterDataArray;

procedure Swap(var A, B: Byte); overload;
procedure Swap(var A, B: Integer); overload;
procedure Swap(var A, B: Extended); overload;
procedure Swap(var A, B: TPoint); overload;
procedure Swap(var A, B: Pointer); overload;
procedure Swap(var A, B: TRGB32); overload;

generic procedure Swap<T>(var A, B: T);

type
  TSyncNested = procedure is nested;
  TThreadedNested = procedure is nested;

  TThreadedMethod = procedure of object;
  TThreadedMethodArray = array of TThreadedMethod;

procedure Sync(Method: TSyncNested);
function Threaded(Method: TThreadedNested): TThread;
procedure Threaded(Methods: TThreadedMethodArray; Interval: Integer = 0);

operator = (Left, Right: TPoint): Boolean;
operator = (Left, Right: TBox): Boolean;

implementation

operator = (Left, Right: TPoint): Boolean;
begin
  Result := (Left.x = Right.x) and (Left.y = Right.y);
end;

operator = (Left, Right: TBox): Boolean;
begin
  Result := (Left.x1 = Right.x1) and (Left.y1 = Right.y1) and (Left.x2 = Right.x2) and (Left.y2 = Right.y2);
end;

function TRGB32.ToString: String;
begin
  Result := '[' + IntToStr(B) + ',' + IntToStr(G) + ',' + IntToStr(R) + ',' + IntToStr(A) + ']';
end;

function TRGB32.Equals(const Other: TRGB32): Boolean;
begin
  Result := AsInteger = Other.AsInteger;
end;

function TRGB32.EqualsIgnoreAlpha(const Other: TRGB32): Boolean;
begin
  Result := (AsInteger and $FFFFFF) = (Other.AsInteger and $FFFFFF);
end;

operator =(Left, Right: TRetData): Boolean;
begin
  Result := (Left.Ptr = Right.Ptr) and (Left.RowLen = Right.RowLen) and (Left.IncPtrWith = Right.IncPtrWith);
end;

function TBoxHelper.GetWidth: Integer;
begin
  Result := (Self.X2 - Self.X1) + 1;
end;

function TBoxHelper.GetHeight: Integer;
begin
  Result := (Self.Y2 - Self.Y1) + 1;
end;

class function TBoxHelper.Create(const X1, Y1, X2, Y2: Integer): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function TBoxHelper.Area: Integer;
begin
  Result := (Width * Height);
end;

function TBoxHelper.Expand(Amount: Integer): TBox;
begin
  Result.X1 := Self.X1 - Amount;
  Result.Y1 := Self.Y1 - Amount;
  Result.X2 := Self.X2 + Amount;
  Result.Y2 := Self.Y2 + Amount;
end;

function TBoxHelper.Contains(X, Y, Width, Height: Integer): Boolean;
begin
  Result := (X >= Self.X1) and (Y >= Self.Y1) and (X + Width <= Self.X2) and (Y + Height <= Self.Y2);
end;

function TBoxHelper.Contains(X, Y: Integer): Boolean;
begin
  Result := (X >= Self.X1) and (Y >= Self.Y1) and (X <= Self.X2) and (Y <= Self.Y2);
end;

procedure TBoxHelper.Clip(Other: TBox);
begin
  if (Self.X1 < Other.X1) then Self.X1 := Other.X1;
  if (Self.X1 > Other.X2) then Self.X1 := Other.X2;
  if (Self.X2 < Other.X1) then Self.X2 := Other.X1;
  if (Self.X2 > Other.X2) then Self.X2 := Other.X2;

  if (Self.Y1 < Other.Y1) then Self.Y1 := Other.Y1;
  if (Self.Y1 > Other.Y2) then Self.Y1 := Other.Y2;
  if (Self.Y2 < Other.Y1) then Self.Y2 := Other.Y1;
  if (Self.Y2 > Other.Y2) then Self.Y2 := Other.Y2;
end;

generic procedure Swap<T>(var A, B: T);
var
  C: T;
begin
  C := A;

  A := B;
  B := C;
end;

function Box(X1, Y1, X2, Y2: Integer): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function Box(Mid: TPoint; XRad, YRad: Integer): TBox;
begin
  Result.X1 := Mid.X-XRad;
  Result.Y1 := Mid.Y-YRad;
  Result.X2 := Mid.X+XRad;
  Result.Y2 := Mid.Y+YRad;
end;

procedure Swap(var A, B: Byte);
begin
  specialize Swap<Byte>(A, B);
end;

procedure Swap(var A, B: Integer);
begin
  specialize Swap<Integer>(A, B);
end;

procedure Swap(var A, B: Extended);
begin
  specialize Swap<Extended>(A, B);
end;

procedure Swap(var A, B: TPoint);
begin
  specialize Swap<TPoint>(A, B);
end;

procedure Swap(var A, B: Pointer);
begin
  specialize Swap<Pointer>(A, B);
end;

procedure Swap(var A, B: TRGB32);
begin
  specialize Swap<TRGB32>(A, B);
end;

type
  TSyncObject = object
    Proc: TSyncNested;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
begin
  Proc();
end;

procedure Sync(Method: TSyncNested);
var
  SyncObject{%H-}: TSyncObject;
begin
  SyncObject.Proc := Method;

  TThread.Synchronize(nil, @SyncObject.Execute);
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TThreadedMethod;
    FNestedProc: TThreadedNested;

    procedure Execute; override;
  public
    constructor Create(Proc: TThreadedNested); reintroduce;
    constructor Create(Proc: TThreadedMethod); reintroduce;
  end;

procedure TThreaded.Execute;
begin
  if Assigned(FNestedProc) then FNestedProc();
  if Assigned(FProc) then FProc();
end;

constructor TThreaded.Create(Proc: TThreadedNested);
begin
  inherited Create(False, 512*512);

  FNestedProc := Proc;
end;

constructor TThreaded.Create(Proc: TThreadedMethod);
begin
  inherited Create(False, 512*512);

  FProc := Proc;
end;

function Threaded(Method: TThreadedNested): TThread;
begin
  Result := TThreaded.Create(Method);
end;

procedure Threaded(Methods: TThreadedMethodArray; Interval: Integer);
var
  Threads: array of TThread;
  I: Integer;
begin
  SetLength(Threads, Length(Methods));
  for I := 0 to High(Threads) do
  begin
    Threads[I] := TThreaded.Create(Methods[I]);
    if (Interval > 0) then
      Sleep(Interval);
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor();
    Threads[I].Free();
  end;
end;

end.

