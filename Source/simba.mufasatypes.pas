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

  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R : byte;
  end;

  PPRGB32 = ^PRGB32; // Pointer to PRGB32
  PRGB32 = ^TRGB32;
  TRGB32 = packed record
    function ToString: String;

    function Equals(const Other: TRGB32): Boolean; inline;
    function EqualsIgnoreAlpha(const Other: TRGB32): Boolean; inline;

    case Byte of
      0: (B, G, R, A: Byte);
      1: (AsInteger: Integer);
  end;

  PRGB32Array = ^TRGB32Array;
  TRGB32Array = array of TRGB32;  // array of TRGB32

  PPRGB32Array = ^TPRGB32Array;
  TPRGB32Array = array of PRGB32; // array of PRGB32

  THSL = packed record
    H, S, L: extended;
  end;
  PHSL = ^THSL;

  PHSLArray = ^THSLArray;
  THSLArray = array of THSL;
  P2DHSLArray = ^T2DHSLArray;
  T2DHSLArray = array of array of THSL;

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
operator =(Left, Right: TRGB32): Boolean; inline;

type
  PStrings = ^TStrings;
  PFont = ^TFont;
  PColor = ^TColor;
  PCanvas = ^TCanvas;

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
  T2DIntegerArray = array of TIntegerArray;
  P2DIntegerArray = ^T2DIntegerArray;

  PByteArray = ^TByteArray;
  TByteArray = array of Byte;
  P2DByteArray = ^T2DByteArray;
  T2DByteArray = array of TByteArray;

  TByteMatrix = T2DByteArray;
  PByteMatrix = ^TByteMatrix;

  TBoolArray = array of boolean;
  TBooleanArray = TBoolArray;
  T2DBoolArray = array of TBoolArray;

  TBooleanMatrix = array of TBoolArray;
  PBooleanMatrix = ^TBooleanMatrix;

  PExtendedArray = ^TExtendedArray;
  TExtendedArray = array of Extended;
  P2DExtendedArray = ^T2DExtendedArray;
  T2DExtendedArray = array of array of Extended;

  PSingleArray  = ^TSingleArray;
  TSingleArray  = array of Single;
  PSingleMatrix = ^TSingleMatrix;
  TSingleMatrix = array of TSingleArray;

  TDoubleArray = array of Double;
  T2DDoubleArray = array of TDoubleArray;
  PDoubleArray = ^TDoubleArray;

  TDoubleMatrix = T2DDoubleArray;
  PDoubleMatrix = ^TDoubleMatrix;

  PIntegerMatrix = ^TIntegerMatrix;
  TIntegerMatrix = array of TIntegerArray;

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

  { Mask Types }
  PMask = ^TMask;
  TMask = record
    White, Black : TPointArray;
    WhiteHi,BlackHi : integer;
    W,H : integer;
  end;

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

  TBoxHelper = record Helper for TBox
  protected
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    class function Create(const X1, Y1, X2, Y2: Integer): TBox; static;

    function Expand(Amount: Integer): TBox;
    function Contains(X, Y, Width, Height: Integer): Boolean; overload;
    function Contains(X, Y: Integer): Boolean; overload;

    procedure Clip(Other: TBox);

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  function Box(const X1, Y1, X2, Y2: Integer): TBox;

type
  TSysProc = record
    Title: WideString;
    Handle: UInt32;
    PID: UInt32;
    Width, Height: integer;
  end;
  TSysProcArr = array of TSysProc;
  PSysProcArr = ^TSysProcArr;
  PSysProc = ^TSysProc;

const
  TMDTMPointSize = 5*SizeOf(integer)+Sizeof(boolean);
type
  TMDTMPoint = record //TMufasaDTMPoint
    x,y,c,t,asz : integer;
    bp : boolean;
  end;

  PMDTMPoint = ^TMDTMPoint; //PointerMufasaDTMPoint
  TMDTMPointArray = array of TMDTMPoint; //TMufasaDTMPointArray


  { Other DTM Types }

  TSDTMPointDef = record
    x, y, Color, Tolerance, AreaSize, AreaShape: integer;
  end;

  TSDTMPointDefArray = array of TSDTMPointDef;

  PSDTM = ^TSDTM;
  TSDTM = record
    MainPoint: TSDTMPointDef;
    SubPoints: TSDTMPointDefArray;
  end;

type
  VirtualKeyInfo = record
    Str : string;
    Key : byte;
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

procedure Sync(const Method: TSyncNested);

implementation

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

operator =(Left, Right: TRGB32): Boolean;
begin
  Result := Int32(Left) = Int32(Right);
end;

function TBoxHelper.GetWidth: Int32;
begin
  Result := (Self.X2 - Self.X1) + 1;
end;

function TBoxHelper.GetHeight: Int32;
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

function Box(const X1, Y1, X2, Y2: Int32): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

generic procedure Swap<T>(var A, B: T);
var
  C: T;
begin
  C := A;

  A := B;
  B := C;
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

procedure Sync(const Method: TSyncNested);
var
  SyncObject{%H-}: TSyncObject;
begin
  SyncObject.Proc := Method;

  TThread.Synchronize(nil, @SyncObject.Execute);
end;

end.

