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
  PPRGB32 = ^PRGB32; // Pointer to PRGB32
  PRGB32 = ^TRGB32;
  TRGB32 = packed record
    B, G, R, A: Byte;

    class operator = (const Left, Right: TRGB32): Boolean; inline;
  end;

  PRGB32Array = ^TRGB32Array;
  TRGB32Array = array of TRGB32;  // array of TRGB32

  PPRGB32Array = ^TPRGB32Array;
  TPRGB32Array = array of PRGB32; // array of PRGB32

  PRetData = ^TRetData;
  TRetData = record
    Ptr: PRGB32;
    IncPtrWith: Integer;
    RowLen: Integer;
  end;

  PWindowHandle = ^TWindowHandle;
  PWindowHandleArray = ^TWindowHandleArray;

  TWindowHandle = type PtrUInt;
  TWindowHandleArray = array of TWindowHandle;

type
  PClickType = ^TClickType;
  TClickType = (
    MOUSE_RIGHT,
    MOUSE_LEFT,
    MOUSE_MIDDLE,
    MOUSE_EXTRA_1,
    MOUSE_EXTRA_2,
    MOUSE_SCROLL_UP,
    MOUSE_SCROLL_DOWN
  );

  PStringArray = ^TStringArray;
  TStringArray = array of String;
  P2DStringArray = ^T2DStringArray;
  T2DStringArray = array of TStringArray;

  PPoint = ^TPoint;

  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;
  P2DPointArray = ^T2DPointArray;
  T2DPointArray = array of TPointArray;

  // Int64
  PInt64Array = ^TInt64Array;
  TInt64Array = array of Int64;

  // Byte
  PByteArray = ^TByteArray;
  TByteArray = array of Byte;

  PByteMatrix = ^TByteMatrix;
  TByteMatrix = array of array of Byte;

  // Boolean
  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array of Boolean;

  PBooleanMatrix = ^TBooleanMatrix;
  TBooleanMatrix = array of array of Boolean;

  // Integer
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array of Integer;

  P2DIntegerArray = ^T2DIntegerArray;
  T2DIntegerArray = array of TIntegerArray;

  PIntegerMatrix = ^TIntegerMatrix;
  TIntegerMatrix = array of array of Integer;

  // Single
  PSingleArray = ^TSingleArray;
  TSingleArray = array of Single;

  PSingleMatrix = ^TSingleMatrix;
  TSingleMatrix = array of array of Single;

  // Double
  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array of Double;

  PDoubleMatrix = ^TDoubleMatrix;
  TDoubleMatrix = array of array of Double;

  // Extended
  PExtendedArray = ^TExtendedArray;
  TExtendedArray = array of Extended;

  P2DExtendedArray = ^T2DExtendedArray;
  T2DExtendedArray = array of TExtendedArray;

  PExtendedMatrix = ^TExtendedMatrix;
  TExtendedMatrix = array of array of Extended;

  // Complex
  TComplex = packed record
    Re, Im: Single;
  end;
  PComplexArray = ^TComplexArray;
  TComplexArray = array of TComplex;

  PComplexMatrix = ^TComplexMatrix;
  TComplexMatrix = array of array of TComplex;

  PComparator = ^EComparator;
  EComparator = (__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__);

  PMask = ^TMask;
  TMask = record
    White, Black : TPointArray;
    WhiteHi,BlackHi : integer;
    W,H : integer;
  end;

  TMufasaFile = record
    Path: String;
    FS: TFileStream;
    BytesRead, Mode: Integer;
  end;
  TMufasaFilesArray = array of TMufasaFile;

  TRegExprMatch = record
    Position: Integer;
    Length: Integer;
    Match: String;
  end;
  TRegExprMatchArray = array of TRegExprMatch;

  PRectangle = ^TRectangle;
  TRectangle = record
    Top, Right, Btm, Left: TPoint;
  end;
  PRectangleArray = ^TRectangleArray;
  TRectangleArray = array of TRectangle;

  PBox = ^TBox;
  TBox = record
    X1, Y1, X2, Y2: Integer;
  end;
  PBoxArray = ^TBoxArray;
  TBoxArray = array of TBox;

  TBoxHelper = record helper for TBox
  public
    function Width: Integer;
    function Height: Integer;
    function Middle: TPoint;

    function Area: Integer;
    function Offset(X, Y: Integer): TBox; overload;
    function Offset(P: TPoint): TBox; overload;

    function Contains(X, Y: Integer): Boolean; overload;
    function Contains(P: TPoint): Boolean; overload;
    function Contains(B: TBox): Boolean; overload;

    function Expand(Size: Integer): TBox; overload;
    function Expand(Size: Integer; MaxBounds: TBox): TBox; overload;

    function Expand(WidthMod, HeightMod: Integer): TBox; overload;
    function Expand(WidthMod, HeightMod: Integer; MaxBounds: TBox): TBox; overload;

    function Combine(Other: TBox): TBox;
    function ToRectangle: TRectangle;

    procedure Clip(Other: TBox);
    procedure Normalize;
  end;

  TRectangleHelper = record helper for TRectangle
  public
    function ToTPA: TPointArray;
    function Bounds: TBox;
    function Middle: TPoint;
    function Offset(X, Y: Integer): TRectangle; overload;
    function Offset(P: TPoint): TRectangle; overload;
    function Expand(Amount: Integer): TRectangle;
    function Contains(X, Y: Integer): Boolean; overload;
    function Contains(P: TPoint): Boolean; overload;
    function Contains(Box: TBox): Boolean; overload;
    function LongSideLen: Integer;
    function ShortSideLen: Integer;
    function Area: Integer;
    function Rotate(Radians: Double): TRectangle;
    function RotateFast(Degrees: Integer): TRectangle;

    procedure Normalize;
  end;

  TPointHelper = record helper for TPoint
  public
    function Offset(X, Y: Integer): TPoint; overload;
    function Offset(P: TPoint): TPoint; overload;
    function Rotate(Radians: Double; Center: TPoint): TPoint;
    function RotateFast(Degrees: Integer; Center: TPoint): TPoint;
    function DistanceTo(Other: TPoint): Integer;
    function AngleBetween(Other: TPoint): Double;
    function InBox(B: TBox): Boolean;
    function InCircle(Center: TPoint; Radius: Integer): Boolean;
    function InRect(R: TRectangle): Boolean;
    function Scale(Radians: Double; Radius: Integer): TPoint;
  end;

  TStringHelper = type helper for String
  public
    class function NumberChars: String; static;
    class function AlphaChars: String; static;

    function Upper: String;
    function Lower: String;

    function Before(const Value: String): String;
    function After(const Value: String): String;

    function Between(const S1, S2: String): String;
    function BetweenAll(const S1, S2: String): TStringArray;

    function RegExprSplit(const Pattern: String): TStringArray;
    function RegExprFindAll(const Pattern: String): TRegExprMatchArray;
    function RegExprFind(const Pattern: String): TRegExprMatch;

    function IndexOfAny(const Values: TStringArray): Integer; overload;
    function IndexOfAny(const Values: TStringArray; Offset: Integer): Integer; overload;

    function IndexOf(const Value: String): Integer; overload;
    function IndexOf(const Value: String; Offset: Integer): Integer; overload;
    function LastIndexOf(const Value: String): Integer; overload;
    function LastIndexOf(const Value: String; Offset: Integer): Integer; overload;

    function IndicesOf(const Value: String): TIntegerArray;
    function IndicesOf(const Value: String; Offset: Integer): TIntegerArray;

    function Extract(const Characters: String): String;
    function ExtractNumber(Default: Int64 = -1): Int64; overload;
    function ExtractNumber(const ANumberChars: String; Default: Int64 = -1): Int64; overload;

    function Trim: String; overload;
    function Trim(const TrimChars: array of Char): String; overload;

    function TrimLeft: String; overload;
    function TrimLeft(const TrimChars: array of Char): String; overload;

    function TrimRight: String; overload;
    function TrimRight(const TrimChars: array of Char): String; overload;

    function StartsWith(const Value: String; CaseSenstive: Boolean = True): Boolean;
    function EndsWith(const Value: String; CaseSenstive: Boolean = True): Boolean;

    function Replace(const OldValue: String; const NewValue: String): String; overload;
    function Replace(const OldValue: String; const NewValue: String; ReplaceFlags: TReplaceFlags): String; overload;

    function Contains(const Value: String; CaseSenstive: Boolean = True): Boolean;
    function ContainsAny(const Values: TStringArray; CaseSenstive: Boolean = True): Boolean;

    function Split(const Seperator: String): TStringArray;

    function Copy: String; overload;
    function Copy(StartIndex, Count: Integer): String; overload;
    function Copy(StartIndex: Integer): String; overload;
    function CopyRange(StartIndex, EndIndex: Integer): String;

    procedure Delete(StartIndex, Count: Integer);
    procedure DeleteRange(StartIndex, EndIndex: Integer);

    procedure Insert(const Value: String; Index: Integer);

    function PadLeft(Count: Integer; PaddingChar: Char = #32): String; overload;
    function PadRight(Count: Integer; PaddingChar: Char = #32): String; overload;
  end;

  function Box(const X1, Y1, X2, Y2: Integer): TBox; inline;
  function Point(const X, Y: Integer): TPoint; inline;

  procedure Swap(var A, B: Byte); overload;
  procedure Swap(var A, B: Integer); overload;
  procedure Swap(var A, B: Extended); overload;
  procedure Swap(var A, B: TPoint); overload;
  procedure Swap(var A, B: Pointer); overload;

  generic procedure Swap<T>(var A, B: T);

implementation

uses
  math, uregexpr, strutils,
  simba.math, simba.tpa, simba.geometry;

{$i simba.stringhelper_body.inc}
{$i simba.pointhelper_body.inc}
{$i simba.boxhelper_body.inc}
{$i simba.rectanglehelper_body.inc}

class operator TRGB32.=(const Left, Right: TRGB32): Boolean;
begin
  Result := Int32(Left) = Int32(Right);
end;

function Point(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Box(const X1, Y1, X2, Y2: Integer): TBox;
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

end.

