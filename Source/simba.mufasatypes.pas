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
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R : byte;
  end;

  PPRGB32 = ^PRGB32;
  PRGB32 = ^TRGB32;
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;

  TARGB32 = packed record A, R, G, B: Byte; end;
  PARGB32 = ^TARGB32;

  PRGB32Array = ^TRGB32Array;
  TRGB32Array = array of TRGB32;

  PPRGB32Array = ^TPRGB32Array;
  TPRGB32Array = array of PRGB32; //array of Pointers

  THSL = packed record
    H, S, L: extended;
  end;
  PHSL = ^THSL;

  PHSLArray = ^THSLArray;
  THSLArray = array of THSL;
  P2DHSLArray = ^T2DHSLArray;
  T2DHSLArray = array of array of THSL;

  PRetData = ^TRetData;
  TRetData = record
    Ptr: PRGB32;
    IncPtrWith: Integer;
    RowLen: Integer;
  end;

const
  NullReturnData: TRetData = (Ptr: nil; IncPtrWith: -1; RowLen: -1);

operator =(Left, Right: TRetData): Boolean; inline;
operator =(Left, Right: TRGB32): Boolean; inline;

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

  // Boolean
  TBooleanArray = array of Boolean;
  T2DBooleanArray = array of TBooleanArray;

  PBooleanArray = ^TBooleanArray;
  P2DBooleanArray = ^T2DBooleanArray;

  // Single
  TSingleArray = array of Single;
  T2DSingleArray = array of TSingleArray;

  PSingleArray = ^TSingleArray;
  P2DSingleArray = ^T2DSingleArray;

  // Double
  TDoubleArray = array of Double;
  T2DDoubleArray = array of TDoubleArray;

  PDoubleArray = ^TDoubleArray;
  P2DDoubleArray = ^T2DDoubleArray;

  // Extended
  TExtendedArray = array of Extended;
  T2DExtendedArray = array of TExtendedArray;

  PExtendedArray = ^TExtendedArray;
  P2DExtendedArray = ^T2DExtendedArray;

  // Complex
  TComplex = packed record
    Re, Im: Single;
  end;
  TComplexArray = array of TComplex;
  T2DComplexArray = array of TComplexArray;

  PComplexArray = ^TComplexArray;
  P2DComplexArray = ^T2DComplexArray;

  PComparator = ^EComparator;
  EComparator = (__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__);

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

  { TBoxHelper }

  TBoxHelper = record Helper for TBox
  protected
    function GetWidth: Int32;
    function GetHeight: Int32;
  public
    function Expand(Amount: Int32): TBox;
    function Contains(X, Y, Width, Height: Int32): Boolean; overload;
    function Contains(X, Y: Int32): Boolean; overload;
    property Width: Int32 read GetWidth;
    property Height: Int32 read GetHeight;
  end;

  function Box(const X1, Y1, X2, Y2: Int32): TBox;

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

implementation

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

function TBoxHelper.Expand(Amount: Int32): TBox;
begin
  Result.X1 := Self.X1 - Amount;
  Result.Y1 := Self.Y1 - Amount;
  Result.X2 := Self.X2 + Amount;
  Result.Y2 := Self.Y2 + Amount;
end;

function TBoxHelper.Contains(X, Y, Width, Height: Int32): Boolean;
begin
  Result := (X >= Self.X1) and (Y >= Self.Y1) and (X + Width <= Self.X2) and (Y + Height <= Self.Y2);
end;

function TBoxHelper.Contains(X, Y: Int32): Boolean;
begin
  Result := (X >= Self.X1) and (Y >= Self.Y1) and (X <= Self.X2) and (Y <= Self.Y2);
end;

function Box(const X1, Y1, X2, Y2: Int32): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

end.

