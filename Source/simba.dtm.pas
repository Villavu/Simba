{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dtm;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  PDTMPoint = ^TDTMPoint;
  TDTMPoint = record
    X, Y: Integer;
    Color: Integer;
    Tolerance: Single; // 0..100
    AreaSize: Integer; // how far round X, Y a match can be. 0 for X, Y only
  end;
  PDTMPointArray = ^TDTMPointArray;
  TDTMPointArray = array of TDTMPoint;

  // The first point is the main one: a match is where it is, and the others are placed from it.
  PDTM = ^TDTM;
  TDTM = record
    Points: TDTMPointArray;

    // Raises for a string that is not a DTM, and the DTM is left as it was.
    procedure FromString(Str: String);
    function ToString: String;

    procedure AddPoint(Point: TDTMPoint); overload;
    procedure AddPoint(X, Y, Color: Integer; Tolerance: Single; AreaSize: Integer); overload;
    procedure DeletePoint(Index: Integer);
    procedure DeletePoints;
    procedure MovePoint(AFrom, ATo: Integer);
    function PointCount: Integer;

    // two points at least
    function Valid: Boolean;
  end;

implementation

uses
  simba.compress, simba.vartype_string, simba.encoding;

{
  DTM string is 'DTM:' followed by the DTM's data that is zlib compressed and base64 encoded.

  Data layout is:
    Count
    X[0 .. Count-1]
    Y[0 .. Count-1]
    Color[0 .. Count-1]
    Tolerance[0 .. Count-1]  Tolerance * 100, rounded: 0..100 to two decimals
    AreaSize[0 .. Count-1]

  So a DTM of 3 points is 4 + 3 * 5 * 4 = 64 bytes:
    Count  X X X  Y Y Y  C C C  T T T  A A A
}
const
  FIELD_COUNT = 5; // X, Y, Color, Tolerance, AreaSize

procedure TDTM.FromString(Str: String);
var
  Stream: TStringStream;
  NewPoints: TDTMPointArray;
  Count: UInt32;
  I: Integer;
begin
  if not Str.StartsWith('DTM:', True) then
    SimbaException('TDTM.FromString: "%s" is not a DTM string', [Str]);

  Stream := TStringStream.Create(DecompressString(ESimbaCompressAlgo.ZLIB, EBaseEncoding.b64, Str.After('DTM:')));
  try
    Count := 0;
    if (Stream.Size >= SizeOf(UInt32)) then
      Count := Stream.ReadDWord();

    SetLength(NewPoints, Count);
    for I := 0 to High(NewPoints) do
      NewPoints[I].X := Int32(Stream.ReadDWord());
    for I := 0 to High(NewPoints) do
      NewPoints[I].Y := Int32(Stream.ReadDWord());
    for I := 0 to High(NewPoints) do
      NewPoints[I].Color := Int32(Stream.ReadDWord());
    for I := 0 to High(NewPoints) do
      NewPoints[I].Tolerance := Stream.ReadDWord() / 100;
    for I := 0 to High(NewPoints) do
      NewPoints[I].AreaSize := Int32(Stream.ReadDWord());
  finally
    Stream.Free();
  end;

  Points := NewPoints;
end;

function TDTM.ToString: String;
var
  Stream: TStringStream;
  I: Integer;
begin
  Result := '';
  if (Length(Points) = 0) then
    Exit;

  Stream := TStringStream.Create();
  try
    Stream.WriteDWord(Length(Points));
    for I := 0 to High(Points) do
      Stream.WriteDWord(UInt32(Points[I].X));
    for I := 0 to High(Points) do
      Stream.WriteDWord(UInt32(Points[I].Y));
    for I := 0 to High(Points) do
      Stream.WriteDWord(UInt32(Points[I].Color));
    for I := 0 to High(Points) do
      Stream.WriteDWord(UInt32(Round(Max(Points[I].Tolerance, 0) * 100))); // two decimals
    for I := 0 to High(Points) do
      Stream.WriteDWord(UInt32(Points[I].AreaSize));

    Result := 'DTM:' + CompressString(ESimbaCompressAlgo.ZLIB, EBaseEncoding.b64, Stream.DataString);
  finally
    Stream.Free();
  end;
end;

function TDTM.Valid: Boolean;
begin
  Result := Length(Points) > 1;
end;

procedure TDTM.DeletePoint(Index: Integer);
begin
  Delete(Points, Index, 1);
end;

procedure TDTM.DeletePoints;
begin
  Points := [];
end;

procedure TDTM.MovePoint(AFrom, ATo: Integer);
begin
  specialize MoveElement<TDTMPoint>(Points, AFrom, ATo);
end;

function TDTM.PointCount: Integer;
begin
  Result := Length(Points);
end;

procedure TDTM.AddPoint(Point: TDTMPoint);
begin
  Points := Points + [Point];
end;

procedure TDTM.AddPoint(X, Y, Color: Integer; Tolerance: Single; AreaSize: Integer);
var
  Point: TDTMPoint;
begin
  Point.X := X;
  Point.Y := Y;
  Point.Color := Color;
  Point.Tolerance := Tolerance;
  Point.AreaSize := AreaSize;

  AddPoint(Point);
end;

end.
