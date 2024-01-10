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
  simba.mufasatypes;

type
  PDTMPoint = ^TDTMPoint;
  TDTMPoint = record
    X, Y: Integer;
    Color: Integer;
    Tolerance: Single;
    AreaSize: Integer;
  end;
  PDTMPointArray = ^TDTMPointArray;
  TDTMPointArray = array of TDTMPoint;

  PDTM = ^TDTM;
  TDTM = record
    Points: TDTMPointArray;

    procedure FromString(Str: String);
    function ToString: String;

    procedure AddPoint(Point: TDTMPoint); overload;
    procedure AddPoint(X, Y, Color, Tolerance, AreaSize: Integer); overload;
    procedure DeletePoint(Index: Integer);
    procedure DeletePoints;
    procedure MovePoint(AFrom, ATo: Integer);
    function PointCount: Integer;

    procedure Normalize;
    function Valid: Boolean;

    class operator Initialize(var Self: TDTM);
  end;

implementation

uses
  simba.compress, simba.encoding;

procedure TDTM.FromString(Str: String);
var
  I: Integer;
  Stream: TStringStream;
begin
  if not Str.StartsWith('DTM:', True) then
    raise Exception.Create('TDTM.FromString: Invalid string "' + Str + '"');

  Stream := TStringStream.Create(ZDecompressString(Base64Decode(Str.After('DTM:'))));

  SetLength(Points, Stream.ReadDWord());
  for I := 0 to High(Points) do
    Points[I].X := Stream.ReadDWord();
  for I := 0 to High(Points) do
    Points[I].Y := Stream.ReadDWord();
  for I := 0 to High(Points) do
    Points[I].Color := Stream.ReadDWord();
  for I := 0 to High(Points) do
    Points[I].Tolerance := Stream.ReadDWord() / 100;
  for I := 0 to High(Points) do
    Points[I].AreaSize := Stream.ReadDWord();

  Stream.Free();

  Normalize();
end;

//We save the data as arrays for each point (so we can add features to DTMs without having to change
//the way the ToString is done..
//E.G. A DTM with 3 points would become
//LenXXXYYYCCCTTTASZASZASZBPBPBP
function TDTM.ToString: String;
var
  I: Integer;
  Stream: TStringStream;
begin
  Result := '';
  if (Length(Points) = 0) then
    Exit;

  Stream := TStringStream.Create();
  Stream.WriteDWord(Length(Points));

  for I := 0 to High(Points) do
    Stream.WriteDWord(Points[I].X);
  for I := 0 to High(Points) do
    Stream.WriteDWord(Points[I].Y);
  for I := 0 to High(Points) do
    Stream.WriteDWord(Points[I].Color);
  for I := 0 to High(Points) do
    Stream.WriteDWord(Round(Points[I].Tolerance * 100)); // tolerance is 0..100 as float, need an integer.
  for I := 0 to High(Points) do
    Stream.WriteDWord(Points[I].AreaSize);

  Result := 'DTM:' + Base64Encode(ZCompressString(Stream.DataString));

  Stream.Free();
end;

procedure TDTM.Normalize;
var
  I: Integer;
begin
  if (Self.PointCount < 1) or ((Points[0].X = 0) and (Points[0].Y = 0)) then // Already normalized
    Exit;

  for I := 1 to Self.PointCount - 1 do
  begin
    Points[I].X := Points[I].X - Points[0].X;
    Points[I].Y := Points[I].Y - Points[0].Y;
  end;

  Points[0].X := 0;
  Points[0].Y := 0;
end;

function TDTM.Valid: Boolean;
begin
  Result := PointCount > 1;
  if Result then
    Normalize();
end;

class operator TDTM.Initialize(var Self: TDTM);
begin
  Self := Default(TDTM);
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

procedure TDTM.AddPoint(X, Y, Color, Tolerance, AreaSize: Integer);
var
  Point: TDTMPoint;
begin
  Point.X := X;
  Point.Y := Y;
  Point.Color := Color;
  Point.Tolerance := Tolerance;
  Point.AreaSize := AreaSize;

  Points := Points + [Point];
end;

end.

