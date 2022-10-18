{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dtm;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.baseclass;

type
  PDTMPoint = ^TDTMPoint;
  TDTMPoint = record
    X, Y, Color, Tolerance, AreaSize: Integer;
  end;
  PDTMPointArray = ^TDTMPointArray;
  TDTMPointArray = array of TDTMPoint;

  PDTM = ^TDTM;
  TDTM = class(TSimbaBaseClass)
  protected
    FPoints: TDTMPointArray;

    procedure CheckIndex(Index: Integer);

    function GetPointAreaSize(Index: Integer): Integer;
    function GetPoints: TDTMPointArray;
    function GetPointColor(Index: Integer): Integer;
    function GetPointCount: Integer;
    function GetPointTolerance(Index: Integer): Integer;
    function GetPointX(Index: Integer): Integer;
    function GetPointY(Index: Integer): Integer;

    procedure SetPointAreaSize(Index: Integer; AValue: Integer);
    procedure SetPointColor(Index: Integer; AValue: Integer);
    procedure SetPointTolerance(Index: Integer; AValue: Integer);
    procedure SetPointX(Index: Integer; AValue: Integer);
    procedure SetPointY(Index: Integer; AValue: Integer);
  public
    constructor CreateFromString(DTMString: String);

    procedure LoadFromString(Str: String);
    function SaveToString: String;

    procedure Normalize;
    function Valid: Boolean;

    procedure DeletePoints;
    procedure DeletePoint(Index: Integer);
    procedure AddPoint(Point: TDTMPoint); overload;
    procedure AddPoint(X, Y, Color, Tolerance, AreaSize: Integer); overload;
    procedure SwapPoint(Index1, Index2: Integer);

    property PointColor[Index: Integer]: Integer read GetPointColor write SetPointColor;
    property PointTolerance[Index: Integer]: Integer read GetPointTolerance write SetPointTolerance;
    property PointX[Index: Integer]: Integer read GetPointX write SetPointX;
    property PointY[Index: Integer]: Integer read GetPointY write SetPointY;
    property PointAreaSize[Index: Integer]: Integer read GetPointAreaSize write SetPointAreaSize;
    property PointCount: Integer read GetPointCount;
    property Points: TDTMPointArray read GetPoints;
  end;

implementation

uses
  simba.compress, simba.encoding;

procedure TDTM.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FPoints)) then
    raise Exception.Create('DTM Point index out of range');
end;

function TDTM.GetPointAreaSize(Index: Integer): Integer;
begin
  CheckIndex(Index);

  Result := FPoints[Index].AreaSize;
end;

function TDTM.GetPointCount: Integer;
begin
  Result := Length(FPoints);
end;

function TDTM.GetPointColor(Index: Integer): Integer;
begin
  CheckIndex(Index);

  Result := FPoints[Index].Color;
end;

function TDTM.GetPointTolerance(Index: Integer): Integer;
begin
  CheckIndex(Index);

  Result := FPoints[Index].Tolerance;
end;

function TDTM.GetPointX(Index: Integer): Integer;
begin
  CheckIndex(Index);

  Result := FPoints[Index].X;
end;

function TDTM.GetPointY(Index: Integer): Integer;
begin
  CheckIndex(Index);

  Result := FPoints[Index].Y;
end;

procedure TDTM.SetPointAreaSize(Index: Integer; AValue: Integer);
begin
  CheckIndex(Index);

  FPoints[Index].AreaSize := AValue;
end;

procedure TDTM.SetPointColor(Index: Integer; AValue: Integer);
begin
  CheckIndex(Index);

  FPoints[Index].Color := AValue;
end;

procedure TDTM.SetPointTolerance(Index: Integer; AValue: Integer);
begin
  CheckIndex(Index);

  FPoints[Index].Tolerance := AValue;
end;

procedure TDTM.SetPointX(Index: Integer; AValue: Integer);
begin
  CheckIndex(Index);

  FPoints[Index].X := AValue;
end;

procedure TDTM.SetPointY(Index: Integer; AValue: Integer);
begin
  CheckIndex(Index);

  FPoints[Index].Y := AValue;
end;

procedure TDTM.LoadFromString(Str: String);
var
  I: Integer;
  Stream: TStringStream;
begin
  if (Length(Str) = 0) or (Str[1] <> 'm') then
    raise Exception.Create('TDTM.LoadFromString: Invalid string "' + Str + '"');
  Delete(Str, 1, 1);

  Stream := TStringStream.Create(DecompressString(Base64Decode(Str)));
  Stream.Position := 0;

  SetLength(FPoints, Stream.ReadDWord());
  for I := 0 to High(FPoints) do
    FPoints[I].X := Stream.ReadDWord();
  for I := 0 to High(FPoints) do
    FPoints[I].Y := Stream.ReadDWord();
  for I := 0 to High(FPoints) do
    FPoints[I].Color := Stream.ReadDWord();
  for I := 0 to High(FPoints) do
    FPoints[I].Tolerance := Stream.ReadDWord();
  for I := 0 to High(FPoints) do
    FPoints[I].AreaSize := Stream.ReadDWord();
  // "badpoints" not used anymore ...
  for I := 0 to High(FPoints) do
    Stream.ReadByte();

  Stream.Free();

  Normalize();
end;

procedure TDTM.Normalize;
var
  I: Integer;
begin
  if (Self.PointCount < 1) or ((FPoints[0].X = 0) and (FPoints[0].Y = 0)) then // Already normalized
    Exit;

  for I := 1 to Self.PointCount - 1 do
  begin
    FPoints[I].X := FPoints[I].X - FPoints[0].X;
    FPoints[I].Y := FPoints[I].Y - FPoints[0].Y;
  end;

  FPoints[0].X := 0;
  FPoints[0].Y := 0;
end;

function TDTM.Valid: Boolean;
begin
  Result := PointCount > 1;
  if Result then
    Normalize();
end;

procedure TDTM.DeletePoint(Index: Integer);
begin
  CheckIndex(Index);

  Delete(FPoints, Index, 1);
end;

procedure TDTM.AddPoint(Point: TDTMPoint);
begin
  FPoints := FPoints + [Point];
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

  FPoints := FPoints + [Point];
end;

procedure TDTM.SwapPoint(Index1, Index2: Integer);
begin
  CheckIndex(Index1);
  CheckIndex(Index2);

  specialize Swap<TDTMPoint>(FPoints[Index1], FPoints[Index2]);
end;

procedure TDTM.DeletePoints;
begin
  FPoints := [];
end;

constructor TDTM.CreateFromString(DTMString: String);
begin
  inherited Create();

  LoadFromString(DTMString);
end;

//We save the data as arrays for each point (so we can add features to DTMs without having to change
//the way the ToString is done..
//E.G. A DTM with 3 points would become
//LenXXXYYYCCCTTTASZASZASZBPBPBP
function TDTM.SaveToString: String;
var
  I: Integer;
  Stream: TStringStream;
begin
  Result := '';
  if (Length(FPoints) < 1) then
    Exit;

  Stream := TStringStream.Create();
  Stream.WriteDWord(Length(FPoints));

  for I := 0 to High(FPoints) do
    Stream.WriteDWord(FPoints[I].X);
  for I := 0 to High(FPoints) do
    Stream.WriteDWord(FPoints[I].Y);
  for I := 0 to High(FPoints) do
    Stream.WriteDWord(FPoints[I].Color);
  for I := 0 to High(FPoints) do
    Stream.WriteDWord(FPoints[I].Tolerance);
  for I := 0 to High(FPoints) do
    Stream.WriteDWord(FPoints[I].AreaSize);
  // "badpoints" not used anymore ...
  for I := 0 to High(FPoints) do
    Stream.WriteByte(0);

  Result := 'm' + Base64Encode(CompressString(Stream.DataString));

  Stream.Free();
end;

function TDTM.GetPoints: TDTMPointArray;
begin
  Result := Copy(FPoints);
end;

end.

