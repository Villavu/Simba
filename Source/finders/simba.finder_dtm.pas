{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder_dtm;

{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.dtm;

type
  TFindDTMBuffer = record
    Data: PRGB32;
    Width: Integer;

    SearchWidth: Integer;
    SearchHeight: Integer;

    Offset: TPoint;

    function FindDTMs(DTM: TDTM; MaxToFind: Integer = 0): TPointArray;
    function FindDTMsRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 0): TPointArray;

    class operator Initialize(var Self: TFindDTMBuffer);
  end;

implementation

uses
  simba.colormath_distance, simba.math, simba.overallocatearray;

function TFindDTMBuffer.FindDTMs(DTM: TDTM; MaxToFind: Integer): TPointArray;
var
  PointColors: TRGB32Array;
  PointTolerances: TIntegerArray;
  Table: array of array of record
    Checked: Int64;
    Hit: Int64;
  end;

  function FindPoint(const Index, Size: Integer; X, Y: Integer): Boolean;
  var
    StartX, StopX, StartY, StopY: Integer;
  begin
    StartX := X - Size;
    StartY := Y - Size;
    StopX := X + Size;
    StopY := Y + Size;

    for Y := StartY to StopY do
      for X := StartX to StopX do
      begin
        if (X < 0) or (Y < 0) or (X >= SearchWidth) or (Y >= SearchHeight) then
          Continue;

        with Table[Y, X] do
        begin
          case Checked.TestBit(Index) of
            True:
              if Hit.TestBit(Index) then
                Exit(True);

            False:
              begin
                Checked.SetBit(Index);

                //if DistanceRGB(Data[Y * Width + X], PointColors[Index]) <= PointTolerances[Index] then
                begin
                  Checked.SetBit(Index);
                  Hit.SetBit(Index);

                  Exit(True);
                end;
              end;
          end;
        end;
      end;

    Result := False;
  end;

  function DTMBounds(DTM: TDTM): TBox;
  var
    I: Integer;
  begin
    Result := TBox.Create(0, 0, 0, 0);

    for I := 1 to DTM.PointCount - 1 do
    begin
      if (DTM.Points[I].X < Result.X1) then Result.X1 := DTM.Points[I].X;
      if (DTM.Points[I].X > Result.X2) then Result.X2 := DTM.Points[I].X;
      if (DTM.Points[I].Y < Result.Y1) then Result.Y1 := DTM.Points[I].Y;
      if (DTM.Points[I].Y > Result.Y2) then Result.Y2 := DTM.Points[I].Y;
    end;
  end;

var
  I, H, X, Y: Integer;
  MainPointArea: TBox;
  PointBuffer: specialize TSimbaOverAllocateArray<TPoint>;
label
  Next;
begin
  if (not DTM.Valid()) then
    Exit(nil);

  MainPointArea := DTMBounds(DTM);
  MainPointArea.X1 := Abs(MainPointArea.X1);
  MainPointArea.Y1 := Abs(MainPointArea.Y1);
  MainPointArea.X2 := SearchWidth - MainPointArea.X2;
  MainPointArea.Y2 := SearchHeight - MainPointArea.Y2;

  // DTM can't fit in search area
  if (MainPointArea.X1 >= MainPointArea.X2) or (MainPointArea.Y1 >= MainPointArea.Y2) then
    Exit(nil);

  SetLength(PointColors, DTM.PointCount);
  SetLength(PointTolerances, DTM.PointCount);
  for I := 0 to DTM.PointCount - 1 do
  begin
    //PointColors[I] := RGBToBGR(DTM.Points[I].Color);

    if (DTM.Points[I].Tolerance = 0) then
      PointTolerances[I] := Sqr(1)
    else
      PointTolerances[I] := Sqr(DTM.Points[I].Tolerance);
  end;

  PointBuffer.Init(256);
  SetLength(Table, SearchHeight, SearchWidth);
  H := DTM.PointCount - 1;

  for Y := MainPointArea.Y1 to MainPointArea.Y2 do
    for X := MainPointArea.X1 to MainPointArea.X2 do
    begin
      for I := 0 to H do
        if not FindPoint(I, DTM.Points[I].AreaSize, X + DTM.Points[I].X, Y + DTM.Points[I].Y) then
          goto Next;

      PointBuffer.Add(TPoint.Create(X + Offset.X, Y + Offset.Y));
      if (PointBuffer.Count = MaxToFind) then
      begin
        Result := PointBuffer.Trim();
        Exit;
      end;

      Next:
    end;

  Result := PointBuffer.Trim();
end;

function TFindDTMBuffer.FindDTMsRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer): TPointArray;
var
  PointColors: TRGB32Array;
  PointTolerances: TIntegerArray;
  Table: array of array of record
    Checked: Int64;
    Hit: Int64;
  end;

  function FindPoint(const Index, Size: Integer; X, Y: Integer): Boolean;
  var
    StartX, StopX, StartY, StopY: Integer;
  begin
    StartX := X - Size;
    StartY := Y - Size;
    StopX  := X + Size;
    StopY  := Y + Size;

    for Y := StartY to StopY do
      for X := StartX to StopX do
      begin
        if (X < 0) or (Y < 0) or (X >= SearchWidth) or (Y >= SearchHeight) then
          Continue;

        with Table[Y, X] do
        begin
          case Checked.TestBit(Index) of
            True:
              if Hit.TestBit(Index) then
                Exit(True);

            False:
              begin
                Checked.SetBit(Index);

                //if DistanceRGB(Data[Y * Width + X], PointColors[Index]) <= PointTolerances[Index] then
                begin
                  Checked.SetBit(Index);
                  Hit.SetBit(Index);

                  Exit(True);
                end;
              end;
          end;
        end;
      end;

    Result := False;
  end;

  procedure RotateDTMPoints(const Points: TPointArray; var RotatedPoints : TPointArray; const A: Double; out Bounds: TBox); inline;
  var
    I: Integer;
  begin
    Bounds := TBox.Create(0, 0, 0, 0);

    for I := 1 to High(Points) do
    begin
      RotatedPoints[I].X := Round(Cos(A) * Points[I].X - Sin(A) * Points[I].Y);
      RotatedPoints[I].Y := Round(Sin(A) * Points[I].X + Cos(A) * Points[I].Y);

      if (RotatedPoints[I].X < Bounds.X1) then Bounds.X1 := RotatedPoints[I].X;
      if (RotatedPoints[I].X > Bounds.X2) then Bounds.X2 := RotatedPoints[I].X;
      if (RotatedPoints[I].Y < Bounds.Y1) then Bounds.Y1 := RotatedPoints[I].Y;
      if (RotatedPoints[I].Y > Bounds.Y2) then Bounds.Y2 := RotatedPoints[I].Y;
    end;
  end;

type
  TMatch = record X,Y: Integer; Deg: Double; end;
  TMatchBuffer = specialize TSimbaOverAllocateArray<TMatch>;
var
  I, X, Y: Integer;
  MainPointArea: TBox;
  Match: TMatch;
  MatchBuffer: TMatchBuffer;
  Points, RotatedPoints: TPointArray;
  MiddleAngle, SearchDegree: Double;
  AngleSteps: Integer;
  DTMBounds: TBox;
label
  Next, Finished;
begin
  if (not DTM.Valid()) then
    Exit(nil);

  SetLength(Points, DTM.PointCount);
  SetLength(RotatedPoints, DTM.PointCount);
  SetLength(PointColors, DTM.PointCount);
  SetLength(PointTolerances, DTM.PointCount);
  for I := 0 to DTM.PointCount - 1 do
  begin
    Points[I] := TPoint.Create(DTM.Points[I].X, DTM.Points[I].Y);
    //PointColors[I] := RGBToBGR(DTM.Points[I].Color);

    if (DTM.Points[I].Tolerance = 0) then
      PointTolerances[I] := Sqr(1)
    else
      PointTolerances[I] := Sqr(DTM.Points[I].Tolerance);
  end;

  SetLength(Table, SearchHeight, SearchWidth);

  StartDegrees := FixD(StartDegrees);
  if (EndDegrees <> 360) then
    EndDegrees := FixD(EndDegrees);
  if (StartDegrees > EndDegrees) then
    EndDegrees := EndDegrees + 360;

  MiddleAngle  := (StartDegrees + EndDegrees) / 2.0;
  SearchDegree := MiddleAngle;
  AngleSteps   := 0;

  while (SearchDegree <= EndDegrees) do
  begin
    if (AngleSteps mod 2 = 0) then
      SearchDegree := MiddleAngle + (Step * (AngleSteps div 2 + 1))
    else
      SearchDegree := MiddleAngle - (Step * (AngleSteps div 2 + 1));

    Inc(AngleSteps);

    RotateDTMPoints(Points, RotatedPoints, Radians(SearchDegree), DTMBounds);

    MainPointArea.X1 := Abs(DTMBounds.X1);
    MainPointArea.Y1 := Abs(DTMBounds.Y1);
    MainPointArea.X2 := SearchWidth - DTMBounds.X2;
    MainPointArea.Y2 := SearchHeight - DTMBounds.Y2;
    if (MainPointArea.X1 >= MainPointArea.X2) or (MainPointArea.Y1 >= MainPointArea.Y2) then
      Continue;

    for Y := MainPointArea.Y1 to MainPointArea.Y2 do
      for X := MainPointArea.X1 to MainPointArea.X2 do
      begin
        for I := 0 to High(RotatedPoints) do
          if not FindPoint(I, DTM.Points[I].AreaSize, X + RotatedPoints[I].X, Y + RotatedPoints[I].Y) then
            goto Next;

        Match.X := X;
        Match.Y := Y;
        Match.Deg := SearchDegree;
        MatchBuffer.Add(Match);

        if (MatchBuffer.Count = MaxToFind) then
          goto Finished;

        Next:
      end;
  end;

  Finished:

  SetLength(Result,       MatchBuffer.Count);
  SetLength(FoundDegrees, MatchBuffer.Count);
  for I := 0 to MatchBuffer.Count - 1 do
    with MatchBuffer[I] do
    begin
      Result[I].X := X + Offset.X;
      Result[I].Y := Y + Offset.Y;
      FoundDegrees[I] := Deg;
    end;
end;

class operator TFindDTMBuffer.Initialize(var Self: TFindDTMBuffer);
begin
  Self := Default(TFindDTMBuffer);
end;

end.

