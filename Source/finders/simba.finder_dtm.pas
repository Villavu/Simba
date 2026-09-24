{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The DTM finder.
}
unit simba.finder_dtm;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base, simba.colormath,
  simba.dtm;

function SimbaFinder_FindDTM(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                             DTM: TDTM; MaxToFind: Integer): TPointArray;

function SimbaFinder_FindDTMRotated(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                                    DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray;
                                    MaxToFind: Integer): TPointArray;

implementation

uses
  simba.colormath_distance, simba.containers, simba.container_point, simba.vartype_box, simba.threading;

type
  TSearchPoint = record
    X, Y: Integer;
    AreaSize: Integer;
    Color: TColorBGRA;
    Tol: Single;
  end;
  TSearchPoints = array of TSearchPoint;

function GetSearchPoints(DTM: TDTM): TSearchPoints;
var
  I: Integer;
begin
  SetLength(Result, DTM.PointCount);
  for I := 0 to DTM.PointCount - 1 do
    with DTM.Points[I] do
    begin
      Result[I].X := X;
      Result[I].Y := Y;
      Result[I].AreaSize := AreaSize;
      Result[I].Color := TColor(Color).ToBGRA();
      Result[I].Tol := Tolerance;
    end;
end;

function Matches(const Point: TSearchPoint; const Pixel: TColorBGRA): Boolean; inline;
begin
  Result := DistanceRGB(Point.Color, Pixel, DefaultMultipliers) <= Point.Tol;
end;

// whether any pixel of the buffer within Point's area of (X, Y) matches it
function FindPoint(const Point: TSearchPoint; X, Y: Integer; Buffer: PColorBGRA; BufferWidth, SearchWidth, SearchHeight: Integer): Boolean;
var
  StartX, StopX, StartY, StopY: Integer;
  Ptr, RowEnd: PColorBGRA;
begin
  if (Point.AreaSize = 0) then
    Exit((X >= 0) and (Y >= 0) and (X < SearchWidth) and (Y < SearchHeight) and Matches(Point, Buffer[Y * BufferWidth + X]));

  StartX := Max(X - Point.AreaSize, 0);
  StartY := Max(Y - Point.AreaSize, 0);
  StopX := Min(X + Point.AreaSize, SearchWidth - 1);
  StopY := Min(Y + Point.AreaSize, SearchHeight - 1);

  for Y := StartY to StopY do
  begin
    Ptr := @Buffer[Y * BufferWidth + StartX];
    RowEnd := Ptr + (StopX - StartX + 1);
    while (Ptr < RowEnd) do
    begin
      if Matches(Point, Ptr^) then
        Exit(True);
      Inc(Ptr);
    end;
  end;

  Result := False;
end;

function SearchDTM(var Limit: TLimit; DTM: TDTM; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;
var
  SearchPoints: TSearchPoints;

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
  PointBuffer: TPointBuffer;
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

  SearchPoints := GetSearchPoints(DTM);
  H := High(SearchPoints);

  for Y := MainPointArea.Y1 to MainPointArea.Y2 do
  begin
    for X := MainPointArea.X1 to MainPointArea.X2 do
    begin
      for I := 0 to H do
        if not FindPoint(SearchPoints[I], X + SearchPoints[I].X, Y + SearchPoints[I].Y, Buffer, BufferWidth, SearchWidth, SearchHeight) then
          goto Next;

      PointBuffer.Add(X + OffsetX, Y + OffsetY);
      Limit.Inc();

      Next:
    end;

    // Check if we reached the limit every row.
    if Limit.Reached() then
      Break;
  end;

  Result := PointBuffer.ToArray(False);
end;

function SearchDTMRotated(var Limit: TLimit; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; OffsetX, OffsetY: Integer): TPointArray;
var
  H: Integer;
  SearchPoints: TSearchPoints;
  RowCandidates: array of TIntegerArray;
  RowSearched: array of Boolean;

  // The main point is the centre of rotation, so where it matches is the same at
  // every angle: each row is searched for it once, when an angle first needs it.
  function Candidates(Y: Integer): TIntegerArray;
  var
    X, Count: Integer;
  begin
    if not RowSearched[Y] then
    begin
      SetLength(RowCandidates[Y], SearchWidth + 1);
      Count := 0;
      for X := 0 to SearchWidth do
        if FindPoint(SearchPoints[0], X, Y, Buffer, BufferWidth, SearchWidth, SearchHeight) then
        begin
          RowCandidates[Y][Count] := X;
          Inc(Count);
        end;
      SetLength(RowCandidates[Y], Count);
      RowSearched[Y] := True;
    end;

    Result := RowCandidates[Y];
  end;

  procedure RotateDTMPoints(var RotatedPoints: TPointArray; const A: Double; out Bounds: TBox); inline;
  var
    I, X, Y: Integer;
  begin
    Bounds := TBox.Create(0, 0, 0, 0);

    for I := 1 to H do
    begin
      X := SearchPoints[I].X;
      Y := SearchPoints[I].Y;

      RotatedPoints[I].X := Round(Cos(A) * X - Sin(A) * Y);
      RotatedPoints[I].Y := Round(Sin(A) * X + Cos(A) * Y);

      if (RotatedPoints[I].X < Bounds.X1) then Bounds.X1 := RotatedPoints[I].X;
      if (RotatedPoints[I].X > Bounds.X2) then Bounds.X2 := RotatedPoints[I].X;
      if (RotatedPoints[I].Y < Bounds.Y1) then Bounds.Y1 := RotatedPoints[I].Y;
      if (RotatedPoints[I].Y > Bounds.Y2) then Bounds.Y2 := RotatedPoints[I].Y;
    end;
  end;

type
  TMatch = record X,Y: Integer; Deg: Double; end;
  TMatchBuffer = specialize TSimbaArrayBuffer<TMatch>;
var
  I, X, Y: Integer;
  MainPointArea: TBox;
  Match: TMatch;
  MatchBuffer: TMatchBuffer;
  RotatedPoints: TPointArray;
  MiddleAngle, SearchDegree: Double;
  AngleIndex, AngleCount: Integer;
  DTMBounds: TBox;
label
  Next;
begin
  if (not DTM.Valid()) then
    Exit(nil);

  SearchPoints := GetSearchPoints(DTM);
  H := High(SearchPoints);

  SetLength(RotatedPoints, Length(SearchPoints));
  // MainPointArea can reach one past the buffer's right and bottom
  SetLength(RowCandidates, SearchHeight + 1);
  SetLength(RowSearched, SearchHeight + 1);

  if (EndDegrees - StartDegrees >= 360) then
  begin
    StartDegrees := DegNormalize(StartDegrees);
    EndDegrees := StartDegrees + 360;
  end else
  begin
    StartDegrees := DegNormalize(StartDegrees);
    EndDegrees := DegNormalize(EndDegrees);
    if (StartDegrees > EndDegrees) then
      EndDegrees := EndDegrees + 360;
  end;

  // the middle first, then out either side of it a step at a time
  MiddleAngle := (StartDegrees + EndDegrees) / 2.0;
  if (Step > 0) then
    AngleCount := 2 * Floor((EndDegrees - StartDegrees) / 2.0 / Step + 1E-9) + 1
  else
    AngleCount := 1;
  // both ends of a full circle are the same angle
  if (AngleCount > 1) and (Step * (AngleCount - 1) >= 360 - 1E-9) then
    Dec(AngleCount);

  for AngleIndex := 0 to AngleCount - 1 do
  begin
    if Odd(AngleIndex) then
      SearchDegree := MiddleAngle + Step * (AngleIndex div 2 + 1)
    else
      SearchDegree := MiddleAngle - Step * (AngleIndex div 2);

    RotateDTMPoints(RotatedPoints, DegToRad(SearchDegree), DTMBounds);

    MainPointArea.X1 := Abs(DTMBounds.X1);
    MainPointArea.Y1 := Abs(DTMBounds.Y1);
    MainPointArea.X2 := SearchWidth - DTMBounds.X2;
    MainPointArea.Y2 := SearchHeight - DTMBounds.Y2;
    if (MainPointArea.X1 >= MainPointArea.X2) or (MainPointArea.Y1 >= MainPointArea.Y2) then
      Continue;

    for Y := MainPointArea.Y1 to MainPointArea.Y2 do
    begin
      for X in Candidates(Y) do
      begin
        if (X < MainPointArea.X1) or (X > MainPointArea.X2) then
          Continue;

        for I := 1 to H do
          if not FindPoint(SearchPoints[I], X + RotatedPoints[I].X, Y + RotatedPoints[I].Y, Buffer, BufferWidth, SearchWidth, SearchHeight) then
            goto Next;

        Match.X := X + OffsetX;
        Match.Y := Y + OffsetY;
        Match.Deg := SearchDegree;
        MatchBuffer.Add(Match);

        Limit.Inc();

        Next:
      end;

      // Check if we reached the limit every row.
      if Limit.Reached() then
        Break;
    end;

    if Limit.Reached() then
      Break;
  end;

  SetLength(Result,       MatchBuffer.Count);
  SetLength(FoundDegrees, MatchBuffer.Count);
  for I := 0 to MatchBuffer.Count - 1 do
    with MatchBuffer[I] do
    begin
      Result[I].X := X;
      Result[I].Y := Y;
      FoundDegrees[I] := Deg;
    end;
end;

function SimbaFinder_FindDTM(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                             DTM: TDTM; MaxToFind: Integer): TPointArray;
var
  Limit: TLimit;
begin
  Result := [];
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Limit := TLimit.Create(MaxToFind);

  Result := SearchDTM(Limit, DTM, Data, PixelsPerRow, AWidth, AHeight, Offset.X, Offset.Y);
  if (MaxToFind > 0) and (Length(Result) > MaxToFind) then
    SetLength(Result, MaxToFind);
end;

function SimbaFinder_FindDTMRotated(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                                    DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray;
                                    MaxToFind: Integer): TPointArray;
var
  Limit: TLimit;
begin
  Result := [];
  FoundDegrees := [];
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Limit := TLimit.Create(MaxToFind);

  Result := SearchDTMRotated(Limit, Data, PixelsPerRow, AWidth, AHeight, DTM, StartDegrees, EndDegrees, Step, FoundDegrees, Offset.X, Offset.Y);
  if (MaxToFind > 0) and (Length(Result) > MaxToFind) then
  begin
    SetLength(Result, MaxToFind);
    SetLength(FoundDegrees, MaxToFind); // one angle per match
  end;
end;

end.
