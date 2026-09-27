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
      // normalize from the main point
      Result[I].X := X - DTM.Points[0].X;
      Result[I].Y := Y - DTM.Points[0].Y;
      Result[I].AreaSize := Max(AreaSize, 0); // a negative area would never match
      Result[I].Color := TColor(Color).ToBGRA();
      Result[I].Tol := Tolerance;
    end;
end;

function Matches(const Point: TSearchPoint; const Pixel: TColorBGRA): Boolean; inline;
begin
  Result := SimilarRGB(Point.Color, Pixel, Point.Tol);
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

// Where the main point can be with every point inside the buffer; X1 > X2 when the DTM does not fit.
// Points are from the main point, which is Points[0].
function MainPointArea(const Points: TPointArray; SearchWidth, SearchHeight: Integer): TBox;
var
  I: Integer;
begin
  Result := TBox.Create(0, 0, SearchWidth - 1, SearchHeight - 1);
  for I := 1 to High(Points) do
  begin
    Result.X1 := Max(Result.X1, -Points[I].X);
    Result.Y1 := Max(Result.Y1, -Points[I].Y);
    Result.X2 := Min(Result.X2, SearchWidth - 1 - Points[I].X);
    Result.Y2 := Min(Result.Y2, SearchHeight - 1 - Points[I].Y);
  end;
end;

function SearchDTM(var Limit: TLimit; DTM: TDTM; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;
var
  SearchPoints: TSearchPoints;
  Offsets: TPointArray;
  I, H, X, Y: Integer;
  Area: TBox;
  PointBuffer: TPointBuffer;
label
  Next;
begin
  if (not DTM.Valid()) then
    Exit(nil);

  SearchPoints := GetSearchPoints(DTM);
  H := High(SearchPoints);

  SetLength(Offsets, Length(SearchPoints));
  for I := 0 to H do
    Offsets[I] := TPoint.Create(SearchPoints[I].X, SearchPoints[I].Y);
  Area := MainPointArea(Offsets, SearchWidth, SearchHeight);
  if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then
    Exit(nil);

  for Y := Area.Y1 to Area.Y2 do
  begin
    for X := Area.X1 to Area.X2 do
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
      SetLength(RowCandidates[Y], SearchWidth);
      Count := 0;
      for X := 0 to SearchWidth - 1 do
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

  // round the main point, which stays at 0, 0
  procedure RotateDTMPoints(var RotatedPoints: TPointArray; const A: Double); inline;
  var
    I, X, Y: Integer;
  begin
    for I := 1 to H do
    begin
      X := SearchPoints[I].X;
      Y := SearchPoints[I].Y;

      RotatedPoints[I].X := Round(Cos(A) * X - Sin(A) * Y);
      RotatedPoints[I].Y := Round(Sin(A) * X + Cos(A) * Y);
    end;
  end;

type
  TMatch = record X,Y: Integer; Deg: Double; end;
  TMatchBuffer = specialize TSimbaArrayBuffer<TMatch>;
var
  I, X, Y: Integer;
  Area: TBox;
  Match: TMatch;
  MatchBuffer: TMatchBuffer;
  RotatedPoints: TPointArray;
  MiddleAngle, SearchDegree, CallerStart: Double;
  AngleIndex, AngleCount: Integer;
label
  Next;
begin
  if (not DTM.Valid()) then
    Exit(nil);

  SearchPoints := GetSearchPoints(DTM);
  H := High(SearchPoints);

  SetLength(RotatedPoints, Length(SearchPoints)); // [0] is the main point: 0, 0
  SetLength(RowCandidates, SearchHeight);
  SetLength(RowSearched, SearchHeight);

  CallerStart := StartDegrees;
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

    RotateDTMPoints(RotatedPoints, DegToRad(SearchDegree));

    Area := MainPointArea(RotatedPoints, SearchWidth, SearchHeight);
    if (Area.X1 > Area.X2) or (Area.Y1 > Area.Y2) then
      Continue;

    // a full circle's end is its start: 0..360 finds 0, not 360. Max: rounding can put the first
    // angle a hair below the start, which DegNormalize would make 360
    Match.Deg := CallerStart + DegNormalize(Max(SearchDegree, StartDegrees) - StartDegrees);

    for Y := Area.Y1 to Area.Y2 do
    begin
      for X in Candidates(Y) do
      begin
        if (X < Area.X1) or (X > Area.X2) then
          Continue;

        for I := 1 to H do
          if not FindPoint(SearchPoints[I], X + RotatedPoints[I].X, Y + RotatedPoints[I].Y, Buffer, BufferWidth, SearchWidth, SearchHeight) then
            goto Next;

        Match.X := X + OffsetX;
        Match.Y := Y + OffsetY;
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
