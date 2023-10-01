{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  The DTM finder.
}
unit simba.finder_dtm;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Math,
  simba.mufasatypes, simba.colormath, simba.target, simba.simplelock,
  simba.dtm;

function FindDTMOnBuffer(var Limit: TSimpleThreadsafeLimit;
                         DTM: TDTM;
                         Buffer: PColorBGRA; BufferWidth: Integer;
                         SearchWidth, SearchHeight: Integer;
                         OffsetX, OffsetY: Integer): TPointArray;

function FindDTMRotatedOnBuffer(var Limit: TSimpleThreadsafeLimit;
                                Buffer: PColorBGRA; BufferWidth: Integer;
                                SearchWidth, SearchHeight: Integer;
                                DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray;
                                OffsetX, OffsetY: Integer): TPointArray;

function FindDTMOnTarget(Target: TSimbaTarget;
                         DTM: TDTM; Bounds: TBox; MaxToFind: Integer): TPointArray;

function FindDTMRotatedOnTarget(Target: TSimbaTarget;
                                DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray;
                                Bounds: TBox; MaxToFind: Integer): TPointArray;

implementation

uses
  simba.colormath_distance, simba.arraybuffer;

type
  TSearchPoint = record
    X, Y: Integer;
    AreaSize: Integer;
    Color: TColorRGB;
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
      Result[I].Color := TColor(Color).ToRGB();
      Result[I].Tol := Tolerance;
    end;
end;

function FindDTMOnBuffer(var Limit: TSimpleThreadsafeLimit; DTM: TDTM; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; OffsetX, OffsetY: Integer): TPointArray;
var
  SearchPoints: TSearchPoints;
  Table: array of array of record
    Checked: Int64;
    Hit: Int64;
  end;

  function FindPoint(const Index: Integer; X, Y: Integer): Boolean;
  var
    StartX, StopX, StartY, StopY: Integer;
    SearchPoint: TSearchPoint;
  begin
    SearchPoint := SearchPoints[Index];
    X += SearchPoint.X;
    Y += SearchPoint.Y;

    StartX := X - SearchPoint.AreaSize;
    StartY := Y - SearchPoint.AreaSize;
    StopX := X + SearchPoint.AreaSize;
    StopY := Y + SearchPoint.AreaSize;

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

                if DistanceRGB(SearchPoint.Color, Buffer[Y * BufferWidth + X].ToRGB(), DefaultMultipliers) <= SearchPoint.Tol then
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
  PointBuffer: TSimbaPointBuffer;
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
  SetLength(Table, SearchHeight, SearchWidth);
  H := High(SearchPoints);

  for Y := MainPointArea.Y1 to MainPointArea.Y2 do
  begin
    for X := MainPointArea.X1 to MainPointArea.X2 do
    begin
      for I := 0 to H do
        if not FindPoint(I, X, Y) then
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

function FindDTMRotatedOnBuffer(var Limit: TSimpleThreadsafeLimit; Buffer: PColorBGRA; BufferWidth: Integer; SearchWidth, SearchHeight: Integer; DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; OffsetX, OffsetY: Integer): TPointArray;
var
  H: Integer;
  SearchPoints: TSearchPoints;
  Table: array of array of record
    Checked: Int64;
    Hit: Int64;
  end;

  function FindPoint(const Index: Integer; X, Y: Integer): Boolean;
  var
    SearchPoint: TSearchPoint;
    StartX, StopX, StartY, StopY: Integer;
  begin
    SearchPoint := SearchPoints[Index];

    StartX := X - SearchPoint.AreaSize;
    StartY := Y - SearchPoint.AreaSize;
    StopX := X + SearchPoint.AreaSize;
    StopY := Y + SearchPoint.AreaSize;

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

                if DistanceRGB(SearchPoint.Color, Buffer[Y * BufferWidth + X].ToRGB(), DefaultMultipliers) <= SearchPoint.Tol then
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
  AngleSteps: Integer;
  DTMBounds: TBox;
label
  Next;
begin
  if (not DTM.Valid()) then
    Exit(nil);

  SearchPoints := GetSearchPoints(DTM);
  H := High(SearchPoints);

  SetLength(RotatedPoints, Length(SearchPoints));
  SetLength(Table, SearchHeight, SearchWidth);

  StartDegrees := DegNormalize(StartDegrees);
  if (EndDegrees <> 360) then
    EndDegrees := DegNormalize(EndDegrees);
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

    RotateDTMPoints(RotatedPoints, DegToRad(SearchDegree), DTMBounds);

    MainPointArea.X1 := Abs(DTMBounds.X1);
    MainPointArea.Y1 := Abs(DTMBounds.Y1);
    MainPointArea.X2 := SearchWidth - DTMBounds.X2;
    MainPointArea.Y2 := SearchHeight - DTMBounds.Y2;
    if (MainPointArea.X1 >= MainPointArea.X2) or (MainPointArea.Y1 >= MainPointArea.Y2) then
      Continue;

    for Y := MainPointArea.Y1 to MainPointArea.Y2 do
    begin
      for X := MainPointArea.X1 to MainPointArea.X2 do
      begin
        for I := 0 to High(RotatedPoints) do
          if not FindPoint(I, X + RotatedPoints[I].X, Y + RotatedPoints[I].Y) then
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

function FindDTMOnTarget(Target: TSimbaTarget; DTM: TDTM; Bounds: TBox; MaxToFind: Integer): TPointArray;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;
  Limit: TSimpleThreadsafeLimit;
begin
  Result := [];

  Limit := TSimpleThreadsafeLimit.Create(MaxToFind);

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    Result := FindDTMOnBuffer(Limit, DTM, Buffer, BufferWidth, Bounds.Width, Bounds.Height, Bounds.X1, Bounds.Y1);
    if (MaxToFind > 0) and (Length(Result) > MaxToFind) then
      SetLength(Result, MaxToFind);

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('FindBitmap: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

function FindDTMRotatedOnTarget(Target: TSimbaTarget; DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox; MaxToFind: Integer): TPointArray;
var
  Buffer: PColorBGRA;
  BufferWidth: Integer;
  Limit: TSimpleThreadsafeLimit;
begin
  Result := [];

  Limit := TSimpleThreadsafeLimit.Create(MaxToFind);

  if Target.GetImageData(Bounds, Buffer, BufferWidth) then
  try
    {$IFDEF SIMBA_BENCHMARKS}
    T := HighResolutionTime();
    {$ENDIF}

    Result := FindDTMRotatedOnBuffer(Limit,
                                     Buffer, BufferWidth, Bounds.Width, Bounds.Height,
                                     DTM, StartDegrees, EndDegrees, Step, FoundDegrees,
                                     Bounds.X1, Bounds.Y1);

    if (MaxToFind > 0) and (Length(Result) > MaxToFind) then
      SetLength(Result, MaxToFind);

    {$IFDEF SIMBA_BENCHMARKS}
    DebugLn('FindDTM: ColorSpace=%s Width=%d Height=%d ThreadsUsed=%d Time=%f', [Formula.AsString(), Bounds.Width, Bounds.Height, ThreadsUsed, HighResolutionTime() - T]);
    {$ENDIF}
  finally
    Target.FreeImageData(Buffer);
  end;
end;

end.
