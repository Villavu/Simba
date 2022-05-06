{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder_dtm;

{$DEFINE SIMBA_O4}
{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.dtm;

type
  TFindDTMBuffer = record
    Data: PRGB32;
    LineWidth: PtrUInt;

    // Area to search
    X1, Y1, X2, Y2: Integer;

    function FindDTMs(DTM: TMDTM; MaxToFind: Integer = 0): TPointArray;
  end;

implementation

uses
  simba.tpa, simba.colormath, simba.overallocatearray;

function DTMBounds(DTM: TMDTM): TBox;
var
  I: Integer;
begin
  with DTM.Points[0] do
    Result := TBox.Create(X, Y, X, Y);

  for I := 1 to DTM.Count - 1 do
  begin
    if (DTM.Points[I].X < Result.X1) then Result.X1 := DTM.Points[I].X;
    if (DTM.Points[I].Y < Result.Y1) then Result.Y1 := DTM.Points[I].Y;
    if (DTM.Points[I].X > Result.X2) then Result.X2 := DTM.Points[I].X;
    if (DTM.Points[I].Y > Result.Y2) then Result.Y2 := DTM.Points[I].Y;
  end;

  Result.X1 := Result.X1 - DTM.Points[0].X;
  Result.Y1 := Result.Y1 - DTM.Points[0].Y;
  Result.X2 := Result.X2 - DTM.Points[0].X;
  Result.Y2 := Result.Y2 - DTM.Points[0].Y;
end;

function TFindDTMBuffer.FindDTMs(DTM: TMDTM; MaxToFind: Integer): TPointArray;
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
        if (X < X1) or (Y < Y1) or (X > X2) or (Y > Y2) then
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

                if RGBDistance(Data[Y * LineWidth +X], PointColors[Index]) <= PointTolerances[Index] then
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
  MainPointArea.X1 := X1 + Abs(MainPointArea.X1);
  MainPointArea.Y1 := Y1 + Abs(MainPointArea.Y1);
  MainPointArea.X2 := X2 - MainPointArea.X2;
  MainPointArea.Y2 := Y2 - MainPointArea.Y2;

  // DTM can't fit in search area
  if (MainPointArea.X1 >= MainPointArea.X2) or (MainPointArea.Y1 >= MainPointArea.Y2) then
    Exit(nil);

  SetLength(PointColors, DTM.Count);
  SetLength(PointTolerances, DTM.Count);
  for I := 0 to DTM.Count - 1 do
  begin
    PointColors[I] := RGBToBGR(DTM.Points[I].C);

    if (DTM.Points[I].T = 0) then
      PointTolerances[I] := Sqr(1)
    else
      PointTolerances[I] := Sqr(DTM.Points[I].T);
  end;

  PointBuffer.Init(256);
  SetLength(Table, Y2-Y1+1, X2-X1+1);
  H := DTM.Count - 1;

  for Y := MainPointArea.Y1 to MainPointArea.Y2 do
    for X := MainPointArea.X1 to MainPointArea.X2 do
    begin
      for I := 0 to H do
        if not FindPoint(I, DTM.Points[I].ASZ, X + DTM.Points[I].X, Y + DTM.Points[I].Y) then
          goto Next;

      PointBuffer.Add(TPoint.Create(X, Y));
      if (PointBuffer.Count = MaxToFind) then
      begin
        Result := PointBuffer.Trim();
        Exit;
      end;

      Next:
    end;

  Result := PointBuffer.Trim();
end;

end.

