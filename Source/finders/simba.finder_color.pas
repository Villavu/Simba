{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.finder_color;

{$DEFINE SIMBA_O4}
{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.overallocatearray, simba.colormath_same;

type
  TFindColorBuffer = record
    Data: PRGB32;
    Width: Integer;

    SearchWidth: Integer;
    SearchHeight: Integer;

    Offset: TPoint;

    function Find(out Points: TPointArray; Color: Integer; MaxToFind: Integer = 0): Boolean;
    function FindCTS0(out Points: TPointArray; Color, Tolerance: Integer; MaxToFind: Integer = 0): Boolean;
    function FindCTS1(out Points: TPointArray; Color, Tolerance: Integer; MaxToFind: Integer = 0): Boolean;
    function FindCTS2(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended; MaxToFind: Integer = 0): Boolean;
    function FindCTS3(out Points: TPointArray; Color, Tolerance: Integer; Modifier: Extended; MaxToFind: Integer = 0): Boolean;

    function Count(Color: Integer; MaxToFind: Integer = 0): Integer;
    function CountCTS0(Color, Tolerance: Integer; MaxToFind: Integer = 0): Integer;
    function CountCTS1(Color, Tolerance: Integer; MaxToFind: Integer = 0): Integer;
    function CountCTS2(Color, Tolerance: Integer; HueMod, SatMod: Extended; MaxToFind: Integer = 0): Integer;
    function CountCTS3(Color, Tolerance: Integer; Modifier: Extended; MaxToFind: Integer = 0): Integer;

    class operator Initialize(var Self: TFindColorBuffer);
  end;

implementation

generic function Finder<TSameColor>(const SameColor: TSameColor; const Offset: TPoint; out Points: TPointArray; Data: PRGB32; DataWidth: Integer; SearchWidth, SearchHeight: Integer; MaxToFind: Integer = 0): Boolean;
var
  X, Y, RowSize: Integer;
  RowPtr, Ptr: PByte;
  PointBuffer: TSimbaPointBuffer;
label
  Finished;
begin
  if (SearchWidth = 0) or (SearchHeight = 0) or (Data = nil) or (DataWidth = 0) then
    Exit(False);

  RowSize := DataWidth * SizeOf(TRGB32);
  RowPtr := PByte(Data);

  Dec(SearchHeight);
  Dec(SearchWidth);
  for Y := 0 to SearchHeight do
  begin
    Ptr := RowPtr;
    for X := 0 to SearchWidth do
    begin
      if SameColor.IsSame(PRGB32(Ptr)^) then
      begin
        PointBuffer.Add(X + Offset.X, Y + Offset.Y);

        if (PointBuffer.Count = MaxToFind) then
        begin
          Points := PointBuffer.Trim();

          goto Finished;
        end;
      end;

      Inc(Ptr, SizeOf(TRGB32));
    end;

    Inc(RowPtr, RowSize);
  end;
  Finished:

  Points := PointBuffer.Trim();

  Result := Length(Points) > 0;
end;

generic function Counter<TSameColor>(const SameColor: TSameColor; Data: PRGB32; DataWidth: Integer; SearchWidth, SearchHeight: Integer; MaxToFind: Integer = 0): Integer;
var
  X, Y, RowSize: Integer;
  RowPtr, Ptr: PByte;
begin
  Result := 0;
  if (SearchWidth = 0) or (SearchHeight = 0) or (Data = nil) or (DataWidth = 0) then
    Exit;

  RowSize := DataWidth * SizeOf(TRGB32);
  RowPtr := PByte(Data);

  Dec(SearchHeight);
  Dec(SearchWidth);
  for Y := 0 to SearchHeight do
  begin
    Ptr := RowPtr;
    for X := 0 to SearchWidth do
    begin
      if SameColor.IsSame(PRGB32(Ptr)^) then
      begin
        Inc(Result);
        if (Result = MaxToFind) then
          Exit;
      end;

      Inc(Ptr, SizeOf(TRGB32));
    end;

    Inc(RowPtr, RowSize);
  end;
end;

function TFindColorBuffer.Find(out Points: TPointArray; Color: Integer; MaxToFind: Integer): Boolean;
begin
  Result := specialize Finder<TSameColor>(TSameColor.Create(Color), Offset, Points, Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.FindCTS0(out Points: TPointArray; Color, Tolerance: Integer; MaxToFind: Integer): Boolean;
begin
  Result := specialize Finder<TSameColorCTS0>(TSameColorCTS0.Create(Color, Tolerance), Offset, Points, Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.FindCTS1(out Points: TPointArray; Color, Tolerance: Integer; MaxToFind: Integer): Boolean;
begin
  Result := specialize Finder<TSameColorCTS1>(TSameColorCTS1.Create(Color, Tolerance), Offset, Points, Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.FindCTS2(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended; MaxToFind: Integer): Boolean;
begin
  Result := specialize Finder<TSameColorCTS2>(TSameColorCTS2.Create(Color, Tolerance, HueMod, SatMod), Offset, Points, Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.FindCTS3(out Points: TPointArray; Color, Tolerance: Integer; Modifier: Extended; MaxToFind: Integer): Boolean;
begin
  Result := specialize Finder<TSameColorCTS3>(TSameColorCTS3.Create(Color, Tolerance, Modifier), Offset, Points, Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.Count(Color: Integer; MaxToFind: Integer): Integer;
begin
  Result := specialize Counter<TSameColor>(TSameColor.Create(Color), Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.CountCTS0(Color, Tolerance: Integer; MaxToFind: Integer): Integer;
begin
  Result := specialize Counter<TSameColorCTS0>(TSameColorCTS0.Create(Color, Tolerance), Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.CountCTS1(Color, Tolerance: Integer; MaxToFind: Integer): Integer;
begin
  Result := specialize Counter<TSameColorCTS1>(TSameColorCTS1.Create(Color, Tolerance), Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.CountCTS2(Color, Tolerance: Integer; HueMod, SatMod: Extended; MaxToFind: Integer): Integer;
begin
  Result := specialize Counter<TSameColorCTS2>(TSameColorCTS2.Create(Color, Tolerance, HueMod, SatMod), Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

function TFindColorBuffer.CountCTS3(Color, Tolerance: Integer; Modifier: Extended; MaxToFind: Integer): Integer;
begin
  Result := specialize Counter<TSameColorCTS3>(TSameColorCTS3.Create(Color, Tolerance, Modifier), Self.Data, Self.Width, Self.SearchWidth, Self.SearchHeight, MaxToFind);
end;

class operator TFindColorBuffer.Initialize(var Self: TFindColorBuffer);
begin
  Self := Default(TFindColorBuffer);
end;

end.

