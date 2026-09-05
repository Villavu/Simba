{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildPolygon requires "_Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

(*
  Fills a polygon with the current colour, or when Inverted everything within Clip that is not the polygon. Scan-line with the nonzero winding rule (the rule of TPolygon.Contains): a pixel is inside when the
  boundary winds around it, so concave and self-crossing polygons fill everything they enclose.
  https://www.tutorialspoint.com/computer_graphics/polygon_filling_algorithm.html
*)
procedure _BuildPolygon(const APoints: TPointArray; const Inverted: Boolean; const Clip: TBox);
var
  Top, Dir, TopX: TIntegerArray;
  Height, Run: array of Int64;
  Crossings, Dirs: TIntegerArray;
  EdgeCount, N, I, Prev, Insert, Count, Y, YMin, YMax, ScanY1, ScanY2, X, Cursor, Wind: Integer;
begin
  N := Length(APoints);
  if (N < 3) then
  begin
    if Inverted then
      for Y := Clip.Y1 to Clip.Y2 do
        _Row(Y, Clip.X1, Clip.X2);
    Exit;
  end;

  SetLength(Top, N);
  SetLength(Height, N);
  SetLength(Run, N);
  SetLength(Dir, N);
  SetLength(TopX, N);
  EdgeCount := 0;
  YMin := APoints[0].Y;
  YMax := YMin;
  Prev := N - 1;
  for I := 0 to N - 1 do
  begin
    if (APoints[I].Y < YMin) then
      YMin := APoints[I].Y
    else if (APoints[I].Y > YMax) then
      YMax := APoints[I].Y;

    if (APoints[I].Y > APoints[Prev].Y) then
    begin
      Top[EdgeCount]    := APoints[Prev].Y;
      Height[EdgeCount] := Int64(APoints[I].Y) - APoints[Prev].Y;
      TopX[EdgeCount]   := APoints[Prev].X;
      Run[EdgeCount]    := Int64(APoints[I].X) - APoints[Prev].X;
      Dir[EdgeCount]    := 1;
      Inc(EdgeCount);
    end
    else if (APoints[I].Y < APoints[Prev].Y) then
    begin
      Top[EdgeCount]    := APoints[I].Y;
      Height[EdgeCount] := Int64(APoints[Prev].Y) - APoints[I].Y;
      TopX[EdgeCount]   := APoints[I].X;
      Run[EdgeCount]    := Int64(APoints[Prev].X) - APoints[I].X;
      Dir[EdgeCount]    := -1;
      Inc(EdgeCount);
    end;
    Prev := I;
  end;
  ScanY1 := Max(Clip.Y1, YMin);
  ScanY2 := Min(Clip.Y2, YMax);
  SetLength(Crossings, EdgeCount);
  SetLength(Dirs, EdgeCount);

  if Inverted then
  begin
    for Y := Clip.Y1 to ScanY1 - 1 do
      _Row(Y, Clip.X1, Clip.X2);
    for Y := ScanY2 + 1 to Clip.Y2 do
      _Row(Y, Clip.X1, Clip.X2);
  end;

  for Y := ScanY1 to ScanY2 do
  begin
    Count := 0;
    for I := 0 to EdgeCount - 1 do
      if (Y >= Top[I]) and (Y < Top[I] + Height[I]) then
      begin
        X := RoundHalfUp(Max(Min(TopX[I] + (Int64(Y) - Top[I]) * Run[I] / Height[I], Clip.X2 + 1.0), Clip.X1 - 1.0));   // held to Clip: a crossing can pass the Integer limits
        Insert := Count;
        while (Insert > 0) and (Crossings[Insert - 1] > X) do
        begin
          Crossings[Insert] := Crossings[Insert - 1];
          Dirs[Insert] := Dirs[Insert - 1];
          Dec(Insert);
        end;
        Crossings[Insert] := X;
        Dirs[Insert] := Dir[I];
        Inc(Count);
      end;

    Wind := 0;
    if (not Inverted) then
      Cursor := Low(Integer)
    else
      Cursor := Clip.X1;
    for I := 0 to Count - 1 do
    begin
      if (Wind = 0) then
      begin
        X := Crossings[I];
        if Inverted and (X > Cursor) then
          _Row(Y, Cursor, X - 1);
      end;
      Wind := Wind + Dirs[I];
      if (Wind = 0) then
      begin
        if (not Inverted) then
        begin
          X := Max(X, Cursor + 1);
          if (X <= Crossings[I]) then
            _Row(Y, X, Crossings[I]);
          Cursor := Crossings[I];
        end
        else
          Cursor := Max(Cursor, Crossings[I] + 1);
      end;
    end;
    Assert(Wind = 0);
    if Inverted and (Cursor <= Clip.X2) then
      _Row(Y, Cursor, Clip.X2);
  end;
end;

{$undef _Row}
