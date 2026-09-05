{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildPolygonAA requires "_Row(Y, X1, X2: Integer; Alpha: Byte)"}
{$ENDIF}
{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildPolygonAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildPolygonAA(const Poly: TPolygon; Feather: Single; const Inverted: Boolean; const Clip: TBox);
type
  TEdge = record
    OriginX, OriginY, DirX, DirY, InvLenSq: Single;
    YLo, YHi: Integer;       // the edge's rows, either way up
    NearLo, NearHi: Integer; // the current row's columns within reach, while the edge is active
  end;
  PEdge = ^TEdge;
  TEdgeArray = array of TEdge;
  TCrossing = record
    X: Single;
    Dir: Integer;
  end;
  TCrossingArray = array of TCrossing;
  TSpan = record
    Lo, Hi: Integer;
  end;
  TSpanArray = array of TSpan;

  function BuildEdges(const Poly: TPolygon; out List: TEdgeArray; out Bounds: TBox): Integer;
  var
    Verts: TPointArray;
    I, N, Prev, Next, VertCount, MinX, MinY, MaxX, MaxY: Integer;
    DX, DY, LenSq: Double;
  begin
    Result := 0;
    N := Length(Poly);
    if (N < 3) then
      Exit;

    SetLength(Verts, N);
    VertCount := 0;
    MinX := Poly[0].X;  MaxX := Poly[0].X;
    MinY := Poly[0].Y;  MaxY := Poly[0].Y;
    for I := 0 to N - 1 do
    begin
      if      (Poly[I].X < MinX) then MinX := Poly[I].X
      else if (Poly[I].X > MaxX) then MaxX := Poly[I].X;
      if      (Poly[I].Y < MinY) then MinY := Poly[I].Y
      else if (Poly[I].Y > MaxY) then MaxY := Poly[I].Y;
      if (Poly[I] <> Poly[(I + N - 1) mod N]) then
      begin
        Verts[VertCount] := Poly[I];
        Inc(VertCount);
      end;
    end;

    N := VertCount;
    VertCount := 0;
    for I := 0 to N - 1 do
    begin
      Prev := (I + N - 1) mod N;
      Next := (I + 1) mod N;
      if ((Int64(Verts[I].X) - Verts[Prev].X) * (Int64(Verts[Next].Y) - Verts[I].Y) - (Int64(Verts[I].Y) - Verts[Prev].Y) * (Int64(Verts[Next].X) - Verts[I].X)) <> 0 then
      begin
        Verts[VertCount] := Verts[I];
        Inc(VertCount);
      end;
    end;
    if (VertCount < 3) then
      Exit;

    Bounds := TBox.Create(MinX, MinY, MaxX, MaxY);
    SetLength(List, VertCount);
    for I := 0 to VertCount - 1 do
    begin
      Next := (I + 1) mod VertCount;
      DX := Double(Verts[Next].X) - Verts[I].X;
      DY := Double(Verts[Next].Y) - Verts[I].Y;
      LenSq := DX*DX + DY*DY;
      List[I].OriginX := Verts[I].X;
      List[I].OriginY := Verts[I].Y;
      List[I].DirX := DX;
      List[I].DirY := DY;
      if (LenSq < 1e-12) then
        List[I].InvLenSq := 0
      else
        List[I].InvLenSq := 1 / LenSq;
      List[I].YLo := Min(Verts[I].Y, Verts[Next].Y);
      List[I].YHi := Max(Verts[I].Y, Verts[Next].Y);
    end;
    Result := VertCount;
  end;

  procedure AddSpan(Lo, Hi: Integer; const Band: TBox; var List: TSpanArray; var Count: Integer);
  var
    I: Integer;
  begin
    Lo := Max(Lo, Band.X1);
    Hi := Min(Hi, Band.X2);
    if (Lo > Hi) then
      Exit;
    I := Count;
    while (I > 0) and (List[I - 1].Lo > Lo) do
    begin
      List[I] := List[I - 1];
      Dec(I);
    end;
    List[I].Lo := Lo;
    List[I].Hi := Hi;
    Inc(Count);
    I := 0;
    while (I < Count - 1) do
      if (List[I].Hi + 1 >= List[I + 1].Lo) then
      begin
        List[I].Hi := Max(List[I].Hi, List[I + 1].Hi);
        Dec(Count);
        Move(List[I + 2], List[I + 1], (Count - I - 1) * SizeOf(TSpan));
      end
      else
        Inc(I);
  end;

var
  Edges: TEdgeArray;
  Edge: PEdge;
  ByRow, Active: TIntegerArray;
  Crossings: TCrossingArray;
  Spans: TSpanArray;
  Band: TBox;
  Count, ActiveCount, NextEdge, CrossCount, SpanCount, Margin, Wind: Integer;
  X, XEnd, Y, ActiveIdx, I, SpanIdx, Insert, Passed, WindAcross: Integer;
  RampWidth, Reach, InvRampWidth, PointX, PointY, ToPointX, ToPointY, Proj, NearX, NearY, CrossX: Single;
  DistSq, MinDistSq, SignedDist, ParamLo, ParamHi, Side, Swap: Single;
  RampValue: Double;
  Alpha: Byte;
begin
  RampWidth := 1 + Max(Feather, 0);
  Reach     := RampWidth * 0.5 + 0.01;
  Margin    := Ceil(RampWidth * 0.5) + 1;

  Count := BuildEdges(Poly, Edges, Band);
  if (Count < 3) then
    Band := TBox.Create(Clip.X2 + 1, Clip.Y2 + 1, Clip.X2, Clip.Y2)
  else
    Band := TBox.Create(Max(Clip.X1, Int64(Band.X1) - Margin), Max(Clip.Y1, Int64(Band.Y1) - Margin),
                        Min(Clip.X2, Int64(Band.X2) + Margin), Min(Clip.Y2, Int64(Band.Y2) + Margin));
  if (Band.X1 > Band.X2) or (Band.Y1 > Band.Y2) then
  begin
    if Inverted then
      for Y := Clip.Y1 to Clip.Y2 do
        _Row(Y, Clip.X1, Clip.X2, 255);
    Exit;
  end;
  if Inverted then
  begin
    for Y := Clip.Y1 to Band.Y1 - 1 do
      _Row(Y, Clip.X1, Clip.X2, 255);
    for Y := Band.Y2 + 1 to Clip.Y2 do
      _Row(Y, Clip.X1, Clip.X2, 255);
  end;

  InvRampWidth := 1 / RampWidth;
  if (not Inverted) then
    InvRampWidth := -InvRampWidth;

  SetLength(Active, Count);
  SetLength(Crossings, Count);
  SetLength(Spans, Count);
  SetLength(ByRow, Count);
  for I := 0 to Count - 1 do
  begin
    Insert := I;
    while (Insert > 0) and (Edges[ByRow[Insert - 1]].YLo > Edges[I].YLo) do
    begin
      ByRow[Insert] := ByRow[Insert - 1];
      Dec(Insert);
    end;
    ByRow[Insert] := I;
  end;
  NextEdge := 0;
  ActiveCount := 0;

  for Y := Band.Y1 to Band.Y2 do
  begin
    PointY := Y;
    if Inverted then
    begin
      _Row(Y, Clip.X1, Band.X1 - 1, 255);
      _Row(Y, Band.X2 + 1, Clip.X2, 255);
    end;

    while (NextEdge < Count) and (PointY >= Edges[ByRow[NextEdge]].YLo - Reach) do
    begin
      Active[ActiveCount] := ByRow[NextEdge];
      Inc(ActiveCount);
      Inc(NextEdge);
    end;
    ActiveIdx := 0;
    while (ActiveIdx < ActiveCount) do
      if (PointY > Edges[Active[ActiveIdx]].YHi + Reach) then
      begin
        Dec(ActiveCount);
        Active[ActiveIdx] := Active[ActiveCount];
      end
      else
        Inc(ActiveIdx);

    CrossCount := 0;
    SpanCount  := 0;
    Wind := 0;
    for ActiveIdx := 0 to ActiveCount - 1 do
    begin
      Edge := @Edges[Active[ActiveIdx]];
      if (Edge^.YLo <= Y) and (Y < Edge^.YHi) then
      begin
        ParamLo := (PointY - Edge^.OriginY) / Edge^.DirY;
        CrossX := Edge^.OriginX + ParamLo * Edge^.DirX;
        Insert := CrossCount;
        while (Insert > 0) and (Crossings[Insert - 1].X > CrossX) do
        begin
          Crossings[Insert] := Crossings[Insert - 1];
          Dec(Insert);
        end;
        Crossings[Insert].X := CrossX;
        if (Edge^.DirY > 0) then Crossings[Insert].Dir := 1 else Crossings[Insert].Dir := -1;
        Wind := Wind + Crossings[Insert].Dir;
        Inc(CrossCount);
      end;

      Edge^.NearLo := 1;
      Edge^.NearHi := 0;
      if (Edge^.DirY <> 0) then
      begin
        ParamLo := (PointY - Reach - Edge^.OriginY) / Edge^.DirY;
        ParamHi := (PointY + Reach - Edge^.OriginY) / Edge^.DirY;
        if (ParamLo > ParamHi) then
        begin
          Swap := ParamLo;
          ParamLo := ParamHi;
          ParamHi := Swap;
        end;
        ParamLo := Max(ParamLo, 0);
        ParamHi := Min(ParamHi, 1);
        if (ParamLo <= ParamHi) then
        begin
          Edge^.NearLo := FloorClamp(Min(Edge^.OriginX + ParamLo * Edge^.DirX, Edge^.OriginX + ParamHi * Edge^.DirX) - Reach, Band.X1 - 1, Band.X2 + 1);
          Edge^.NearHi := CeilClamp (Max(Edge^.OriginX + ParamLo * Edge^.DirX, Edge^.OriginX + ParamHi * Edge^.DirX) + Reach, Band.X1 - 1, Band.X2 + 1);
        end;
      end
      else
      begin
        Edge^.NearLo := FloorClamp(Min(Edge^.OriginX, Edge^.OriginX + Edge^.DirX) - Reach, Band.X1 - 1, Band.X2 + 1);
        Edge^.NearHi := CeilClamp (Max(Edge^.OriginX, Edge^.OriginX + Edge^.DirX) + Reach, Band.X1 - 1, Band.X2 + 1);
      end;
      AddSpan(Edge^.NearLo, Edge^.NearHi, Band, Spans, SpanCount);
    end;
    Assert(Wind = 0);

    X := Band.X1;
    SpanIdx := 0;
    Passed := 0;
    Wind := 0;
    while (X <= Band.X2) do
      if (SpanIdx < SpanCount) and (X >= Spans[SpanIdx].Lo) then
      begin
        XEnd := Spans[SpanIdx].Hi;
        Inc(SpanIdx);
        PointX := X;
        while (X <= XEnd) do
        begin
          while (Passed < CrossCount) and (Crossings[Passed].X <= PointX) do
          begin
            Wind := Wind + Crossings[Passed].Dir;
            Inc(Passed);
          end;

          MinDistSq := 1e30;
          for ActiveIdx := 0 to ActiveCount - 1 do
          begin
            Edge := @Edges[Active[ActiveIdx]];
            if (X < Edge^.NearLo) or (X > Edge^.NearHi) then
              Continue;
            ToPointX := PointX - Edge^.OriginX;
            ToPointY := PointY - Edge^.OriginY;
            Side := Edge^.DirX * ToPointY - Edge^.DirY * ToPointX;
            if (Side > 0) then
              WindAcross := Wind + 1
            else if (Side < 0) then
              WindAcross := Wind - 1
            else
              WindAcross := 1 - Abs(Wind);
            if ((WindAcross <> 0) = (Wind <> 0)) then
              Continue;
            Proj := (ToPointX * Edge^.DirX + ToPointY * Edge^.DirY) * Edge^.InvLenSq;
            Proj := Min(Max(Proj, 0.0), 1.0);
            NearX  := Proj * Edge^.DirX - ToPointX;
            NearY  := Proj * Edge^.DirY - ToPointY;
            DistSq := NearX * NearX + NearY * NearY;
            if (DistSq < MinDistSq) then
              MinDistSq := DistSq;
          end;
          SignedDist := Sqrt(MinDistSq);
          if (Wind <> 0) then
            SignedDist := -SignedDist;

          RampValue := (SignedDist * InvRampWidth + 0.5) * 255;
          if (RampValue >= 255) then
            Alpha := 255
          else if (RampValue > 0) then
            Alpha := Round(RampValue)
          else
            Alpha := 0;
          _Pixel(X, Y, Alpha);
          X := X + 1;  PointX := PointX + 1.0;
        end;
      end
      else
      begin
        XEnd := Band.X2;
        if (SpanIdx < SpanCount) then
          XEnd := Spans[SpanIdx].Lo - 1;
        while (Passed < CrossCount) and (Crossings[Passed].X <= X) do
        begin
          Wind := Wind + Crossings[Passed].Dir;
          Inc(Passed);
        end;
        if ((Wind <> 0) <> Inverted) then
          _Row(Y, X, XEnd, 255);
        X := XEnd + 1;
      end;
  end;
end;

{$undef _Row}
{$undef _Pixel}
