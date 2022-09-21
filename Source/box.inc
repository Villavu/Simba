{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  TBoxHelper = record Helper for TBox
  protected
    function GetCenter: TPoint;
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    class function Create(X1, Y1, X2, Y2: Integer): TBox; static; overload;
    class function Create(Center: TPoint; XRad, YRad: Integer): TBox; static; overload;
    class function Default: TBox; static;

    function ToQuad: TQuad;
    function EqualDimensions(Other: TBox): Boolean;
    function Area: Integer;
    function Expand(SizeMod: Integer): TBox; overload;
    function Expand(SizeMod: Integer; MaxBounds: TBox): TBox; overload;
    function Expand(WidMod, HeiMod: Integer): TBox; overload;
    function Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox; overload;
    function Contains(Other: TBox): Boolean; overload; inline;
    function Contains(Other: TPoint): Boolean; overload; inline;
    function Contains(X, Y: Integer): Boolean; overload; inline;
    function Contains(Other: TQuad): Boolean; overload; inline;

    function Offset(X, Y: Integer): TBox; overload;
    function Offset(P: TPoint): TBox; overload;
    function Combine(Other: TBox): TBox;
    function Invert(Area: TBox): TBoxArray;
    function Partition(Rows, Cols: Integer): TBoxArray;
    function Filter(Points: TPointArray): TPointArray;

    function NearestEdge(P: TPoint): TPoint;
    function Intersect(P: TPoint): TPoint;
    function Corners: TPointArray;

    procedure Clip(Other: TBox);
    procedure Normalize;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Center: TPoint read GetCenter;
  end;

  function Box(X1, Y1, X2, Y2: Integer): TBox;
  function Box(Mid: TPoint; XRad, YRad: Integer): TBox;

  operator = (const Left, Right: TBox): Boolean;
{$ENDIF}

{$IFDEF BODY}
function TBoxHelper.GetCenter: TPoint;
begin
  Result := TPoint.Create(
    (Self.X2 + Self.X1 + 1) div 2,
    (Self.Y2 + Self.Y1 + 1) div 2
  );
end;

function TBoxHelper.GetWidth: Integer;
begin
  Result := (Self.X2 - Self.X1) + 1;
end;

function TBoxHelper.GetHeight: Integer;
begin
  Result := (Self.Y2 - Self.Y1) + 1;
end;

class function TBoxHelper.Create(X1, Y1, X2, Y2: Integer): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

class function TBoxHelper.Create(Center: TPoint; XRad, YRad: Integer): TBox;
begin
  Result.X1 := Center.X - XRad;
  Result.Y1 := Center.Y - YRad;
  Result.X2 := Center.X + XRad;
  Result.Y2 := Center.Y + YRad;
end;

class function TBoxHelper.Default: TBox;
begin
  Result := System.Default(TBox);
end;

function TBoxHelper.ToQuad: TQuad;
begin
  Result.Top    := TPoint.Create(Self.X1, Self.Y1);
  Result.Right  := TPoint.Create(Self.X2, Self.Y1);
  Result.Bottom := TPoint.Create(Self.X2, Self.Y2);
  Result.Left   := TPoint.Create(Self.X1, Self.Y2);
end;

function TBoxHelper.EqualDimensions(Other: TBox): Boolean;
begin
  Result := (Self.Width = Other.Width) and (Self.Height = Other.Height);
end;

function TBoxHelper.Area: Integer;
begin
  Result := (Width * Height);
end;

function TBoxHelper.Expand(SizeMod: Integer): TBox;
begin
  Result.X1 := Self.X1 - SizeMod;
  Result.Y1 := Self.Y1 - SizeMod;
  Result.X2 := Self.X2 + SizeMod;
  Result.Y2 := Self.Y2 + SizeMod;
end;

function TBoxHelper.Expand(SizeMod: Integer; MaxBounds: TBox): TBox;
begin
  Result := Self.Expand(SizeMod);
  Result.Clip(MaxBounds);
end;

function TBoxHelper.Expand(WidMod, HeiMod: Integer): TBox;
begin
  Result.X1 := Self.X1 - WidMod;
  Result.Y1 := Self.Y1 - HeiMod;
  Result.X2 := Self.X2 + WidMod;
  Result.Y2 := Self.Y2 + HeiMod;
end;

function TBoxHelper.Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox;
begin
  Result := Self.Expand(WidMod, HeiMod);
  Result.Clip(MaxBounds);
end;

function TBoxHelper.Contains(Other: TBox): Boolean;
begin
  Result := (Other.X1 >= Self.X1) and (Other.Y1 >= Self.Y1) and (Other.X2 <= Self.X2) and (Other.Y2 <= Self.Y2);
end;

function TBoxHelper.Contains(Other: TPoint): Boolean;
begin
  Result := (Other.X >= Self.X1) and (Other.Y >= Self.Y1) and (Other.X <= Self.X2) and (Other.Y <= Self.Y2);
end;

function TBoxHelper.Contains(X, Y: Integer): Boolean;
begin
  Result := (X >= Self.X1) and (Y >= Self.Y1) and (X <= Self.X2) and (Y <= Self.Y2);
end;

function TBoxHelper.Contains(Other: TQuad): Boolean;
begin
  Result := Contains(Other.Left) and Contains(Other.Right) and Contains(Other.Top) and Contains(Other.Bottom);
end;

function TBoxHelper.Offset(X, Y: Integer): TBox;
begin
  Result.X1 := Self.X1 + X;
  Result.Y1 := Self.Y1 + Y;
  Result.X2 := Self.X2 + X;
  Result.Y2 := Self.Y2 + Y;
end;

function TBoxHelper.Offset(P: TPoint): TBox;
begin
  Result.X1 := Self.X1 + P.X;
  Result.Y1 := Self.Y1 + P.Y;
  Result.X2 := Self.X2 + P.X;
  Result.Y2 := Self.Y2 + P.Y;
end;

function TBoxHelper.Combine(Other: TBox): TBox;
begin
  Result.X1 := Min(Min(Self.X1, Other.X2), Min(Other.X1, Self.X2));
  Result.Y1 := Min(Min(Self.Y1, Other.Y2), Min(Other.Y1, Self.Y2));
  Result.X2 := Max(Max(Self.X1, Other.X2), Max(Other.X1, Self.X2));
  Result.Y2 := Max(Max(Self.Y1, Other.Y2), Max(Other.Y1, Self.Y2));
end;

function TBoxHelper.Invert(Area: TBox): TBoxArray;
var
  B: TBox;
  Boxes: TBoxArray;
  I, Count: Integer;
begin
  Result := System.Default(TBoxArray);

  B.X1 := Max(Area.X1, Self.X1-1);
  B.X2 := Min(Area.X2, Self.X2+1);
  B.Y1 := Max(Area.Y1, Self.Y1-1);
  B.Y2 := Min(Area.Y2, Self.Y2+1);

  Boxes := [
    TBox.Create(Area.X1, Area.Y1, B.X1,    B.Y1),
    TBox.Create(Area.X1, B.Y1,    B.X1,    B.Y2),
    TBox.Create(Area.X1, B.Y1,    B.X1,    Area.Y2),
    TBox.Create(B.X1,    Area.Y1, B.X2,    B.Y1),
    TBox.Create(B.X1,    B.Y2,    B.X2,    Area.Y2),
    TBox.Create(B.X2,    Area.Y1, Area.X2, B.Y1),
    TBox.Create(B.X2,    B.Y1,    Area.X2, B.Y2),
    TBox.Create(B.X2,    B.Y1,    Area.X2, Area.Y2)
  ];

  Count := 0;
  SetLength(Result, Length(Boxes));
  for I := 0 to High(Boxes) do
    with Boxes[I] do
      if (X2 > X1) and (Y2 > Y1) then
      begin
        Result[Count] := Boxes[I];
        Inc(Count);
      end;

  SetLength(Result, Count);
end;

function TBoxHelper.Partition(Rows, Cols: Integer): TBoxArray;
var
  idx,x,y: Integer;
  BoxW, BoxH: Single;
begin
  SetLength(Result, Cols * Rows);
  BoxW := (Self.X2 - Self.X1 + 1) / Cols;
  BoxH := (Self.Y2 - Self.Y1 + 1) / Rows;
  for y:=0 to Rows-1 do
    for x:=0 to Cols-1 do
    begin
      idx := (Y * Cols) + X;
      Result[idx].X1 := Ceil(Self.X1 + (BoxW * x));
      Result[idx].Y1 := Ceil(Self.Y1 + (BoxH * y));
      Result[idx].X2 := Trunc(Self.X1 + (BoxW * x) + BoxW-1);
      Result[idx].Y2 := Trunc(Self.Y1 + (BoxH * y) + BoxH-1);
    end;
end;

function TBoxHelper.Filter(Points: TPointArray): TPointArray;
var
  I, Count: Integer;
begin
  SetLength(Result, Length(Points));
  Count := 0;
  for I := 0 to High(Points) do
    if Contains(Points[I]) then
    begin
      Result[Count] := Points[I];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function TBoxHelper.NearestEdge(P: TPoint): TPoint;
begin
  Result := P;

  if Self.Contains(Result) then
  begin
    if Min(Abs(Self.Y1 - P.Y), Abs(P.Y - Self.Y2)) > Min(Abs(Self.X1 - P.X), Abs(P.X - Self.X2)) then
    begin
      Result.X := Self.X1;
      if (P.X - Self.X1 > Self.X2 - P.X) then
        Result.X := Self.X2;
    end else
     begin
      Result.Y := Self.Y1;
      if (P.Y - Self.Y1 > Self.Y2 - P.Y) then
        Result.Y := Self.Y2;
    end;
  end else
  begin
    if (Result.X < Self.X1) then Result.X := Self.X1;
    if (Result.X > Self.X2) then Result.X := Self.X2;
    if (Result.Y < Self.Y1) then Result.Y := Self.Y1;
    if (Result.Y > Self.Y2) then Result.Y := Self.Y2;
  end;
end;

function TBoxHelper.Intersect(P: TPoint): TPoint;
var
  i: TPoint;
begin
  Result := P;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X1, Self.Y1), TPoint.Create(Self.X2, Self.Y1), i) then Result := i;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X2, Self.Y1), TPoint.Create(Self.X2, Self.Y2), i) then Result := i;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X2, Self.Y2), TPoint.Create(Self.X1, Self.Y2), i) then Result := i;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X1, Self.Y2), TPoint.Create(Self.X1, Self.Y1), i) then Result := i;
end;

function TBoxHelper.Corners: TPointArray;
begin
  Result := [TPoint.Create(X1, Y1), TPoint.Create(X2, Y1), TPoint.Create(X2, Y2), TPoint.Create(X1, Y2)];
end;

procedure TBoxHelper.Clip(Other: TBox);
begin
  if (Self.X1 < Other.X1) then Self.X1 := Other.X1;
  if (Self.X1 > Other.X2) then Self.X1 := Other.X2;
  if (Self.X2 < Other.X1) then Self.X2 := Other.X1;
  if (Self.X2 > Other.X2) then Self.X2 := Other.X2;

  if (Self.Y1 < Other.Y1) then Self.Y1 := Other.Y1;
  if (Self.Y1 > Other.Y2) then Self.Y1 := Other.Y2;
  if (Self.Y2 < Other.Y1) then Self.Y2 := Other.Y1;
  if (Self.Y2 > Other.Y2) then Self.Y2 := Other.Y2;
end;

procedure TBoxHelper.Normalize;
begin
  if (Self.X1 > Self.X2) then
    Swap(Self.X1, Self.X2);
  if (Self.Y1 > Self.Y2) then
    Swap(Self.Y1, Self.Y2);
end;

function Box(X1, Y1, X2, Y2: Integer): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function Box(Mid: TPoint; XRad, YRad: Integer): TBox;
begin
  Result.X1 := Mid.X-XRad;
  Result.Y1 := Mid.Y-YRad;
  Result.X2 := Mid.X+XRad;
  Result.Y2 := Mid.Y+YRad;
end;

operator = (const Left, Right: TBox): Boolean;
begin
  Result := CompareMem(@Left, @Right, SizeOf(TBox));
end;
{$ENDIF}

