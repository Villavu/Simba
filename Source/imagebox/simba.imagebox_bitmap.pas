unit simba.imagebox_bitmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils, graphics, lcltype,
  simba.mufasatypes;

type
  PSimbaImageBoxBitmap = ^TSimbaImageBoxBitmap;
  TSimbaImageBoxBitmap = class(TObject)
  protected
    FBitmap: TBitmap;
    FPixelFormat: String;
    FOffset: TPoint;
    FRect: TRect;

    FZoom: Single;
    FPixelSize: Integer;
    FWidth: Integer;
    FHeight: Integer;

    FData: PByte;
    FBytesPerLine: Integer;
    FBytesPerPixel: Byte;

    FPixelSizeMinus1: Integer;
    FWidthMinus1: Integer;
    FHeightMinus1: Integer;

    function GetHandle: HDC;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate(Rect: TRect; Width, Height, PixelSize: Integer; Zoom: Single);
    procedure EndUpdate;

    procedure DrawLine(Start, Stop: TPoint; Color: TColor);
    procedure DrawCross(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);
    procedure DrawCrossHair(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawBox(Box: TBox; Color: TColor);
    procedure DrawBoxFilled(Box: TBox; Color: TColor);
    procedure DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);
    procedure DrawPoint(P: TPoint; Color: TColor);
    procedure DrawPoints(TPA: TPointArray; Color: TColor);
    procedure DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);

    property Bitmap: TBitmap read FBitmap;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Handle: HDC read GetHandle;
  end;

implementation

uses
  simba.bitmap_misc, simba.colormath, simba.tpa, simba.overallocatearray;

type
  PARGB = ^TARGB;
  TARGB = packed record
    A, R, G, B: Byte;
  end;

  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R : Byte;
  end;

function TSimbaImageBoxBitmap.GetHandle: HDC;
begin
  Result := FBitmap.Canvas.Handle;
end;

constructor TSimbaImageBoxBitmap.Create;
begin
  inherited Create();

  FBitmap := TBitmap.Create();
end;

destructor TSimbaImageBoxBitmap.Destroy;
begin
  if (FBitmap <> nil) then
    FreeAndNil(FBitmap);

  inherited Destroy();
end;

procedure TSimbaImageBoxBitmap.BeginUpdate(Rect: TRect; Width, Height, PixelSize: Integer; Zoom: Single);
begin
  FBitmap.BeginUpdate();
  if (FBitmap.Width < Width) or (FBitmap.Height < Height) then
    FBitmap.SetSize(
       Max(FBitmap.Width,  Width + 150), // over allocate a little
       Max(FBitmap.Height, Height + 150)
     );

  FZoom := Zoom;
  FRect := Rect;
  FWidth  := Width;
  FHeight := Height;
  FPixelSize := PixelSize;

  FWidthMinus1 := FWidth - 1;
  FHeightMinus1 := FHeight - 1;
  FPixelSizeMinus1 := FPixelSize - 1;

  FOffset.X := -FRect.Left;
  FOffset.Y := -FRect.Top;

  FBytesPerLine := FBitmap.RawImage.Description.BytesPerLine;
  FBytesPerPixel := FBitmap.RawImage.Description.BitsPerPixel shr 3;
  FData := FBitmap.RawImage.Data;
  FPixelFormat := GetBitmapPixelFormat(FBitmap);
end;

procedure TSimbaImageBoxBitmap.EndUpdate;
begin
  FBitmap.EndUpdate();
end;

procedure TSimbaImageBoxBitmap.DrawLine(Start, Stop: TPoint; Color: TColor);
begin
  DrawPoints(TPAFromLine(Start, Stop), Color);
end;

procedure TSimbaImageBoxBitmap.DrawCross(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Radius := Radius div 2;

  DrawPoints(
    TPAFromLine(Center.X - Radius, Center.Y - Radius, Center.X + Radius, Center.Y + Radius) +
    TPAFromLine(Center.X + Radius, Center.Y - Radius, Center.X - Radius, Center.Y + Radius),
    Color
  );
end;

procedure TSimbaImageBoxBitmap.DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);
var
  Template, Points: TPointArray;
  P: TPoint;
  I, J, Count: Integer;
  Center: TPoint;
  R: TRect;
begin
  Radius := Radius div 2;

  Template := TPAFromLine(-Radius, -Radius, +Radius, + Radius) +
              TPAFromLine(-Radius, +Radius, +Radius, - Radius);

  SetLength(Points, Length(Centers) * Length(Template));
  Count := 0;

  R := FRect;
  R.Inflate(Radius, Radius);

  for I := 0 to High(Centers) do
  begin
    Center := Centers[I];
    if (Center.X < R.Left) or (Center.Y < R.Top) or (Center.X > R.Right) or (Center.Y > FRect.Bottom) then
      Continue;

    P := Centers[I];
    for J := 0 to High(Template) do
    begin
      Points[Count].X := P.X + Template[J].X;
      Points[Count].Y := P.Y + Template[J].Y;

      Inc(Count);
    end;
  end;
  SetLength(Points, Count);

  DrawPoints(Points, Color);
end;

procedure TSimbaImageBoxBitmap.DrawCrossHair(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Radius := Radius div 2;

  DrawPoints(
    TPAFromLine(Center.X - Radius, Center.Y, Center.X + Radius, Center.Y) +
    TPAFromLine(Center.X, Center.Y - Radius, Center.X, Center.Y + Radius),
    Color
  );
end;

procedure TSimbaImageBoxBitmap.DrawBox(Box: TBox; Color: TColor);
begin
  Box.Normalize();

  DrawPoints(EdgeFromBox(Box), Color);
end;

procedure TSimbaImageBoxBitmap.DrawBoxFilled(Box: TBox; Color: TColor);
begin
  DrawPoints(TPAFromBox(Box), Color);
end;

procedure TSimbaImageBoxBitmap.DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);
var
  TPA: TPointArray;
  I: Integer;
begin
  if (Length(Poly) > 1) then
  begin
    TPA := [];
    for I := 0 to High(Poly) - 1 do
      TPA += TPAFromLine(Poly[I].X, Poly[I].Y, Poly[I+1].X, Poly[I+1].Y);
    if Connect then
      TPA += TPAFromLine(Poly[High(Poly)].X, Poly[High(Poly)].Y, Poly[0].X, Poly[0].Y);

    DrawPoints(TPA, Color);
  end;
end;

procedure TSimbaImageBoxBitmap.DrawPoint(P: TPoint; Color: TColor);
begin
  DrawPoints([P], Color);
end;

procedure TSimbaImageBoxBitmap.DrawPoints(TPA: TPointArray; Color: TColor);

  procedure PixelBGR(const X, Y: Integer; const Pixel: TRGB24); inline;
  var
    LoopX, LoopY, EndX, EndY: Integer;
  begin
    if (FZoom > 1) then
    begin
      EndX := Min(FWidthMinus1,  X + FPixelSizeMinus1);
      EndY := Min(FHeightMinus1, Y + FPixelSizeMinus1);

      for LoopY := Y to EndY do
        for LoopX := X to EndX do
          PRGB24(FData + (LoopY * FBytesPerLine + LoopX * FBytesPerPixel))^ := Pixel;
    end else
      PRGB24(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ := Pixel;
  end;

  procedure PixelBGRA(const X, Y: Integer; const Pixel: TRGB32); inline;
  var
    LoopX, LoopY, EndX, EndY: Integer;
  begin
    if (FZoom > 1) then
    begin
      EndX := Min(FWidthMinus1,  X + FPixelSizeMinus1);
      EndY := Min(FHeightMinus1, Y + FPixelSizeMinus1);

      for LoopY := Y to EndY do
        for LoopX := X to EndX do
          PRGB32(FData + (LoopY * FBytesPerLine + LoopX * FBytesPerPixel))^ := Pixel;
    end else
      PRGB32(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ := Pixel;
  end;

  procedure PixelARGB(const X, Y: Integer; const Pixel: TARGB); inline;
  var
    LoopX, LoopY, EndX, EndY: Integer;
  begin
    if (FZoom > 1) then
    begin
      EndX := Min(FWidthMinus1,  X + FPixelSizeMinus1);
      EndY := Min(FHeightMinus1, Y + FPixelSizeMinus1);

      for LoopY := Y to EndY do
        for LoopX := X to EndX do
          PARGB(FData + (LoopY * FBytesPerLine + LoopX * FBytesPerPixel))^ := Pixel;
    end else
      PARGB(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ := Pixel;
  end;

  procedure BGR;
  var
    Pixel: TRGB24;
    I: Integer;
    X, Y: Integer;
  begin
    Pixel := Default(TRGB24);

    ColorToRGB(Color, Pixel.R, Pixel.G, Pixel.B);

    for I := 0 to High(TPA) do
    begin
      X := TPA[I].X;
      Y := TPA[I].Y;
      if (X < FRect.Left) or (Y < FRect.Top) or (X > FRect.Right) or (Y > FRect.Bottom) then
        Continue;

      PixelBGR(Trunc((X + FOffset.X) * FZoom), Trunc((Y + FOffset.Y) * FZoom), Pixel);
    end;
  end;

  procedure BGRA;
  var
    Pixel: TRGB32;
    I: Integer;
    X, Y: Integer;
  begin
    Pixel := Default(TRGB32);

    ColorToRGB(Color, Pixel.R, Pixel.G, Pixel.B);

    for I := 0 to High(TPA) do
    begin
      X := TPA[I].X;
      Y := TPA[I].Y;
      if (X < FRect.Left) or (Y < FRect.Top) or (X > FRect.Right) or (Y > FRect.Bottom) then
        Continue;

      PixelBGRA(Trunc((X + FOffset.X) * FZoom), Trunc((Y + FOffset.Y) * FZoom), Pixel);
    end;
  end;

  procedure ARGB;
  var
    Pixel: TARGB;
    I: Integer;
    X, Y: Integer;
  begin
    Pixel := Default(TARGB);

    ColorToRGB(Color, Pixel.R, Pixel.G, Pixel.B);

    for I := 0 to High(TPA) do
    begin
      X := TPA[I].X;
      Y := TPA[I].Y;
      if (X < FRect.Left) or (Y < FRect.Top) or (X > FRect.Right) or (Y > FRect.Bottom) then
        Continue;

      PixelARGB(Trunc((X + FOffset.X) * FZoom), Trunc((Y + FOffset.Y) * FZoom), Pixel);
    end;
  end;

begin
  if Length(TPA) = 0 then
    Exit;

  case FPixelFormat of
    'BGR':  BGR();
    'BGRA': BGRA();
    'ARGB': ARGB();
  end;
end;

procedure TSimbaImageBoxBitmap.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);
begin
  DrawPoints(TPAFromCircle(Center, Radius, False), Color);
end;

procedure TSimbaImageBoxBitmap.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);
begin
  DrawPoints(TPAFromCircle(Center, Radius, True), Color);
end;

end.

