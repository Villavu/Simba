unit simba.imagebox_bitmap;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType,
  simba.mufasatypes, simba.bitmap_misc, simba.colormath_conversion;

type
  PSimbaImageBoxBitmap = ^TSimbaImageBoxBitmap;
  TSimbaImageBoxBitmap = class(TObject)
  protected
  type
    PBGR = ^TBGR;
    TBGR = packed record
      B,G,R: Byte;
    end;
    PBGRA = ^TBGRA;
    TBGRA = packed record
      B,G,R,A: Byte;
    end;
    PARGB = ^TARGB;
    TARGB = packed record
      A,R,G,B: Byte;
    end;
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

    function BGR(Color: TColor): TBGR; inline;
    function BGRA(Color: TColor): TBGRA; inline;
    function ARGB(Color: TColor): TARGB; inline;

    procedure PixelBGR(X, Y: Integer; const Pixel: TBGR); inline;
    procedure PixelBGRA(X, Y: Integer; const Pixel: TBGRA); inline;
    procedure PixelARGB(X, Y: Integer; const Pixel: TARGB); inline;

    procedure PixelBGRTransparency(X, Y: Integer; const T, RMod, GMod, BMod: Single); inline;
    procedure PixelBGRATransparency(X, Y: Integer; const T, RMod, GMod, BMod: Single); inline;
    procedure PixelARGBTransparency(X, Y: Integer; const T, RMod, GMod, BMod: Single); inline;

    function GetHandle: HDC;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate(Rect: TRect; Width, Height, PixelSize: Integer; Zoom: Single);
    procedure EndUpdate;

    procedure DrawLine(Start, Stop: TPoint; Color: TColor);
    procedure DrawLineGap(Start, Stop: TPoint; Gap: Integer; Color: TColor);
    procedure DrawCross(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);
    procedure DrawCrossHair(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawBox(Box: TBox; Color: TColor);
    procedure DrawBoxTransparent(Box: TBox; Color: TColor; Transparency: Single);
    procedure DrawBoxFilled(Box: TBox; Color: TColor);
    procedure DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);
    procedure DrawPoint(P: TPoint; Color: TColor);
    procedure DrawPoints(TPA: TPointArray; Color: TColor);
    procedure DrawEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Color: TColor);
    procedure DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);
    procedure DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);

    property Bitmap: TBitmap read FBitmap;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Handle: HDC read GetHandle;
  end;

implementation

{$DEFINE MACRO_PIXEL :=
  var
    StartX, StartY, EndX, EndY: Integer;
  begin
    X := Trunc((X + FOffset.X) * FZoom);
    Y := Trunc((Y + FOffset.Y) * FZoom);

    if (FZoom > 1) then
    begin
      StartX := X;
      StartY := Y;
      EndX := Min(FWidthMinus1,  X + FPixelSizeMinus1);
      EndY := Min(FHeightMinus1, Y + FPixelSizeMinus1);

      for Y := StartY to EndY do
        for X := StartX to EndX do
          if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
            PixelType(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ := Pixel;
    end else
      if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
        PixelType(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ := Pixel;
  end;
}

{$DEFINE MACRO_PIXEL_TRANSPARENCY :=
  var
    StartX, StartY, EndX, EndY: Integer;
  begin
    X := Trunc((X + FOffset.X) * FZoom);
    Y := Trunc((Y + FOffset.Y) * FZoom);

    if (FZoom > 1) then
    begin
      StartX := X;
      StartY := Y;
      EndX := Min(FWidthMinus1,  X + FPixelSizeMinus1);
      EndY := Min(FHeightMinus1, Y + FPixelSizeMinus1);

      for Y := StartY to EndY do
        for X := StartX to EndX do
          if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
            with PixelType(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ do
            begin
              R := Byte(Round((R * T) + RMod));
              G := Byte(Round((G * T) + GMod));
              B := Byte(Round((B * T) + BMod));
            end;
    end else
      if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
        with PixelType(FData + (Y * FBytesPerLine + X * FBytesPerPixel))^ do
        begin
          R := Byte(Round((R * T) + RMod));
          G := Byte(Round((G * T) + GMod));
          B := Byte(Round((B * T) + BMod));
        end;
  end;
}

{$DEFINE PixelType := PBGR}
procedure TSimbaImageBoxBitmap.PixelBGR(X, Y: Integer; const Pixel: TBGR);   MACRO_PIXEL
{$DEFINE PixelType := PBGRA}
procedure TSimbaImageBoxBitmap.PixelBGRA(X, Y: Integer; const Pixel: TBGRA); MACRO_PIXEL
{$DEFINE PixelType := PARGB}
procedure TSimbaImageBoxBitmap.PixelARGB(X, Y: Integer; const Pixel: TARGB); MACRO_PIXEL

{$DEFINE PixelType := PBGR}
procedure TSimbaImageBoxBitmap.PixelBGRTransparency(X, Y: Integer; const T, RMod, GMod, BMod: Single);  MACRO_PIXEL_TRANSPARENCY
{$DEFINE PixelType := PBGRA}
procedure TSimbaImageBoxBitmap.PixelBGRATransparency(X, Y: Integer; const T, RMod, GMod, BMod: Single); MACRO_PIXEL_TRANSPARENCY
{$DEFINE PixelType := PARGB}
procedure TSimbaImageBoxBitmap.PixelARGBTransparency(X, Y: Integer; const T, RMod, GMod, BMod: Single); MACRO_PIXEL_TRANSPARENCY

function TSimbaImageBoxBitmap.BGR(Color: TColor): TBGR;
begin
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
end;

function TSimbaImageBoxBitmap.BGRA(Color: TColor): TBGRA;
begin
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
  Result.A := 0;
end;

function TSimbaImageBoxBitmap.ARGB(Color: TColor): TARGB;
begin
  Result.A := 0;
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
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
  FWidth := Width;
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

  {$DEFINE MACRO_LINE :=
  var
    DX, DY, Step, I: Integer;
    RX, RY, X, Y: Single;
  begin
    DX := (Stop.X - Start.X);
    DY := (Stop.Y - Start.Y);
    if (Abs(DX) > Abs(DY)) then
      Step := Abs(DX)
    else
      Step := Abs(DY);

    if (Step = 0) then
    begin
      RX := DX;
      RY := DY;
    end else
    begin
      RX := DX / Step;
      RY := DY / Step;
    end;
    X := Start.X;
    Y := Start.Y;

    PixelProc(Round(X), Round(Y), Color);
    for I := 1 to Step do
    begin
      X := X + RX;
      Y := Y + RY;

      PixelProc(Round(X), Round(Y), Color);
    end;
  end;
  }

  {$DEFINE PixelProc := PixelBGR}
  procedure DrawBGR(Color: TBGR);   MACRO_LINE
  {$DEFINE PixelProc := PixelBGRA}
  procedure DrawBGRA(Color: TBGRA); MACRO_LINE
  {$DEFINE PixelProc := PixelARGB}
  procedure DrawARGB(Color: TARGB); MACRO_LINE

begin
  case FPixelFormat of
    'BGR':  DrawBGR(BGR(Color));
    'BGRA': DrawBGRA(BGRA(Color));
    'ARGB': DrawARGB(ARGB(Color));
  end;
end;

procedure TSimbaImageBoxBitmap.DrawLineGap(Start, Stop: TPoint; Gap: Integer; Color: TColor);

  {$DEFINE MACRO_LINE :=
  var
    DX, DY, Step, I, G: Integer;
    RX, RY, X, Y: Single;
  begin
    DX := (Stop.X - Start.X);
    DY := (Stop.Y - Start.Y);
    if (Abs(DX) > Abs(DY)) then
      Step := Abs(DX)
    else
      Step := Abs(DY);

    if (Step = 0) then
    begin
      RX := DX;
      RY := DY;
    end else
    begin
      RX := DX / Step;
      RY := DY / Step;
    end;
    X := Start.X;
    Y := Start.Y;
    G := 0;

    PixelProc(Round(X), Round(Y), Color);
    for I := 1 to Step do
    begin
      X := X + RX;
      Y := Y + RY;

      Inc(G);
      if (G > 0) then
      begin
        PixelProc(Round(X), Round(Y), Color);
        if (G > Gap) then
          G := -Gap;
      end;
    end;

    PixelProc(Round(X), Round(Y), Color);
  end;
  }

  {$DEFINE PixelProc := PixelBGR}
  procedure DrawBGR(Color: TBGR);   MACRO_LINE
  {$DEFINE PixelProc := PixelBGRA}
  procedure DrawBGRA(Color: TBGRA); MACRO_LINE
  {$DEFINE PixelProc := PixelARGB}
  procedure DrawARGB(Color: TARGB); MACRO_LINE

begin
  case FPixelFormat of
    'BGR':  DrawBGR(BGR(Color));
    'BGRA': DrawBGRA(BGRA(Color));
    'ARGB': DrawARGB(ARGB(Color));
  end;
end;

procedure TSimbaImageBoxBitmap.DrawCross(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Radius := Radius div 2;

  DrawLine(TPoint.Create(Center.X - Radius, Center.Y - Radius), TPoint.Create(Center.X + Radius, Center.Y + Radius), Color);
  DrawLine(TPoint.Create(Center.X + Radius, Center.Y - Radius), TPoint.Create(Center.X - Radius, Center.Y + Radius), Color);
end;

procedure TSimbaImageBoxBitmap.DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);
var
  I: Integer;
  Center: TPoint;
  R: TRect;
begin
  R := FRect;
  R.Inflate(Radius, Radius);

  Radius := Radius div 2;
  for I := 0 to High(Centers) do
  begin
    Center := Centers[I];
    if (Center.X < R.Left) or (Center.Y < R.Top) or (Center.X > R.Right) or (Center.Y > FRect.Bottom) then
      Continue;

    DrawLine(TPoint.Create(Center.X - Radius, Center.Y - Radius), TPoint.Create(Center.X + Radius, Center.Y + Radius), Color);
    DrawLine(TPoint.Create(Center.X + Radius, Center.Y - Radius), TPoint.Create(Center.X - Radius, Center.Y + Radius), Color);
  end;
end;

procedure TSimbaImageBoxBitmap.DrawCrossHair(Center: TPoint; Radius: Integer; Color: TColor);
begin
  Radius := Radius div 2;

  DrawLine(TPoint.Create(Center.X - Radius, Center.Y), TPoint.Create(Center.X + Radius, Center.Y), Color);
  DrawLine(TPoint.Create(Center.X, Center.Y - Radius), TPoint.Create(Center.X, Center.Y + Radius), Color);
end;

procedure TSimbaImageBoxBitmap.DrawBox(Box: TBox; Color: TColor);
begin
  Box.Normalize();

  DrawLine(TPoint.Create(Box.X1, Box.Y1), TPoint.Create(Box.X2, Box.Y1), Color);
  DrawLine(TPoint.Create(Box.X2, Box.Y1), TPoint.Create(Box.X2, Box.Y2), Color);
  DrawLine(TPoint.Create(Box.X2, Box.Y2), TPoint.Create(Box.X1, Box.Y2), Color);
  DrawLine(TPoint.Create(Box.X1, Box.Y2), TPoint.Create(Box.X1, Box.Y1), Color);
end;

procedure TSimbaImageBoxBitmap.DrawBoxTransparent(Box: TBox; Color: TColor; Transparency: Single);
var
  RMod, GMod, BMod: Single;

  {$DEFINE MACRO_PIXELS :=
    var
      X, Y: Integer;
    begin
      for Y := Box.Y1 to Box.Y2 - 1 do
        for X := Box.X1 to Box.X2 - 1 do
        begin
          if ((FZoom >= 1) or ((Y mod FPixelSize = 0) and (X mod FPixelSize = 0))) then
            PixelProc(X, Y, Transparency, RMod, GMod, BMod);
        end;
    end;
  }

  {$DEFINE PixelProc := PixelBGRTransparency}
  procedure DrawBGR;  MACRO_PIXELS
  {$DEFINE PixelProc := PixelBGRATransparency}
  procedure DrawBGRA; MACRO_PIXELS
  {$DEFINE PixelProc := PixelARGBTransparency}
  procedure DrawARGB; MACRO_PIXELS

begin
  Box.Normalize();

  RMod := ((Color and $FF)        * Transparency);
  GMod := ((Color shr 8 and $FF)  * Transparency);
  BMod := ((Color shr 16 and $FF) * Transparency);

  Transparency := 1.0 - Transparency;

  case FPixelFormat of
    'BGR':  DrawBGR();
    'BGRA': DrawBGRA();
    'ARGB': DrawARGB();
  end;
end;

procedure TSimbaImageBoxBitmap.DrawBoxFilled(Box: TBox; Color: TColor);

  {$DEFINE MACRO_PIXELS :=
    var
      X, Y: Integer;
    begin
      for Y := Box.Y1 to Box.Y2 do
        for X := Box.X1 to Box.X2 do
          PixelProc(X, Y, Color);
    end;
  }

  {$DEFINE PixelProc := PixelBGR}
  procedure DrawBGR(Color: TBGR);   MACRO_PIXELS
  {$DEFINE PixelProc := PixelBGRA}
  procedure DrawBGRA(Color: TBGRA); MACRO_PIXELS
  {$DEFINE PixelProc := PixelARGB}
  procedure DrawARGB(Color: TARGB); MACRO_PIXELS

begin
  Box.Normalize();

  case FPixelFormat of
    'BGR':  DrawBGR(BGR(Color));
    'BGRA': DrawBGRA(BGRA(Color));
    'ARGB': DrawARGB(ARGB(Color));
  end;
end;

procedure TSimbaImageBoxBitmap.DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);
var
  I: Integer;
begin
  if (Length(Poly) <= 1) then
    Exit;

  for I := 0 to High(Poly) - 1 do
    DrawLine(Poly[I], Poly[I+1], Color);
  if Connect then
    DrawLine(Poly[High(Poly)], Poly[0], Color);
end;

procedure TSimbaImageBoxBitmap.DrawPoint(P: TPoint; Color: TColor);
begin
  DrawPoints([P], Color);
end;

procedure TSimbaImageBoxBitmap.DrawPoints(TPA: TPointArray; Color: TColor);

  procedure DrawBGR(const Color: TBGR);
  var
    I: Integer;
  begin
    for I := 0 to High(TPA) do
      PixelBGR(TPA[I].X, TPA[I].Y, Color);
  end;

  procedure DrawBGRA(const Color: TBGRA);
  var
    I: Integer;
  begin
    for I := 0 to High(TPA) do
      PixelBGRA(TPA[I].X, TPA[I].Y, Color);
  end;

  procedure DrawARGB(const Color: TARGB);
  var
    I: Integer;
  begin
    for I := 0 to High(TPA) do
      PixelARGB(TPA[I].X, TPA[I].Y, Color);
  end;

begin
  if (Length(TPA) = 0) then
    Exit;

  case FPixelFormat of
    'BGR':  DrawBGR(BGR(Color));
    'BGRA': DrawBGRA(BGRA(Color));
    'ARGB': DrawARGB(ARGB(Color));
  end;
end;

 procedure TSimbaImageBoxBitmap.DrawEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Color: TColor);

  {$DEFINE MACRO_ELLIPSE :=
    var
      RadXSq, RadYSq: Integer;
      TwoSqX, TwoSqY: Integer;
      X, Y, P, PX, PY: Integer;
    begin
      RadXSq := RadiusX * RadiusX;
      RadYSq := RadiusY * RadiusY;
      TwoSqX := 2 * RadXSq;
      TwoSqY := 2 * RadYSq;
      X  := 0;
      Y  := RadiusY;
      PX := 0;
      PY := TwoSqX * Y;

      PixelProc(Center.Y+Y, Center.X+X, Color);
      PixelProc(Center.Y+Y, Center.X-X, Color);
      PixelProc(Center.Y-Y, Center.X+X, Color);
      PixelProc(Center.Y-Y, Center.X-X, Color);

      P := Round(RadYSQ - (RadXSQ * RadiusY) + (0.25 * RadXSQ));
      while PX<PY do
      begin
        Inc(X);
        Inc(PX, TwoSqY);

        if (P < 0) then
          Inc(P, RadYSq + PX)
        else begin
          Dec(Y);
          Dec(PY, TwoSqX);
          Inc(P, RadYSq + PX - PY);
        end;

        // Filled
        //DrawLine(TPoint.Create(Center.Y+Y,Center.X+X), TPoint.Create(Center.Y+Y,Center.X-X), Color);
        //DrawLine(TPoint.Create(Center.Y-Y,Center.X+X), TPoint.Create(Center.Y-Y,Center.X-X), Color);

        PixelProc(Center.Y+Y, Center.X+X, Color);
        PixelProc(Center.Y+Y, Center.X-X, Color);
        PixelProc(Center.Y-Y, Center.X+X, Color);
        PixelProc(Center.Y-Y, Center.X-X, Color);
      end;

      P := Round(RadYSQ * Sqr(X + 0.5) + RadXSQ * Sqr(Y - 1) - RadXSQ * RadYSQ);
      while (Y>0) do
      begin
        Dec(Y);
        Dec(PY, TwoSqX);
        if (P > 0) then
          Inc(P, RadXSq - PY)
        else begin
          Inc(X);
          Inc(PX, TwoSqY);
          Inc(P, RadXSq - PY + PX);
        end;

        // Filled
        //DrawLine(TPoint.Create(Center.Y+Y,Center.X+X), TPoint.Create(Center.Y+Y,Center.X-X), Color);
        //DrawLine(TPoint.Create(Center.Y-Y,Center.X+X), TPoint.Create(Center.Y-Y,Center.X-X), Color);

        PixelProc(Center.Y+Y,Center.X+X, Color);
        PixelProc(Center.Y+Y,Center.X-X, Color);
        PixelProc(Center.Y-Y,Center.X+X, Color);
        PixelProc(Center.Y-Y,Center.X-X, Color);
      end;
    end;
  }

  {$DEFINE PixelProc := PixelBGR}
  procedure DrawBGR(Color: TBGR);   MACRO_ELLIPSE
  {$DEFINE PixelProc := PixelBGRA}
  procedure DrawBGRA(Color: TBGRA); MACRO_ELLIPSE
  {$DEFINE PixelProc := PixelARGB}
  procedure DrawARGB(Color: TARGB); MACRO_ELLIPSE

begin
  case FPixelFormat of
    'BGR':  DrawBGR(BGR(Color));
    'BGRA': DrawBGRA(BGRA(Color));
    'ARGB': DrawARGB(ARGB(Color));
  end;
end;

procedure TSimbaImageBoxBitmap.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);
begin
  case FPixelFormat of
    'BGR':  DrawEllipse(Center, Radius, Radius, Color);
    'BGRA': DrawEllipse(Center, Radius, Radius, Color);
    'ARGB': DrawEllipse(Center, Radius, Radius, Color);
  end;
end;

procedure TSimbaImageBoxBitmap.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);

  {$DEFINE MACRO_CIRCLE_FILLED :=
    var
      X, Y: Integer;
      SqRad: Single;
    begin
      SqRad := Trunc(Sqr(Radius + 0.5));
      with TBox.Create(Center, Radius, Radius) do
        for Y := Y1 to Y2 do
          for X := X1 to X2 do
            if Sqr(X - Center.X) + Sqr(Y - Center.Y) < SqRad then
              PixelProc(X, Y, Color);
    end;
  }

  {$DEFINE PixelProc := PixelBGR}
  procedure DrawBGR(Color: TBGR);   MACRO_CIRCLE_FILLED
  {$DEFINE PixelProc := PixelBGRA}
  procedure DrawBGRA(Color: TBGRA); MACRO_CIRCLE_FILLED
  {$DEFINE PixelProc := PixelARGB}
  procedure DrawARGB(Color: TARGB); MACRO_CIRCLE_FILLED

begin
  case FPixelFormat of
    'BGR':  DrawBGR(BGR(Color));
    'BGRA': DrawBGRA(BGRA(Color));
    'ARGB': DrawARGB(ARGB(Color));
  end;
end;

end.

