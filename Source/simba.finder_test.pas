unit simba.finder_test;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes, simba.colormath_conversion, simba.finder_color_new, simba.bitmap;

type
  PColorSpace = ^TColorSpace;
  TColorSpace = record
    ColorSpace: EColorSpace;
    Multipliers: TChannelMultipliers;
  end;

  PColorTolerance = ^TColorTolerance;
  TColorTolerance = record
    Color: Integer;
    Tolerance: Single;
    ColorSpace: EColorSpace;
    Multipliers: TChannelMultipliers;
  end;

  {$PUSH}
  {$SCOPEDENUMS ON}
  ETargetType = (NONE, BITMAP, WINDOW);
  {$POP}

  PSimbaFinder = ^TSimbaFinder;
  TSimbaFinder = record
  private
    FTargetType: ETargetType;
    FTarget: record
      Bitmap: TMufasaBitmap;
      Window: TWindowHandle;
    end;
    FColorFinder: TColorFinder;

    function DoFindColor(Bounds: TBox): TPointArray;
    function DoCountColor(Bounds: TBox): Integer;

    procedure FreeData(var Data: PColorBGRA);
    function ValidateTargetBounds(var Bounds: TBox): Boolean;
    function GetTargetData(var Bounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
  public
    procedure SetTarget(Bitmap: TMufasaBitmap);
    procedure SetTarget(Window: TWindowHandle);

    function FindColor(Color: TColor; Bounds: TBox): TPointArray; // RGB Colorpace
    function FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray; // RGB Colorpace

    function FindColor(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox): TPointArray;
    function FindColorEx(Color: TColorTolerance; Bounds: TBox): TPointArray;

    function CountColor(Color: TColor; Bounds: TBox): Integer; // RGB Colorpace
    function CountColor(Color: TColor; Tolerance: Single; Bounds: TBox): Integer; // RGB Colorpace

    function CountColor(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox): Integer;
    function CountColorEx(Color: TColorTolerance; Bounds: TBox): Integer;

    function GetColor(X, Y: Integer): TColor;
    function GetColors(Points: TPointArray): TColorArray;
    function GetColorsMatrix(Bounds: TBox): TIntegerMatrix;
  end;

implementation

uses
  simba.nativeinterface, simba.tpa;

function TSimbaFinder.DoFindColor(Bounds: TBox): TPointArray;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetTargetData(Bounds, Data, DataWidth) then
  begin
    Result := FColorFinder.Find(Data, DataWidth, Bounds.Width, Bounds.Height, Bounds.TopLeft);

    FreeData(Data);
  end else
    Result := nil;
end;

function TSimbaFinder.DoCountColor(Bounds: TBox): Integer;
var
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  if GetTargetData(Bounds, Data, DataWidth) then
  begin
    Result := FColorFinder.Count(Data, DataWidth, Bounds.Width, Bounds.Height);

    FreeData(Data);
  end else
    Result := 0;
end;

procedure TSimbaFinder.FreeData(var Data: PColorBGRA);
begin
  if (FTargetType in [ETargetType.WINDOW]) then
    FreeMem(Data);
end;

function TSimbaFinder.ValidateTargetBounds(var Bounds: TBox): Boolean;
var
  Width, Height: Integer;
  B: TBox;
begin
  Result := False;

  case FTargetType of
    ETargetType.BITMAP:
      begin
        Result := True;

        Width := FTarget.Bitmap.Width;
        Height := FTarget.Bitmap.Height;
      end;

    ETargetType.WINDOW:
      begin
        Result := SimbaNativeInterface.GetWindowBounds(FTarget.Window, B);

        Width := B.Width;
        Height := B.Height;
      end;

    else
      raise Exception.Create('TSimbaFinder: Target not set');
  end;

  if Result then
  begin
    if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    begin
      Bounds.X1 := 0;
      Bounds.Y1 := 0;
      Bounds.X2 := Width -1;
      Bounds.Y2 := Height - 1;
    end else
    begin
      if (Bounds.X1 < 0) then Bounds.X1 := 0;
      if (Bounds.Y1 < 0) then Bounds.Y1 := 0;
      if (Bounds.X2 >= Width)  then Bounds.X2 := Width - 1;
      if (Bounds.Y2 >= Height) then Bounds.Y2 := Height - 1;
    end;
  end;
end;

function TSimbaFinder.GetTargetData(var Bounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
begin
  Result := False;
  Data := nil;

  if ValidateTargetBounds(Bounds) then
  begin
    case FTargetType of
      ETargetType.BITMAP:
        begin
          DataWidth := FTarget.Bitmap.Width;
          Data := FTarget.Bitmap.Data + (Bounds.Y1 * FTarget.Bitmap.Width) + Bounds.X1;

          Result := True;
        end;

      ETargetType.WINDOW:
        begin
          DataWidth := Bounds.Width;

          Result := SimbaNativeInterface.GetWindowImage(FTarget.Window, Bounds.X1, Bounds.Y1, Bounds.Width, Bounds.Height, Data);
        end;
      else
        raise Exception.Create('TSimbaFinder: Target not set');
    end;
  end;
end;

procedure TSimbaFinder.SetTarget(Bitmap: TMufasaBitmap);
begin
  FTargetType := ETargetType.BITMAP;
  FTarget.Bitmap := Bitmap;
end;

procedure TSimbaFinder.SetTarget(Window: TWindowHandle);
begin
  FTargetType := ETargetType.WINDOW;
  FTarget.Window := Window;
end;

function TSimbaFinder.FindColor(Color: TColor; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(EColorSpace.RGB, Color);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(EColorSpace.RGB, Color, Tolerance);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(ColorSpace.ColorSpace, Color, Tolerance);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColorEx(Color: TColorTolerance; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.CountColor(Color: TColor; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(EColorSpace.RGB, Color);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(EColorSpace.RGB, Color, Tolerance);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(ColorSpace.ColorSpace, Color, Tolerance);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.CountColorEx(Color: TColorTolerance; Bounds: TBox): Integer;
begin
  FColorFinder.Setup(Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);

  Result := DoCountColor(Bounds);
end;

function TSimbaFinder.GetColor(X, Y: Integer): TColor;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
begin
  B.X1 := X; B.Y1 := Y;
  B.X2 := X; B.Y2 := Y;

  if GetTargetData(B, Data, DataWidth) then
    Result := Data^.ToColor()
  else
    Result := -1;
end;

function TSimbaFinder.GetColors(Points: TPointArray): TColorArray;
var
  B: TBox;
  Data: PColorBGRA;
  DataWidth: Integer;
  I, X, Y, Count: Integer;
begin
  Result := nil;

  B := Points.Bounds();
  if GetTargetData(B, Data, DataWidth) then
  begin
    SetLength(Result, Length(Points));

    Count := 0;
    for I := 0 to High(Points) do
    begin
      X := Points[I].X - B.X1;
      Y := Points[I].Y - B.Y1;

      Result[Count] := Data[Y * DataWidth + X].ToColor();
      Inc(Count);
    end;
    SetLength(Result, Count);
  end;
end;

function TSimbaFinder.GetColorsMatrix(Bounds: TBox): TIntegerMatrix;
var
  Data: PColorBGRA;
  DataWidth: Integer;
  Width, Height, X, Y: Integer;
begin
  Result := nil;

  if GetTargetData(Bounds, Data, DataWidth) then
  begin
    Width := Bounds.Width - 1;
    Height := Bounds.Height - 1;

    Result.SetSize(Width + 1, Height + 1);

    for Y := 0 to Height do
      for X := 0 to Width do
        Result[Y, X] := Data[Y * DataWidth + X].ToColor();
  end;
end;

end.

