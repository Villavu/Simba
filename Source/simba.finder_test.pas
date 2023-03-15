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

    procedure FreeData(var Data: PColorBGRA);
    function ValidateTargetBounds(var Bounds: TBox): Boolean;
    function GetTargetData(var Bounds: TBox; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
  public
    procedure SetTarget(Bitmap: TMufasaBitmap);
    procedure SetTarget(Window: TWindowHandle);

    function FindColors(Color: TColor; Bounds: TBox): TPointArray; // RGB colorspace
    function FindColors(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray; // RGB colorspace

    function FindColors(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox): TPointArray;
    function FindColorsEx(Color: TColorTolerance; Bounds: TBox): TPointArray;

    function GetColor(X, Y: Integer): TColor;
    function GetColors(Points: TPointArray): TColorArray;
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

  if (Bounds.X1 < 0) then Bounds.X1 := 0;
  if (Bounds.Y1 < 0) then Bounds.Y1 := 0;

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
    if (Bounds.X2 >= Width)  then Bounds.X2 := Width - 1;
    if (Bounds.Y2 >= Height) then Bounds.Y2 := Height - 1;
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

function TSimbaFinder.FindColors(Color: TColor; Bounds: TBox): TPointArray;
const
  DefaultMultipliers: TChannelMultipliers = (1,1,1);
begin
  FColorFinder.Setup(EColorSpace.RGB, Color, 1, DefaultMultipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColors(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray;
const
  DefaultMultipliers: TChannelMultipliers = (1,1,1);
begin
  FColorFinder.Setup(EColorSpace.RGB, Color, Tolerance, DefaultMultipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColors(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(ColorSpace.ColorSpace, Color, Tolerance, ColorSpace.Multipliers);

  Result := DoFindColor(Bounds);
end;

function TSimbaFinder.FindColorsEx(Color: TColorTolerance; Bounds: TBox): TPointArray;
begin
  FColorFinder.Setup(Color.ColorSpace, Color.Color, Color.Tolerance, Color.Multipliers);

  Result := DoFindColor(Bounds);
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

end.

