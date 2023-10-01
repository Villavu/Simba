unit simba.externalimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes, simba.baseclass, simba.image, simba.simplelock;

type
  TSimbaExternalImageCallback = procedure(Image: Pointer); cdecl;

  PSimbaExternalImage = ^TSimbaExternalImage;
  TSimbaExternalImage = class(TSimbaBaseClass)
  protected
    FImage: TSimbaImage;
    FLock: TSimpleEnterableLock;
    FLockCount: Integer;

    FUnlockCallbacks: array of TSimbaExternalImageCallback;

    function GetFontAntialiasing: Boolean;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetFontName: String;
    function GetFontSize: Single;

    procedure SetFontAntialiasing(Value: Boolean);
    procedure SetFontBold(Value: Boolean);
    procedure SetFontItalic(Value: Boolean);
    procedure SetFontName(Value: String);
    procedure SetFontSize(Value: Single);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddUnlockCallback(Proc: TSimbaExternalImageCallback);
    procedure RemoveUnlockCallback(Proc: TSimbaExternalImageCallback);

    function InternalImage: TSimbaImage;

    function TryLock: Boolean;
    procedure Lock; inline;
    procedure Unlock; inline;

    function Width: Integer; inline;
    function Height: Integer; inline;

    property FontName: String read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontAntialiasing: Boolean read GetFontAntialiasing write SetFontAntialiasing;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;

    procedure SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; ACenter: Boolean; Color: TColor); overload;
    procedure DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);

    procedure Fill(Color: TColor);

    procedure Clear;
    procedure Clear(Area: TBox);
    procedure ClearInverted(Area: TBox);

    procedure Draw(Image: TSimbaImage; Position: TPoint);

    procedure DrawATPA(ATPA: T2DPointArray; Color: TColor = -1);
    procedure DrawTPA(Points: TPointArray; Color: TColor);

    procedure DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor);
    procedure DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor);

    procedure DrawLine(Start, Stop: TPoint; Color: TColor);

    procedure DrawPolygon(Points: TPointArray; Color: TColor);
    procedure DrawPolygonFilled(Points: TPointArray; Color: TColor);
    procedure DrawPolygonInverted(Points: TPointArray; Color: TColor);

    procedure DrawCircle(Circle: TCircle; Color: TColor);
    procedure DrawCircleFilled(Circle: TCircle; Color: TColor);
    procedure DrawCircleInverted(Circle: TCircle; Color: TColor);

    procedure DrawBox(B: TBox; Color: TColor);
    procedure DrawBoxFilled(B: TBox; Color: TColor);
    procedure DrawBoxInverted(B: TBox; Color: TColor);

    procedure DrawQuad(Quad: TQuad; Color: TColor);
    procedure DrawQuadFilled(Quad: TQuad; Color: TColor);
    procedure DrawQuadInverted(Quad: TQuad; Color: TColor);

    procedure DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawCircleArray(Circles: TCircleArray; Filled: Boolean; Color: TColor = -1);
    procedure DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);
  end;

implementation

function TSimbaExternalImage.GetFontAntialiasing: Boolean;
begin
  Lock();
  try
    Result := FImage.FontAntialiasing;
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.GetFontBold: Boolean;
begin
  Lock();
  try
    Result := FImage.FontBold;
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.GetFontItalic: Boolean;
begin
  Lock();
  try
    Result := FImage.FontItalic;
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.GetFontName: String;
begin
  Lock();
  try
    Result := FImage.FontName;
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.GetFontSize: Single;
begin
  Lock();
  try
    Result := FImage.FontSize;
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.SetFontAntialiasing(Value: Boolean);
begin
  Lock();
  try
    FImage.FontAntialiasing := Value;
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.SetFontBold(Value: Boolean);
begin
  Lock();
  try
    FImage.FontBold := Value;
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.SetFontItalic(Value: Boolean);
begin
  Lock();
  try
    FImage.FontItalic := Value;
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.SetFontName(Value: String);
begin
  Lock();
  try
    FImage.FontName := Value;
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.SetFontSize(Value: Single);
begin
  Lock();
  try
    FImage.FontSize := Value;
  finally
    Unlock();
  end;
end;

constructor TSimbaExternalImage.Create;
begin
  inherited Create();

  FImage := TSimbaImage.Create();
end;

destructor TSimbaExternalImage.Destroy;
begin
  if Assigned(FImage) then
    FreeAndNil(FImage);

  inherited Destroy();
end;

procedure TSimbaExternalImage.AddUnlockCallback(Proc: TSimbaExternalImageCallback);
begin
  FLock.Enter();
  FUnlockCallbacks += [Proc];
  FLock.Leave();
end;

procedure TSimbaExternalImage.RemoveUnlockCallback(Proc: TSimbaExternalImageCallback);
var
  i: Integer;
begin
  FLock.Enter();
  for i := High(FUnlockCallbacks) downto 0 do
    if (FUnlockCallbacks[i] = Proc) then
      Delete(FUnlockCallbacks, i, 1);
  FLock.Leave();
end;

function TSimbaExternalImage.TryLock: Boolean;
begin
  Result := FLock.TryEnter();
  if Result then
    Inc(FLockCount);
end;

procedure TSimbaExternalImage.Lock;
begin
  FLock.Enter();
  Inc(FLockCount);
end;

procedure TSimbaExternalImage.Unlock;
var
  i: Integer;
begin
  Dec(FLockCount);
  if (FLockCount = 0) then
    for i := 0 to High(FUnlockCallbacks) do
      FUnlockCallbacks[i](Self);
  FLock.Leave();
end;

function TSimbaExternalImage.InternalImage: TSimbaImage;
begin
  Result := FImage;
end;

function TSimbaExternalImage.Width: Integer;
begin
  Result := FImage.Width;
end;

function TSimbaExternalImage.Height: Integer;
begin
  Result := FImage.Height;
end;

procedure TSimbaExternalImage.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
begin
  Lock();
  try
    FImage.ResetExternalData();
    FImage.SetExternalData(Data, AWidth, AHeight);
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.TextWidth(Text: String): Integer;
begin
  Lock();
  try
    Result := FImage.TextWidth(Text);
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.TextHeight(Text: String): Integer;
begin
  Lock();
  try
    Result := FImage.TextHeight(Text);
  finally
    Unlock();
  end;
end;

function TSimbaExternalImage.TextSize(Text: String): TPoint;
begin
  Lock();
  try
    Result := FImage.TextSize(Text);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  Lock();
  try
    FImage.DrawText(Text, Position, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawText(Text: String; Box: TBox; ACenter: Boolean; Color: TColor);
begin
  Lock();
  try
    FImage.DrawText(Text, Box, ACenter, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
begin
  Lock();
  try
    FImage.DrawTextLines(Text, Position, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.Fill(Color: TColor);
begin
  Lock();
  try
    FImage.Fill(Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.Clear;
begin
  Lock();
  try
    FImage.Clear();
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.Clear(Area: TBox);
begin
  Lock();
  try
    FImage.Clear(Area);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.ClearInverted(Area: TBox);
begin
  Lock();
  try
    FImage.ClearInverted(Area);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.Draw(Image: TSimbaImage; Position: TPoint);
begin
  Lock();
  try
    FImage.Draw(Image, Position);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawATPA(ATPA: T2DPointArray; Color: TColor);
begin
  Lock();
  try
    FImage.DrawATPA(ATPA, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawTPA(Points: TPointArray; Color: TColor);
begin
  Lock();
  try
    FImage.DrawTPA(Points, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCrosshairs(ACenter, Size, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCross(ACenter, Radius, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawLine(Start, Stop: TPoint; Color: TColor);
begin
  Lock();
  try
    FImage.DrawLine(Start, Stop, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawPolygon(Points: TPointArray; Color: TColor);
begin
  Lock();
  try
    FImage.DrawPolygon(Points, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);
begin
  Lock();
  try
    FImage.DrawPolygonFilled(Points, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);
begin
  Lock();
  try
    FImage.DrawPolygonInverted(Points, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCircle(Circle: TCircle; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCircle(Circle, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCircleFilled(Circle: TCircle; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCircleFilled(Circle, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCircleInverted(Circle: TCircle; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCircleInverted(Circle, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawBox(B: TBox; Color: TColor);
begin
  Lock();
  try
    FImage.DrawBox(B, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawBoxFilled(B: TBox; Color: TColor);
begin
  Lock();
  try
    FImage.DrawBoxFilled(B, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawBoxInverted(B: TBox; Color: TColor);
begin
  Lock();
  try
    FImage.DrawBoxInverted(B, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawQuad(Quad: TQuad; Color: TColor);
begin
  Lock();
  try
    FImage.DrawQuad(Quad, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawQuadFilled(Quad: TQuad; Color: TColor);
begin
  Lock();
  try
    FImage.DrawQuadFilled(Quad, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawQuadInverted(Quad: TQuad; Color: TColor);
begin
  Lock();
  try
    FImage.DrawQuadInverted(Quad, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor);
begin
  Lock();
  try
    FImage.DrawQuadArray(Quads, Filled, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor);
begin
  Lock();
  try
    FImage.DrawBoxArray(Boxes, Filled, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor);
begin
  Lock();
  try
    FImage.DrawPolygonArray(Polygons, Filled, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCircleArray(Circles: TCircleArray; Filled: Boolean; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCircleArray(Circles, Filled, Color);
  finally
    Unlock();
  end;
end;

procedure TSimbaExternalImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor);
begin
  Lock();
  try
    FImage.DrawCrossArray(Points, Radius, Color);
  finally
    Unlock();
  end;
end;

end.

