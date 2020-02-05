unit simba.imagebox_overlay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, extctrls,
  simba.imagebox, simba.bitmap, simba.client, simba.mufasatypes, simba.dtm;

type
  TSimbaImageOverlayBenchmark = record Matches: Int32; Time: Double; end;
  TSimbaImageOverlay = class
  protected
    FImage: TImage;
    FBuffer: TBitmap;
    FClient: TClient;

    function GetBrush: TBrush;
    function GetPen: TPen;
  public
    property Buffer: TBitmap read FBuffer;

    property Brush: TBrush read GetBrush;
    property Pen: TPen read GetPen;

    procedure Clear;
    procedure Update;

    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure FillRect(X1, Y1, X2, Y2: Integer);

    procedure DrawPoints(TPA: TPointArray);

    function DebugColorCTS0(Color: TColor; Tolerance: Int32): TSimbaImageOverlayBenchmark;
    function DebugColorCTS1(Color: TColor; Tolerance: Int32): TSimbaImageOverlayBenchmark;
    function DebugColorCTS2(Color: TColor; Tolerance: Int32; Hue, Sat: Extended): TSimbaImageOverlayBenchmark;

    function DebugDTM(DTM: TMDTM): TSimbaImageOverlayBenchmark;

    constructor Create(Image: TImage; Client: TClient);
    destructor Destroy; override;
  end;

  TSimbaImageBox_Overlay = class(TSimbaImageBox)
  protected
    FOverlay: TSimbaImageOverlay;
    FBitmap: TMufasaBitmap;
    FClient: TClient;
  public
    property Image: TMufasaBitmap read FBitmap;
    property Overlay: TSimbaImageOverlay read FOverlay;

    procedure Changed; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  simba.colormath, simba.misc;

procedure TSimbaImageBox_Overlay.Changed;
begin
  FOverlay.Buffer.LoadFromRawImage(FBitmap.ToRawImage(), False);
  FOverlay.Clear();

  inherited Changed();
end;

constructor TSimbaImageBox_Overlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBitmap := TMufasaBitmap.Create();
  FClient := TClient.Create();
  FClient.IOManager.SetTarget(FBitmap);

  FOverlay := TSimbaImageOverlay.Create(FImage, FClient);
end;

destructor TSimbaImageBox_Overlay.Destroy;
begin
  FBitmap.Free();
  FClient.Free();

  inherited Destroy();
end;

function TSimbaImageOverlay.GetBrush: TBrush;
begin
  Result := FImage.Picture.Bitmap.Canvas.Brush;
end;

function TSimbaImageOverlay.GetPen: TPen;
begin
  Result := FImage.Picture.Bitmap.Canvas.Pen;
end;

procedure TSimbaImageOverlay.Clear;
begin
  FImage.Picture.Bitmap.SetSize(FBuffer.Width, FBuffer.Height);
  FImage.Picture.Bitmap.Canvas.Draw(0, 0, FBuffer);
end;

procedure TSimbaImageOverlay.Update;
begin
  FImage.Update();
end;

procedure TSimbaImageOverlay.Line(X1, Y1, X2, Y2: Integer);
begin
  FImage.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
end;

procedure TSimbaImageOverlay.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FImage.Picture.Bitmap.Canvas.FillRect(X1, Y1, X2, Y2);
end;

procedure TSimbaImageOverlay.DrawPoints(TPA: TPointArray);
var
  Data: PByte;
  BytesPerLine, BytesPerPixel: Int32;
  Color: TRGB24;
  P: TPoint;
begin
  with FImage.Picture.Bitmap do
  begin
    BeginUpdate(False);

    try
      Data := RawImage.Data;
      BytesPerLine := RawImage.Description.BytesPerLine;
      BytesPerPixel := RawImage.Description.BitsPerPixel div 8;

      ColorToRGB(Self.Pen.Color, Color.R, Color.G, Color.B);
      for P in TPA do
        Move(Color, Data[P.Y * BytesPerLine + P.X * BytesPerPixel], SizeOf(TRGB24));
    finally
      EndUpdate();
    end;
  end;
end;

function TSimbaImageOverlay.DebugColorCTS0(Color: TColor; Tolerance: Int32): TSimbaImageOverlayBenchmark;
var
  W, H: Int32;
  TPA: TPointArray;
begin
  FClient.IOManager.GetDimensions(W, H);
  FClient.MFinder.SetToleranceSpeed(0);

  Result.Time := PerformanceTimer();

  if (FClient <> nil) then
    FClient.MFinder.FindColorsTolerance(TPA, Color, 0, 0, W-1, H-1, Tolerance);

  Result.Time := PerformanceTimer() - Result.Time;
  Result.Matches := Length(TPA);

  Clear();
  DrawPoints(TPA);
end;

function TSimbaImageOverlay.DebugColorCTS1(Color: TColor; Tolerance: Int32): TSimbaImageOverlayBenchmark;
var
  W, H: Int32;
  TPA: TPointArray;
begin
  FClient.IOManager.GetDimensions(W, H);
  FClient.MFinder.SetToleranceSpeed(1);

  Result.Time := PerformanceTimer();

  if (FClient <> nil) then
    FClient.MFinder.FindColorsTolerance(TPA, Color, 0, 0, W-1, H-1, Tolerance);

  Result.Time := PerformanceTimer() - Result.Time;
  Result.Matches := Length(TPA);

  Clear();
  DrawPoints(TPA);
end;

function TSimbaImageOverlay.DebugColorCTS2(Color: TColor; Tolerance: Int32; Hue, Sat: Extended): TSimbaImageOverlayBenchmark;
var
  W, H: Int32;
  TPA: TPointArray;
begin
  FClient.IOManager.GetDimensions(W, H);
  FClient.MFinder.SetToleranceSpeed(2);
  FClient.MFinder.SetToleranceSpeed2Modifiers(Hue, Sat);

  Result.Time := PerformanceTimer();

  if (FClient <> nil) then
    FClient.MFinder.FindColorsTolerance(TPA, Color, 0, 0, W-1, H-1, Tolerance);

  Result.Time := PerformanceTimer() - Result.Time;
  Result.Matches := Length(TPA);

  Clear();
  DrawPoints(TPA);
end;

function TSimbaImageOverlay.DebugDTM(DTM: TMDTM): TSimbaImageOverlayBenchmark;
var
  W, H: Int32;
  TPA: TPointArray;
  P: TPoint;
begin
  FClient.IOManager.GetDimensions(W, H);
  FClient.MFinder.SetToleranceSpeed(1);

  Result.Time := PerformanceTimer();

  FClient.MFinder.FindDTMs(DTM, TPA, 0, 0, W-1, H-1);

  Result.Time := PerformanceTimer() - Result.Time;
  Result.Matches := Length(TPA);

  Clear();

  FImage.Picture.Bitmap.BeginUpdate(True);

  for P in TPA do
  begin
    FImage.Picture.Bitmap.Canvas.Line(P.X - 4, P.Y - 4, P.X + 5, P.Y + 5);
    FImage.Picture.Bitmap.Canvas.Line(P.X + 4, P.Y - 4, P.X - 5, P.Y + 5);
  end;

  FImage.Picture.Bitmap.EndUpdate();
end;

destructor TSimbaImageOverlay.Destroy;
begin
  FBuffer.Free();

  inherited Destroy();
end;

constructor TSimbaImageOverlay.Create(Image: TImage; Client: TClient);
begin
  inherited Create();

  FImage := Image;
  FBuffer := TBitmap.Create();
  FClient := Client;
end;

end.

