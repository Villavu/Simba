unit simba.component_statusbar;


// editor popup menu
// aca,dtm script, rename to ShowACA/ShowDTM

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Grids, LMessages,
  simba.mufasatypes;

type
  TSimbaStatusBar = class(TCustomControl)
  protected
    FPanelCount: Integer;
    FPanelText: TStringArray;
    FPanelTextMeasure: TStringArray;
    FPanelWidths: TIntegerArray;

    FPaintRect: TRect;

    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;

    procedure CalculateSizes;
    procedure Paint; override;
    procedure PaintPanel(Index: Integer);

    procedure InvalidatePanel(Index: Integer);
    function PanelRect(Index: Integer): TRect;

    procedure CheckIndex(Index: Integer);
    function GetPanelText(Index: Integer): String;
    function GetPanelTextMeasure(Index: Integer): String;
    procedure SetPanelText(Index: Integer; Value: String);
    procedure SetPanelTextMeasure(Index: Integer; Value: String);

    procedure SetPanelCount(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure EraseBackground(DC: HDC); override;

    property PanelText[Index: Integer]: String read GetPanelText write SetPanelText;
    property PanelTextMeasure[Index: Integer]: String read GetPanelTextMeasure write SetPanelTextMeasure;
    property PanelCount: Integer read FPanelCount write SetPanelCount;
  end;

implementation

uses
  LCLIntf;

procedure TSimbaStatusBar.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FPanelCount) then
    SimbaException('TSimbaStatusBar.CheckIndex: Index %d out of range', [Index]);
end;

function TSimbaStatusBar.GetPanelText(Index: Integer): String;
begin
  Result := FPanelText[Index];
end;

function TSimbaStatusBar.GetPanelTextMeasure(Index: Integer): String;
begin
  Result := FPanelTextMeasure[Index];
end;

procedure TSimbaStatusBar.SetPanelText(Index: Integer; Value: String);
begin
  if (FPanelText[Index] = Value) then
    Exit;

  FPanelText[Index] := Value;

  InvalidatePanel(Index);
end;

procedure TSimbaStatusBar.SetPanelTextMeasure(Index: Integer; Value: String);
begin
  FPanelTextMeasure[Index] := Value;

  CalculateSizes();
end;

procedure TSimbaStatusBar.SetPanelCount(Value: Integer);
begin
  FPanelCount := Value;

  SetLength(FPanelText, FPanelCount);
  SetLength(FPanelTextMeasure, FPanelCount);
  SetLength(FPanelWidths, FPanelCount);
end;

constructor TSimbaStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  CalculateSizes();
end;

procedure TSimbaStatusBar.WMPaint(var Message: TLMPaint);
begin
  if Assigned(Message.PaintStruct) then
    FPaintRect := Message.PaintStruct^.rcPaint
  else
    FPaintRect := ClientRect;

  inherited;
end;

procedure TSimbaStatusBar.CalculateSizes;
var
  I: Integer;
begin
  with TBitmap.Create() do
  try
    // Measure on larger font size
    // Font size can be 0 so use GetFontData
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(-GetFontData(Canvas.Font.Reference.Handle).Height * 72 / Canvas.Font.PixelsPerInch) + 3;

    for I := 0 to FPanelCount - 1 do
      FPanelWidths[I] := Canvas.TextWidth(FPanelTextMeasure[I]);

    Self.Height := Canvas.TextHeight('TaylorSwift');
  finally
    Free();
  end;
end;

procedure TSimbaStatusBar.EraseBackground(DC: HDC);
begin
  { nothing }
end;

procedure TSimbaStatusBar.Paint;
var
  I: Integer;
begin
  if (FPanelCount = 0) then
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := $4A4136;
    Canvas.Line(0, 0, Width, 0);
    Canvas.FillRect(0, 1, Width, Height);
  end else
  begin
    for I := 0 to FPanelCount - 1 do
      if (FPaintRect = PanelRect(I)) then
      begin
        PaintPanel(I);
        Exit;
      end;

    // Paint everything
    for I := 0 to FPanelCount - 1 do
      PaintPanel(I);
  end;
end;

procedure TSimbaStatusBar.PaintPanel(Index: Integer);
var
  Style: TTextStyle;
  R: TRect;
begin
  Style := Canvas.TextStyle;
  Style.Layout := tlCenter;

  Canvas.Font.Color := clWhite;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := $4A4136;

  R := PanelRect(Index);

  Canvas.FillRect(R);
  Canvas.Line(R.Left, R.Top, R.Right, R.Top);
  Canvas.Line(R.Right, 0, R.Right, Height);
  Canvas.TextRect(R, R.Left + 5, R.Top, FPanelText[Index], Style);
end;

procedure TSimbaStatusBar.InvalidatePanel(Index: Integer);
var
  R: TRect;
begin
  R := PanelRect(Index);

  InvalidateRect(Handle, @R, False);
end;

function TSimbaStatusBar.PanelRect(Index: Integer): TRect;
var
  I: Integer;
begin
  CheckIndex(Index);

  Result.Left := 0;
  Result.Top := 0;
  Result.Bottom := Height;

  for I := 0 to Index do
  begin
    if (FPanelWidths[I] = 0) then
      Result.Right := BoundsRect.Right
    else
      Result.Right := Result.Left + FPanelWidths[I];

    if (I < Index) then
      Result.Left := Result.Right + 1;
  end;
end;

end.

