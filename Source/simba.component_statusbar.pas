{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Custom drawn status bar.
}
unit simba.component_statusbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LCLType, LMessages,
  simba.base;

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
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    procedure PaintPanel(Index: Integer);

    procedure InvalidatePanelASync(Data: PtrInt);
    function PanelRect(Index: Integer): TRect;

    procedure CheckIndex(Index: Integer);
    function GetPanelText(Index: Integer): String;
    function GetPanelTextMeasure(Index: Integer): String;
    procedure SetPanelText(Index: Integer; Value: String);
    procedure SetPanelTextMeasure(Index: Integer; Value: String);

    procedure SetPanelCount(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EraseBackground(DC: HDC); override;

    property PanelText[Index: Integer]: String read GetPanelText write SetPanelText;
    property PanelTextMeasure[Index: Integer]: String read GetPanelTextMeasure write SetPanelTextMeasure;
    property PanelCount: Integer read FPanelCount write SetPanelCount;
  end;

implementation

uses
  LCLIntf, ATCanvasPrimitives,
  simba.ide_theme;

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

  Application.QueueAsyncCall(@InvalidatePanelASync, Index);
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

destructor TSimbaStatusBar.Destroy;
begin
  Application.RemoveAsyncCalls(Self);

  inherited Destroy();
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
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(Abs(GetFontData(Canvas.Font.Handle).Height) * 72 / Canvas.Font.PixelsPerInch) + 4; // Measure on larger font size - Font size can be 0

    for I := 0 to FPanelCount - 1 do
      FPanelWidths[I] := Canvas.TextWidth(FPanelTextMeasure[I]);

    Self.Height := Canvas.TextHeight('Tay');
  finally
    Free();
  end;
end;

procedure TSimbaStatusBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  CalculateSizes();
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
    Canvas.Pen.Color := ColorBlendHalf(SimbaTheme.ColorFrame, SimbaTheme.ColorLine);
    Canvas.Brush.Color := SimbaTheme.ColorFrame;
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

  Canvas.Font.Color := SimbaTheme.ColorFont;
  Canvas.Pen.Color := ColorBlendHalf(SimbaTheme.ColorFrame, SimbaTheme.ColorLine);
  Canvas.Brush.Color := SimbaTheme.ColorFrame;

  R := PanelRect(Index);

  Canvas.FillRect(R);
  Canvas.Line(R.Left, R.Top, R.Right, R.Top);
  Canvas.Line(R.Right, 0, R.Right, Height);
  Canvas.TextRect(R, R.Left + 5, R.Top, FPanelText[Index], Style);
end;

procedure TSimbaStatusBar.InvalidatePanelASync(Data: PtrInt);
var
  R: TRect;
begin
  if not HandleAllocated then
    Exit;

  R := PanelRect(Data);
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
