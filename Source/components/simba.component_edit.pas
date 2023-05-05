{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Relatively simple themed TEdit.
}
unit simba.component_edit;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LMessages, LCLType;

type
  TSimbaEdit = class(TCustomControl)
  protected
  type
    {$SCOPEDENUMS ON}
    EPaintCache = (TEXT, CARET_VISIBLE, SEL_START, SEL_END);
    {$SCOPEDENUMS OFF}
  public
    FTextWidthCache: array[EPaintCache] of record
      Str: String;
      Width: Integer;
    end;

    FCaretTimer: TTimer;
    FCaretX: Integer;
    FCaretFlash: Integer;

    FDrawOffsetX: Integer;

    FSelecting: Boolean;
    FSelectingStartX: Integer;
    FSelectingEndX: Integer;

    FHintText: String;
    FHintTextColor: TColor;
    FHintTextStyle: TFontStyles;

    FColorBorder: TColor;
    FColorBorderActive: TColor;
    FColorSelection: TColor;

    FOnChange: TNotifyEvent;

    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;

    procedure DoCaretTimer(Sender: TObject);

    procedure ClearCache;

    function GetTextWidthCache(const Cache: EPaintCache; const Str: String): Integer;

    procedure SelectAll;
    procedure ClearSelection;
    function GetSelectionLen: Integer;
    function HasSelection: Boolean;
    function CharIndexAtXY(X, Y: Integer): Integer;
    function CalculateHeight: Integer;

    function GetAvailableWidth: Integer;
    function GetSelectedText: String;

    procedure AddCharAtCursor(C: Char);
    procedure AddStringAtCursor(Str: String);
    procedure DeleteCharAtCursor;
    procedure DeleteSelection;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure FontChanged(Sender: TObject); override;
    procedure TextChanged; override;
    procedure Paint; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;

    procedure SetCaretPos(Pos: Integer);
    procedure SetColor(Value: TColor); override;
    procedure SetColorBorder(Value: TColor);
    procedure SetColorSelection(Value: TColor);
    procedure SetColorBorderActive(Value: TColor);

    procedure SetHintText(Value: String);
    procedure SetHintTextColor(Value: TColor);
    procedure SetHintTextStyle(Value: TFontStyles);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Clear;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property ColorBorderActive: TColor read FColorBorderActive write SetColorBorderActive;
    property ColorBorder: TColor read FColorBorder write SetColorBorder;
    property ColorSelection: TColor read FColorSelection write SetColorSelection;
    property Color;
    property Font;
    property Text;

    property HintText: String read FHintText write SetHintText;
    property HintTextColor: TColor read FHintTextColor write SetHintTextColor;
    property HintTextStyle: TFontStyles read FHintTextStyle write SetHintTextStyle;
  end;

implementation

uses
  Math, Clipbrd;

procedure TSimbaEdit.SetHintText(Value: String);
begin
  if (FHintText = Value) then
    Exit;
  FHintText := Value;

  Invalidate();
end;

procedure TSimbaEdit.SetHintTextColor(Value: TColor);
begin
  if (FHintTextColor = Value) then
    Exit;
  FHintTextColor := Value;

  Invalidate();
end;

procedure TSimbaEdit.SetHintTextStyle(Value: TFontStyles);
begin
  if (FHintTextStyle = Value) then
    Exit;
  FHintTextStyle := Value;

  Invalidate();
end;

procedure TSimbaEdit.ClearCache;
var
  Cache: EPaintCache;
begin
  for Cache in EPaintCache do
  begin
    FTextWidthCache[Cache].Str   := '';
    FTextWidthCache[Cache].Width := 0;
  end;
end;

procedure TSimbaEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;

  FCaretTimer.Enabled := True;
  Invalidate();
end;

procedure TSimbaEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;

  FCaretTimer.Enabled := False;
  Invalidate();
end;

function TSimbaEdit.GetTextWidthCache(const Cache: EPaintCache; const Str: String): Integer;
begin
  if (Str <> FTextWidthCache[Cache].Str) then
  begin
    FTextWidthCache[Cache].Str := Str;
    FTextWidthCache[Cache].Width := Canvas.TextWidth(Str);
  end;

  Result := FTextWidthCache[Cache].Width;
end;

procedure TSimbaEdit.SelectAll;
begin
  FSelectingStartX := 0;
  FSelectingEndX   := Length(Text);

  Invalidate();
end;

procedure TSimbaEdit.ClearSelection;
begin
  FSelectingStartX := 0;
  FSelectingEndX := 0;
end;

function TSimbaEdit.GetSelectionLen: Integer;
begin
  Result := Abs(FSelectingStartX - FSelectingEndX);
end;

function TSimbaEdit.HasSelection: Boolean;
begin
  Result := GetSelectionLen > 0;
end;

function TSimbaEdit.CharIndexAtXY(X, Y: Integer): Integer;
var
  I, Test: Integer;
  W: Integer;
begin
  Result := Length(Text);

  Test := FDrawOffsetX;
  for I := 1 to Length(Text) do
  begin
    W := Canvas.TextWidth(Text[I]);
    Test += W;
    if ((Test-(W div 2)) >= X) then
    begin
      Result := I-1;
      Exit;
    end;
  end;
end;

function TSimbaEdit.CalculateHeight: Integer;
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(-GetFontData(Canvas.Font.Reference.Handle).Height * 72 / Canvas.Font.PixelsPerInch) + 2;

    Result := Canvas.TextHeight('Olly') + (BorderWidth * 2);
  finally
    Free();
  end;
end;

function TSimbaEdit.GetAvailableWidth: Integer;
begin
  Result := Width - (BorderWidth * 4);
end;

function TSimbaEdit.GetSelectedText: String;
begin
  if (FSelectingStartX > FSelectingEndX) then
    Result := Copy(Text, FSelectingEndX + 1, FSelectingStartX - FSelectingEndX)
  else
    Result := Copy(Text, FSelectingStartX + 1, FSelectingEndX - FSelectingStartX);
end;

procedure TSimbaEdit.AddCharAtCursor(C: Char);
var
  NewText: String;
begin
  if (Ord(C) < 32) then
    Exit;

  Inc(FCaretX);
  NewText := Text;
  Insert(C, NewText, FCaretX);
  Text := NewText;
end;

procedure TSimbaEdit.AddStringAtCursor(Str: String);
var
  NewText: String;
begin
  NewText := Text;

  Insert(Str, NewText, FCaretX + 1);
  Inc(FCaretX, Length(Str));

  Text := NewText;
end;

procedure TSimbaEdit.DeleteCharAtCursor;
var
  NewText: String;
begin
  if (FCaretX >= 1) and (FCaretX <= Length(Text)) then
  begin
    NewText := Text;

    Delete(NewText, FCaretX, 1);
    Dec(FCaretX);

    Text := NewText;
  end;
end;

procedure TSimbaEdit.DeleteSelection;
var
  NewText: String;
begin
  if HasSelection() then
  begin
    NewText := Text;

    if (FSelectingStartX > FSelectingEndX) then
      Delete(NewText, FSelectingEndX + 1, GetSelectionLen())
    else
      Delete(NewText, FSelectingStartX + 1, GetSelectionLen());

    if (FSelectingEndX > FSelectingStartX) then
      SetCaretPos(FCaretX - GetSelectionLen());
    Text := NewText;
    ClearSelection();
  end;
end;

procedure TSimbaEdit.DoCaretTimer(Sender: TObject);
begin
  Invalidate();
end;

procedure TSimbaEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FSelecting then
  begin
    SetCaretPos(CharIndexAtXY(X, Y));

    FSelectingEndX := CharIndexAtXY(X, Y);

    Invalidate();
  end;
end;

procedure TSimbaEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if CanSetFocus() then
    SetFocus();

  I := CharIndexAtXY(X, Y);

  SetCaretPos(I);

  FSelecting := True;
  FSelectingStartX := I;
  FSelectingEndX := I;
end;

procedure TSimbaEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FSelecting := False;
  FSelectingEndX := CharIndexAtXY(X, Y);
end;

procedure TSimbaEdit.SetCaretPos(Pos: Integer);
begin
  if (Pos < 0) then
    FCaretX := 0
  else
  if (Pos > Length(Text)) then
    FCaretX := Length(Text)
  else
    FCaretX := Pos;

  Invalidate();
end;

procedure TSimbaEdit.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  Height := CalculateHeight();
  ClearCache();
end;

procedure TSimbaEdit.TextChanged;
begin
  inherited TextChanged();

  if Assigned(OnChange) then
    OnChange(Self);

  Invalidate();
end;

procedure TSimbaEdit.Paint;

  function IsCaretVisible: Boolean;
  begin
    Result := InRange(FDrawOffsetX + GetTextWidthCache(EPaintCache.CARET_VISIBLE, Copy(Text, 1, FCaretX)), 0, Width);
  end;

var
  Style: TTextStyle;
  OldFontStyles: TFontStyles;
  OldFontColor: TColor;
  TextWidth: Integer;
  X1, X2: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  Style := Canvas.TextStyle;
  Style.Layout := tlCenter;

  // Text
  if (Text <> '') then
  begin
    TextWidth := GetTextWidthCache(EPaintCache.TEXT, Copy(Text, 1, FCaretX));

    if (not IsCaretVisible()) then
    begin
      if (TextWidth > Width) then
        FDrawOffsetX := -(TextWidth - GetAvailableWidth()) + (BorderWidth*2)
      else
        FDrawOffsetX := 0;
    end;

    if (FDrawOffsetX = 0) then
      FDrawOffsetX := (BorderWidth*2);

    // Selection
    if HasSelection() then
    begin
      if (FSelectingStartX > FSelectingEndX) then
      begin
        X1 := FSelectingEndX;
        X2 := FSelectingStartX;
      end else
      if (FSelectingStartX < FSelectingEndX) then
      begin
        X1 := FSelectingStartX;
        X2 := FSelectingEndX;
      end else
      begin
        X1 := FSelectingStartX;
        X2 := FSelectingStartX + 1;
      end;

      X1 := FDrawOffsetX + GetTextWidthCache(EPaintCache.SEL_START, Copy(Text, 1, FSelectingStartX));
      X2 := FDrawOffsetX + GetTextWidthCache(EPaintCache.SEL_END,   Copy(Text, 1, FSelectingEndX));

      Canvas.Brush.Color := FColorSelection;
      Canvas.FillRect(X1, BorderWidth, X2, Height - BorderWidth);
    end;

    Canvas.TextRect(ClientRect, FDrawOffsetX, 0, Text, Style);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(0, 0, 2, Height);
    Canvas.FillRect(Width - 2, 0, Width, Height);

    // Caret
    if FCaretTimer.Enabled then
    begin
      Inc(FCaretFlash);
      if Odd(FCaretFlash) then
        Canvas.Pen.Color := clWhite
      else
        Canvas.Pen.Color := clBlack;
      Canvas.Pen.Mode := pmXor;
      Canvas.Line((FDrawOffsetX + TextWidth) - 1, BorderWidth, (FDrawOffsetX + TextWidth) - 1, Height - BorderWidth);
    end;
  end else
  if (FHintText <> '') then
  begin
    OldFontColor  := Canvas.Font.Color;
    OldFontStyles := Canvas.Font.Style;

    Canvas.Font.Color := FHintTextColor;
    Canvas.Font.Style := FHintTextStyle;

    Canvas.TextRect(ClientRect, BorderWidth*2, 0, FHintText, Style);

    Canvas.Font.Color := OldFontColor;
    Canvas.Font.Style := OldFontStyles;
  end;

  if Focused then
    Canvas.Brush.Color := ColorBorderActive
  else
    Canvas.Brush.Color := ColorBorder;

  Canvas.FrameRect(0, 0, Width, Height);
end;

procedure TSimbaEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (ssCtrl in Shift) then
    case Key of
      VK_A:
        begin
          SelectAll();

          Key := 0;
        end;

      VK_V:
        begin
          AddStringAtCursor(Clipboard.AsText);

          Key := 0;
        end;

      VK_C:
        begin
          Clipboard.AsText := GetSelectedText();

          Key := 0;
        end;
    end;

  case Key of
    VK_BACK:
      begin
        if HasSelection() then
          DeleteSelection()
        else
          DeleteCharAtCursor();

        Key := 0;
      end;

    VK_LEFT:
      begin
        SetCaretPos(FCaretX-1);

        Key := 0;
      end;

    VK_RIGHT:
      begin
        SetCaretPos(FCaretX+1);

        Key := 0;
      end;

    VK_DELETE:
      begin
        DeleteSelection();

        Key := 0;
      end;
  end;
end;

procedure TSimbaEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

  AddCharAtCursor(Key);

  Key := #0;
end;

procedure TSimbaEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);

  AddCharAtCursor(UTF8Decode(UTF8Key)[1]);

  UTF8Key := '';
end;

procedure TSimbaEdit.SetColor(Value: TColor);
begin
  inherited SetColor(Value);

  Invalidate();
end;

procedure TSimbaEdit.SetColorBorder(Value: TColor);
begin
  if (FColorBorder = Value) then
    Exit;
  FColorBorder := Value;

  Invalidate();
end;

procedure TSimbaEdit.SetColorSelection(Value: TColor);
begin
  if (FColorSelection = Value) then
    Exit;
  FColorSelection := Value;

  Invalidate();
end;

procedure TSimbaEdit.SetColorBorderActive(Value: TColor);
begin
  if (FColorBorderActive = Value) then
    Exit;
  FColorBorderActive := Value;
end;

constructor TSimbaEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.Enabled := False;
  FCaretTimer.Interval := 530;
  FCaretTimer.OnTimer := @DoCaretTimer;

  ControlStyle := ControlStyle + [csOpaque];
  Cursor := crIBeam;
  TabStop := True;
  Height := CalculateHeight();

  HintTextStyle := [fsItalic];
  HintTextColor := cl3DDkShadow;

  Color := clWindow;
  ColorSelection := clHighlight;
  ColorBorder := cl3DDkShadow;
  ColorBorderActive := cl3DDkShadow;

  BorderWidth := 3;
end;

procedure TSimbaEdit.Clear;
begin
  Text := '';
end;

end.

