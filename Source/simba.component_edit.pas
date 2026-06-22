{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Custom drawn TEdit like component.
}
unit simba.component_edit;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LMessages, LCLType;

type
  (*
   Fully custom drawn "TEdit" component.
   Pretty crude and somewhat hacky but works quite well.
  *)
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
    FOnUserChange: TNotifyEvent;

    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;

    procedure ClearCache;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    function GetTextWidthCache(const Cache: EPaintCache; const Str: String): Integer;

    procedure SelectAll;
    procedure SelectWord;
    procedure ClearSelection;
    function GetSelectionLen: Integer;
    function HasSelection: Boolean;
    function CharIndexAtXY(X, Y: Integer): Integer;
    function CalculateHeight: Integer;

    function GetAvailableWidth: Integer;
    function GetSelectedText: String;

    procedure AddCharAtCursor(C: Char);
    procedure AddStringAtCursor(Str: String; ADeleteSelection: Boolean = False);
    procedure DeleteCharAtCursor;
    procedure DeleteSelection;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    procedure ParentFontChanged; override;

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
    destructor Destroy; override;

    procedure Clear;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;

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

  (*
   Label on the left with rest of the space being the edit.
   - Optional button can be added on the far right
   - If LabelMeasure is assigned this will be used to measure the placement of the edit.
  *)
  TSimbaLabeledEdit = class(TCustomControl)
  protected
    FButton: TControl;
    FLabel: TPanel;
    FLabelMeasure: String;
    FEdit: TSimbaEdit;

    procedure DoPaintLabel(Sender: TObject);
    procedure DoMeasure;

    procedure TextChanged; override;
    procedure FontChanged(Sender: TObject); override;

    procedure SetButton(AValue: TControl);
    procedure SetLabelMeasure(Value: String);
  public
    constructor Create(AOwner: TComponent); override;

    property Button: TControl read FButton write SetButton;
    property Edit: TSimbaEdit read FEdit;
    property LabelMeasure: String read FLabelMeasure write SetLabelMeasure;
  end;

implementation

uses
  Math, Clipbrd,
  simba.component_theme,
  simba.misc,
  simba.initializations;

type
  TCaretFlasher = class
  protected
    FTimer: TTimer;
    FList: TList;

    procedure DoTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Edit: TSimbaEdit);
    procedure Remove(Edit: TSimbaEdit);
  end;

var
  EditCaretFlasher: TCaretFlasher;

procedure TCaretFlasher.DoTimer(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FList.Count -1 do
    TSimbaEdit(FList[I]).Invalidate();
end;

constructor TCaretFlasher.Create;
begin
  inherited Create();

  FList := TList.Create();
end;

destructor TCaretFlasher.Destroy;
begin
  if (FList <> nil) then
    FreeAndNil(FList);
  if (FTimer <> nil) then
    FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TCaretFlasher.Add(Edit: TSimbaEdit);
begin
  FList.Add(Edit);

  if (FTimer = nil) then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.OnTimer := @DoTimer;
    FTimer.Interval := 500;
  end;
  FTimer.Enabled := True;
end;

procedure TCaretFlasher.Remove(Edit: TSimbaEdit);
begin
  FList.Remove(Edit);
  if (FList.Count = 0) and (FTimer <> nil) then
    FTimer.Enabled := False;
end;

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

procedure TSimbaEdit.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredHeight := CalculateHeight();
end;

procedure TSimbaEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;

  EditCaretFlasher.Add(Self);
  Invalidate();
end;

procedure TSimbaEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;

  EditCaretFlasher.Remove(Self);
  ClearSelection();
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
  FSelecting := False;
  FSelectingStartX := 0;
  FSelectingEndX   := Length(Text);

  Invalidate();
end;

procedure TSimbaEdit.SelectWord;
var
  I, StartX, EndX: Integer;
  Str: String;
begin
  ClearSelection();

  if (Text <> '') and (FCaretX >= 1) and (FCaretX <= Length(Text)) then
  begin
    Str := Text;

    StartX := 0;
    for I := FCaretX downto 1 do
      if (Str[I] <= #32) then
      begin
        StartX := I;
        Break;
      end;
    EndX := Length(Str);
    for I := FCaretX to Length(Str) do
      if (Str[I] <= #32) then
      begin
        EndX := I-1;
        Break;
      end;

    FSelectingStartX := StartX;
    FSelectingEndX := EndX;
    FCaretX := EndX;
  end;

  Invalidate();
end;

procedure TSimbaEdit.ClearSelection;
begin
  FSelecting := False;
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
    Canvas.Font.Size := GetFontSize(Self, 1);

    Result := Canvas.TextHeight('Tay') + (BorderWidth * 2);
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

  if HasSelection then
    DeleteSelection();

  Inc(FCaretX);
  NewText := Text;
  Insert(C, NewText, FCaretX);
  Text := NewText;

  if Assigned(FOnUserChange) then
    FOnUserChange(Self);
end;

procedure TSimbaEdit.AddStringAtCursor(Str: String; ADeleteSelection: Boolean);
var
  NewText: String;
begin
  if ADeleteSelection then
    DeleteSelection();

  NewText := Text;

  Insert(Str, NewText, FCaretX + 1);
  Inc(FCaretX, Length(Str));

  Text := NewText;

  if Assigned(FOnUserChange) then
    FOnUserChange(Self);
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

    if Assigned(FOnUserChange) then
      FOnUserChange(Self);
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

    if Assigned(FOnUserChange) then
      FOnUserChange(Self);
  end;
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

  if (not (ssDouble in Shift)) then
  begin
    I := CharIndexAtXY(X, Y);

    SetCaretPos(I);

    FSelecting := True;
    FSelectingStartX := I;
    FSelectingEndX := I;
  end;
end;

procedure TSimbaEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (not (ssDouble in Shift)) then
  begin
    FSelecting := False;
    FSelectingEndX := CharIndexAtXY(X, Y);
  end;
end;

procedure TSimbaEdit.DblClick;
begin
  inherited DblClick();

  SelectWord();
end;

procedure TSimbaEdit.ParentFontChanged;
begin
  inherited ParentFontChanged;

  if Assigned(Parent) then
  begin
    Font.BeginUpdate();
    Font := Parent.Font;
    Font.Color := SimbaComponentTheme.ColorFont;
    Font.EndUpdate();
  end;
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
var
  NewHeight: Integer;
begin
  inherited FontChanged(Sender);

  NewHeight := CalculateHeight();

  Constraints.MinHeight := NewHeight;
  Constraints.MaxHeight := NewHeight;

  ClearCache();
end;

procedure TSimbaEdit.TextChanged;
begin
  inherited TextChanged();

  if Assigned(FOnChange) then
    FOnChange(Self);

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
  DrawCaretX: Integer;
begin
  Canvas.Font := Font;
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
      FDrawOffsetX := (BorderWidth * 2);

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

  if Focused then // flash caret and active border
  begin
    Inc(FCaretFlash);
    if Odd(FCaretFlash) then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    Canvas.Pen.Mode := pmXor;

    if (Text = '') then
      DrawCaretX := (BorderWidth * 2)
    else
      DrawCaretX := (FDrawOffsetX + TextWidth) - 1;

    Canvas.Line(DrawCaretX, (BorderWidth*2), DrawCaretX, Height - (BorderWidth*2));

    Canvas.Brush.Color := ColorBorderActive;
  end else
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
          AddStringAtCursor(Clipboard.AsText, True);

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

  // Only consume printable characters - let control keys (Tab, Escape, ...) fall through
  if (Key >= #32) then
  begin
    AddCharAtCursor(Key);
    Key := #0;
  end;
end;

procedure TSimbaEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);

  if (UTF8Key <> '') and (UTF8Key[1] >= #32) then
  begin
    AddCharAtCursor(UTF8Decode(UTF8Key)[1]);
    UTF8Key := '';
  end;
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

  ControlStyle := ControlStyle + [csOpaque];
  Cursor := crIBeam;
  TabStop := True;
  BorderWidth := 2;

  Font.Color := SimbaComponentTheme.ColorFont;

  Color := SimbaComponentTheme.ColorBackground;
  ColorBorder := SimbaComponentTheme.ColorBackground;
  ColorBorderActive := SimbaComponentTheme.ColorActive;
  ColorSelection := SimbaComponentTheme.ColorActive;

  HintTextStyle := [fsItalic];
  HintTextColor := clLtGray;

  Height := CalculateHeight();
end;

destructor TSimbaEdit.Destroy;
begin
  EditCaretFlasher.Remove(Self);

  inherited Destroy();
end;

procedure TSimbaEdit.Clear;
begin
  Text := '';
end;

procedure TSimbaLabeledEdit.SetButton(AValue: TControl);
begin
  if (FButton = AValue) then
    Exit;

  // remove from this control, does not free though!
  if (FButton <> nil) then
    FButton.Parent := nil;
  FButton := AValue;

  if (FButton <> nil) then
  begin
    FButton.Parent := Self;
    FButton.Align := alRight;
    FButton.BorderSpacing.Left := 5;

    FEdit.AnchorSide[akRight].Control := FButton;
    FEdit.AnchorSide[akRight].Side := asrLeft;
  end else
  begin
    FEdit.AnchorSide[akRight].Control := Self;
    FEdit.AnchorSide[akRight].Side := asrRight;
  end;
end;

procedure TSimbaLabeledEdit.DoPaintLabel(Sender: TObject);
var
  Style: TTextStyle;
begin
  with TPanel(Sender) do
  begin
    Style := Canvas.TextStyle;
    Style.Layout := tlCenter;

    Canvas.TextRect(ClientRect, 0, 0, Self.Caption, Style);
  end;
end;

procedure TSimbaLabeledEdit.DoMeasure;
begin
  if (FEdit = nil) then
    Exit;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    if (FLabelMeasure <> '') then
      FLabel.Width := Canvas.TextWidth(FLabelMeasure)
    else
      FLabel.Width := Canvas.TextWidth(Caption);
  finally
    Free();
  end;
end;

procedure TSimbaLabeledEdit.TextChanged;
begin
  inherited TextChanged();

  DoMeasure();
end;

procedure TSimbaLabeledEdit.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  DoMeasure();
end;

procedure TSimbaLabeledEdit.SetLabelMeasure(Value: String);
begin
  if (FLabelMeasure = Value) then
    Exit;
  FLabelMeasure := Value;
  DoMeasure();
end;

constructor TSimbaLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  AutoSize := True;

  FEdit := TSimbaEdit.Create(Self);

  FLabel := TPanel.Create(Self);
  FLabel.Parent := Self;
  FLabel.BevelOuter := bvNone;
  FLabel.OnPaint := @DoPaintLabel;
  FLabel.Anchors := [akLeft, akTop, akBottom];
  FLabel.AnchorSide[akLeft].Control := Self;
  FLabel.AnchorSide[akLeft].Side := asrLeft;
  FLabel.AnchorSide[akTop].Control := Self;
  FLabel.AnchorSide[akTop].Side := asrTop;
  FLabel.AnchorSide[akBottom].Control := FEdit;
  FLabel.AnchorSide[akBottom].Side := asrBottom;

  FEdit.Parent := Self;
  FEdit.Anchors := [akLeft, akTop, akRight, akBottom];
  FEdit.AnchorSide[akLeft].Control := FLabel;
  FEdit.AnchorSide[akLeft].Side := asrRight;
  FEdit.AnchorSide[akTop].Control := Self;
  FEdit.AnchorSide[akTop].Side := asrTop;
  FEdit.AnchorSide[akRight].Control := Self;
  FEdit.AnchorSide[akRight].Side := asrRight;
  FEdit.AnchorSide[akBottom].Control := Self;
  FEdit.AnchorSide[akBottom].Side := asrBottom;
  FEdit.BorderSpacing.Left := 5;
end;

procedure DoCreate;
begin
  EditCaretFlasher := TCaretFlasher.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(EditCaretFlasher);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.CREATE, @DoCreate, 'EditCaretFlasher', -10);
  SimbaInitialization_Add(ESimbaInit.DESTROY, @DoDestroy, 'EditCaretFlasher', 10);

end.

