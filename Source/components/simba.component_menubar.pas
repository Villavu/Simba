{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_menubar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Menus, Graphics,
  simba.settings;

type
  TSimbaMainMenuBar = class(TCustomControl)
  protected
    FItems: array of record
      Text: String;
      Rect: TRect;
      Menu: TPopupMenu;
      ClosedAt: UInt64;
    end;
    FHotIndex: Integer;

    function IndexAtXY(X, Y: Integer): Integer;

    procedure CalculateSizes;
    procedure Paint; override;
    procedure FontChanged(Sender: TObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;

    procedure DoMenuClose(Sender: TObject);

    procedure MaybeReplaceModifiers(Menu: TPopupMenu);
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddMenu(Title: String; APopupMenu: TPopupMenu);
  end;

implementation

uses
  simba.theme;

function TSimbaMainMenuBar.IndexAtXY(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
    if (X >= FItems[I].Rect.Left) and (X <= FItems[I].Rect.Right) then
      Exit(I);
end;

procedure TSimbaMainMenuBar.CalculateSizes;
var
  I: Integer;
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(Abs(GetFontData(Canvas.Font.Handle).Height) * 72 / Canvas.Font.PixelsPerInch) + 4; // Measure on larger font size - Font size can be 0

    if (Length(FItems) > 0) then
    begin
      FItems[0].Rect.Left  := 5;
      FItems[0].Rect.Right := FItems[0].Rect.Left + Canvas.TextWidth(FItems[0].Text);
      for I := 1 to High(FItems) do
      begin
        FItems[I].Rect.Left := FItems[I-1].Rect.Right + 5;
        FItems[I].Rect.Right := FItems[I].Rect.Left + Canvas.TextWidth(FItems[I].Text);
      end;
    end;

    Self.Height := Canvas.TextHeight('Tay');
  finally
    Free();
  end;
end;

procedure TSimbaMainMenuBar.Paint;
var
  I: Integer;
  R: TRect;
  Style: TTextStyle;
begin
  Style := Canvas.TextStyle;
  Style.Alignment := taCenter;
  Style.Layout := tlCenter;

  Canvas.Font.Color := SimbaTheme.ColorFont;
  Canvas.Brush.Color := SimbaTheme.ColorFrame;
  Canvas.FillRect(ClientRect);

  for I := 0 to High(FItems) do
  begin
    R := FItems[I].Rect;
    R.Top := 0;
    R.Height := Height;

    if (I = FHotIndex) then
    begin
      Canvas.Brush.Color := SimbaTheme.ColorActive;
      Canvas.FillRect(R.Left, R.Top + 2, R.Right, R.Height - 2);
    end;

    Canvas.TextRect(R, R.Left, R.Top, FItems[I].Text, Style);
  end;
  Canvas.Pen.Color := SimbaTheme.ColorLine;
  Canvas.Line(0, Height - 1, Width, Height - 1);
end;

procedure TSimbaMainMenuBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  CalculateSizes();
end;

procedure TSimbaMainMenuBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  I := IndexAtXY(X, Y);
  if (I > -1) and (GetTickCount64() - FItems[I].ClosedAt > 100) then
    with ClientToScreen(TPoint.Create(FItems[I].Rect.Left, Height)) do
      FItems[I].Menu.PopUp(X, Y);
end;

procedure TSimbaMainMenuBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  n: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  n := IndexAtXY(X,Y);
  if (n <> FHotIndex) then
  begin
    FHotIndex := n;
    Invalidate();
  end;
end;

procedure TSimbaMainMenuBar.MouseLeave;
begin
  inherited MouseLeave();

  if (FHotIndex > -1) then
  begin
    FHotIndex := -1;
    Invalidate();
  end;
end;

procedure TSimbaMainMenuBar.DoMenuClose(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if (FItems[I].Menu = Sender) then
      FItems[I].ClosedAt := GetTickCount64();
end;

procedure TSimbaMainMenuBar.MaybeReplaceModifiers(Menu: TPopupMenu);

  procedure ReplaceModifiers(const MenuItem: TMenuItem);
  var
    I: Integer;
    Key: Word;
    Shift: TShiftState;
  begin
    if (MenuItem.ShortCut > 0) then
    begin
      ShortCutToKey(MenuItem.ShortCut, Key, Shift);
      if (ssCtrl in Shift) then
        MenuItem.ShortCut := ShortCut(Key, Shift - [ssCtrl] + [ssMeta]);
    end;

    for I := 0 to MenuItem.Count - 1 do
      ReplaceModifiers(MenuItem.Items[I]);
  end;

var
  I: Integer;
begin
  if Application.HasOption('no-macos-commandkey') then
    Exit;

  for I := 0 to Menu.Items.Count - 1 do
    ReplaceModifiers(Menu.Items[I]);
end;

constructor TSimbaMainMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ParentFont := True;
  ControlStyle := ControlStyle + [csOpaque];

  CalculateSizes();
end;

procedure TSimbaMainMenuBar.AddMenu(Title: String; APopupMenu: TPopupMenu);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)].Text := Title;
  FItems[High(FItems)].Menu := APopupMenu;
  APopupMenu.OnClose := @DoMenuClose;

  {$IFDEF DARWIN}
  MaybeReplaceModifiers(APopupMenu);
  {$ENDIF}

  CalculateSizes();
end;

end.

