{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_menubar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Menus, Graphics, ExtCtrls,
  simba.settings;

type
  TPopupMenuArray = array of TPopupMenu;

  TSimbaMenuBar = class(TCustomControl)
  protected
    FItems: array of record
      Text: String;
      Rect: TRect;
      Menu: TPopupMenu;
    end;
    FHotIndex: Integer;
    FTrackTimer: TTimer;
    FPopupIndex: Integer;

    procedure SetHotIndex(Index: Integer);

    function IndexAtXY(X, Y: Integer): Integer;

    procedure DoTrackTimer(Sender: TObject);
    procedure DoChangePopupMenu(Data: PtrInt);
    procedure ClearPopupIndex(Data: PtrInt);

    procedure CalculateSizes;
    procedure Paint; override;
    procedure FontChanged(Sender: TObject); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;

    procedure DoMenuClose(Sender: TObject);

    procedure MaybeReplaceModifiers(Menu: TPopupMenu);

    function GetMenuCount: Integer;
    function GetMenus: TPopupMenuArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PopupDelayed(Data: PtrInt);
    procedure Popup(Index: Integer);

    procedure AddMenu(Title: String; APopupMenu: TPopupMenu);

    property HotIndex: Integer read FHotIndex write SetHotIndex;
    property Menus: TPopupMenuArray read GetMenus;
    property MenuCount: Integer read GetMenuCount;
  end;

implementation

uses
  LCLType, LCLIntf, LMessages, ATCanvasPrimitives,
  simba.ide_theme, simba.misc;

function TSimbaMenuBar.GetMenus: TPopupMenuArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FItems));
  for I := 0 to High(Result) do
    Result[I] := FItems[I].Menu;
end;

procedure TSimbaMenuBar.PopupDelayed(Data: PtrInt);
begin
  Popup(Data);
end;

procedure TSimbaMenuBar.SetHotIndex(Index: Integer);
begin
  FHotIndex := Index;
  if (FHotIndex = -1) and (FPopupIndex > -1) then
    FHotIndex := FPopupIndex;

  Invalidate();
end;

function TSimbaMenuBar.IndexAtXY(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
    if (X >= FItems[I].Rect.Left) and (X <= FItems[I].Rect.Right) and (Y >= 0) and (Y < FItems[I].Rect.Height) then
      Exit(I);
end;

procedure TSimbaMenuBar.Popup(Index: Integer);
begin
  if (FPopupIndex = Index) then
    Exit;
  FPopupIndex := Index;
  if (FPopupIndex = -1) then
    Exit;

  FTrackTimer.Enabled := True;

  with ClientToScreen(TPoint.Create(FItems[Index].Rect.Left, Height)) do
  begin
    FItems[Index].Menu.PopupComponent := Self;
    FItems[Index].Menu.PopUp(X, Y);
  end;
end;

procedure TSimbaMenuBar.DoTrackTimer(Sender: TObject);
var
  NewIndex: Integer;
begin
  with ScreenToClient(Mouse.CursorPos) do
  begin
    NewIndex := IndexAtXY(X, Y);
    SetHotIndex(NewIndex);

    if (NewIndex <> -1) and (NewIndex <> FPopupIndex) then
    begin
      if (GetCapture <> 0) then
        SendMessage(GetCapture(), LM_CANCELMODE, 0, 0);
      ReleaseCapture();

      Application.QueueAsyncCall(@DoChangePopupMenu, NewIndex);
    end;
  end;
end;

procedure TSimbaMenuBar.DoChangePopupMenu(Data: PtrInt);
begin
  Popup(Data);
end;

procedure TSimbaMenuBar.ClearPopupIndex(Data: PtrInt);
begin
  FPopupIndex := -1;

  SetHotIndex(-1);
end;

procedure TSimbaMenuBar.CalculateSizes;
var
  I: Integer;
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := GetFontSize(Self);

    if (Length(FItems) > 0) then
    begin
      FItems[0].Rect.Top := 0;
      FItems[0].Rect.Bottom := Self.Height;
      FItems[0].Rect.Left  := 5;
      FItems[0].Rect.Right := FItems[0].Rect.Left + Canvas.TextWidth(FItems[0].Text) + 10;

      for I := 1 to High(FItems) do
      begin
        FItems[I].Rect.Top := 0;
        FItems[I].Rect.Bottom := Self.Height;
        FItems[I].Rect.Left := FItems[I-1].Rect.Right + 5;
        FItems[I].Rect.Right := FItems[I].Rect.Left + Canvas.TextWidth(FItems[I].Text) + 10;
      end;
    end;

    Canvas.Font.Size := GetFontSize(Self, 3);
    Self.Height := Canvas.TextHeight('Tay');
  finally
    Free();
  end;
end;

procedure TSimbaMenuBar.Paint;
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

  Canvas.Pen.Color := ColorBlendHalf(SimbaTheme.ColorFrame, SimbaTheme.ColorLine);
  Canvas.Line(0, Height - 1, Width, Height - 1);
end;

procedure TSimbaMenuBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  CalculateSizes();
end;

procedure TSimbaMenuBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  Popup(IndexAtXY(X, Y));
end;

procedure TSimbaMenuBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  SetHotIndex(IndexAtXY(X, Y));
end;

procedure TSimbaMenuBar.MouseLeave;
begin
  inherited MouseLeave();

  if (FPopupIndex = -1) then
    SetHotIndex(-1);
end;

procedure TSimbaMenuBar.DoMenuClose(Sender: TObject);
begin
  if (FTrackTimer = nil) then
    Exit;
  FTrackTimer.Enabled := False;

  Application.QueueAsyncCall(@ClearPopupIndex, 0);
end;

procedure TSimbaMenuBar.MaybeReplaceModifiers(Menu: TPopupMenu);

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

function TSimbaMenuBar.GetMenuCount: Integer;
begin
  Result := Length(FItems);
end;

constructor TSimbaMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPopupIndex := -1;
  FHotIndex := -1;

  FTrackTimer := TTimer.Create(Self);
  FTrackTimer.Enabled := False;
  FTrackTimer.Interval := 100;
  FTrackTimer.OnTimer := @DoTrackTimer;

  ControlStyle := ControlStyle + [csOpaque, csNoFocus];

  CalculateSizes();
end;

destructor TSimbaMenuBar.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  FTrackTimer := nil;

  inherited Destroy();
end;

procedure TSimbaMenuBar.AddMenu(Title: String; APopupMenu: TPopupMenu);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    if (FItems[I].Menu = APopupMenu) then
      Exit;

  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)].Text := Title;
  FItems[High(FItems)].Menu := APopupMenu;
  APopupMenu.OnClose := @DoMenuClose;

  {$IFDEF DARWIN}
  MaybeReplaceModifiers(APopupMenu);
  {$ENDIF}

  CalculateSizes();
  Invalidate();
end;

end.

