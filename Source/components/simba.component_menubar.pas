{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_menubar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Menus, Graphics, ExtCtrls, LMessages,
  simba.settings;

type
  TPopupMenuArray = array of TPopupMenu;

  TSimbaMainMenuBar = class(TCustomControl)
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
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;

    procedure DoMenuClose(Sender: TObject);

    procedure MaybeReplaceModifiers(Menu: TPopupMenu);

    function GetMenus: TPopupMenuArray;

    procedure PopupDelayed(Data: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure SetFocus; override;

    property HotIndex: Integer read FHotIndex write SetHotIndex;
    property Menus: TPopupMenuArray read GetMenus;
    procedure Popup(Index: Integer);
    procedure AddMenu(Title: String; APopupMenu: TPopupMenu);
  end;

implementation

uses
  LCLType, LCLIntf, ATCanvasPrimitives,
  simba.theme, simba.fonthelpers, simba.scripttabsform;

function TSimbaMainMenuBar.GetMenus: TPopupMenuArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FItems));
  for I := 0 to High(Result) do
    Result[I] := FItems[I].Menu;
end;

procedure TSimbaMainMenuBar.PopupDelayed(Data: PtrInt);
begin
  Popup(Data);
end;

procedure TSimbaMainMenuBar.SetHotIndex(Index: Integer);
begin
  FHotIndex := Index;
  if (FHotIndex = -1) and (FPopupIndex > -1) then
    FHotIndex := FPopupIndex;

  Invalidate();
end;

function TSimbaMainMenuBar.IndexAtXY(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
    if (X >= FItems[I].Rect.Left) and (X <= FItems[I].Rect.Right) and (Y >= 0) and (Y < FItems[I].Rect.Height) then
      Exit(I);
end;

procedure TSimbaMainMenuBar.Popup(Index: Integer);
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

procedure TSimbaMainMenuBar.DoTrackTimer(Sender: TObject);
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

procedure TSimbaMainMenuBar.DoChangePopupMenu(Data: PtrInt);
begin
  Popup(Data);
end;

procedure TSimbaMainMenuBar.ClearPopupIndex(Data: PtrInt);
begin
  FPopupIndex := -1;

  SetHotIndex(-1);
end;

procedure TSimbaMainMenuBar.CalculateSizes;
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

  Canvas.Pen.Color := ColorBlendHalf(SimbaTheme.ColorFrame, SimbaTheme.ColorLine);
  Canvas.Line(0, Height - 1, Width, Height - 1);
end;

procedure TSimbaMainMenuBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  CalculateSizes();
end;

procedure TSimbaMainMenuBar.SetFocus;
begin
  inherited SetFocus();

  if (HotIndex = -1) then
    HotIndex := 0;
end;

procedure TSimbaMainMenuBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  Msg: TLMKillFocus;
begin
  if Focused then
  begin
    if (Key = VK_MENU) and (not (ssAltGr in Shift)) then
    begin
      Key := VK_RIGHT;
    end;
    if (Key = VK_ESCAPE) then
    begin
      WMKillFocus(Msg{%H-});
      Key := 0;
      Exit;
    end;

    if (Key = VK_RETURN) then
    begin
      Application.QueueAsyncCall(@PopupDelayed, HotIndex);
      Key := 0;
    end
    else if (HotIndex = -1) then
      HotIndex := 0
    else
    if (Key = VK_LEFT) then
    begin
      if (HotIndex = 0) then
        HotIndex := High(FItems)
      else
        HotIndex := HotIndex - 1;

      Key := 0;
    end
    else if (Key = VK_RIGHT) then
    begin
      if (HotIndex = High(FItems)) then
        HotIndex := 0
      else
        HotIndex := HotIndex + 1;

      Key := 0;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TSimbaMainMenuBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  Popup(IndexAtXY(X, Y));
end;

procedure TSimbaMainMenuBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  SetHotIndex(IndexAtXY(X, Y));
end;

procedure TSimbaMainMenuBar.MouseLeave;
begin
  inherited MouseLeave();

  if (FPopupIndex = -1) then
    SetHotIndex(-1);
end;

procedure TSimbaMainMenuBar.DoMenuClose(Sender: TObject);
var
  Msg: TLMKillFocus;
begin
  WMKillFocus(Msg{%H-});
  if (FTrackTimer = nil) then
    Exit;
  FTrackTimer.Enabled := False;

  Application.QueueAsyncCall(@ClearPopupIndex, 0);
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

  FPopupIndex := -1;
  FHotIndex := -1;

  FTrackTimer := TTimer.Create(Self);
  FTrackTimer.Enabled := False;
  FTrackTimer.Interval := 100;
  FTrackTimer.OnTimer := @DoTrackTimer;

  ControlStyle := ControlStyle + [csOpaque];

  CalculateSizes();
end;

destructor TSimbaMainMenuBar.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  FTrackTimer := nil;

  inherited Destroy();
end;

procedure TSimbaMainMenuBar.WMKillFocus(var Message: TLMKillFocus);
begin
  HotIndex := -1;

  if Assigned(SimbaScriptTabsForm) and Assigned(SimbaScriptTabsForm.CurrentEditor) then
    if SimbaScriptTabsForm.CurrentEditor.CanSetFocus() then
      SimbaScriptTabsForm.CurrentEditor.SetFocus();
end;

procedure TSimbaMainMenuBar.AddMenu(Title: String; APopupMenu: TPopupMenu);
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

