{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Custom drawn hint window for TTreeView which ensures the hint is hidden under under multiple circumstances.
}
unit simba.component_treeviewhint;

{$i simba.inc}

interface

uses
  classes, sysutils, controls, comctrls, graphics, forms, extctrls;

type
  TSimbaTreeViewHint = class(TComponent)
  protected
    FTreeView: TTreeView;
    FHintWindow: THintWindow;
    FNodeRect: TRect;
    FTimer: TTimer;

    procedure DoTimerExecute(Sender: TObject);
    procedure DoHintWindowHide(Sender: TObject);
    procedure DoHintWindowShow(Sender: TObject);
  public
    constructor Create(AOwner: TTreeView); reintroduce;
    destructor Destroy; override;

    procedure Show(Node: TTreeNode; Caption: String);
    procedure Hide;
  end;

implementation

uses
  LCLType,
  simba.theme;

type
  TCustomHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  public
    procedure EraseBackground(DC: HDC); override;
  end;

procedure TCustomHintWindow.EraseBackground(DC: HDC);
begin
  { nothing }
end;

procedure TCustomHintWindow.Paint;
var
  TextStyle: TTextStyle;
begin
  TextStyle := Default(TTextStyle);
  TextStyle.Layout := tlCenter;

  Canvas.Font := TSimbaTreeViewHint(Owner).FTreeView.Font;
  Canvas.Font.Color := clWhite;
  Canvas.Pen.Color := SimbaTheme.ColorActive;
  Canvas.Brush.Color := SimbaTheme.ColorBackground;
  Canvas.Rectangle(ClientRect);
  Canvas.TextRect(ClientRect, 4, 0, Caption, TextStyle);
end;

procedure TSimbaTreeViewHint.DoTimerExecute(Sender: TObject);
begin
  if (not FNodeRect.Contains(Mouse.CursorPos)) or (not FTreeView.Visible) or (not Application.Active) then
    FHintWindow.Visible := False;
end;

procedure TSimbaTreeViewHint.DoHintWindowHide(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

procedure TSimbaTreeViewHint.DoHintWindowShow(Sender: TObject);
begin
  FTimer.Enabled := True;
end;

constructor TSimbaTreeViewHint.Create(AOwner: TTreeView);
begin
  inherited Create(AOwner);

  if (not (Owner is TTreeView)) then
    raise Exception.Create('TSimbaTreeViewHint.Create: Owner is not a TTreeView');

  FTimer          := TTimer.Create(Self);
  FTimer.Enabled  := False;
  FTimer.Interval := 500;
  FTimer.OnTimer  := @DoTimerExecute;

  FHintWindow        := TCustomHintWindow.Create(Self);
  FHintWindow.OnHide := @DoHintWindowHide;
  FHintWindow.OnShow := @DoHintWindowShow;
  FHintWindow.Color  := clRed; // disable "UseBGThemes" to stop flickering. We custom draw so this color doesn't matter.

  FTreeView := AOwner;
end;

destructor TSimbaTreeViewHint.Destroy;
begin
  FTimer.Enabled := False;
  if (FHintWindow <> nil) then
    FreeAndNil(FHintWindow);

  inherited Destroy();
end;

procedure TSimbaTreeViewHint.Show(Node: TTreeNode; Caption: String);
begin
  if (Caption = '') then
  begin
    FHintWindow.Visible := False;
    Exit;
  end;

  FNodeRect := Node.DisplayRect(True);
  FNodeRect.Offset(FTreeView.ClientOrigin);
  FNodeRect.Left := FNodeRect.Left - 1;
  FNodeRect.Right := FNodeRect.Left + FHintWindow.Canvas.TextWidth(Caption) + 8;

  FHintWindow.ActivateHint(FNodeRect, Caption);
end;

procedure TSimbaTreeViewHint.Hide;
begin
  FHintWindow.Visible := False;
end;

end.

