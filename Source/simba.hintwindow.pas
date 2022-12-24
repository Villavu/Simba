{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Hint window for TTreeview which ensures the hint is hidden under under multiple circumstances.
}
unit simba.hintwindow;

{$i simba.inc}

interface

uses
  classes, sysutils, controls, comctrls, graphics, forms, extctrls;

type
  TSimbaHintWindow = class(TComponent)
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

type
  TCustomHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  end;

procedure TCustomHintWindow.Paint;
var
  TextStyle: TTextStyle;
begin
  TextStyle := Default(TTextStyle);
  TextStyle.Layout := tlCenter;

  Canvas.Font := TSimbaHintWindow(Owner).FTreeView.Font;
  Canvas.Font.Color := clWindowText;
  Canvas.Pen.Color := clWindowText;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(ClientRect);
  Canvas.TextRect(ClientRect, 3, 0, Caption, TextStyle);
end;

procedure TSimbaHintWindow.DoTimerExecute(Sender: TObject);
begin
  if (not FNodeRect.Contains(Mouse.CursorPos)) or (not FTreeView.Visible) or (not Application.Active) then
    FHintWindow.Visible := False;
end;

procedure TSimbaHintWindow.DoHintWindowHide(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

procedure TSimbaHintWindow.DoHintWindowShow(Sender: TObject);
begin
  FTimer.Enabled := True;
end;

constructor TSimbaHintWindow.Create(AOwner: TTreeView);
begin
  inherited Create(AOwner);

  if (not (Owner is TTreeView)) then
    raise Exception.Create('TSimbaHintWindow.Create: Owner is not a TTreeView');

  FTimer          := TTimer.Create(Self);
  FTimer.Enabled  := False;
  FTimer.Interval := 500;
  FTimer.OnTimer  := @DoTimerExecute;

  FHintWindow        := TCustomHintWindow.Create(Self);
  FHintWindow.OnHide := @DoHintWindowHide;
  FHintWindow.OnShow := @DoHintWindowShow;

  FTreeView := AOwner;
end;

destructor TSimbaHintWindow.Destroy;
begin
  FTimer.Enabled := False;
  if (FHintWindow <> nil) then
    FreeAndNil(FHintWindow);

  inherited Destroy();
end;

procedure TSimbaHintWindow.Show(Node: TTreeNode; Caption: String);
begin
  if (not Application.Active) then
    Exit;

  FNodeRect := Node.DisplayRect(True);
  FNodeRect.SetLocation(FTreeView.ClientToScreen(FNodeRect.TopLeft));
  FNodeRect.Right := FNodeRect.Left + FHintWindow.Canvas.TextWidth(Caption) + 6;

  FHintWindow.HintRect := FNodeRect;
  FHintWindow.ActivateHint(FNodeRect, Caption);

  FNodeRect.Left := FTreeView.ClientOrigin.X + Node.DisplayIconLeft;
end;

procedure TSimbaHintWindow.Hide;
begin
  FHintWindow.Visible := False;
end;

end.

