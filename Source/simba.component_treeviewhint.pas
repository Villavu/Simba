{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  TTreeView node hint for mouse-over
}
unit simba.component_treeviewhint;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, Forms, ExtCtrls;

type
  TSimbaTreeViewHint = class(TComponent)
  protected
    FTreeView: TTreeView;     // treeview owning the currently shown hint
    FHintWindow: THintWindow;
    FNodeRect: TRect;
    FTimer: TTimer;

    procedure DoTimerExecute(Sender: TObject);
    procedure DoHintWindowHide(Sender: TObject);
    procedure DoHintWindowShow(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Show(ATreeView: TTreeView; Node: TTreeNode; Caption: String);
    procedure Hide;
  end;

// One shared hint - only one can be visible at a time.
function GetTreeViewHint: TSimbaTreeViewHint;

implementation

uses
  LCLType,
  simba.component_theme;

type
  TCustomHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  public
    procedure EraseBackground(DC: HDC); override;
  end;

var
  _TreeViewHint: TSimbaTreeViewHint = nil;

function GetTreeViewHint: TSimbaTreeViewHint;
begin
  if (_TreeViewHint = nil) then
    _TreeViewHint := TSimbaTreeViewHint.Create();
  Result := _TreeViewHint;
end;

procedure TCustomHintWindow.EraseBackground(DC: HDC);
begin
  { nothing }
end;

procedure TCustomHintWindow.Paint;
var
  TextStyle: TTextStyle;
  TreeView: TTreeView;
begin
  TreeView := TSimbaTreeViewHint(Owner).FTreeView;
  if (TreeView = nil) then
    Exit;

  TextStyle := Default(TTextStyle);
  TextStyle.Layout := tlCenter;

  Canvas.Font := TreeView.Font;
  Canvas.Font.Color := clWhite;
  Canvas.Pen.Color := SimbaComponentTheme.ColorActive;
  Canvas.Brush.Color := SimbaComponentTheme.ColorBackground;
  Canvas.Rectangle(ClientRect);
  Canvas.TextRect(ClientRect, 4, 0, Caption, TextStyle);
end;

procedure TSimbaTreeViewHint.DoTimerExecute(Sender: TObject);
begin
  if (FTreeView = nil) or (not FNodeRect.Contains(Mouse.CursorPos)) or (not FTreeView.Visible) or (not Application.Active) then
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

procedure TSimbaTreeViewHint.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FTreeView) then
  begin
    FHintWindow.Visible := False;
    FTreeView := nil;
  end;
end;

constructor TSimbaTreeViewHint.Create;
begin
  inherited Create(nil);

  FTimer          := TTimer.Create(Self);
  FTimer.Enabled  := False;
  FTimer.Interval := 500;
  FTimer.OnTimer  := @DoTimerExecute;

  FHintWindow        := TCustomHintWindow.Create(Self);
  FHintWindow.OnHide := @DoHintWindowHide;
  FHintWindow.OnShow := @DoHintWindowShow;
  FHintWindow.Color  := clRed; // disable "UseBGThemes" to stop flickering. We custom draw so this color doesn't matter.
end;

destructor TSimbaTreeViewHint.Destroy;
begin
  FTimer.Enabled := False;
  if (FHintWindow <> nil) then
    FreeAndNil(FHintWindow);

  inherited Destroy();
end;

procedure TSimbaTreeViewHint.Show(ATreeView: TTreeView; Node: TTreeNode; Caption: String);
begin
  if (Caption = '') then
  begin
    FHintWindow.Visible := False;
    Exit;
  end;

  // Track the owning treeview so we get told (Notification) if it is freed while
  // the hint is up - otherwise our reference to it would dangle.
  if (FTreeView <> ATreeView) then
  begin
    if Assigned(FTreeView) then
      FTreeView.RemoveFreeNotification(Self);
    FTreeView := ATreeView;
    FTreeView.FreeNotification(Self);
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

finalization
  FreeAndNil(_TreeViewHint);

end.
