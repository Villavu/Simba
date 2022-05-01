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
  classes, sysutils, controls, comctrls, graphics, forms;

type
  TSimbaHintWindow = class(TComponent)
  protected
    FTreeView: TTreeView;
    FHintWindow: THintWindow;

    procedure DoHide(Sender: TObject);
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
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(ClientRect);
  Canvas.TextRect(ClientRect, 3, 0, Caption, TextStyle);
end;

procedure TSimbaHintWindow.DoHide(Sender: TObject);
begin
  if (FHintWindow <> nil) then
    FHintWindow.Hide()
end;

constructor TSimbaHintWindow.Create(AOwner: TTreeView);
begin
  inherited Create(AOwner);

  if (not (Owner is TTreeView)) then
    raise Exception.Create('TSimbaHintWindow.Create: Owner is not a TTreeView');

  FHintWindow := TCustomHintWindow.Create(Self);

  FTreeView := AOwner;
  FTreeView.OnMouseLeave := @DoHide; // A handler would be a better
  FTreeView.AddHandlerOnVisibleChanged(@DoHide);

  Application.AddOnDeactivateHandler(@DoHide);
end;

destructor TSimbaHintWindow.Destroy;
begin
  if (FHintWindow <> nil) then
    FreeAndNil(FHintWindow);

  if (FTreeView <> nil) then
  begin
    FTreeView.OnMouseLeave := nil;
    FTreeView.RemoveHandlerOnVisibleChanged(@DoHide);
  end;

  if (Application <> nil) then
    Application.RemoveOnDeactivateHandler(@DoHide);

  inherited Destroy();
end;

procedure TSimbaHintWindow.Show(Node: TTreeNode; Caption: String);
var
  NodeRect: TRect;
begin
  NodeRect.TopLeft := Node.TreeView.ClientToScreen(Node.DisplayRect(True).TopLeft);
  NodeRect.BottomRight := Node.TreeView.ClientToScreen(Node.DisplayRect(True).BottomRight);

  FHintWindow.HintRect := TRect.Create(
    NodeRect.Left - 1,
    NodeRect.Top - 2,
    NodeRect.Left + FHintWindow.Canvas.TextWidth(Caption) + 6,
    NodeRect.Bottom + 1
  );

  FHintWindow.ActivateHint(Caption);
end;

procedure TSimbaHintWindow.Hide;
begin
  FHintWindow.Visible := False;
end;

end.

