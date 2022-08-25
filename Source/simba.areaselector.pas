{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.areaselector;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls,
  simba.mufasatypes;

type
  TSimbaAreaSelector = class(TObject)
  protected type
    {$scopedenums on}
    EDragEdge = (
      NONE,
      LEFT, RIGHT, TOP, BOTTOM,
      TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT
    );
    {$scopedenums off}
  protected
    FForm: TForm;
    FDownEdge: EDragEdge;
    FDownX: Integer;
    FDownY: Integer;
    FDown: Boolean;

    function GetDragEdge(X, Y: Integer): EDragEdge;

    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoDblClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Pick(TargetWindow: TWindowHandle): TBox;
  end;

implementation

uses
  math, lcltype,
  simba.windowhandle;

function TSimbaAreaSelector.GetDragEdge(X, Y: Integer): EDragEdge;

  function DistToCorner(CornerX, CornerY: Integer): Integer;
  begin
    Result := Round(Hypot(CornerX - X, CornerY - Y));
  end;

begin
  Result := EDragEdge.NONE;

  with FForm.ClientRect do
  begin
    if (DistToCorner(Left,  Top) < 10)    then Exit(EDragEdge.TOP_LEFT);
    if (DistToCorner(Right, Top) < 10)    then Exit(EDragEdge.TOP_RIGHT);
    if (DistToCorner(Left,  Bottom) < 10) then Exit(EDragEdge.BOTTOM_LEFT);
    if (DistToCorner(Right, Bottom) < 10) then Exit(EDragEdge.BOTTOM_RIGHT);

    if (X < 10)          then Exit(EDragEdge.LEFT);
    if (Y < 10)          then Exit(EDragEdge.TOP);
    if (X > Width - 10)  then Exit(EDragEdge.RIGHT);
    if (Y > Height - 10) then Exit(EDragEdge.BOTTOM);
  end;
end;

procedure TSimbaAreaSelector.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDownEdge := GetDragEdge(X, Y);
  FDownX := X;
  FDownY := Y;
  FDown := True;
end;

procedure TSimbaAreaSelector.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := False;
end;

procedure TSimbaAreaSelector.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  MouseX, MouseY: Integer;
  NewRect: TRect;
begin
  case GetDragEdge(X, Y) of
    EDragEdge.LEFT:   FForm.Cursor := crSizeE;
    EDragEdge.TOP:    FForm.Cursor := crSizeN;
    EDragEdge.RIGHT:  FForm.Cursor := crSizeW;
    EDragEdge.BOTTOM: FForm.Cursor := crSizeS;

    EDragEdge.TOP_LEFT:     FForm.Cursor := crSizeNW;
    EDragEdge.TOP_RIGHT:    FForm.Cursor := crSizeNE;
    EDragEdge.BOTTOM_LEFT:  FForm.Cursor := crSizeSW;
    EDragEdge.BOTTOM_RIGHT: FForm.Cursor := crSizeSE;
    else
      FForm.Cursor := crSize;
  end;

  if (not FDown) then
    Exit;

  MouseX := Mouse.CursorPos.X;
  MouseY := Mouse.CursorPos.Y;

  NewRect := FForm.BoundsRect;

  case FDownEdge of
    EDragEdge.LEFT:   NewRect.Left   := MouseX;
    EDragEdge.RIGHT:  NewRect.Right  := MouseX;
    EDragEdge.TOP:    NewRect.Top    := MouseY;
    EDragEdge.BOTTOM: NewRect.Height := MouseY;

    EDragEdge.TOP_LEFT:     begin NewRect.Left  := MouseX; NewRect.Top := MouseY;    end;
    EDragEdge.TOP_RIGHT:    begin NewRect.Right := MouseX; NewRect.Top := MouseY;    end;
    EDragEdge.BOTTOM_LEFT:  begin NewRect.Left  := MouseX; NewRect.Bottom := MouseY; end;
    EDragEdge.BOTTOM_RIGHT: begin NewRect.Right := MouseX; NewRect.Bottom := MouseY; end;
    else
      NewRect.SetLocation(MouseX - FDownX, MouseY - FDownY);
  end;

  FForm.BoundsRect := NewRect;
end;

procedure TSimbaAreaSelector.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
    FForm.Close();
  Key := VK_UNKNOWN;
end;

procedure TSimbaAreaSelector.DoDblClick(Sender: TObject);
begin
  FForm.Close();
end;

constructor TSimbaAreaSelector.Create;
begin
  inherited Create();

  FForm := TForm.CreateNew(nil);
  FForm.Scaled := False;
  FForm.BorderStyle := bsNone;
  FForm.Color := clGreen;
  FForm.FormStyle := fsSystemStayOnTop;
  FForm.ShowInTaskBar := stNever;
  FForm.AlphaBlend := True;
  FForm.AlphaBlendValue := 80;
  FForm.OnMouseDown := @DoMouseDown;
  FForm.OnMouseUp   := @DoMouseUp;
  FForm.OnMouseMove := @DoMouseMove;
  FForm.OnKeyDown   := @DoKeyDown;
  FForm.OnDblClick  := @DoDblClick;
  with Screen.WorkAreaRect do
    FForm.BoundsRect := TRect.Create(CenterPoint - TPoint.Create(200, 200), 400, 400);
end;

destructor TSimbaAreaSelector.Destroy;
begin
  if (FForm <> nil) then
    FreeAndNil(FForm);

  inherited Destroy();
end;

function TSimbaAreaSelector.Pick(TargetWindow: TWindowHandle): TBox;
var
  WindowBounds: TBox;
begin
  FForm.ShowModal();

  WindowBounds := Default(TBox);
  if TargetWindow.IsValid() then
    WindowBounds := TargetWindow.GetBounds();

  with FForm.BoundsRect do
  begin
    Result.X1 := Left - WindowBounds.X1;
    Result.Y1 := Top - WindowBounds.Y1;
    Result.X2 := Right - WindowBounds.X1;
    Result.Y2 := Bottom - WindowBounds.Y1;
  end;
end;

end.

