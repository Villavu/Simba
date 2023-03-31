{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.colorpicker;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, stdctrls,
  simba.imagebox_zoom, simba.mufasatypes;

type
  TSimbaColorPickerHint = class(THintWindow)
  protected
    procedure Paint; override;
  public
    Zoom: TSimbaImageBoxZoom;
    Info: TLabel;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaColorPicker = class(TObject)
  protected
    FForm: TForm;
    FColor: TColor;
    FPoint: TPoint;
    FHint: TSimbaColorPickerHint;
    FImage: TImage;
    FPicked: Boolean;
    FImageX, FImageY: Integer;
    FSelectedWindow: TWindowHandle;

    procedure FormClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure HintKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    property Color: TColor read FColor;
    property Point: TPoint read FPoint;
    property Picked: Boolean read FPicked;

    constructor Create(SelectedWindow: TWindowHandle); reintroduce;
  end;

implementation

uses
  LCLType,
  simba.bitmap, simba.windowhandle;

procedure TSimbaColorPickerHint.Paint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clForm;
  Canvas.Rectangle(ClientRect);

  inherited Paint();
end;

constructor TSimbaColorPickerHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;
  AutoSize := True;

  Zoom := TSimbaImageBoxZoom.Create(Self);
  Zoom.Parent := Self;
  Zoom.Align := alLeft;
  Zoom.SetZoom(4, 5);
  Zoom.BorderSpacing.Around := 10;

  Info := TLabel.Create(Self);
  Info.Parent := Self;
  Info.Font.Color := clBlack;
  Info.BorderSpacing.Right := 10;
  Info.AnchorToNeighbour(akLeft, 10, Zoom);
  Info.AnchorVerticalCenterTo(Zoom);
end;

procedure TSimbaColorPicker.FormClosed(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FPicked then
  begin
    FPoint := FSelectedWindow.GetRelativeCursorPos();
    FColor := FImage.Picture.Bitmap.Canvas.Pixels[FImageX, FImageY];
  end;

  CloseAction := caFree;
end;

procedure TSimbaColorPicker.HintKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:     Mouse.CursorPos := Mouse.CursorPos + TPoint.Create(0, -1);
    VK_LEFT:   Mouse.CursorPos := Mouse.CursorPos + TPoint.Create(-1, 0);
    VK_RIGHT:  Mouse.CursorPos := Mouse.CursorPos + TPoint.Create(1, 0);
    VK_DOWN:   Mouse.CursorPos := Mouse.CursorPos + TPoint.Create(0, 1);
    VK_ESCAPE: FForm.Close();
    VK_RETURN:
      begin
        FPicked := True;

        FForm.Close();
      end;
  end;

  Key := VK_UNKNOWN;
end;

procedure TSimbaColorPicker.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  INFO = 'Color: %d' + LineEnding +
         'Position: %d, %d';
begin
  FImageX := X;
  FImageY := Y;

  FPoint := FSelectedWindow.GetRelativeCursorPos();

  with FImage.ClientToScreen(TPoint.Create(X + 25, Y - (FHint.Height div 2))) do
  begin
    FHint.Left := X;
    FHint.Top := Y;
  end;

  FHint.Info.Caption := Format(INFO, [FImage.Picture.Bitmap.Canvas.Pixels[X, Y], FPoint.X, FPoint.Y]);
  FHint.Zoom.Move(Sender as TImage, X, Y);
end;

procedure TSimbaColorPicker.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPicked := True;

  FForm.Close();
end;

constructor TSimbaColorPicker.Create(SelectedWindow: TWindowHandle);
var
  DesktopWindow: TWindowHandle;
  DesktopBounds: TBox;
begin
  inherited Create();

  DesktopWindow := GetDesktopWindow();
  DesktopBounds := DesktopWindow.GetBounds();

  FForm := TForm.CreateNew(nil);
  with FForm do
  begin
    Left := DesktopBounds.X1;
    Top := DesktopBounds.Y1;
    Width := DesktopBounds.X2;
    Height := DesktopBounds.Y2;

    BorderStyle := bsNone;

    OnClose := @FormClosed;
  end;

  FImage := TImage.Create(FForm);
  with FImage do
  begin
    Parent := FForm;
    Align := alClient;
    Cursor := crCross;

    OnMouseUp := @ImageMouseUp;
    OnMouseMove := @ImageMouseMove;

    with TMufasaBitmap.CreateFromWindow(DesktopWindow) do
    try
      Picture.Bitmap.LoadFromRawImage(ToRawImage(), True);

      DataOwner := False;
    finally
      Free();
    end;
  end;

  FSelectedWindow := SelectedWindow;
  if (FSelectedWindow = 0) or (not FSelectedWindow.IsValid()) then
    FSelectedWindow := GetDesktopWindow();

  FForm.ShowOnTop();

  FHint := TSimbaColorPickerHint.Create(FForm);
  FHint.OnKeyDown := @HintKeyDown;
  FHint.Show();
  FHint.BringToFront();

  while FForm.Showing do
  begin
    Application.ProcessMessages();

    Sleep(25);
  end;
end;

end.

