unit simba.colorpicker;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  simba.iomanager, simba.oswindow, simba.imageboxzoom;

type
  TSimbaColorPickerHint = class(TForm)
  protected
    procedure Paint; override;
  public
    Zoom: TSimbaImageBoxZoom;
    Info: TLabel;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

  TSimbaColorPicker = class
  protected
    FForm: TForm;
    FColor: Int32;
    FPoint: TPoint;
    FIOManager: TIOManager;
    FHint: TSimbaColorPickerHint;
    FImage: TImage;

    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    property Color: Int32 read FColor;
    property Point: TPoint read FPoint;

    constructor Create(TargetWindow: TOSWindow);
    destructor Destroy; override;
  end;

implementation

uses
  simba.bitmap;

procedure TSimbaColorPickerHint.Paint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clForm;
  Canvas.Rectangle(ClientRect);

  inherited Paint();
end;

constructor TSimbaColorPickerHint.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner);

  BorderStyle := bsNone;
  AutoSize := True;

  Zoom := TSimbaImageBoxZoom.Create(Self);
  Zoom.Parent := Self;
  Zoom.Align := alLeft;
  Zoom.SetZoom(4, 5);
  Zoom.BorderSpacing.Around := 10;

  Info := TLabel.Create(Self);
  Info.Parent := Self;
  Info.BorderSpacing.Right:=10;
  Info.AnchorToNeighbour(akLeft, 10, Zoom);
  Info.AnchorVerticalCenterTo(Zoom);
end;

procedure TSimbaColorPicker.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  INFO = 'Color: %d' + LineEnding +
         'Position: %d, %d';
begin
  FIOManager.GetMousePos(FPoint.X, FPoint.Y);

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
  FIOManager.GetMousePos(FPoint.X, FPoint.Y);

  FColor := TImage(Sender).Picture.Bitmap.Canvas.Pixels[X, Y];

  FForm.Close();
end;

constructor TSimbaColorPicker.Create(TargetWindow: TOSWindow);
var
  DesktopLeft, DesktopTop, DesktopWidth, DesktopHeight: Int32;
begin
  FIOManager := TIOManager.Create();
  FIOManager.GetPosition(DesktopLeft, DesktopTop);
  FIOManager.GetDimensions(DesktopWidth, DesktopHeight);

  FForm := TForm.CreateNew(nil);
  with FForm do
  begin
    BorderStyle := bsNone;

    Left := DesktopLeft;
    Top := DesktopTop;
    Width := DesktopWidth;
    Height := DesktopHeight;
  end;

  FImage := TImage.Create(FForm);
  with FImage do
  begin
    Parent := FForm;
    Align := alClient;
    Cursor := crCross;

    OnMouseUp := @ImageMouseUp;
    OnMouseMove := @ImageMouseMove;

    with TMufasaBitmap.Create() do
    try
      CopyClientToBitmap(FIOManager, True, 0, 0, DesktopWidth - 1, DesktopHeight - 1);

      Picture.Bitmap.LoadFromRawImage(ToRawImage(), False);
    finally
      Free();
    end;
  end;

  FIOManager.SetTarget(TargetWindow);
  if not FIOManager.TargetValid() then
    FIOManager.SetDesktop();

  FForm.ShowOnTop();

  FHint := TSimbaColorPickerHint.CreateNew(nil);
  FHint.Show();
  FHint.BringToFront();

  while FForm.Showing do
  begin
    Application.ProcessMessages();

    Sleep(25);
  end;
end;

destructor TSimbaColorPicker.Destroy;
begin
  FForm.Free();
  FHint.Free();
  FIOManager.Free();

  inherited Destroy();
end;

end.

