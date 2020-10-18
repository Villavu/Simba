unit simba.colorpicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  simba.iomanager, simba.oswindow;

type
  TSimbaColorPickerHint = class(TForm)
  const
    ZOOM_PIXELS = 3; // 3, 5, 7, 9 ...
  protected
    procedure ImagePaint(Sender: TObject);
    procedure Paint; override;
  public
    Image: TImage;
    ColorLabel: TLabel;
    PositionLabel: TLabel;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

  TSimbaColorPicker = class
  protected
    FForm: TForm;
    FColor: Int32;
    FPoint: TPoint;
    FIOManager: TIOManager;
    FOffset: TPoint;
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
  math,
  simba.bitmap, simba.main ;

procedure TSimbaColorPickerHint.ImagePaint(Sender: TObject);
begin
  with Image do
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Frame(ClientRect);
    Canvas.Pen.Color := clRed;

    Canvas.Frame(
      (Width div 2)  - Floor(Width / ZOOM_PIXELS / 2),
      (Height div 2) - Floor(Width / ZOOM_PIXELS / 2),
      (Width div 2)  + Ceil(Width / ZOOM_PIXELS / 2),
      (Height div 2) + Ceil(Width / ZOOM_PIXELS / 2)
    );
  end;
end;

procedure TSimbaColorPickerHint.Paint;
begin
  inherited Paint();

  Canvas.Pen.Color := clBlack;
  Canvas.Frame(ClientRect);
end;

constructor TSimbaColorPickerHint.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner);

  Self.BorderStyle := bsNone;
  Self.Font := SimbaForm.Font;

  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.Stretch := True;
  Image.Picture.Bitmap.Width := ZOOM_PIXELS;
  Image.Picture.Bitmap.Height := ZOOM_PIXELS;
  Image.OnPaint := @ImagePaint;
  Image.AntialiasingMode := amOff;

  Image.Top := 5;
  Image.Left := Canvas.TextWidth('Position: 99999, 99999');
  Image.Width := Canvas.TextHeight('Position: 99999, 99999') * 2 + 2;
  Image.Height := Canvas.TextHeight('Position: 99999, 99999') * 2 + 2;
  Image.Width := Image.Width + (ZOOM_PIXELS - Image.Width mod ZOOM_PIXELS);
  Image.Height := Image.Height + (ZOOM_PIXELS - Image.Height mod ZOOM_PIXELS);

  Width := Image.Left + Image.Width + 5;
  Height := Image.Height + Image.Top + 5;

  ColorLabel := TLabel.Create(Self);
  ColorLabel.Parent := Self;
  ColorLabel.Left := 5;
  ColorLabel.Anchors := [akLeft, akTop];
  ColorLabel.AnchorSide[akTop].Side := asrTop;
  ColorLabel.AnchorSide[akTop].Control := Image;
  ColorLabel.BorderSpacing.Top := 2;

  PositionLabel := TLabel.Create(Self);
  PositionLabel.Left := 5;
  PositionLabel.Parent := Self;
  PositionLabel.Anchors := [akLeft, akBottom];
  PositionLabel.AnchorSide[akBottom].Side := asrBottom;
  PositionLabel.AnchorSide[akBottom].Control := Image;
  PositionLabel.BorderSpacing.Bottom := 2;
end;

procedure TSimbaColorPicker.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PixelX, PixelY: Int32;
begin
  FHint.Left := X + 15;
  FHint.Top := Y - (FHint.Height div 2);

  FHint.ColorLabel.Caption := 'Color: ' + IntToStr(FImage.Picture.Bitmap.Canvas.Pixels[X, Y]);
  FHint.PositionLabel.Caption := 'Position: ' + IntToStr(X - FOffset.X) + ', ' + IntToStr(Y - FOffset.Y);

  FHint.Image.Picture.Bitmap.BeginUpdate(True);
  FHint.Image.Picture.Bitmap.Canvas.AntialiasingMode := amOff;

  try
    FHint.Image.Picture.Bitmap.Canvas.Clear();

    for PixelX := 0 to FHint.Image.Picture.Bitmap.Width - 1 do
      for PixelY := 0 to FHint.Image.Picture.Bitmap.Height - 1 do
        FHint.Image.Picture.Bitmap.Canvas.Pixels[PixelX, PixelY] := FImage.Picture.Bitmap.Canvas.Pixels[X + PixelX - (FHint.Image.Picture.Bitmap.Width div 2),                                                                                          Y + PixelY - (FHint.Image.Picture.Bitmap.Height div 2)];
  finally
    FHint.Image.Picture.Bitmap.EndUpdate(False);
  end;
end;

procedure TSimbaColorPicker.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPoint.X := FForm.Left + X - FOffset.X;
  FPoint.Y := FForm.Top + Y - FOffset.Y;

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

  FForm.ShowOnTop();

  FIOManager.SetTarget(TargetWindow);
  if not FIOManager.TargetValid() then
    FIOManager.SetDesktop();

  FIOManager.GetPosition(FOffset.X, FOffset.Y);

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

