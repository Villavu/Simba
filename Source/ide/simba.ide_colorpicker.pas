{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_colorpicker;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  simba.base,
  simba.ide_events,
  simba.component_imageboxzoom;

type
  TSimbaColorPicker = class(TComponent)
  private
    FForm: TForm;
    FHint: THintWindow;
    FImage: TImage;
    FPicked: Boolean;
    FImageX, FImageY: Integer;
    FPoint: TPoint;
    FColor: TColor;
    FWindowSelection: TWindowHandle;

    procedure Pick;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoFormClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoHintKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  SimbaColorPicker: TSimbaColorPicker;

implementation

uses
  ATCanvasPrimitives,
  LCLType,
  simba.initializations,
  simba.dialog,
  simba.image,
  simba.colormath,
  simba.vartype_windowhandle,
  simba.vartype_box,
  simba.component_theme,
  simba.ide_controller;

type
  TSimbaColorPickerHint = class(THintWindow)
  protected
    function DoHintTextMeasure(Sender: TObject): String;
    function DoHintText(Sender: TObject; AColor: TColor; X, Y: Integer): String;

    procedure Paint; override;
  public
    Zoom: TSimbaImageBoxZoomPanel;

    constructor Create(AOwner: TComponent); override;
  end;

function TSimbaColorPickerHint.DoHintTextMeasure(Sender: TObject): String;
begin
  Result := 'Position: 12345, 12345';
end;

function TSimbaColorPickerHint.DoHintText(Sender: TObject; AColor: TColor; X, Y: Integer): String;
begin
  Result := 'Color: ' + ColorToStr(AColor) + LineEnding + 'Position: ' + IntToStr(X) + ', ' + IntToStr(Y);
end;

procedure TSimbaColorPickerHint.Paint;
begin
  inherited Paint;

  Canvas.Pen.Color := ColorBlendHalf(SimbaComponentTheme.ColorFrame, SimbaComponentTheme.ColorLine);
  Canvas.Frame(ClientRect);
end;

constructor TSimbaColorPickerHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color := SimbaComponentTheme.ColorFrame;
  Font.Color := SimbaComponentTheme.ColorFont;

  BorderStyle := bsNone;
  AutoSize := True;

  Zoom := TSimbaImageBoxZoomPanel.Create(Self);
  Zoom.Parent := Self;
  Zoom.Align := alClient;
  Zoom.OnGetTextMeasure := @DoHintTextMeasure;
  Zoom.OnGetText := @DoHintText;
  Zoom.BorderSpacing.Around := 10;
  Zoom.FrameColor := ColorBlendHalf(SimbaComponentTheme.ColorFrame, SimbaComponentTheme.ColorLine);
end;

procedure TSimbaColorPicker.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.ACTION_PICKCOLOR:
      Pick();
  end;
end;

procedure TSimbaColorPicker.DoFormClosed(Sender: TObject; var CloseAction: TCloseAction);
var
  EventData: TSimbaEvents.TColorPicked;
begin
  if FPicked then
  begin
    DebugLn('Color picked: %s at (%d, %d)', [ColorToStr(FColor), FPoint.X, FPoint.Y]);
    DebugLn(DEBUG_FOCUS);

    EventData.Color := FColor;
    EventData.Point := FPoint;

    SimbaEvents.Post(ESimbaEvent.COLOR_PICKED, @EventData);
  end;

  FHint.Close();
  FImage.Picture.Clear(); // Free up mem

  CloseAction := caHide;
end;

procedure TSimbaColorPicker.DoHintKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TSimbaColorPicker.DoImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FImageX := X;
  FImageY := Y;

  FPoint := FWindowSelection.GetRelativeCursorPos();
  FColor := FImage.Picture.Bitmap.Canvas.Pixels[X, Y];
  with FImage.ClientToScreen(TPoint.Create(X + 25, Y - (FHint.Height div 2))) do
  begin
    FHint.Left := X;
    FHint.Top := Y;
  end;

  TSimbaColorPickerHint(FHint).Zoom.Move(TImage(Sender).Canvas, X, Y);
end;

procedure TSimbaColorPicker.DoImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPicked := True;

  FForm.Close();
end;

procedure TSimbaColorPicker.Pick;
var
  DesktopWindow: TWindowHandle;
  DesktopBounds: TBox;
  DesktopImage: TSimbaImage;
begin
  DesktopImage := nil;

  try
    if (FForm = nil) then // only create form when actually needed
    begin
      FForm := TForm.CreateNew(nil);
      FForm.BorderStyle := bsNone;
      FForm.OnClose := @DoFormClosed;

      FImage := TImage.Create(FForm);
      FImage.Parent := FForm;
      FImage.Align := alClient;
      FImage.Cursor := crCross;
      FImage.OnMouseUp := @DoImageMouseUp;
      FImage.OnMouseMove := @DoImageMouseMove;

      FHint := TSimbaColorPickerHint.Create(FForm);
      FHint.OnKeyDown := @DoHintKeyDown;
    end;

    DesktopWindow := GetDesktopWindow();
    DesktopBounds := DesktopWindow.GetBounds();
    DesktopImage := SimbaController.GetDesktopImage();

    FWindowSelection := SimbaController.WindowSelection.EnsureValid();

    FForm.Left := DesktopBounds.X1;
    FForm.Top := DesktopBounds.Y1;
    FForm.Width := DesktopBounds.Width;
    FForm.Height := DesktopBounds.Height;

    FImage.Picture.Bitmap := DesktopImage.ToLazBitmap();

    FForm.ShowOnTop();
    FHint.Show();
    FHint.BringToFront();

    while FForm.Showing do
    begin
      Application.ProcessMessages();

      Sleep(25);
    end;
  except
    on E: Exception do
    begin
      ShowErrorDialog('Color Picker', 'Exception occurred while picking color %s', [E.Message]);
      if (FForm <> nil) then
        FForm.Close();
    end;
  end;

  if (DesktopImage <> nil) then
    FreeAndNil(DesktopImage);
end;

constructor TSimbaColorPicker.Create();
begin
  inherited Create(nil);

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_PICKCOLOR]);
end;

destructor TSimbaColorPicker.Destroy;
begin
  if (FForm <> nil) then
    FreeAndNil(FForm);

  inherited Destroy();
end;

procedure DoCreate;
begin
  SimbaColorPicker := TSimbaColorPicker.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaColorPicker);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaColorPicker');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaColorPicker');

end.
