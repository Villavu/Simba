{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_colorpicker;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  simba.base, simba.image,
  simba.ide_events,
  simba.component_imageboxrender;

type
  TSimbaColorPicker = class(TComponent)
  private
    FForm: TForm;
    FHint: THintWindow;
    FRenderer: TSimbaImageBoxRenderer; // blits FDesktopImage straight to the form, no LCL copy
    FDesktopImage: TSimbaImage; // the frozen desktop, the picker's only full sized buffer
    FPicked: Boolean;
    FImageX, FImageY: Integer;
    FPoint: TPoint;
    FColor: TColor;
    FWindowSelection: TWindowHandle;

    procedure Pick;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoFormClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoHintKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoFormPaint(Sender: TObject);
    procedure DoFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
  simba.colormath,
  simba.vartype_windowhandle,
  simba.vartype_box,
  simba.component_imageboxzoom,
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

  CloseAction := caHide;
end;

procedure TSimbaColorPicker.DoFormPaint(Sender: TObject);
begin
  // the frozen desktop goes straight from its BGRA buffer onto the form at 1:1,
  // no layers and no overlay, so nothing here keeps a second full sized copy of it
  if (FDesktopImage <> nil) then
    FRenderer.Render(FForm.Canvas, FDesktopImage, [], TRect.Create(0, 0, FForm.ClientWidth, FForm.ClientHeight), 1, False, nil);
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

procedure TSimbaColorPicker.DoFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FImageX := X;
  FImageY := Y;

  FPoint := FWindowSelection.GetRelativeCursorPos();
  if FDesktopImage.InImage(X, Y) then
    FColor := FDesktopImage.Pixel[X, Y];
  with FForm.ClientToScreen(TPoint.Create(X + 25, Y - (FHint.Height div 2))) do
  begin
    FHint.Left := X;
    FHint.Top := Y;
  end;

  TSimbaColorPickerHint(FHint).Zoom.Move(FDesktopImage, X, Y);
end;

procedure TSimbaColorPicker.DoFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPicked := True;

  FForm.Close();
end;

procedure TSimbaColorPicker.Pick;
var
  DesktopWindow: TWindowHandle;
  DesktopBounds: TBox;
begin
  try
    if (FForm = nil) then // only create form when actually needed
    begin
      FForm := TForm.CreateNew(nil);
      FForm.BorderStyle := bsNone;
      FForm.Cursor := crCross;
      FForm.OnClose := @DoFormClosed;
      FForm.OnPaint := @DoFormPaint;
      FForm.OnMouseUp := @DoFormMouseUp;
      FForm.OnMouseMove := @DoFormMouseMove;

      FRenderer := CreateImageBoxRenderer();

      FHint := TSimbaColorPickerHint.Create(FForm);
      FHint.OnKeyDown := @DoHintKeyDown;
    end;

    DesktopWindow := GetDesktopWindow();
    DesktopBounds := DesktopWindow.GetBounds();
    FDesktopImage := SimbaController.GetDesktopImage();

    FWindowSelection := SimbaController.WindowSelection.EnsureValid();

    FForm.Left := DesktopBounds.X1;
    FForm.Top := DesktopBounds.Y1;
    FForm.Width := DesktopBounds.Width;
    FForm.Height := DesktopBounds.Height;

    FForm.Invalidate(); // the form blits FDesktopImage the next time it paints

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

  if (FDesktopImage <> nil) then
    FreeAndNil(FDesktopImage);
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
  FreeAndNil(FRenderer);

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
