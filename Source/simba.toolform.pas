{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Base tool form.
}
unit simba.toolform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Forms, Menus, Graphics,
  simba.base,
  simba.image,
  simba.colormath,
  simba.component_imagebox,
  simba.component_imageboxcanvas,
  simba.component_imageboxzoom,
  simba.component_button,
  simba.component_edit,
  simba.component_menubar,
  simba.component_splitter;

type
  TACAImageSupplier = function(): TSimbaImage of object;
  TACAImageSupplierLape = function(): TByteArray of object;

  TSimbaToolForm = class(TForm)
  private const
    DEF_WIDTH = 1000;
    DEF_HEIGHT = 650;
  protected
    FMenuBar: TSimbaMenuBar;
    FDrawColorMenu: TMenuItem;
    FDrawColor: TColor;
    FSidePanel: TPanel;
    FButtonPanel: TPanel;
    FImageBox: TSimbaImageBox;
    FImageBoxZoom: TSimbaImageBoxZoomPanel;
    FImageSupplier: TACAImageSupplier;
    FImageSupplierLape: TACAImageSupplierLape;

    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoFirstShow; override;

    procedure DoLoadImageClick(Sender: TObject); virtual;
    procedure DoDrawColorChange(Sender: TObject); virtual;
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect); virtual;
    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure DoImageUpdated; virtual;

    procedure UpdateImage();

    function GetUserPanel: TPanel;
    procedure SetImage(Image: TSimbaImage);
  public
    FreeOnClose: Boolean;

    constructor Create(ImageSupplier: TACAImageSupplier); virtual; reintroduce;
    constructor CreateLape(ImageSupplier: TACAImageSupplierLape); virtual; reintroduce;

    function addButton(ACaption: String; AOnClick: TNotifyEvent): TSimbaButton;

    property UserPanel: TPanel read GetUserPanel;
    property Image: TSimbaImage write SetImage;
    property ImageBox: TSimbaImageBox read FImageBox;
  end;

implementation

uses
  LCLType, Dialogs,
  simba.env,
  simba.component_divider,
  simba.component_theme,
  simba.threading;

function TSimbaToolForm.GetUserPanel: TPanel;
begin
  Result := FImageBox.UserPanel;
end;

procedure TSimbaToolForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  if FreeOnClose then
    CloseAction := caFree;
end;

procedure TSimbaToolForm.DoFirstShow;
begin
  inherited DoFirstShow();

  UpdateImage();
end;

procedure TSimbaToolForm.DoLoadImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    InitialDir := Application.Location;
    if Execute() and FileExists(FileName) then
      FImageBox.SetImage(TSimbaImage.Create(FileName));
  finally
    Free();
  end;
end;

procedure TSimbaToolForm.DoDrawColorChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FDrawColorMenu.Count - 1 do
    FDrawColorMenu.Items[I].Checked := FDrawColorMenu.Items[I] = Sender;

  case TMenuItem(Sender).Caption of
    'Red':    FDrawColor := clRed;
    'Green':  FDrawColor := clGreen;
    'Blue':   FDrawColor := clBlue;
    'Yellow': FDrawColor := clYellow;
    'Aqua':   FDrawColor := clAqua;
  end;

  FImageBox.Repaint();
end;

procedure TSimbaToolForm.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) then
  begin
    Key := 0;
    UpdateImage();
  end;
end;

procedure TSimbaToolForm.SetImage(Image: TSimbaImage);
begin
  FImageBox.SetImage(Image);
end;

procedure TSimbaToolForm.DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);
begin
end;

procedure TSimbaToolForm.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
begin
  if FImageBox.MouseInClient then
    FImageBoxZoom.Move(FImageBox.Background.Canvas, X ,Y);
end;

procedure TSimbaToolForm.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TSimbaToolForm.DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TSimbaToolForm.UpdateImage();

  procedure SetImageNormal;
  begin
    FImageBox.SetImage(FImageSupplier());
  end;

  procedure SetImageFromLape;
  var
    LapeObject: TByteArray;
  begin
    LapeObject := FImageSupplierLape();

    FImageBox.SetImage(
      PSimbaImage(LapeObject)^,
      PSizeInt(LapeObject)[-2] <= 1 // If has no image references, we need to free it
    );
  end;

begin
  if Assigned(FImageSupplierLape) then
    SetImageFromLape()
  else if Assigned(FImageSupplier) then
    SetImageNormal();

  DoImageUpdated();
end;

procedure TSimbaToolForm.DoImageUpdated;
begin
end;

constructor TSimbaToolForm.Create(ImageSupplier: TACAImageSupplier);

  function CreateImageMenu: TPopupMenu;
  begin
    FDrawColorMenu := NewItem('Draw Color', scNone, False, True, nil, 0, '');
    FDrawColorMenu.Add(NewItem('Red', scNone, True, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Green', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Blue', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Yellow', scNone, False, True, @DoDrawColorChange, 0, ''));
    FDrawColorMenu.Add(NewItem('Aqua', scNone, False, True, @DoDrawColorChange, 0, ''));

    Result := TPopupMenu.Create(Self);
    Result.Items.Add(NewItem('Load Image', scNone, False, True, @DoLoadImageClick, 0, ''));
    Result.Items.Add(NewItem('Update Image', ShortCut(VK_F5, []), False, True, nil, 0, ''));
    Result.Items.Add(NewLine());
    Result.Items.Add(FDrawColorMenu);
  end;

begin
  inherited CreateNew(nil);

  FImageSupplier := ImageSupplier;
  FDrawColor := clRed;

  Width := Min(Scale96ToScreen(DEF_WIDTH), Monitor.WorkareaRect.Width - 200);
  Height := Min(Scale96ToScreen(DEF_HEIGHT), Monitor.WorkareaRect.Height - 100);
  Position := poScreenCenter;
  KeyPreview := True;
  OnKeyDown := @DoFormKeyDown;
  ShowInTaskBar := stAlways;
  Font.Color := SimbaComponentTheme.ColorFont;

  FMenuBar := TSimbaMenuBar.Create(Self);
  FMenuBar.Parent := Self;
  FMenuBar.Align := alTop;
  FMenuBar.AddMenu('Image', CreateImageMenu());

  FSidePanel := TPanel.Create(Self);
  FSidePanel.Parent := Self;
  FSidePanel.Align := alRight;
  FSidePanel.BevelOuter := bvNone;
  FSidePanel.BevelInner := bvNone;
  FSidePanel.Color := SimbaComponentTheme.ColorFrame;
  FSidePanel.Constraints.MinWidth := 500;

  with TSimbaSplitter.Create(Self) do
  begin
    Parent := Self;
    Align := alRight;
  end;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnImgMouseMove := @DoImgMouseMove;
  FImageBox.OnImgMouseDown := @DoImgMouseDown;
  FImageBox.OnImgMouseUp := @DoImgMouseUp;
  FImageBox.OnImgPaint := @DoImgPaintArea;

  FImageBoxZoom := TSimbaImageBoxZoomPanel.Create(FSidePanel);
  FImageBoxZoom.Parent := FSidePanel;
  FImageBoxZoom.Align := alTop;
  FImageBoxZoom.BorderSpacing.Top := 5;
  FImageBoxZoom.BorderSpacing.Bottom := 5;
  FImageBoxZoom.Font.Color := SimbaComponentTheme.ColorFont;
  FImageBoxZoom.FrameColor := SimbaComponentTheme.ColorScrollBarActive;

  with TSimbaDivider.Create(FSidePanel) do
  begin
    Parent := FSidePanel;
    Align := alBottom;
    BorderSpacing.Top := 8;
    BorderSpacing.Bottom := 8;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 5;
  end;

  FButtonPanel := TPanel.Create(FSidePanel);
  FButtonPanel.Parent := FSidePanel;
  FButtonPanel.Align := alBottom;
  FButtonPanel.AutoSize := True;
  FButtonPanel.BorderSpacing.Around := 5;
  FButtonPanel.ChildSizing.EnlargeHorizontal := crsSameSize;
  FButtonPanel.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FButtonPanel.ChildSizing.ControlsPerLine := 2;
  FButtonPanel.ChildSizing.VerticalSpacing := 5;
  FButtonPanel.ChildSizing.HorizontalSpacing := 5;
  FButtonPanel.BevelOuter := bvNone;
end;

constructor TSimbaToolForm.CreateLape(ImageSupplier: TACAImageSupplierLape);
begin
  Create(nil);

  FImageSupplierLape := ImageSupplier;
end;

function TSimbaToolForm.addButton(ACaption: String; AOnClick: TNotifyEvent): TSimbaButton;
begin
  Result := TSimbaButton.Create(FSidePanel);
  Result.Parent := FButtonPanel;
  Result.Caption := ACaption;
  Result.OnClick := AOnClick;
end;

end.

