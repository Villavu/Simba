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
  Classes, SysUtils, Controls, ExtCtrls, Forms, Menus,
  simba.base,
  simba.image,
  simba.component_imagebox,
  simba.canvas,
  simba.component_imageboxzoom,
  simba.component_button,
  simba.component_divider,
  simba.component_menubar;

type
  TImageSupplier = function(): TSimbaImage of object;
  TImageSupplierLape = function(): TByteArray of object;

  TSimbaToolForm = class(TForm)
  private const
    DEF_WIDTH = 1000;
    DEF_HEIGHT = 650;
    RECENT_IMAGE_COUNT = 5;
  protected
    FMenuBar: TSimbaMenuBar;
    FRecentImagesMenu: TMenuItem;
    FSidePanelFrame: TPanel;
    FSidePanel: TPanel;
    FSidePanelPercent: Single; // its share of the form's width, kept as the form resizes
    FDivider: TSimbaDivider;
    FButtonPanel: TSimbaButtonGrid;
    FImageBox: TSimbaImageBox;
    FImageBoxZoom: TSimbaImageBoxZoomPanel;
    FTopLayer: TSimbaImageBoxLayer;
    FImageSupplier: TImageSupplier;
    FImageSupplierLape: TImageSupplierLape;
    FUpdateImageOnFirstShow: Boolean;

    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoFirstShow; override;
    procedure DoOnResize; override;
    procedure DoSidePanelResizing(Sender: TObject; var NewSize: Integer; var Accept: Boolean); virtual;
    procedure DoSidePanelFrameResize(Sender: TObject); virtual;
    procedure DoSidePanelMoved(Sender: TObject); virtual;

    procedure DoImageMenuPopup(Sender: TObject); virtual;
    procedure DoLoadImageClick(Sender: TObject); virtual;
    procedure DoRecentImageClick(Sender: TObject); virtual;
    procedure DoUpdateImageClick(Sender: TObject); virtual;
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaCanvas; R: TRect); virtual;
    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure UpdateImage(); virtual;
    // put first in Recent Images, which every tool form shares
    procedure LoadImage(FileName: String); virtual;
    // the image box it is built round: a descendant can make its own
    function CreateImageBox: TSimbaImageBox; virtual;

    function GetShowButtonDivider: Boolean; virtual;
    procedure SetShowButtonDivider(Value: Boolean); virtual;
    function GetShowZoom: Boolean; virtual;
    procedure SetShowZoom(Value: Boolean); virtual;
    function GetTopLayer: TSimbaImageBoxLayer; virtual;
    // every image change comes through here; nil leaves the image as it is
    procedure SetImage(Value: TSimbaImage); virtual;
  public
    FreeOnClose: Boolean;

    constructor Create(ImageSupplier: TImageSupplier); virtual; reintroduce;
    constructor CreateLape(ImageSupplier: TImageSupplierLape); virtual; reintroduce;

    function addButton(ACaption: String; AOnClick: TNotifyEvent): TSimbaButton; virtual;
    // a menu on the menu bar, for its items
    function addMenu(ACaption: String; AOnPopup: TNotifyEvent = nil): TMenuItem; virtual;
    // made and added to AParent; a caption of '-' is a line
    function addMenuItem(AParent: TMenuItem; ACaption: String; AOnClick: TNotifyEvent = nil; AShortCut: TShortCut = scNone): TMenuItem; virtual;
    // the Image menu with Load Image, Recent Images and Update Image (F5), for a form's own image items after
    function addImageMenu: TMenuItem; virtual;

    // the line over the buttons at the bottom of the side panel
    property ShowButtonDivider: Boolean read GetShowButtonDivider write SetShowButtonDivider;
    // the magnifier at the top of the side panel
    property ShowZoom: Boolean read GetShowZoom write SetShowZoom;
    // a layer over the image and every other layer, made the first time it is asked for
    property TopLayer: TSimbaImageBoxLayer read GetTopLayer;
    property Image: TSimbaImage write SetImage;
    property ImageBox: TSimbaImageBox read FImageBox;
  end;

implementation

uses
  LCLType, Dialogs, LazFileUtils,
  simba.component_splitter,
  simba.component_theme,
  simba.settings;

function TSimbaToolForm.GetShowButtonDivider: Boolean;
begin
  Result := FDivider.Visible;
end;

procedure TSimbaToolForm.SetShowButtonDivider(Value: Boolean);
begin
  FDivider.Visible := Value;
end;

function TSimbaToolForm.GetShowZoom: Boolean;
begin
  Result := FImageBoxZoom.Visible;
end;

procedure TSimbaToolForm.SetShowZoom(Value: Boolean);
begin
  FImageBoxZoom.Visible := Value;
end;

function TSimbaToolForm.GetTopLayer: TSimbaImageBoxLayer;
begin
  if (FTopLayer = nil) then
  begin
    FTopLayer := TSimbaImageBoxLayer.Create(FImageBox);
    FTopLayer.Priority := High(Integer);
  end;

  Result := FTopLayer;
end;

function TSimbaToolForm.CreateImageBox: TSimbaImageBox;
begin
  Result := TSimbaImageBox.Create(Self);
end;

procedure TSimbaToolForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  if FreeOnClose then
    CloseAction := caFree;
end;

procedure TSimbaToolForm.DoFirstShow;
var
  Area: TRect;
begin
  inherited DoFirstShow();

  Area := Screen.MonitorFromRect(BoundsRect).WorkareaRect;
  if (Width > Area.Width) or (Height > Area.Height) then
    WindowState := wsMaximized;

  if FUpdateImageOnFirstShow then
    UpdateImage();
end;

// Like docking's ScaleOnResize: the side panel keeps its share of the width as the form resizes.
procedure TSimbaToolForm.DoOnResize;
begin
  inherited DoOnResize();

  if (FSidePanelFrame <> nil) and (FImageBox <> nil) then
    FSidePanelFrame.Width := Min(Round(ClientWidth * FSidePanelPercent), ClientWidth - FImageBox.Constraints.MinWidth);
end;

// Lock resizing of all components while dragging to prevent tons of updates
procedure TSimbaToolForm.DoSidePanelResizing(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  if (FSidePanel.Constraints.MaxWidth = 0) then
  begin
    FSidePanel.Constraints.MinWidth := FSidePanel.Width;
    FSidePanel.Constraints.MaxWidth := FSidePanel.Width;
  end;
end;

procedure TSimbaToolForm.DoSidePanelFrameResize(Sender: TObject);
begin
  FSidePanelFrame.Update();
end;

procedure TSimbaToolForm.DoSidePanelMoved(Sender: TObject);
begin
  FSidePanelPercent := FSidePanelFrame.Width / ClientWidth;

  // let go of the update locking
  FSidePanel.Constraints.MaxWidth := 0;
  FSidePanel.Constraints.MinWidth := 0;
end;

procedure TSimbaToolForm.DoImageMenuPopup(Sender: TObject);
var
  Files: TStringList;
  I: Integer;
begin
  FRecentImagesMenu.Clear();

  Files := TStringList.Create();
  try
    Files.Text := SimbaSettings.General.RecentImages.Value;
    for I := 0 to Files.Count - 1 do
      if FileExists(Files[I]) then
        addMenuItem(FRecentImagesMenu, ShortDisplayFilename(Files[I]), @DoRecentImageClick).Hint := Files[I];
  finally
    Files.Free();
  end;

  FRecentImagesMenu.Enabled := (FRecentImagesMenu.Count > 0);
end;

procedure TSimbaToolForm.DoLoadImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    InitialDir := Application.Location;
    if Execute() and FileExists(FileName) then
      LoadImage(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaToolForm.DoRecentImageClick(Sender: TObject);
begin
  LoadImage(TMenuItem(Sender).Hint);
end;

procedure TSimbaToolForm.DoUpdateImageClick(Sender: TObject);
begin
  UpdateImage();
end;

procedure TSimbaToolForm.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) then
  begin
    Key := 0;
    UpdateImage();
  end;
end;

procedure TSimbaToolForm.SetImage(Value: TSimbaImage);
begin
  if (Value = nil) then
    Exit;

  FUpdateImageOnFirstShow := False; // an image given before the first show is the one shown
  FImageBox.Background := Value;
  // the magnifier still has the old image, just freed, for a repaint it has queued
  if FImageBoxZoom.Visible then
    FImageBoxZoom.Move(Value, FImageBox.MouseXY.X, FImageBox.MouseXY.Y);
end;

procedure TSimbaToolForm.DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaCanvas; R: TRect);
begin
end;

procedure TSimbaToolForm.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
begin
  if FImageBoxZoom.Visible and FImageBox.MouseInClient then
    FImageBoxZoom.Move(FImageBox.Background, X, Y);
end;

procedure TSimbaToolForm.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TSimbaToolForm.DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

// no image to give keeps the one shown
procedure TSimbaToolForm.UpdateImage();
var
  LapeObject: TByteArray;
begin
  if Assigned(FImageSupplierLape) then
  begin
    LapeObject := FImageSupplierLape();
    if (LapeObject <> nil) then
      SetImage(PSimbaImage(LapeObject)^.Copy());
  end
  else if Assigned(FImageSupplier) then
    SetImage(FImageSupplier());
end;

procedure TSimbaToolForm.LoadImage(FileName: String);
var
  Files: TStringList;
begin
  SetImage(TSimbaImage.Create(FileName));

  Files := TStringList.Create();
  try
    Files.CaseSensitive := FileNameCaseSensitive;
    Files.Text := SimbaSettings.General.RecentImages.Value;
    if (Files.IndexOf(FileName) > -1) then
      Files.Delete(Files.IndexOf(FileName));
    Files.Insert(0, FileName);
    while (Files.Count > RECENT_IMAGE_COUNT) do
      Files.Delete(Files.Count - 1);
    SimbaSettings.General.RecentImages.Value := Files.Text;
  finally
    Files.Free();
  end;
end;

constructor TSimbaToolForm.Create(ImageSupplier: TImageSupplier);
begin
  inherited CreateNew(nil);

  FImageSupplier := ImageSupplier;
  FUpdateImageOnFirstShow := True;

  Width := Scale96ToScreen(DEF_WIDTH);
  Height := Scale96ToScreen(DEF_HEIGHT);
  Position := poScreenCenter;
  KeyPreview := True;
  OnKeyDown := @DoFormKeyDown;
  ShowInTaskBar := stAlways;
  Font.Color := SimbaComponentTheme.ColorFont;

  FMenuBar := TSimbaMenuBar.Create(Self);
  FMenuBar.Parent := Self;
  FMenuBar.Align := alTop;
  FMenuBar.Visible := False; // until a menu is added

  FSidePanelFrame := TPanel.Create(Self);
  FSidePanelFrame.Parent := Self;
  FSidePanelFrame.Align := alRight;
  FSidePanelFrame.BevelOuter := bvNone;
  FSidePanelFrame.BevelInner := bvNone;
  FSidePanelFrame.Color := SimbaComponentTheme.ColorFrame;
  FSidePanelFrame.Constraints.MinWidth := Scale96ToScreen(150); // only so dragging cannot lose it
  FSidePanelFrame.OnResize := @DoSidePanelFrameResize;
  FSidePanelPercent := 0.3;
  FSidePanelFrame.Width := Round(ClientWidth * FSidePanelPercent);

  FSidePanel := TPanel.Create(Self);
  FSidePanel.Parent := FSidePanelFrame;
  FSidePanel.Align := alClient;
  FSidePanel.BevelOuter := bvNone;
  FSidePanel.BevelInner := bvNone;
  FSidePanel.Color := SimbaComponentTheme.ColorFrame;

  with TSimbaSplitter.Create(Self) do
  begin
    Parent := Self;
    Align := alRight;
    OnCanResize := @DoSidePanelResizing;
    OnMoved := @DoSidePanelMoved;
  end;

  FImageBox := CreateImageBox();
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.Constraints.MinWidth := Scale96ToScreen(200);
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

  FDivider := TSimbaDivider.Create(FSidePanel);
  FDivider.Parent := FSidePanel;
  FDivider.Align := alBottom;
  FDivider.BorderSpacing.Top := 8;
  FDivider.BorderSpacing.Bottom := 8;
  FDivider.BorderSpacing.Left := 5;
  FDivider.BorderSpacing.Right := 5;

  FButtonPanel := TSimbaButtonGrid.Create(FSidePanel);
  FButtonPanel.Parent := FSidePanel;
  FButtonPanel.Align := alBottom;
  FButtonPanel.BorderSpacing.Around := 5;
end;

constructor TSimbaToolForm.CreateLape(ImageSupplier: TImageSupplierLape);
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

function TSimbaToolForm.addMenu(ACaption: String; AOnPopup: TNotifyEvent): TMenuItem;
var
  Popup: TPopupMenu;
begin
  Popup := TPopupMenu.Create(Self);
  Popup.OnPopup := AOnPopup;
  FMenuBar.AddMenu(ACaption, Popup);
  FMenuBar.Visible := True;

  Result := Popup.Items;
end;

function TSimbaToolForm.addMenuItem(AParent: TMenuItem; ACaption: String; AOnClick: TNotifyEvent; AShortCut: TShortCut): TMenuItem;
begin
  Result := NewItem(ACaption, AShortCut, False, True, AOnClick, 0, '');
  AParent.Add(Result);
end;

function TSimbaToolForm.addImageMenu: TMenuItem;
begin
  Result := addMenu('Image', @DoImageMenuPopup);
  addMenuItem(Result, 'Load Image', @DoLoadImageClick);
  FRecentImagesMenu := addMenuItem(Result, 'Recent Images');
  addMenuItem(Result, 'Update Image', @DoUpdateImageClick, ShortCut(VK_F5, [])); // the key itself is DoFormKeyDown's
end;

end.

