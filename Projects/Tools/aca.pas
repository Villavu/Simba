unit ACA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, LResources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus, ColorBox,
  Types, client, mufasatypes, GraphType, Buttons, clipbrd,
  {$IFDEF WINDOWS} os_windows {$ELSE} os_linux {$ENDIF};

type
  TACAResult = procedure(CTS, Color, Tolerance: Int32; Hue, Sat: Extended) of object;

  TACAForm = class(TForm)
    btnFindBestColor: TButton;
    btnCTS2: TButton;
    btnUpdateImage: TButton;
    btnCTS1: TButton;
    btnClearImage: TButton;
    btnCTS0: TButton;
    listColors: TColorListBox;
    Divider1: TDividerBevel;
    Divider2: TDividerBevel;
    editColor: TEdit;
    editTolerance: TEdit;
    editHue: TEdit;
    editSat: TEdit;
    imgClient: TImage;
    imgMouseZoom: TImage;
    lblColor: TLabel;
    lblTolerance: TLabel;
    lblHue: TLabel;
    lblSat: TLabel;
    lblMouseZoom: TLabel;
    MainMenu: TMainMenu;
    menuImage: TMenuItem;
    menuColors: TMenuItem;
    menuColorAdd: TMenuItem;
    menuColorDelete: TMenuItem;
    menuColorClear: TMenuItem;
    menuFile: TMenuItem;
    menuNew: TMenuItem;
    menuSeperator2: TMenuItem;
    menuCopyBestColor: TMenuItem;
    menuOpen: TMenuItem;
    menuSave: TMenuItem;
    OpenDialog: TOpenDialog;
    popupColorClear: TMenuItem;
    popupColorDelete: TMenuItem;
    popupColorAdd: TMenuItem;
    menuUpdate: TMenuItem;
    menuClear: TMenuItem;
    menuSeperator1: TMenuItem;
    menuDrawColor: TMenuItem;
    menuColorRed: TMenuItem;
    menuColorGreen: TMenuItem;
    menuColorBlue: TMenuItem;
    menuColorYellow: TMenuItem;
    pnlMain: TPanel;
    pnlRight: TPanel;
    pnlZoom: TPanel;
    PopupColors: TPopupMenu;
    SaveDialog: TSaveDialog;
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;

    procedure AddColor(Sender: TObject);
    procedure ClearColors(Sender: TObject);
    procedure CalculateBestColor(Sender: TObject);
    procedure DeleteColor(Sender: TObject);
    procedure FindBestColorClick(Sender: TObject);
    procedure LoadColors(Sender: TObject);
    procedure menuCopyBestColorClick(Sender: TObject);
    procedure MouseZoomPaint(Sender: TObject);
    procedure SaveColors(Sender: TObject);
    procedure SelectColor(Sender: TObject; User: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ChangeDrawColor(Sender: TObject);
    procedure ClientImageClear(Sender: TObject);
    procedure ClientImageUpdate(Sender: TObject);
    procedure ClientImageMouseLeave(Sender: TObject);
    procedure ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageScrollDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ClientImageScrollUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ClientImageCenter(Sender: TObject);
  protected
    FScroll: record
      X, Y: Int32;
      Active: Boolean;
    end;

    FZoom: record
      Previous: Double;
      Current: Double;
    end;

    FClient: TClient;
    FManager: TIOManager;
    FDrawColor: TColor;
    FCTS: PtrInt;

    function GetColors: TIntegerArray;

    procedure Status(S: String);
    procedure Coords(X, Y: Int32);
    procedure Dimensions(W, H: Int32);

    procedure DrawTPA(TPA: TPointArray);
    procedure FindColor(Col, Tolerance: Int32; Hue, Sat: Extended);
    procedure ApplyZoom(Reset: Boolean = False);
  public
    OnGetResult: TACAResult;

    constructor Create(AManager: TIOManager); overload; reintroduce;
    destructor Destroy; override;
  end;

var
  ACAForm: TACAForm;

implementation

uses
  aca_math, mmisc, colour_conv, bitmaps;

type
  TBitmap_Helper = class helper for TBitmap
    procedure FromBitmap(Bitmap: TMufasaBitmap);
    procedure FromWindow(Manager: TIOManager; Buffer: TMufasaBitmap);
  end;

procedure TBitmap_Helper.FromBitmap(Bitmap: TMufasaBitmap);
var
  RawImage: TRawImage;
begin
  RawImage.Init();
  RawImage.Description.Init_BPP32_B8G8R8_BIO_TTB(Bitmap.Width, Bitmap.Height);
  RawImage.DataSize := RawImage.Description.Width * RawImage.Description.Height * (RawImage.Description.BitsPerPixel shr 3);
  RawImage.Data := PByte(Bitmap.FData);

  LoadFromRawImage(RawImage, False);
end;

procedure TBitmap_Helper.FromWindow(Manager: TIOManager; Buffer: TMufasaBitmap);
var
  W, H: Int32;
begin
  if Manager.TargetValid() then
  begin
    Manager.GetDimensions(W, H);

    Buffer.CopyClientToBitmap(Manager, True, 0, 0, W-1, H-1);
  end;

  FromBitmap(Buffer);
end;

procedure TACAForm.ApplyZoom(Reset: Boolean);
var
  X, Y, W, H: Int32;
  P: TPoint;
begin
  if Reset then
  begin
    FZoom.Previous := 1;
    FZoom.Current := 1;
  end;

  // Zoom in towards the mouse cursor
  X := Mouse.CursorPos.X - Self.ClientToScreen(Point(ScrollBox.ClientRect.Left, 0)).X;
  Y := Mouse.CursorPos.Y - Self.ClientToScreen(Point(ScrollBox.ClientRect.Top, 0)).Y;

  W := imgClient.Width;
  H := imgClient.Height;

  // Get the center of the currently visible area at 1x zoom.
  P.X := Trunc(ScrollBox.HorzScrollBar.Position + (X * FZoom.Previous) / FZoom.Previous);
  P.Y := Trunc(ScrollBox.VertScrollBar.Position + (Y * FZoom.Previous) / FZoom.Previous);

  imgClient.Width := Trunc(FClient.MBitmaps[0].Width * FZoom.Current);
  imgClient.Height := Trunc(FClient.MBitmaps[0].Height * FZoom.Current);

  if (not (fsCreating in FormState)) then
  begin
    ScrollBox.HorzScrollBar.Position := Trunc(P.X * imgClient.Width / W) - X;
    ScrollBox.VertScrollBar.Position := Trunc(P.Y * imgClient.Height / H) - Y;
  end;

  ClientImageCenter(nil);
end;

procedure TACAForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R, G, B: Byte;
  H, S, L: Extended;
  localX, localY, globalX, globalY: Int32;
begin
  if FScroll.Active then
  begin
    if (Abs(FScroll.Y - Y) > 0) then
      ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position + (FScroll.Y - Y);
    if (Abs(FScroll.X - X) > 0) then
      ScrollBox.HorzScrollBar.Position := ScrollBox.HorzScrollBar.Position + (FScroll.X - X);
  end else
  begin
    X := Trunc(X / FZoom.Current);
    Y := Trunc(Y / FZoom.Current);

    imgMouseZoom.Picture.Bitmap.BeginUpdate(True);
    imgMouseZoom.Tag := PtrInt(True);

    try
      imgMouseZoom.Picture.Bitmap.Canvas.Clear();

      for localX := 0 to 5 do
        for localY := 0 to 5 do
        begin
          globalX := X + localX - 2;
          globalY := Y + localY - 2;

          if FClient.MBitmaps[0].PointInBitmap(globalX, globalY) then
            imgMouseZoom.Picture.Bitmap.Canvas.Pixels[localX, localY] := FClient.MBitmaps[0].FastGetPixel(globalX, globalY)
          else
            imgMouseZoom.Picture.Bitmap.Canvas.Pixels[localX, localY] := clBlack;
        end;
    finally
      imgMouseZoom.Picture.Bitmap.EndUpdate(False);
    end;

    ColorToRGB(FClient.MBitmaps[0].FastGetPixel(X, Y), R, G, B);
    ColorToHSL(FClient.MBitmaps[0].FastGetPixel(X, Y), H, S, L);

    lblMouseZoom.Caption := Format('Color: %d', [FClient.MBitmaps[0].FastGetPixel(X, Y)]) + LineEnding +
                            Format('RGB: [%d, %d, %d]', [R, G, B])                        + LineEnding +
                            Format('HSL: [%f, %f, %f]', [H, S, L])                        + LineEnding;

    Coords(X, Y);
  end;
end;

procedure TACAForm.ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := Trunc(X / FZoom.Current);
  Y := Trunc(Y / FZoom.Current);

  case Button of
    mbRight:
      begin
        imgClient.Cursor := crSizeAll;

        FScroll.X := Trunc(X * FZoom.Current);
        FScroll.Y := Trunc(Y * FZoom.Current);
        FScroll.Active := True;
      end;

    mbLeft:
      with FClient.MBitmaps[0] do
      begin
        if (listColors.Items.IndexOf(IntToStr(FastGetPixel(X, Y))) = -1) then
          listColors.AddItem(IntToStr(FastGetPixel(X, Y)), TObject(PtrUInt(FastGetPixel(X, Y))));
      end;
  end;
end;

procedure TACAForm.ClientImageScrollDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
  begin
    FZoom.Previous := FZoom.Current;
    FZoom.Current /= 2.00;
    if (FZoom.Current < 0.0625) then
      FZoom.Current := 0.0625;

    ApplyZoom();
  end;
end;

procedure TACAForm.ClientImageScrollUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
  begin
    FZoom.Previous := FZoom.Current;
    FZoom.Current *= 2.00;
    if (FZoom.Current > {$IFDEF LINUX}2.00{$ELSE}32.00{$ENDIF}) then
      FZoom.Current := {$IFDEF LINUX}2.00{$ELSE}32.00{$ENDIF};
    // Weird limitation in SetBounds
    if (Trunc(FClient.MBitmaps[0].Width * FZoom.Current) > 100000) or (Trunc(FClient.MBitmaps[0].Height * FZoom.Current) > 100000) then
      FZoom.Current := FZoom.Previous;

    ApplyZoom();
  end;
end;

procedure TACAForm.ClientImageCenter(Sender: TObject);
begin
  if (imgClient.Width < ScrollBox.ClientWidth) then
    imgClient.Left := (ScrollBox.ClientWidth div 2) - (imgClient.Width div 2)
  else
    imgClient.Left := 0;

  if (imgClient.Height < ScrollBox.ClientHeight) then
    imgClient.Top := (ScrollBox.ClientHeight div 2) - (imgClient.Height div 2)
  else
    imgClient.Top := 0;
end;

procedure TACAForm.ChangeDrawColor(Sender: TObject);
var
  i: Int32;
begin
  with Sender as TMenuItem do
  begin
    for i := 0 to Parent.Count - 1 do
      if (Parent[i] <> Sender) then
        Parent[i].Checked := False;

    case Caption of
      'Red': FDrawColor := clRed;
      'Green': FDrawColor := clGreen;
      'Blue': FDrawColor := clBlue;
      'Yellow': FDrawColor := clYellow;
    end;
  end;
end;

procedure TACAForm.Status(S: String);
begin
  StatusBar.Panels[2].Text := S;
end;

procedure TACAForm.Coords(X, Y: Int32);
begin
  StatusBar.Panels[0].Text := Format('(%d, %d)', [X, Y]);
end;

procedure TACAForm.Dimensions(W, H: Int32);
begin
  StatusBar.Panels[1].Text := IntToStr(W) + 'x' + IntToStr(H);
end;

procedure TACAForm.FindColor(Col, Tolerance: Int32; Hue, Sat: Extended);
var
  W, H: Int32;
  TPA: TPointArray;
  T: Double;
begin
  T := MarkTime();

  FClient.IOManager.GetDimensions(W, H);

  FClient.MFinder.SetToleranceSpeed(FCTS);
  FClient.MFinder.SetToleranceSpeed2Modifiers(Hue, Sat);
  FClient.MFinder.FindColorsTolerance(TPA, Col, 0, 0, W-1, H-1, Tolerance);

  Status(Format('Found %d matches in %f ms', [Length(TPA), MarkTime() - T]));

  DrawTPA(TPA);
end;

procedure TACAForm.DrawTPA(TPA: TPointArray);
var
  P: TPoint;
  Data: PByte;
  BytesPerLine, BytesPerPixel: Int32;
  RGB: TRGB24;
begin
  imgClient.Picture.Bitmap.FromBitmap(FClient.MBitmaps[0]);
  imgClient.Picture.Bitmap.BeginUpdate(False);

  Data := imgClient.Picture.Bitmap.RawImage.Data;
  BytesPerLine := imgClient.Picture.Bitmap.RawImage.Description.BytesPerLine;
  BytesPerPixel := imgClient.Picture.Bitmap.RawImage.Description.BitsPerPixel div 8;

  try
    ColorToRGB(FDrawColor, RGB.R, RGB.G, RGB.B);

    for P in TPA do
      Move(RGB, Data[P.Y * BytesPerLine + P.X * BytesPerPixel], SizeOf(TRGB24));
  finally
    imgClient.Picture.Bitmap.EndUpdate(False);
  end;
end;

function TACAForm.GetColors: TIntegerArray;
var
  i: Int32;
begin
  SetLength(Result, listColors.Count);
  for i := 0 to listColors.Count - 1 do
    Result[i] := listColors.Colors[i];
end;

procedure TACAForm.ClientImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbRight:
      begin
        FScroll.Active := not FScroll.Active;
        if (not FScroll.Active) then
          imgClient.Cursor := crDefault;
      end;
  end;
end;

procedure TACAForm.ClientImageUpdate(Sender: TObject);
begin
  imgClient.Picture.Bitmap.FromWindow(FManager, FClient.MBitmaps[0]);

  Dimensions(FClient.MBitmaps[0].Width, FClient.MBitmaps[0].Height);
  ApplyZoom(True);
end;

procedure TACAForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if (OnGetResult <> nil) then
    OnGetResult(FCTS, StrToIntDef(editColor.Text, -1), StrToIntDef(editTolerance.Text, -1), StrToFloatDef(editHue.Text, -1), StrToFloatDef(editSat.Text, -1));
end;

procedure TACAForm.MouseZoomPaint(Sender: TObject);
begin
  with Sender as TGraphicControl do
    if Boolean(Tag) then
    begin
      Canvas.Pen.Color := FDrawColor;
      Canvas.Frame(40, 40, 60, 60);
    end;
end;

procedure TACAForm.SaveColors(Sender: TObject);
begin
  if SaveDialog.Execute() then
    listColors.Items.SaveToFile(SaveDialog.FileName);
end;

procedure TACAForm.SelectColor(Sender: TObject; User: Boolean);
var
  R, G, B: Int32;
  H, S, L: Extended;
begin
  if TColorListBox(Sender).ItemIndex >= 0 then
  begin
    imgMouseZoom.Tag := PtrInt(False);
    imgMouseZoom.Canvas.Brush.Color := TColorListBox(Sender).Selected;
    imgMouseZoom.Canvas.Clear();

    ColorToRGB(TColorListBox(Sender).Selected, R, G, B);
    ColorToHSL(TColorListBox(Sender).Selected, H, S, L);

    lblMouseZoom.Caption := Format('Color: %d', [TColorListBox(Sender).Selected]) + LineEnding +
                            Format('RGB: [%d, %d, %d]', [R, G, B])                + LineEnding +
                            Format('HSL: [%f, %f, %f]', [H, S, L])                + LineEnding;
  end;
end;

procedure TACAForm.ClientImageMouseLeave(Sender: TObject);
begin
  Coords(-1, -1);
end;

procedure TACAForm.FindBestColorClick(Sender: TObject);
var
  Col, Tolerance: Int32;
  Hue, Sat: Extended;
begin
  try
    Col := StrToInt(editColor.Text);
    Tolerance := StrToInt(editTolerance.Text);

    if (FCTS = 2) then
    begin
      Hue := StrToFloat(editHue.Text);
      Sat := StrToFloat(editSat.Text);
    end;
  except
    Exit;
  end;

  FindColor(Col, Tolerance, Hue, Sat);
end;

procedure TACAForm.LoadColors(Sender: TObject);
var
  i: Int32;
begin
  if OpenDialog.Execute() and FileExists(OpenDialog.FileName) then
  begin
    listColors.Items.BeginUpdate();
    listColors.Items.Clear();
    listColors.Items.LoadFromFile(OpenDialog.FileName);

    for i := 0 to listColors.Items.Count - 1 do
      listColors.Items.Objects[i] := TObject(PtrInt(StrToIntDef(listColors.Items[i], 0)));

    listColors.Items.EndUpdate();
  end;
end;

procedure TACAForm.menuCopyBestColorClick(Sender: TObject);
begin
  case FCTS of
    0: Clipboard.AsText := Format('CTS0(%s, %s)', [editColor.Text, editTolerance.Text]);
    1: Clipboard.AsText := Format('CTS1(%s, %s)', [editColor.Text, editTolerance.Text]);
    2: Clipboard.AsText := Format('CTS2(%s, %s, %s, %s)', [editColor.Text, editTolerance.Text, editHue.Text, editSat.Text]);
  end;
end;

procedure TACAForm.CalculateBestColor(Sender: TObject);
var
  Col, Tolerance: Int32;
  Hue, Sat: Extended;
begin
  if Length(GetColors()) = 0 then
  begin
    editColor.Text := '';
    editTolerance.Text := '';
    editHue.Text := '';
    editSat.Text := '';
  end else
  begin
    FCTS := TButton(Sender).Tag;

    case FCTS of
      0: BestColor_CTS0(GetColors(), Col, Tolerance);
      1: BestColor_CTS1(GetColors(), Col, Tolerance);
      2: BestColor_CTS2(GetColors(), Col, Tolerance, Hue, Sat);
    end;

    editColor.Text := Format('%d', [Col]);
    editTolerance.Text := Format('%d', [Tolerance]);
    editHue.Text := '';
    editSat.Text := '';

    if (FCTS = 2) then
    begin
      editHue.Text := Format('%f', [Hue]);
      editSat.Text := Format('%f', [Sat]);
    end;
  end;
end;

procedure TACAForm.AddColor(Sender: TObject);
var
  Input: String = '';
begin
  if InputQuery('Add Color', 'Color:', Input) and (StrToIntDef(Input, -1) >= 0) then
    listColors.AddItem(Input, TObject(PtrInt(StrToInt(Input))));
end;

procedure TACAForm.ClearColors(Sender: TObject);
begin
  listColors.Items.Clear();

  editColor.Text := '';
  editTolerance.Text := '';
  editHue.Text := '';
  editSat.Text := '';
end;

procedure TACAForm.DeleteColor(Sender: TObject);
begin
  if (listColors.ItemIndex >= 0) then
    listColors.Items.Delete(listColors.ItemIndex);
end;

procedure TACAForm.ClientImageClear(Sender: TObject);
begin
  imgClient.Picture.Bitmap.FromBitmap(FClient.MBitmaps[0]);
end;

constructor TACAForm.Create(AManager: TIOManager);
begin
  inherited Create(Application.MainForm);

  FManager := AManager;
  FDrawColor := clRed;
  FClient := TClient.Create();
  FClient.MBitmaps.AddBMP(TMufasaBitmap.Create()); // MBitmaps[0] will be our client image.
  FClient.IOManager.SetTarget(FClient.MBitmaps[0]);

  imgMouseZoom.Picture.Bitmap.Width := 5;
  imgMouseZoom.Picture.Bitmap.Height := 5;

  listColors.Options := listColors.Options - [lboDrawFocusRect];

  ScrollBox.DoubleBuffered := True;

  ClientImageUpdate(nil);
end;

destructor TACAForm.Destroy;
begin
  FClient.Free();

  inherited Destroy();
end;

initialization
  {$I aca.lrs}

end.

