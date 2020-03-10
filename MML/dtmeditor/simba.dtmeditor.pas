unit simba.dtmeditor;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, fileutil, dividerbevel, lresources, forms, controls,
  graphics, dialogs, extctrls, comctrls, stdctrls, menus, lcltype,
  simba.client, simba.dtm, simba.imagebox, simba.mufasatypes;

type
  TSimbaDTMEditorForm = class(TForm)
    ButtonUpdateImage: TButton;
    ButtonClearImage: TButton;
    MenuItemUpdateImage: TMenuItem;
    MenuItemClearImage: TMenuItem;
    MenuItemFindDTM: TMenuItem;
    MenuItemPrintDTM: TMenuItem;
    MenuItemOffsetDTM: TMenuItem;
    MenuItemLoadImage: TMenuItem;
    PanelAlignment: TPanel;
    ButtonPrintDTM: TButton;
    FindDTMButton: TButton;
    ButtonDeletePoints: TButton;
    ButtonDebugColor: TButton;
    ButtonDeletePoint: TButton;
    Divider1: TDividerBevel;
    Divider3: TDividerBevel;
    Divider2: TDividerBevel;
    EditPointX: TEdit;
    EditPointY: TEdit;
    EditPointColor: TEdit;
    EditPointTolerance: TEdit;
    EditPointSize: TEdit;
    ImageZoom: TImage;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelColor: TLabel;
    LabelTolerance: TLabel;
    LabelSize: TLabel;
    LabelColorZoom: TLabel;
    ListBox: TListBox;
    MainMenu: TMainMenu;
    MenuItemImage: TMenuItem;
    PanelColorZoom: TPanel;
    PanelSelectedPoint: TPanel;
    MenuDTM: TMenuItem;
    MenuItemLoadDTM: TMenuItem;
    MenuItemSeperator: TMenuItem;
    MenuItemDebugColor: TMenuItem;
    MenuItemColorRed: TMenuItem;
    MenuItemColorGreen: TMenuItem;
    MenuItemColorBlue: TMenuItem;
    MenuItemColorYellow: TMenuItem;
    PanelMain: TPanel;
    PanelRight: TPanel;
    PanelZoom: TPanel;
    PointFlashTimer: TTimer;

    procedure ButtonClearImageClick(Sender: TObject);
    procedure ButtonDeletePointsClick(Sender: TObject);
    procedure ButtonDebugColorClick(Sender: TObject);
    procedure ButtonDeletePointClick(Sender: TObject);
    procedure CenterDivider(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItemOffsetDTMClick(Sender: TObject);
    procedure MenuItemLoadImageClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure PointEditChanged(Sender: TObject);
    procedure ButtonPrintDTMClick(Sender: TObject);
    procedure FindDTMClick(Sender: TObject);
    procedure PointFlash(Sender: TObject);
    procedure LoadDTMClick(Sender: TObject);
    procedure MouseZoomPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ChangeDrawColor(Sender: TObject);
    procedure ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClientImageClear(Sender: TObject);
    procedure ButtonUpdateImageClick(Sender: TObject);
  protected
    FImageBox: TSimbaImageBox;
    FClient: TClient;
    FDragging: Int32;

    procedure FontChanged(Sender: TObject); override;

    procedure AddPoint(X, Y, Col, Tol, Size: Int32); overload;
    procedure AddPoint(X, Y, Col: Int32); overload;
    procedure EditPoint(Index: Int32; X, Y, Col, Tol, Size: Int32);
    procedure OffsetPoint(Index: Int32; X, Y: Int32);

    function GetPointAt(X, Y: Int32): Int32;
    function GetPoint(Index: Int32): TMDTMPoint;
    function GetPoints: TMDTMPointArray;
    function GetDTM: TMDTM;

    procedure DrawDTM;
  public
    OnPrintDTM: procedure(constref DTM: String) of object;

    constructor Create(TargetWindow: THandle); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  math,
  simba.colormath;

procedure TSimbaDTMEditorForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  R, G, B: Byte;
  H, S, L: Extended;
  localX, localY, globalX, globalY: Int32;
  Point: TMDTMPoint;
begin
  ImageZoom.Picture.Bitmap.BeginUpdate(True);
  ImageZoom.Tag := PtrInt(True);

  try
    for localX := 0 to 5 do
      for localY := 0 to 5 do
      begin
        globalX := X + localX - 2;
        globalY := Y + localY - 2;

        ImageZoom.Picture.Bitmap.Canvas.Pixels[localX, localY] :=  FImageBox.Background.Canvas.Pixels[globalX, globalY];
      end;
  finally
    ImageZoom.Picture.Bitmap.EndUpdate(False);
  end;

  ColorToRGB(FImageBox.Background.Canvas.Pixels[X, Y], R, G, B);
  ColorToHSL(FImageBox.Background.Canvas.Pixels[X, Y], H, S, L);

  LabelColorZoom.Caption := Format('Color: %d', [FImageBox.Background.Canvas.Pixels[X, Y]]) + LineEnding +
                            Format('RGB: %d, %d, %d', [R, G, B])                            + LineEnding +
                            Format('HSL: %f, %f, %f', [H, S, L])                            + LineEnding;
  if (FDragging > -1) then
  begin
    Point := GetPoint(FDragging);

    EditPoint(FDragging, X, Y, FImageBox.Background.Canvas.Pixels[X, Y], Point.T, Point.ASZ);
  end;

  if (GetPointAt(X, Y) > -1) then
    FImageBox.Cursor := crHandPoint
  else
    FImageBox.Cursor := crDefault;
end;

procedure TSimbaDTMEditorForm.ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
      begin
        FDragging := GetPointAt(X, Y);

        case FImageBox.Cursor of
          crDefault:
            begin
              AddPoint(X, Y, FImageBox.Background.Canvas.Pixels[X, Y]);

              FImageBox.Cursor := crHandPoint;
            end;

          crHandPoint:
            ListBox.ItemIndex := GetPointAt(X, Y);
        end;
      end;
  end;
end;

procedure TSimbaDTMEditorForm.ClientImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
    FDragging := -1;
end;

procedure TSimbaDTMEditorForm.ChangeDrawColor(Sender: TObject);
var
  i: Int32;
begin
  with Sender as TMenuItem do
  begin
    for i := 0 to Parent.Count - 1 do
      if (Parent[i] <> Sender) then
        Parent[i].Checked := False;

    case Caption of
      'Red': FImageBox.Overlay.Canvas.Pen.Color := clRed;
      'Green': FImageBox.Overlay.Canvas.Pen.Color := clGreen;
      'Blue': FImageBox.Overlay.Canvas.Pen.Color := clBlue;
      'Yellow': FImageBox.Overlay.Canvas.Pen.Color := clYellow;
    end;
  end;
end;

procedure TSimbaDTMEditorForm.DrawDTM;
var
  Points: TMDTMPointArray;
  Point: TMDTMPoint;
  i: Int32;
begin
  PointFlashTimer.Enabled := True;

  FImageBox.Overlay.Clear();

  Points := GetPoints();

  if (Length(Points) > 0) then
  begin
    for i := 0 to High(Points) do
    begin
      Point := Points[i];

      if (Length(Points) > 1) then // Connect to main point
      begin
        FImageBox.Overlay.Canvas.Line(Points[0].X, Points[0].Y, Point.X, Point.Y);
        FImageBox.Overlay.Canvas.FillRect(Points[0].X - Max(1, Points[0].ASZ), Points[0].Y - Max(1, Points[0].ASZ),
                                          Points[0].X + Max(1, Points[0].ASZ), Points[0].Y + Max(1, Points[0].ASZ));
      end;

      FImageBox.Overlay.Canvas.FillRect(Point.X - Max(1, Point.ASZ), Point.Y - Max(1, Point.ASZ),
                                        Point.X + Max(1, Point.ASZ), Point.Y + Max(1, Point.ASZ));
    end;
  end;

  FImageBox.Update();
end;

function TSimbaDTMEditorForm.GetPointAt(X, Y: Int32): Int32;
var
  Points: TMDTMPointArray;
  i: Int32;
begin
  Result := -1;

  Points := GetPoints();

  for i := 0 to High(Points) do
  begin
    if (X >= Points[i].X - Max(1, Points[i].ASZ)) and (Y >= Points[i].Y - Max(1, Points[i].ASZ)) and
       (X <= Points[i].X + Max(1, Points[i].ASZ)) and (Y <= Points[i].Y + Max(1, Points[i].ASZ)) then
      begin
        Result := i;
        Break;
      end;
  end;
end;

procedure TSimbaDTMEditorForm.ButtonUpdateImageClick(Sender: TObject);
var
  W, H: Int32;
begin
  if not FClient.IOManager.TargetValid then
    FClient.IOManager.SetDesktop();

  FClient.IOManager.GetDimensions(W, H);
  FClient.MBitmaps[0].CopyClientToBitmap(FClient.IOManager, True, 0, 0, W-1, H-1);

  FImageBox.Background.LoadFromMufasaBitmap(FClient.MBitmaps[0]);
  FImageBox.BackgroundChanged();

  DrawDTM();
end;

procedure TSimbaDTMEditorForm.AddPoint(X, Y, Col, Tol, Size: Int32);
begin
  ListBox.ItemIndex := ListBox.Items.Add('%d, %d, %d, %d, %d', [X, Y, Col, Tol, Size]);
  DrawDTM();
end;

procedure TSimbaDTMEditorForm.AddPoint(X, Y, Col: Int32);
begin
  ListBox.ItemIndex := ListBox.Items.Add('%d, %d, %d, 0, 1', [X, Y, Col]);
  DrawDTM();
end;

procedure TSimbaDTMEditorForm.EditPoint(Index: Int32; X, Y, Col, Tol, Size: Int32);
begin
  ListBox.Items[Index] := Format('%d, %d, %d, %d, %d', [X, Y, Col, Tol, Size]);

  if ListBox.ItemIndex = Index then
  begin
    EditPointX.Text := IntToStr(X);
    EditPointY.Text := IntToStr(Y);
    EditPointColor.Text := IntToStr(Col);
    EditPointTolerance.Text := IntToStr(Tol);
    EditPointSize.Text := IntToStr(Size);
  end;

  DrawDTM();
end;

procedure TSimbaDTMEditorForm.OffsetPoint(Index: Int32; X, Y: Int32);
var
  Point: TMDTMPoint;
begin
  Point := GetPoint(Index); EditPoint(Index, Point.X + X, Point.Y + Y, Point.C, Point.T, Point.ASZ);
  Point := GetPoint(Index); EditPoint(Index, Point.X + X, Point.Y + Y, Point.C, Point.T, Point.ASZ);
end;

function TSimbaDTMEditorForm.GetPoint(Index: Int32): TMDTMPoint;
var
  Items: TStringArray;
begin
  Items := ListBox.Items[Index].Split(', ');

  Result.x := StrToInt(Items[0]);
  Result.y := StrToInt(Items[2]);
  Result.c := StrToInt(Items[4]);
  Result.t := StrToInt(Items[6]);
  Result.asz := StrToInt(Items[8]);
  Result.bp := False;
end;

function TSimbaDTMEditorForm.GetPoints: TMDTMPointArray;
var
  i: Int32;
begin
  SetLength(Result, ListBox.Items.Count);
  for i := 0 to High(Result) do
    Result[i] := GetPoint(i);
end;

function TSimbaDTMEditorForm.GetDTM: TMDTM;
var
  Points: TMDTMPointArray;
  i: Int32;
begin
  Points := GetPoints();

  Result := TMDTM.Create();
  for i := 0 to High(Points) do
    Result.AddPoint(Points[i]);
end;

procedure TSimbaDTMEditorForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (not (csLoading in ComponentState)) then
  begin
    with Canvas.TextExtent(' HSL: 100.00, 100.00, 100.00 ') do
    begin
      PanelColorZoom.Width := CX;
      PanelColorZoom.Height := CY * 3;
    end;

    if PanelColorZoom.Height > PanelZoom.Height then
    begin
      PanelZoom.Width := (PanelColorZoom.Height-5) + (5 - (PanelColorZoom.Height-5) mod 5);
      PanelZoom.Height := (PanelColorZoom.Height-5) + (5 - (PanelColorZoom.Height-5) mod 5);
      PanelZoom.Height := PanelZoom.Height + 2;
      PanelZoom.Width := PanelZoom.Height + 2;
    end;
  end;
end;

procedure TSimbaDTMEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSimbaDTMEditorForm.MouseZoomPaint(Sender: TObject);
begin
  with Sender as TGraphicControl do
    if Boolean(Tag) then
    begin
      Canvas.Pen.Color := FImageBox.Overlay.Canvas.Pen.Color;
      Canvas.Frame(
        (Width div 2)  - Floor(Width / 5 / 2),
        (Height div 2) - Floor(Width / 5 / 2),
        (Width div 2)  + Ceil(Width / 5 / 2),
        (Height div 2) + Ceil(Width / 5 / 2)
      );
    end;
end;

procedure TSimbaDTMEditorForm.LoadDTMClick(Sender: TObject);
var
  Value: String;
  DTM: TMDTM;
  Point: TMDTMPoint;
begin
  Value := '';

  if InputQuery('Open DTM', 'Enter DTM String (DTM will be normalized)', Value) then
  begin
    ButtonDeletePoints.Click();

    if (Pos(#39, Value) > 0) then // in case someone passes more than the actual string value.
    begin
      Value := Copy(Value, Pos(#39, Value) + 1, $FFFFFF);
      Value := Copy(Value, 1, Pos(#39, Value) - 1);
    end;

    DTM := TMDTM.Create();

    try
      DTM.LoadFromString(Value);
      for Point in DTM.Points do
        AddPoint(Point.X, Point.Y, Point.C, Point.T, Point.ASZ);
    except
      ShowMessage('Invalid DTM String: ' + Value);
    end;

    DTM.Free();
  end;
end;

procedure TSimbaDTMEditorForm.PointFlash(Sender: TObject);
var
  Point: TMDTMPoint;
begin
  if ListBox.ItemIndex > -1 then
  begin
    Point := GetPoint(ListBox.ItemIndex);

    if Odd(PointFlashTimer.Tag) then
      FImageBox.Overlay.Canvas.Brush.Color := clYellow
    else
      FImageBox.Overlay.Canvas.Brush.Color := FImageBox.Overlay.Canvas.Pen.Color;

    FImageBox.Overlay.Canvas.FillRect(Point.X - Max(1, Point.ASZ), Point.Y - Max(1, Point.ASZ),
                                      Point.X + Max(1, Point.ASZ), Point.Y + Max(1, Point.ASZ));

    FImageBox.Update();

    PointFlashTimer.Tag := PointFlashTimer.Tag + 1;
  end;
end;

procedure TSimbaDTMEditorForm.FindDTMClick(Sender: TObject);
var
  DTM: TMDTM;
  Matches: Int32;
begin
  ListBox.ClearSelection();

  DTM := GetDTM();

  try
    Matches := FImageBox.Overlay.DebugDTM(DTM);

    FImageBox.Update();
    FImageBox.StatusPanel.Text := Format('%.0n matches found.', [Double(Matches)]);
  finally
    DTM.Free();
  end;
end;

procedure TSimbaDTMEditorForm.ButtonPrintDTMClick(Sender: TObject);
var
  DTM: TMDTM;
begin
  DTM := GetDTM();

  try
    if (@OnPrintDTM <> nil) then
      OnPrintDTM(DTM.ToString());
  finally
    DTM.Free();
  end;
end;

procedure TSimbaDTMEditorForm.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if ListBox.ItemIndex > -1 then
  begin
    PointFlashTimer.Enabled := True;
    PanelSelectedPoint.Enabled := True;

    with GetPoint(ListBox.ItemIndex) do
    begin
      if not FImageBox.IsVisible(X, Y) then
        FImageBox.MoveTo(X, Y);

      EditPointX.Text := IntToStr(X);
      EditPointY.Text := IntToStr(Y);
      EditPointColor.Text := IntToStr(C);
      EditPointTolerance.Text := IntToStr(T);
      EditPointSize.Text := IntToStr(ASZ);
    end;
  end else
    PanelSelectedPoint.Enabled := False;

  DrawDTM();
end;

procedure TSimbaDTMEditorForm.ButtonDebugColorClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
    with GetPoint(ListBox.ItemIndex) do
    begin
      PointFlashTimer.Enabled := False;

      FImageBox.Overlay.DebugColorCTS1(C, T);
    end;
end;

procedure TSimbaDTMEditorForm.ButtonDeletePointClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
  begin
    ListBox.DeleteSelected();
    ListBox.OnSelectionChange(Self, False);
  end;
end;

procedure TSimbaDTMEditorForm.CenterDivider(Sender: TObject);
var
  Divider: TDividerBevel absolute Sender;
begin
  Divider.LeftIndent := (Divider.Width div 2) - (Divider.Canvas.TextWidth(Divider.Caption) div 2) - Divider.CaptionSpacing;
end;

procedure TSimbaDTMEditorForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) then
  begin
    ButtonDeletePoint.Click();

    Key := VK_UNKNOWN;
  end;
end;

procedure TSimbaDTMEditorForm.MenuItemOffsetDTMClick(Sender: TObject);
var
  Values: array[0..1] of String;
  X, Y, I: Int32;
begin
  Values[0] := '0';
  Values[1] := '0';

  if InputQuery('Offset DTM', ['X Offset', 'Y Offset'], Values) then
  begin
    X := StrToIntDef(Values[0], 0);
    Y := StrToIntDef(Values[1], 0);

    for I := 0 to ListBox.Count - 1 do
      OffsetPoint(I, X, Y);
  end;
end;

procedure TSimbaDTMEditorForm.MenuItemLoadImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    InitialDir := Application.Location;

    if Execute() then
    try
      FImageBox.Background.LoadFromFile(FileName);
      FImageBox.BackgroundChanged();
    except
    end;
  finally
    Free();
  end;
end;

procedure TSimbaDTMEditorForm.ButtonClearImageClick(Sender: TObject);
begin
  DrawDTM();
end;

procedure TSimbaDTMEditorForm.ButtonDeletePointsClick(Sender: TObject);
begin
  ListBox.Clear();
  ListBox.OnSelectionChange(Self, False);
end;

procedure TSimbaDTMEditorForm.PointEditChanged(Sender: TObject);
var
  X, Y, Col, Tol, Size: Int32;
  Point: TMDTMPoint;
begin
  if (ListBox.ItemIndex > -1) and (TEdit(Sender).Text <> '') then
  begin
    Point := GetPoint(ListBox.ItemIndex);

    X := StrToIntDef(EditPointX.Text, Point.X);
    Y := StrToIntDef(EditPointY.Text, Point.Y);
    Col := StrToIntDef(EditPointColor.Text, Point.C);
    Tol := StrToIntDef(EditPointTolerance.Text, Point.T);
    Size := StrToIntDef(EditPointSize.Text, Point.ASZ);

    EditPoint(ListBox.ItemIndex, X, Y, Col, Tol, Size);
  end;
end;

procedure TSimbaDTMEditorForm.ClientImageClear(Sender: TObject);
begin
  DrawDTM();
end;

constructor TSimbaDTMEditorForm.Create(TargetWindow: THandle);
begin
  inherited Create(Application.MainForm);

  FDragging := -1;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := PanelMain;
  FImageBox.Align := alClient;
  FImageBox.OnMouseDown := @ClientImageMouseDown;
  FImageBox.OnMouseMove := @ClientImageMouseMove;
  FImageBox.OnMouseUp := @ClientImageMouseUp;
  FImageBox.Overlay.Canvas.Pen.Color := clRed;

  FClient := TClient.Create();
  FClient.IOManager.SetTarget(TargetWindow);
  FClient.MBitmaps.CreateBMP(0, 0);

  ImageZoom.Picture.Bitmap.Width := 5;
  ImageZoom.Picture.Bitmap.Height := 5;

  ButtonUpdateImage.Click();
end;

destructor TSimbaDTMEditorForm.Destroy;
begin
  FClient.MBitmaps[0].Free();
  FClient.Free();

  inherited Destroy();
end;

initialization
  {$I simba.dtmeditor.lrs}

end.

