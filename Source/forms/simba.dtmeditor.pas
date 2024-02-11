{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dtmeditor;

{$i simba.inc}

interface

uses
  classes, sysutils, fileutil, dividerbevel, forms, controls,
  graphics, dialogs, extctrls, comctrls, stdctrls, menus, lcltype,
  simba.base, simba.dtm, simba.imagebox, simba.imagebox_canvas, simba.imagebox_zoom;

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
    LabelX: TLabel;
    LabelY: TLabel;
    LabelColor: TLabel;
    LabelTolerance: TLabel;
    LabelSize: TLabel;
    ListBox: TListBox;
    MainMenu: TMainMenu;
    MenuItemImage: TMenuItem;
    PanelTop: TPanel;
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

    procedure ButtonClearImageClick(Sender: TObject);
    procedure ButtonDeletePointsClick(Sender: TObject);
    procedure ButtonDebugColorClick(Sender: TObject);
    procedure ButtonDeletePointClick(Sender: TObject);
    procedure CenterDivider(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItemOffsetDTMClick(Sender: TObject);
    procedure MenuItemLoadImageClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure PanelRightResize(Sender: TObject);
    procedure PointEditChanged(Sender: TObject);
    procedure ButtonPrintDTMClick(Sender: TObject);
    procedure FindDTMClick(Sender: TObject);
    procedure LoadDTMClick(Sender: TObject);
    procedure ChangeDrawColor(Sender: TObject);
    procedure ClientImageClear(Sender: TObject);
    procedure ButtonUpdateImageClick(Sender: TObject);
  protected
    FFreeOnClose: Boolean;
    FImageBox: TSimbaImageBox;
    FImageZoom: TSimbaImageBoxZoom;
    FZoomInfo: TLabel;
    FDragging: Integer;
    FDebugDTM: TPointArray;
    FDebugColor: TPointArray;

    FDrawColor: TColor;
    FWindow: TWindowHandle;
    FDTMString: String;

    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoImgPaint(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);

    procedure AddPoint(X, Y, Col: Integer; Tol: Single; Size: Integer); overload;
    procedure AddPoint(X, Y, Col: Integer); overload;
    procedure EditPoint(Index: Integer; X, Y, Col: Integer; Tol: Single; Size: Integer);
    procedure OffsetPoint(Index: Integer; X, Y: Integer);

    function GetPointAt(X, Y: Integer): Integer;
    function GetPoint(Index: Integer): TDTMPoint;
    function GetPoints: TDTMPointArray;
    function GetDTM: TDTM;

    procedure DrawDTM;
  public
    constructor Create(Window: TWindowHandle); reintroduce;

    property FreeOnClose: Boolean read FFreeOnClose write FFreeOnClose;
    property DTMString: String read FDTMString;
  end;

implementation

{$R *.lfm}

uses
  simba.windowhandle, simba.colormath, simba.dialog;

procedure TSimbaDTMEditorForm.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
var
  Point: TDTMPoint;
begin
  FImageZoom.Move(FImageBox.Background.Canvas, X, Y);
  with FImageBox.Background.Canvas.Pixels[X, Y].ToRGB(), FImageBox.Background.Canvas.Pixels[X, Y].ToHSL() do
    FZoomInfo.Caption := Format('Color: %d', [FImageBox.Background.Canvas.Pixels[X, Y]]) + LineEnding +
                         Format('RGB: %d, %d, %d', [R, G, B])                            + LineEnding +
                         Format('HSL: %.2f, %.2f, %.2f', [H, S, L])                      + LineEnding;

  if (FDragging > -1) then
  begin
    Point := GetPoint(FDragging);

    EditPoint(FDragging, X, Y, FImageBox.Background.Canvas.Pixels[X, Y], Point.Tolerance, Point.AreaSize);
  end;

  if (GetPointAt(X, Y) > -1) then
    FImageBox.Cursor := crHandPoint
  else
    FImageBox.Cursor := crDefault;
end;

procedure TSimbaDTMEditorForm.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TSimbaDTMEditorForm.DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
    FDragging := -1;
end;

procedure TSimbaDTMEditorForm.ChangeDrawColor(Sender: TObject);
var
  i: Integer;
begin
  with Sender as TMenuItem do
  begin
    for i := 0 to Parent.Count - 1 do
      if (Parent[i] <> Sender) then
        Parent[i].Checked := False;

    case Caption of
      'Red':    FDrawColor := clRed;
      'Green':  FDrawColor := clGreen;
      'Blue':   FDrawColor := clBlue;
      'Yellow': FDrawColor := clYellow;
    end;
  end;
end;

procedure TSimbaDTMEditorForm.DrawDTM;
begin
  FDebugColor := [];
  FDebugDTM   := [];

  FImageBox.RePaint();
end;

function TSimbaDTMEditorForm.GetPointAt(X, Y: Integer): Integer;
var
  Points: TDTMPointArray;
  i: Integer;
begin
  Result := -1;

  Points := GetPoints();

  for i := 0 to High(Points) do
  begin
    if (X >= Points[i].X - Max(1, Points[i].AreaSize)) and (Y >= Points[i].Y - Max(1, Points[i].AreaSize)) and
       (X <= Points[i].X + Max(1, Points[i].AreaSize)) and (Y <= Points[i].Y + Max(1, Points[i].AreaSize)) then
      begin
        Result := i;
        Break;
      end;
  end;
end;

procedure TSimbaDTMEditorForm.ButtonUpdateImageClick(Sender: TObject);
begin
  if (not FWindow.IsValid()) then
    FWindow := GetDesktopWindow();
  FImageBox.SetBackgroundFromWindow(FWindow);

  DrawDTM();
end;

procedure TSimbaDTMEditorForm.DoImgPaint(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);
var
  Points: TDTMPointArray;
  I: Integer;
begin
  if Length(FDebugDTM) > 0 then
    ACanvas.DrawCrossArray(FDebugDTM, 10, FDrawColor)
  else
  if Length(FDebugColor) > 0 then
    ACanvas.DrawPoints(FDebugColor, FDrawColor)
  else
  begin
    Points := GetPoints();
    for I := 0 to High(Points) do
    begin
      if (Length(Points) > 1) then // Connect to main point
        ACanvas.DrawLine(TPoint.Create(Points[0].X, Points[0].Y),
                         TPoint.Create(Points[I].X, Points[I].Y), clRed);

      with Points[I] do
        ACanvas.DrawBoxFilled(
          TBox.Create(TPoint.Create(X,Y), AreaSize+1, AreaSize+1), clYellow, 0.65
        );
    end;

    if Length(Points) > 0 then
      with Points[0] do
        ACanvas.DrawBoxFilled(
          TBox.Create(X - AreaSize, Y - AreaSize, X + AreaSize, Y + AreaSize), clYellow, 0.65
        );

    if ListBox.ItemIndex > -1 then
      with GetPoint(ListBox.ItemIndex) do
        ACanvas.DrawCircle(
          TPoint.Create(X,Y), AreaSize + 5, clYellow
        );
  end;
end;

procedure TSimbaDTMEditorForm.AddPoint(X, Y, Col: Integer; Tol: Single; Size: Integer);
begin
  ListBox.ItemIndex := ListBox.Items.Add('%d, %d, %d, %f, %d', [X, Y, Col, Tol, Size]);
  DrawDTM();
end;

procedure TSimbaDTMEditorForm.AddPoint(X, Y, Col: Integer);
begin
  ListBox.ItemIndex := ListBox.Items.Add('%d, %d, %d, 0, 0', [X, Y, Col]);
  DrawDTM();
end;

procedure TSimbaDTMEditorForm.EditPoint(Index: Integer; X, Y, Col: Integer; Tol: Single; Size: Integer);
begin
  ListBox.Items[Index] := Format('%d, %d, %d, %f, %d', [X, Y, Col, Tol, Size]);

  if ListBox.ItemIndex = Index then
  begin
    EditPointX.Text := IntToStr(X);
    EditPointY.Text := IntToStr(Y);
    EditPointColor.Text := IntToStr(Col);
    EditPointTolerance.Text := Format('%f', [Tol]);
    EditPointSize.Text := IntToStr(Size);
  end;

  DrawDTM();
end;

procedure TSimbaDTMEditorForm.OffsetPoint(Index: Integer; X, Y: Integer);
var
  Point: TDTMPoint;
begin
  Point := GetPoint(Index);

  EditPoint(Index, Point.X + X, Point.Y + Y, Point.Color, Point.Tolerance, Point.AreaSize);
end;

function TSimbaDTMEditorForm.GetPoint(Index: Integer): TDTMPoint;
var
  Items: TStringArray;
begin
  Items := ListBox.Items[Index].Split(', ');

  Result.X := StrToInt(Items[0]);
  Result.Y := StrToInt(Items[1]);
  Result.Color := StrToInt(Items[2]);
  Result.Tolerance := StrToFloat(Items[3]);
  Result.AreaSize := StrToInt(Items[4]);
end;

function TSimbaDTMEditorForm.GetPoints: TDTMPointArray;
var
  i: Integer;
begin
  SetLength(Result, ListBox.Items.Count);
  for i := 0 to High(Result) do
    Result[i] := GetPoint(i);
end;

function TSimbaDTMEditorForm.GetDTM: TDTM;
begin
  Result.Points := GetPoints();
end;

procedure TSimbaDTMEditorForm.LoadDTMClick(Sender: TObject);
var
  Value: String;
  DTM: TDTM;
  I: Integer;
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

    try
      DTM.FromString(Value);
      for I := 0 to DTM.PointCount - 1 do
        with DTM.Points[I] do
          AddPoint(X, Y, Color, Tolerance, AreaSize);
    except
      ShowMessage('Invalid DTM String: ' + Value);
    end;
  end;
end;

procedure TSimbaDTMEditorForm.FindDTMClick(Sender: TObject);
begin
  ListBox.ClearSelection();

  FDebugColor := [];
  FDebugDTM   := FImageBox.FindDTM(GetDTM());

  FImageBox.RePaint();
end;

procedure TSimbaDTMEditorForm.ButtonPrintDTMClick(Sender: TObject);
begin
  FDTMString := GetDTM().ToString();

  DebugLn([EDebugLn.FOCUS], 'DTM := TDTM.CreateFromString(' + #39 + FDTMString + #39 + ');');
end;

procedure TSimbaDTMEditorForm.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if (ListBox.ItemIndex > -1) then
  begin
    PanelSelectedPoint.Enabled := True;

    with GetPoint(ListBox.ItemIndex) do
    begin
      if not FImageBox.IsPointVisible(TPoint.Create(X, Y)) then
        FImageBox.MoveTo(TPoint.Create(X, Y));

      EditPointX.Text := IntToStr(X);
      EditPointY.Text := IntToStr(Y);
      EditPointColor.Text := IntToStr(Color);
      EditPointTolerance.Text := Format('%f', [Tolerance]);
      EditPointSize.Text := IntToStr(AreaSize);
    end;
  end else
    PanelSelectedPoint.Enabled := False;

  DrawDTM();
end;

procedure TSimbaDTMEditorForm.PanelRightResize(Sender: TObject);
begin
  PanelRight.Constraints.MinWidth := PanelRight.Width;
end;

procedure TSimbaDTMEditorForm.ButtonDebugColorClick(Sender: TObject);
begin
  if ListBox.ItemIndex > -1 then
    with GetPoint(ListBox.ItemIndex) do
    begin
      FDebugDTM   := [];
      FDebugColor := FImageBox.FindColor(Color, Tolerance, EColorSpace.RGB, DefaultMultipliers);

      FImageBox.RePaint();
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

procedure TSimbaDTMEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FFreeOnClose then
    CloseAction := caFree;
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
  X, Y, I: Integer;
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
      FImageBox.SetBackgroundFromFile(FileName);
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
  if (SimbaQuestionDlg('DTMEditor', 'Clear All Points?') = ESimbaDialogResult.YES) then
  begin
    ListBox.Clear();
    ListBox.OnSelectionChange(Self, False);
  end;
end;

procedure TSimbaDTMEditorForm.PointEditChanged(Sender: TObject);
var
  X, Y, Col, Size: Integer;
  Tol: Single;
  Point: TDTMPoint;
begin
  if (ListBox.ItemIndex > -1) and (TEdit(Sender).Text <> '') then
  begin
    Point := GetPoint(ListBox.ItemIndex);

    X := StrToIntDef(EditPointX.Text, Point.X);
    Y := StrToIntDef(EditPointY.Text, Point.Y);
    Col := StrToIntDef(EditPointColor.Text, Point.Color);
    Tol := StrToFloatDef(EditPointTolerance.Text, Point.Tolerance);
    Size := StrToIntDef(EditPointSize.Text, Point.AreaSize);

    EditPoint(ListBox.ItemIndex, X, Y, Col, Tol, Size);
  end;
end;

procedure TSimbaDTMEditorForm.ClientImageClear(Sender: TObject);
begin
  DrawDTM();
end;

constructor TSimbaDTMEditorForm.Create(Window: TWindowHandle);
begin
  inherited Create(Application.MainForm);

  FWindow := Window;
  if (FWindow = 0) or (not FWindow.IsValid()) then
    FWindow := GetDesktopWindow();

  FFreeOnClose := True;

  FDrawColor := clRed;
  FDragging := -1;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := PanelMain;
  FImageBox.Align := alClient;
  FImageBox.OnImgPaint := @DoImgPaint;
  FImageBox.OnImgMouseDown := @DoImgMouseDown;
  FImageBox.OnImgMouseMove := @DoImgMouseMove;
  FImageBox.OnImgMouseUp := @DoImgMouseUp;
  FImageBox.SetBackgroundFromWindow(FWindow);

  FImageZoom := TSimbaImageBoxZoom.Create(Self);
  FImageZoom.Parent := PanelTop;
  FImageZoom.SetZoom(4, 5);
  FImageZoom.BorderSpacing.Around := 10;

  FZoomInfo := TLabel.Create(Self);
  FZoomInfo.Parent := PanelTop;
  FZoomInfo.BorderSpacing.Right := 10;
  FZoomInfo.AnchorToNeighbour(akLeft, 10, FImageZoom);
end;

end.
