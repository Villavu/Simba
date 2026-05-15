{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_dtmeditor;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Menus, Graphics,
  simba.base,
  simba.colormath,
  simba.toolform,
  simba.dtm,
  simba.component_imagebox,
  simba.component_imageboxcanvas,
  simba.component_treeview,
  simba.component_edit,
  simba.component_button;

type
  TSimbaDTMEditor = class(TSimbaToolForm)
  protected type
    TDTMPointNode = class(TTreeNode)
    public
      Point: TDTMPoint;

      procedure PointChanged;
    end;
  protected
    FFormatSettingsDot: TFormatSettings;
    FPointTree: TSimbaTreeView;
    FEditX: TSimbaLabeledEdit;
    FEditY: TSimbaLabeledEdit;
    FEditColor: TSimbaLabeledEdit;
    FEditTol: TSimbaLabeledEdit;
    FEditSize: TSimbaLabeledEdit;
    FDragging: TDTMPointNode;

    FDebugDTM: TPointArray;
    FDebugColor: TPointArray;

    function MakeDTM: TDTM;
    function GetSelectedPoint: TDTMPointNode;
    function GetPointAt(X, Y: Integer): TDTMPointNode;
    procedure AddPoint(X, Y: Integer; AColor: TColor);
    procedure RepaintImg(DebugDTM, DebugColor: TPointArray);

    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer); override;
    procedure DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect); override;

    procedure DoFindDTMClick(Sender: TObject);
    procedure DoPrintDTMClick(Sender: TObject);
    procedure DoDebugColorClick(Sender: TObject);
    procedure DoUpdateImageClick(Sender: TObject);
    procedure DoClearImageClick(Sender: TObject);
    procedure DoPointSelectionChange(Sender: TObject);
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
    procedure DoUserChange(Sender: TObject);

    procedure DoClearClick(Sender: TObject);
    procedure DoDeleteSelectedClick(Sender: TObject);
    procedure DoLoadFromString(Sender: TObject);
    procedure DoOffsetDTM(Sender: TObject);
  public
    constructor Create(ImageSupplier: TACAImageSupplier); override;
  end;

implementation

uses
  dialogs,
  simba.vartype_box,
  simba.component_theme;

procedure TSimbaDTMEditor.TDTMPointNode.PointChanged;
begin
  Text := Format('%d, %d, %s, %.1f, %d', [Point.X, Point.Y, ColorToStr(Point.Color), Point.Tolerance, Point.AreaSize]);
end;

function TSimbaDTMEditor.MakeDTM: TDTM;
var
  i: Integer;
begin
  Result := Default(TDTM);
  SetLength(Result.Points, FPointTree.TopLevelCount);
  for i := 0 to FPointTree.TopLevelCount - 1 do
    Result.Points[i] := TDTMPointNode(FPointTree.TopLevelItem[i]).Point;
end;

function TSimbaDTMEditor.GetSelectedPoint: TDTMPointNode;
begin
  Result := TDTMPointNode(FPointTree.Selected);
end;

function TSimbaDTMEditor.GetPointAt(X, Y: Integer): TDTMPointNode;
var
  i: Integer;
begin
  for i := 0 to FPointTree.TopLevelCount - 1 do
    with TDTMPointNode(FPointTree.TopLevelItem[i]) do
      if (X >= Point.X - Max(1, Point.AreaSize)) and (Y >= Point.Y - Max(1, Point.AreaSize)) and
         (X <= Point.X + Max(1, Point.AreaSize)) and (Y <= Point.Y + Max(1, Point.AreaSize)) then
        Exit(TDTMPointNode(FPointTree.TopLevelItem[i]));

  Result := nil;
end;

procedure TSimbaDTMEditor.AddPoint(X, Y: Integer; AColor: TColor);
var
  Node: TDTMPointNode;
begin
  Node := TDTMPointNode(FPointTree.AddNode(''));
  Node.Point.X := X;
  Node.Point.Y := Y;
  Node.Point.Color := AColor;
  Node.PointChanged();

  FPointTree.Selected := Node;

  RepaintImg([], []);
end;

procedure TSimbaDTMEditor.RepaintImg(DebugDTM, DebugColor: TPointArray);
begin
  FDebugDTM   := DebugDTM;
  FDebugColor := DebugColor;

  FImageBox.Repaint();
end;

procedure TSimbaDTMEditor.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoImgMouseDown(Sender, Button, Shift, X, Y);

  if (Button = mbLeft) then
  begin
    FDragging := GetPointAt(X, Y);
    case FImageBox.Cursor of
      crDefault:
        begin
          AddPoint(X, Y, FImageBox.Background.Canvas.Pixels[X, Y]);

          FImageBox.Cursor := crHandPoint;
        end;

      crHandPoint:
        FPointTree.Selected := GetPointAt(X, Y);
    end;
  end;
end;

procedure TSimbaDTMEditor.DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoImgMouseUp(Sender, Button, Shift, X, Y);

  if (Button = mbLeft) and (FDragging <> nil) then
  begin
    FDragging := nil;

    DoPointSelectionChange(nil);
  end;
end;

procedure TSimbaDTMEditor.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoImgMouseMove(Sender, Shift, X, Y);

  if (FDragging <> nil) then
  begin
    FDragging.Point.X := X;
    FDragging.Point.Y := Y;
    FDragging.Point.Color := FImageBox.Background.Canvas.Pixels[X, Y];
    FDragging.PointChanged();

    RepaintImg([], []);
  end;

  if (GetPointAt(X, Y) <> nil) then
    FImageBox.Cursor := crHandPoint
  else
    FImageBox.Cursor := crDefault;
end;

procedure TSimbaDTMEditor.DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaImageBoxCanvas; R: TRect);
var
  MainPoint: TDTMPoint;
  CurPoint: TDTMPoint;
  I: Integer;
begin
  inherited DoImgPaintArea(Sender, ACanvas, R);

  if Length(FDebugDTM) > 0 then
    ACanvas.DrawCrossArray(FDebugDTM, 10, FDrawColor)
  else
  if Length(FDebugColor) > 0 then
    ACanvas.DrawPoints(FDebugColor, FDrawColor)
  else
  if (FPointTree.TopLevelCount > 0) then
  begin
    MainPoint := TDTMPointNode(FPointTree.TopLevelItem[0]).Point;

    for I := 0 to FPointTree.TopLevelCount - 1 do
    begin
      CurPoint := TDTMPointNode(FPointTree.TopLevelItem[I]).Point;
      if (I > 0) then // Connect to main point
        ACanvas.DrawLine(TPoint.Create(MainPoint.X, MainPoint.Y),
                         TPoint.Create(CurPoint.X, CurPoint.Y), clRed);

      ACanvas.DrawBoxFilled(
        TBox.Create(TPoint.Create(CurPoint.X,CurPoint.Y), CurPoint.AreaSize+1, CurPoint.AreaSize+1), clYellow, 0.65
      );
    end;

    with MainPoint do
      ACanvas.DrawBoxFilled(
        TBox.Create(X - (AreaSize+1), Y - (AreaSize+1), X + (AreaSize+1), Y + (AreaSize+1)), clYellow, 0.65
      );

    if (FPointTree.Selected <> nil) then
      with TDTMPointNode(FPointTree.Selected).Point do
        ACanvas.DrawCircle(
          TPoint.Create(X, Y), AreaSize + 6, clAqua
        );
  end;
end;

procedure TSimbaDTMEditor.DoFindDTMClick(Sender: TObject);
begin
  RepaintImg(FImageBox.FindDTM(MakeDTM()), []);
end;

procedure TSimbaDTMEditor.DoPrintDTMClick(Sender: TObject);
begin
  DebugLn('DTM := TDTM.CreateFromString(' + #39 + MakeDTM().ToString() + #39 + ');');
  DebugLn(DEBUG_FOCUS);
end;

procedure TSimbaDTMEditor.DoDebugColorClick(Sender: TObject);
var
  Selected: TDTMPointNode;
  Col: TColorTolerance;
begin
  Selected := GetSelectedPoint();
  if (Selected = nil) then
    Exit;

  Col.Color := Selected.Point.Color;
  Col.Tolerance := Selected.Point.Tolerance;
  Col.ColorSpace := EColorSpace.RGB;
  Col.Multipliers := DefaultMultipliers;

  RepaintImg([], FImageBox.FindColor(Col));
end;

procedure TSimbaDTMEditor.DoUpdateImageClick(Sender: TObject);
begin
  UpdateImage();
end;

procedure TSimbaDTMEditor.DoClearImageClick(Sender: TObject);
begin
  FDebugDTM := [];
  FDebugColor := [];

  FImageBox.Repaint();
end;

procedure TSimbaDTMEditor.DoPointSelectionChange(Sender: TObject);
var
  Node: TDTMPointNode;
begin
  Node := TDTMPointNode(FPointTree.Selected);
  if (Node <> nil) then
  begin
    FEditX.Edit.Caption := IntToStr(Node.Point.X);
    FEditY.Edit.Caption := IntToStr(Node.Point.Y);
    FEditColor.Edit.Caption := ColorToStr(Node.Point.Color);
    FEditTol.Edit.Caption := FloatToStr(Node.Point.Tolerance, FFormatSettingsDot);
    FEditSize.Edit.Caption := IntToStr(Node.Point.AreaSize);
  end;
  RepaintImg([], []);
end;

procedure TSimbaDTMEditor.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
var
  BaseRect, ColorRect: TRect;
  S: TTextStyle;
begin
  if (Node.Level <> 0) then
    Exit;

  BaseRect := Node.DisplayRect(True);
  BaseRect.Right := Node.TreeView.ClientWidth;

  ColorRect := baseRect;
  ColorRect.Top += 5;
  ColorRect.Bottom -= 5;
  ColorRect.Left += 5;
  ColorRect.Right := BaseRect.Left + Round(ColorRect.Height * 1.25);

  ACanvas.FillRect(BaseRect);

  BaseRect.Left := ColorRect.Right + 5;
  BaseRect.Right += ColorRect.Width + 5;

  if Node.Selected then
  begin
    ACanvas.Brush.Color := SimbaComponentTheme.ColorActive;
    ACanvas.FillRect(BaseRect);
  end;

  ACanvas.Brush.Color := TDTMPointNode(Node).Point.Color;
  ACanvas.Pen.Color := SimbaComponentTheme.ColorFont;
  ACanvas.Pen.Width := 1;
  ACanvas.Rectangle(ColorRect);

  S := ACanvas.TextStyle;
  S.Layout := tlCenter;
  S.Clipping := False;

  ACanvas.TextRect(BaseRect, BaseRect.Left, BaseRect.Top, Node.Text, S);
end;

procedure TSimbaDTMEditor.DoUserChange(Sender: TObject);
var
  Node: TDTMPointNode;
begin
  if (Sender is TSimbaEdit) and (TSimbaEdit(Sender).Text <> '') and (TSimbaEdit(Sender).Text <> '$')  then
  begin
    Node := GetSelectedPoint();
    if (Node <> nil) then
    try
      if (Sender = FEditColor.Edit) then
        Node.Point.Color := StrToInt(FEditColor.Edit.Text)
      else if (Sender = FEditTol.Edit) then
        Node.Point.Tolerance := StrToFloat(FEditTol.Edit.Text, FFormatSettingsDot)
      else if (Sender = FEditSize.Edit) then
        Node.Point.AreaSize := StrToInt(FEditSize.Edit.Text)
      else if (Sender = FEditX.Edit) then
        Node.Point.X := StrToInt(FEditX.Edit.Text)
      else if (Sender = FEditY.Edit) then
        Node.Point.Y := StrToInt(FEditY.Edit.Text);

      Node.PointChanged();
      RepaintImg([], []);
    except
    end;
  end;
end;

procedure TSimbaDTMEditor.DoClearClick(Sender: TObject);
begin
  FPointTree.Clear();
  RepaintImg([], []);
end;

procedure TSimbaDTMEditor.DoDeleteSelectedClick(Sender: TObject);
begin
  if (FPointTree.Selected <> nil) then
  begin
    FPointTree.DeleteSelection();
    FPointTree.Selected := nil;
    RepaintImg([], []);
  end;
end;

procedure TSimbaDTMEditor.DoLoadFromString(Sender: TObject);
var
  Value: String;
  DTM: TDTM;
  I: Integer;
  Node: TDTMPointNode;
begin
  Value := '';

  if InputQuery('Load DTM', 'Enter DTM String (DTM will be normalized - Use offset to move)', Value) then
  begin
    FPointTree.Clear();

    if (Pos(#39, Value) > 0) then // in case someone passes more than the actual string value.
    begin
      Value := Copy(Value, Pos(#39, Value) + 1, $FFFFFF);
      Value := Copy(Value, 1, Pos(#39, Value) - 1);
    end;

    DTM := Default(TDTM);
    try
      DTM.FromString(Value);
    except
      ShowMessage('Invalid DTM String: ' + Value);
    end;

    if DTM.Valid() then
    begin
      for I := 0 to DTM.PointCount - 1 do
        with DTM.Points[I] do
        begin
          Node := TDTMPointNode(FPointTree.AddNode(''));
          Node.Point.X := X;
          Node.Point.Y := Y;
          Node.Point.Color := Color;
          Node.Point.Tolerance := Tolerance;
          Node.Point.AreaSize := AreaSize;
          Node.PointChanged();
        end;
    end;
  end;
end;

procedure TSimbaDTMEditor.DoOffsetDTM(Sender: TObject);
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

    for I := 0 to FPointTree.TopLevelCount - 1 do
    begin
      TDTMPointNode(FPointTree.TopLevelItem[I]).Point.X += X;
      TDTMPointNode(FPointTree.TopLevelItem[I]).Point.Y += Y;
      TDTMPointNode(FPointTree.TopLevelItem[I]).PointChanged();
    end;
    RepaintImg([], []);
  end;
end;

constructor TSimbaDTMEditor.Create(ImageSupplier: TACAImageSupplier);

  function CreateListPopupMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(Self);
    Result.Items.Add(NewItem('Delete Selected', scNone, False, True, @DoDeleteSelectedClick, 0, ''));
    Result.Items.Add(NewLine);
    Result.Items.Add(NewItem('Clear', scNone, False, True, @DoClearClick, 0, ''));
  end;

  function CreateDTMPopupMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(Self);
    Result.Items.Add(NewItem('Load', scNone, False, True, @DoLoadFromString, 0, ''));
    Result.Items.Add(NewItem('Offset', scNone, False, True, @DoOffsetDTM, 0, ''));
    Result.Items.Add(NewItem('Print', scNone, False, True, @DoPrintDTMClick, 0, ''));
    Result.Items.Add(NewItem('Find', scNone, False, True, @DoFindDTMClick, 0, ''));
  end;

var
  pnl: TPanel;
begin
  inherited Create(ImageSupplier);

  FFormatSettingsDot := FormatSettings;
  FFormatSettingsDot.DecimalSeparator := '.';

  Caption := 'DTM Editor';

  pnl := TPanel.Create(Self);
  pnl.Parent := FSidePanel;
  pnl.Align := alClient;
  pnl.BevelOuter := bvNone;
  pnl.ChildSizing.VerticalSpacing := 5;

  FPointTree := TSimbaTreeView.Create(Self, TDTMPointNode);
  FPointTree.Parent := pnl;
  FPointTree.Align := alClient;
  FPointTree.FilterVisible := False;
  FPointTree.OnSelectionChange := @DoPointSelectionChange;
  FPointTree.OnPaintNode := @DoPaintNode;
  FPointTree.PopupMenu := CreateListPopupMenu();

  FEditX := TSimbaLabeledEdit.Create(Self);
  FEditX.Parent := pnl;
  FEditX.Align := alBottom;
  FEditX.Caption := 'X';
  FEditX.LabelMeasure := 'Tolerance';
  FEditX.Edit.OnUserChange := @DoUserChange;

  FEditY := TSimbaLabeledEdit.Create(Self);
  FEditY.Parent := pnl;
  FEditY.Align := alBottom;
  FEditY.Caption := 'Y';
  FEditY.LabelMeasure := 'Tolerance';
  FEditY.Edit.OnUserChange := @DoUserChange;

  FEditSize := TSimbaLabeledEdit.Create(Self);
  FEditSize.Parent := pnl;
  FEditSize.Align := alBottom;
  FEditSize.Caption := 'Size';
  FEditSize.LabelMeasure := 'Tolerance';
  FEditSize.Edit.OnUserChange := @DoUserChange;

  FEditColor := TSimbaLabeledEdit.Create(Self);
  FEditColor.Parent := pnl;
  FEditColor.Align := alBottom;
  FEditColor.Caption := 'Color';
  FEditColor.LabelMeasure := 'Tolerance';
  FEditColor.Edit.OnUserChange := @DoUserChange;

  FEditTol := TSimbaLabeledEdit.Create(Self);
  FEditTol.Parent := pnl;
  FEditTol.Align := alBottom;
  FEditTol.Caption := 'Tolerance';
  FEditTol.LabelMeasure := 'Tolerance';
  FEditTol.Edit.OnUserChange := @DoUserChange;

  with TSimbaButton.Create(pnl) do
  begin
    Parent := pnl;
    Caption := 'Find Color';
    Align := alBottom;

    BorderSpacing.Left := Canvas.TextWidth('Tolerance') + 5;
    BorderSpacing.Top := 5;
    BorderSpacing.Right := 5;

    OnClick := @DoDebugColorClick;
  end;

  FMenuBar.AddMenu('DTM', CreateDTMPopupMenu());

  addButton('Find DTM', @DoFindDTMClick);
  addButton('Print DTM', @DoPrintDTMClick);
  addButton('Update Image', @DoUpdateImageClick);
  addButton('Clear Image', @DoClearImageClick);
end;

end.

