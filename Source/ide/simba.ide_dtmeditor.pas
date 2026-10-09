{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_dtmeditor;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, Menus,
  simba.base,
  simba.image,
  simba.toolform,
  simba.dtm,
  simba.component_imagebox,
  simba.canvas,
  simba.component_treeview,
  simba.component_edit;

type
  {$push}
  {$scopedenums on}
  EDTMEditorSearch = (NONE, FIND_DTM, FIND_COLOR);
  {$pop}

  TSimbaDTMEditor = class(TSimbaToolForm)
  protected type
    TDTMPointNode = class(TTreeNode)
    public
      Point: TDTMPoint;

      procedure PointChanged;
    end;
  protected
    FPointTree: TSimbaTreeView;
    FEditX: TSimbaLabeledEdit;
    FEditY: TSimbaLabeledEdit;
    FEditColor: TSimbaLabeledEdit;
    FEditTol: TSimbaLabeledEdit;
    FEditSize: TSimbaLabeledEdit;
    FDragging: TDTMPointNode;

    FSearch: EDTMEditorSearch;
    FDrawColor: TColor;
    FDrawAlpha: Byte;
    FDrawColorMenu: TMenuItem;
    FDrawAlphaMenu: TMenuItem;

    function PointNode(Index: Integer): TDTMPointNode;
    function MakeDTM: TDTM;
    function GetSelectedPoint: TDTMPointNode;
    function GetPointAt(X, Y: Integer): TDTMPointNode;
    procedure AddPoint(const Point: TDTMPoint);
    procedure PointsChanged;
    procedure Search(Value: EDTMEditorSearch);
    procedure SetDrawColor(Value: TColor);
    procedure SetDrawAlpha(Value: Byte);
    procedure DoDrawMenuClick(Sender: TObject);
    procedure DoClearImageClick(Sender: TObject);

    procedure SetImage(Value: TSimbaImage); override;
    procedure DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoImgMouseUp(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer); override;
    procedure DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaCanvas; R: TRect); override;

    procedure DoFindDTMClick(Sender: TObject);
    procedure DoPrintDTMClick(Sender: TObject);
    procedure DoFindColorClick(Sender: TObject);
    procedure DoPointSelectionChange(Sender: TObject);
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
    procedure DoUserChange(Sender: TObject);
    procedure DoTreeDeleteKey(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoClearClick(Sender: TObject);
    procedure DoDeleteSelectedClick(Sender: TObject);
    procedure DoLoadFromString(Sender: TObject);
    procedure DoOffsetDTM(Sender: TObject);
  public
    constructor Create(ImageSupplier: TImageSupplier); override;
  end;

implementation

uses
  ExtCtrls, Dialogs, LCLType,
  simba.colormath,
  simba.vartype_box,
  simba.component_button,
  simba.component_theme;

const
  MARKER_RADIUS = 4; // a point's box reaches this far past its area

procedure TSimbaDTMEditor.TDTMPointNode.PointChanged;
begin
  Text := Format('%d, %d, %s, %.1f, %d', [Point.X, Point.Y, ColorToStr(Point.Color), Point.Tolerance, Point.AreaSize]);
end;

function TSimbaDTMEditor.PointNode(Index: Integer): TDTMPointNode;
begin
  Result := TDTMPointNode(FPointTree.TopLevelItem[Index]);
end;


function TSimbaDTMEditor.MakeDTM: TDTM;
var
  I: Integer;
begin
  Result := Default(TDTM);
  SetLength(Result.Points, FPointTree.TopLevelCount);
  for I := 0 to FPointTree.TopLevelCount - 1 do
    Result.Points[I] := PointNode(I).Point;
end;

function TSimbaDTMEditor.GetSelectedPoint: TDTMPointNode;
begin
  Result := TDTMPointNode(FPointTree.Selected);
end;

function TSimbaDTMEditor.GetPointAt(X, Y: Integer): TDTMPointNode;
var
  I, Size: Integer;
begin
  for I := 0 to FPointTree.TopLevelCount - 1 do
  begin
    Result := PointNode(I);
    Size := Result.Point.AreaSize + MARKER_RADIUS;
    if (Abs(X - Result.Point.X) <= Size) and (Abs(Y - Result.Point.Y) <= Size) then
      Exit;
  end;

  Result := nil;
end;

// selected
procedure TSimbaDTMEditor.AddPoint(const Point: TDTMPoint);
var
  Node: TDTMPointNode;
begin
  Node := TDTMPointNode(FPointTree.AddNode(''));
  Node.Point := Point;
  Node.PointChanged();

  FPointTree.Selected := Node;
end;

// what was found was for the points as they were
procedure TSimbaDTMEditor.PointsChanged;
begin
  Search(EDTMEditorSearch.NONE);
end;

// A search that cannot be done is none: a DTM needs two points, a colour a selected point.
procedure TSimbaDTMEditor.Search(Value: EDTMEditorSearch);
var
  Selected: TDTMPointNode;
  TPA: TPointArray;
begin
  // the layer only when a search drew on it: a point being dragged comes here on every mouse move
  if (FSearch <> EDTMEditorSearch.NONE) then
    TopLayer.Clear();

  FSearch := Value;
  FImageBox.Status := '';
  FImageBox.Invalidate();

  case FSearch of
    EDTMEditorSearch.NONE:
      Exit;

    EDTMEditorSearch.FIND_DTM:
      begin
        if (FPointTree.TopLevelCount < 2) then
        begin
          FSearch := EDTMEditorSearch.NONE;
          FImageBox.Status := 'A DTM needs at least two points';
          Exit;
        end;

        TPA := FImageBox.Background.Finder.FindDTMEx(MakeDTM(), -1, TBox.Create(-1, -1, -1, -1));
      end;

    EDTMEditorSearch.FIND_COLOR:
      begin
        Selected := GetSelectedPoint();
        if (Selected = nil) then
        begin
          FSearch := EDTMEditorSearch.NONE;
          Exit;
        end;

        TPA := FImageBox.Background.Finder.FindColor(TColorTolerance.Create(Selected.Point.Color, Selected.Point.Tolerance, EColorSpace.RGB, DefaultMultipliers), TBox.Create(-1, -1, -1, -1));
      end;
  end;

  TopLayer.Opacity := FDrawAlpha;
  if (FDrawColor = clNone) then
    TopLayer.DrawColor := GetContrastingColor(FImageBox.Background.GetPixels(TPA), [clRed, clLime, clBlue, clYellow, clAqua, clFuchsia])
  else
    TopLayer.DrawColor := FDrawColor;

  if (FSearch = EDTMEditorSearch.FIND_DTM) then
  begin
    TopLayer.DrawAntialiasing := True;
    TopLayer.DrawCrossArray(TPA, 10);
  end else
  begin
    TopLayer.DrawAntialiasing := False; // colour matches stay one pixel each
    TopLayer.DrawTPA(TPA);
  end;

  FImageBox.Status := Format('Found %.0n matches', [Double(Length(TPA))]);
end;

// the item's Tag is the colour or the alpha, by which menu it is in
procedure TSimbaDTMEditor.DoDrawMenuClick(Sender: TObject);
begin
  if (TMenuItem(Sender).Parent = FDrawColorMenu) then
    SetDrawColor(TMenuItem(Sender).Tag)
  else
    SetDrawAlpha(TMenuItem(Sender).Tag);
end;

procedure TSimbaDTMEditor.SetDrawColor(Value: TColor);
var
  I: Integer;
begin
  FDrawColor := Value;
  for I := 0 to FDrawColorMenu.Count - 1 do
    if not FDrawColorMenu.Items[I].IsLine then // its Tag is 0, which is black
      FDrawColorMenu.Items[I].Checked := (FDrawColorMenu.Items[I].Tag = Value);

  if (FTopLayer <> nil) then
    Search(FSearch);
end;

procedure TSimbaDTMEditor.SetDrawAlpha(Value: Byte);
var
  I: Integer;
begin
  FDrawAlpha := Value;
  for I := 0 to FDrawAlphaMenu.Count - 1 do
    FDrawAlphaMenu.Items[I].Checked := (FDrawAlphaMenu.Items[I].Tag = Value);

  if (FTopLayer <> nil) then
    FTopLayer.Opacity := Value;
end;

procedure TSimbaDTMEditor.DoClearImageClick(Sender: TObject);
begin
  Search(EDTMEditorSearch.NONE);
end;

procedure TSimbaDTMEditor.SetImage(Value: TSimbaImage);
begin
  inherited SetImage(Value);

  Search(FSearch);
end;

procedure TSimbaDTMEditor.DoImgMouseDown(Sender: TSimbaImageBox; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Point: TDTMPoint;
begin
  inherited DoImgMouseDown(Sender, Button, Shift, X, Y);

  if (Button <> mbLeft) or (not FImageBox.Background.InImage(X, Y)) then
    Exit;

  FDragging := GetPointAt(X, Y);
  if (FDragging <> nil) then
    FPointTree.Selected := FDragging
  else
  begin
    Point := Default(TDTMPoint);
    Point.X := X;
    Point.Y := Y;
    Point.Color := FImageBox.Background.Pixel[X, Y];

    AddPoint(Point);
    PointsChanged();

    FImageBox.Cursor := crHandPoint;
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

// a dragged point stays on the image, at its edge when the mouse leaves it
procedure TSimbaDTMEditor.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoImgMouseMove(Sender, Shift, X, Y);

  // the button came up where the box could not see it (the mouse was captured away)
  if (FDragging <> nil) and (not (ssLeft in Shift)) then
    FDragging := nil;

  if (FDragging <> nil) then
  begin
    FDragging.Point.X := Max(0, Min(X, FImageBox.Background.Width - 1));
    FDragging.Point.Y := Max(0, Min(Y, FImageBox.Background.Height - 1));
    FDragging.Point.Color := FImageBox.Background.Pixel[FDragging.Point.X, FDragging.Point.Y];
    FDragging.PointChanged();

    PointsChanged();
    FImageBox.Update(); // the drag outpaces a queued paint
  end;

  if (GetPointAt(X, Y) <> nil) then
    FImageBox.Cursor := crHandPoint
  else
    FImageBox.Cursor := crDefault;
end;

// the points and the lines from the main one, unless a search is showing
procedure TSimbaDTMEditor.DoImgPaintArea(Sender: TSimbaImageBox; ACanvas: TSimbaCanvas; R: TRect);
const
  BOX_ALPHA = 166;
  MAIN_BOX_ALPHA = 224;
var
  Main, Cur: TDTMPoint;
  I: Integer;
  SavedState: TSimbaCanvasState;
begin
  inherited DoImgPaintArea(Sender, ACanvas, R);

  if (FPointTree.TopLevelCount = 0) or (FSearch <> EDTMEditorSearch.NONE) then
    Exit;

  SavedState := ACanvas.State;
  ACanvas.DrawAntialiasing := True;

  Main := PointNode(0).Point;

  ACanvas.DrawColor := clRed;
  for I := 1 to FPointTree.TopLevelCount - 1 do
  begin
    Cur := PointNode(I).Point;
    ACanvas.DrawLine(TPoint.Create(Main.X, Main.Y), TPoint.Create(Cur.X, Cur.Y));
  end;

  ACanvas.DrawColor := clYellow;
  ACanvas.DrawFilled := True;
  for I := 0 to FPointTree.TopLevelCount - 1 do
  begin
    Cur := PointNode(I).Point;
    if (I = 0) then
      ACanvas.DrawAlpha := MAIN_BOX_ALPHA
    else
      ACanvas.DrawAlpha := BOX_ALPHA;
    ACanvas.DrawBox(TBox.Create(TPoint.Create(Cur.X, Cur.Y), Cur.AreaSize + MARKER_RADIUS, Cur.AreaSize + MARKER_RADIUS));
  end;
  ACanvas.DrawFilled := False;
  ACanvas.DrawAlpha := ALPHA_OPAQUE;

  if (FPointTree.Selected <> nil) then
  begin
    Cur := GetSelectedPoint().Point;
    ACanvas.DrawColor := clAqua;
    ACanvas.DrawCircle(TPoint.Create(Cur.X, Cur.Y), Cur.AreaSize + MARKER_RADIUS + 5);
  end;

  ACanvas.State := SavedState;
end;

procedure TSimbaDTMEditor.DoFindDTMClick(Sender: TObject);
begin
  Search(EDTMEditorSearch.FIND_DTM);
end;

procedure TSimbaDTMEditor.DoPrintDTMClick(Sender: TObject);
begin
  if (FPointTree.TopLevelCount < 2) then
  begin
    FImageBox.Status := 'A DTM needs at least two points';
    Exit;
  end;

  DebugLn('DTM.FromString(' + #39 + MakeDTM().ToString() + #39 + ');');
  DebugLn(DEBUG_FOCUS);
end;

procedure TSimbaDTMEditor.DoFindColorClick(Sender: TObject);
begin
  Search(EDTMEditorSearch.FIND_COLOR);
end;

procedure TSimbaDTMEditor.DoPointSelectionChange(Sender: TObject);
var
  Node: TDTMPointNode;
begin
  Node := GetSelectedPoint();
  if (Node <> nil) then
  begin
    FEditX.Edit.Text := IntToStr(Node.Point.X);
    FEditY.Edit.Text := IntToStr(Node.Point.Y);
    FEditColor.Edit.Text := ColorToStr(Node.Point.Color);
    FEditTol.Edit.Text := FloatToStr(Node.Point.Tolerance);
    FEditSize.Edit.Text := IntToStr(Node.Point.AreaSize);
  end else
  begin
    FEditX.Edit.Clear();
    FEditY.Edit.Clear();
    FEditColor.Edit.Clear();
    FEditTol.Edit.Clear();
    FEditSize.Edit.Clear();
  end;

  // a colour search is for the selected point
  if (FSearch = EDTMEditorSearch.FIND_COLOR) then
    Search(FSearch);

  FImageBox.Invalidate();
end;

procedure TSimbaDTMEditor.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
begin
  PaintColorNode(ACanvas, Node, TDTMPointNode(Node).Point.Color);
end;

// an edit that does not hold a number yet leaves the point as it is
procedure TSimbaDTMEditor.DoUserChange(Sender: TObject);
var
  Node: TDTMPointNode;
  Value: String;
begin
  Node := GetSelectedPoint();
  if (Node = nil) or (not (Sender is TSimbaEdit)) then
    Exit;
  Value := TSimbaEdit(Sender).Text;
  if (Value = '') or (Value = '$') then
    Exit;

  try
    if (Sender = FEditColor.Edit) then
      Node.Point.Color := StrToInt(Value)
    else if (Sender = FEditTol.Edit) then
      Node.Point.Tolerance := Max(StrToFloat(Value), 0)
    else if (Sender = FEditSize.Edit) then
      Node.Point.AreaSize := Max(StrToInt(Value), 0)
    else if (Sender = FEditX.Edit) then
      Node.Point.X := StrToInt(Value)
    else if (Sender = FEditY.Edit) then
      Node.Point.Y := StrToInt(Value);
  except
    Exit;
  end;

  Node.PointChanged();
  PointsChanged();
end;

procedure TSimbaDTMEditor.DoTreeDeleteKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DoDeleteSelectedClick(nil);
end;

procedure TSimbaDTMEditor.DoClearClick(Sender: TObject);
begin
  FDragging := nil;
  FPointTree.Clear();
  DoPointSelectionChange(nil);
  PointsChanged();
end;

procedure TSimbaDTMEditor.DoDeleteSelectedClick(Sender: TObject);
begin
  if (FPointTree.Selected <> nil) then
  begin
    FDragging := nil;
    FPointTree.DeleteSelection();
    FPointTree.Selected := nil;
    DoPointSelectionChange(nil);
    PointsChanged();
  end;
end;

// the points only change once the string has made a DTM
procedure TSimbaDTMEditor.DoLoadFromString(Sender: TObject);
var
  Value: String;
  DTM: TDTM;
  I: Integer;
  Bounds: TBox;
  Center: TPoint;
begin
  Value := '';
  if not InputQuery('Load DTM', 'Enter DTM String', Value) then
    Exit;

  // in case more than the string itself was pasted
  if (Pos(#39, Value) > 0) then
  begin
    Value := Copy(Value, Pos(#39, Value) + 1, Length(Value));
    Value := Copy(Value, 1, Pos(#39, Value) - 1);
  end;

  DTM := Default(TDTM);
  try
    DTM.FromString(Value);
  except
  end;
  if not DTM.Valid() then
  begin
    ShowMessage('Invalid DTM String: ' + Value);
    Exit;
  end;

  // center the dtm if its not on the image
  Bounds := TBox.Create(DTM.Points[0].X, DTM.Points[0].Y, DTM.Points[0].X, DTM.Points[0].Y);
  for I := 1 to DTM.PointCount - 1 do
    Bounds := Bounds.Combine(TBox.Create(DTM.Points[I].X, DTM.Points[I].Y, DTM.Points[I].X, DTM.Points[I].Y));
  if not (FImageBox.Background.InImage(Bounds.X1, Bounds.Y1) and FImageBox.Background.InImage(Bounds.X2, Bounds.Y2)) then
  begin
    Center := Bounds.Center;
    for I := 0 to DTM.PointCount - 1 do
    begin
      DTM.Points[I].X := DTM.Points[I].X - Center.X + FImageBox.Background.Width div 2;
      DTM.Points[I].Y := DTM.Points[I].Y - Center.Y + FImageBox.Background.Height div 2;
    end;
  end;

  FDragging := nil;
  FPointTree.BeginUpdate();
  try
    FPointTree.Clear();
    for I := 0 to DTM.PointCount - 1 do
      AddPoint(DTM.Points[I]);
  finally
    FPointTree.EndUpdate();
  end;

  DoPointSelectionChange(nil);
  PointsChanged();
end;

procedure TSimbaDTMEditor.DoOffsetDTM(Sender: TObject);
var
  Values: array[0..1] of String;
  X, Y, I: Integer;
begin
  Values[0] := '0';
  Values[1] := '0';
  if not InputQuery('Offset DTM', ['X Offset', 'Y Offset'], Values) then
    Exit;

  X := StrToIntDef(Values[0], 0);
  Y := StrToIntDef(Values[1], 0);
  for I := 0 to FPointTree.TopLevelCount - 1 do
  begin
    PointNode(I).Point.X += X;
    PointNode(I).Point.Y += Y;
    PointNode(I).PointChanged();
  end;

  DoPointSelectionChange(nil);
  PointsChanged();
end;

constructor TSimbaDTMEditor.Create(ImageSupplier: TImageSupplier);
var
  Panel: TPanel;
  Items: TMenuItem;

  procedure AddDrawItem(AParent: TMenuItem; ACaption: String; Value, Current: PtrInt);
  var
    Item: TMenuItem;
  begin
    Item := addMenuItem(AParent, ACaption, @DoDrawMenuClick);
    Item.Checked := (Value = Current);
    Item.Tag := Value;
  end;

  function CreateListPopupMenu: TPopupMenu;
  begin
    Result := TPopupMenu.Create(Self);
    addMenuItem(Result.Items, 'Delete Selected', @DoDeleteSelectedClick);
    addMenuItem(Result.Items, '-');
    addMenuItem(Result.Items, 'Clear', @DoClearClick);
  end;

  // made top to bottom
  function NewEdit(ACaption: String): TSimbaLabeledEdit;
  begin
    Result := TSimbaLabeledEdit.Create(Self);
    Result.Parent := Panel;
    Result.Align := alBottom;
    Result.Caption := ACaption;
    Result.LabelMeasure := 'Tolerance';
    Result.Edit.OnUserChange := @DoUserChange;
  end;

begin
  inherited Create(ImageSupplier);

  Caption := 'DTM Editor';

  Panel := TPanel.Create(Self);
  Panel.Parent := FSidePanel;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;
  Panel.ChildSizing.VerticalSpacing := 5;

  FPointTree := TSimbaTreeView.Create(Self, TDTMPointNode);
  FPointTree.Parent := Panel;
  FPointTree.Align := alClient;
  FPointTree.FilterVisible := False;
  FPointTree.OnSelectionChange := @DoPointSelectionChange;
  FPointTree.OnPaintNode := @DoPaintNode;
  FPointTree.AddKeyEvent(VK_DELETE, [], @DoTreeDeleteKey);
  FPointTree.PopupMenu := CreateListPopupMenu();

  FEditX := NewEdit('X');
  FEditY := NewEdit('Y');
  FEditSize := NewEdit('Size');
  FEditColor := NewEdit('Color');
  FEditTol := NewEdit('Tolerance');

  with TSimbaButton.Create(Panel) do
  begin
    Parent := Panel;
    Caption := 'Find Color';
    Align := alBottom;

    BorderSpacing.Left := Canvas.TextWidth('Tolerance') + 5;
    BorderSpacing.Top := 5;
    BorderSpacing.Right := 5;

    OnClick := @DoFindColorClick;
  end;

  FDrawColor := clNone;
  FDrawAlpha := ALPHA_OPAQUE;

  Items := addImageMenu();
  addMenuItem(Items, '-');

  FDrawColorMenu := addMenuItem(Items, 'Draw Color');
  AddDrawItem(FDrawColorMenu, 'Auto', clNone, FDrawColor);
  addMenuItem(FDrawColorMenu, '-');
  AddDrawItem(FDrawColorMenu, 'Red', clRed, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Green', clGreen, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Blue', clBlue, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Yellow', clYellow, FDrawColor);
  AddDrawItem(FDrawColorMenu, 'Aqua', clAqua, FDrawColor);

  FDrawAlphaMenu := addMenuItem(Items, 'Draw Alpha');
  AddDrawItem(FDrawAlphaMenu, '100%', 255, FDrawAlpha);
  AddDrawItem(FDrawAlphaMenu, '75%', 191, FDrawAlpha);
  AddDrawItem(FDrawAlphaMenu, '50%', 128, FDrawAlpha);
  AddDrawItem(FDrawAlphaMenu, '25%', 64, FDrawAlpha);

  Items := addMenu('DTM');
  addMenuItem(Items, 'Load', @DoLoadFromString);
  addMenuItem(Items, 'Offset', @DoOffsetDTM);
  addMenuItem(Items, 'Print', @DoPrintDTMClick);
  addMenuItem(Items, 'Find', @DoFindDTMClick);

  addButton('Find DTM', @DoFindDTMClick);
  addButton('Print DTM', @DoPrintDTMClick);
  addButton('Update Image', @DoUpdateImageClick);
  addButton('Clear Image', @DoClearImageClick);
end;

end.

