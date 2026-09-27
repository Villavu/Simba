{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_shapebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  simba.base, simba.canvas,
  simba.component_imagebox;

type
  {$push}
  {$scopedenums on}
  EShapeBoxKind = (POINT, BOX, CIRCLE, POLY, PATH); // in the order of their buttons
  {$pop}

  TSimbaShapeBox = class(TSimbaImageBox)
  protected type
    TShape = record
      Kind: EShapeBoxKind;
      Name: String; // its kind's name while unnamed
      Points: TPointArray;

      // no points yet, and named after its kind
      class function Create(AKind: EShapeBoxKind): TShape; static;

      // as saved, and the name of a shape not named yet
      function KindName: String;
      // the status bar's while it is placed
      function PlaceHint: String;
      function ListText: String;
      function Describe: String;
      // Name := [value]; for a script
      function ToCode: String;
      function Copy: TShape;
      function Radius: Integer;
      function ToStr: String;
      // False for a value that is not numbers
      function FromStr(Str: String): Boolean;

      function Bounds: TBox;
      function Handles: TPointArray;
      function HandleAt(P: TPoint): Integer;
      // how far P is from the outline, -1 when P is neither on nor in the shape
      function HitDistance(P: TPoint): Double;
      function GrabCursor(P: TPoint): TCursor;
      procedure Drag(Handle: Integer; From: TPointArray; Start, P: TPoint);
      procedure Move(DX, DY: Integer);
      function PlaceClick(P: TPoint): Boolean;
      function CanFinish: Boolean;
      // takes out a Poly's or Path's point under P while it is placed; False for none
      function DeleteHandleAt(P: TPoint): Boolean;

      procedure Paint(ACanvas: TSimbaCanvas; Highlighted: Boolean);
    end;
    TShapeArray = array of TShape;

    // what undo puts back
    TState = record
      Shapes: TShapeArray;
      Selected: Integer;
    end;
    TStateArray = array of TState;
  protected
    FShapes: TShapeArray;
    FSelectedIndex: Integer;
    FPlacingIndex: Integer; // already in FShapes, -1 when nothing is placed

    FDragIndex: Integer;
    FDragHandle: Integer;
    FDragStart: TPoint;
    FDragFrom: TPointArray;
    FDragSaved: Boolean;

    FUndo: TStateArray;
    FRedo: TStateArray;
    FNudging: Boolean;

    FOnSelectionChange: TNotifyEvent;
    FOnShapesChange: TNotifyEvent;

    function CheckIndex(Index: Integer): Boolean;
    procedure RangeCheck(Index: Integer);
    function AddShape(const Shape: TShape): Integer;
    procedure RemoveShape(Index: Integer);
    // every shape replaced, nothing selected, placed or dragged
    procedure SetShapes(const Shapes: TShapeArray);
    procedure SelectionChanged;
    procedure ShapesChanged;
    procedure RepaintNow;
    procedure SetPlacingIndex(Index: Integer);
    procedure StopPlacing(Keep: Boolean);
    function ShapeAt(P: TPoint): Integer;

    function TakeState: TState;
    procedure PushUndo(const State: TState);
    procedure RestoreState(const State: TState);

    function GetCount: Integer;
    function GetShapeName(Index: Integer): String;
    procedure SetShapeName(Index: Integer; Value: String);
    procedure SetSelectedIndex(Value: Integer);

    procedure ImgKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ImgMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ImgMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ImgMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ImgPaintArea(ACanvas: TSimbaCanvas; R: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;

    // a shape of AKind's KindName, for a host's buttons
    class function KindName(AKind: EShapeBoxKind): String; static;

    // starts placing a shape of AKind, or puts down the one of AKind being placed
    procedure Place(AKind: EShapeBoxKind);

    procedure Undo;
    procedure Redo;
    procedure UndoKeyDown(var Key: UInt16; Shift: TShiftState);

    procedure Print(Index: Integer); overload;
    procedure Print; overload;

    // With HistoryFileName, the undo and redo steps too, for a host that keeps them between sessions:
    // a history only loads onto the shapes it was saved with.
    procedure Save(FileName: String; HistoryFileName: String = '');
    procedure Load(FileName: String; HistoryFileName: String = '');

    // Each an undo step. An index out of range does nothing, as does
    // copying the shape being placed; deleting it stops placing it.
    function Copy(Index: Integer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;

    function HasSelection: Boolean;
    procedure MakeSelectionVisible;

    // Kind "name", or Kind "" while it is named after its kind
    function ListText(Index: Integer): String;
    // loc=, size=, radius= or points=
    function Describe(Index: Integer): String;

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    // a shape added, deleted, changed or renamed
    property OnShapesChange: TNotifyEvent read FOnShapesChange write FOnShapesChange;

    property Count: Integer read GetCount;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;

    // Setting it empty names it after its kind again, and a new name is an undo step.
    // Name is the component's.
    property ShapeName[Index: Integer]: String read GetShapeName write SetShapeName;
  end;

implementation

uses
  LCLType,
  simba.image, simba.geometry, simba.vartype_pointarray, simba.vartype_box, simba.vartype_string, simba.vartype_point,
  simba.json;

const
  CLOSE_DISTANCE = 5;  // image pixels from a point or an edge that still count as on it
  HANDLE_RADIUS  = 3;  // the square on each point
  MARKER_RADIUS  = 6;  // the ring round a point shape
  MARKER_SIZE    = 16; // how far its crosshair reaches
  LINE_WIDTH     = 2;

// a Box's points: the corners of the box between A and B, top left first and clockwise
function BoxCorners(A, B: TPoint): TPointArray;
begin
  Result := TBox.Create(Min(A.X, B.X), Min(A.Y, B.Y), Max(A.X, B.X), Max(A.Y, B.Y)).Corners();
end;

// how far P is from the nearest line between the points, a Poly's last joined back to its first
function LinesDistance(P: TPoint; Points: TPointArray; Closed: Boolean): Double;
var
  I: Integer;
begin
  if (Length(Points) = 1) then
    Exit(P.DistanceTo(Points[0]));

  Result := TSimbaGeometry.DistToLine(P, Points[0], Points[1]);
  for I := 2 to High(Points) do
    Result := Min(Result, TSimbaGeometry.DistToLine(P, Points[I - 1], Points[I]));
  if Closed and (Length(Points) >= 3) then
    Result := Min(Result, TSimbaGeometry.DistToLine(P, Points[High(Points)], Points[0]));
end;

function LineColor(Selected: Boolean): TColor;
begin
  if Selected then
    Result := clRed
  else
    Result := $F070E0; // light violet
end;

procedure DrawHandles(ACanvas: TSimbaCanvas; Points: TPointArray; Selected: Boolean);
var
  P: TPoint;
begin
  if Selected then
    ACanvas.DrawColor := clYellow
  else
    ACanvas.DrawColor := clLime;

  ACanvas.DrawFilled := True;
  for P in Points do
    ACanvas.DrawBox(TBox.Create(P.X - HANDLE_RADIUS, P.Y - HANDLE_RADIUS, P.X + HANDLE_RADIUS, P.Y + HANDLE_RADIUS));
  ACanvas.DrawFilled := False;
end;

procedure DrawMarker(ACanvas: TSimbaCanvas; P: TPoint; Selected: Boolean);
const
  GAP = MARKER_RADIUS + 3;
begin
  ACanvas.DrawColor := LineColor(Selected);
  ACanvas.DrawCircle(P, MARKER_RADIUS);
  ACanvas.DrawLine(TPoint.Create(P.X - MARKER_SIZE, P.Y), TPoint.Create(P.X - GAP, P.Y));
  ACanvas.DrawLine(TPoint.Create(P.X + GAP, P.Y), TPoint.Create(P.X + MARKER_SIZE, P.Y));
  ACanvas.DrawLine(TPoint.Create(P.X, P.Y - MARKER_SIZE), TPoint.Create(P.X, P.Y - GAP));
  ACanvas.DrawLine(TPoint.Create(P.X, P.Y + GAP), TPoint.Create(P.X, P.Y + MARKER_SIZE));
end;

procedure DrawPathLines(ACanvas: TSimbaCanvas; Points: TPointArray; Selected: Boolean);
var
  I: Integer;
  Mid: TPoint;
  DirX, DirY, Len: Double;
begin
  if Selected then
    ACanvas.DrawColor := $0090FF // orange
  else
    ACanvas.DrawColor := $C8C820; // teal

  for I := 0 to High(Points) - 1 do
  begin
    ACanvas.DrawLine(Points[I], Points[I + 1]);

    DirX := Points[I + 1].X - Points[I].X;
    DirY := Points[I + 1].Y - Points[I].Y;
    Len := Sqrt(Sqr(DirX) + Sqr(DirY));
    if (Len >= 28) then
    begin
      DirX := DirX / Len;
      DirY := DirY / Len;
      Mid := TPoint.Create(Round((Points[I].X + Points[I + 1].X) / 2 + DirX * 6), Round((Points[I].Y + Points[I + 1].Y) / 2 + DirY * 6));
      ACanvas.DrawLine(Mid, TPoint.Create(Round(Mid.X - DirX * 12 - DirY * 7), Round(Mid.Y - DirY * 12 + DirX * 7)));
      ACanvas.DrawLine(Mid, TPoint.Create(Round(Mid.X - DirX * 12 + DirY * 7), Round(Mid.Y - DirY * 12 - DirX * 7)));
    end;
  end;
end;

class function TSimbaShapeBox.TShape.Create(AKind: EShapeBoxKind): TShape;
begin
  Result := Default(TShape);
  Result.Kind := AKind;
  Result.Name := Result.KindName();
end;

function TSimbaShapeBox.TShape.KindName: String;
begin
  case Kind of
    EShapeBoxKind.POINT:  Result := 'Point';
    EShapeBoxKind.BOX:    Result := 'Box';
    EShapeBoxKind.CIRCLE: Result := 'Circle';
    EShapeBoxKind.POLY:   Result := 'Poly';
    EShapeBoxKind.PATH:   Result := 'Path';
  end;
end;

function TSimbaShapeBox.TShape.PlaceHint: String;
begin
  case Kind of
    EShapeBoxKind.POINT:  Result := 'Point: click to place it, Esc to cancel';
    EShapeBoxKind.BOX:    Result := 'Box: click one corner then the opposite one, Esc to cancel';
    EShapeBoxKind.CIRCLE: Result := 'Circle: click the centre then a point on the edge, Esc to cancel';
    EShapeBoxKind.POLY:   Result := 'Polygon: click to add points, Enter or the first point to finish, Esc to cancel';
    EShapeBoxKind.PATH:   Result := 'Path: click to add points, Enter to finish, Esc to cancel';
  end;
end;

function TSimbaShapeBox.TShape.ListText: String;
begin
  if (Name = KindName()) then
    Result := KindName() + ' ""'
  else
    Result := KindName() + ' "' + Name + '"';
end;

function TSimbaShapeBox.TShape.Describe: String;
var
  B: TBox;
begin
  Result := '';
  if (Length(Points) = 0) then
    Exit;

  case Kind of
    EShapeBoxKind.POINT:
      Result := Format('loc=%d,%d', [Points[0].X, Points[0].Y]);

    EShapeBoxKind.BOX:
      begin
        B := Bounds();
        Result := Format('size=%dx%d, loc=%d,%d', [B.Width, B.Height, B.X1, B.Y1]);
      end;

    EShapeBoxKind.CIRCLE:
      Result := Format('radius=%d, loc=%d,%d', [Radius, Points[0].X, Points[0].Y]);

    EShapeBoxKind.POLY,
    EShapeBoxKind.PATH:
      Result := Format('points=%d', [Length(Points)]);
  end;
end;

function TSimbaShapeBox.TShape.ToCode: String;
begin
  Result := Name + ' := [' + ToStr() + '];';
end;

function TSimbaShapeBox.TShape.Copy: TShape;
begin
  Result := Self;
  Result.Points := System.Copy(Points);
end;

// a Circle's: from its centre to the point on its edge
function TSimbaShapeBox.TShape.Radius: Integer;
begin
  if (Length(Points) >= 2) then
    Result := Round(Points[1].DistanceTo(Points[0]))
  else
    Result := 0;
end;

function TSimbaShapeBox.TShape.ToStr: String;
var
  B: TBox;
  I: Integer;
begin
  Result := '';
  if (Length(Points) = 0) then
    Exit;

  case Kind of
    EShapeBoxKind.POINT:
      Result := Format('%d,%d', [Points[0].X, Points[0].Y]);

    EShapeBoxKind.BOX:
      begin
        B := Bounds();
        Result := Format('%d,%d,%d,%d', [B.X1, B.Y1, B.X2, B.Y2]);
      end;

    EShapeBoxKind.CIRCLE:
      Result := Format('%d,%d,%d', [Points[0].X, Points[0].Y, Radius]);

    EShapeBoxKind.POLY,
    EShapeBoxKind.PATH:
      for I := 0 to High(Points) do
      begin
        if (I > 0) then
          Result := Result + ', ';
        Result := Result + Format('[%d,%d]', [Points[I].X, Points[I].Y]);
      end;
  end;
end;

function TSimbaShapeBox.TShape.FromStr(Str: String): Boolean;
var
  B: TBox;
  C: TPoint;
  R, I: Integer;
  Elements: TStringArray;
begin
  Result := True;

  try
    case Kind of
      EShapeBoxKind.POINT:
        begin
          SetLength(Points, 1);
          SScanf(Str, '%d,%d', [@Points[0].X, @Points[0].Y]);
        end;

      EShapeBoxKind.BOX:
        begin
          B := Default(TBox);
          SScanf(Str, '%d,%d,%d,%d', [@B.X1, @B.Y1, @B.X2, @B.Y2]);
          Points := BoxCorners(B.TopLeft, B.BottomRight);
        end;

      EShapeBoxKind.CIRCLE:
        begin
          C := Default(TPoint);
          R := 0;
          SScanf(Str, '%d,%d,%d', [@C.X, @C.Y, @R]);
          Points := [C, C.Offset(R, 0)];
        end;

      EShapeBoxKind.POLY,
      EShapeBoxKind.PATH:
        begin
          Elements := Str.BetweenAll('[', ']');
          SetLength(Points, Length(Elements));
          for I := 0 to High(Elements) do
            SScanf(Elements[I], '%d,%d', [@Points[I].X, @Points[I].Y]);
        end;
    end;
  except
    on EConvertError do
      Result := False;
  end;
end;

function TSimbaShapeBox.TShape.Bounds: TBox;
begin
  if (Kind = EShapeBoxKind.CIRCLE) and (Length(Points) > 0) then
    Result := TBox.Create(Points[0], Radius, Radius)
  else
    Result := Points.Bounds();
end;

function TSimbaShapeBox.TShape.Handles: TPointArray;
begin
  if (Kind = EShapeBoxKind.CIRCLE) then
    Result := [Points[0].Offset(0, -Radius), Points[0].Offset(Radius, 0), Points[0].Offset(0, Radius), Points[0].Offset(-Radius, 0)]
  else
    Result := Points;
end;

function TSimbaShapeBox.TShape.HandleAt(P: TPoint): Integer;
var
  H: TPointArray;
  I: Integer;
begin
  Result := -1;

  // on the ring, but not so near the centre that a small circle could not be moved
  if (Kind = EShapeBoxKind.CIRCLE) then
  begin
    if (Abs(P.DistanceTo(Points[0]) - Radius) <= CLOSE_DISTANCE) and (P.DistanceTo(Points[0]) > CLOSE_DISTANCE) then
      Result := 0;
    Exit;
  end;

  H := Handles();
  for I := 0 to High(H) do
    if (P.DistanceTo(H[I]) <= CLOSE_DISTANCE) then
      Exit(I);
end;

function TSimbaShapeBox.TShape.HitDistance(P: TPoint): Double;
var
  B: TBox;
  DX, DY: Integer;
  Hit: Boolean;
begin
  Result := -1;
  if (Length(Points) = 0) then
    Exit;

  Hit := False;
  case Kind of
    EShapeBoxKind.POINT:
      begin
        Result := P.DistanceTo(Points[0]);
        Hit := Result <= MARKER_SIZE; // anywhere on its marker, the crosshair's arms too
      end;

    EShapeBoxKind.BOX:
      begin
        B := Bounds();
        Hit := B.Expand(CLOSE_DISTANCE).Contains(P);
        if B.Contains(P) then
          Result := Min(Min(P.X - B.X1, B.X2 - P.X), Min(P.Y - B.Y1, B.Y2 - P.Y))
        else
        begin
          DX := Max(Max(B.X1 - P.X, P.X - B.X2), 0);
          DY := Max(Max(B.Y1 - P.Y, P.Y - B.Y2), 0);
          Result := Sqrt(Sqr(Double(DX)) + Sqr(Double(DY)));
        end;
      end;

    EShapeBoxKind.CIRCLE:
      begin
        Result := Abs(P.DistanceTo(Points[0]) - Radius);
        Hit := P.DistanceTo(Points[0]) <= Radius + CLOSE_DISTANCE;
      end;

    EShapeBoxKind.POLY:
      begin
        Result := LinesDistance(P, Points, True);
        Hit := (Result <= CLOSE_DISTANCE) or TSimbaGeometry.PointInPolygon(P, Points);
      end;

    EShapeBoxKind.PATH:
      begin
        Result := LinesDistance(P, Points, False);
        Hit := Result <= CLOSE_DISTANCE;
      end;
  end;

  if not Hit then
    Result := -1;
end;

function TSimbaShapeBox.TShape.GrabCursor(P: TPoint): TCursor;
var
  Handle, DX, DY: Integer;
begin
  Handle := HandleAt(P);
  if (Handle = -1) then
    Exit(crSizeAll);

  case Kind of
    EShapeBoxKind.BOX:
      if Odd(Handle) then // the top right or bottom left
        Result := crSizeNESW
      else
        Result := crSizeNWSE;

    EShapeBoxKind.CIRCLE:
      begin
        // pointing the way the ring would go
        DX := P.X - Points[0].X;
        DY := P.Y - Points[0].Y;
        if (Abs(DX) > 2 * Abs(DY)) then
          Result := crSizeWE
        else
        if (Abs(DY) > 2 * Abs(DX)) then
          Result := crSizeNS
        else
        if ((DX > 0) = (DY > 0)) then
          Result := crSizeNWSE
        else
          Result := crSizeNESW;
      end;
    else
      Result := crHandPoint;
  end;
end;

procedure TSimbaShapeBox.TShape.Drag(Handle: Integer; From: TPointArray; Start, P: TPoint);
begin
  if (Handle = -1) then
    Points := From.Offset(P.X - Start.X, P.Y - Start.Y)
  else
    case Kind of
      EShapeBoxKind.BOX:
        Points := BoxCorners(From[(Handle + 2) mod 4], P); // the corner across stays put
      EShapeBoxKind.CIRCLE:
        Points[1] := P;
      else
        Points[Handle] := P;
    end;
end;

procedure TSimbaShapeBox.TShape.Move(DX, DY: Integer);
begin
  Points := Points.Offset(DX, DY);
end;

function TSimbaShapeBox.TShape.PlaceClick(P: TPoint): Boolean;
begin
  // a Poly also finishes on a click back on its first point
  if (Kind = EShapeBoxKind.POLY) and CanFinish() and (P.DistanceTo(Points[0]) <= CLOSE_DISTANCE) then
    Exit(True);

  Points := Points + [P];
  case Kind of
    EShapeBoxKind.POINT:
      Result := True;
    EShapeBoxKind.BOX:
      begin
        Result := (Length(Points) = 2);
        if Result then
          Points := BoxCorners(Points[0], Points[1]);
      end;
    EShapeBoxKind.CIRCLE:
      begin
        // the centre, then a point on the ring
        Result := (Length(Points) = 2);
      end;
    else
      Result := False;
  end;
end;

function TSimbaShapeBox.TShape.CanFinish: Boolean;
begin
  case Kind of
    EShapeBoxKind.POLY:
      Result := Length(Points) >= 3;
    EShapeBoxKind.PATH:
      Result := Length(Points) >= 2;
    else
      Result := False;
  end;
end;

function TSimbaShapeBox.TShape.DeleteHandleAt(P: TPoint): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if not (Kind in [EShapeBoxKind.POLY, EShapeBoxKind.PATH]) then
    Exit;

  Index := HandleAt(P);
  Result := (Index > -1);
  if Result then
    System.Delete(Points, Index, 1);
end;

procedure TSimbaShapeBox.TShape.Paint(ACanvas: TSimbaCanvas; Highlighted: Boolean);
begin
  if (Length(Points) = 0) then
    Exit;

  case Kind of
    EShapeBoxKind.POINT:
      DrawMarker(ACanvas, Points[0], Highlighted);
    EShapeBoxKind.BOX:
      begin
        ACanvas.DrawColor := LineColor(Highlighted);
        ACanvas.DrawBox(Bounds());
      end;
    EShapeBoxKind.CIRCLE:
      begin
        ACanvas.DrawColor := LineColor(Highlighted);
        ACanvas.DrawCircle(Points[0], Radius);
      end;
    EShapeBoxKind.POLY:
      begin
        ACanvas.DrawColor := LineColor(Highlighted);
        if (Length(Points) >= 3) then
          ACanvas.DrawPolygon(Points)
        else
        if (Length(Points) = 2) then
          ACanvas.DrawLine(Points[0], Points[1]);
      end;
    EShapeBoxKind.PATH:
      DrawPathLines(ACanvas, Points, Highlighted);
  end;

  if (Kind <> EShapeBoxKind.POINT) then
    DrawHandles(ACanvas, Handles(), Highlighted);
end;

function ShapesToJSON(const Shapes: TSimbaShapeBox.TShapeArray): TSimbaJSONItem;
var
  I: Integer;
  Item: TSimbaJSONItem;
begin
  Result := NewJSONArray();
  for I := 0 to High(Shapes) do
  begin
    Item := NewJSONObject();
    Item.AddString('shape', Shapes[I].KindName());
    Item.AddString('name', Shapes[I].Name);
    Item.AddString('value', Shapes[I].ToStr());
    Result.Add('', Item);
  end;
end;

function JSONToShapes(Json: TSimbaJSONItem): TSimbaShapeBox.TShapeArray;
var
  I: Integer;
  Item: TSimbaJSONItem;
  Kind: EShapeBoxKind;
  Shape: TSimbaShapeBox.TShape;
  ShapeVal, NameVal, ValueVal: String;
begin
  if (Json = nil) or (Json.Typ <> EJSONItemType.ARR) then
    SimbaException('Not a shapes file');

  Result := [];
  for I := 0 to Json.Count - 1 do
  begin
    Item := Json.ItemsByIndex[I];
    if (Item.Typ <> EJSONItemType.OBJ) then
      Continue;

    if Item.GetString('shape', ShapeVal) and Item.GetString('name', NameVal) and Item.GetString('value', ValueVal) then
      for Kind in EShapeBoxKind do
      begin
        Shape := TSimbaShapeBox.TShape.Create(Kind);
        if (Shape.KindName() <> ShapeVal) then
          Continue;

        if (NameVal <> '') then
          Shape.Name := NameVal;
        if Shape.FromStr(ValueVal) then
          Result := Result + [Shape];
        Break;
      end;
  end;
end;

function TSimbaShapeBox.CheckIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FShapes));
end;

procedure TSimbaShapeBox.RangeCheck(Index: Integer);
begin
  if not CheckIndex(Index) then
    SimbaException('TSimbaShapeBox: Index %d is out of range (Count = %d)', [Index, Length(FShapes)]);
end;

function TSimbaShapeBox.AddShape(const Shape: TShape): Integer;
begin
  Result := Length(FShapes);
  SetLength(FShapes, Result + 1);
  FShapes[Result] := Shape;
  ShapesChanged();

  SelectedIndex := Result;
end;

procedure TSimbaShapeBox.RemoveShape(Index: Integer);
var
  Deselect: Boolean;
begin
  if (Index = FPlacingIndex) then
    SetPlacingIndex(-1)
  else
  if (Index < FPlacingIndex) then
    Dec(FPlacingIndex);

  if (Index = FDragIndex) then
    FDragIndex := -1
  else
  if (Index < FDragIndex) then
    Dec(FDragIndex);

  Deselect := (Index = FSelectedIndex);
  if Deselect then
    FSelectedIndex := -1
  else
  if (Index < FSelectedIndex) then
    Dec(FSelectedIndex);

  System.Delete(FShapes, Index, 1);
  ShapesChanged();
  if Deselect then
    SelectionChanged();
end;

procedure TSimbaShapeBox.SetShapes(const Shapes: TShapeArray);
var
  Deselect: Boolean;
begin
  SetPlacingIndex(-1);
  FDragIndex := -1;
  Deselect := (FSelectedIndex > -1);
  FSelectedIndex := -1;

  FShapes := Shapes;
  ShapesChanged();
  if Deselect then
    SelectionChanged();
end;

procedure TSimbaShapeBox.SelectionChanged;
begin
  FNudging := False;
  Invalidate();

  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TSimbaShapeBox.ShapesChanged;
begin
  Invalidate();

  if Assigned(FOnShapesChange) then
    FOnShapesChange(Self);
end;

procedure TSimbaShapeBox.RepaintNow;
begin
  Invalidate();
  Update();
end;

procedure TSimbaShapeBox.SetPlacingIndex(Index: Integer);
begin
  if (Index > -1) then
    Status := FShapes[Index].PlaceHint()
  else
  if (FPlacingIndex > -1) then
    Status := '';
  FPlacingIndex := Index;
end;

procedure TSimbaShapeBox.StopPlacing(Keep: Boolean);
var
  Placed: Integer;
begin
  Placed := FPlacingIndex;
  if Keep and (Placed > -1) then
    PushUndo(TakeState()); // without the placed shape: as things were before it
  SetPlacingIndex(-1);

  if (Placed > -1) and (not Keep) then
    RemoveShape(Placed);

  ShapesChanged();
end;

function TSimbaShapeBox.ShapeAt(P: TPoint): Integer;
var
  I: Integer;
  Dist, BestDist: Double;
begin
  Result := -1;
  BestDist := 0;

  for I := High(FShapes) downto 0 do
  begin
    Dist := FShapes[I].HitDistance(P);
    if (Dist >= 0) and ((Result = -1) or (Dist < BestDist)) then
    begin
      BestDist := Dist;
      Result := I;
    end;
  end;
end;

procedure TSimbaShapeBox.ImgPaintArea(ACanvas: TSimbaCanvas; R: TRect);

  procedure PaintShape(Index: Integer; Highlighted: Boolean);
  var
    B: TBox;
    Preview: TShape;
  begin
    if (Index = FPlacingIndex) then
    begin
      Preview := FShapes[Index];
      if (MouseInClient and Preview.PlaceClick(MouseXY)) or (Preview.Kind in [EShapeBoxKind.POLY, EShapeBoxKind.PATH]) then
        Preview.Paint(ACanvas, True);
    end else
    begin
      B := FShapes[Index].Bounds().Expand(MARKER_SIZE + LINE_WIDTH);
      if (B.X2 >= R.Left) and (B.X1 < R.Right) and (B.Y2 >= R.Top) and (B.Y1 < R.Bottom) then
        FShapes[Index].Paint(ACanvas, Highlighted);
    end;
  end;

var
  SavedState: TSimbaCanvasState;
  I: Integer;
begin
  inherited ImgPaintArea(ACanvas, R);

  // put back whatever the host had set
  SavedState := ACanvas.State;
  ACanvas.DrawAntialiasing := True;
  ACanvas.DrawAlpha := ALPHA_OPAQUE;
  ACanvas.DrawFilled := False;
  ACanvas.DrawThickness := LINE_WIDTH;

  // the selected shape last, on top of the rest
  for I := 0 to High(FShapes) do
    if (I <> FSelectedIndex) then
      PaintShape(I, False);
  if (FSelectedIndex > -1) then
    PaintShape(FSelectedIndex, True);

  ACanvas.State := SavedState;
end;

function TSimbaShapeBox.TakeState: TState;
var
  I, N: Integer;
begin
  Result.Selected := FSelectedIndex;
  if (FPlacingIndex > -1) then
  begin
    if (Result.Selected = FPlacingIndex) then
      Result.Selected := -1
    else
    if (Result.Selected > FPlacingIndex) then
      Dec(Result.Selected);
  end;

  SetLength(Result.Shapes, Length(FShapes));
  N := 0;
  for I := 0 to High(FShapes) do
    if (I <> FPlacingIndex) then
    begin
      Result.Shapes[N] := FShapes[I].Copy();
      Inc(N);
    end;
  SetLength(Result.Shapes, N);
end;

procedure TSimbaShapeBox.PushUndo(const State: TState);
const
  UNDO_LIMIT = 100;
begin
  FUndo := FUndo + [State];
  if (Length(FUndo) > UNDO_LIMIT) then
    System.Delete(FUndo, 0, 1);
  FRedo := [];
  FNudging := False;
end;

procedure TSimbaShapeBox.RestoreState(const State: TState);
begin
  SetShapes(State.Shapes);
  SelectedIndex := State.Selected;
  FNudging := False;
end;

procedure TSimbaShapeBox.Undo;
var
  State: TState;
begin
  if (FPlacingIndex > -1) then
    StopPlacing(False)
  else
  if (Length(FUndo) > 0) then
  begin
    State := FUndo[High(FUndo)];
    SetLength(FUndo, High(FUndo));

    FRedo := FRedo + [TakeState()];
    RestoreState(State);
  end;
end;

procedure TSimbaShapeBox.Redo;
var
  State: TState;
begin
  if (Length(FRedo) = 0) then
    Exit;
  if (FPlacingIndex > -1) then
    StopPlacing(False);

  State := FRedo[High(FRedo)];
  SetLength(FRedo, High(FRedo));

  FUndo := FUndo + [TakeState()];
  RestoreState(State);
end;

procedure TSimbaShapeBox.UndoKeyDown(var Key: UInt16; Shift: TShiftState);
var
  Ctrl: Boolean;
begin
  Ctrl := (ssCtrl in Shift) or (ssMeta in Shift);
  if Ctrl and (Key = VK_Z) and not (ssShift in Shift) then
    Undo()
  else
  if Ctrl and ((Key = VK_Y) or ((Key = VK_Z) and (ssShift in Shift))) then
    Redo()
  else
    Exit;

  Key := 0;
end;

function TSimbaShapeBox.GetCount: Integer;
begin
  Result := Length(FShapes);
end;

function TSimbaShapeBox.GetShapeName(Index: Integer): String;
begin
  RangeCheck(Index);

  Result := FShapes[Index].Name;
end;

procedure TSimbaShapeBox.SetShapeName(Index: Integer; Value: String);
begin
  RangeCheck(Index);

  if (Value = '') then
    Value := FShapes[Index].KindName();
  if (Value = FShapes[Index].Name) then
    Exit;

  if (Index <> FPlacingIndex) then
    PushUndo(TakeState());
  FShapes[Index].Name := Value;
  ShapesChanged();
end;

procedure TSimbaShapeBox.SetSelectedIndex(Value: Integer);
begin
  if not CheckIndex(Value) then
    Value := -1;

  if (Value <> FSelectedIndex) then
  begin
    FSelectedIndex := Value;
    SelectionChanged();
  end;
end;

procedure TSimbaShapeBox.ImgKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited ImgKeyDown(Key, Shift);

  UndoKeyDown(Key, Shift);
  if (Key = 0) then
    Exit;

  if (FPlacingIndex > -1) then
  begin
    if (Key = VK_ESCAPE) then
      StopPlacing(False)
    else
    if (Key = VK_RETURN) and FShapes[FPlacingIndex].CanFinish() then
      StopPlacing(True)
    else
    if (Key = VK_DELETE) and FShapes[FPlacingIndex].DeleteHandleAt(MouseXY) then
      ShapesChanged();

    Key := 0;
    Exit;
  end;

  if HasSelection() and (FDragIndex = -1) and (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN]) then
  begin
    if not FNudging then
      PushUndo(TakeState());
    FNudging := True;

    case Key of
      VK_LEFT:  FShapes[FSelectedIndex].Move(-1, 0);
      VK_RIGHT: FShapes[FSelectedIndex].Move(1, 0);
      VK_UP:    FShapes[FSelectedIndex].Move(0, -1);
      VK_DOWN:  FShapes[FSelectedIndex].Move(0, 1);
    end;

    Key := 0;
    ShapesChanged();
  end;
end;

procedure TSimbaShapeBox.ImgMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Index: Integer;
begin
  inherited ImgMouseDown(Button, Shift, X, Y);

  if (Button <> mbLeft) then
    Exit;
  P := TPoint.Create(X, Y);
  FNudging := False;

  if (FPlacingIndex > -1) then
  begin
    if FShapes[FPlacingIndex].PlaceClick(P) then
      StopPlacing(True)
    else
      ShapesChanged();
    Exit;
  end;

  Index := ShapeAt(P);
  if (Index > -1) then
  begin
    FDragIndex := Index;
    FDragHandle := FShapes[Index].HandleAt(P);
    FDragStart := P;
    FDragFrom := System.Copy(FShapes[Index].Points);
    FDragSaved := False;
    SelectedIndex := Index;
  end;
end;

procedure TSimbaShapeBox.ImgMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited ImgMouseUp(Button, Shift, X, Y);

  if (Button = mbLeft) then
    FDragIndex := -1;
end;

procedure TSimbaShapeBox.ImgMouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited ImgMouseMove(Shift, X, Y);

  if (FDragIndex > -1) and (not (ssLeft in Shift)) then
    FDragIndex := -1;

  if (FDragIndex > -1) then
  begin
    // a click that does not move only selects: no undo step
    if (not FDragSaved) and (X = FDragStart.X) and (Y = FDragStart.Y) then
      Exit;
    if not FDragSaved then
    begin
      PushUndo(TakeState());
      FDragSaved := True;
    end;
    FShapes[FDragIndex].Drag(FDragHandle, FDragFrom, FDragStart, TPoint.Create(X, Y));
    ShapesChanged();
    RepaintNow();
  end else
  if (FPlacingIndex > -1) then
  begin
    Cursor := crDefault;
    RepaintNow();
  end else
  begin
    Index := ShapeAt(TPoint.Create(X, Y));
    if (Index > -1) then
      Cursor := FShapes[Index].GrabCursor(TPoint.Create(X, Y))
    else
      Cursor := crDefault;
  end;
end;

constructor TSimbaShapeBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Background := TSimbaImage.Create(1500, 1500); // black, until it is given an image
  FSelectedIndex := -1;
  FPlacingIndex := -1;
  FDragIndex := -1;
end;

class function TSimbaShapeBox.KindName(AKind: EShapeBoxKind): String;
var
  Shape: TShape;
begin
  Shape := TShape.Create(AKind);

  Result := Shape.KindName();
end;

procedure TSimbaShapeBox.Place(AKind: EShapeBoxKind);
var
  SameKind: Boolean;
begin
  if (FPlacingIndex > -1) then
  begin
    SameKind := (FShapes[FPlacingIndex].Kind = AKind);
    StopPlacing(FShapes[FPlacingIndex].CanFinish());
    if SameKind then
      Exit;
  end;

  SetPlacingIndex(AddShape(TShape.Create(AKind)));
end;

procedure TSimbaShapeBox.Print(Index: Integer);
begin
  if (not CheckIndex(Index)) or (Index = FPlacingIndex) then
    Exit;

  DebugLn(FShapes[Index].ToCode());
  DebugLn(DEBUG_FOCUS);
end;

procedure TSimbaShapeBox.Print;
var
  Shape: TShape;
begin
  for Shape in TakeState().Shapes do
    DebugLn(Shape.ToCode());
  DebugLn(DEBUG_FOCUS);
end;

procedure TSimbaShapeBox.Save(FileName: String; HistoryFileName: String);

  function StatesToJSON(const States: TStateArray): TSimbaJSONItem;
  var
    I: Integer;
    Step: TSimbaJSONItem;
  begin
    Result := NewJSONArray();
    for I := 0 to High(States) do
    begin
      Step := NewJSONObject();
      Step.AddInt('selected', States[I].Selected);
      Step.AddArray('shapes', ShapesToJSON(States[I].Shapes));
      Result.Add('', Step);
    end;
  end;

var
  Json: TSimbaJSONItem;
begin
  Json := ShapesToJSON(TakeState().Shapes);
  try
    try
      SaveJSON(Json, FileName);
    except
      on E: Exception do
        DebugLn('TSimbaShapeBox.Save: %s', [E.Message]);
    end;
  finally
    Json.Free();
  end;

  if (HistoryFileName = '') then
    Exit;

  Json := NewJSONObject();
  try
    try
      Json.AddArray('shapes', ShapesToJSON(TakeState().Shapes));
      Json.AddArray('undo', StatesToJSON(FUndo));
      Json.AddArray('redo', StatesToJSON(FRedo));

      SaveJSON(Json, HistoryFileName);
    except
      on E: Exception do
        DebugLn('TSimbaShapeBox.Save: %s', [E.Message]);
    end;
  finally
    Json.Free();
  end;
end;

procedure TSimbaShapeBox.Load(FileName: String; HistoryFileName: String);

  function StatesFromJSON(Json: TSimbaJSONItem): TStateArray;
  var
    I: Integer;
    Selected: Int64;
    Shapes: TSimbaJSONItem;
    State: TState;
  begin
    Result := [];
    for I := 0 to Json.Count - 1 do
      if Json.ItemsByIndex[I].GetInt('selected', Selected) and Json.ItemsByIndex[I].GetArray('shapes', Shapes) then
      begin
        State.Selected := Selected;
        State.Shapes := JSONToShapes(Shapes);
        Result := Result + [State];
      end;
  end;

var
  Json, Current, Saved, Steps: TSimbaJSONItem;
  Shapes: TShapeArray;
begin
  Shapes := [];
  if FileExists(FileName) then
  begin
    Json := nil;
    try
      try
        Json := LoadJSON(FileName);
        Shapes := JSONToShapes(Json);
      finally
        Json.Free();
      end;
    except
      on E: Exception do
      begin
        DebugLn('TSimbaShapeBox.Load: %s', [E.Message]);
        Exit;
      end;
    end;
  end;

  FUndo := [];
  FRedo := [];
  SetShapes(Shapes);

  if (HistoryFileName = '') or (not FileExists(HistoryFileName)) then
    Exit;

  Json := nil;
  Current := ShapesToJSON(TakeState().Shapes);
  try
    try
      Json := LoadJSON(HistoryFileName);
      if Json.GetArray('shapes', Saved) and (Saved.Format() = Current.Format()) then
      begin
        if Json.GetArray('undo', Steps) then
          FUndo := StatesFromJSON(Steps);
        if Json.GetArray('redo', Steps) then
          FRedo := StatesFromJSON(Steps);
      end;
    except
      on E: Exception do
      begin
        FUndo := [];
        FRedo := [];
        DebugLn('TSimbaShapeBox.Load: %s', [E.Message]);
      end;
    end;
  finally
    Json.Free();
    Current.Free();
  end;
end;

function TSimbaShapeBox.Copy(Index: Integer): Integer;
begin
  Result := -1;
  if (not CheckIndex(Index)) or (Index = FPlacingIndex) then
    Exit;

  PushUndo(TakeState());
  Result := AddShape(FShapes[Index].Copy());
end;

procedure TSimbaShapeBox.Delete(Index: Integer);
begin
  if not CheckIndex(Index) then
    Exit;

  if (Index = FPlacingIndex) then
    StopPlacing(False)
  else
  begin
    PushUndo(TakeState());
    RemoveShape(Index);
  end;
end;

procedure TSimbaShapeBox.Clear;
begin
  if (FPlacingIndex > -1) then
    StopPlacing(False);
  if (Length(FShapes) = 0) then
    Exit;

  PushUndo(TakeState());
  SetShapes([]);
end;

function TSimbaShapeBox.HasSelection: Boolean;
begin
  Result := (FSelectedIndex > -1);
end;

procedure TSimbaShapeBox.MakeSelectionVisible;
var
  P: TPoint;
begin
  if HasSelection() and (Length(FShapes[FSelectedIndex].Points) > 0) then
  begin
    P := FShapes[FSelectedIndex].Bounds().Center;
    if not IsPointVisible(P) then
      MoveTo(P);
  end;
end;

function TSimbaShapeBox.ListText(Index: Integer): String;
begin
  RangeCheck(Index);

  Result := FShapes[Index].ListText();
end;

function TSimbaShapeBox.Describe(Index: Integer): String;
begin
  RangeCheck(Index);

  Result := FShapes[Index].Describe();
end;

end.
