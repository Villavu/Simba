unit simba.shapebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ComCtrls, StdCtrls, ExtCtrls, Dialogs,
  simba.imagebox, simba.imagebox_bitmap, simba.mufasatypes;

type
  TSimbaShapeBox = class;

  {$PUSH}
  {$SCOPEDENUMS ON}
  EPaintShapeFlag  = (SELECTING, SELECTED);
  EPaintShapeFlags = set of EPaintShapeFlag;
  {$POP}

  TSimbaShapeBoxShapeClass = class of TSimbaShapeBoxShape;
  TSimbaShapeBoxShape = class
  public
    FDragStart: TPoint;
    FName: String;
    FShapeType: String;
    FUserData: Pointer;

    destructor Destroy; override;

    function GetLineColor(const Flags: EPaintShapeFlags): TColor; inline;
    function GetConnectorColor(const Flags: EPaintShapeFlags): TColor; inline;

    procedure SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint); virtual; abstract;
    procedure SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint); virtual; abstract;

    function Center: TPoint; virtual; abstract;
    function DistToEdge(P: TPoint): Integer; virtual; abstract;
    function Contains(P: TPoint; ExpandMod: Integer = 0): Boolean; virtual; abstract;
    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); virtual; abstract;

    function BeginDrag(MousePoint: TPoint): Boolean; virtual; abstract;
    function CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean; virtual; abstract;
    procedure Drag(MousePoint: TPoint); virtual; abstract;
    function NeedPaint(PaintArea: TRect): Boolean; virtual; abstract;
  end;

  TSimbaShapeBoxShape_Point = class(TSimbaShapeBoxShape)
  public
    FPoint: TPoint;

    constructor Create(AName: String = '');

    procedure SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint); override;
    procedure SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint); override;

    function Center: TPoint; override;
    function DistToEdge(P: TPoint): Integer; override;
    function Contains(P: TPoint; ExpandMod: Integer = 0): Boolean; override;

    function BeginDrag(MousePoint: TPoint): Boolean; override;

    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); override;

    function CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean; override;
    procedure Drag(MousePoint: TPoint); override;
    function NeedPaint(PaintArea: TRect): Boolean; override;
  end;

  TSimbaShapeBoxShape_Box = class(TSimbaShapeBoxShape)
  protected
  type
    EDragPart = (NONE, TL, TR, BL, BR, DCENTER);
  public
    FDraggingCorner: EDragPart;
    FRect: TRect;

    constructor Create(AName: String = '');

    procedure SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint); override;
    procedure SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint); override;

    function Center: TPoint; override;
    function DistToEdge(P: TPoint): Integer; override;
    function Contains(P: TPoint; ExpandMod: Integer = 0): Boolean; override;
    function GetDragPart(MousePoint: TPoint): EDragPart;

    function BeginDrag(MousePoint: TPoint): Boolean; override;

    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); override;

    function CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean; override;
    procedure Drag(MousePoint: TPoint); override;
    function NeedPaint(PaintArea: TRect): Boolean; override;
  end;

  TSimbaShapeBoxShape_Poly = class(TSimbaShapeBoxShape)
  protected
    procedure BuildContainsCache;
  public
    FPoly: TPointArray;
    FContainsCache: TPointArray;
    FDraggingPoly: TPointArray;
    FDraggingIndex: Integer;

    procedure SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint); override;
    procedure SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint); override;

    constructor Create(AName: String = '');
    function Center: TPoint; override;
    function GetDragIndex(MousePoint: TPoint): Integer;
    function DistToEdge(P: TPoint): Integer; override;
    function Contains(P: TPoint; ExpandMod: Integer = 0): Boolean; override;

    function BeginDrag(MousePoint: TPoint): Boolean; override;
    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); override;

    function CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean; override;
    procedure Drag(MousePoint: TPoint); override;
    function NeedPaint(PaintArea: TRect): Boolean; override;
  end;

  TSimbaShapeBoxShape_Path = class(TSimbaShapeBoxShape_Poly)
  public
    constructor Create(AName: String = '');
    function DistToEdge(P: TPoint): Integer; override;
    function Contains(P: TPoint; ExpandMod: Integer=0): Boolean; override;
    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); override;
  end;

  PSimbaShapeBox = ^TSimbaShapeBox;
  TSimbaShapeBox = class(TSimbaImageBox)
  protected
    FSelecting: TSimbaShapeBoxShape;
    FDragging: TSimbaShapeBoxShape;

    FLeftPanel: TPanel;
    FList: TListBox;

    FPointButton: TButton;
    FBoxButton: TButton;
    FPolyButton: TButton;
    FPathButton: TButton;

    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DoPaintArea(Bitmap: TSimbaImageBoxBitmap; R: TRect); override;
    procedure DoSelectionChanged(Sender: TObject; User: Boolean);
    procedure DoShapeAddButtonClick(Sender: TObject);
    procedure DoShapeDeleteClick(Sender: TObject);
    procedure DoShapeDeleteAllClick(Sender: TObject);
    procedure DoShapeNameClick(Sender: TObject);

    procedure DeleteShapeIndex(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer);

    function GetShapeUserData(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer): Pointer;
    procedure SetShapeUserData(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer; Data: Pointer);

    function GetShapeCount(ShapeClass: TSimbaShapeBoxShapeClass): Integer;
    function GetShapeFromIndex(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer): TSimbaShapeBoxShape;
    function GetShapeNameIndex(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer): String;
    function GetSelectedShape: TSimbaShapeBoxShape;
    function GetShapeAt(P: TPoint): TSimbaShapeBoxShape;

    function GetPanelVisible: Boolean;
    function GetSelecting: TSimbaShapeBoxShape;

    function GetPoint(Index: Integer): TPoint;
    function GetPointCount: Integer;
    function GetPointName(Index: Integer): String;
    function GetBox(Index: Integer): TBox;
    function GetBoxCount: Integer;
    function GetBoxName(Index: Integer): String;
    function GetPath(Index: Integer): TPointArray;
    function GetPathCount: Integer;
    function GetPathName(Index: Integer): String;
    function GetPoly(Index: Integer): TPointArray;
    function GetPolyCount: Integer;
    function GetPolyName(Index: Integer): String;

    function GetBoxUserData(Index: Integer): Pointer;
    function GetPathUserData(Index: Integer): Pointer;
    function GetPointUserData(Index: Integer): Pointer;
    function GetPolyUserData(Index: Integer): Pointer;

    procedure SetBoxUserData(Index: Integer; Value: Pointer);
    procedure SetPathUserData(Index: Integer; Value: Pointer);
    procedure SetPointUserData(Index: Integer; Value: Pointer);
    procedure SetPolyUserData(Index: Integer; Value: Pointer);

    procedure SetSelecting(AValue: TSimbaShapeBoxShape);
  public
    constructor Create(AOwner: TComponent); override;

    function NewPoint: Integer;
    function NewBox: Integer;
    function NewPoly: Integer;
    function NewPath: Integer;

    procedure DeletePoint(Index: Integer);
    procedure DeleteBox(Index: Integer);
    procedure DeletePoly(Index: Integer);
    procedure DeletePath(Index: Integer);

    procedure AddPoint(Point: TPoint; AName: String = '');
    procedure AddBox(Box: TBox; AName: String = '');
    procedure AddPoly(Poly: TPointArray; AName: String = '');
    procedure AddPath(Path: TPointArray; AName: String = '');

    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);

    property LeftPanel: TPanel read FLeftPanel;
    property Selecting: TSimbaShapeBoxShape read GetSelecting write SetSelecting;

    property PointCount: Integer read GetPointCount;
    property Point[Index: Integer]: TPoint read GetPoint;
    property PointName[Index: Integer]: String read GetPointName;
    property PointUserData[Index: Integer]: Pointer read GetPointUserData write SetPointUserData;

    property PathCount: Integer read GetPathCount;
    property Path[Index: Integer]: TPointArray read GetPath;
    property PathName[Index: Integer]: String read GetPathName;
    property PathUserData[Index: Integer]: Pointer read GetPathUserData write SetPathUserData;

    property PolyCount: Integer read GetPolyCount;
    property Poly[Index: Integer]: TPointArray read GetPoly;
    property PolyName[Index: Integer]: String read GetPolyName;
    property PolyUserData[Index: Integer]: Pointer read GetPolyUserData write SetPolyUserData;

    property BoxCount: Integer read GetBoxCount;
    property Box[Index: Integer]: TBox read GetBox;
    property BoxName[Index: Integer]: String read GetBoxName;
    property BoxUserData[Index: Integer]: Pointer read GetBoxUserData write SetBoxUserData;
  end;

implementation

uses
  LCLType, simba.geometry, simba.tpa, math;

const
  CLOSE_DISTANCE = 8;

destructor TSimbaShapeBoxShape.Destroy;
begin
  if (FUserData <> nil) then
    FreeMemAndNil(FUserData);

  inherited Destroy();
end;

function TSimbaShapeBoxShape.GetLineColor(const Flags: EPaintShapeFlags): TColor;
begin
  if (EPaintShapeFlag.SELECTED in Flags) or (EPaintShapeFlag.SELECTING in Flags) then
    Result := clRed
  else
    Result := clPurple;
end;

function TSimbaShapeBoxShape.GetConnectorColor(const Flags: EPaintShapeFlags): TColor;
begin
  if (EPaintShapeFlag.SELECTED in Flags) or (EPaintShapeFlag.SELECTING in Flags) then
    Result := clYellow
  else
    Result := clLime;
end;

constructor TSimbaShapeBoxShape_Point.Create(AName: String);
begin
  inherited Create();

  FShapeType := 'Point';
  FName := AName;
  if (FName = '') then
    FName := FShapeType;

  FPoint := TPoint.Create(-1, -1);
end;

procedure TSimbaShapeBoxShape_Point.SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint);
begin
  FPoint := MousePoint;

  Sender.Selecting := nil;
end;

procedure TSimbaShapeBoxShape_Point.SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint);
begin

end;

function TSimbaShapeBoxShape_Point.Center: TPoint;
begin
  Result := FPoint;
end;

function TSimbaShapeBoxShape_Point.DistToEdge(P: TPoint): Integer;
begin
  Result := Round(P.DistanceTo(FPoint));
end;

function TSimbaShapeBoxShape_Point.Contains(P: TPoint; ExpandMod: Integer): Boolean;
begin
  Result := DistToEdge(P) <= CLOSE_DISTANCE;
end;

function TSimbaShapeBoxShape_Point.BeginDrag(MousePoint: TPoint): Boolean;
begin
  Result := DistToEdge(MousePoint) <= CLOSE_DISTANCE;
end;

procedure TSimbaShapeBoxShape_Point.Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint);
begin
  if (EPaintShapeFlag.SELECTING in Flags) then
    FPoint := MousePoint;

  ACanvas.DrawCrossHair(FPoint, 15, GetLineColor(Flags));
  ACanvas.DrawBoxFilled(TBox.Create(FPoint.X - 1, FPoint.Y - 1, FPoint.X + 1, FPoint.Y + 1), GetConnectorColor(Flags));
end;

function TSimbaShapeBoxShape_Point.CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean;
begin
  Result := DistToEdge(MousePoint) <= CLOSE_DISTANCE;
  if Result then
    ACursor := crHandPoint;
end;

procedure TSimbaShapeBoxShape_Point.Drag(MousePoint: TPoint);
begin
  FPoint := MousePoint;
end;

function TSimbaShapeBoxShape_Point.NeedPaint(PaintArea: TRect): Boolean;
begin
  Result := PaintArea.Contains(FPoint);
end;

constructor TSimbaShapeBoxShape_Path.Create(AName: String);
begin
  inherited Create();

  FShapeType := 'Path';
  FName := AName;
  if (FName = '') then
    FName := FShapeType;
end;

function TSimbaShapeBoxShape_Path.DistToEdge(P: TPoint): Integer;
var
  i: Integer;
  d: Integer;
begin
  Result := $FFFFFF;
  for i:=1 to High(FPoly) do
  begin
    d := Round(TSimbaGeometry.DistToLine(P, FPoly[I-1], FPoly[I]));
    if (d < Result) then
      Result := d;
  end;
end;

function TSimbaShapeBoxShape_Path.Contains(P: TPoint; ExpandMod: Integer): Boolean;
begin
  Result := DistToEdge(P) <= CLOSE_DISTANCE;
end;

procedure TSimbaShapeBoxShape_Path.Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint);
var
  P: TPoint;
begin
  if EPaintShapeFlag.SELECTING in Flags then
    ACanvas.DrawPoly(FPoly + [MousePoint], False, GetLineColor(Flags))
  else
    ACanvas.DrawPoly(FPoly, False, GetLineColor(Flags));

  for P in FPoly do
    ACanvas.DrawCircleFilled(P, 3, GetConnectorColor(Flags));
end;

procedure TSimbaShapeBoxShape_Poly.BuildContainsCache;
begin
  FContainsCache := TSimbaGeometry.ExpandPolygon(ConvexHull(FPoly), CLOSE_DISTANCE);
end;

procedure TSimbaShapeBoxShape_Poly.SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint);
begin
  if (Length(FPoly) = 0) or (MousePoint.DistanceTo(FPoly[0]) > CLOSE_DISTANCE) then
  begin
    FPoly := FPoly + [MousePoint];

    BuildContainsCache();
  end else
    Sender.Selecting := nil;
end;

procedure TSimbaShapeBoxShape_Poly.SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint);
var
  Index: Integer;
begin
  case Key of
    VK_RETURN:
      begin
        Sender.Selecting := nil;

        Key := 0;
      end;

    VK_DELETE:
      begin
        Index := GetDragIndex(MousePoint);
        if (Index > -1) and (Index < Length(FPoly)) then
          Delete(FPoly, 1, 1);

        Key := 0;
      end;
  end;
end;

constructor TSimbaShapeBoxShape_Poly.Create(AName: String);
begin
  inherited Create();

  FShapeType := 'Poly';
  FName := AName;
  if (FName = '') then
    FName := FShapeType;
end;

function TSimbaShapeBoxShape_Poly.Center: TPoint;
begin
  Result := MiddleTPA(FPoly);
end;

function TSimbaShapeBoxShape_Poly.GetDragIndex(MousePoint: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FPoly) do
    if (MousePoint.DistanceTo(FPoly[I]) <= CLOSE_DISTANCE) then
      Exit(I);

  if Contains(MousePoint) then
    Exit(High(Integer));
end;

function TSimbaShapeBoxShape_Poly.DistToEdge(P: TPoint): Integer;
var
  I,d: Integer;
begin
  Result := $FFFFFF;
  for I := 0 to High(FPoly) do
  begin
    d := Round(P.DistanceTo(FPoly[I]));
    if (d < Result) then
      Result := d;
  end;
end;

function TSimbaShapeBoxShape_Poly.Contains(P: TPoint; ExpandMod: Integer): Boolean;
begin
  if (Length(FContainsCache) = 0) then
    BuildContainsCache();

  Result := TSimbaGeometry.PointInPolygon(P, FContainsCache);
end;

function TSimbaShapeBoxShape_Poly.BeginDrag(MousePoint: TPoint): Boolean;
begin
  FDraggingIndex := GetDragIndex(MousePoint);
  FDraggingPoly := Copy(FPoly);

  Result := FDraggingIndex > -1;
  if Result then
    FDragStart := MousePoint;
end;

procedure TSimbaShapeBoxShape_Poly.Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint);
var
  P: TPoint;
begin
  if EPaintShapeFlag.SELECTING in Flags then
    ACanvas.DrawPoly(FPoly + [MousePoint], True, GetLineColor(Flags))
  else
    ACanvas.DrawPoly(FPoly + [FPoly[0]], True, GetLineColor(Flags));

  for P in FPoly do
    ACanvas.DrawBoxFilled(TBox.Create(P, 3, 3), GetConnectorColor(Flags));
end;

function TSimbaShapeBoxShape_Poly.CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean;
var
  Index: Integer;
begin
  Index := GetDragIndex(MousePoint);

  Result := Index > -1;
  if Result then
  begin
    if Index = High(Integer) then
      ACursor := crSizeAll
    else
      ACursor := crHandPoint;
  end;
end;

procedure TSimbaShapeBoxShape_Poly.Drag(MousePoint: TPoint);
var
  I: Integer;
begin
  FContainsCache := [];

  if FDraggingIndex = High(Integer) then
  begin
    FPoly := Copy(FDraggingPoly);
    for I := 0 to High(FPoly) do
      FPoly[I] += MousePoint-FDragStart;
  end
  else
    FPoly[FDraggingIndex] := MousePoint;
end;

function TSimbaShapeBoxShape_Poly.NeedPaint(PaintArea: TRect): Boolean;
var
  P: TPoint;
begin
  if (Length(FPoly) = 0) then
    Exit(False);

  for P in FPoly do
    if PaintArea.Contains(P) then
      Exit(True);

  Result := False;
end;

constructor TSimbaShapeBoxShape_Box.Create(AName: String);
begin
  inherited Create();

  FShapeType := 'Box';
  FName := AName;
  if (FName = '') then
    FName := FShapeType;

  FRect.Left := -1;
  FRect.Top  := -1;
end;

procedure TSimbaShapeBoxShape_Box.SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint);
begin
  if (FRect.Left = -1) and (FRect.Top = -1) then
    FRect.TopLeft := MousePoint
  else
  begin
    FRect.BottomRight := MousePoint;

    Sender.Selecting := nil;
  end;
end;

procedure TSimbaShapeBoxShape_Box.SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint);
begin

end;

function TSimbaShapeBoxShape_Box.Center: TPoint;
begin
  Result := FRect.CenterPoint;
end;

function TSimbaShapeBoxShape_Box.DistToEdge(P: TPoint): Integer;
var
  Edge: TPoint;
  b: TBox;
begin
  Edge := P;

  b := TBox.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom);
  if Min(Abs(b.Y1 - P.Y), Abs(P.Y - b.Y2)) > Min(Abs(b.X1 - P.X), Abs(P.X - b.X2)) then
  begin
    Edge.X := b.X1;
    if (P.X - b.X1 > b.X2 - P.X) then
      Edge.X := b.X2;
  end else
  begin
    Edge.Y := b.Y1;
    if (P.Y - b.Y1 > b.Y2 - P.Y) then
      Edge.Y := b.Y2;
  end;

   Result := Round(Edge.DistanceTo(P));
end;

function TSimbaShapeBoxShape_Box.Contains(P: TPoint; ExpandMod: Integer): Boolean;
var
  R: TRect;
begin
  FRect.NormalizeRect();

  R := FRect;
  if (ExpandMod > 0) then
    R.Inflate(ExpandMod, ExpandMod);

  Result := R.Contains(P);
end;

function TSimbaShapeBoxShape_Box.GetDragPart(MousePoint: TPoint): EDragPart;
begin
  if TPoint.Create(FRect.Left, FRect.Top).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := TL
  else
  if TPoint.Create(FRect.Right, FRect.Top).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := TR
  else
  if TPoint.Create(FRect.Left, FRect.Bottom).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := BL
  else
  if TPoint.Create(FRect.Right, FRect.Bottom).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := BR
  else
  if FRect.Contains(MousePoint) then
    Result := DCENTER
  else
    Result := NONE;
end;

function TSimbaShapeBoxShape_Box.BeginDrag(MousePoint: TPoint): Boolean;
begin
  FDraggingCorner := GetDragPart(MousePoint);

  Result := FDraggingCorner <> NONE;
  if Result then
    FDragStart := MousePoint - FRect.TopLeft;
end;

procedure TSimbaShapeBoxShape_Box.Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint);
begin
  if (FRect.Left = -1) and (FRect.Top = -1) then
    Exit;

  if (EPaintShapeFlag.SELECTING in Flags) then
    FRect.BottomRight := MousePoint;

  ACanvas.DrawBox(TBox.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom), GetLineColor(Flags));
  with FRect do
  begin
    ACanvas.DrawBoxFilled(TBox.Create(Left-3, Top-3, Left+3, Top+3), GetConnectorColor(Flags));
    ACanvas.DrawBoxFilled(TBox.Create(Right-3, Bottom-3, Right+3, Bottom+3), GetConnectorColor(Flags));

    ACanvas.DrawBoxFilled(TBox.Create(Right-3, Top-3, Right+3, Top+3), GetConnectorColor(Flags));
    ACanvas.DrawBoxFilled(TBox.Create(Left-3, Bottom-3, Left+3, Bottom+3), GetConnectorColor(Flags));
  end;
end;

function TSimbaShapeBoxShape_Box.CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean;
begin
  Result := True;

  case GetDragPart(MousePoint) of
    TL:      ACursor := crSizeNWSE;
    TR:      ACursor := crSizeNESW;
    BL:      ACursor := crSizeNESW;
    BR:      ACursor := crSizeNWSE;
    DCENTER: ACursor := crSizeAll;
    else
      Result := False;
  end;
end;

procedure TSimbaShapeBoxShape_Box.Drag(MousePoint: TPoint);
begin
  case FDraggingCorner of
    DCENTER:
        begin
          FRect.SetLocation(MousePoint - FDragStart);
        end;
    TL: begin
          FRect.Left := MousePoint.X;
          FRect.Top  := MousePoint.Y;
        end;
    TR: begin
          FRect.Right := MousePoint.X;
          FRect.Top   := MousePoint.Y;
        end;
    BL: begin
          FRect.Left   := MousePoint.X;
          FRect.Bottom := MousePoint.Y;
        end;
    BR: begin
          FRect.Right  := MousePoint.X;
          FRect.Bottom := MousePoint.Y;
        end;
  end;
end;

function TSimbaShapeBoxShape_Box.NeedPaint(PaintArea: TRect): Boolean;
begin
  Result := InRange(FRect.Top,    PaintArea.Top,  PaintArea.Bottom) or
            InRange(FRect.Bottom, PaintArea.Top,  PaintArea.Bottom) or
            InRange(FRect.Left,   PaintArea.Left, PaintArea.Right)  or
            InRange(FRect.Right,  PaintArea.Left, PaintArea.Right);
end;

function TSimbaShapeBox.GetPanelVisible: Boolean;
begin
  Result := FLeftPanel.Visible;
end;

function TSimbaShapeBox.GetPoint(Index: Integer): TPoint;
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(TSimbaShapeBoxShape_Point, Index);
  if (Shape = nil) then
    Exit(TPoint.Create(-1, -1));

  with TSimbaShapeBoxShape_Point(Shape) do
    Result := FPoint;
end;

function TSimbaShapeBox.GetPointCount: Integer;
begin
  Result := GetShapeCount(TSimbaShapeBoxShape_Point);
end;

function TSimbaShapeBox.GetPointName(Index: Integer): String;
begin
  Result := GetShapeNameIndex(TSimbaShapeBoxShape_Point, Index);
end;

function TSimbaShapeBox.GetSelecting: TSimbaShapeBoxShape;
begin
  Result := FSelecting;
end;

function TSimbaShapeBox.GetShapeCount(ShapeClass: TSimbaShapeBoxShapeClass): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to FList.Count - 1 do
    if (FList.Items.Objects[I].ClassType = ShapeClass) then
      Inc(Result);
end;

function TSimbaShapeBox.GetShapeFromIndex(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer): TSimbaShapeBoxShape;
var
  I, Curr: Integer;
begin
  Result := nil;

  Curr := 0;
  for I := 0 to FList.Count - 1 do
    if (FList.Items.Objects[I].ClassType = ShapeClass) then
    begin
      if (Curr = Index) then
        Exit(FList.Items.Objects[I] as TSimbaShapeBoxShape);
      Inc(Curr);
    end;
end;

function TSimbaShapeBox.GetShapeNameIndex(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer): String;
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(ShapeClass, Index);
  if (Shape = nil) then
    Result := ''
  else
    Result := Shape.FName;
end;

procedure TSimbaShapeBox.DeleteShapeIndex(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer);
var
  I, Curr: Integer;
begin
  Curr := 0;

  for I := 0 to FList.Count - 1 do
    if (FList.Items.Objects[I].ClassType = ShapeClass) then
    begin
      if (Curr = Index) then
      begin
        FList.Items.Delete(Index);
        Exit;
      end;
      Inc(Curr);
    end;
end;

function TSimbaShapeBox.GetShapeUserData(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer): Pointer;
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(ShapeClass, Index);

  if (Shape = nil) then
    Result := nil
  else
    Result := Shape.FUserData;
end;

procedure TSimbaShapeBox.SetShapeUserData(ShapeClass: TSimbaShapeBoxShapeClass; Index: Integer; Data: Pointer);
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(ShapeClass, Index);

  if (Shape <> nil) then
  begin
    if (Shape.FUserData <> nil) then
      FreeMem(Shape.FUserData);

    Shape.FUserData := Data;
  end;
end;

function TSimbaShapeBox.GetBox(Index: Integer): TBox;
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(TSimbaShapeBoxShape_Box, Index);
  if (Shape = nil) then
    Exit(TBox.Default());

  with TSimbaShapeBoxShape_Box(Shape) do
  begin
    FRect.NormalizeRect();

    Result := TBox.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom);
  end;
end;

function TSimbaShapeBox.GetBoxCount: Integer;
begin
  Result := GetShapeCount(TSimbaShapeBoxShape_Box);
end;

function TSimbaShapeBox.GetBoxName(Index: Integer): String;
begin
  Result := GetShapeNameIndex(TSimbaShapeBoxShape_Box, Index);
end;

function TSimbaShapeBox.GetPath(Index: Integer): TPointArray;
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(TSimbaShapeBoxShape_Path, Index);
  if (Shape = nil) then
    Exit(Default(TPointArray));

  with TSimbaShapeBoxShape_Path(Shape) do
    Result := FPoly;
end;

function TSimbaShapeBox.GetPathCount: Integer;
begin
  Result := GetShapeCount(TSimbaShapeBoxShape_Path);
end;

function TSimbaShapeBox.GetPathName(Index: Integer): String;
begin
  Result := GetShapeNameIndex(TSimbaShapeBoxShape_Path, Index);
end;

function TSimbaShapeBox.GetPoly(Index: Integer): TPointArray;
var
  Shape: TSimbaShapeBoxShape;
begin
  Shape := GetShapeFromIndex(TSimbaShapeBoxShape_Poly, Index);
  if (Shape = nil) then
    Exit(Default(TPointArray));

  with TSimbaShapeBoxShape_Poly(Shape) do
    Result := FPoly;
end;

function TSimbaShapeBox.GetPolyCount: Integer;
begin
  Result := GetShapeCount(TSimbaShapeBoxShape_Poly);
end;

function TSimbaShapeBox.GetPolyName(Index: Integer): String;
begin
  Result := GetShapeNameIndex(TSimbaShapeBoxShape_Poly, Index);
end;

procedure TSimbaShapeBox.SetSelecting(AValue: TSimbaShapeBoxShape);
begin
  FSelecting := AValue;

  StatusPanel.Text := '';
  Paint();
end;

procedure TSimbaShapeBox.DoSelectionChanged(Sender: TObject; User: Boolean);
var
  Shape: TSimbaShapeBoxShape;
  P: TPoint;
begin
  if User then
  begin
    Shape := GetSelectedShape();

    if (Shape <> nil) then
    begin
      P := Shape.Center;
      if not IsVisible(P.X, P.Y) then
        MoveTo(P.X, P.Y);
    end;
  end;

  Paint();
end;

procedure TSimbaShapeBox.DoShapeAddButtonClick(Sender: TObject);
begin
  case TButton(Sender).Caption of
    'New Point':
       begin
         Selecting := TSimbaShapeBoxShape_Point.Create();
         StatusPanel.Text := 'Selecting point: Click to set';
       end;

    'New Box':
       begin
         Selecting := TSimbaShapeBoxShape_Box.Create();
         StatusPanel.Text := 'Selecting box: Click to set top left then again for bottom left';
       end;

    'New Poly':
       begin
        Selecting := TSimbaShapeBoxShape_Poly.Create();
        StatusPanel.Text := 'Selecting polygon: Click to set points and press ENTER to finish';
       end;

    'New Path':
       begin
         Selecting := TSimbaShapeBoxShape_Path.Create();
         StatusPanel.Text := 'Selecting path: Click to set points and press ENTER to finish';
       end;
  end;

  FList.ItemIndex := FList.Items.AddObject(Selecting.FName, Selecting);
end;

procedure TSimbaShapeBox.DoShapeDeleteClick(Sender: TObject);
begin
  if (FList.ItemIndex > -1) then
    FList.Items.Delete(FList.ItemIndex);

  Paint();
end;

procedure TSimbaShapeBox.DoShapeDeleteAllClick(Sender: TObject);
begin
  FList.Clear();

  Paint();
end;

procedure TSimbaShapeBox.DoShapeNameClick(Sender: TObject);
var
  Shape: TSimbaShapeBoxShape;
  Value: String;
begin
  Shape := GetSelectedShape();
  if (Shape = nil) then
    Exit;

  if InputQuery('Shape name', 'Enter shape name', Value) then
  begin
    Shape.FName := Value;
    FList.Items[FList.ItemIndex] := Shape.FShapeType + ' - ' + Shape.FName;
  end;
end;

function TSimbaShapeBox.GetSelectedShape: TSimbaShapeBoxShape;
begin
  if (FList.ItemIndex > -1) and (FList.ItemIndex < FList.Count) then
    Result := FList.Items.Objects[FList.ItemIndex] as TSimbaShapeBoxShape
  else
    Result := nil;
end;

function TSimbaShapeBox.GetShapeAt(P: TPoint): TSimbaShapeBoxShape;
var
  I: Integer;
  Best: Integer;
  Shape: TSimbaShapeBoxShape;
begin
  Result := nil;
  Best := $FFFFFF;
  for I := 0 to FList.Items.Count - 1 do
  begin
    Shape := TSimbaShapeBoxShape(FList.Items.Objects[I]);
    if Shape.Contains(P, CLOSE_DISTANCE) and (Shape.DistToEdge(P) < Best) then
    begin
      Best := Shape.DistToEdge(P);
      Result := Shape;
    end;
  end;
end;

function TSimbaShapeBox.GetBoxUserData(Index: Integer): Pointer;
begin
  Result := GetShapeUserData(TSimbaShapeBoxShape_Box, Index);
end;

function TSimbaShapeBox.GetPathUserData(Index: Integer): Pointer;
begin
  Result := GetShapeUserData(TSimbaShapeBoxShape_Path, Index);
end;

function TSimbaShapeBox.GetPointUserData(Index: Integer): Pointer;
begin
  Result := GetShapeUserData(TSimbaShapeBoxShape_Point, Index);
end;

function TSimbaShapeBox.GetPolyUserData(Index: Integer): Pointer;
begin
  Result := GetShapeUserData(TSimbaShapeBoxShape_Poly, Index);
end;

procedure TSimbaShapeBox.SetBoxUserData(Index: Integer; Value: Pointer);
begin
  SetShapeUserData(TSimbaShapeBoxShape_Box, Index, Value);
end;

procedure TSimbaShapeBox.SetPathUserData(Index: Integer; Value: Pointer);
begin
  SetShapeUserData(TSimbaShapeBoxShape_Path, Index, Value);
end;

procedure TSimbaShapeBox.SetPointUserData(Index: Integer; Value: Pointer);
begin
  SetShapeUserData(TSimbaShapeBoxShape_Point, Index, Value);
end;

procedure TSimbaShapeBox.SetPolyUserData(Index: Integer; Value: Pointer);
begin
  SetShapeUserData(TSimbaShapeBoxShape_Poly, Index, Value);
end;

procedure TSimbaShapeBox.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited ImageMouseUp(Sender, Button, Shift, X, Y);

  if (FDragging <> nil) then
    FDragging := nil;
end;

procedure TSimbaShapeBox.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Shape: TSimbaShapeBoxShape;
begin
  inherited ImageMouseDown(Sender, Button, Shift, X, Y);

  if (Button <> mbLeft) then
    Exit;

  if (Selecting = nil) then
  begin
    Shape := Self.GetShapeAt(FMousePoint);
    if (Shape <> nil) then
      FList.ItemIndex := FList.Items.IndexOfObject(Shape);

    if (Shape <> nil) and Shape.BeginDrag(FMousePoint) then
    begin
      FDragging := Shape;
      Exit;
    end;
  end;

  if (Selecting <> nil) then
    Selecting.SelectingMouseDown(Self, Button, Shift, FMousePoint);
end;

procedure TSimbaShapeBox.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Shape: TSimbaShapeBoxShape;
  NewCursor: TCursor;
begin
  inherited ImageMouseMove(Sender, Shift, X, Y);

  if (Selecting = nil) then
  begin
    if (FDragging = nil) then
    begin
      Shape := Self.GetShapeAt(FMousePoint);
      if (Shape <> nil) and Shape.CanDrag(FMousePoint, NewCursor) then
      begin
        Cursor := NewCursor;
        Exit;
      end;
    end else
    begin
      FDragging.Drag(FMousePoint);
      Paint();
      Exit;
    end;
  end else
    Paint();

  Cursor := crDefault;
end;

procedure TSimbaShapeBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Shape: TSimbaShapeBoxShape;
begin
  inherited KeyDown(Key, Shift);

  Shape := GetSelectedShape();
  if (Shape <> nil) then
  begin
    Shape.SelectingKeyDown(Self, Key, Shift, FMousePoint);
    if (Key = 0) then // Something happened
      Paint();
  end;
end;

function TSimbaShapeBox.NewPoint: Integer;
begin
  FPointButton.Click();

  Result := FList.Items.Count - 1;
end;

procedure TSimbaShapeBox.DoPaintArea(Bitmap: TSimbaImageBoxBitmap; R: TRect);
var
  I: Integer;
  Shape: TSimbaShapeBoxShape;
  Flags: EPaintShapeFlags;
begin
  for I := 0 to FList.Items.Count - 1 do
  begin
    Shape := TSimbaShapeBoxShape(FList.Items.Objects[I]);
    Flags := [];

    if Shape.NeedPaint(R) then
    begin
      if Shape = Selecting then
        Flags := [EPaintShapeFlag.SELECTING];
      if FList.ItemIndex > -1 then
        if FList.Items.Objects[FList.ItemIndex] = Shape then
          Flags := Flags + [EPaintShapeFlag.SELECTED];

      Shape.Paint(Self, Bitmap, Flags, FMousePoint);
    end;
  end;

  inherited;
end;

function TSimbaShapeBox.NewBox: Integer;
begin
  FBoxButton.Click();

  Result := FList.Items.Count - 1;
end;

function TSimbaShapeBox.NewPoly: Integer;
begin
  FPolyButton.Click();

  Result := FList.Items.Count - 1;
end;

function TSimbaShapeBox.NewPath: Integer;
begin
  FPathButton.Click();

  Result := FList.Items.Count - 1;
end;

procedure TSimbaShapeBox.DeletePoint(Index: Integer);
begin
  DeleteShapeIndex(TSimbaShapeBoxShape_Point, Index);
end;

procedure TSimbaShapeBox.DeleteBox(Index: Integer);
begin
  DeleteShapeIndex(TSimbaShapeBoxShape_Box, Index);
end;

procedure TSimbaShapeBox.DeletePoly(Index: Integer);
begin
  DeleteShapeIndex(TSimbaShapeBoxShape_Poly, Index);
end;

procedure TSimbaShapeBox.DeletePath(Index: Integer);
begin
  DeleteShapeIndex(TSimbaShapeBoxShape_Path, Index);
end;

procedure TSimbaShapeBox.AddPoint(Point: TPoint; AName: String);
var
  Shape: TSimbaShapeBoxShape_Point;
begin
  Shape := TSimbaShapeBoxShape_Point.Create(AName);
  Shape.FPoint := Point;

  FList.Items.AddObject(Shape.FName, Shape);
end;

procedure TSimbaShapeBox.AddBox(Box: TBox; AName: String);
var
  Shape: TSimbaShapeBoxShape_Box;
begin
  Shape := TSimbaShapeBoxShape_Box.Create(AName);
  Shape.FRect := TRect.Create(Box.X1, Box.Y1, Box.X2, Box.Y2);

  FList.Items.AddObject(Shape.FName, Shape);
end;

procedure TSimbaShapeBox.AddPoly(Poly: TPointArray; AName: String);
var
  Shape: TSimbaShapeBoxShape_Poly;
begin
  Shape := TSimbaShapeBoxShape_Poly.Create(AName);
  Shape.FPoly := Poly;

  FList.Items.AddObject(Shape.FName, Shape);
end;

procedure TSimbaShapeBox.AddPath(Path: TPointArray; AName: String);
var
  Shape: TSimbaShapeBoxShape_Path;
begin
  Shape := TSimbaShapeBoxShape_Path.Create(AName);
  Shape.FPoly := Path;

  FList.Items.AddObject(Shape.FName, Shape);
end;

procedure TSimbaShapeBox.SaveToFile(FileName: String);

  function PathToStr(const Path: TPointArray): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to High(Path) do
      Result := Result + '[' + IntToStr(Path[I].X) + ',' + IntToStr(Path[I].Y) + '], ';
    Result := Result.Trim([' ', ',']);
  end;

  function BoxToStr(const Rect: TRect): String;
  begin
    Result := IntToStr(Rect.Left) + ',' + IntToStr(Rect.Top) + ',' + IntToStr(Rect.Right) + ',' + IntToStr(Rect.Bottom);
  end;

  function PointToStr(const P: TPoint): String;
  begin
    Result := IntToStr(P.X) + ',' + IntToStr(P.Y);
  end;

var
  I: Integer;
  ShapeName: String;
  Shape: TSimbaShapeBoxShape;
begin
  if (FList.Count > 0) then
  try
    with TStringList.Create() do
    try
      for I := 0 to FList.Items.Count - 1 do
      begin
        Shape := TSimbaShapeBoxShape(FList.Items.Objects[I]);
        ShapeName := Shape.FName;
        if (ShapeName = 'Point') or (ShapeName = 'Box') or (ShapeName = 'Poly') or (ShapeName = 'Path') then
          ShapeName := '';
        if (ShapeName <> '') then
          ShapeName := '_' + ShapeName;

        if (Shape.ClassType = TSimbaShapeBoxShape_Point) then
          Add('Point' + ShapeName + '=' + PointToStr(TSimbaShapeBoxShape_Point(Shape).FPoint))
        else
        if (Shape.ClassType = TSimbaShapeBoxShape_Box) then
          Add('Box'  + ShapeName + '=' + BoxToStr(TSimbaShapeBoxShape_Box(Shape).FRect))
        else
        if (Shape.ClassType = TSimbaShapeBoxShape_Poly) then
          Add('Poly' + ShapeName + '=' + PathToStr(TSimbaShapeBoxShape_Poly(Shape).FPoly))
        else
        if (Shape.ClassType = TSimbaShapeBoxShape_Path) then
          Add('Path' + ShapeName + '=' + PathToStr(TSimbaShapeBoxShape_Poly(Shape).FPoly));
      end;

      SaveToFile(FileName);
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaShapeBox.LoadFromFile(FileName: String);

  function StrToPoint(const S: String): TPoint;
  begin
    SScanf(S, '%d,%d', [@Result.X, @Result.Y]);
  end;

  function StrToRect(const S: String): TRect;
  begin
    SScanf(S, '%d,%d,%d,%d', [@Result.Left, @Result.Top, @Result.Right, @Result.Bottom]);
  end;

  function StrToTPA(const S: String): TPointArray;
  var
    Elements, Element: TStringArray;
    I: Integer;
  begin
    Elements := S.BetweenAll('[', ']');

    SetLength(Result, Length(Elements));
    for I := 0 to High(Result) do
    begin
      Element := Elements[I].Split(',');
      if Length(Element) <> 2 then
        Continue;

      Result[I].X := Element[0].ToInteger(0);
      Result[I].Y := Element[1].ToInteger(0);
    end;
  end;

var
  I: Integer;
  Shape: TSimbaShapeBoxShape;
  ShapeName, ShapeType, ShapeValue: String;
begin
  FList.Items.Clear();

  if FileExists(FileName) then
  try
    with TStringList.Create() do
    try
      LoadFromFile(FileName);

      for I := 0 to Count - 1 do
      begin
        Shape := nil;
        ShapeName := Strings[I].Between('_', '=');
        ShapeType := Strings[I].Before('_');
        if (ShapeType = '') then
          ShapeType := Strings[I].Before('=');
        ShapeValue := Strings[I].After('=');

        case ShapeType of
          'Point':
             begin
               Shape := TSimbaShapeBoxShape_Point.Create(ShapeName);
               with TSimbaShapeBoxShape_Point(Shape) do
                 FPoint := StrToPoint(ShapeValue);
             end;

          'Box':
             begin
               Shape := TSimbaShapeBoxShape_Box.Create(ShapeName);
               with TSimbaShapeBoxShape_Box(Shape) do
                 FRect := StrToRect(ShapeValue);
             end;

          'Poly':
             begin
               Shape := TSimbaShapeBoxShape_Poly.Create(ShapeName);
               with TSimbaShapeBoxShape_Poly(Shape) do
                 FPoly := StrToTPA(ShapeValue);
             end;

          'Path':
             begin
               Shape := TSimbaShapeBoxShape_Path.Create(ShapeName);
               with TSimbaShapeBoxShape_Path(Shape) do
                 FPoly := StrToTPA(ShapeValue);
             end;
        end;

        if (Shape <> nil) then
        begin
          if (ShapeName <> '') then
            FList.Items.AddObject(ShapeName + ' - ' + ShapeType, Shape)
          else
            FList.Items.AddObject(ShapeType, Shape);
        end;
      end;

      FList.ItemIndex := FList.Items.Count - 1;
    finally
      Free();
    end;
  except
  end;
end;

constructor TSimbaShapeBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLeftPanel := TPanel.Create(Self);
  FLeftPanel.Parent := Self;
  FLeftPanel.Align := alLeft;
  FLeftPanel.BevelOuter := bvNone;
  FLeftPanel.Width := 200;

  FList := TListBox.Create(FLeftPanel);
  FList.Parent := FLeftPanel;
  FList.BorderSpacing.Around := 10;
  FList.Align := alClient;
  FList.OnSelectionChange := @DoSelectionChanged;
  if (FList.Items is TStringList) then
    TStringList(FList.Items).OwnsObjects := True;

  FPointButton := TButton.Create(FLeftPanel);
  with FPointButton do
  begin
    Parent := FLeftPanel;
    Align := alTop;
    Caption := 'New Point';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FBoxButton := TButton.Create(FLeftPanel);
  with FBoxButton do
  begin
    Parent := FLeftPanel;
    Align := alTop;
    Caption := 'New Box';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FPolyButton := TButton.Create(FLeftPanel);
  with FPolyButton do
  begin
    Parent := FLeftPanel;
    Align := alTop;
    Caption := 'New Poly';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FPathButton := TButton.Create(FLeftPanel);
  with FPathButton do
  begin
    Parent := FLeftPanel;
    Align := alTop;
    Caption := 'New Path';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  with TButton.Create(FLeftPanel) do
  begin
    Parent := FLeftPanel;
    Align := alBottom;
    Caption := 'Name Shape';
    OnClick := @DoShapeNameClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  with TButton.Create(FLeftPanel) do
  begin
    Parent := FLeftPanel;
    Align := alBottom;
    Caption := 'Delete Shape';
    OnClick := @DoShapeDeleteClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  with TButton.Create(FLeftPanel) do
  begin
    Parent := FLeftPanel;
    Align := alBottom;
    Caption := 'Delete All Shapes';
    OnClick := @DoShapeDeleteAllClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;
end;

end.

