unit simba.shapebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ComCtrls, StdCtrls, ExtCtrls, Dialogs, fgl,
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
  protected
    function GetLineColor(const Flags: EPaintShapeFlags): TColor; inline;
    function GetConnectorColor(const Flags: EPaintShapeFlags): TColor; inline;
  public
    FDragStart: TPoint;
    FName: String;
    FUserData: Pointer;
    FShapeType: String;

    constructor Create(ShapeBox: TSimbaShapeBox); virtual;
    destructor Destroy; override;

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

    function ToStr: String; virtual; abstract;
    procedure FromStr(Str: String); virtual; abstract;
  end;

  TSimbaShapeBoxShape_Point = class(TSimbaShapeBoxShape)
  public
    FPoint: TPoint;

    constructor Create(ShapeBox: TSimbaShapeBox); override;

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

    function GetPoint: TPoint;

    function ToStr: String; override;
    procedure FromStr(Str: String); override;
  end;

  TSimbaShapeBoxShape_Box = class(TSimbaShapeBoxShape)
  protected
  type
    EDragPart = (NONE, TL, TR, BL, BR, DCENTER);
  public
    FDraggingCorner: EDragPart;
    FBox: TBox;

    constructor Create(ShapeBox: TSimbaShapeBox); override;

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

    function GetBox: TBox;

    function ToStr: String; override;
    procedure FromStr(Str: String); override;
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

    function Center: TPoint; override;
    function GetDragIndex(MousePoint: TPoint): Integer;
    function DistToEdge(P: TPoint): Integer; override;
    function Contains(P: TPoint; ExpandMod: Integer = 0): Boolean; override;

    function BeginDrag(MousePoint: TPoint): Boolean; override;
    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); override;

    function CanDrag(MousePoint: TPoint; out ACursor: TCursor): Boolean; override;
    procedure Drag(MousePoint: TPoint); override;
    function NeedPaint(PaintArea: TRect): Boolean; override;

    function GetPoly: TPointArray;

    function ToStr: String; override;
    procedure FromStr(Str: String); override;
  end;

  TSimbaShapeBoxShape_Path = class(TSimbaShapeBoxShape_Poly)
  public
    function DistToEdge(P: TPoint): Integer; override;
    function Contains(P: TPoint; ExpandMod: Integer=0): Boolean; override;
    procedure Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint); override;
  end;

  PShapeBoxShape = ^TShapeBoxShape;
  TShapeBoxShape = record
    Name: String;
    Index: Integer;
    UserData: Pointer;

    IsBox: Boolean;
    IsPoint: Boolean;
    IsPoly: Boolean;
    IsPath: Boolean;

    Box: TBox;
    Point: TPoint;
    Path: TPointArray;
    Poly: TPointArray;
  end;

  PSimbaShapeBox = ^TSimbaShapeBox;
  TSimbaShapeBox = class(TSimbaImageBox)
  protected
  type
    TShapeList = specialize TFPGObjectList<TSimbaShapeBoxShape>;
  protected
    FShapes: TShapeList;

    FSelecting: TSimbaShapeBoxShape;
    FDragging: TSimbaShapeBoxShape;

    FPanel: TPanel;
    FListBox: TListBox;

    FPointButton: TButton;
    FBoxButton: TButton;
    FPolyButton: TButton;
    FPathButton: TButton;

    FNameButton: TButton;
    FDeleteButton: TButton;
    FDeleteAllButton: TButton;

    FUserDataSize: Integer;
    FQueryNameOnNew: Boolean;

    function CheckIndex(Index: Integer): Boolean;

    procedure InternalAddShape(Shape: TSimbaShapeBoxShape; IsSelecting: Boolean = False);
    procedure InternalDeleteShape(Index: Integer);
    procedure InternalNameShape(Index: Integer; AName: String);
    procedure InternalStartDragging(Shape: TSimbaShapeBoxShape);
    procedure InternalSetSelecting(Shape: TSimbaShapeBoxShape);
    procedure InternalClear;

    function GetShape(Index: Integer): TShapeBoxShape;
    function GetShapeCount: Integer;
    function GetShapeIndex: Integer;
    function GetSelectedShape: TSimbaShapeBoxShape;
    function GetShapeAt(P: TPoint): TSimbaShapeBoxShape;

    procedure SetUserDataSize(AValue: Integer);

    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure ImageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;

    procedure DoPaintArea(Bitmap: TSimbaImageBoxBitmap; R: TRect); override;
    procedure DoSelectionChanged(Sender: TObject; User: Boolean);
    procedure DoShapeAddButtonClick(Sender: TObject);
    procedure DoShapeDeleteClick(Sender: TObject);
    procedure DoShapeDeleteAllClick(Sender: TObject);
    procedure DoShapeNameClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);

    procedure DeleteShape(Index: Integer);
    procedure DeleteAllShapes;

    procedure ManualAddPoint(Point: TPoint; AName: String = ''); overload;
    procedure ManualAddPoint(Point: TPoint; AName: String; constref UserData); overload;
    procedure ManualAddBox(Box: TBox; AName: String = '');
    procedure ManualAddBox(Box: TBox; AName: String; constref UserData); overload;
    procedure ManualAddPoly(Poly: TPointArray; AName: String = '');
    procedure ManualAddPoly(Poly: TPointArray; AName: String; constref UserData); overload;
    procedure ManualAddPath(Path: TPointArray; AName: String = '');
    procedure ManualAddPath(Path: TPointArray; AName: String; constref UserData); overload;

    property QueryNameOnNew: Boolean read FQueryNameOnNew write FQueryNameOnNew;
    property UserDataSize: Integer read FUserDataSize write SetUserDataSize;
    property Panel: TPanel read FPanel;

    property PointButton: TButton read FPointButton;
    property BoxButton: TButton read FBoxButton;
    property PolyButton: TButton read FPolyButton;
    property PathButton: TButton read FPathButton;
    property NameButton: TButton read FNameButton;
    property DeleteButton: TButton read FDeleteButton;
    property DeleteAllButton: TButton read FDeleteAllButton;

    property ShapeIndex: Integer read GetShapeIndex;
    property ShapeCount: Integer read GetShapeCount;
    property Shape[Index: Integer]: TShapeBoxShape read GetShape;
  end;

implementation

uses
  LCLType, simba.geometry, simba.tpa, math;

const
  CLOSE_DISTANCE = 4;

constructor TSimbaShapeBoxShape.Create(ShapeBox: TSimbaShapeBox);
begin
  inherited Create();

  FShapeType := String(ClassName).After('_');
  FName := FShapeType;
  FUserData := AllocMem(ShapeBox.UserDataSize);
end;

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

constructor TSimbaShapeBoxShape_Point.Create(ShapeBox: TSimbaShapeBox);
begin
  inherited Create(ShapeBox);

  FPoint := TPoint.Create(-1, -1);
end;

procedure TSimbaShapeBoxShape_Point.SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint);
begin
  FPoint := MousePoint;

  Sender.InternalSetSelecting(nil);
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

function TSimbaShapeBoxShape_Point.GetPoint: TPoint;
begin
  Result := FPoint;
end;

function TSimbaShapeBoxShape_Point.ToStr: String;
begin
  Result := IntToStr(FPoint.X) + ',' + IntToStr(FPoint.Y);
end;

procedure TSimbaShapeBoxShape_Point.FromStr(Str: String);
begin
  SScanf(Str, '%d,%d', [@FPoint.X, @FPoint.Y]);
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
  FContainsCache := TSimbaGeometry.ExpandPolygon(FPoly.ConvexHull(), CLOSE_DISTANCE);
end;

procedure TSimbaShapeBoxShape_Poly.SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint);
begin
  if (Length(FPoly) = 0) or (MousePoint.DistanceTo(FPoly[0]) > CLOSE_DISTANCE) then
  begin
    FPoly := FPoly + [MousePoint];

    BuildContainsCache();
  end else
    Sender.InternalSetSelecting(nil);
end;

procedure TSimbaShapeBoxShape_Poly.SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint);
var
  Index: Integer;
begin
  case Key of
    VK_RETURN:
      begin
        Sender.InternalSetSelecting(nil);

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

function TSimbaShapeBoxShape_Poly.Center: TPoint;
begin
  Result := FPoly.Mean();
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

function TSimbaShapeBoxShape_Poly.GetPoly: TPointArray;
begin
  Result := Copy(FPoly);
end;

function TSimbaShapeBoxShape_Poly.ToStr: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(FPoly) do
    Result := Result + '[' + IntToStr(FPoly[I].X) + ',' + IntToStr(FPoly[I].Y) + '], ';
  Result := Result.Trim([' ', ',']);
end;

procedure TSimbaShapeBoxShape_Poly.FromStr(Str: String);
var
  Elements, Element: TStringArray;
  I: Integer;
begin
  Elements := Str.BetweenAll('[', ']');

  SetLength(FPoly, Length(Elements));
  for I := 0 to High(Elements) do
    SScanf(Elements[I], '%d,%d', [@FPoly[I].X, @FPoly[I].Y]);
end;

function TSimbaShapeBoxShape_Box.GetBox: TBox;
begin
  Result := FBox;
  Result.Normalize();
end;

function TSimbaShapeBoxShape_Box.ToStr: String;
begin
  Result := IntToStr(FBox.X1) + ',' + IntToStr(FBox.Y1) + ',' + IntToStr(FBox.X2) + ',' + IntToStr(FBox.Y2);
end;

procedure TSimbaShapeBoxShape_Box.FromStr(Str: String);
begin
  SScanf(Str, '%d,%d,%d,%d', [@FBox.X1, @FBox.Y1, @FBox.X2, @FBox.Y2]);
end;

constructor TSimbaShapeBoxShape_Box.Create(ShapeBox: TSimbaShapeBox);
begin
  inherited Create(ShapeBox);

  FBox.X1 := -1;
  FBox.Y1  := -1;
end;

procedure TSimbaShapeBoxShape_Box.SelectingMouseDown(Sender: TSimbaShapeBox; Button: TMouseButton; Shift: TShiftState; MousePoint: TPoint);
begin
  if (FBox.X1 = -1) and (FBox.Y1 = -1) then
  begin
    FBox.X1 := MousePoint.X;
    FBox.Y1 := MousePoint.Y;
  end
  else
  begin
    FBox.X2 := MousePoint.X;
    FBox.Y2 := MousePoint.Y;

    Sender.InternalSetSelecting(nil);
  end;
end;

procedure TSimbaShapeBoxShape_Box.SelectingKeyDown(Sender: TSimbaShapeBox; var Key: Word; Shift: TShiftState; MousePoint: TPoint);
begin

end;

function TSimbaShapeBoxShape_Box.Center: TPoint;
begin
  Result := FBox.Center;
end;

function TSimbaShapeBoxShape_Box.DistToEdge(P: TPoint): Integer;
var
  Edge: TPoint;
  b: TBox;
begin
  Edge := P;

  b := TBox.Create(FBox.X1, FBox.Y1, FBox.X2, FBox.Y2);
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
begin
  FBox.Normalize();

  Result := FBox.Expand(ExpandMod).Contains(P);
end;

function TSimbaShapeBoxShape_Box.GetDragPart(MousePoint: TPoint): EDragPart;
begin
  if TPoint.Create(FBox.X1, FBox.Y1).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := TL
  else
  if TPoint.Create(FBox.X2, FBox.Y1).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := TR
  else
  if TPoint.Create(FBox.X1, FBox.Y2).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := BL
  else
  if TPoint.Create(FBox.X2, FBox.Y2).DistanceTo(MousePoint) <= CLOSE_DISTANCE then
    Result := BR
  else
  if FBox.Contains(MousePoint) then
    Result := DCENTER
  else
    Result := NONE;
end;

function TSimbaShapeBoxShape_Box.BeginDrag(MousePoint: TPoint): Boolean;
begin
  FDraggingCorner := GetDragPart(MousePoint);

  Result := FDraggingCorner <> NONE;
  if Result then
    FDragStart := MousePoint - TPoint.Create(FBox.X1, FBox.Y1);
end;

procedure TSimbaShapeBoxShape_Box.Paint(Sender: TSimbaShapeBox; ACanvas: TSimbaImageBoxBitmap; Flags: EPaintShapeFlags; MousePoint: TPoint);
begin
  if (FBox.X1 = -1) and (FBox.Y1 = -1) then
    Exit;

  if (EPaintShapeFlag.SELECTING in Flags) then
  begin
    FBox.X2 := MousePoint.X;
    FBox.Y2 := MousePoint.Y;
  end;

  ACanvas.DrawBox(FBox, GetLineColor(Flags));
  ACanvas.DrawBoxFilled(FBox.Create(TPoint.Create(FBox.X1, FBox.Y1), 2, 2), GetConnectorColor(Flags));
  ACanvas.DrawBoxFilled(FBox.Create(TPoint.Create(FBox.X2, FBox.Y1), 2, 2), GetConnectorColor(Flags));

  ACanvas.DrawBoxFilled(FBox.Create(TPoint.Create(FBox.X2, FBox.Y2), 2, 2), GetConnectorColor(Flags));
  ACanvas.DrawBoxFilled(FBox.Create(TPoint.Create(FBox.X1, FBox.Y2), 2, 2), GetConnectorColor(Flags));
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
          //   Offset(X-Left, Y-Top);
          FBox := FBox.Offset(
            (MousePoint.X - FDragStart.X) - FBox.X1,
            (MousePoint.Y - FDragStart.Y) - FBox.Y1
          );
        end;
    TL: begin
          FBox.X1 := MousePoint.X;
          FBox.Y1 := MousePoint.Y;
        end;
    TR: begin
          FBox.X2 := MousePoint.X;
          FBox.Y1 := MousePoint.Y;
        end;
    BL: begin
          FBox.X1 := MousePoint.X;
          FBox.Y2 := MousePoint.Y;
        end;
    BR: begin
          FBox.X2 := MousePoint.X;
          FBox.Y2 := MousePoint.Y;
        end;
  end;
end;

function TSimbaShapeBoxShape_Box.NeedPaint(PaintArea: TRect): Boolean;
begin
  Result := InRange(FBox.Y1, PaintArea.Top,  PaintArea.Bottom) or
            InRange(FBox.Y2, PaintArea.Top,  PaintArea.Bottom) or
            InRange(FBox.X1, PaintArea.Left, PaintArea.Right)  or
            InRange(FBox.X2, PaintArea.Left, PaintArea.Right);
end;

procedure TSimbaShapeBox.DoSelectionChanged(Sender: TObject; User: Boolean);
begin
  if User and (GetSelectedShape() <> nil) then
  begin
    with GetSelectedShape().Center() do
      if not IsVisible(X, Y) then
        MoveTo(X, Y);
  end;

  Paint();
end;

procedure TSimbaShapeBox.DoShapeAddButtonClick(Sender: TObject);
var
  NewShape: TSimbaShapeBoxShape;
  NewName: String;
begin
  NewShape := nil;
  if FQueryNameOnNew and (not InputQuery('Simba', 'Enter name', NewName)) then
    Exit;

  if (Sender = FPathButton) then
  begin
    NewShape := TSimbaShapeBoxShape_Path.Create(Self);
    StatusPanel.Text := 'Selecting path: Click to set points and press ENTER to finish';
  end;

  if (Sender = FPolyButton) then
  begin
    NewShape := TSimbaShapeBoxShape_Poly.Create(Self);
    StatusPanel.Text := 'Selecting polygon: Click to set points and press ENTER to finish';
  end;

  if (Sender = FPointButton) then
  begin
    NewShape := TSimbaShapeBoxShape_Point.Create(Self);
    StatusPanel.Text := 'Selecting point: Click to set';
  end;

  if (Sender = FBoxButton) then
  begin
    NewShape := TSimbaShapeBoxShape_Box.Create(Self);
    StatusPanel.Text := 'Selecting box: Click to set top left then again for bottom left';
  end;

  if (NewShape <> nil) then
  begin
    if FQueryNameOnNew then
      NewShape.FName := NewName;

    InternalAddShape(NewShape, True);
  end;
end;

procedure TSimbaShapeBox.DoShapeDeleteClick(Sender: TObject);
begin
  InternalDeleteShape(FListBox.ItemIndex);

  Paint();
end;

procedure TSimbaShapeBox.DoShapeDeleteAllClick(Sender: TObject);
begin
  FShapes.Clear();
  FListBox.Clear();

  Paint();
end;

procedure TSimbaShapeBox.DoShapeNameClick(Sender: TObject);
var
  Value: String;
begin
  if (FListBox.ItemIndex > -1) then
    if InputQuery('Shape name', 'Enter shape name', Value) then
      InternalNameShape(FListBox.ItemIndex, Value);
end;

function TSimbaShapeBox.GetShapeAt(P: TPoint): TSimbaShapeBoxShape;
var
  I: Integer;
  Dist, BestDist: Integer;
begin
  Result := nil;

  BestDist := Integer.MaxValue;
  for I := 0 to FShapes.Count - 1 do
  begin
    Dist := FShapes[I].DistToEdge(P);

    if FShapes[I].Contains(P, CLOSE_DISTANCE) and (Dist < BestDist) then
    begin
      BestDist := Dist;
      Result   := FShapes[I];
    end;
  end;
end;

function TSimbaShapeBox.GetShape(Index: Integer): TShapeBoxShape;
begin
  Result := Default(TShapeBoxShape);

  if CheckIndex(Index) then
  begin
    Result.Index    := Index;

    Result.Name     := FShapes[Index].FName;
    Result.UserData := FShapes[Index].FUserData;

    Result.IsPoint := (FShapes[Index].ClassType = TSimbaShapeBoxShape_Point);
    Result.IsBox   := (FShapes[Index].ClassType = TSimbaShapeBoxShape_Box);
    Result.IsPath  := (FShapes[Index].ClassType = TSimbaShapeBoxShape_Path);
    Result.IsPoly  := (FShapes[Index].ClassType = TSimbaShapeBoxShape_Poly);

    if Result.IsPoint then
      Result.Point := TSimbaShapeBoxShape_Point(FShapes[Index]).GetPoint();
    if Result.IsPoly then
      Result.Poly := TSimbaShapeBoxShape_Poly(FShapes[Index]).GetPoly();
    if Result.IsPath then
      Result.Path := TSimbaShapeBoxShape_Path(FShapes[Index]).GetPoly();
    if Result.IsBox then
      Result.Box :=  TSimbaShapeBoxShape_Box(FShapes[Index]).GetBox();
  end;
end;

function TSimbaShapeBox.GetShapeCount: Integer;
begin
  Result := FListBox.Count;
end;

function TSimbaShapeBox.GetShapeIndex: Integer;
begin
  Result := FListBox.ItemIndex;
end;

procedure TSimbaShapeBox.SetUserDataSize(AValue: Integer);
begin
  FUserDataSize := AValue;
end;

function TSimbaShapeBox.CheckIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FListBox.Count);
end;

function TSimbaShapeBox.GetSelectedShape: TSimbaShapeBoxShape;
begin
  Result := nil;
  if CheckIndex(FListBox.ItemIndex) then
    Result := FShapes[FListBox.ItemIndex];
end;

procedure TSimbaShapeBox.InternalAddShape(Shape: TSimbaShapeBoxShape; IsSelecting: Boolean);
begin
  FShapes.Add(Shape);
  FListBox.ItemIndex := FListBox.Items.Add(Shape.FName);
  if IsSelecting then
    InternalSetSelecting(Shape);
end;

procedure TSimbaShapeBox.InternalDeleteShape(Index: Integer);
begin
  if CheckIndex(Index) then
  begin
    FShapes.Delete(Index);
    FListBox.Items.Delete(Index);
  end;
end;

procedure TSimbaShapeBox.InternalNameShape(Index: Integer; AName: String);
begin
  if CheckIndex(Index) then
  begin
    FShapes[Index].FName := AName;
    FListBox.Items[Index] := AName;
  end;
end;

procedure TSimbaShapeBox.InternalStartDragging(Shape: TSimbaShapeBoxShape);
begin
  FDragging := Shape;
  FListBox.ItemIndex := FShapes.IndexOf(Shape);
end;

procedure TSimbaShapeBox.InternalSetSelecting(Shape: TSimbaShapeBoxShape);
begin
  FSelecting := Shape;
  if (FSelecting = nil) then
    StatusPanel.Text := '';

  Paint();
end;

procedure TSimbaShapeBox.InternalClear;
begin
  FShapes.Clear();
  FListBox.Items.Clear();
end;

procedure TSimbaShapeBox.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited ImageMouseUp(Sender, Button, Shift, X, Y);

  if (FDragging <> nil) then
    FDragging := nil;
end;

procedure TSimbaShapeBox.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShapeAtMouse: TSimbaShapeBoxShape;
begin
  inherited ImageMouseDown(Sender, Button, Shift, X, Y);

  if (Button <> mbLeft) then
    Exit;

  if (FSelecting = nil) then
  begin
    ShapeAtMouse := Self.GetShapeAt(FMousePoint);
    if (ShapeAtMouse <> nil) and ShapeAtMouse.BeginDrag(FMousePoint) then
    begin
      InternalStartDragging(ShapeAtMouse);
      Exit;
    end;
  end;

  if (FSelecting <> nil) then
    FSelecting.SelectingMouseDown(Self, Button, Shift, FMousePoint);
end;

procedure TSimbaShapeBox.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ShapeAtMouse: TSimbaShapeBoxShape;
  NewCursor: TCursor;
begin
  inherited ImageMouseMove(Sender, Shift, X, Y);

  if (FSelecting = nil) then
  begin
    if (FDragging = nil) then
    begin
      ShapeAtMouse := Self.GetShapeAt(FMousePoint);
      if (ShapeAtMouse <> nil) and ShapeAtMouse.CanDrag(FMousePoint, NewCursor) then
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

procedure TSimbaShapeBox.ImageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited ImageKeyDown(Sender, Key, Shift);

  if (FSelecting <> nil) then
  begin
    FSelecting.SelectingKeyDown(Self, Key, Shift, FMousePoint);
    if (Key = 0) then // Something happened
      Paint();
  end;
end;

procedure TSimbaShapeBox.DoPaintArea(Bitmap: TSimbaImageBoxBitmap; R: TRect);
var
  I: Integer;
  SelectedShape, SelectingShape: TSimbaShapeBoxShape;
  Flags: EPaintShapeFlags;
begin
  SelectingShape := FSelecting;
  SelectedShape  := GetSelectedShape();

  for I := 0 to FShapes.Count - 1 do
    if FShapes[I].NeedPaint(R) then
    begin
      Flags := [];
      if (FShapes[I] = SelectingShape) then Flags := Flags + [EPaintShapeFlag.SELECTING];
      if (FShapes[I] = SelectedShape)  then Flags := Flags + [EPaintShapeFlag.SELECTED];

      FShapes[I].Paint(Self, Bitmap, Flags, FMousePoint);
    end;

  inherited;
end;

procedure TSimbaShapeBox.ManualAddPoint(Point: TPoint; AName: String);
var
  NewShape: TSimbaShapeBoxShape_Point;
begin
  NewShape := TSimbaShapeBoxShape_Point.Create(Self);
  NewShape.FPoint := Point;
  if (AName <> '') then
    NewShape.FName := AName;

  InternalAddShape(NewShape);
end;

procedure TSimbaShapeBox.ManualAddPoint(Point: TPoint; AName: String; constref UserData);
begin
  ManualAddPoint(Point, AName);
  if (FUserDataSize > 0) then
    Move(UserData, FShapes.Last.FUserData^, FUserDataSize);
end;

procedure TSimbaShapeBox.ManualAddBox(Box: TBox; AName: String);
var
  NewShape: TSimbaShapeBoxShape_Box;
begin
  NewShape := TSimbaShapeBoxShape_Box.Create(Self);
  NewShape.FBox := Box;
  if (AName <> '') then
    NewShape.FName := AName;

  InternalAddShape(NewShape);
end;

procedure TSimbaShapeBox.ManualAddBox(Box: TBox; AName: String; constref UserData);
begin
  ManualAddBox(Box, AName);
  if (FUserDataSize > 0) then
    Move(UserData, FShapes.Last.FUserData^, FUserDataSize);
end;

procedure TSimbaShapeBox.ManualAddPoly(Poly: TPointArray; AName: String);
var
  NewShape: TSimbaShapeBoxShape_Poly;
begin
  NewShape := TSimbaShapeBoxShape_Poly.Create(Self);
  NewShape.FPoly := Poly;
  if (AName <> '') then
    NewShape.FName := AName;

  InternalAddShape(NewShape);
end;

procedure TSimbaShapeBox.ManualAddPoly(Poly: TPointArray; AName: String; constref UserData);
begin
  ManualAddPoly(Poly, AName);
  if (FUserDataSize > 0) then
    Move(UserData, FShapes.Last.FUserData^, FUserDataSize);
end;

procedure TSimbaShapeBox.ManualAddPath(Path: TPointArray; AName: String);
var
  NewShape: TSimbaShapeBoxShape_Path;
begin
  NewShape := TSimbaShapeBoxShape_Path.Create(Self);
  NewShape.FPoly := Path;
  if (AName <> '') then
    NewShape.FName := AName;

  InternalAddShape(NewShape);
end;

procedure TSimbaShapeBox.ManualAddPath(Path: TPointArray; AName: String; constref UserData);
begin
  ManualAddPath(Path, AName);
  if (FUserDataSize > 0) then
    Move(UserData, FShapes.Last.FUserData^, FUserDataSize);
end;

procedure TSimbaShapeBox.SaveToFile(FileName: String);
var
  I: Integer;
begin
  if (FShapes.Count = 0) then
    Exit;

  with TStringList.Create() do
  try
    for I := 0 to FShapes.Count - 1 do
    begin
      Add('[' + FShapes[I].FShapeType + ']');
      Add('Name=' + FShapes[I].FName);
      Add('Value=' + FShapes[I].ToStr());
      Add('');
    end;

    SaveToFile(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaShapeBox.LoadFromFile(FileName: String);
var
  I: Integer;
  ShapeName, ShapeValue: String;
  NewShape: TSimbaShapeBoxShape;
  ShapeClass: TSimbaShapeBoxShapeClass;
begin
  InternalClear();

  if FileExists(FileName) then
  try
    with TStringList.Create() do
    try
      LoadFromFile(FileName);

      I := 0;
      while (I < (Count - 3)) do
      begin
        ShapeClass := nil;

        if (Strings[I] = '[Box]')   then ShapeClass := TSimbaShapeBoxShape_Box   else
        if (Strings[I] = '[Point]') then ShapeClass := TSimbaShapeBoxShape_Point else
        if (Strings[I] = '[Path]')  then ShapeClass := TSimbaShapeBoxShape_Path  else
        if (Strings[I] = '[Poly]')  then ShapeClass := TSimbaShapeBoxShape_Poly;

        if (ShapeClass <> nil) then
        begin
          ShapeName := Strings[I+1].After('Name=');
          ShapeValue := Strings[I+2].After('Value=');

          NewShape := ShapeClass.Create(Self);
          NewShape.FromStr(ShapeValue);
          NewShape.FName := ShapeName;

          InternalAddShape(NewShape);

          I := I + 3;
        end else
          I := I + 1;
      end;
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaShapeBox.DeleteShape(Index: Integer);
begin
  InternalDeleteShape(Index);
end;

procedure TSimbaShapeBox.DeleteAllShapes;
begin
  InternalClear();
end;

constructor TSimbaShapeBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FShapes := TShapeList.Create();

  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alLeft;
  FPanel.BevelOuter := bvNone;
  FPanel.Width := 200;

  FListBox := TListBox.Create(FPanel);
  FListBox.Parent := FPanel;
  FListBox.BorderSpacing.Around := 10;
  FListBox.Align := alClient;
  FListBox.OnSelectionChange := @DoSelectionChanged;

  FPointButton := TButton.Create(FPanel);
  with FPointButton do
  begin
    Parent := FPanel;
    Align := alTop;
    Caption := 'New Point';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FBoxButton := TButton.Create(FPanel);
  with FBoxButton do
  begin
    Parent := FPanel;
    Align := alTop;
    Caption := 'New Box';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FPolyButton := TButton.Create(FPanel);
  with FPolyButton do
  begin
    Parent := FPanel;
    Align := alTop;
    Caption := 'New Poly';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FPathButton := TButton.Create(FPanel);
  with FPathButton do
  begin
    Parent := FPanel;
    Align := alTop;
    Caption := 'New Path';
    OnClick := @DoShapeAddButtonClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FNameButton := TButton.Create(FPanel);
  with FNameButton do
  begin
    Parent := FPanel;
    Align := alBottom;
    Caption := 'Name Shape';
    OnClick := @DoShapeNameClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FDeleteButton := TButton.Create(FPanel);
  with FDeleteButton do
  begin
    Parent := FPanel;
    Align := alBottom;
    Caption := 'Delete Shape';
    OnClick := @DoShapeDeleteClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;

  FDeleteAllButton := TButton.Create(FPanel);
  with FDeleteAllButton do
  begin
    Parent := FPanel;
    Align := alBottom;
    Caption := 'Delete All Shapes';
    OnClick := @DoShapeDeleteAllClick;
    AutoSize := True;
    BorderSpacing.Around := 5;
  end;
end;

destructor TSimbaShapeBox.Destroy;
begin
  if (FShapes <> nil) then
    FreeAndNil(FShapes);

  inherited Destroy();
end;

end.

