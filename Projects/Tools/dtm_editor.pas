unit dtm_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, LResources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus, Types, Client,
  MufasaTypes, GraphType, Buttons, DTM,
  {$IFDEF WINDOWS} os_windows {$ELSE} os_linux {$ENDIF};

type
  TDTMNode = class(TTreeNode)
  protected
    function GetPoint: PMDTMPoint;
  public
    __Point: TMDTMPoint;

    property Point: PMDTMPoint read GetPoint;
  end;

  TDTMTree = class(TTreeView)
  protected
    FOnBeforeSelectionChange: TNotifyEvent;
    FOnPointEdited: TNotifyEvent;

    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DoEdit(Sender: TObject; Node: TTreeNode; var S: string);
    procedure DoAllowEdit(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure DoBeforeSelectionChange(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure DoDrawNode(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPaint; override;
  public
    property OnPointEdited: TNotifyEvent read FOnPointEdited write FOnPointEdited;
    property OnBeforeSelectionChange: TNotifyEvent read FOnBeforeSelectionChange write FOnBeforeSelectionChange;

    function Add(Point: TMDTMPoint): TDTMNode; overload;

    function GetDTM: TMDTM;
    function GetDTMPoints: TMDTMPointArray;

    constructor Create(AnOwner: TComponent); override;
  end;

  TDTMEditorResult = procedure(DTM: String) of object;

  TDTMForm = class(TForm)
    btnUpdateImage: TButton;
    btnClearImage: TButton;
    btnFindDTM: TButton;
    btnPrintDTM: TButton;
    Divider2: TDividerBevel;
    Images: TImageList;
    imgClient: TImage;
    imgMouseZoom: TImage;
    lblMouseZoom: TLabel;
    MainMenu: TMainMenu;
    menuImage: TMenuItem;
    menuSave: TMenuItem;
    menuMarkColor: TMenuItem;
    popupMarkColor: TMenuItem;
    popupClear: TMenuItem;
    popupDelete: TMenuItem;
    menuPoints: TMenuItem;
    menuPointDelete: TMenuItem;
    menuPointClear: TMenuItem;
    menuFile: TMenuItem;
    menuNew: TMenuItem;
    menuOpen: TMenuItem;
    OpenDialog: TOpenDialog;
    pnlTree: TPanel;
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
    popupTree: TPopupMenu;
    SaveDialog: TSaveDialog;
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;
    PointFlasher: TTimer;

    procedure MarkColorClick(Sender: TObject);
    procedure menuPointsClick(Sender: TObject);
    procedure popupTreeOpen(Sender: TObject);
    procedure PrintDTMClick(Sender: TObject);
    procedure FindDTMClick(Sender: TObject);
    procedure PointFlash(Sender: TObject);
    procedure ClearPoints(Sender: TObject);
    procedure DeletePoint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenDTM(Sender: TObject);
    procedure SaveDTM(Sender: TObject);
    procedure MouseZoomPaint(Sender: TObject);
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
    FTree: TDTMTree;
    FDebug: TMemo;

    procedure Status(S: String);
    procedure Coords(X, Y: Int32);
    procedure Dimensions(W, H: Int32);

    procedure DrawDTMPoint(Point: TMDTMPoint; Flash: Boolean = False);
    procedure DrawDTM;

    function GetPointAt(X, Y: Int32): TTreeNode;

    procedure SelectColor(AColor: Int32);

    procedure PointEdited(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure BeforeSelectionChange(Sender: TObject);

    procedure DrawTPA(TPA: TPointArray);
    procedure ApplyZoom(Reset: Boolean = False);
  public
    OnGetResult: TDTMEditorResult;

    constructor Create(AManager: TIOManager; Debug: TMemo); overload; reintroduce;
    destructor Destroy; override;
  end;

var
  DTMForm: TDTMForm;

implementation

uses
  mmisc, colour_conv, bitmaps, lcltype, dtmutil, math, tpa;

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

function TDTMTree.Add(Point: TMDTMPoint): TDTMNode;

  function AddNode(Parent: TTreeNode; Text: String): TDTMNode;
  begin
    if (Parent <> nil) then
      Result := Items.AddNode(TDTMNode.Create(Items), Parent, Text, nil, naAddChild) as TDTMNode
    else
      Result := Items.AddNode(TDTMNode.Create(Items), nil, Text, nil, naAdd) as TDTMNode;
  end;

begin
  Result := AddNode(nil, 'Point[' + IntToStr(Items.TopLvlCount) + ']');
  Result.ImageIndex := 0;
  Result.SelectedIndex := 0;
  Result.Point^ := Point;

  AddNode(AddNode(Result, 'X'), IntToStr(Point.X));
  AddNode(AddNode(Result, 'Y'), IntToStr(Point.Y));
  AddNode(AddNode(Result, 'Color'), IntToStr(Point.C));
  AddNode(AddNode(Result, 'Tolerance'), IntToStr(Point.T));
  AddNode(AddNode(Result, 'Size'), IntToStr(Point.ASZ));

  Selected := Result;
  ScrolledTop := $FFFFFF;
end;

function TDTMTree.GetDTM: TMDTM;
var
  Points: TMDTMPointArray;
  i: Int32;
begin
  Points := GetDTMPoints();
  if (Length(Points) < 2) then
    Exit(nil);

  Result := TMDTM.Create();
  for i := 0 to High(Points) do
    Result.AddPoint(Points[i]);
end;

function TDTMTree.GetDTMPoints: TMDTMPointArray;
var
  i: Int32;
begin
  SetLength(Result, Items.TopLvlCount);
  for i := 0 to High(Result) do
    Result[i] := TDTMNode(Items.TopLvlItems[i]).Point^;
end;

constructor TDTMTree.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  FEditor := TEdit.Create(Self);
  FEditor.OnEditingDone := @EditorEditingDone;
  FEditor.OnKeyDown := @EditorKeyDown;
  FEditor.NumbersOnly := True;
  FEditor.OnMouseWheel := @DoEditorMouseWheel;

  OnAdvancedCustomDrawItem := @DoDrawNode;
  OnMouseDown := @DoMouseDown;
  OnChanging := @DoBeforeSelectionChange;
  OnEditing := @DoAllowEdit;
  OnEdited := @DoEdit;
  OnKeyDown := @DoKeyDown;

  AutoExpand := True;
  Options := Options - [tvoThemedDraw, tvoShowButtons] + [tvoNoDoubleClickExpand, tvoRowSelect];
  BackgroundColor := clNone;
  SelectionColor := cl3DLight;
end;

function TDTMNode.GetPoint: PMDTMPoint;
var
  Node: TTreeNode;
begin
  Node := Self;
  while (Node.Level > 0) do
    Node := Node.Parent;

  Result := @TDTMNode(Node).__Point;
end;

procedure TDTMTree.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Selected := GetNodeAtY(Y);
end;

procedure TDTMTree.DoBeforeSelectionChange(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  if (FOnBeforeSelectionChange <> nil) then
    FOnBeforeSelectionChange(Self);
end;

procedure TDTMTree.DoDrawNode(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);

  function IsPointSelected(Node: TTreeNode): Boolean;
  begin
    while (Node.Level > 0) do
      Node := Node.Parent;

    while (Node <> nil)  do
    begin
      if Node.Selected then
        Exit(True);

      Node := Node.GetNext();
      if (Node = nil) or (Node.Level = 0) then
        Exit(False);
    end;

    Exit(False);
  end;

begin
  PaintImages := True;
  DefaultDraw := True;

  case Stage of
    cdPrePaint:
      begin
        if IsPointSelected(Node) then
        begin
          Sender.Canvas.Brush.Color := SelectionColor;
          Sender.Canvas.Pen.Color := SelectionColor;
        end else
        begin
          Sender.Canvas.Brush.Color := clWhite;
          Sender.Canvas.Pen.Color := clWhite;
        end;

        Sender.Canvas.Rectangle(ClientRect.Left, Node.DisplayRect(False).Top, ClientRect.Right, Node.DisplayRect(False).Bottom);
      end;
  end;
end;

procedure TDTMTree.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Node: TTreeNode;
  i: Int32;
begin
  Node := Selected;

  if (Node <> nil) then
  try
    BeginUpdate();

    case Key of
      VK_DELETE:
        begin
          while (Node.Level > 0) do
            Node := Node.Parent;

          Node.Delete();
        end;

      VK_UP:
        begin
          while (Node.Level > 0) do
            Node := Node.Parent;

         for i := 0 to Items.TopLvlCount - 1 do
           if (Items.TopLvlItems[i] = Node) then
             if (i > 0) then
             begin
               Selected := Items.TopLvlItems[i-1];
               Selected.MakeVisible();
             end;
        end;

      VK_DOWN:
        begin
          while (Node.Level > 0) do
            Node := Node.Parent;

         for i := 0 to Items.TopLvlCount - 1 do
           if (Items.TopLvlItems[i] = Node) then
             if (i < Items.TopLvlCount - 1) then
             begin
               Selected := Items.TopLvlItems[i+1];
               Selected.MakeVisible();
             end;
        end;
    end;
  finally
    EndUpdate();
  end;

  if (Key in [VK_DELETE, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ADD, VK_SUBTRACT]) then
    Key := VK_UNKNOWN;
end;

procedure TDTMTree.DoEditorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  DoMouseWheel(Shift, WheelDelta, MousePos);

  Handled := True;
end;

procedure TDTMTree.DoEdit(Sender: TObject; Node: TTreeNode; var S: string);
begin
  if (Node.Text <> S) then
  begin
    Node.Text := S;

    while (Node.Level > 0) do
      Node := Node.Parent;

    with Node as TDTMNode do
    begin
      Point^.X := StrToIntDef(FindNode('X').Items[0].Text, 0);
      Point^.Y := StrToIntDef(FindNode('Y').Items[0].Text, 0);
      Point^.C := StrToIntDef(FindNode('Color').Items[0].Text, 0);
      Point^.T := StrToIntDef(FindNode('Tolerance').Items[0].Text, 0);
      Point^.ASZ := StrToIntDef(FindNode('Size').Items[0].Text, 0);
    end;

    if (FOnPointEdited <> nil) then
      FOnPointEdited(Self);
  end;
end;

procedure TDTMTree.DoAllowEdit(Sender: TObject; Node: TTreeNode;  var AllowEdit: Boolean);
var
  R: TRect;
  P: TPoint;
begin
  R := Node.DisplayRect(True);
  R.Inflate(2, 2);
  P := ScreenToClient(Mouse.CursorPos);

  AllowEdit := (Node.Level = 2) and PtInRect(R, P);
end;

procedure TDTMTree.DoPaint;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clWhite;
  Canvas.Rectangle(ClientRect);

  inherited DoPaint();
end;

procedure TDTMForm.ApplyZoom(Reset: Boolean);
var
  X, Y, W, H: Int32;
begin
  if Reset then
  begin
    FZoom.Previous := 1;
    FZoom.Current := 1;
  end;

  X := imgClient.ScreenToClient(Mouse.CursorPos).X;
  Y := imgClient.ScreenToClient(Mouse.CursorPos).Y;
  W := imgClient.Width;
  H := imgClient.Height;

  imgClient.Width := Trunc(FClient.MBitmaps[0].Width * FZoom.Current);
  imgClient.Height := Trunc(FClient.MBitmaps[0].Height * FZoom.Current);

  if (FZoom.Previous <> FZoom.Current) then
  begin
    ScrollBox.HorzScrollBar.Position := Round(X * imgClient.Width / W) - Round(ScrollBox.ClientWidth / 2);
    ScrollBox.VertScrollBar.Position := Round(Y * imgClient.Height / H) - Round(ScrollBox.ClientHeight / 2);
  end;

  ClientImageCenter(nil);
end;

procedure TDTMForm.ClientImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

    if FClient.MBitmaps[0].PointInBitmap(X, Y) then
    begin
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
    end;

    if (GetPointAt(X, Y) <> nil) then
      imgClient.Cursor := crHandPoint
    else
      imgClient.Cursor := crDefault;

    Coords(X, Y);
  end;
end;

procedure TDTMForm.ClientImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
      case imgClient.Cursor of
        crDefault:
          begin
            if FClient.MBitmaps[0].PointInBitmap(X, Y) then
              DrawDTMPoint(FTree.Add(CreateDTMPoint(X, Y, FClient.MBitmaps[0].FastGetPixel(X, Y), 0, 0, False)).Point^);

            imgClient.Cursor := crHandPoint;
          end;
        crHandPoint:
          FTree.Selected := GetPointAt(X, Y);
      end;
  end;
end;

procedure TDTMForm.ClientImageScrollDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
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

procedure TDTMForm.ClientImageScrollUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if (ssCtrl in Shift) then
  begin
    FZoom.Previous := FZoom.Current;
    FZoom.Current *= 2.00;
    // Lazarus image stretching SUCKS on linux
    if (FZoom.Current > {$IFDEF LINUX}4.00{$ELSE}32.00{$ENDIF}) then
      FZoom.Current := {$IFDEF LINUX}4.00{$ELSE}32.00{$ENDIF};
    // limitation in SetBounds
    if (Trunc(FClient.MBitmaps[0].Width * FZoom.Current) > 100000) or (Trunc(FClient.MBitmaps[0].Height * FZoom.Current) > 100000) then
      FZoom.Current := FZoom.Previous;

    ApplyZoom();
  end;
end;

procedure TDTMForm.ClientImageCenter(Sender: TObject);
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

procedure TDTMForm.ChangeDrawColor(Sender: TObject);
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

procedure TDTMForm.Status(S: String);
begin
  StatusBar.Panels[2].Text := S;
end;

procedure TDTMForm.Coords(X, Y: Int32);
begin
  StatusBar.Panels[0].Text := Format('(%d, %d)', [X, Y]);
end;

procedure TDTMForm.Dimensions(W, H: Int32);
begin
  StatusBar.Panels[1].Text := IntToStr(W) + 'x' + IntToStr(H);
end;

procedure TDTMForm.DrawDTMPoint(Point: TMDTMPoint; Flash: Boolean);
var
  Points: TMDTMPointArray;
begin
  Points := FTree.GetDTMPoints();

  imgClient.Canvas.Pen.Color := clRed;
  imgClient.Canvas.Brush.Color := clRed;

  if (Length(Points) > 1) then
  begin
    imgClient.Canvas.Line(Points[0].X, Points[0].Y, Point.X, Point.Y);
    imgClient.Canvas.FillRect(Points[0].X - Max(1, Points[0].ASZ), Points[0].Y - Max(1, Points[0].ASZ),
                              Points[0].X + Max(1, Points[0].ASZ), Points[0].Y + Max(1, Points[0].ASZ)
                             );
  end;

  if Flash then
    case Odd(PointFlasher.Tag) of
      True: imgClient.Canvas.Brush.Color := clYellow;
      False: imgClient.Canvas.Brush.Color := clBlack;
    end;


  imgClient.Canvas.FillRect(Point.X - Max(1, Point.ASZ), Point.Y - Max(1, Point.ASZ),
                            Point.X + Max(1, Point.ASZ), Point.Y + Max(1, Point.ASZ)
                             );

  PointFlasher.Enabled := True;
  PointFlasher.Tag := PointFlasher.Tag + 1;
end;

procedure TDTMForm.DrawDTM;
var
  Points: TMDTMPointArray;
  i: Int32;
begin
  PointFlasher.Enabled := False;

  imgClient.Picture.Bitmap.FromBitmap(FClient.MBitmaps[0]);
  imgClient.Picture.Bitmap.BeginUpdate(True);

  try
    Points := FTree.GetDTMPoints();

    if (Length(Points) > 0) then
      for i := 0 to High(Points) do
        DrawDTMPoint(Points[i]);
  finally
    imgClient.Picture.Bitmap.EndUpdate();
  end;
end;

function TDTMForm.GetPointAt(X, Y: Int32): TTreeNode;
var
  Points: TMDTMPointArray;
  i: Int32;
begin
  if PointFlasher.Enabled then
  begin
    Points := FTree.GetDTMPoints();

    for i := 0 to High(Points) do
    begin
      if (X >= Points[i].X - Max(1, Points[i].ASZ)) and (Y >= Points[i].Y - Max(1, Points[i].ASZ)) and
         (X <= Points[i].X + Max(1, Points[i].ASZ)) and (Y <= Points[i].Y + Max(1, Points[i].ASZ)) then
        Exit(FTree.Items.TopLvlItems[i]);
    end;
  end;

  Exit(nil);
end;

procedure TDTMForm.PointEdited(Sender: TObject);
begin
  DrawDTM();

  if (FTree.Selected <> nil) then
    SelectColor(TDTMNode(FTree.Selected).Point^.C);
end;

procedure TDTMForm.SelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
  i: Int32;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if (FTree.Selected = nil) then // Delete
  begin
    for i := 0 to FTree.Items.TopLvlCount - 1 do
      FTree.Items.TopLvlItems[i].Text := 'Point[' + IntToStr(i) + ']';

    PointFlasher.Enabled := False;

    DrawDTM();
  end else
  begin
    // Make point fully visible.
    Node := FTree.Selected;
    while (Node.Level > 0) do
      Node := Node.Parent;
    Node.GetLastSubChild().MakeVisible();

    SelectColor(TDTMNode(FTree.Selected).Point^.C);
  end;
end;

// Reset the flashing point to yellow.
procedure TDTMForm.BeforeSelectionChange(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;

  if (not PointFlasher.Enabled) then
    DrawDTM();

  if (FTree.Selected <> nil) then
    DrawDTMPoint(TDTMNode(FTree.Selected).Point^);
end;

procedure TDTMForm.DrawTPA(TPA: TPointArray);
var
  P: TPoint;
  Data: PByte;
  BytesPerLine, BytesPerPixel, W, H: Int32;
  RGB: TRGB24;
begin
  imgClient.Picture.Bitmap.FromBitmap(FClient.MBitmaps[0]);
  imgClient.Picture.Bitmap.BeginUpdate(False);

  W := imgClient.Picture.Bitmap.Width;
  H := imgClient.Picture.Bitmap.Height;

  Data := imgClient.Picture.Bitmap.RawImage.Data;
  BytesPerLine := imgClient.Picture.Bitmap.RawImage.Description.BytesPerLine;
  BytesPerPixel := imgClient.Picture.Bitmap.RawImage.Description.BitsPerPixel div 8;

  try
    ColorToRGB(FDrawColor, RGB.R, RGB.G, RGB.B);

    for P in TPA do
      if (P.X >= 0) and (P.Y >= 0) and (P.X < W) and (P.Y < H) then
        Move(RGB, Data[P.Y * BytesPerLine + P.X * BytesPerPixel], SizeOf(TRGB24));
  finally
    imgClient.Picture.Bitmap.EndUpdate(False);
  end;
end;

procedure TDTMForm.ClientImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TDTMForm.ClientImageUpdate(Sender: TObject);
begin
  imgClient.Picture.Bitmap.FromWindow(FManager, FClient.MBitmaps[0]);

  Dimensions(FClient.MBitmaps[0].Width, FClient.MBitmaps[0].Height);
  ApplyZoom(True);
end;

procedure TDTMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  DTM: TMDTM;
begin
  CloseAction := caFree;

  if (OnGetResult <> nil) then
  begin
    DTM := FTree.GetDTM();

    if (DTM <> nil) then
    begin
      OnGetResult(DTM.ToString());

      DTM.Free();
    end;
  end;
end;

procedure TDTMForm.MouseZoomPaint(Sender: TObject);
begin
  with Sender as TGraphicControl do
    if Boolean(Tag) then
    begin
      Canvas.Pen.Color := FDrawColor;
      Canvas.Frame(40, 40, 60, 60);
    end;
end;

procedure TDTMForm.SelectColor(AColor: Int32);
var
  R, G, B: Int32;
  H, S, L: Extended;
begin
  imgMouseZoom.Tag := PtrInt(False);
  imgMouseZoom.Canvas.Brush.Color := AColor;
  imgMouseZoom.Canvas.Clear();

  ColorToRGB(AColor, R, G, B);
  ColorToHSL(AColor, H, S, L);

  lblMouseZoom.Caption := Format('Color: %d', [AColor])                         + LineEnding +
                          Format('RGB: [%d, %d, %d]', [R, G, B])                + LineEnding +
                          Format('HSL: [%f, %f, %f]', [H, S, L])                + LineEnding;
end;

procedure TDTMForm.ClientImageMouseLeave(Sender: TObject);
begin
  Coords(-1, -1);

  FScroll.Active := False;

  imgClient.Cursor := crDefault;
end;

procedure TDTMForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_DELETE, VK_UP, VK_DOWN]) and (ActiveControl <> FTree) then
    FTree.KeyDown(Key, Shift);
end;

procedure TDTMForm.OpenDTM(Sender: TObject);
var
  Value: String = '';
  DTM: TMDTM;
  i: Int32;
begin
  if InputQuery('Open DTM', 'Enter DTM String (DTM will be normalized)', Value) then
  begin
    ClearPoints(nil);

    if (Pos(#39, Value) > 0) then // in case someone passes more than the actual string value.
    begin
      Value := Copy(Value, Pos(#39, Value) + 1, $FFFFFF);
      Value := Copy(Value, 1, Pos(#39, Value) - 1);
    end;

    DTM := TMDTM.Create();

    try
      DTM.LoadFromString(Value);

      for i := 0 to High(DTM.Points) do
        FTree.Add(DTM.Points[i]);
    except
      ShowMessage('Invalid DTM String');
    end;

    DTM.Free();
  end;
end;

procedure TDTMForm.SaveDTM(Sender: TObject);
var
  List: TStringList;
  DTM: TMDTM;
begin
  DTM := FTree.GetDTM();

  if (DTM <> nil) and SaveDialog.Execute then
  begin
    List := TStringList.Create();

    try
      List.Add('DTM := DTMFromString(' + #39 + DTM.ToString() + #39 + ');');
      List.SaveToFile(SaveDialog.FileName);
    except
    end;

    List.Free();
    DTM.Free();
  end;
end;

procedure TDTMForm.PointFlash(Sender: TObject);
begin
  if (FTree.Selected <> nil) then
    DrawDTMPoint(TDTMNode(FTree.Selected).Point^, True);
end;

procedure TDTMForm.FindDTMClick(Sender: TObject);
var
  W, H: Int32;
  TPA: TPointArray;
  i: Int32;
  T: Double;
  P: TPoint;
  DTM: TMDTM;
begin
  DTM := FTree.GetDTM();

  if (DTM <> nil) then
  begin
    PointFlasher.Enabled := False;

    T := MarkTime();

    FClient.IOManager.GetDimensions(W, H);
    FClient.MFinder.FindDTMs(DTM, TPA, 0, 0, W-1, H-1);

    if (Length(TPA) < 2500) then
    begin
      Status(Format('Found %d matches in %f ms', [Length(TPA), MarkTime() - T]));

      for i := 0 to High(TPA) do
      begin
        P := TPA[i];

        AppendTPA(TPA, TPAFromLine(P.X - 4, P.Y - 4, P.X + 4, P.Y + 4));
        AppendTPA(TPA, TPAFromLine(P.X + 4, P.Y - 4, P.X - 4, P.Y + 4));
      end;

      DrawTPA(TPA);
    end else
      Status(Format('Found too many matches in %f ms! Unable to show matches.', [MarkTime() - T]));

    DTM.Free();
  end;
end;

procedure TDTMForm.PrintDTMClick(Sender: TObject);
var
  DTM: TMDTM;
begin
  DTM := FTree.GetDTM();

  if (DTM <> nil) then
  begin
    FDebug.Lines.Add('DTM := DTMFromString(' + #39 + DTM.ToString + #39 + ');');

    DTM.Free();
  end;
end;

procedure TDTMForm.MarkColorClick(Sender: TObject);
var
  W, H: Int32;
  TPA: TPointArray;
  T: Double;
begin
  if (FTree.Selected <> nil) then
  begin
    PointFlasher.Enabled := False;

    T := MarkTime();

    FClient.IOManager.GetDimensions(W, H);
    FClient.MFinder.SetToleranceSpeed(1);
    with TDTMNode(FTree.Selected).Point^ do
      FClient.MFinder.FindColorsTolerance(TPA, C, 0, 0, W-1, H-1, T);

    Status(Format('Found %d matches in %f ms', [Length(TPA), MarkTime() - T]));

    DrawTPA(TPA);
  end;
end;

procedure TDTMForm.menuPointsClick(Sender: TObject);
begin
  menuPointDelete.Enabled := FTree.Selected <> nil;
  menuMarkColor.Enabled := FTree.Selected <> nil;
  menuPointClear.Enabled := FTree.Items.Count > 0;
end;

procedure TDTMForm.popupTreeOpen(Sender: TObject);
begin
  popupMarkColor.Enabled := FTree.Selected <> nil;
  popupDelete.Enabled := FTree.Selected <> nil;
  popupClear.Enabled := FTree.Items.Count > 0;
end;

procedure TDTMForm.ClearPoints(Sender: TObject);
begin
  FTree.Items.Clear();

  ClientImageClear(nil);
end;

procedure TDTMForm.DeletePoint(Sender: TObject);
var
  Key: Word = VK_DELETE;
begin
  FTree.KeyDown(Key, []); // ya!
end;

procedure TDTMForm.ClientImageClear(Sender: TObject);
begin
  DrawDTM();
end;

constructor TDTMForm.Create(AManager: TIOManager; Debug: TMemo);
begin
  inherited Create(Application.MainForm);

  FManager := AManager;
  FDebug := Debug;
  FDrawColor := clRed;
  FClient := TClient.Create();
  FClient.MBitmaps.AddBMP(TMufasaBitmap.Create()); // MBitmaps[0] will be our client image.
  FClient.IOManager.SetTarget(FClient.MBitmaps[0]);

  FTree := TDTMTree.Create(Self);
  FTree.Parent := pnlTree;
  FTree.Align := alClient;
  FTree.BorderSpacing.Left := 5;
  FTree.BorderSpacing.Right := 5;
  FTree.BorderSpacing.Top := 5;
  FTree.Images := Images;
  FTree.SelectionFontColor := FTree.Font.Color;
  FTree.SelectionFontColorUsed := True;
  FTree.OnPointEdited := @PointEdited;
  FTree.OnBeforeSelectionChange := @BeforeSelectionChange;
  FTree.OnSelectionChanged := @SelectionChanged;
  FTree.PopupMenu := popupTree;
  FTree.ScrollBars := ssAutoBoth;

  imgMouseZoom.Picture.Bitmap.Width := 5;
  imgMouseZoom.Picture.Bitmap.Height := 5;

  ScrollBox.DoubleBuffered := True;

  ClientImageUpdate(nil);
end;

destructor TDTMForm.Destroy;
begin
  FClient.Free();

  inherited Destroy();
end;

initialization
  {$I dtm_editor.lrs}

end.

