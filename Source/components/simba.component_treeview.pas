{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Treeview container that includes custom scrollbars and filter edit.
}
unit simba.component_treeview;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, StdCtrls, ComCtrls, LMessages, LCLType, TreeFilterEdit, ImgList,
  ATScrollBar,
  simba.component_edit, simba.component_treeviewhint;
     
type
  TSimbaTreeView = class;

  TSimbaInternalTreeView = class(TTreeView)
  protected
    FScrollbarVert: TATScrollbar;
    FScrollbarHorz: TATScrollbar;

    procedure UpdateScrollBars;
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
  public
    constructor Create(AnOwner: TComponent); override;
  end;

  TNodeHintEvent = function(Node: TTreeNode): String of object;

  TSimbaTreeView = class(TCustomControl)
  private
    function GetImages: TCustomImageList;
    function GetOnAfterFilter: TNotifyEvent;
    function GetOnDoubleClick: TNotifyEvent;
    function GetOnSelectionChange: TNotifyEvent;
    procedure SetImages(Value: TCustomImageList);
    procedure SetOnAfterFilter(Value: TNotifyEvent);
    procedure SetOnDoubleClick(Value: TNotifyEvent);
    procedure SetOnSelectionChange(Value: TNotifyEvent);
  protected
    FHint: TSimbaTreeViewHint;
    FFilterEdit: TSimbaEdit;
    FFilter: TTreeFilterEdit;
    FTree: TSimbaInternalTreeView;
    FScrollbarVert: TATScrollbar;
    FScrollbarHorz: TATScrollbar;
    FOnGetNodeHint: TNodeHintEvent;
    FNodeClass: TTreeNodeClass;

    function GetItems: TTreeNodes;
    function GetSelected: TTreeNode;

    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure DoFilterChange(Sender: TObject);

    procedure ScrollHorzChange(Sender: TObject);
    procedure ScrollVertChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; NodeClass: TTreeNodeClass = nil); reintroduce;

    procedure ExpandTopLevelNode(const NodeText: String);
    procedure ExpandFirstNode;

    procedure FullCollapse;
    procedure FullExpand;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure ClearFilter;

    procedure Invalidate; override;

    property OnGetNodeHint: TNodeHintEvent read FOnGetNodeHint write FOnGetNodeHint;
    property OnDoubleClick: TNotifyEvent read GetOnDoubleClick write SetOnDoubleClick;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
    property OnAfterFilter: TNotifyEvent read GetOnAfterFilter write SetOnAfterFilter;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTreeNodes read GetItems;
    property Selected: TTreeNode read GetSelected;

    function AddNode(Str: String; ImageIndex: Integer = -1): TTreeNode;
    function AddChildNode(Node: TTreeNode; Str: String; ImageIndex: Integer = -1): TTreeNode;
  end;

implementation

uses
  Math, simba.theme;

constructor TSimbaTreeView.Create(AOwner: TComponent; NodeClass: TTreeNodeClass);
begin
  inherited Create(AOwner);

  FNodeClass := NodeClass;

  FScrollbarVert := TATScrollbar.Create(Self);
  FScrollbarVert.Parent:= Self;
  FScrollbarVert.Kind:= sbVertical;
  FScrollbarVert.Align:= alRight;
  FScrollbarVert.Width:= ATScrollbarTheme.InitialSize;
  FScrollbarVert.OnChange:= @ScrollVertChange;
  FScrollbarVert.Visible := True;

  FScrollbarHorz := TATScrollbar.Create(Self);
  FScrollbarHorz.Parent:= Self;
  FScrollbarHorz.Kind:= sbHorizontal;
  FScrollbarHorz.Align:= alBottom;
  FScrollbarHorz.Height:= ATScrollbarTheme.InitialSize;
  FScrollbarHorz.IndentCorner := 100;
  FScrollbarHorz.OnChange:= @ScrollHorzChange;
  FScrollbarHorz.Visible := True;

  FTree := TSimbaInternalTreeView.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.ScrollBars := ssNone;
  FTree.FScrollbarVert := FScrollbarVert;
  FTree.FScrollbarHorz := FScrollbarHorz;
  FTree.BorderStyle := bsNone;
  FTree.Options := FTree.Options + [tvoRightClickSelect, tvoReadOnly, tvoAutoItemHeight] - [tvoToolTips, tvoThemedDraw];
  FTree.ExpandSignType := tvestArrow;
  FTree.ExpandSignColor := clWhite;
  FTree.TreeLinePenStyle := psClear;
  FTree.Font.Color := SimbaTheme.ColorFont;
  FTree.BackgroundColor := SimbaTheme.ColorBackground;
  FTree.SelectionColor := SimbaTheme.ColorActive;
  FTree.OnCreateNodeClass := @DoCreateNodeClass;
  FTree.OnMouseMove := @DoMouseMove;
  FTree.DragMode := dmAutomatic;
  FTree.TabStop := False;

  FFilter := TTreeFilterEdit.Create(Self);
  FFilter.FilteredTreeview := FTree;
  FFilter.ExpandAllInitially := True;

  FHint := TSimbaTreeViewHint.Create(FTree);

  FFilterEdit := TSimbaEdit.Create(Self);
  FFilterEdit.Parent := Self;
  FFilterEdit.Align := alBottom;
  FFilterEdit.OnChange := @DoFilterChange;
  FFilterEdit.Color := SimbaTheme.ColorBackground;
  FFilterEdit.ColorBorder := SimbaTheme.ColorFrame;
  FFilterEdit.ColorBorderActive := SimbaTheme.ColorActive;
  FFilterEdit.ColorSelection := SimbaTheme.ColorActive;
  FFilterEdit.Font.Color := SimbaTheme.ColorFont;
  FFilterEdit.HintTextColor := clLtGray;
  FFilterEdit.HintText := '(search)';

  with ATScrollbarTheme do
  begin
    InitialSize := Scale96ToScreen(16);
    ThumbMinSize := Scale96ToScreen(36);
    ThumbRoundedRect := False;
    DirectJumpOnClickPageUpDown := True;

    ColorCorner := SimbaTheme.ColorFrame;
    ColorBG := SimbaTheme.ColorScrollBarInActive;
    ColorThumbBorder := SimbaTheme.ColorScrollBarActive;
    ColorThumbFill := SimbaTheme.ColorScrollBarActive;
    ColorThumbFillOver := SimbaTheme.ColorScrollBarActive;
    ColorThumbFillPressed := SimbaTheme.ColorScrollBarActive;
    ColorThumbDecor := SimbaTheme.ColorScrollBarActive;
    ColorArrowFill := SimbaTheme.ColorScrollBarActive;
    ColorArrowBorder := SimbaTheme.ColorScrollBarActive;
    ColorArrowSign := SimbaTheme.ColorLine;
  end;
end;

procedure TSimbaTreeView.ExpandTopLevelNode(const NodeText: String);
var
  Node: TTreeNode;
begin
  Node := FTree.Items.FindTopLvlNode(NodeText);
  if Assigned(Node) then
    Node.Expanded := True;
end;

procedure TSimbaTreeView.ExpandFirstNode;
begin
  if (FTree.Items.GetFirstNode() <> nil) then
    FTree.Items.GetFirstNode().Expanded := True;
end;

procedure TSimbaTreeView.FullCollapse;
begin
  FTree.FullCollapse();
end;

procedure TSimbaTreeView.FullExpand;
begin
  FTree.FullExpand();
end;

procedure TSimbaTreeView.BeginUpdate;
begin
  FTree.BeginUpdate();
end;

procedure TSimbaTreeView.EndUpdate;
begin
  FTree.EndUpdate();
end;

procedure TSimbaTreeView.Clear;
begin
  FTree.Items.Clear();
  FFilterEdit.Clear();
end;

procedure TSimbaTreeView.ClearFilter;
begin
  FFilterEdit.Clear();
end;

procedure TSimbaTreeView.Invalidate;
begin
  inherited;

  FTree.Invalidate();

  FScrollbarHorz.Invalidate();
  FScrollbarVert.Invalidate();
end;

function TSimbaTreeView.AddNode(Str: String; ImageIndex: Integer): TTreeNode;
begin
  Result := FTree.Items.Add(nil, Str);
  Result.ImageIndex := ImageIndex;
  Result.SelectedIndex := ImageIndex;
end;

function TSimbaTreeView.AddChildNode(Node: TTreeNode; Str: String; ImageIndex: Integer): TTreeNode;
begin
  Result := FTree.Items.AddChild(Node, Str);
  Result.ImageIndex := ImageIndex;
  Result.SelectedIndex := ImageIndex;
end;

function TSimbaTreeView.GetImages: TCustomImageList;
begin
  Result := FTree.Images;
end;

function TSimbaTreeView.GetOnAfterFilter: TNotifyEvent;
begin
  Result := FFilter.OnAfterFilter;
end;

function TSimbaTreeView.GetOnDoubleClick: TNotifyEvent;
begin
  Result := FTree.OnDblClick;
end;

function TSimbaTreeView.GetOnSelectionChange: TNotifyEvent;
begin
  Result := FTree.OnSelectionChanged;
end;

procedure TSimbaTreeView.SetImages(Value: TCustomImageList);
begin
  FTree.Images := Value;
end;

procedure TSimbaTreeView.SetOnAfterFilter(Value: TNotifyEvent);
begin
  FFilter.OnAfterFilter := Value;
end;

procedure TSimbaTreeView.SetOnDoubleClick(Value: TNotifyEvent);
begin
  FTree.OnDblClick := Value;
end;

procedure TSimbaTreeView.SetOnSelectionChange(Value: TNotifyEvent);
begin
  FTree.OnSelectionChanged := Value;
end;

function TSimbaTreeView.GetItems: TTreeNodes;
begin
  Result := FTree.Items;
end;

function TSimbaTreeView.GetSelected: TTreeNode;
begin
  Result := FTree.Selected;
end;

procedure TSimbaTreeView.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  HintText: String;
begin
  if Assigned(OnGetNodeHint) then
  begin
    Node := FTree.GetNodeAt(X, Y);

    if Assigned(Node) then
    begin
      HintText := FOnGetNodeHint(Node);

      if (HintText = '') then
        FHint.Hide()
      else
        FHint.Show(Node, HintText);
    end;
  end;
end;

procedure TSimbaTreeView.DoCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  if Assigned(FNodeClass) then
    NodeClass := FNodeClass
  else
    NodeClass := TTreeNode;
end;

procedure TSimbaTreeView.DoFilterChange(Sender: TObject);
begin
  FFilter.Text := TSimbaEdit(Sender).Text;
end;

procedure TSimbaTreeView.ScrollVertChange(Sender: TObject);
begin
  FTree.ScrolledTop := FScrollbarVert.Position;
end;

procedure TSimbaTreeView.ScrollHorzChange(Sender: TObject);
begin
  FTree.ScrolledLeft := FScrollbarHorz.Position;
end;

procedure TSimbaInternalTreeView.UpdateScrollBars();
begin
  if FScrollbarVert=nil then Exit;
  if FScrollbarHorz=nil then Exit;

  FScrollbarVert.Min := 0;
  FScrollbarVert.PageSize := Height;
  FScrollbarVert.Max := GetMaxScrollTop + FScrollbarVert.PageSize;
  FScrollbarVert.Position := ScrolledTop;

  FScrollbarHorz.Min := 0;
  FScrollbarHorz.PageSize := Max(1, ClientWidth);
  FScrollbarHorz.Max := Max(1, GetMaxScrollLeft + FScrollbarHorz.PageSize);
  FScrollbarHorz.Position := Max(0, ScrolledLeft);

  FScrollbarVert.Update();
  FScrollbarHorz.Update();
end;

procedure TSimbaInternalTreeView.DoDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
var
  CosValue, SinValue: Double;

  function RotatePoint(const P: TPoint; const X, Y: Double): TPoint; inline;
  begin
    Result.X := Round(X + CosValue * (P.X - X) - SinValue * (P.Y - Y));
    Result.Y := Round(Y + SinValue * (P.X - X) + CosValue * (P.Y - Y));
  end;

var
  MidX, MidY: Integer;
  Points: array[0..2] of TPoint;
begin
  MidX := ARect.CenterPoint.X;
  MidY := ARect.CenterPoint.Y;

  Points[0] := Point(MidX - 1, ARect.Top);
  Points[1] := Point(ARect.Right - 1, MidY);
  Points[2] := Point(MidX - 1, ARect.Bottom);

  if (not ACollapsed) then
  begin
    SinCos(DegToRad(90), SinValue, CosValue);

    Points[0] := RotatePoint(Points[0], MidX+2, MidY);
    Points[1] := RotatePoint(Points[1], MidX+2, MidY);
    Points[2] := RotatePoint(Points[2], MidX+2, MidY);
  end;

  Sender.Canvas.Pen.Width := 2;
  Sender.Canvas.Pen.Color := SimbaTheme.ColorLine;
  Sender.Canvas.Polyline(Points);
end;

constructor TSimbaInternalTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  OnCustomDrawArrow := @DoDrawArrow;
end;

procedure TSimbaInternalTreeView.DoSelectionChanged;
begin
  inherited;
  UpdateScrollBars();
end;

procedure TSimbaInternalTreeView.Resize;
begin
  inherited;
  UpdateScrollBars();
end;

procedure TSimbaInternalTreeView.CMChanged(var Message: TLMessage);
begin
  inherited;
  UpdateScrollBars();
end;

function TSimbaInternalTreeView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited;
  UpdateScrollBars();
end;

procedure TSimbaInternalTreeView.Collapse(Node: TTreeNode);
begin
  inherited;
  UpdateScrollBars();
end;

procedure TSimbaInternalTreeView.Expand(Node: TTreeNode);
begin
  inherited;
  UpdateScrollBars();
end;

end.

