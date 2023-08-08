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
  Classes, SysUtils, Controls, Forms, Graphics, StdCtrls, ComCtrls, LMessages, LCLType, ImgList,
  simba.component_edit, simba.component_treeviewhint, simba.component_scrollbar, simba.component_button;
     
type
  TSimbaTreeView = class;

  TSimbaInternalTreeView = class(TTreeView)
  protected
    FLoading: Boolean;
    FScrollbarVert: TSimbaScrollBar;
    FScrollbarHorz: TSimbaScrollBar;

    procedure UpdateScrollBars;
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetLoading(Value: Boolean);
  public
    property Loading: Boolean read FLoading write SetLoading;

    constructor Create(AnOwner: TComponent); override;
  end;

  TNodeHintEvent = function(const Node: TTreeNode): String of object;
  TNodeForEachEvent = procedure(const Node: TTreeNode) is nested;

  TSimbaTreeView = class(TCustomControl)
  protected
    FFilterPanel: TCustomControl;
    FFilterEdit: TSimbaEdit;
    FFilterClearButton: TSimbaButton;
    FHint: TSimbaTreeViewHint;
    FTree: TSimbaInternalTreeView;
    FScrollbarVert: TSimbaScrollBar;
    FScrollbarHorz: TSimbaScrollBar;
    FOnGetNodeHint: TNodeHintEvent;
    FNodeClass: TTreeNodeClass;
    FOnAfterFilter: TNotifyEvent;

    procedure FontChanged(Sender: TObject); override;

    procedure UpdateFilter;

    function GetImages: TCustomImageList;
    function GetLoading: Boolean;
    function GetOnDoubleClick: TNotifyEvent;
    function GetOnSelectionChange: TNotifyEvent;

    procedure SetFilter(Value: String);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLoading(Value: Boolean);
    procedure SetOnDoubleClick(Value: TNotifyEvent);
    procedure SetOnSelectionChange(Value: TNotifyEvent);
    procedure SetScrolledLeft(Value: Integer);
    procedure SetScrolledTop(Value: Integer);

    function GetScrolledLeft: Integer;
    function GetScrolledTop: Integer;

    function GetItems: TTreeNodes;
    function GetSelected: TTreeNode;
    function GetFilter: String;
    function GetTopLevelCount: Integer;

    procedure DoClearFilterClick(Sender: TObject);
    procedure DoFilterEditChange(Sender: TObject);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);

    procedure ScrollHorzChange(Sender: TObject);
    procedure ScrollVertChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; NodeClass: TTreeNodeClass = nil); reintroduce;

    procedure FullCollapse;
    procedure FullExpand;

    procedure ForEachTopLevel(Func: TNodeForEachEvent);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure ClearSelection;

    procedure Invalidate; override;

    property OnGetNodeHint: TNodeHintEvent read FOnGetNodeHint write FOnGetNodeHint;
    property OnDoubleClick: TNotifyEvent read GetOnDoubleClick write SetOnDoubleClick;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
    property OnAfterFilter: TNotifyEvent read FOnAfterFilter write FOnAfterFilter;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTreeNodes read GetItems;
    property Selected: TTreeNode read GetSelected;
    property Filter: String read GetFilter write SetFilter;
    property Loading: Boolean read GetLoading write SetLoading;
    property ScrolledLeft: Integer read GetScrolledLeft write SetScrolledLeft;
    property ScrolledTop: Integer read GetScrolledTop write SetScrolledTop;
    property TopLevelCount: Integer read GetTopLevelCount;

    function AddNode(const NodeText: String; const ImageIndex: Integer = -1): TTreeNode; overload;
    function AddNode(const ParentNode: TTreeNode; const NodeText: String; const ImageIndex: Integer = -1): TTreeNode; overload;
  end;

implementation

uses
  Math,
  simba.theme, simba.main;

constructor TSimbaTreeView.Create(AOwner: TComponent; NodeClass: TTreeNodeClass);
var
  test: TCustomControl;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  FNodeClass := NodeClass;

  test := TCustomControl.Create(Self);
  test.Parent := Self;
  test.Align := alClient;

  FScrollbarVert := TSimbaScrollBar.Create(Self);
  FScrollbarVert.Parent := test;
  FScrollbarVert.Kind := sbVertical;
  FScrollbarVert.Align := alRight;
  FScrollbarVert.OnChange := @ScrollVertChange;
  FScrollbarVert.Visible := True;

  FScrollbarHorz := TSimbaScrollBar.Create(Self);
  FScrollbarHorz.Parent := test;
  FScrollbarHorz.Kind := sbHorizontal;
  FScrollbarHorz.Align := alBottom;
  FScrollbarHorz.IndentCorner := 100;
  FScrollbarHorz.OnChange := @ScrollHorzChange;
  FScrollbarHorz.Visible := True;

  FTree := TSimbaInternalTreeView.Create(Self);
  FTree.Parent := test;
  FTree.Align := alClient;
  FTree.ScrollBars := ssNone;
  FTree.FScrollbarVert := FScrollbarVert;
  FTree.FScrollbarHorz := FScrollbarHorz;
  FTree.BorderStyle := bsNone;
  FTree.Options := FTree.Options + [tvoReadOnly, tvoAutoItemHeight, tvoNoDoubleClickExpand] - [tvoToolTips, tvoThemedDraw];

  FTree.ExpandSignType := tvestArrow;
  FTree.ExpandSignColor := clWhite;
  FTree.TreeLinePenStyle := psClear;
  FTree.OnCreateNodeClass := @DoCreateNodeClass;
  FTree.OnMouseMove := @DoMouseMove;
  FTree.DragMode := dmAutomatic;
  FTree.TabStop := False;
  FTree.BackgroundColor := SimbaTheme.ColorBackground;
  FTree.SelectionColor := SimbaTheme.ColorActive;
  FTree.Font.Color := SimbaTheme.ColorFont;
  FTree.Images := SimbaForm.Images;

  FScrollbarVert.ForwardScrollControl := FTree;

  FHint := TSimbaTreeViewHint.Create(FTree);

  FFilterPanel := TCustomControl.Create(Self);
  FFilterPanel.Parent := Self;
  FFilterPanel.Align := alBottom;
  FFilterPanel.AutoSize := True;
  FFilterPanel.Color := SimbaTheme.ColorFrame;

  FFilterEdit := TSimbaEdit.Create(Self);
  FFilterEdit.Parent := FFilterPanel;
  FFilterEdit.Align := alClient;
  FFilterEdit.OnChange := @DoFilterEditChange;
  FFilterEdit.HintText := '(search)';

  FFilterClearButton := TSimbaTransparentButton.Create(Self);
  FFilterClearButton.Parent := FFilterPanel;
  FFilterClearButton.Align := alRight;
  FFilterClearButton.AutoSize := True;
  FFilterClearButton.OnClick := @DoClearFilterClick;
  FFilterClearButton.Hint := 'Clear Filter';
  FFilterClearButton.ShowHint := True;
  FFilterClearButton.SetClearFilterGlyph();
end;

procedure TSimbaTreeView.FullCollapse;
begin
  FTree.BeginUpdate();
  FTree.FullCollapse();
  FTree.EndUpdate();
end;

procedure TSimbaTreeView.FullExpand;
begin
  FTree.BeginUpdate();
  FTree.FullExpand();
  FTree.EndUpdate();
end;

procedure TSimbaTreeView.ForEachTopLevel(Func: TNodeForEachEvent);
var
  I: Integer;
begin
  FTree.BeginUpdate();
  for I := 0 to FTree.Items.TopLvlCount - 1 do
    Func(FTree.Items.TopLvlItems[I]);
  FTree.EndUpdate();
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

procedure TSimbaTreeView.ClearSelection;
begin
  FTree.ClearSelection();
end;

procedure TSimbaTreeView.Invalidate;
begin
  inherited;

  FTree.Invalidate();

  FScrollbarHorz.Invalidate();
  FScrollbarVert.Invalidate();
end;

function TSimbaTreeView.AddNode(const NodeText: String; const ImageIndex: Integer): TTreeNode;
begin
  Result := FTree.Items.Add(nil, NodeText);
  Result.ImageIndex := ImageIndex;
  Result.SelectedIndex := ImageIndex;
end;

function TSimbaTreeView.AddNode(const ParentNode: TTreeNode; const NodeText: String; const ImageIndex: Integer): TTreeNode;
begin
  if (ParentNode = nil) then
    Result := FTree.Items.Add(nil, NodeText)
  else
    Result := FTree.Items.AddChild(ParentNode, NodeText);

  Result.ImageIndex := ImageIndex;
  Result.SelectedIndex := ImageIndex;
end;

function TSimbaTreeView.GetFilter: String;
begin
  Result := FFilterEdit.Text;
end;

function TSimbaTreeView.GetScrolledLeft: Integer;
begin
  Result := FTree.ScrolledLeft;
end;

function TSimbaTreeView.GetScrolledTop: Integer;
begin
  Result := FTree.ScrolledTop;
end;

procedure TSimbaTreeView.SetScrolledLeft(Value: Integer);
begin
  FTree.ScrolledLeft := Value;
  FTree.UpdateScrollBars();
end;

procedure TSimbaTreeView.SetScrolledTop(Value: Integer);
begin
  FTree.ScrolledTop := Value;
  FTree.UpdateScrollBars();
end;

function TSimbaTreeView.GetTopLevelCount: Integer;
begin
  Result := FTree.Items.TopLvlCount;
end;

procedure TSimbaTreeView.DoClearFilterClick(Sender: TObject);
begin
  Filter := '';
end;

procedure TSimbaTreeView.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  FTree.Font := Self.Font;
  FTree.Font.Color := SimbaTheme.ColorFont;

  FFilterEdit.Font := Self.Font;
  FFilterEdit.Font.Color := SimbaTheme.ColorFont;
end;

procedure TSimbaTreeView.UpdateFilter;
var
  Node, NodeParent: TTreeNode;
  FilterText: String;
begin
  FilterText := LowerCase(FFilterEdit.Text);

  Items.BeginUpdate();

  try
    Node := Items.GetFirstNode();
    while Assigned(Node) do
    begin
      Node.Visible := (FilterText = '') or (Pos(FilterText, LowerCase(Node.Text)) > 0);

      if Node.Visible then
      begin
        Node.Expanded := True;

        NodeParent := Node.Parent;
        while Assigned(NodeParent) do
        begin
          NodeParent.Expanded := True;
          NodeParent.Visible := True;
          NodeParent := NodeParent.Parent;
        end;
      end;

      Node := Node.GetNext();
    end;

    if Assigned(FOnAfterFilter) then
      FOnAfterFilter(Self);
  finally
    Items.EndUpdate();
  end;
end;

function TSimbaTreeView.GetImages: TCustomImageList;
begin
  Result := FTree.Images;
end;

function TSimbaTreeView.GetLoading: Boolean;
begin
  Result := FTree.Loading;
end;

function TSimbaTreeView.GetOnDoubleClick: TNotifyEvent;
begin
  Result := FTree.OnDblClick;
end;

function TSimbaTreeView.GetOnSelectionChange: TNotifyEvent;
begin
  Result := FTree.OnSelectionChanged;
end;

procedure TSimbaTreeView.SetFilter(Value: String);
begin
  if (FFilterEdit.Text <> Value) then
    FFilterEdit.Text := Value;
end;

procedure TSimbaTreeView.SetImages(Value: TCustomImageList);
begin
  FTree.Images := Value;
end;

procedure TSimbaTreeView.SetLoading(Value: Boolean);
begin
  FTree.Enabled := not Value;
  FTree.Loading := Value;
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

procedure TSimbaTreeView.DoFilterEditChange(Sender: TObject);
begin
  UpdateFilter();
end;

procedure TSimbaTreeView.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  HintText: String;
begin
  if Assigned(OnGetNodeHint) then
  begin
    Node := FTree.GetNodeAt(X, Y);

    if Assigned(Node) and (X > Node.DisplayTextLeft) and (X < Node.DisplayTextRight) then
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

procedure TSimbaInternalTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  n: TTreeNode;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (Button = mbLeft) and (not (ssDouble in Shift)) then
  begin
    n := GetNodeAt(X,Y);
    if Assigned(n) then
      n.Expanded := not n.Expanded;
  end;
end;

procedure TSimbaInternalTreeView.Paint;
begin
  if FLoading then
  begin
    Canvas.Brush.Color := BackgroundColor;
    Canvas.FillRect(ClientRect);

    Exit;
  end;

  inherited Paint();
end;

procedure TSimbaInternalTreeView.SetLoading(Value: Boolean);
begin
  if (Value = FLoading) then
    Exit;
  FLoading := Value;

  Invalidate();
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

