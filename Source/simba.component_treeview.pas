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
  Classes, SysUtils, Controls, Forms, Graphics, StdCtrls, ComCtrls, LMessages, LCLType, ImgList, Types,
  simba.component_edit, simba.component_treeviewhint, simba.component_scrollbar, simba.component_button,
  simba.settings;

type
  TSimbaInternalTreeView = class(TTreeView)
  protected
  type
    TSimbaInternalTreeNodes = class(TTreeNodes)
    public
      OnBeginUpdate: TNotifyEvent;
      OnEndUpdate: TNotifyEvent;
      procedure BeginUpdate; override;
      procedure EndUpdate; override;
    end;
  protected
    FLoading: Boolean;
    FScrollbarVert: TSimbaScrollBar;
    FScrollbarHorz: TSimbaScrollBar;

    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;

    procedure DoBeginUpdate(Sender: TObject);
    procedure DoEndUpdate(Sender: TObject);

    function CreateNodes: TTreeNodes; override;
    procedure UpdateScrollBars;
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetLoading(Value: Boolean);
  public
    property Loading: Boolean read FLoading write SetLoading;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TNotifyEvent read FOnEndUpdate write FOnEndUpdate;
  end;

  TNodePaintEvent = procedure(Canvas: TCanvas; Node: TTreeNode) of object;
  TNodeColorEvent = procedure(Node: TTreeNode; var Color: TColor) of object;
  TNodeHintEvent = function(Node: TTreeNode): String of object;
  TNodeForEachEvent = procedure(Node: TTreeNode) is nested;

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
    FOnGetNodeColor: TNodeColorEvent;
    FOnPaintNode: TNodePaintEvent;
    FOnAfterFilter: TNotifyEvent;
    FOnClear: TNotifyEvent;
    FOnModify: TNotifyEvent;
    FNodeClass: TTreeNodeClass;
    FTempBackgroundColor: TColor;
    FFilterOnlyTopLevel: Boolean;
    FFilterCollapseOnClear: Boolean;
    FKeyEvents: array of record
      Key: Integer;
      Shift: TShiftState;
      Callback: TKeyEvent;
    end;
    FModified: Boolean;

    procedure FontChanged(Sender: TObject); override;

    procedure UpdateFilter;

    function GetImages: TCustomImageList;
    function GetLoading: Boolean;
    function GetOnDoubleClick: TNotifyEvent;
    function GetOnSelectionChange: TNotifyEvent;
    function GetFilterVisible: Boolean;
    function GetScrolledLeft: Integer;
    function GetScrolledTop: Integer;
    function GetItems: TTreeNodes;
    function GetSelected: TTreeNode;
    function GetFilter: String;
    function GetTopLevelCount: Integer;
    function GetTopLevelItem(Index: Integer): TTreeNode;

    procedure SetFilterVisible(Value: Boolean);
    procedure SetFilter(Value: String);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLoading(Value: Boolean);
    procedure SetOnDoubleClick(Value: TNotifyEvent);
    procedure SetOnSelectionChange(Value: TNotifyEvent);
    procedure SetScrolledLeft(Value: Integer);
    procedure SetScrolledTop(Value: Integer);
    procedure SetSelected(AValue: TTreeNode);

    procedure DoClearFilterClick(Sender: TObject);
    procedure DoFilterEditChange(Sender: TObject);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure DoDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure DoSettingChanged_ImageSize(Setting: TSimbaSetting);
    procedure DoScrollHorzChange(Sender: TObject);
    procedure DoScrollVertChange(Sender: TObject);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEndUpdate(Sender: TObject);
    procedure DoTreeAddOrDelete(Sender: TObject; Node: TTreeNode);
    procedure DoDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
  public
    constructor Create(AOwner: TComponent; NodeClass: TTreeNodeClass = nil); reintroduce;

    procedure HideRoot;

    procedure FullCollapse;
    procedure FullExpand;

    procedure ForEachTopLevel(Func: TNodeForEachEvent);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure ClearSelection;
    procedure DeleteSelection;

    procedure Invalidate; override;

    property OnPaintNode: TNodePaintEvent read FOnPaintNode write FOnPaintNode;
    property OnGetNodeColor: TNodeColorEvent read FOnGetNodeColor write FOnGetNodeColor;
    property OnGetNodeHint: TNodeHintEvent read FOnGetNodeHint write FOnGetNodeHint;
    property OnDoubleClick: TNotifyEvent read GetOnDoubleClick write SetOnDoubleClick;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
    property OnAfterFilter: TNotifyEvent read FOnAfterFilter write FOnAfterFilter;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTreeNodes read GetItems;
    property Selected: TTreeNode read GetSelected write SetSelected;
    property Filter: String read GetFilter write SetFilter;
    property Loading: Boolean read GetLoading write SetLoading;
    property ScrolledLeft: Integer read GetScrolledLeft write SetScrolledLeft;
    property ScrolledTop: Integer read GetScrolledTop write SetScrolledTop;
    property TopLevelCount: Integer read GetTopLevelCount;
    property TopLevelItem[Index: Integer]: TTreeNode read GetTopLevelItem;
    property FilterVisible: Boolean read GetFilterVisible write SetFilterVisible;
    property FilterOnlyTopLevel: Boolean read FFilterOnlyTopLevel write FFilterOnlyTopLevel;
    property FilterCollapseOnClear: Boolean read FFilterCollapseOnClear write FFilterCollapseOnClear;

    property ScrollbarHorz: TSimbaScrollBar read FScrollbarHorz;
    property ScrollbarVert: TSimbaScrollBar read FScrollbarVert;

    function AddNode(const NodeText: String; const ImageIndex: Integer = -1): TTreeNode; overload;
    function AddNode(const ParentNode: TTreeNode; const NodeText: String; const ImageIndex: Integer = -1): TTreeNode; overload;

    procedure AddKeyEvent(Key: Integer; Shift: TShiftState; Callback: TKeyEvent);
    procedure RemoveKeyEvent(Key: Integer; Shift: TShiftState; Callback: TKeyEvent);
  end;

implementation

uses
  Math,
  simba.ide_theme,
  simba.ide_utils,
  simba.component_images;

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
  FScrollbarVert.OnChange := @DoScrollVertChange;
  FScrollbarVert.Visible := True;

  FScrollbarHorz := TSimbaScrollBar.Create(Self);
  FScrollbarHorz.Parent := test;
  FScrollbarHorz.Kind := sbHorizontal;
  FScrollbarHorz.Align := alBottom;
  FScrollbarHorz.IndentCorner := 100;
  FScrollbarHorz.OnChange := @DoScrollHorzChange;
  FScrollbarHorz.Visible := True;

  FTree := TSimbaInternalTreeView.Create(Self);
  FTree.Parent := test;
  FTree.Align := alClient;
  FTree.ScrollBars := ssNone;
  FTree.FScrollbarVert := FScrollbarVert;
  FTree.FScrollbarHorz := FScrollbarHorz;
  FTree.BorderStyle := bsNone;
  FTree.Options := FTree.Options + [tvoReadOnly, tvoAutoItemHeight, tvoNoDoubleClickExpand, tvoRightClickSelect] - [tvoToolTips, tvoThemedDraw];
  FTree.ExpandSignType := tvestArrow;
  FTree.ExpandSignColor := clWhite;
  FTree.TreeLinePenStyle := psClear;
  FTree.OnCreateNodeClass := @DoCreateNodeClass;
  FTree.OnCustomDrawArrow := @DoDrawArrow;
  FTree.OnMouseMove := @DoMouseMove;
  FTree.DragMode := dmAutomatic;
  FTree.TabStop := False;
  FTree.BackgroundColor := SimbaTheme.ColorBackground;
  FTree.SelectionColor := SimbaTheme.ColorActive;
  FTree.Font.Color := SimbaTheme.ColorFont;
  FTree.OnAdvancedCustomDrawItem := @DoDrawItem;
  FTree.OnEndUpdate := @DoEndUpdate;
  FTree.OnAddition := @DoTreeAddOrDelete;
  FTree.OnDeletion := @DoTreeAddOrDelete;
  FTree.AddHandlerOnKeyDown(@DoKeyDown);
  FTree.ExpandSignSize := ImageWidthForDPI(Font.PixelsPerInch) - 2;
  FTree.Indent := ImageWidthForDPI(Font.PixelsPerInch) - 2;

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
  FFilterClearButton.Image := ESimbaButtonImage.CLEAR_FILTER;
  FFilterClearButton.BorderSpacing.Around := 2;
  FFilterClearButton.XPadding := 3;

  FScrollbarVert.ForwardScrollControl := FTree;

  with SimbaSettings do
    RegisterChangeHandler(Self, General.CustomImageSize, @DoSettingChanged_ImageSize);
end;

procedure TSimbaTreeView.HideRoot;
begin
  FTree.Options := FTree.Options - [tvoShowRoot];
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

  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TSimbaTreeView.ClearSelection;
begin
  FTree.ClearSelection();
end;

procedure TSimbaTreeView.DeleteSelection;
begin
  if (FTree.Selected <> nil) then
    FTree.Items.Delete(FTree.Selected);
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

procedure TSimbaTreeView.AddKeyEvent(Key: Integer; Shift: TShiftState; Callback: TKeyEvent);
begin
  SetLength(FKeyEvents, Length(FKeyEvents) + 1);
  FKeyEvents[High(FKeyEvents)].Key := Key;
  FKeyEvents[High(FKeyEvents)].Shift := Shift;
  FKeyEvents[High(FKeyEvents)].Callback := Callback;
end;

procedure TSimbaTreeView.RemoveKeyEvent(Key: Integer; Shift: TShiftState; Callback: TKeyEvent);
var
  I: Integer;
begin
  for I := 0 to High(FKeyEvents) do
    if (FKeyEvents[I].Key = Key) and (FKeyEvents[I].Shift = Shift) and (FKeyEvents[I].Callback = Callback) then
    begin
      Delete(FKeyEvents, I, 1);
      Break;
    end;
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

function TSimbaTreeView.GetFilterVisible: Boolean;
begin
  Result := FFilterPanel.Visible;
end;

procedure TSimbaTreeView.SetFilterVisible(Value: Boolean);
begin
  FFilterPanel.Visible := Value;
end;

procedure TSimbaTreeView.SetSelected(AValue: TTreeNode);
begin
  FTree.Selected := AValue;
end;

function TSimbaTreeView.GetTopLevelItem(Index: Integer): TTreeNode;
begin
  Result := FTree.Items.TopLvlItems[Index];
end;

procedure TSimbaTreeView.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  FTree.Font := Self.Font;
  FTree.Font.Color := SimbaTheme.ColorFont;
  FTree.ExpandSignSize := ImageWidthForDPI(Font.PixelsPerInch) - 2;
  FTree.Indent := ImageWidthForDPI(Font.PixelsPerInch) - 2;

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
      if not Node.Enabled then
      begin
        Node.Visible := False;
        Node := Node.GetNextSibling;

        Continue;
      end;

      Node.Visible := ((FilterText = '') or (Pos(FilterText, LowerCase(Node.Text)) > 0));

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

      if FFilterOnlyTopLevel then
        Node := Node.GetNextSkipChildren()
      else
        Node := Node.GetNext();
    end;

    if (FilterText = '') and FFilterCollapseOnClear then
      FullCollapse();

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

procedure TSimbaTreeView.DoDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  TheColor: TColor;
begin
  if Assigned(FOnGetNodeColor) then
    case Stage of
      cdPrePaint:
        begin
          FTempBackgroundColor := Sender.BackgroundColor;
          TheColor := Sender.BackgroundColor;
          FOnGetNodeColor(Node, TheColor);
          Sender.BackgroundColor := TheColor;
        end;

      cdPostPaint:
        Sender.BackgroundColor := FTempBackgroundColor;
    end;

  if Assigned(FOnPaintNode) then
    if (Stage = cdPostPaint) then
    begin
      FOnPaintNode(Sender.Canvas, Node);
    end;
end;

procedure TSimbaTreeView.DoSettingChanged_ImageSize(Setting: TSimbaSetting);
begin
  FTree.ExpandSignSize := ImageWidthForDPI(Font.PixelsPerInch) - 2;
  FTree.Indent := ImageWidthForDPI(Font.PixelsPerInch) - 2;

  Invalidate();
end;

procedure TSimbaTreeView.DoScrollVertChange(Sender: TObject);
begin
  FTree.ScrolledTop := FScrollbarVert.Position;
end;

procedure TSimbaTreeView.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  for I := 0 to High(FKeyEvents) do
    if (FKeyEvents[I].Key = Key) and (FKeyEvents[I].Shift = Shift) then
      FKeyEvents[I].Callback(Self, Key, Shift);
end;

procedure TSimbaTreeView.DoEndUpdate(Sender: TObject);
begin
  if FModified and Assigned(FOnModify) then
    FOnModify(Self);
  FModified := False;
end;

procedure TSimbaTreeView.DoTreeAddOrDelete(Sender: TObject; Node: TTreeNode);
begin
  if FTree.Items.IsUpdating then // will get called in DoEndUpdate
  begin
    FModified := True;
    Exit;
  end;

  if Assigned(FOnModify) then
    FOnModify(Self);
end;

procedure TSimbaTreeView.DoDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
var
  R: TScaledImageListResolution;
begin
  R := SimbaComponentImages.ResolutionForPPI[16, Sender.Font.PixelsPerInch, Sender.GetCanvasScaleFactor];
  R.Draw(
    Sender.Canvas,
    ARect.Left + (ARect.Right - ARect.Left - R.Height) div 2,
    ARect.Top + (ARect.Bottom - ARect.Top - R.Height) div 2,
    IfThen(ACollapsed, SimbaComponentImages.ARROW_RIGHT, SimbaComponentImages.ARROW_DOWN)
  );
end;

procedure TSimbaTreeView.DoScrollHorzChange(Sender: TObject);
begin
  FTree.ScrolledLeft := FScrollbarHorz.Position;
end;

procedure TSimbaInternalTreeView.DoBeginUpdate(Sender: TObject);
begin
  if Assigned(FOnBeginUpdate) then
    FOnBeginUpdate(Self);
end;

procedure TSimbaInternalTreeView.DoEndUpdate(Sender: TObject);
begin
  if Assigned(FOnEndUpdate) then
    FOnEndUpdate(Self);
end;

function TSimbaInternalTreeView.CreateNodes: TTreeNodes;
begin
  Result := TSimbaInternalTreeNodes.Create(Self);
  TSimbaInternalTreeNodes(Result).OnBeginUpdate := @DoBeginUpdate;
  TSimbaInternalTreeNodes(Result).OnEndUpdate := @DoEndUpdate;
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

procedure TSimbaInternalTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  n: TTreeNode;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (Button = mbLeft) and (not (ssDouble in Shift)) then
  begin
    n := GetNodeAtY(Y);
    if Assigned(n) and (X > n.DisplayStateIconLeft)  then
    begin
      n.Selected := True;
      n.Expanded := not n.Expanded;
    end;
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

procedure TSimbaInternalTreeView.TSimbaInternalTreeNodes.BeginUpdate;
var
  WasUpdating: Boolean;
begin
  WasUpdating := IsUpdating;
  inherited BeginUpdate();
  if Assigned(OnBeginUpdate) and (not WasUpdating) then
    OnBeginUpdate(Self);
end;

procedure TSimbaInternalTreeView.TSimbaInternalTreeNodes.EndUpdate;
begin
  inherited EndUpdate();
  if Assigned(OnEndUpdate) and (not IsUpdating) then
    OnEndUpdate(Self);
end;

end.

