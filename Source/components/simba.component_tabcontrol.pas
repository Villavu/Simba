unit simba.component_tabcontrol;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ImgList, Menus,
  attabs;

type
  TSimbaTabControl = class;
  TSimbaTab = class(TCustomControl)
  protected
    procedure TextChanged; override;

    procedure SetVisible(Value: Boolean); override;

    procedure TabShow; virtual;
    procedure TabHide; virtual;

    function GetImageIndex: TImageIndex;
    function GetTabControl: TSimbaTabControl;
    function GetTabData: TATTabData;
    procedure SetImageIndex(Value: TImageIndex);
  public
    property TabControl: TSimbaTabControl read GetTabControl;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex;
  end;
  TSimbaTabClass = class of TSimbaTab;

  TSimbaTabControl = class(TCustomControl)
  public
  type
    TTabMovedEvent     = procedure(Sender: TSimbaTabControl; AFrom, ATo: Integer) of object;
    TTabCanChangeEvent = procedure(Sender: TSimbaTabControl; OldTab, NewTab: TSimbaTab; var AllowChange: Boolean) of object;
    TTabCloseEvent     = procedure(Sender: TSimbaTabControl; Tab: TSimbaTab; var CanClose: Boolean) of object;
  protected
    FTabs: TATTabs;
    FTabClass: TSimbaTabClass;
    FTabMovedEvent: TTabMovedEvent;
    FTabCloseEvent: TTabCloseEvent;
    FTabCanChangeEvent: TTabCanChangeEvent;
    FDefaultTitle: String;

    function GetTabHeight: Integer;

    procedure ShowControl(AControl: TControl); override;
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;

    procedure DoTabMoved(Sender: TObject; AIndexFrom, AIndexTo: Integer);
    procedure DoTabChanged(Sender: TObject);
    procedure DoTabPlusClick(Sender: TObject);
    procedure DoTabClose(Sender: TObject; ATabIndex: Integer; var ACanClose, ACanContinue: Boolean);
    procedure DoTabRightClick(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoTabChangeQuery(Sender: TObject; ANewTabIndex: Integer; var ACanChange: Boolean);

    function GetIsClickingCloseButton: Boolean;
    function GetActiveTab: TSimbaTab;
    function GetCanAddTabOnDoubleClick: Boolean;
    function GetCanMoveTabs: Boolean;
    function GetTabByIndex(Index: Integer): TSimbaTab;
    function GetTabCount: Integer;
    function GetShowCloseButtons: Boolean;
    function GetOnMouseDown: TMouseEvent;
    function GetOnMouseLeave: TNotifyEvent;
    function GetOnMouseMove: TMouseMoveEvent;

    procedure SetActiveTab(Value: TSimbaTab);
    procedure SetCanAddTabOnDoubleClick(Value: Boolean);
    procedure SetCanMoveTabs(Value: Boolean);
    procedure SetShowCloseButtons(Value: Boolean);
    procedure SetOnMouseDown(Value: TMouseEvent);
    procedure SetOnMouseLeave(Value: TNotifyEvent);
    procedure SetOnMouseMove(Value: TMouseMoveEvent);
  public
    constructor Create(AOwner: TComponent; TabClass: TSimbaTabClass; ADefaultTitle: String = ''); reintroduce;

    function AddTab(Title: String = ''): TSimbaTab;
    function DeleteTab(Tab: TSimbaTab): Boolean;
    procedure MoveTab(AFrom, ATo: Integer);

    property DefaultTitle: String read FDefaultTitle write FDefaultTitle;
    property IsClickingCloseButton: Boolean read GetIsClickingCloseButton;
    property CanMoveTabs: Boolean read GetCanMoveTabs write SetCanMoveTabs;
    property CanAddTabOnDoubleClick: Boolean read GetCanAddTabOnDoubleClick write SetCanAddTabOnDoubleClick;
    property ShowCloseButtons: Boolean read GetShowCloseButtons write SetShowCloseButtons;

    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TSimbaTab read GetTabByIndex;
    property ActiveTab: TSimbaTab read GetActiveTab write SetActiveTab;

    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;

    property OnTabMoved: TTabMovedEvent read FTabMovedEvent write FTabMovedEvent;
    property OnTabClose: TTabCloseEvent read FTabCloseEvent write FTabCloseEvent;
    property OnTabCanChange: TTabCanChangeEvent read FTabCanChangeEvent write FTabCanChangeEvent;

    function InEmptySpace(X, Y: Integer): Boolean;
    function GetTabAt(X, Y: Integer): TSimbaTab;
  end;

implementation

uses
  simba.main, simba.mufasatypes;

function TSimbaTab.GetImageIndex: TImageIndex;
begin
  Result := GetTabData().TabImageIndex;
end;

function TSimbaTab.GetTabControl: TSimbaTabControl;
begin
  Result := Owner as TSimbaTabControl;
end;

procedure TSimbaTab.SetImageIndex(Value: TImageIndex);
begin
  GetTabData().TabImageIndex := Value;
end;

function TSimbaTab.GetTabData: TATTabData;
var
  I: Integer;
begin
  Result := nil;

  with TabControl.FTabs do
  begin
    I := FindTabByObject(Self);
    if (I > -1) then
      Result := GetTabData(I);
  end;
end;

procedure TSimbaTab.TextChanged;
var
  I: Integer;
begin
  inherited TextChanged();

  with TabControl.FTabs do
  begin
    I := FindTabByObject(Self);
    if (I > -1) then
      GetTabData(I).TabCaption := Self.Caption;
  end;
end;

procedure TSimbaTab.SetVisible(Value: Boolean);
var
  ValueChanged: Boolean;
begin
  ValueChanged := Value <> Visible;

  inherited SetVisible(Value);

  if ValueChanged then
    if Visible then
      TabShow()
    else
      TabHide();
end;

procedure TSimbaTab.TabShow;
begin

end;

procedure TSimbaTab.TabHide;
begin

end;

function TSimbaTabControl.GetActiveTab: TSimbaTab;
begin
  Result := GetTabByIndex(FTabs.TabIndex);
end;

function TSimbaTabControl.GetTabByIndex(Index: Integer): TSimbaTab;
begin
  if (FTabs.GetTabData(Index) <> nil) then
    Result := TSimbaTab(FTabs.GetTabData(Index).TabObject)
  else
    Result := nil;
end;

function TSimbaTabControl.GetTabCount: Integer;
begin
  Result := FTabs.TabCount;
end;

procedure TSimbaTabControl.SetActiveTab(Value: TSimbaTab);
begin
  if (Value <> nil) then
    FTabs.TabIndex := FTabs.FindTabByObject(Value);
end;

function TSimbaTabControl.GetCanAddTabOnDoubleClick: Boolean;
begin
  Result := FTabs.OptMouseDoubleClickPlus;
end;

function TSimbaTabControl.GetCanMoveTabs: Boolean;
begin
  Result := FTabs.OptMouseDragEnabled;
end;

procedure TSimbaTabControl.SetCanAddTabOnDoubleClick(Value: Boolean);
begin
  FTabs.OptMouseDoubleClickPlus := Value;
end;

procedure TSimbaTabControl.SetCanMoveTabs(Value: Boolean);
begin
  FTabs.OptMouseDragEnabled := Value;
end;

function TSimbaTabControl.GetShowCloseButtons: Boolean;
begin
  Result := FTabs.OptShowXButtons = atbxShowAll;
end;

procedure TSimbaTabControl.SetShowCloseButtons(Value: Boolean);
begin
  if Value then
    FTabs.OptShowXButtons := atbxShowAll
  else
    FTabs.OptShowXButtons := atbxShowNone;
end;

function TSimbaTabControl.GetIsClickingCloseButton: Boolean;
begin
  Result := FTabs.IsClickingCloseButton;
end;

function TSimbaTabControl.GetOnMouseDown: TMouseEvent;
begin
  Result := FTabs.OnMouseDown;
end;

function TSimbaTabControl.GetOnMouseLeave: TNotifyEvent;
begin
  Result := FTabs.OnMouseLeave;
end;

function TSimbaTabControl.GetOnMouseMove: TMouseMoveEvent;
begin
  Result := FTabs.OnMouseMove;
end;

procedure TSimbaTabControl.SetOnMouseDown(Value: TMouseEvent);
begin
  FTabs.OnMouseDown := Value;
end;

procedure TSimbaTabControl.SetOnMouseLeave(Value: TNotifyEvent);
begin
  FTabs.OnMouseLeave := Value;
end;

procedure TSimbaTabControl.SetOnMouseMove(Value: TMouseMoveEvent);
begin
  FTabs.OnMouseMove := Value;
end;

function TSimbaTabControl.GetTabHeight: Integer;
begin
  with TBitmap.Create() do
  try
    // Measure on larger font size
    // Font size can be 0 so use GetFontData
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(-GetFontData(Canvas.Font.Reference.Handle).Height * 72 / Canvas.Font.PixelsPerInch) + 4;

    Result := Canvas.TextHeight('TaylorSwift');
  finally
    Free();
  end;
end;

procedure TSimbaTabControl.ShowControl(AControl: TControl);
var
  I, TabIndex: Integer;
begin
  if (AControl is TSimbaTab) and ContainsControl(AControl) then
  begin
    for I := 0 to ControlCount - 1 do
      if (Controls[I] is TSimbaTab) and Controls[I].Visible then
        Controls[I].Visible := False;
    AControl.Visible := True;

    TabIndex := FTabs.FindTabByObject(AControl);
    if (TabIndex > -1) then
      FTabs.TabIndex := TabIndex;
  end;
end;

procedure TSimbaTabControl.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FTabs <> nil) then
  begin
    FTabs.Height := GetTabHeight();
    FTabs.OptTabHeight := FTabs.Height;
  end;
end;

procedure TSimbaTabControl.Paint;
begin
  Canvas.Brush.Color := clHighlight;
  Canvas.FillRect(0, FTabs.Height, Width, FTabs.Height + FTabs.BorderSpacing.Bottom);
end;

procedure TSimbaTabControl.DoTabMoved(Sender: TObject; AIndexFrom, AIndexTo: Integer);
begin
  if (AIndexFrom = -1) or (AIndexTo = -1) then
    Exit;

  if Assigned(FTabMovedEvent) then
    FTabMovedEvent(Self, AIndexFrom, AIndexTo);
end;

procedure TSimbaTabControl.DoTabChanged(Sender: TObject);
var
  NewTab: TSimbaTab;
begin
  NewTab := GetTabByIndex(FTabs.TabIndex);
  if Assigned(NewTab) then
    NewTab.Show();
end;

procedure TSimbaTabControl.DoTabPlusClick(Sender: TObject);
begin
  AddTab();
end;

procedure TSimbaTabControl.DoTabClose(Sender: TObject; ATabIndex: Integer; var ACanClose, ACanContinue: Boolean);
var
  Tab: TSimbaTab;
begin
  Tab := GetTabByIndex(ATabIndex);

  if Assigned(Tab) then
  begin
    if Assigned(FTabCloseEvent) then
    begin
      FTabCloseEvent(Self, Tab, ACanClose);

      ACanContinue := ACanClose;
    end;

    if ACanClose then
      Tab.Free();
  end;
end;

procedure TSimbaTabControl.DoTabRightClick(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  PressedX: Boolean;
  TabIndex: Integer;
begin
  if Assigned(PopupMenu) then
  begin
    TabIndex := FTabs.GetTabAt(MousePos.X, MousePos.Y, PressedX);
    if (TabIndex >= 0) then
      with FTabs.ClientToScreen(MousePos) do
        PopupMenu.PopUp(X, Y);
  end;

  Handled := True;
end;

procedure TSimbaTabControl.DoTabChangeQuery(Sender: TObject; ANewTabIndex: Integer; var ACanChange: Boolean);
begin
  if Assigned(FTabCanChangeEvent) then
    FTabCanChangeEvent(Self, GetTabByIndex(FTabs.TabIndex), GetTabByIndex(ANewTabIndex), ACanChange);
end;

constructor TSimbaTabControl.Create(AOwner: TComponent; TabClass: TSimbaTabClass; ADefaultTitle: String);
begin
  inherited Create(AOwner);

  FDefaultTitle := ADefaultTitle;
  FTabClass := TabClass;

  FTabs := TATTabs.Create(Self);
  FTabs.Parent := Self;
  FTabs.Align := alTop;
  FTabs.Height := GetTabHeight();
  FTabs.OnTabChanged := @DoTabChanged;
  FTabs.OnTabPlusClick := @DoTabPlusClick;
  FTabs.OnTabMove := @DoTabMoved;
  FTabs.OnTabClose := @DoTabClose;
  FTabs.OnTabChangeQuery := @DoTabChangeQuery;
  FTabs.OnContextPopup := @DoTabRightClick;
  FTabs.ColorFont := clWhite;
  FTabs.Images := SimbaForm.Images;
  FTabs.BorderSpacing.Bottom := 5;

  FTabs.OptSpaceBeforeText := 10;
  FTabs.OptSpaceAfterText := 10;
  FTabs.OptShowPlusTab := False;
  FTabs.OptVarWidth := True;
  FTabs.OptShowScrollMark := False;
  FTabs.OptSpacer := 0;
  FTabs.OptMouseDoubleClickPlus := True;
  FTabs.OptActiveVisibleOnResize := False;
  FTabs.OptSpaceBetweenIconCaption := 2;
  FTabs.OptTabHeight := FTabs.Height;
  FTabs.OptShowFlat := True;

  FTabs.ColorBg := $4A4136;
  FTabs.ColorTabPassive := $4A4136;
  FTabs.ColorTabOver := clHighlight;
  FTabs.ColorSeparator := clWhite;
  FTabs.ColorTabActive := clHighlight;
  FTabs.ColorActiveMark := clHighlight;
  FTabs.ColorCloseBgOver := clNone;
end;

function TSimbaTabControl.AddTab(Title: String): TSimbaTab;
begin
  Result := FTabClass.Create(Self);
  Result.Parent := Self;

  Result.Anchors := [akTop,akBottom, akLeft, akRight];
  Result.AnchorSide[akTop].Control := FTabs;
  Result.AnchorSide[akTop].Side := asrBottom;
  Result.AnchorSide[akBottom].Control := Self;
  Result.AnchorSide[akBottom].Side := asrBottom;
  Result.AnchorSide[akRight].Control := Self;
  Result.AnchorSide[akRight].Side := asrRight;

  FTabs.AddTab(FTabs.TabCount, IfThen(Title <> '', Title, FDefaultTitle), Result).TabPopupMenu := PopupMenu;
  FTabs.TabIndex := FTabs.TabCount - 1;
end;

procedure TSimbaTabControl.MoveTab(AFrom, ATo: Integer);
begin
  FTabs.MoveTab(AFrom, ATo, True);
end;

function TSimbaTabControl.InEmptySpace(X, Y: Integer): Boolean;
var
  PressedX: Boolean;
begin
  Result := FTabs.GetTabAt(X, Y, PressedX) = cTabIndexEmptyArea;
end;

function TSimbaTabControl.DeleteTab(Tab: TSimbaTab): Boolean;
begin
  Result := FTabs.DeleteTab(FTabs.FindTabByObject(Tab), True, True);
end;

function TSimbaTabControl.GetTabAt(X, Y: Integer): TSimbaTab;
var
  PressedX: Boolean;
begin
  Result := GetTabByIndex(FTabs.GetTabAt(X, Y, PressedX));
end;

end.
