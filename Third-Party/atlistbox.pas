{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATListbox;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  {$ifdef FPC}
  LMessages,
  {$else}
  Messages, Windows, System.UITypes,
  {$endif}
  StrUtils,
  ATScrollBar,
  ATCanvasPrimitives;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect) of object;
  TATListboxCalcWidth = function (Sender: TObject; C: TCanvas): integer of object;
  TATListboxClickHeaderEvent = procedure(Sender: TObject; AColumn: integer) of object;

type
  TATIntArray = array of integer;

type
  TATListboxScrollStyle = (
    alssHide,
    alssShow,
    alssAuto
    );

type
  { TATListboxItemProp }

  TATListboxItemProp = class
  public
    Tag: Int64;
    Modified: boolean;
    DataText: string;
    constructor Create(const ATag: Int64; AModified: boolean; const ADataText: string);
  end;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FScrollbar: TATScrollbar;
    FScrollbarHorz: TATScrollbar;
    FScrollStyleHorz: TATListboxScrollStyle;
    FScrollStyleVert: TATListboxScrollStyle;
    FOwnerDrawn: boolean;
    FVirtualMode: boolean;
    FVirtualItemCount: integer;
    FItemIndex: integer;
    FItemHeightPercents: integer;
    FItemHeight: integer;
    FItemHeightIsFixed: boolean;
    FItemTop: integer;
    FHeaderImages: TImageList;
    FScrollHorz: integer;
    FBitmap: Graphics.TBitmap;
    FBorderVisible: boolean;
    FCanGetFocus: boolean;
    FList: TStringList;
    FHotTrack: boolean;
    FHotTrackIndex: integer;
    FIndentLeft: integer;
    FIndentTop: integer;
    FColumnSep: char;
    FColumnSizes: TATIntArray;
    FColumnWidths: TATIntArray;
    FHeaderImageIndexes: TATIntArray;
    FHeaderText: string;
    FClientOriginY: integer;
    FClientWidth: integer;
    FClientHeight: integer;
    FMaxWidth: integer;
    FOnDrawItem: TATListboxDrawItemEvent;
    FOnCalcScrollWidth: TATListboxCalcWidth;
    FOnClickHeader: TATListboxClickHeaderEvent;
    FOnChangeSel: TNotifyEvent;
    FOnScroll: TNotifyEvent;

    FColorFontListbox: TColor;
    FColorFontListboxSel: TColor;
    FColorFontListboxHeader: TColor;
    FColorBgListbox: TColor;
    FColorBgListboxSel: TColor;
    FColorBgListboxHottrack: TColor;
    FColorBgListboxHeader: TColor;
    FColorListboxBorderPassive: TColor;
    FColorListboxBorderFocused: TColor;
    FColorSeparators: TColor;

    function ShowColumns: boolean;
    procedure DoPaintTo(C: TCanvas);
    function GetMaxWidth(C: TCanvas): integer;
    function GetOnDrawScrollbar: TATScrollbarDrawEvent;
    function ItemBottom: integer;
    procedure ScrollbarChange(Sender: TObject);
    procedure ScrollbarHorzChange(Sender: TObject);
    procedure SetCanBeFocused(AValue: boolean);
    procedure SetItemHeightPercents(AValue: integer);
    procedure SetOnDrawScrollbar(AValue: TATScrollbarDrawEvent);
    procedure SetScrollHorz(AValue: integer);
    procedure SetVirtualItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure SetItemHeight(AValue: integer);
    procedure UpdateClientSizes;
    procedure UpdateColumnWidths;
    procedure UpdateFromScrollbarMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMVScroll{$endif});
    procedure UpdateFromScrollbarHorzMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMHScroll{$endif});
    {$ifndef FPC}
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
    procedure UpdateScrollbars(C: TCanvas);
    function GetVisibleItems: integer;
    function GetItemHeightDefault: integer;
    function GetColumnWidth(AIndex: integer): integer;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure Resize; override;
    procedure DoExit; override;
    {$ifdef FPC}
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure LMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure MouseLeave; override;
    {$else}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    {$endif}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure ChangedSelection; virtual;
    procedure Scrolled; virtual;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ColorFontListbox: TColor read FColorFontListbox write FColorFontListbox;
    property ColorFontListboxSel: TColor read FColorFontListboxSel write FColorFontListboxSel;
    property ColorFontListboxHeader: TColor read FColorFontListboxHeader write FColorFontListboxHeader;
    property ColorBgListbox: TColor read FColorBgListbox write FColorBgListbox;
    property ColorBgListboxSel: TColor read FColorBgListboxSel write FColorBgListboxSel;
    property ColorBgListboxHottrack: TColor read FColorBgListboxHottrack write FColorBgListboxHottrack;
    property ColorBgListboxHeader: TColor read FColorBgListboxHeader write FColorBgListboxHeader;
    property ColorListboxBorderPassive: TColor read FColorListboxBorderPassive write FColorListboxBorderPassive;
    property ColorListboxBorderFocused: TColor read FColorListboxBorderFocused write FColorListboxBorderFocused;
    property ColorSeparators: TColor read FColorSeparators write FColorSeparators;

    property Items: TStringList read FList;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemTop: integer read FItemTop write SetItemTop;
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property ItemHeightDefault: integer read GetItemHeightDefault;
    function ItemCount: integer;
    function IsIndexValid(AValue: integer): boolean;
    property ClientWidth: integer read FClientWidth write FClientWidth;
    property ClientHeight: integer read FClientHeight write FClientHeight;
    property ScrollHorz: integer read FScrollHorz write SetScrollHorz;
    property HotTrackIndex: integer read FHotTrackIndex;
    property VirtualItemCount: integer read FVirtualItemCount write SetVirtualItemCount;
    property VisibleItems: integer read GetVisibleItems;
    function GetItemIndexAt(Pnt: TPoint): integer;
    function GetColumnIndexAt(Pnt: TPoint): integer;
    property Scrollbar: TATScrollbar read FScrollbar;
    property ScrollbarHorz: TATScrollbar read FScrollbarHorz;
    property ColumnSeparator: char read FColumnSep write FColumnSep;
    property ColumnSizes: TATIntArray read FColumnSizes write FColumnSizes;
    property ColumnWidth[AIndex: integer]: integer read GetColumnWidth;
    property HeaderText: string read FHeaderText write FHeaderText;
    property HeaderImages: TImageList read FHeaderImages write FHeaderImages;
    property HeaderImageIndexes: TATIntArray read FHeaderImageIndexes write FHeaderImageIndexes;
    {$ifdef FPC}
    function CanFocus: boolean; override;
    function CanSetFocus: boolean; override;
    {$endif}
    procedure Invalidate; override;
    procedure UpdateItemHeight;
    procedure DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
  published
    property Align;
    property Anchors;
    property BorderVisible: boolean read FBorderVisible write FBorderVisible default false;
    {$ifdef FPC}
    property BorderStyle;
    property BorderSpacing;
    {$endif}
    property CanGetFocus: boolean read FCanGetFocus write SetCanBeFocused default false;
    property DoubleBuffered stored false;
    property Enabled;
    property HotTrack: boolean read FHotTrack write FHotTrack default false;
    property IndentLeft: integer read FIndentLeft write FIndentLeft default 4;
    property IndentTop: integer read FIndentTop write FIndentTop default 2;
    property ItemHeightPercents: integer read FItemHeightPercents write SetItemHeightPercents default 100;
    property OwnerDrawn: boolean read FOwnerDrawn write FOwnerDrawn default false;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollStyleHorz: TATListboxScrollStyle read FScrollStyleHorz write FScrollStyleHorz default alssAuto;
    property ScrollStyleVert: TATListboxScrollStyle read FScrollStyleVert write FScrollStyleVert default alssShow;
    property ShowHint;
    property VirtualMode: boolean read FVirtualMode write FVirtualMode default true;
    property Visible;
    property OnClick;
    property OnClickHeader: TATListboxClickHeaderEvent read FOnClickHeader write FOnClickHeader;
    property OnDblClick;
    property OnContextPopup;
    property OnChangedSel: TNotifyEvent read FOnChangeSel write FOnChangeSel;
    property OnDrawItem: TATListboxDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnCalcScrollWidth: TATListboxCalcWidth read FOnCalcScrollWidth write FOnCalcScrollWidth;
    property OnDrawScrollbar: TATScrollbarDrawEvent read GetOnDrawScrollbar write SetOnDrawScrollbar;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnResize;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;


implementation

uses
  {$ifdef FPC}
  Types,
  InterfaceBase,
  LCLType, LCLIntf,
  {$endif}
  Math;

type
  TATStringSeparator = record
  private
    FSep: char;
    FStr: string;
    FPos: integer;
  public
    procedure Init(const AStr: string; ASep: char);
    function GetItemStr(out AValue: string): boolean;
  end;

procedure TATStringSeparator.Init(const AStr: string; ASep: char);
begin
  FStr:= AStr;
  FSep:= ASep;
  FPos:= 1;
end;

function TATStringSeparator.GetItemStr(out AValue: string): boolean;
var
  N: integer;
begin
  if FPos>Length(FStr) then
  begin
    AValue:= '';
    exit(false);
  end;
  N:= PosEx(FSep, FStr, FPos);
  if N=0 then
    N:= Length(FStr)+1;
  AValue:= Copy(FStr, FPos, N-FPos);
  FPos:= N+1;
  Result:= true;
end;

function IsDoubleBufferedNeeded: boolean;
begin
  Result := true;
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$endif}
end;

{ TATListboxItemProp }

constructor TATListboxItemProp.Create(const ATag: Int64; AModified: boolean;
  const ADataText: string);
begin
  Tag:= ATag;
  Modified:= AModified;
  DataText:= ADataText;
end;

{ TATListbox }

function TATListbox.GetVisibleItems: integer;
begin
  Result:= (ClientHeight-FClientOriginY) div FItemHeight;
end;

function TATListbox.IsIndexValid(AValue: integer): boolean;
begin
  Result:= (AValue>=0) and (AValue<ItemCount);
end;

function TATListbox.GetItemHeightDefault: integer;
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Result := Canvas.TextHeight('Fj') + 4;
  finally
    Free();
  end;
end;

procedure TATListbox.UpdateItemHeight;
begin
  if not FItemHeightIsFixed then
    FItemHeight:= GetItemHeightDefault * FItemHeightPercents div 100;
end;

procedure TATListbox.ChangedSelection;
begin
  if Assigned(FOnChangeSel) then
    FOnChangeSel(Self);
end;

procedure TATListbox.Scrolled;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATListbox.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  {$ifdef FPC}
  if not IsEnabled then //prevent popup menu if form is disabled, needed for CudaText plugins dlg_proc API on Qt5
  begin
    Handled:= true;
    exit;
  end;
  {$endif}

  //must select item under mouse cursor
  ItemIndex:= GetItemIndexAt(MousePos);

  inherited;
end;

function TATListbox.GetMaxWidth(C: TCanvas): integer;
var
  i: integer;
begin
  Result:= 0;

  if FVirtualMode then
  begin
    if Assigned(FOnCalcScrollWidth) then
      Result:= FOnCalcScrollWidth(Self, C);
  end
  else
  if ShowColumns then
  begin
    for i:= 0 to High(FColumnWidths) do
      Inc(Result, FColumnWidths[i]);
  end
  else
  begin
    for i:= 0 to Min(ItemCount, Items.Count)-1 do
      Result:= Max(Result, C.TextWidth(Items[i]));
    Inc(Result, FIndentLeft+2);
  end;
end;

procedure TATListbox.UpdateScrollbars(C: TCanvas);
var
  NeedVertBar, NeedHorzBar: boolean;
begin
  if FScrollStyleHorz in [alssShow, alssAuto] then
    FMaxWidth:= GetMaxWidth(C)
  else
    FMaxWidth:= 10;

  case FScrollStyleVert of
    alssAuto:
      NeedVertBar:= ItemCount*ItemHeight>Height; //not ClientHeight
    alssShow:
      NeedVertBar:= true;
    alssHide:
      NeedVertBar:= false;
  end;

  case FScrollStyleHorz of
    alssAuto:
      NeedHorzBar:= FMaxWidth>Width; //not ClientWidth
    alssShow:
      NeedHorzBar:= true;
    alssHide:
      NeedHorzBar:= false;
  end;

  FScrollbar.Visible     := NeedVertBar;
  FScrollbarHorz.Visible := NeedHorzBar;

  if FScrollbar.Visible then
  begin
    FScrollbar.Min:= 0;
    FScrollbar.Max:= ItemCount;
    FScrollbar.PageSize:= VisibleItems;
    FScrollbar.Position:= ItemTop;
    FScrollbar.Update;
  end;

  if FScrollbarHorz.Visible then
  begin
    if FScrollbar.Visible then
      FScrollbarHorz.IndentCorner:= 100
    else
      FScrollbarHorz.IndentCorner:= 0;

    FScrollbarHorz.Min:= 0;
    FScrollbarHorz.Max:= FMaxWidth;
    FScrollbarHorz.PageSize:= ClientWidth;
    FScrollbarHorz.Position:= Max(0, ScrollHorz);
    FScrollbarHorz.Update;
  end;
end;

function TATListbox.ItemCount: integer;
begin
  if FVirtualMode then
    Result:= FVirtualItemCount
  else
    Result:= Items.Count;
end;

procedure TATListbox.DoPaintTo(C: TCanvas);
var
  Index: integer;
  R: TRect;
  W, H, CurTopItem: integer;
begin
  W:= ClientWidth;
  H:= Height; //better ClientHeight, but it gave issue CudaText #4281 on macOS

  C.Font := Font;
  C.Brush.Color:= ColorToRGB(FColorBgListbox);
  C.FillRect(Rect(0, 0, W, H));

  if FHeaderText<>'' then
    FClientOriginY:= FItemHeight
  else
    FClientOriginY:= 0;

  if FHeaderText<>'' then
  begin
    r.Top:= 0;
    r.Bottom:= FItemHeight;
    r.Left:= 0;
    r.Right:= W;

    C.Font.Style:= [];
    DoDefaultDrawItem(C, -1, r);

    C.Pen.Color := FColorSeparators;
    C.MoveTo(0, FItemHeight-1);
    C.LineTo(W, FItemHeight-1);
  end;

  //adjust ItemTop, to not leave empty space on bottom
  FItemTop:= Max(0, Min(FItemTop,
    ItemCount - ClientHeight div FItemHeight + 1));

  CurTopItem:= FItemTop;
  for Index:= CurTopItem to ItemCount-1 do
  begin
    r.Top:= (Index-CurTopItem)*FItemHeight + FClientOriginY;
    r.Bottom:= r.Top+FItemHeight;
    r.Left:= 0;
    r.Right:= W;
    if r.Top>=H then Break;

    if FOwnerDrawn then
    begin
      if Assigned(FOnDrawItem) then
        FOnDrawItem(Self, C, Index, r);
    end
    else
    begin
      DoDefaultDrawItem(C, Index, r);
    end;
  end;

  if FBorderVisible then
  begin
    if Focused then
      C.Brush.Color:= FColorListboxBorderFocused
    else
      C.Brush.Color:= FColorListboxBorderPassive;

    C.FrameRect(Rect(0, 0, W, H));
  end;
end;

function TATListbox.GetOnDrawScrollbar: TATScrollbarDrawEvent;
begin
  Result:= FScrollbar.OnOwnerDraw;
end;

function TATListbox.GetColumnWidth(AIndex: integer): integer;
begin
  if (AIndex>=0) and (AIndex<Length(FColumnWidths)) then
    Result:= FColumnWidths[AIndex]
  else
    Result:= 0;
end;

procedure TATListbox.UpdateColumnWidths;
var
  NTotalWidth, NTotalWidthEx,
  NAutoSized,
  NSize, NFixedSize, i: integer;
begin
  NTotalWidth:= ClientWidth;
  NTotalWidthEx:= NTotalWidth;
  NAutoSized:= 0;
  NFixedSize:= 0;

  SetLength(FColumnWidths, Length(FColumnSizes));

  for i:= 0 to High(FColumnSizes) do
    if FColumnSizes[i]>0 then
      Dec(NTotalWidthEx, FColumnSizes[i]);
  NTotalWidthEx:= Max(0, NTotalWidthEx);

  //set width of fixed columns
  for i:= 0 to High(FColumnSizes) do
  begin
    NSize:= FColumnSizes[i];

    //auto-sized?
    if NSize=0 then
      Inc(NAutoSized)
    else
    //in percents?
    if NSize<0 then
      NSize:= NTotalWidthEx * -NSize div 100;

    Inc(NFixedSize, NSize);
    FColumnWidths[i]:= NSize;
  end;

  //set width of auto-sized columns
  for i:= 0 to High(FColumnSizes) do
    if FColumnSizes[i]=0 then
      FColumnWidths[i]:= Max(0, NTotalWidth-NFixedSize) div NAutoSized;
end;

function TATListbox.ShowColumns: boolean;
begin
  Result:= Length(FColumnSizes)>0;
end;

procedure TATListbox.DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
//AIndex=-1 means 'paint header'
var
  Sep: TATStringSeparator;
  SLine, SItem: string;
  NIndentLeft, NIndentTop, NLineHeight, NIndentText,
  NColOffset, NColWidth, NAllWidth, i: integer;
begin
  if AIndex=-1 then
  begin
    C.Brush.Color := ColorToRGB(FColorBgListboxHeader);
    C.Font.Color := ColorToRGB(FColorFontListboxHeader);
  end
  else
  if AIndex=FItemIndex then
  begin
    C.Brush.Color := ColorToRGB(FColorBgListboxSel);
    C.Font.Color := ColorToRGB(FColorFontListboxSel);
  end
  else
  if FHotTrack and (AIndex=FHotTrackIndex) then
  begin
    C.Brush.Color := ColorToRGB(FColorBgListboxHottrack);
    C.Font.Color := ColorToRGB(FColorFontListbox);
  end
  else
  begin
    C.Brush.Color := ColorToRGB(FColorBgListbox);
    C.Font.Color := ColorToRGB(FColorFontListbox);
  end;
  C.FillRect(R);

  if AIndex=-1 then
    SLine:= FHeaderText
  else
  if (AIndex>=0) and (AIndex<FList.Count) then
    SLine:= FList[AIndex]
  else
    SLine:= '('+IntToStr(AIndex)+')';

  NIndentLeft:= FIndentLeft;
  NLineHeight:= C.TextHeight(SLine);
  NIndentTop:= (FItemHeight-NLineHeight) div 2;

  if not ShowColumns then
  begin
    C.TextOut(
      R.Left+NIndentLeft-ScrollHorz,
      R.Top+NIndentTop,
      SLine);
  end
  else
  begin
    NAllWidth:= ClientWidth;
    NColOffset:= R.Left+FIndentLeft-ScrollHorz;
    C.Pen.Color:= FColorSeparators;
    Sep.Init(SLine, FColumnSep);

    for i:= 0 to Min(Length(FColumnSizes), Length(FColumnWidths))-1 do
    begin
      NColWidth:= FColumnWidths[i];
      Sep.GetItemStr(SItem);

      NIndentText:= NColOffset+1+NIndentLeft;


      C.FillRect(
        Rect(NColOffset,
        R.Top,
        NAllWidth,
        R.Bottom)
        );

      if AIndex=-1 then
        if Assigned(FHeaderImages) and
          (i<=High(FHeaderImageIndexes)) and
          (FHeaderImageIndexes[i]>=0) then
        begin
          FHeaderImages.Draw(C,
            NIndentText,
            R.Top+(R.Height-FHeaderImages.Height) div 2,
            FHeaderImageIndexes[i]);
          Inc(NIndentText, FHeaderImages.Width);
        end;

      C.TextOut(
        NIndentText,
        R.Top+NIndentTop,
        SItem
        );

      Inc(NColOffset, NColWidth);
      {$ifdef FPC}
      C.Line(NColOffset-1, R.Top, NColOffset-1, R.Bottom);
      {$else}
      C.MoveTo (NColOffset-1, R.Top);
      C.LineTo (NColOffset-1, R.Bottom);
      {$endif}
    end;
  end;
end;

procedure TATListbox.UpdateClientSizes;
begin
  FClientWidth:= Width;
  if FScrollbar.Visible then
    FClientWidth:= Max(0, FClientWidth-FScrollbar.Width);

  FClientHeight:= Height;
  if FScrollbarHorz.Visible then
    FClientHeight:= Max(0, FClientHeight-FScrollbarHorz.Height);
end;

procedure TATListbox.Paint;
var
  C: TCanvas;
begin
  inherited;

  if DoubleBuffered then
    C:= FBitmap.Canvas
  else
    C:= Canvas;

  UpdateItemHeight;
  UpdateScrollbars(C);
  UpdateClientSizes;
  UpdateColumnWidths;

  if DoubleBuffered then
  begin
    DoPaintTo(C);
    Canvas.Draw(0, 0, FBitmap);
  end
  else
    DoPaintTo(C);
end;

procedure TATListbox.Click;
var
  Pnt: TPoint;
  NItem, NColumn: integer;
begin
  if FCanGetFocus then
    {$ifdef FPC}
    LCLIntf.SetFocus(Handle);
    {$else}
    SetFocus;
    {$endif}

  Pnt:= ScreenToClient(Mouse.CursorPos);
  NItem:= GetItemIndexAt(Pnt);

  if NItem=-2 then
  begin
    if Assigned(FOnClickHeader) then
    begin
      NColumn:= GetColumnIndexAt(Pnt);
      FOnClickHeader(Self, NColumn);
    end;
    exit;
  end;

  inherited; //OnClick must be after ItemIndex set
end;

procedure TATListbox.Resize;
begin
  inherited;

  //ATSynEdit has the same code
  if DoubleBuffered then
    if Assigned(FBitmap) then
      BitmapResizeBySteps(FBitmap, Width, Height);

  Invalidate;
end;

procedure TATListbox.DoExit;
begin
  inherited;
  if FBorderVisible then
    Invalidate;
end;

function TATListbox.GetColumnIndexAt(Pnt: TPoint): integer;
var
  NSize, i: integer;
begin
  Result:= -1;
  NSize:= 0;

  if not ShowColumns then
    exit;

  for i:= 0 to High(FColumnWidths) do
  begin
    if Pnt.X<NSize+FColumnWidths[i] then
    begin
      Result:= i;
      exit;
    end;
    Inc(NSize, FColumnWidths[i]);
  end;
end;

function TATListbox.GetItemIndexAt(Pnt: TPoint): integer;
begin
  if FHeaderText<>'' then
    if (Pnt.Y>=0) and (Pnt.Y<FItemHeight) then
    begin
      Result:= -2;
      exit
    end;

  Result:= -1;
  if ItemCount=0 then exit;

  Dec(Pnt.Y, FClientOriginY);

  if (Pnt.X>=0) and (Pnt.X<ClientWidth) then
  begin
    Result:= Pnt.Y div FItemHeight + FItemTop;
    if Result>=ItemCount then
      Result:= -1;
  end;
end;

function TATListbox.ItemBottom: integer;
begin
  Result:= Min(ItemCount-1, FItemTop+GetVisibleItems-1);
end;

procedure TATListbox.ScrollbarChange(Sender: TObject);
begin
  ItemTop:= Max(0, FScrollbar.Position);
end;

procedure TATListbox.ScrollbarHorzChange(Sender: TObject);
begin
  ScrollHorz:= FScrollbarHorz.Position;
end;

procedure TATListbox.SetCanBeFocused(AValue: boolean);
begin
  if FCanGetFocus=AValue then Exit;
  FCanGetFocus:= AValue;
  {$ifdef FPC}
  if AValue then
    ControlStyle:= ControlStyle-[csNoFocus]
  else
    ControlStyle:= ControlStyle+[csNoFocus];
  {$endif}
end;

procedure TATListbox.SetItemHeightPercents(AValue: integer);
begin
  if FItemHeightPercents=AValue then Exit;
  FItemHeightPercents:= AValue;
  FItemHeightIsFixed:= false;
end;

procedure TATListbox.SetOnDrawScrollbar(AValue: TATScrollbarDrawEvent);
begin
  FScrollbar.OnOwnerDraw:= AValue;
end;

procedure TATListbox.SetScrollHorz(AValue: integer);
begin
  if FScrollHorz=AValue then Exit;
  FScrollHorz:= AValue;
  FScrollbarHorz.Position:= AValue;
  Invalidate;
end;

procedure TATListbox.SetVirtualItemCount(AValue: integer);
begin
  if FVirtualItemCount=AValue then Exit;
  if AValue<0 then Exit;
  FVirtualItemCount:= AValue;
  Scrolled;
  Invalidate;
end;

procedure TATListbox.SetItemIndex(AValue: integer);
begin
  if FItemIndex=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemIndex:= AValue;

  UpdateItemHeight; //ItemHeight may be not inited
  UpdateClientSizes; //ClientHeight may be not inited

  //scroll if needed
  if FItemIndex=0 then
    FItemTop:= 0
  else
  if FItemIndex<FItemTop then
    FItemTop:= Max(0, FItemIndex)
  else
  if FItemIndex>ItemBottom then
    FItemTop:= Max(0, FItemIndex-GetVisibleItems+1);

  ChangedSelection;
  Invalidate;
end;

procedure TATListbox.SetItemTop(AValue: integer);
begin
  if FItemTop=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemTop:= AValue;
  Scrolled;
  Invalidate;
end;

procedure TATListbox.SetItemHeight(AValue: integer);
begin
  if AValue=FItemHeight then exit;
  FItemHeight:= AValue;
  FItemHeightIsFixed:= true;
end;

constructor TATListbox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle+[csOpaque] {$ifdef FPC}-[csTripleClicks]{$endif};
  DoubleBuffered:= IsDoubleBufferedNeeded;
  Width:= 180;
  Height:= 150;

  CanGetFocus:= false;
  FBorderVisible:= false;
  FList:= TStringList.Create;
  FVirtualItemCount:= 0;
  FItemIndex:= 0;
  FItemHeightPercents:= 100;
  FItemHeight:= 17;
  FItemTop:= 0;
  FHotTrackIndex:= -1;
  FScrollStyleVert:= alssShow;
  FScrollStyleHorz:= alssAuto;
  FScrollHorz:= 0;
  FIndentLeft:= 4;
  FIndentTop:= 2;
  FOwnerDrawn:= false;
  FVirtualMode:= true;
  FHotTrack:= false;
  FColumnSep:= #9;
  SetLength(FColumnSizes, 0);
  SetLength(FColumnWidths, 0);

  FBitmap:= Graphics.TBitmap.Create;
  BitmapResize(FBitmap, 800, 600);

  FScrollbar:= TATScrollbar.Create(Self);
  FScrollbar.Hide;
  FScrollbar.Parent:= Self;
  FScrollbar.Kind:= sbVertical;
  FScrollbar.Align:= alRight;
  FScrollbar.OnChange:= ScrollbarChange;

  FScrollbarHorz:= TATScrollbar.Create(Self);
  FScrollbarHorz.Hide;
  FScrollbarHorz.Parent:= Self;
  FScrollbarHorz.Kind:= sbHorizontal;
  FScrollbarHorz.Align:= alBottom;
  FScrollbarHorz.IndentCorner:= 100;
  FScrollbarHorz.OnChange:= ScrollbarHorzChange;

  FColorFontListbox := $303030;
  FColorFontListboxSel := clWhite;
  FColorFontListboxHeader := $303030;

  FColorBgListbox := $e0e0e0;
  FColorBgListboxSel := clMedGray;
  FColorBgListboxHottrack := clMoneyGreen;
  FColorBgListboxHeader := $80aa80;

  FColorListboxBorderPassive := $a0a0a0;
  FColorListboxBorderFocused := clNavy;

  FColorSeparators := clDkGray;
end;

destructor TATListbox.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATListbox.UpdateFromScrollbarMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMVScroll{$endif});
var
  NMax: integer;
begin
  NMax:= Max(0, ItemCount-GetVisibleItems);

  case Msg.ScrollCode of
    SB_TOP:
      FItemTop:= 0;
    SB_BOTTOM:
      FItemTop:= NMax;

    SB_LINEUP:
      FItemTop:= FItemTop-1;
    SB_LINEDOWN:
      FItemTop:= Min(NMax, FItemTop+1);

    SB_PAGEUP:
      FItemTop:= FItemTop-GetVisibleItems;
    SB_PAGEDOWN:
      FItemTop:= Min(NMax, FItemTop+GetVisibleItems);

    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      FItemTop:= Msg.Pos;
  end;

  //limit by 0
  FItemTop:= Max(0, FItemTop);
end;

procedure TATListbox.UpdateFromScrollbarHorzMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMHScroll{$endif});
var
  NMax: integer;
begin
  NMax:= Max(0, FMaxWidth-ClientWidth);

  case Msg.ScrollCode of
    SB_TOP:        FScrollHorz:= 0;
    SB_BOTTOM:     FScrollHorz:= NMax;

    SB_LINEUP:     FScrollHorz:= Max(0, FScrollHorz-1);
    SB_LINEDOWN:   FScrollHorz:= Min(NMax, FScrollHorz+1);

    SB_PAGEUP:     FScrollHorz:= Max(0, FScrollHorz-ClientWidth);
    SB_PAGEDOWN:   FScrollHorz:= Min(NMax, FScrollHorz+ClientWidth);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: FScrollHorz:= Max(0, Msg.Pos);
  end;
end;

{$ifndef FPC}
procedure TATListbox.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TATListbox.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TATListbox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
  if Assigned(FScrollbar) then
  if not MouseInClient then
    FScrollbar.Refresh;
end;
{$endif}

{$ifdef FPC}
procedure TATListbox.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

procedure TATListbox.LMHScroll(var Msg: TLMHScroll);
begin
  UpdateFromScrollbarHorzMsg(Msg);
  Invalidate;
end;
{$endif}

{$ifndef FPC}
procedure TATListbox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if (csCreating in ControlState) then exit;
  Invalidate;
end;

procedure TATListbox.WMVScroll(var Msg: TWMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

procedure TATListbox.WMHScroll(var Msg: TWMHScroll);
begin
  UpdateFromScrollbarHorzMsg(Msg);
  Invalidate;
end;

procedure TATListbox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
 inherited;
 Message.Result:= Message.Result or DLGC_WANTARROWS;
end;

procedure TATListbox.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
begin

 { Check the ShiftState, like delphi does while processing WMKeyDown }
 ShiftState := KeyDataToShiftState(Message.KeyData);
 DoKeyDown(Message.CharCode,ShiftState);

 inherited;

end;

{$endif}

{$ifdef FPC}
function TATListbox.CanFocus: boolean;
begin
  Result:= FCanGetFocus;
end;
{$endif}

{$ifdef FPC}
function TATListbox.CanSetFocus: boolean;
begin
  Result:= FCanGetFocus;
end;
{$endif}

procedure TATListbox.Invalidate;
{$ifndef FPC}
var
  R: TRect;
{$endif}
begin
  {$ifdef FPC}
  inherited Invalidate;
  {$else}
  // https://github.com/Alexey-T/ATFlatControls/issues/32
  if (Assigned(FScrollbar) and FScrollbar.Visible) or
  (Assigned(FScrollbarHorz) and FScrollbarHorz.Visible) then
  begin
    R:= Rect(0, 0, ClientWidth, ClientHeight);
    InvalidateRect(Handle, R, false);
  end
  else
    inherited Invalidate;
  {$endif}
end;

function TATListbox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  NDelta: integer;
begin
  Result:= true;

  if ssShift in Shift then
  begin
    NDelta:= FScrollbarHorz.PageSize div 2;
    if WheelDelta>0 then
      ScrollHorz:= Max(0, ScrollHorz-NDelta)
    else
      ScrollHorz:= Min(Max(0, FScrollbarHorz.Max-FScrollbarHorz.PageSize), ScrollHorz+NDelta);
  end
  else
  begin
    if WheelDelta>0 then
      ItemTop:= Max(0, ItemTop-Mouse.WheelScrollLines)
    else
      ItemTop:= Max(0, Min(ItemCount-VisibleItems, ItemTop+Mouse.WheelScrollLines));
  end;
end;

procedure TATListbox.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_UP) and (Shift=[]) then
  begin
    ItemIndex:= ItemIndex-1;
    key:= 0;
    Exit
  end;
  if (Key=VK_DOWN) and (Shift=[]) then
  begin
    ItemIndex:= ItemIndex+1;
    key:= 0;
    Exit
  end;

  if (Key=VK_PRIOR) and (Shift=[]) then
  begin
    ItemIndex:= Max(0, ItemIndex-(VisibleItems-1));
    key:= 0;
    Exit
  end;
  if (Key=VK_NEXT) and (Shift=[]) then
  begin
    ItemIndex:= Min(ItemCount-1, ItemIndex+(VisibleItems-1));
    key:= 0;
    Exit
  end;

  if (Key=VK_HOME) and (Shift=[]) then
  begin
    ItemIndex:= 0;
    key:= 0;
    Exit
  end;
  if (Key=VK_END) and (Shift=[]) then
  begin
    ItemIndex:= ItemCount-1;
    key:= 0;
    Exit
  end;

  if (Key=VK_RETURN) and (Shift=[]) then
  begin
    DblClick;
    key:= 0;
    Exit
  end;
end;

procedure TATListbox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  DoKeyDown(Key, Shift);
end;

procedure TATListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewIndex: integer;
begin
  inherited;

  if FHotTrack then
  begin
    NewIndex:= GetItemIndexAt(Point(X, Y));
    if (FHotTrackIndex<>NewIndex) then
    begin
      FHotTrackIndex:= NewIndex;
      Invalidate;
    end;
  end
  else
    FHotTrackIndex:= -1;
end;

procedure TATListbox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  ItemIndex:= GetItemIndexAt(Point(X, Y));
  inherited;
end;

{$ifdef fpc}
procedure TATListbox.MouseLeave;
begin
  inherited;
  if FHotTrack then
  begin
    FHotTrackIndex:= -1;
    Invalidate;
  end;
end;
{$endif}

initialization

end.

