{
ATTabs component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVviewsoft.com)
License: MPL 2.0 or LGPL
}

//{$define tabs_paint_counter}

unit attabs;

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
  {$ifdef VER150} //Delphi 7
    {$define WIDE}
  {$endif}
{$endif}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, Types, Graphics,
  Controls, Messages, ImgList,
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  LCLProc,
  {$else}
  System.UITypes,
  {$endif}
  ATFlatControls_Separator,
  ATCanvasPrimitives,
  Menus;

type
  TATTabString = {$ifdef WIDE} WideString {$else} string {$endif};

const
  cRect0: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

//these global options disable features in all ATTabs objects
var
  ATTabsStretchDrawEnabled: boolean = true;
  ATTabsCircleDrawEnabled: boolean = true;
  ATTabsPixelsDrawEnabled: boolean = true;
  ATTabsAddonSeparator: string = ' • ';

type
  TATTabPosition = (
    atpTop,
    atpBottom,
    atpLeft,
    atpRight
    );

type

  { TATTabListCollection }

  TATTabListCollection = class(TCollection)
  public
    AOwner: TCustomControl;
    destructor Destroy; override;
    procedure Clear;
  end;

type
  { TATTabData }

  TATTabData = class(TCollectionItem)
  private
    FTabCaption: TATTabString;
    FTabCaptionAddon: TATTabString;
    FTabCaptionRect: TRect;
    FTabHint: TATTabString;
    FTabObject: TObject;
    FTabColor: TColor;
    FTabColorActive: TColor;
    FTabColorOver: TColor;
    FTabFontColor: TColor;
    FTabModified: boolean;
    FTabModified2: boolean;
    FTabExtModified: boolean;
    FTabExtModified2: boolean;
    FTabExtDeleted: boolean;
    FTabExtDeleted2: boolean;
    FTabTwoDocuments: boolean;
    FTabSpecial: boolean;
    FTabSpecialWidth: integer;
    FTabSpecialHeight: integer;
    FTabRect: TRect;
    FTabRectX: TRect;
    FTabImageIndex: TImageIndex;
    FTabPopupMenu: TPopupMenu;
    FTabFontStyle: TFontStyles;
    FTabStartsNewLine: boolean;
    FTabHideXButton: boolean;
    FTabVisible: boolean;
    FTabVisibleX: boolean;
    FTabPinned: boolean;
    FTag: NativeInt; //PtrInt not exists in Delphi
    function GetTabCaptionFull: TATTabString;
    procedure UpdateTabSet;
    procedure SetTabImageIndex(const Value: TImageIndex);
    procedure SetTabCaption(const Value: TATTabString);
    procedure SetTabColor(const Value: TColor);
    procedure SetTabColorActive(const Value: TColor);
    procedure SetTabColorOver(const Value: TColor);
    procedure SetTabFontColor(const Value: TColor);
    procedure SetTabHideXButton(const Value: boolean);
    procedure SetTabVisible(const Value: boolean);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Clear;
    property TabCaptionFull: TATTabString read GetTabCaptionFull;
    property TabCaptionRect: TRect read FTabCaptionRect write FTabCaptionRect;
    property TabObject: TObject read FTabObject write FTabObject;
    property TabRect: TRect read FTabRect write FTabRect;
    property TabRectX: TRect read FTabRectX write FTabRectX;
    property TabSpecial: boolean read FTabSpecial write FTabSpecial;
    property TabStartsNewLine: boolean read FTabStartsNewLine write FTabStartsNewLine;
    property TabVisibleX: boolean read FTabVisibleX write FTabVisibleX;
    property Tag: NativeInt read FTag write FTag;
    procedure Assign(Source: TPersistent); override;
  published
    property TabCaption: TATTabString read FTabCaption write SetTabCaption;
    property TabCaptionAddon: TATTabString read FTabCaptionAddon write FTabCaptionAddon;
    property TabHint: TATTabString read FTabHint write FTabHint;
    property TabColor: TColor read FTabColor write SetTabColor default clNone;
    property TabColorActive: TColor read FTabColorActive write SetTabColorActive default clNone;
    property TabColorOver: TColor read FTabColorOver write SetTabColorOver default clNone;
    property TabFontColor: TColor read FTabFontColor write SetTabFontColor default clNone;
    property TabModified: boolean read FTabModified write FTabModified default false;
    property TabModified2: boolean read FTabModified2 write FTabModified2 default false;
    property TabExtModified: boolean read FTabExtModified write FTabExtModified default false;
    property TabExtModified2: boolean read FTabExtModified2 write FTabExtModified2 default false;
    property TabExtDeleted: boolean read FTabExtDeleted write FTabExtDeleted default false;
    property TabExtDeleted2: boolean read FTabExtDeleted2 write FTabExtDeleted2 default false;
    property TabTwoDocuments: boolean read FTabTwoDocuments write FTabTwoDocuments default false;
    property TabImageIndex: TImageIndex read FTabImageIndex write SetTabImageIndex default -1;
    property TabFontStyle: TFontStyles read FTabFontStyle write FTabFontStyle default [];
    property TabPopupMenu: TPopupMenu read FTabPopupMenu write FTabPopupMenu;
    property TabSpecialWidth: integer read FTabSpecialWidth write FTabSpecialWidth default 0;
    property TabSpecialHeight: integer read FTabSpecialHeight write FTabSpecialHeight default 0;
    property TabHideXButton: boolean read FTabHideXButton write SetTabHideXButton default false;
    property TabVisible: boolean read FTabVisible write SetTabVisible default true;
    property TabPinned: boolean read FTabPinned write FTabPinned default false;
  end;

type
  TATTabElemType = (
    aeBackground,
    aeSpacerRect,
    aeTabActive,
    aeTabPassive,
    aeTabPassiveOver,
    aeTabPlus,
    aeTabPlusOver,
    aeTabIconX,
    aeTabIconXOver,
    aeArrowDropdown,
    aeArrowDropdownOver,
    aeArrowScrollLeft,
    aeArrowScrollLeftOver,
    aeArrowScrollRight,
    aeArrowScrollRightOver,
    aeButtonPlus,
    aeButtonPlusOver,
    aeButtonClose,
    aeButtonCloseOver,
    aeButtonUser,
    aeButtonUserOver
    );

type
  TATTabButton = (
    atbPlus,
    atbClose,
    atbScrollLeft,
    atbScrollRight,
    atbDropdownMenu,
    atbSpace,
    atbSeparator,
    atbUser0,
    atbUser1,
    atbUser2,
    atbUser3,
    atbUser4
    );

  TATTabButtons = array of record
    Id: TATTabButton;
    Size: integer;
  end;

type
  TATTabIconPosition = (
    aipIconLefterThanText,
    aipIconRighterThanText,
    aipIconCentered,
    aipIconAboveTextCentered,
    aipIconBelowTextCentered
    );

type
  TATTabActionOnClose = (
    aocNone,
    aocDefault,
    aocRight,
    aocRecent
    );

  { TATTabPaintInfo }

  TATTabPaintInfo = record
    Rect: TRect;
    RectX: TRect;
    Caption: TATTabString;
    Modified,
    Modified2,
    ExtModified,
    ExtModified2,
    ExtDeleted,
    ExtDeleted2: boolean;
    Pinned: boolean;
    TabIndex: integer;
    ImageIndex: integer;
    ColorFont: TColor;
    TwoDocuments: boolean;
    TabActive,
    TabMouseOver,
    TabMouseOverX: boolean;
    FontStyle: TFontStyles;
    procedure Clear;
  end;

type
  TATTabOverEvent = procedure (Sender: TObject; ATabIndex: integer) of object;
  TATTabCloseEvent = procedure (Sender: TObject; ATabIndex: integer;
    var ACanClose, ACanContinue: boolean) of object;
  TATTabMenuEvent = procedure (Sender: TObject; var ACanShow: boolean) of object;
  TATTabDrawEvent = procedure (Sender: TObject;
    AElemType: TATTabElemType; ATabIndex: integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;
  TATTabMoveEvent = procedure (Sender: TObject; AIndexFrom, AIndexTo: integer) of object;
  TATTabChangeQueryEvent = procedure (Sender: TObject; ANewTabIndex: integer;
    var ACanChange: boolean) of object;
  TATTabClickUserButton = procedure (Sender: TObject; AIndex: integer) of object;
  TATTabGetTickEvent = function (Sender: TObject; ATabObject: TObject): Int64 of object;
  TATTabGetCloseActionEvent = procedure (Sender: TObject; var AAction: TATTabActionOnClose) of object;
  TATTabDblClickEvent = procedure (Sender: TObject; AIndex: integer) of object;
  TATTabDropQueryEvent = procedure (Sender: TObject; AIndexFrom, AIndexTo: integer; var ACanDrop: boolean) of object;

type
  TATTabTriangle = (
    atriDown,
    atriLeft,
    atriRight
    );

  TATTabShowClose = (
    atbxShowNone,
    atbxShowAll,
    atbxShowActive,
    atbxShowMouseOver,
    atbxShowActiveAndMouseOver
    );

  TATTabMouseWheelMode = (
    amwIgnoreWheel,
    amwNormalScroll,
    amwNormalScrollAndShiftSwitch,
    amwNormalSwitch,
    amwNormalSwitchAndShiftScroll
    );

  TATTabTheme = record
    FileName_Left: string;
    FileName_Right: string;
    FileName_Center: string;
    FileName_LeftActive: string;
    FileName_RightActive: string;
    FileName_CenterActive: string;
    FileName_X: string;
    FileName_XActive: string;
    FileName_Plus: string;
    FileName_PlusActive: string;
    FileName_ArrowLeft: string;
    FileName_ArrowLeftActive: string;
    FileName_ArrowRight: string;
    FileName_ArrowRightActive: string;
    FileName_ArrowDown: string;
    FileName_ArrowDownActive: string;
    SpaceBetweenInPercentsOfSide: integer;
    IndentOfX: integer;
  end;

//int constants for GetTabAt
const
  cTabIndexNone = -1; //none tab
  cTabIndexPlus = -2;
  cTabIndexPlusBtn = -3;
  cTabIndexCloseBtn = -4;
  cTabIndexArrowMenu = -5;
  cTabIndexArrowScrollLeft = -6;
  cTabIndexArrowScrollRight = -7;
  cTabIndexUser0 = -10;
  cTabIndexUser1 = -11;
  cTabIndexUser2 = -12;
  cTabIndexUser3 = -13;
  cTabIndexUser4 = -14;
  cTabIndexEmptyArea = -20;

const
  _InitTabColorBg = $585858;
  _InitTabColorTabActive = $808080;
  _InitTabColorTabPassive = $786868;
  _InitTabColorTabOver = $A08080;
  _InitTabColorActiveMark = $C04040;
  _InitTabColorFont = $D0D0D0;
  _InitTabColorFontModified = $A00000;
  _InitTabColorFontActive = clNone;
  _InitTabColorFontHot = clNone;
  _InitTabColorBorderActive = $A0A0A0;
  _InitTabColorBorderPassive = $A07070;
  _InitTabColorCloseBg = clNone;
  _InitTabColorCloseBgOver = $6060E0;
  _InitTabColorCloseBorderOver = _InitTabColorCloseBgOver;
  _InitTabColorCloseX = clLtGray;
  _InitTabColorCloseXOver = clWhite;
  _InitTabColorArrow = $999999;
  _InitTabColorArrowOver = $E0E0E0;
  _InitTabColorDropMark = $6060E0;
  _InitTabColorScrollMark = _InitTabColorDropMark;

const
  _InitOptTruncateCaption = acsmMiddle;
  _InitOptButtonLayout = '<>,v';
  _InitOptButtonSize = 16;
  _InitOptButtonSizeSpace = 10;
  _InitOptButtonSizeSeparator = 5;
  _InitOptTabHeight = 24;
  _InitOptTabWidthMinimal = 40;
  _InitOptTabWidthMaximal = 300;
  _InitOptTabWidthNormal = 130;
  _InitOptTabWidthMinimalHidesX = 55;
  _InitOptTabRounded = true;
  _InitOptMinimalWidthForSides = 140;
  _InitOptSpaceSide = 10;
  _InitOptSpaceInitial = 5;
  _InitOptSpaceBeforeText = 6;
  _InitOptSpaceBeforeTextForMinWidth = 30;
  _InitOptSpaceAfterText = 6;
  _InitOptSpaceBetweenTabs = 0;
  _InitOptSpaceBetweenLines = 4;
  _InitOptSpaceBetweenIconCaption = 0;
  _InitOptSpaceSeparator = 2;
  _InitOptSpacer = 4;
  _InitOptSpacer2 = 10;
  _InitOptSpaceXRight = 10;
  _InitOptSpaceXInner = 3;
  _InitOptSpaceXSize = 12;
  _InitOptSpaceXIncrementRound = 1;
  _InitOptSpaceModifiedCircle = 5;
  _InitOptArrowSize = 4;
  _InitOptArrowSpaceLeft = 4;
  _InitOptColoredBandSize = 4;
  _InitOptActiveMarkSize = 4;
  _InitOptScrollMarkSizeX = 20;
  _InitOptScrollMarkSizeY = 3;
  _InitOptScrollPagesizePercents = 20;
  _InitOptDropMarkSize = 6;
  _InitOptActiveFontStyle = [fsUnderline];
  _InitOptActiveFontStyleUsed = false;
  _InitOptHotFontStyle = [fsUnderline];
  _InitOptHotFontStyleUsed = false;

  _InitOptShowFlat = false;
  _InitOptShowFlatMouseOver = true;
  _InitOptShowFlatSep = true;
  _InitOptShowModifiedCircle = true;
  _InitOptShowModifiedColorOnX = false;
  _InitOptPosition = atpTop;
  _InitOptFillWidth = true;
  _InitOptFillWidthLastToo = false;
  _InitOptShowNumberPrefix = '';
  _InitOptShowScrollMark = true;
  _InitOptShowDropMark = true;
  _InitOptShowArrowsNear = true;
  _InitOptShowXRounded = true;
  _InitOptShowXButtons = atbxShowAll;
  _InitOptShowPlusTab = true;
  _InitOptShowPinnedText = '!';
  _InitOptShowEntireColor = false;
  _InitOptShowActiveMarkInverted = true;
  _InitRoundedBitmapSize = 60;

  _InitOptMouseWheelMode = amwNormalScrollAndShiftSwitch;
  _InitOptMouseMiddleClickClose = false;
  _InitOptMouseDoubleClickClose = false;
  _InitOptMouseDoubleClickPlus = false;
  _InitOptMouseDragEnabled = true;
  _InitOptMouseDragOutEnabled = true;
  _InitOptMouseDragFromNotATTabs = false;

type
  { TATTabs }

  TATTabs = class(TCustomControl)
  private
    //drag-drop
    FMouseDown: boolean;
    FMouseDownPnt: TPoint;
    FMouseDownDbl: boolean;
    FMouseDownButton: TMouseButton;
    FMouseDownShift: TShiftState;
    FMouseDownRightBtn: boolean;
    FMouseDragBegins: boolean;

    //colors
    FColorBg: TColor; //color of background (visible at top and between tabs)
    FColorBorderActive: TColor; //color of 1px border of active tab
    FColorBorderPassive: TColor; //color of 1px border of inactive tabs
    FColorSeparator: TColor; //vertical lines between tabs in Flat mode
    FColorTabActive: TColor; //color of active tab
    FColorTabPassive: TColor; //color of inactive tabs
    FColorTabOver: TColor; //color of inactive tabs, mouse-over
    FColorActiveMark: TColor;
    FColorFont: TColor;
    FColorFontModified: TColor;
    FColorFontActive: TColor;
    FColorFontHot: TColor;
    FColorCloseBg: TColor; //color of small square with "x" mark, inactive
    FColorCloseBgOver: TColor; //color of small square with "x" mark, mouse-over
    FColorCloseBorderOver: TColor; //color of 1px border of "x" mark, mouse-over
    FColorCloseX: TColor; //color of "x" mark
    FColorCloseXOver: TColor; //"color of "x" mark, mouseover
    FColorArrow: TColor; //color of "down" arrow (tab menu), inactive
    FColorArrowOver: TColor; //color of "down" arrow, mouse-over
    FColorDropMark: TColor;
    FColorScrollMark: TColor;

    //opts
    FButtonsLeft: TATTabButtons;
    FButtonsRight: TATTabButtons;
    FOptButtonSize: integer;
    FOptButtonSizeSpace: integer;
    FOptButtonSizeSeparator: integer;
    FOptButtonLayout: string;

    FOptScalePercents: integer;
    FOptVarWidth: boolean;
    FOptMultiline: boolean;
    FOptTruncateCaption: TATCollapseStringMode;
    FOptFillWidth: boolean;
    FOptFillWidthLastToo: boolean;
    FOptTabHeight: integer;
    FOptTabWidthMinimal: integer; //tab minimal width (used when lot of tabs)
    FOptTabWidthMaximal: integer;
    FOptTabWidthNormal: integer; //tab maximal width (used when only few tabs)
    FOptTabWidthMinimalHidesX: integer; //tab minimal width, after which "x" mark hides for inactive tabs
    FOptTabRounded: boolean;
    FOptSpaceBetweenTabs: integer; //space between nearest tabs
    FOptSpaceBetweenLines: integer;
    FOptSpaceBetweenIconCaption: integer;
    FOptSpaceInitial: integer; //space between first tab and left control edge
    FOptSpaceBeforeText: integer; //space between text and tab left edge
    FOptSpaceBeforeTextForMinWidth: integer;
    FOptSpaceAfterText: integer; //space between text and [x] icon righter than text
    FOptSpaceSeparator: integer;
    FOptSpacer: integer; //height of top empty space (colored with bg)
    FOptSpacer2: integer;
    FOptSpaceXRight: integer; //space from "x" btn to right tab edge
    FOptSpaceXInner: integer; //space from "x" square edge to "x" mark
    FOptSpaceXSize: integer; //size of "x" mark
    FOptSpaceXIncrementRound: integer;
    FOptSpaceModifiedCircle: integer; //diameter of 'modified circle mark' above caption
    FOptColoredBandSize: integer; //height of "misc color" line
    FOptColoredBandForTop: TATTabPosition;
    FOptColoredBandForBottom: TATTabPosition;
    FOptColoredBandForLeft: TATTabPosition;
    FOptColoredBandForRight: TATTabPosition;
    FOptActiveMarkSize: integer;
    FOptArrowSize: integer; //half-size of "arrow" mark
    FOptDropMarkSize: integer;
    FOptScrollMarkSizeX: integer;
    FOptScrollMarkSizeY: integer;
    FOptScrollPagesizePercents: integer;
    FOptActiveVisibleOnResize: boolean;

    FOptPosition: TATTabPosition;
    FOptIconPosition: TATTabIconPosition;
    FOptWhichActivateOnClose: TATTabActionOnClose;
    FOptCaptionAlignment: TAlignment;
    FOptShowModifiedCircle: boolean;
    FOptShowModifiedColorOnX: boolean;
    FOptShowFlat: boolean;
    FOptShowFlatMouseOver: boolean;
    FOptShowFlatSepar: boolean;
    FOptShowXRounded: boolean;
    FOptShowXButtons: TATTabShowClose; //show mode for "x" buttons
    FOptShowArrowsNear: boolean;
    FOptShowPlusTab: boolean; //show "plus" tab
    FOptShowPinnedText: TATTabString;
    FOptShowEntireColor: boolean;
    FOptShowNumberPrefix: TATTabString;
    FOptShowScrollMark: boolean;
    FOptShowDropMark: boolean;
    FOptShowActiveMarkInverted: boolean;
    FOptActiveFontStyle: TFontStyles;
    FOptActiveFontStyleUsed: boolean;
    FOptHotFontStyle: TFontStyles;
    FOptHotFontStyleUsed: boolean;

    FOptMouseWheelMode: TATTabMouseWheelMode;
    FOptMouseMiddleClickClose: boolean; //enable close tab by middle-click
    FOptMouseDoubleClickClose: boolean;
    FOptMouseDoubleClickPlus: boolean; //enable call "+" tab with dbl-click on empty area
    FOptMouseDragEnabled: boolean; //enable drag-drop
    FOptMouseDragOutEnabled: boolean; //also enable drag-drop to another controls
    FOptMouseDragFromNotATTabs: boolean;

    //others
    FTabWidth: integer;
    FTabIndex: integer;
    FTabIndexLoaded: integer;
    FTabIndexOver: integer;
    FTabIndexDrop: integer;
    FTabIndexDropOld: integer;
    FTabIndexHinted: integer;
    FTabIndexHintedPrev: integer;
    FTabList: TATTabListCollection;
    FTabMenu: TPopupMenu;
    FMultilineActive: boolean;

    FRealIndentLeft: integer;
    FRealIndentRight: integer;
    FOptFontScale: integer;
    FOptMinimalWidthForSides: integer;
    FOptSpaceSide: integer;
    FPaintCount: integer;
    FLastOverIndex: integer;
    FLastOverX: boolean;
    FLastSpaceSide: integer;
    FActualMultiline: boolean;
    FTabsChanged: boolean;
    FTabsResized: boolean;
    FScrollingNeeded: boolean;

    FScrollPos: integer;
    FImages: TImageList;
    FBitmap: TBitmap;
    FBitmapAngleL: TBitmap;
    FBitmapAngleR: TBitmap;
    FBitmapRound: TBitmap;

    FRectTabLast_Scrolled: TRect;
    FRectTabLast_NotScrolled: TRect;
    FRectTabPlus_Scrolled: TRect; //uses scroll pos
    FRectTabPlus_NotScrolled: TRect; //ignores scroll pos, not real
    FRectArrowDown: TRect;
    FRectArrowLeft: TRect;
    FRectArrowRight: TRect;
    FRectButtonPlus: TRect;
    FRectButtonClose: TRect;
    FRectButtonUser0: TRect;
    FRectButtonUser1: TRect;
    FRectButtonUser2: TRect;
    FRectButtonUser3: TRect;
    FRectButtonUser4: TRect;

    FHintForX: string;
    FHintForPlus: string;
    FHintForArrowLeft: string;
    FHintForArrowRight: string;
    FHintForArrowMenu: string;
    FHintForUser0: string;
    FHintForUser1: string;
    FHintForUser2: string;
    FHintForUser3: string;
    FHintForUser4: string;

    FIsClickingCloseButton: Boolean;

    //events    
    FOnTabClick: TNotifyEvent;
    FOnTabChanged: TNotifyEvent;                   // aducom
    FOnTabPlusClick: TNotifyEvent;
    FOnTabClickUserButton: TATTabClickUserButton;
    FOnTabClose: TATTabCloseEvent;
    FOnTabMenu: TATTabMenuEvent;
    FOnTabDrawBefore: TATTabDrawEvent;
    FOnTabDrawAfter: TATTabDrawEvent;
    FOnTabEmpty: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;
    FOnTabChangeQuery: TATTabChangeQueryEvent;
    FOnTabGetTick: TATTabGetTickEvent;
    FOnTabGetCloseAction: TATTabGetCloseActionEvent;
    FOnTabDblClick: TATTabDblClickEvent;
    FOnTabDropQuery: TATTabDropQueryEvent;
    FOnTabDragging: TATTabDropQueryEvent;

    function ConvertButtonIdToTabIndex(Id: TATTabButton): integer; inline;
    procedure DoClickUser(AIndex: integer);
    procedure DoHandleClick;
    procedure DoHandleRightClick;
    procedure DoPaintArrowDown(C: TCanvas);
    procedure DoPaintArrowLeft(C: TCanvas);
    procedure DoPaintArrowRight(C: TCanvas);
    procedure DoPaintButtonClose(C: TCanvas);
    procedure DoPaintButtonPlus(C: TCanvas);
    procedure DoPaintButtonsBG(C: TCanvas);
    procedure DoPaintColoredBand(C: TCanvas; const ARect: TRect; AColor: TColor;
      APos: TATTabPosition);
    procedure DoPaintPlus(C: TCanvas; const ARect: TRect);
    procedure DoPaintSeparator(C: TCanvas; const R: TRect);
    procedure DoPaintSpaceInital(C: TCanvas); inline;
    procedure DoPaintSpacerRect(C: TCanvas);
    procedure DoPaintTabShape(C: TCanvas; const ATabRect: TRect;
      ATabActive: boolean; ATabIndex: integer);
    procedure DoPaintTabShape_C(C: TCanvas; ATabActive: boolean;
      ATabIndex: integer; const ARect: TRect; const PL1, PL2, PR1, PR2: TPoint);
    procedure DoPaintTabShape_L(C: TCanvas; const ARect: TRect;
      ATabActive: boolean; ATabIndex: integer);
    procedure DoPaintTabShape_R(C: TCanvas; const ARect: TRect;
      ATabActive: boolean; ATabIndex: integer);
    procedure DoPaintTo(C: TCanvas);
    procedure DoTextOut(C: TCanvas; AX, AY: integer; const AClipRect: TRect; const AText: string); inline;
    procedure DoPaintBgTo(C: TCanvas; const ARect: TRect);
    procedure DoPaintTabTo(C: TCanvas; const AInfo: TATTabPaintInfo);
    procedure DoPaintX(C: TCanvas; const AInfo: TATTabPaintInfo);
    procedure DoPaintXTo(C: TCanvas; const AInfo: TATTabPaintInfo);
    procedure DoPaintArrowTo(C: TCanvas; ATyp: TATTabTriangle; ARect: TRect; AActive, AEnabled: boolean);
    procedure DoPaintUserButtons(C: TCanvas; const AButtons: TATTabButtons; AtLeft: boolean);
    procedure DoPaintDropMark(C: TCanvas);
    procedure DoPaintScrollMark(C: TCanvas);
    procedure GetTabFirstCoord(var R: TRect);
    function GetTabCaptionFinal(AData: TATTabData; ATabIndex: integer): TATTabString;
    function GetButtonsEdgeCoord(AtLeft: boolean): integer;
    function GetButtonsWidth(const B: TATTabButtons): integer;
    function GetPositionInverted(APos: TATTabPosition): TATTabPosition; inline;
    function GetIndexOfButton(const AButtons: TATTabButtons; ABtn: TATTabButton): integer;
    function GetInitialVerticalIndent: integer; inline;
    function GetButtonsEmpty: boolean; inline;
    function GetTabBgColor_Passive(AIndex: integer): TColor;
    function GetTabBgColor_Active(AIndex: integer): TColor;
    function GetTabFlatEffective(AIndex: integer): boolean; inline;
    procedure GetTabXColors(AIndex: integer; AMouseOverX: boolean; out AColorXBg,
      AColorXBorder, AColorXMark: TColor);
    function GetScrollMarkNeeded: boolean;
    function GetMaxEdgePos: integer;
    function GetRectOfButton(AButton: TATTabButton): TRect;
    function GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
    function GetScrollPageSize: integer;
    function IsDraggingAllowed: boolean;
    procedure PaintSimulated;
    procedure SetOptButtonLayout(const AValue: string);
    procedure SetOptScalePercents(AValue: integer);
    procedure SetOptVarWidth(AValue: boolean);
    procedure SetScrollPos(AValue: integer);
    procedure SetTabIndexEx(AIndex: integer; ADisableEvent: boolean);
    procedure SetTabIndex(AIndex: integer);
    procedure GetTabXProps(AIndex: integer; const ARect: TRect; out
      AMouseOverX: boolean; out ARectX: TRect);
    function IsIndexOk(AIndex: integer): boolean; inline;
    function GetTabVisibleX(AIndex: integer; const D: TATTabData): boolean;
    function IsPaintNeeded(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoPaintAfter(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    procedure TabMenuClick(Sender: TObject);
    function GetTabWidth_Plus_Raw: integer; inline;
    procedure UpdateTabWidths;
    procedure UpdateTabRects(C: TCanvas);
    procedure UpdateTabRectsSpecial;
    procedure UpdateTabPropsX;
    procedure UpdateTabRectsToFillLine(AIndexFrom, AIndexTo: integer; ALastLine: boolean);
    procedure UpdateCanvasAntialiasMode(C: TCanvas); inline;
    procedure UpdateCaptionProps(C: TCanvas; const ACaption: TATTabString;
      out ALineHeight: integer; out ATextSize: TSize);
    procedure DoTabDrop;
    function GetTabTick(AIndex: integer): Int64;
    function _IsDrag: boolean; inline;
    procedure SetOptShowPlusTab(const Value: boolean);

  public
    TabMenuExternal: TPopupMenu;

    constructor Create(AOwner: TComponent); override;
    function CanFocus: boolean; override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;

    procedure ApplyButtonLayout;
    function GetTabRectWidth(APlusBtn: boolean): integer;
    procedure UpdateRectPlus(var R: TRect);
    procedure UpdateTabTooltip;
    function GetTabRect_X(const ARect: TRect): TRect;
    function GetRectScrolled(const R: TRect): TRect;
    function GetTabAt(AX, AY: integer; out APressedX: boolean): integer;
    function GetTabData(AIndex: integer): TATTabData;
    function GetTabLastVisibleIndex: integer;
    function TabCount: integer;
    function AddTab(
      AIndex: integer;
      const ACaption: TATTabString;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone;
      AImageIndex: TImageIndex = -1): TATTabData; overload;
    procedure AddTab(AIndex: integer; AData: TATTabData); overload;
    procedure Clear;
    function DeleteTab(AIndex: integer; AAllowEvent, AWithCancelBtn: boolean;
      AAction: TATTabActionOnClose=aocDefault): boolean;
    function HideTab(AIndex: integer): boolean;
    function ShowTab(AIndex: integer): boolean;
    procedure MakeVisible(AIndex: integer);
    function IsTabVisible(AIndex: integer): boolean;
    procedure ShowTabMenu;
    procedure SwitchTab(ANext: boolean; ALoopAtEdge: boolean= true);
    procedure MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
    function FindTabByObject(AObject: TObject): integer;
    procedure DoScrollLeft;
    procedure DoScrollRight;
    function GetMaxScrollPos: integer;
    property ScrollPos: integer read FScrollPos write SetScrollPos;
    function DoScale(AValue: integer): integer;
    function DoScaleFont(AValue: integer): integer;
    procedure DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState; var Accept: Boolean); override;
    procedure Loaded; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

  published
    //inherited
    property Align;
    property Anchors;
    {$ifdef fpc}
    property BorderSpacing;
    {$endif}
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    //property Tabs: TCollection read FTabList write FTabList;
    property Tabs: TATTabListCollection read FTabList write FTabList;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnContextPopup;
    //these 2 lines don't compile under Delphi 7
    {$ifndef VER150}
    property OnMouseEnter;
    property OnMouseLeave;
    {$endif}
    //
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;

    //new
    property DoubleBuffered;
    property Images: TImageList read FImages write FImages;
    property TabIndex: integer read FTabIndex write SetTabIndex default 0;

    property IsClickingCloseButton: Boolean read FIsClickingCloseButton;

    //colors
    property ColorBg: TColor read FColorBg write FColorBg default _InitTabColorBg;
    property ColorBorderActive: TColor read FColorBorderActive write FColorBorderActive default _InitTabColorBorderActive;
    property ColorBorderPassive: TColor read FColorBorderPassive write FColorBorderPassive default _InitTabColorBorderPassive;
    property ColorSeparator: TColor read FColorSeparator write FColorSeparator default _InitTabColorArrow;
    property ColorTabActive: TColor read FColorTabActive write FColorTabActive default _InitTabColorTabActive;
    property ColorTabPassive: TColor read FColorTabPassive write FColorTabPassive default _InitTabColorTabPassive;
    property ColorTabOver: TColor read FColorTabOver write FColorTabOver default _InitTabColorTabOver;
    property ColorActiveMark: TColor read FColorActiveMark write FColorActiveMark default _InitTabColorActiveMark;
    property ColorFont: TColor read FColorFont write FColorFont default _InitTabColorFont;
    property ColorFontModified: TColor read FColorFontModified write FColorFontModified default _InitTabColorFontModified;
    property ColorFontActive: TColor read FColorFontActive write FColorFontActive default _InitTabColorFontActive;
    property ColorFontHot: TColor read FColorFontHot write FColorFontHot default _InitTabColorFontHot;
    property ColorCloseBg: TColor read FColorCloseBg write FColorCloseBg default _InitTabColorCloseBg;
    property ColorCloseBgOver: TColor read FColorCloseBgOver write FColorCloseBgOver default _InitTabColorCloseBgOver;
    property ColorCloseBorderOver: TColor read FColorCloseBorderOver write FColorCloseBorderOver default _InitTabColorCloseBorderOver;
    property ColorCloseX: TColor read FColorCloseX write FColorCloseX default _InitTabColorCloseX;
    property ColorCloseXOver: TColor read FColorCloseXOver write FColorCloseXOver default _InitTabColorCloseXOver;
    property ColorArrow: TColor read FColorArrow write FColorArrow default _InitTabColorArrow;
    property ColorArrowOver: TColor read FColorArrowOver write FColorArrowOver default _InitTabColorArrowOver;
    property ColorDropMark: TColor read FColorDropMark write FColorDropMark default _InitTabColorDropMark;
    property ColorScrollMark: TColor read FColorScrollMark write FColorScrollMark default _InitTabColorScrollMark;

    //options
    property OptButtonLayout: string read FOptButtonLayout write SetOptButtonLayout;
    property OptButtonSize: integer read FOptButtonSize write FOptButtonSize default _InitOptButtonSize;
    property OptButtonSizeSpace: integer read FOptButtonSizeSpace write FOptButtonSizeSpace default _InitOptButtonSizeSpace;
    property OptButtonSizeSeparator: integer read FOptButtonSizeSeparator write FOptButtonSizeSeparator default _InitOptButtonSizeSeparator;
    property OptScalePercents: integer read FOptScalePercents write SetOptScalePercents default 100;
    property OptVarWidth: boolean read FOptVarWidth write SetOptVarWidth default false;
    property OptMultiline: boolean read FOptMultiline write FOptMultiline default false;
    property OptFillWidth: boolean read FOptFillWidth write FOptFillWidth default _InitOptFillWidth;
    property OptFillWidthLastToo: boolean read FOptFillWidthLastToo write FOptFillWidthLastToo default _InitOptFillWidthLastToo;
    property OptTruncateCaption: TATCollapseStringMode read FOptTruncateCaption write FOptTruncateCaption default _InitOptTruncateCaption;
    property OptTabHeight: integer read FOptTabHeight write FOptTabHeight default _InitOptTabHeight;
    property OptTabWidthNormal: integer read FOptTabWidthNormal write FOptTabWidthNormal default _InitOptTabWidthNormal;
    property OptTabWidthMinimal: integer read FOptTabWidthMinimal write FOptTabWidthMinimal default _InitOptTabWidthMinimal;
    property OptTabWidthMaximal: integer read FOptTabWidthMaximal write FOptTabWidthMaximal default _InitOptTabWidthMaximal;
    property OptTabWidthMinimalHidesX: integer read FOptTabWidthMinimalHidesX write FOptTabWidthMinimalHidesX default _InitOptTabWidthMinimalHidesX;
    property OptTabRounded: boolean read FOptTabRounded write FOptTabRounded default _InitOptTabRounded;
    property OptFontScale: integer read FOptFontScale write FOptFontScale default 100;
    property OptMinimalWidthForSides: integer read FOptMinimalWidthForSides write FOptMinimalWidthForSides default _InitOptMinimalWidthForSides;
    property OptSpaceSide: integer read FOptSpaceSide write FOptSpaceSide default _InitOptSpaceSide;
    property OptSpaceBetweenTabs: integer read FOptSpaceBetweenTabs write FOptSpaceBetweenTabs default _InitOptSpaceBetweenTabs;
    property OptSpaceBetweenLines: integer read FOptSpaceBetweenLines write FOptSpaceBetweenLines default _InitOptSpaceBetweenLines;
    property OptSpaceBetweenIconCaption: integer read FOptSpaceBetweenIconCaption write FOptSpaceBetweenIconCaption default _InitOptSpaceBetweenIconCaption;
    property OptSpaceInitial: integer read FOptSpaceInitial write FOptSpaceInitial default _InitOptSpaceInitial;
    property OptSpaceBeforeText: integer read FOptSpaceBeforeText write FOptSpaceBeforeText default _InitOptSpaceBeforeText;
    property OptSpaceBeforeTextForMinWidth: integer read FOptSpaceBeforeTextForMinWidth write FOptSpaceBeforeTextForMinWidth default _InitOptSpaceBeforeTextForMinWidth;
    property OptSpaceAfterText: integer read FOptSpaceAfterText write FOptSpaceAfterText default _InitOptSpaceAfterText;
    property OptSpaceSeparator: integer read FOptSpaceSeparator write FOptSpaceSeparator default _InitOptSpaceSeparator;
    property OptSpacer: integer read FOptSpacer write FOptSpacer default _InitOptSpacer;
    property OptSpacer2: integer read FOptSpacer2 write FOptSpacer2 default _InitOptSpacer2;
    property OptSpaceXRight: integer read FOptSpaceXRight write FOptSpaceXRight default _InitOptSpaceXRight;
    property OptSpaceXInner: integer read FOptSpaceXInner write FOptSpaceXInner default _InitOptSpaceXInner;
    property OptSpaceXSize: integer read FOptSpaceXSize write FOptSpaceXSize default _InitOptSpaceXSize;
    property OptSpaceXIncrementRound: integer read FOptSpaceXIncrementRound write FOptSpaceXIncrementRound default _InitOptSpaceXIncrementRound;
    property OptSpaceModifiedCircle: integer read FOptSpaceModifiedCircle write FOptSpaceModifiedCircle default _InitOptSpaceModifiedCircle;
    property OptColoredBandSize: integer read FOptColoredBandSize write FOptColoredBandSize default _InitOptColoredBandSize;
    property OptColoredBandForTop: TATTabPosition read FOptColoredBandForTop write FOptColoredBandForTop default atpTop;
    property OptColoredBandForBottom: TATTabPosition read FOptColoredBandForBottom write FOptColoredBandForBottom default atpBottom;
    property OptColoredBandForLeft: TATTabPosition read FOptColoredBandForLeft write FOptColoredBandForLeft default atpLeft;
    property OptColoredBandForRight: TATTabPosition read FOptColoredBandForRight write FOptColoredBandForRight default atpRight;
    property OptActiveMarkSize: integer read FOptActiveMarkSize write FOptActiveMarkSize default _InitOptActiveMarkSize;
    property OptArrowSize: integer read FOptArrowSize write FOptArrowSize default _InitOptArrowSize;
    property OptScrollMarkSizeX: integer read FOptScrollMarkSizeX write FOptScrollMarkSizeX default _InitOptScrollMarkSizeX;
    property OptScrollMarkSizeY: integer read FOptScrollMarkSizeY write FOptScrollMarkSizeY default _InitOptScrollMarkSizeY;
    property OptScrollPagesizePercents: integer read FOptScrollPagesizePercents write FOptScrollPagesizePercents default _InitOptScrollPagesizePercents;
    property OptDropMarkSize: integer read FOptDropMarkSize write FOptDropMarkSize default _InitOptDropMarkSize;
    property OptActiveVisibleOnResize: boolean read FOptActiveVisibleOnResize write FOptActiveVisibleOnResize default true;

    property OptPosition: TATTabPosition read FOptPosition write FOptPosition default _InitOptPosition;
    property OptIconPosition: TATTabIconPosition read FOptIconPosition write FOptIconPosition default aipIconLefterThanText;
    property OptWhichActivateOnClose: TATTabActionOnClose read FOptWhichActivateOnClose write FOptWhichActivateOnClose default aocRight;
    property OptCaptionAlignment: TAlignment read FOptCaptionAlignment write FOptCaptionAlignment default taLeftJustify;
    property OptShowFlat: boolean read FOptShowFlat write FOptShowFlat default _InitOptShowFlat;
    property OptShowFlatMouseOver: boolean read FOptShowFlatMouseOver write FOptShowFlatMouseOver default _InitOptShowFlatMouseOver;
    property OptShowFlatSepar: boolean read FOptShowFlatSepar write FOptShowFlatSepar default _InitOptShowFlatSep;
    property OptShowModifiedCircle: boolean read FOptShowModifiedCircle write FOptShowModifiedCircle default _InitOptShowModifiedCircle;
    property OptShowModifiedColorOnX: boolean read FOptShowModifiedColorOnX write FOptShowModifiedColorOnX default _InitOptShowModifiedColorOnX;
    property OptShowScrollMark: boolean read FOptShowScrollMark write FOptShowScrollMark default _InitOptShowScrollMark;
    property OptShowDropMark: boolean read FOptShowDropMark write FOptShowDropMark default _InitOptShowDropMark;
    property OptShowXRounded: boolean read FOptShowXRounded write FOptShowXRounded default _InitOptShowXRounded;
    property OptShowXButtons: TATTabShowClose read FOptShowXButtons write FOptShowXButtons default _InitOptShowXButtons;
    property OptShowPlusTab: boolean read FOptShowPlusTab write SetOptShowPlusTab default _InitOptShowPlusTab;
    property OptShowArrowsNear: boolean read FOptShowArrowsNear write FOptShowArrowsNear default _InitOptShowArrowsNear;
    property OptShowPinnedText: TATTabString read FOptShowPinnedText write FOptShowPinnedText;
    property OptShowEntireColor: boolean read FOptShowEntireColor write FOptShowEntireColor default _InitOptShowEntireColor;
    property OptShowNumberPrefix: TATTabString read FOptShowNumberPrefix write FOptShowNumberPrefix;
    property OptShowActiveMarkInverted: boolean read FOptShowActiveMarkInverted write FOptShowActiveMarkInverted default _InitOptShowActiveMarkInverted;
    property OptActiveFontStyle: TFontStyles read FOptActiveFontStyle write FOptActiveFontStyle default _InitOptActiveFontStyle;
    property OptActiveFontStyleUsed: boolean read FOptActiveFontStyleUsed write FOptActiveFontStyleUsed default _InitOptActiveFontStyleUsed;
    property OptHotFontStyle: TFontStyles read FOptHotFontStyle write FOptHotFontStyle default _InitOptHotFontStyle;
    property OptHotFontStyleUsed: boolean read FOptHotFontStyleUsed write FOptHotFontStyleUsed default _InitOptHotFontStyleUsed;

    property OptMouseWheelMode: TATTabMouseWheelMode read FOptMouseWheelMode write FOptMouseWheelMode default _InitOptMouseWheelMode;
    property OptMouseMiddleClickClose: boolean read FOptMouseMiddleClickClose write FOptMouseMiddleClickClose default _InitOptMouseMiddleClickClose;
    property OptMouseDoubleClickClose: boolean read FOptMouseDoubleClickClose write FOptMouseDoubleClickClose default _InitOptMouseDoubleClickClose;
    property OptMouseDoubleClickPlus: boolean read FOptMouseDoubleClickPlus write FOptMouseDoubleClickPlus default _InitOptMouseDoubleClickPlus;
    property OptMouseDragEnabled: boolean read FOptMouseDragEnabled write FOptMouseDragEnabled default _InitOptMouseDragEnabled;
    property OptMouseDragOutEnabled: boolean read FOptMouseDragOutEnabled write FOptMouseDragOutEnabled default _InitOptMouseDragOutEnabled;
    property OptMouseDragFromNotATTabs: boolean read FOptMouseDragFromNotATTabs write FOptMouseDragFromNotATTabs default _InitOptMouseDragFromNotATTabs;

    property OptHintForX: string read FHintForX write FHintForX;
    property OptHintForPlus: string read FHintForPlus write FHintForPlus;
    property OptHintForArrowLeft: string read FHintForArrowLeft write FHintForArrowLeft;
    property OptHintForArrowRight: string read FHintForArrowRight write FHintForArrowRight;
    property OptHintForArrowMenu: string read FHintForArrowMenu write FHintForArrowMenu;
    property OptHintForUser0: string read FHintForUser0 write FHintForUser0;
    property OptHintForUser1: string read FHintForUser1 write FHintForUser1;
    property OptHintForUser2: string read FHintForUser2 write FHintForUser2;
    property OptHintForUser3: string read FHintForUser3 write FHintForUser3;
    property OptHintForUser4: string read FHintForUser4 write FHintForUser4;

    //events
    property OnTabClick: TNotifyEvent read FOnTabClick write FOnTabClick;
    property OnTabChanged: TNotifyEvent read FOnTabChanged write FOnTabChanged;           // aducom
    property OnTabPlusClick: TNotifyEvent read FOnTabPlusClick write FOnTabPlusClick;
    property OnTabClickUserButton: TATTabClickUserButton read FOnTabClickUserButton write FOnTabClickUserButton;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnTabMenu: TATTabMenuEvent read FOnTabMenu write FOnTabMenu;
    property OnTabDrawBefore: TATTabDrawEvent read FOnTabDrawBefore write FOnTabDrawBefore;
    property OnTabDrawAfter: TATTabDrawEvent read FOnTabDrawAfter write FOnTabDrawAfter;
    property OnTabEmpty: TNotifyEvent read FOnTabEmpty write FOnTabEmpty;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
    property OnTabChangeQuery: TATTabChangeQueryEvent read FOnTabChangeQuery write FOnTabChangeQuery;
    property OnTabGetTick: TATTabGetTickEvent read FOnTabGetTick write FOnTabGetTick;
    property OnTabGetCloseAction: TATTabGetCloseActionEvent read FOnTabGetCloseAction write FOnTabGetCloseAction;
    property OnTabDblClick: TATTabDblClickEvent read FOnTabDblClick write FOnTabDblClick;
    property OnTabDropQuery: TATTabDropQueryEvent read FOnTabDropQuery write FOnTabDropQuery;
    property OnTabDragging: TATTabDropQueryEvent read FOnTabDragging write FOnTabDragging;
  end;

var
  cTabsMouseMinDistanceToDrag: integer = 10; //mouse must move >=N pixels to start drag-drop
  cTabsMouseMaxDistanceToClick: integer = 4; //if mouse moves during mouse-down >=N pixels, dont click
  cTabsMinWidthForCaption: integer = 8; //don't draw caption if width of tab is less

implementation

uses
  SysUtils,
  Dialogs,
  Forms,
  Math;

const
  cSmoothScale = 5;

procedure AddTabButton(var Buttons: TATTabButtons; Id: TATTabButton; Size: integer);
begin
  SetLength(Buttons, Length(Buttons)+1);
  Buttons[Length(Buttons)-1].Id:= Id;
  Buttons[Length(Buttons)-1].Size:= Size;
end;

{ TATTabPaintInfo }

procedure TATTabPaintInfo.Clear;
begin
  Self.Caption:= '';
  FillChar(Self, SizeOf(Self), 0);
  ImageIndex:= -1;
end;

{ TATTabListCollection }

destructor TATTabListCollection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TATTabListCollection.Clear;
var
  Item: TObject;
  i: integer;
begin
  for i:= Count-1 downto 0 do
  begin
    Item:= Items[i];
    if Assigned(Item) then
      Item.Free;
  end;
  inherited Clear;
end;

procedure TATTabData.UpdateTabSet;
begin
  if Collection is TATTabListCollection then
    if TATTabListCollection(Collection).AOwner is TATTabs then
      TATTabListCollection(Collection).AOwner.Invalidate;
end;

function TATTabData.GetTabCaptionFull: TATTabString;
begin
  Result:= FTabCaption;
  if FTabCaptionAddon<>'' then
    Result:= Result+ATTabsAddonSeparator+FTabCaptionAddon;
end;

procedure TATTabData.SetTabImageIndex(const Value: TImageIndex);
begin
  if FTabImageIndex<>Value then
  begin
    FTabImageIndex:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabCaption(const Value: TATTabString);
begin
  if FTabCaption<>Value then
  begin
    FTabCaption:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabColor(const Value: TColor);
begin
  if FTabColor<>Value then
  begin
    FTabColor:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabColorActive(const Value: TColor);
begin
  if FTabColorActive<>Value then
  begin
    FTabColorActive:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabColorOver(const Value: TColor);
begin
  if FTabColorOver<>Value then
  begin
    FTabColorOver:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabFontColor(const Value: TColor);
begin
  if FTabFontColor<>Value then
  begin
    FTabFontColor:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabHideXButton(const Value: boolean);
begin
  if FTabHideXButton<>Value then
  begin
    FTabHideXButton:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabVisible(const Value: boolean);
begin
  if FTabVisible<>Value then
  begin
    FTabVisible:= Value;
    UpdateTabSet;
  end;
end;

procedure TATTabs.SetOptShowPlusTab(const Value: boolean);
begin
  FOptShowPlusTab:= Value;
  Invalidate;
end;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$else}
  Result:= true;
  {$endif}
end;

function _FindControl(Pnt: TPoint): TControl;
begin
  {$ifdef FPC}
  Result:= FindControlAtPosition(Pnt, false);
  {$else}
  Result:= FindVCLWindow(Pnt);
  {$endif}
end;

function PtInControl(Control: TControl; const ScreenPnt: TPoint): boolean;
begin
  Result:= PtInRect(Control.ClientRect, Control.ScreenToClient(ScreenPnt));
end;

procedure DrawLine(C: TCanvas; X1, Y1, X2, Y2: integer; AColor: TColor);
begin
  if Y1=Y2 then
    if X2>X1 then Inc(X2) else Dec(X2);
  if X1=X2 then
    if Y2>Y1 then Inc(Y2) else Dec(Y2);

  C.Pen.Color:= AColor;
  C.MoveTo(X1, Y1);
  C.LineTo(X2, Y2);
end;

procedure DrawTriangleType(C: TCanvas; AType: TATTabTriangle; const ARect: TRect; AColor: TColor; ASize: integer);
begin
  case AType of
    atriDown:
      CanvasPaintTriangleDown(C, AColor, CenterPoint(ARect), ASize);
    atriRight:
      CanvasPaintTriangleRight(C, AColor, CenterPoint(ARect), ASize);
    atriLeft:
      CanvasPaintTriangleLeft(C, AColor, CenterPoint(ARect), ASize);
  end;
end;


procedure DrawPlusSign(C: TCanvas; const R: TRect; ASize, ALineWidth: integer; AColor: TColor);
var
  CX, CY: integer;
begin
  CX:= (R.Left+R.Right) div 2;
  CY:= (R.Top+R.Bottom) div 2;
  C.Pen.Width:= ALineWidth;
  {$ifdef FPC}
  C.Pen.EndCap:= pecSquare;
  {$endif}
  DrawLine(C, CX-ASize, CY, CX+ASize, CY, AColor);
  DrawLine(C, CX, CY-ASize, CX, CY+ASize, AColor);
  C.Pen.Width:= 1;
end;


procedure CanvasStretchDraw(C: TCanvas; const R: TRect; Bmp: TBitmap); {$ifdef fpc}inline;{$endif}
begin
  {$ifdef fpc}
  C.StretchDraw(R, Bmp);
  {$else}
  //Delphi: StretchDraw cannot draw smooth
  StretchBlt(
    C.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
    Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height,
    C.CopyMode);
  {$endif}
end;

type
  TATMissedPoint = (
    ampnTopLeft,
    ampnTopRight,
    ampnBottomLeft,
    ampnBottomRight
    );

procedure DrawTriangleRectFramed(C: TCanvas;
  AX, AY, ASizeX, ASizeY, AScale: integer;
  ATriKind: TATMissedPoint;
  AColorFill, AColorLine: TColor;
  b: TBitmap);
var
  p0, p1, p2, p3: TPoint;
  line1, line2: TPoint;
  ar: array[0..2] of TPoint;
begin
  BitmapResize(b, ASizeX*AScale, ASizeY*AScale);

  //b.Canvas.Brush.Color:= AColorBG;
  //b.Canvas.FillRect(0, 0, b.Width, b.Height);
  b.Canvas.CopyRect(
    Rect(0, 0, b.Width, b.Height),
    C,
    Rect(AX, AY, AX+ASizeX, AY+ASizeY)
    );

  p0:= Point(0, 0);
  p1:= Point(b.Width, 0);
  p2:= Point(0, b.Height);
  p3:= Point(b.Width, b.Height);

  case ATriKind of
    ampnTopLeft: begin ar[0]:= p1; ar[1]:= p2; ar[2]:= p3; line1:= p1; line2:= p2; end;
    ampnTopRight: begin ar[0]:= p0; ar[1]:= p2; ar[2]:= p3; line1:= p0; line2:= p3; end;
    ampnBottomLeft: begin ar[0]:= p0; ar[1]:= p1; ar[2]:= p3; line1:= p0; line2:= p3; end;
    ampnBottomRight: begin ar[0]:= p0; ar[1]:= p1; ar[2]:= p2; line1:= p1; line2:= p2; end;
  end;

  b.Canvas.Pen.Style:= psClear;
  b.Canvas.Brush.Color:= AColorFill;
  b.Canvas.Polygon(ar);
  b.Canvas.Pen.Style:= psSolid;

  b.Canvas.Pen.Color:= AColorLine;
  b.Canvas.Pen.Width:= AScale;
  b.Canvas.MoveTo(line1.X, line1.Y);
  b.Canvas.LineTo(line2.X, line2.Y);
  b.Canvas.Pen.Width:= 1;

  CanvasStretchDraw(C, Rect(AX, AY, AX+ASizeX, AY+ASizeY), b);
end;

{ TATTabData }

constructor TATTabData.Create(ACollection: TCollection);
begin
  inherited;
  Clear;
  TabVisible:= true;
  TabColor:= clNone;
  TabColorActive:= clNone;
  TabColorOver:= clNone;
  TabFontColor:= clNone;
  TabImageIndex:= -1;
  TabFontStyle:= [];
end;

destructor TATTabData.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TATTabData.Clear;
begin
  FTabCaption:= '';
  FTabCaptionAddon:= '';
  FTabHint:= '';
end;

procedure TATTabData.Assign(Source: TPersistent);
var
  D: TATTabData;
begin
  if Source is TATTabData then
  begin
    D:= TATTabData(Source);
    TabCaption:= D.TabCaption;
    TabCaptionAddon:= D.TabCaptionAddon;
    TabObject:= D.TabObject;
    TabHint:= D.TabHint;
    TabColor:= D.TabColor;
    TabColorActive:= D.TabColorActive;
    TabColorOver:= D.TabColorOver;
    TabFontColor:= D.TabFontColor;
    TabModified:= D.TabModified;
    TabModified2:= D.TabModified2;
    TabExtModified:= D.TabExtModified;
    TabExtModified2:= D.TabExtModified2;
    TabExtDeleted:= D.TabExtDeleted;
    TabExtDeleted2:= D.TabExtDeleted2;
    TabTwoDocuments:= D.TabTwoDocuments;
    TabImageIndex:= D.TabImageIndex;
    TabFontStyle:= D.TabFontStyle;
    TabPopupMenu:= D.TabPopupMenu;
    TabSpecialWidth:= D.TabSpecialWidth;
    TabSpecialHeight:= D.TabSpecialHeight;
    TabHideXButton:= D.TabHideXButton;
    TabVisible:= D.TabVisible;
    TabSpecial:= D.TabSpecial;
    TabPinned:= D.TabPinned;
  end
  else
    inherited Assign(Source);
end;

{ TATTabs }

function TATTabs.IsIndexOk(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FTabList.Count);
end;

function TATTabs.TabCount: integer;
begin
  if Assigned(FTabList) then
    Result:= FTabList.Count
  else
    Result:= 0;
end;

constructor TATTabs.Create(AOwner: TComponent);
begin
  inherited;

  Caption:= '';
  ControlStyle:= ControlStyle+[csOpaque];
  DoubleBuffered:= IsDoubleBufferedNeeded;
  DragMode:= dmManual; //required Manual
  ParentColor:= false; //better don't support ParentColor, it's mess in code
  Width:= 400;
  Height:= 35;

  FMouseDown:= false;
  FMouseDownPnt:= Point(0, 0);
  FMouseDownDbl:= false;
  FMouseDownRightBtn:= false;

  FPaintCount:= 0;
  FLastOverIndex:= -100;
  FLastOverX:= false;

  FColorBg:= _InitTabColorBg;
  FColorSeparator:= _InitTabColorArrow;
  FColorTabActive:= _InitTabColorTabActive;
  FColorTabPassive:= _InitTabColorTabPassive;
  FColorTabOver:= _InitTabColorTabOver;
  FColorActiveMark:= _InitTabColorActiveMark;
  FColorFont:= _InitTabColorFont;
  FColorFontModified:= _InitTabColorFontModified;
  FColorFontActive:= _InitTabColorFontActive;
  FColorFontHot:= _InitTabColorFontHot;
  FColorBorderActive:= _InitTabColorBorderActive;
  FColorBorderPassive:= _InitTabColorBorderPassive;
  FColorCloseBg:= _InitTabColorCloseBg;
  FColorCloseBgOver:= _InitTabColorCloseBgOver;
  FColorCloseBorderOver:= _InitTabColorCloseBorderOver;
  FColorCloseX:= _InitTabColorCloseX;
  FColorCloseXOver:= _InitTabColorCloseXOver;
  FColorArrow:= _InitTabColorArrow;
  FColorArrowOver:= _InitTabColorArrowOver;
  FColorDropMark:= _InitTabColorDropMark;
  FColorScrollMark:= _InitTabColorScrollMark;

  FOptScalePercents:= 100;
  FOptButtonSize:= _InitOptButtonSize;
  FOptButtonSizeSpace:= _InitOptButtonSizeSpace;
  FOptButtonSizeSeparator:= _InitOptButtonSizeSeparator;

  FOptButtonLayout:= _InitOptButtonLayout;
  ApplyButtonLayout;

  FOptCaptionAlignment:= taLeftJustify;
  FOptIconPosition:= aipIconLefterThanText;
  FOptWhichActivateOnClose:= aocRight;
  FOptFillWidth:= _InitOptFillWidth;
  FOptFillWidthLastToo:= _InitOptFillWidthLastToo;
  FOptTruncateCaption:= _InitOptTruncateCaption;
  FOptTabHeight:= _InitOptTabHeight;
  FOptTabWidthMinimal:= _InitOptTabWidthMinimal;
  FOptTabWidthMaximal:= _InitOptTabWidthMaximal;
  FOptTabWidthNormal:= _InitOptTabWidthNormal;
  FOptTabWidthMinimalHidesX:= _InitOptTabWidthMinimalHidesX;
  FOptTabRounded:= _InitOptTabRounded;
  FOptFontScale:= 100;
  FOptMinimalWidthForSides:= _InitOptMinimalWidthForSides;
  FOptSpaceSide:= _InitOptSpaceSide;
  FOptSpaceInitial:= _InitOptSpaceInitial;
  FOptSpaceBeforeText:= _InitOptSpaceBeforeText;
  FOptSpaceBeforeTextForMinWidth:= _InitOptSpaceBeforeTextForMinWidth;
  FOptSpaceAfterText:= _InitOptSpaceAfterText;
  FOptSpaceBetweenTabs:= _InitOptSpaceBetweenTabs;
  FOptSpaceBetweenLines:= _InitOptSpaceBetweenLines;
  FOptSpaceBetweenIconCaption:= _InitOptSpaceBetweenIconCaption;
  FOptSpaceSeparator:= _InitOptSpaceSeparator;
  FOptSpacer:= _InitOptSpacer;
  FOptSpacer2:= _InitOptSpacer2;
  FOptSpaceXRight:= _InitOptSpaceXRight;
  FOptSpaceXInner:= _InitOptSpaceXInner;
  FOptSpaceXSize:= _InitOptSpaceXSize;
  FOptSpaceXIncrementRound:= _InitOptSpaceXIncrementRound;
  FOptSpaceModifiedCircle:= _InitOptSpaceModifiedCircle;
  FOptArrowSize:= _InitOptArrowSize;
  FOptColoredBandSize:= _InitOptColoredBandSize;
  FOptColoredBandForTop:= atpTop;
  FOptColoredBandForBottom:= atpBottom;
  FOptColoredBandForLeft:= atpLeft;
  FOptColoredBandForRight:= atpRight;
  FOptActiveMarkSize:= _InitOptActiveMarkSize;
  FOptScrollMarkSizeX:= _InitOptScrollMarkSizeX;
  FOptScrollMarkSizeY:= _InitOptScrollMarkSizeY;
  FOptScrollPagesizePercents:= _InitOptScrollPagesizePercents;
  FOptActiveVisibleOnResize:= true;
  FOptDropMarkSize:= _InitOptDropMarkSize;
  FOptActiveFontStyle:= _InitOptActiveFontStyle;
  FOptActiveFontStyleUsed:= _InitOptActiveFontStyleUsed;
  FOptHotFontStyle:= _InitOptHotFontStyle;
  FOptHotFontStyleUsed:= _InitOptHotFontStyleUsed;

  FOptShowFlat:= _InitOptShowFlat;
  FOptShowFlatMouseOver:= _InitOptShowFlatMouseOver;
  FOptShowFlatSepar:= _InitOptShowFlatSep;
  FOptShowModifiedCircle:= _InitOptShowModifiedCircle;
  FOptShowModifiedColorOnX:= _InitOptShowModifiedColorOnX;
  FOptPosition:= _InitOptPosition;
  FOptShowNumberPrefix:= _InitOptShowNumberPrefix;
  FOptShowScrollMark:= _InitOptShowScrollMark;
  FOptShowDropMark:= _InitOptShowDropMark;
  FOptShowXRounded:= _InitOptShowXRounded;
  FOptShowXButtons:= _InitOptShowXButtons;
  FOptShowPlusTab:= _InitOptShowPlusTab;
  FOptShowArrowsNear:= _InitOptShowArrowsNear;
  FOptShowPinnedText:= _InitOptShowPinnedText;
  FOptShowEntireColor:= _InitOptShowEntireColor;
  FOptShowActiveMarkInverted:= _InitOptShowActiveMarkInverted;

  FOptMouseWheelMode:= _InitOptMouseWheelMode;
  FOptMouseMiddleClickClose:= _InitOptMouseMiddleClickClose;
  FOptMouseDoubleClickClose:= _InitOptMouseDoubleClickClose;
  FOptMouseDoubleClickPlus:= _InitOptMouseDoubleClickPlus;
  FOptMouseDragEnabled:= _InitOptMouseDragEnabled;
  FOptMouseDragOutEnabled:= _InitOptMouseDragOutEnabled;
  FOptMouseDragFromNotATTabs:= _InitOptMouseDragFromNotATTabs;

  FHintForX:= 'Close tab';
  FHintForPlus:= 'Add tab';
  FHintForArrowLeft:= 'Scroll tabs left';
  FHintForArrowRight:= 'Scroll tabs right';
  FHintForArrowMenu:= 'Show tabs list';
  FHintForUser0:= '0';
  FHintForUser1:= '1';
  FHintForUser2:= '2';
  FHintForUser3:= '3';
  FHintForUser4:= '4';

  FBitmap:= TBitmap.Create;
  BitmapResize(FBitmap, 1600, 60);

  FBitmapAngleL:= TBitmap.Create;
  FBitmapAngleR:= TBitmap.Create;
  FBitmapRound:= TBitmap.Create;
  BitmapResize(FBitmapRound, _InitRoundedBitmapSize, _InitRoundedBitmapSize);

  FTabIndex:= -1;
  FTabIndexOver:= -1;
  FTabIndexHinted:= -1;
  FTabIndexHintedPrev:= -1;
  FTabIndexDrop:= -1;
  FTabIndexDropOld:= -1;

  FTabList:= TATTabListCollection.Create(TATTabData);
  FTabList.AOwner:= Self;
  FTabMenu:= nil;
  FScrollPos:= 0;
end;

function TATTabs.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATTabs.Clear;
begin
  FTabList.Clear;
  FTabIndex:= -1;
end;

destructor TATTabs.Destroy;
begin
  Clear;
  FreeAndNil(FTabList);
  FreeAndNil(FBitmapRound);
  FreeAndNil(FBitmapAngleL);
  FreeAndNil(FBitmapAngleR);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATTabs.PaintSimulated;
begin
  if Assigned(FBitmap) then
    DoPaintTo(FBitmap.Canvas);
end;

procedure TATTabs.Paint;
begin
  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
    begin
      DoPaintTo(FBitmap.Canvas);
      Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
    end;
  end
  else
  begin
    DoPaintTo(Canvas);
  end;

  {$ifdef tabs_paint_counter}
  Inc(FPaintCount);
  Canvas.Font.Color:= clRed;
  Canvas.TextOut(0, 0, IntToStr(FPaintCount));;
  {$endif}
end;

procedure _PaintMaybeCircle(C: TCanvas; X1, Y1, X2, Y2: integer);
begin
  if ATTabsCircleDrawEnabled then
    C.Ellipse(X1, Y1, X2, Y2)
  else
    C.Rectangle(X1, Y1, X2, Y2);
end;

procedure TATTabs.DoPaintTabTo(C: TCanvas; const AInfo: TATTabPaintInfo);
const
  cIndentFlatSeparatorTop = 0;
  cIndentFlatSeparatorBottom = 1;
  cIndentAboveCircle = 1;
  cIndentBetweenCircles = 1;
var
  RectText: TRect;
  NIndentL, NIndentR, NIndentTop, NLeft, NTop,
  NLineHeight, NLineWidth, NLineIndex: integer;
  NCircleSize: integer;
  TempCaption: TATTabString;
  Extent: TSize;
  bNeedMoreSpace: boolean;
  NColor: TColor;
  ColorPos: TATTabPosition;
  Data: TATTabData;
  Sep: TATStringSeparator;
  SepItem: string;
  bOneLiner: boolean;
begin
  //optimize for 200 tabs
  if AInfo.Rect.Left>=Width then exit;
  if AInfo.Rect.Top>=Height then exit;
  if AInfo.Rect.Right<=0 then exit;
  if AInfo.Rect.Bottom<=0 then exit;

  Data:= GetTabData(AInfo.TabIndex);
  if Assigned(Data) then
  begin
    // if tab is not visible then don't draw
    if not Data.TabVisible then
      exit;
  end;

  UpdateCanvasAntialiasMode(C);

  DoPaintTabShape(C,
    Rect(
      AInfo.Rect.Left-DoScale(FLastSpaceSide),
      AInfo.Rect.Top,
      AInfo.Rect.Right+DoScale(FLastSpaceSide),
      AInfo.Rect.Bottom),
    AInfo.TabActive,
    AInfo.TabIndex
    );

  RectText:= AInfo.Rect;
  bNeedMoreSpace:= (RectText.Right-RectText.Left<=DoScale(FOptSpaceBeforeTextForMinWidth)) and (AInfo.Caption<>'');
  NIndentL:= IfThen(not bNeedMoreSpace, DoScale(FOptSpaceBeforeText), 2);
  NIndentR:= IfThen(not bNeedMoreSpace, DoScale(FOptSpaceAfterText), 2) + IfThen(Assigned(Data) and Data.TabVisibleX, DoScale(FOptSpaceXRight));
  RectText:= Rect(AInfo.Rect.Left+NIndentL, AInfo.Rect.Top, AInfo.Rect.Right-NIndentR, AInfo.Rect.Bottom);
  if Assigned(Data) then
    Data.TabCaptionRect:= RectText;

  if FOptShowFlat and FOptShowFlatSepar then
  begin
    NLeft:= AInfo.Rect.Left - DoScale(FOptSpaceBetweenTabs) div 2;
    DrawLine(C, NLeft, AInfo.Rect.Top+cIndentFlatSeparatorTop, NLeft, AInfo.Rect.Bottom-cIndentFlatSeparatorBottom, FColorSeparator);
  end;

  //imagelist
  if Assigned(FImages) then
    if (AInfo.ImageIndex>=0) and (AInfo.ImageIndex<FImages.Count) then
    begin
      NIndentTop:=
        (RectText.Top + RectText.Bottom - FImages.Height + DoScale(FOptColoredBandSize)) div 2;
      case FOptIconPosition of
        aipIconLefterThanText:
          begin
            FImages.Draw(C,
              RectText.Left - 2,
              NIndentTop,
              AInfo.ImageIndex);
            Inc(RectText.Left, FImages.Width+DoScale(FOptSpaceBetweenIconCaption));
          end;
        aipIconRighterThanText:
          begin
            FImages.Draw(C,
              RectText.Right - FImages.Width + 2,
              NIndentTop,
              AInfo.ImageIndex);
            Dec(RectText.Right, FImages.Width+DoScale(FOptSpaceBetweenIconCaption));
          end;
        aipIconCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              NIndentTop,
              AInfo.ImageIndex);
          end;
        aipIconAboveTextCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              RectText.Top + DoScale(FOptColoredBandSize),
              AInfo.ImageIndex);
            Inc(RectText.Top, FImages.Height+DoScale(FOptSpaceBetweenIconCaption));
          end;
        aipIconBelowTextCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              RectText.Bottom - FImages.Height,
              AInfo.ImageIndex);
            Dec(RectText.Bottom, FImages.Height+DoScale(FOptSpaceBetweenIconCaption));
          end;
      end;
    end;

  //caption
  C.Brush.Style:= bsClear;
  if RectText.Right-RectText.Left>=cTabsMinWidthForCaption then
  begin
    C.Font.Assign(Self.Font);
    C.Font.Style:= AInfo.FontStyle;
    if Assigned(Data) and (Data.TabFontColor<>clNone) then
      C.Font.Color:= Data.TabFontColor
    else
      C.Font.Color:= AInfo.ColorFont;
    C.Font.Size:= DoScaleFont(C.Font.Size);

    TempCaption:= AInfo.Caption;
    UpdateCaptionProps(C, TempCaption, NLineHeight, Extent);

    NIndentTop:= (RectText.Bottom-RectText.Top-Extent.cy) div 2 + 1;

    bOneLiner:= Pos(#10, TempCaption)=0;
    Sep.Init(TempCaption, #10);
    NLineIndex:= -1;

    while Sep.GetItemStr(SepItem) do
    begin
      Inc(NLineIndex);

      //calculate center pos for each SepItem
      case FOptCaptionAlignment of
        taLeftJustify:
          NIndentL:= RectText.Left;

        taCenter:
          begin
            if bOneLiner then
              NLineWidth:= Extent.cx
            else
              NLineWidth:= C.TextWidth(SepItem);
            NIndentL:= Max(
              RectText.Left,
              (RectText.Left+RectText.Right-NLineWidth) div 2
              );
          end;

        taRightJustify:
          begin
            if bOneLiner then
              NLineWidth:= Extent.cx
            else
              NLineWidth:= C.TextWidth(SepItem);
            NIndentL:= Max(
              RectText.Left,
              RectText.Right-NLineWidth
              );
          end;
      end;

      SepItem:= CanvasCollapseStringByDots(C,
        SepItem,
        FOptTruncateCaption,
        RectText.Right-RectText.Left
        );
      DoTextOut(C,
        NIndentL,
        RectText.Top+NIndentTop+NLineIndex*NLineHeight,
        RectText,
        SepItem
        );
    end;
  end;

  NColor:= clNone;
  if AInfo.TabMouseOver and not AInfo.TabActive and Assigned(Data) and (Data.TabColorOver<>clNone) then
    NColor:= Data.TabColorOver
  else
  if AInfo.TabActive and Assigned(Data) and (Data.TabColorActive<>clNone) then
    NColor:= Data.TabColorActive
  else
  if Assigned(Data) and (Data.TabColor<>clNone) then
    NColor:= Data.TabColor;

  //colored band
  if not FOptShowEntireColor then
  begin
    if NColor<>clNone then
    begin
      case FOptPosition of
        atpTop:
          ColorPos:= FOptColoredBandForTop;
        atpBottom:
          ColorPos:= FOptColoredBandForBottom;
        atpLeft:
          ColorPos:= FOptColoredBandForLeft;
        atpRight:
          ColorPos:= FOptColoredBandForRight;
        else
          raise Exception.Create('Unknown tab pos');
      end;
      DoPaintColoredBand(C, AInfo.Rect, NColor, ColorPos);
    end;
  end;

  if FOptShowModifiedCircle and (
    AInfo.Modified or
    AInfo.Modified2 or
    AInfo.ExtModified or
    AInfo.ExtModified2 or
    AInfo.ExtDeleted or
    AInfo.ExtDeleted2
    ) then
  begin
    NCircleSize:= DoScale(FOptSpaceModifiedCircle);
    NLeft:= (AInfo.Rect.Left+AInfo.Rect.Right) div 2;
    if AInfo.TwoDocuments then
      Dec(NLeft, NCircleSize)
    else
      Dec(NLeft, NCircleSize div 2);
    NTop:= RectText.Top+DoScale(cIndentAboveCircle);

    C.Pen.Color:= AInfo.ColorFont;
    C.Brush.Color:= AInfo.ColorFont;

    if AInfo.TwoDocuments or AInfo.Modified or AInfo.ExtModified or AInfo.ExtDeleted then
    begin
      if AInfo.Modified or AInfo.ExtModified or AInfo.ExtDeleted then
        C.Brush.Style:= bsSolid
      else
        C.Brush.Style:= bsClear;
      _PaintMaybeCircle(C, NLeft, NTop, NLeft+NCircleSize, NTop+NCircleSize);
    end;
    if AInfo.TwoDocuments then
    begin
      if AInfo.Modified2 or AInfo.Modified2 or AInfo.ExtDeleted2 then
        C.Brush.Style:= bsSolid
      else
        C.Brush.Style:= bsClear;
      Inc(NLeft, NCircleSize+DoScale(cIndentBetweenCircles));
      _PaintMaybeCircle(C, NLeft, NTop, NLeft+NCircleSize, NTop+NCircleSize);
    end;

    C.Brush.Style:= bsSolid;
  end;
end;

procedure TATTabs.DoPaintPlus(C: TCanvas; const ARect: TRect);
var
  NColorFont: TColor;
  ElemType: TATTabElemType;
  bActive: boolean;
  Info: TATTabPaintInfo;
begin
  bActive:= FTabIndexOver=cTabIndexPlus;
  if bActive then
    ElemType:= aeTabPlusOver
  else
    ElemType:= aeTabPlus;

  if IsPaintNeeded(ElemType, -1, C, ARect) then
  begin
    NColorFont:= FColorFont;

    Info.Clear;
    Info.Rect:= ARect;
    Info.TabIndex:= cTabIndexPlus;
    Info.ColorFont:= NColorFont;
    DoPaintTabTo(C, Info);

    DrawPlusSign(C, ARect, DoScale(FOptArrowSize), DoScale(1), FColorFont);
    DoPaintAfter(ElemType, -1, C, ARect);
  end;
end;


procedure TATTabs.DoPaintTabShape(C: TCanvas; const ATabRect: TRect;
  ATabActive: boolean; ATabIndex: integer);
var
  NColorBg, NColorEmpty, NColorBorder: TColor;
  PL1, PL2, PR1, PR2: TPoint;
  R: TRect;
begin
  R.Top:= ATabRect.Top;
  R.Bottom:= ATabRect.Bottom;
  R.Left:= ATabRect.Left+DoScale(FLastSpaceSide);
  R.Right:= ATabRect.Right-DoScale(FLastSpaceSide);
  if ATabActive then
    NColorBg:= GetTabBgColor_Active(ATabIndex)
  else
    NColorBg:= GetTabBgColor_Passive(ATabIndex);

  C.Pen.Color:= NColorBg;
  C.Brush.Color:= NColorBg;
  C.FillRect(R);

  PL1:= Point(R.Left, R.Top);
  PL2:= Point(R.Left, R.Bottom-1);
  PR1:= Point(R.Right-1, R.Top);
  PR2:= Point(R.Right-1, R.Bottom-1);

  //center shape
  DoPaintTabShape_C(C, ATabActive, ATabIndex, R, PL1, PL2, PR1, PR2);

  //left/right edges
  if (FLastSpaceSide>0) and not FOptShowFlat then
  begin
    DoPaintTabShape_L(C, R, ATabActive, ATabIndex);
    DoPaintTabShape_R(C, R, ATabActive, ATabIndex);
  end
  else
  if FOptTabRounded and not FOptShowFlat and ATTabsPixelsDrawEnabled then
  begin
    NColorEmpty:= ColorBg;
    if ATabActive then
      NColorBorder:= ColorBorderActive
    else
      NColorBorder:= ColorBorderPassive;

    //paint rounded corners
    case FOptPosition of
      atpTop:
        begin
          CanvasPaintRoundedCorners(C, R, [acckLeftTop, acckRightTop], NColorEmpty, NColorBorder, NColorBg);
        end;
      atpBottom:
        begin
          Inc(R.Bottom, 1);
          CanvasPaintRoundedCorners(C, R, [acckLeftBottom, acckRightBottom], NColorEmpty, NColorBorder, NColorBg);
        end;
      atpLeft:
        begin
          CanvasPaintRoundedCorners(C, R, [acckLeftTop, acckLeftBottom], NColorEmpty, NColorBorder, NColorBg);
        end;
      atpRight:
        begin
          CanvasPaintRoundedCorners(C, R, [acckRightTop, acckRightBottom], NColorEmpty, NColorBorder, NColorBg);
        end;
    end;
  end;
end;

procedure TATTabs.DoPaintTabShape_C(C: TCanvas;
  ATabActive: boolean;
  ATabIndex: integer;
  const ARect: TRect;
  const PL1, PL2, PR1, PR2: TPoint);
var
  ColorPos: TATTabPosition;
  AColorBg, AColorBorder, AColorBorderLow: TColor;
begin
  if ATabActive then
  begin
    AColorBg:= GetTabBgColor_Active(ATabIndex);
    AColorBorder:= FColorBorderActive;
    AColorBorderLow:= clNone;
  end
  else
  begin
    AColorBg:= GetTabBgColor_Passive(ATabIndex);
    AColorBorder:= FColorBorderPassive;
    AColorBorderLow:= FColorBorderActive;
  end;

  if FOptShowFlat then
  begin
    if ATabActive then
    begin
      ColorPos:= FOptPosition;
      if FOptShowActiveMarkInverted then
        ColorPos:= GetPositionInverted(ColorPos);
      DoPaintColoredBand(C, ARect, FColorActiveMark, ColorPos);
    end;
  end
  else
  case FOptPosition of
    atpTop:
      begin
        if FLastSpaceSide=0 then
          DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, AColorBorder);
        if FLastSpaceSide=0 then
          DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, AColorBorder);
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, AColorBorder);
        if AColorBorderLow<>clNone then
          DrawLine(C, PL2.X-DoScale(FLastSpaceSide), ARect.Bottom,
                      PR2.X+DoScale(FLastSpaceSide), ARect.Bottom, AColorBorderLow)
        else
          DrawLine(C, PL2.X+1, ARect.Bottom, PR2.X-1, ARect.Bottom, AColorBg);
      end;
    atpBottom:
      begin
        DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, AColorBorder);
        DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y+1, PR2.X, PL2.Y+1, AColorBorder);
        if AColorBorderLow<>clNone then
          DrawLine(C, PL1.X-DoScale(FLastSpaceSide), ARect.Top,
                      PR1.X+DoScale(FLastSpaceSide), ARect.Top, AColorBorderLow)
      end;
    atpLeft:
      begin
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PR1.Y, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y, PR2.X, PR2.Y, AColorBorder);
        DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y, AColorBorder);
        DrawLine(C, PR1.X+1, PR1.Y+1, PR1.X+1, PR2.Y-1, IfThen(AColorBorderLow<>clNone, AColorBorderLow, AColorBg));
      end;
    atpRight:
      begin
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PR1.Y, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y, PR2.X, PR2.Y, AColorBorder);
        DrawLine(C, PL1.X-1, PL1.Y+1, PL1.X-1, PL2.Y-1, IfThen(AColorBorderLow<>clNone, AColorBorderLow, AColorBg));
        DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y, AColorBorder);
      end;
  end;
end;

procedure TATTabs.DoPaintTabShape_L(C: TCanvas; const ARect: TRect; ATabActive: boolean; ATabIndex: integer);
var
  AColorBg, AColorBorder: TColor;
begin
  if ATabActive then
  begin
    AColorBg:= GetTabBgColor_Active(ATabIndex);
    AColorBorder:= FColorBorderActive
  end
  else
  begin
    AColorBg:= GetTabBgColor_Passive(ATabIndex);
    AColorBorder:= FColorBorderPassive;
  end;

  if not FOptShowFlat then
    case FOptPosition of
      atpTop:
        begin
          DrawTriangleRectFramed(C,
            ARect.Left-DoScale(FLastSpaceSide)+1,
            ARect.Top,
            DoScale(FLastSpaceSide),
            DoScale(FOptTabHeight)+IfThen(ATabActive, 1),
            cSmoothScale,
            ampnTopLeft,
            AColorBg,
            AColorBorder,
            FBitmapAngleL);
        end;
      atpBottom:
        begin
          DrawTriangleRectFramed(C,
            ARect.Left-DoScale(FLastSpaceSide)+1,
            ARect.Top+IfThen(not ATabActive, 1),
            DoScale(FLastSpaceSide),
            DoScale(FOptTabHeight),
            cSmoothScale,
            ampnBottomLeft,
            AColorBg,
            AColorBorder,
            FBitmapAngleL);
        end;
    end;
end;

procedure TATTabs.DoPaintTabShape_R(C: TCanvas; const ARect: TRect;
  ATabActive: boolean; ATabIndex: integer);
var
  AColorBg, AColorBorder: TColor;
begin
  if ATabActive then
  begin
    AColorBg:= GetTabBgColor_Active(ATabIndex);
    AColorBorder:= FColorBorderActive
  end
  else
  begin
    AColorBg:= GetTabBgColor_Passive(ATabIndex);
    AColorBorder:= FColorBorderPassive;
  end;

  if not FOptShowFlat then
    case FOptPosition of
      atpTop:
        begin
          DrawTriangleRectFramed(C,
            ARect.Right-1,
            ARect.Top,
            DoScale(FLastSpaceSide),
            DoScale(FOptTabHeight)+IfThen(ATabActive, 1),
            cSmoothScale,
            ampnTopRight,
            AColorBg,
            AColorBorder,
            FBitmapAngleR);
        end;
      atpBottom:
        begin
          DrawTriangleRectFramed(C,
            ARect.Right-1,
            ARect.Top+IfThen(not ATabActive, 1),
            DoScale(FLastSpaceSide),
            DoScale(FOptTabHeight),
            cSmoothScale,
            ampnBottomRight,
            AColorBg,
            AColorBorder,
            FBitmapAngleR);
        end;
    end;
end;


procedure TATTabs.DoPaintX(C: TCanvas; const AInfo: TATTabPaintInfo);
var
  ElemType: TATTabElemType;
begin
  if AInfo.TabMouseOverX then
    ElemType:= aeTabIconXOver
  else
    ElemType:= aeTabIconX;

  if IsPaintNeeded(ElemType, -1, C, AInfo.RectX) then
  begin
    DoPaintXTo(C, AInfo);
    DoPaintAfter(ElemType, -1, C, AInfo.RectX);
  end;
end;

procedure TATTabs.DoPaintXTo(C: TCanvas; const AInfo: TATTabPaintInfo);
var
  RectRound, RectBitmap: TRect;
  NColorBg, NColorXBg, NColorXBorder, NColorXMark: TColor;
  NSize: integer;
begin
  if AInfo.TabActive then
    NColorBg:= GetTabBgColor_Active(AInfo.TabIndex)
  else
    NColorBg:= GetTabBgColor_Passive(AInfo.TabIndex);
  GetTabXColors(AInfo.TabIndex, AInfo.TabMouseOverX, NColorXBg, NColorXBorder, NColorXMark);

  if FOptShowModifiedColorOnX then
    if AInfo.Modified or
       AInfo.Modified2 or
       AInfo.ExtModified or
       AInfo.ExtModified2 or
       AInfo.ExtDeleted or
       AInfo.ExtDeleted2 then
      NColorXMark:= FColorFontModified;

  if FOptShowXRounded and ATTabsCircleDrawEnabled then
  begin
    if NColorXBg<>clNone then
    begin
      RectRound:= AInfo.RectX;
      NSize:= DoScale(FOptSpaceXIncrementRound);
      InflateRect(RectRound, NSize, NSize);

      RectBitmap.Left:= 0;
      RectBitmap.Top:= 0;
      RectBitmap.Right:= FBitmapRound.Width;
      RectBitmap.Bottom:= RectBitmap.Right;

      FBitmapRound.Canvas.Brush.Color:= NColorBg;
      FBitmapRound.Canvas.FillRect(RectBitmap);

      FBitmapRound.Canvas.Brush.Color:= NColorXBg;
      FBitmapRound.Canvas.Pen.Color:= NColorXBorder;
      FBitmapRound.Canvas.Ellipse(RectBitmap);

      CanvasStretchDraw(C, RectRound, FBitmapRound);
    end
    else
    begin
      C.Brush.Color:= NColorBg;
      C.FillRect(AInfo.RectX);
    end;
  end
  else
  begin
    C.Brush.Color:= IfThen(NColorXBg<>clNone, NColorXBg, NColorBg);
    C.FillRect(AInfo.RectX);
    C.Pen.Color:= IfThen(NColorXBorder<>clNone, NColorXBorder, NColorBg);
    C.Rectangle(AInfo.RectX);
  end;

  RectRound:= AInfo.RectX;
  Dec(RectRound.Right);
  Dec(RectRound.Bottom);
  NSize:= DoScale(FOptSpaceXInner);
  CanvasPaintXMark(C, RectRound, NColorXMark, NSize, NSize, DoScale(1));
  C.Brush.Color:= NColorBg;
end;

function TATTabs.GetTabWidth_Plus_Raw: integer;
begin
  Result:= DoScale(FOptArrowSize)*4;
end;

function TATTabs.GetTabRectWidth(APlusBtn: boolean): integer;
begin
  case FOptPosition of
    atpLeft,
    atpRight:
      begin
        Result:= Width-DoScale(FOptSpacer);
      end;
    else
      begin
        if APlusBtn then
          Result:= GetTabWidth_Plus_Raw
        else
          Result:= DoScale(FOptTabWidthNormal);
        Inc(Result, DoScale(FOptSpaceBeforeText+FOptSpaceAfterText));
      end;
  end;
end;


function TATTabs.GetRectScrolled(const R: TRect): TRect;
begin
  Result:= R;
  if Result=cRect0 then Exit;
  if not FActualMultiline then
  begin
    Dec(Result.Left, FScrollPos);
    Dec(Result.Right, FScrollPos);
  end
  else
  begin
    Dec(Result.Top, FScrollPos);
    Dec(Result.Bottom, FScrollPos);
  end;
end;

procedure TATTabs.GetTabFirstCoord(var R: TRect);
begin
  if FOptPosition in [atpLeft, atpRight] then
  begin
    if FOptPosition=atpLeft then
    begin
      R.Left:= DoScale(FOptSpacer);
      R.Right:= Width-DoScale(FOptSpacer2);
    end
    else
    begin
      R.Left:= DoScale(FOptSpacer2)+1;
      R.Right:= Width-DoScale(FOptSpacer);
    end;
    R.Bottom:= GetInitialVerticalIndent;
    R.Top:= R.Bottom;
  end
  else
  begin
    R.Left:= FRealIndentLeft+DoScale(FLastSpaceSide);
    R.Right:= R.Left;
    R.Top:= DoScale(FOptSpacer);
    R.Bottom:= R.Top+DoScale(FOptTabHeight);
  end;
end;

procedure TATTabs.UpdateTabRects(C: TCanvas);
var
  TempCaption: TATTabString;
  Data: TATTabData;
  R: TRect;
  Extent: TSize;
  NWidthPlus, NIndexLineStart, NLineHeight, NWidthSaved: integer;
  NWidthMin, NWidthMax: integer;
  NSelfHeight, NFormHeight: integer;
  bFitLastRow: boolean;
  i: integer;
begin
  GetTabFirstCoord(R);

  //left/right tabs
  if FOptPosition in [atpLeft, atpRight] then
  begin
    for i:= 0 to TabCount-1 do
    begin
      Data:= GetTabData(i);
      if not Assigned(Data) then Continue;
      if not Data.TabVisible then
      begin
        Data.TabRect:= cRect0;
        Continue;
      end;

      R.Top:= R.Bottom;
      if i>0 then
        Inc(R.Top, DoScale(FOptSpaceBetweenTabs));

      if Data.TabSpecialHeight>0 then
        NLineHeight:= Data.TabSpecialHeight
      else
      if FOptVarWidth then
      begin
        UpdateCaptionProps(C, GetTabCaptionFinal(Data, i), NLineHeight, Extent);
        NLineHeight:= DoScale(FOptSpaceBeforeText+FOptSpaceAfterText) + Extent.CY;
      end
      else
        NLineHeight:= DoScale(FOptTabHeight);

      R.Bottom:= R.Top + NLineHeight;
      Data.TabRect:= R;
    end;

    exit;
  end;

  //top/bottom tabs
  FMultilineActive:= false;
  NWidthMin:= DoScale(FOptTabWidthMinimal);
  NWidthMax:= DoScale(FOptTabWidthMaximal);
  NWidthPlus:= 0;
  if FOptShowPlusTab then
    NWidthPlus:= GetTabRectWidth(true);
  if FOptMultiline then
    FTabWidth:= DoScale(FOptTabWidthNormal);
  NWidthSaved:= FTabWidth;

  NIndexLineStart:= 0;

  for i:= 0 to TabCount-1 do
  begin
    Data:= GetTabData(i);
    if not Assigned(Data) then Continue;
    if not Data.TabVisible then Continue;
    Data.TabStartsNewLine:= false;

    R.Left:= R.Right;
    if i>0 then
      Inc(R.Left, DoScale(FOptSpaceBetweenTabs));

    if Data.TabSpecialWidth>0 then
      FTabWidth:= Data.TabSpecialWidth
    else
    if FOptVarWidth then
    begin
      C.Font.Style:= Data.TabFontStyle;

      if FOptActiveFontStyleUsed then
        if i=FTabIndex then
          C.Font.Style:= FOptActiveFontStyle;

      TempCaption:= GetTabCaptionFinal(Data, i);

      UpdateCaptionProps(C, TempCaption, NLineHeight, Extent);
      FTabWidth:= Extent.CX + DoScale(FOptSpaceBeforeText+FOptSpaceAfterText);

      if not Assigned(FImages) then //no imagelist
        Data.TabImageIndex:= -1;

      if Data.TabImageIndex>=0 then
        if FOptIconPosition in [aipIconLefterThanText, aipIconRighterThanText] then
          Inc(FTabWidth, FImages.Width+FOptSpaceBetweenIconCaption);

      if FOptShowXButtons<>atbxShowNone then
        if not Data.TabHideXButton then
          Inc(FTabWidth, DoScale(FOptSpaceXSize));

      if FTabWidth<NWidthMin then
        FTabWidth:= NWidthMin;
      if FTabWidth>NWidthMax then
        FTabWidth:= NWidthMax;
    end;

    if FOptMultiline and (i>0) then
      if R.Left+FTabWidth+FRealIndentRight+NWidthPlus >= Width then
      begin
        Data.TabStartsNewLine:= true;
        FMultilineActive:= true;

        R.Left:= FRealIndentLeft;
        R.Top:= R.Bottom+DoScale(FOptSpaceBetweenLines);
        R.Bottom:= R.Top+DoScale(FOptTabHeight);

        if FOptFillWidth then
          UpdateTabRectsToFillLine(NIndexLineStart, i-1, false);
        NIndexLineStart:= i;
      end;

    R.Right:= R.Left + FTabWidth;
    Data.TabRect:= R;
  end;

  //fix for the case of many tabs, vertically scrolled, and last tab is not shrinked
  bFitLastRow:= FOptFillWidthLastToo or (Width<FOptTabWidthNormal);

  if FOptFillWidth and bFitLastRow then
    UpdateTabRectsToFillLine(NIndexLineStart, TabCount-1, true);

  if FOptMultiline then
  begin
    NFormHeight:= GetParentForm(Self).Height;
    NSelfHeight:= R.Bottom+DoScale(FOptSpacer2);
    NSelfHeight:= Min(NSelfHeight, NFormHeight);
    if Constraints.MaxHeight>0 then
      NSelfHeight:= Min(NSelfHeight, Constraints.MaxHeight);
    if Height<>NSelfHeight then
      Height:= NSelfHeight;
    //Application.MainForm.Caption:='newh '+inttostr(NSelfHeight)+', tabs '+inttostr(TabCount);
  end;

  //restore FTabWidth for other methods
  if not FOptVarWidth then
    FTabWidth:= NWidthSaved;
end;

function TATTabs.GetTabLastVisibleIndex: integer;
var
  Data: TATTabData;
begin
  Result:= TabCount;
  repeat
    Dec(Result);
    if Result<0 then Break;
    Data:= GetTabData(Result);
    if Assigned(Data) and Data.TabVisible then Break;
  until false;
end;

procedure TATTabs.UpdateTabRectsSpecial;
var
  Data: TATTabData;
  NIndex: integer;
begin
  NIndex:= GetTabLastVisibleIndex;
  Data:= GetTabData(NIndex);
  if Assigned(Data) then
  begin
    FRectTabLast_NotScrolled:= Data.TabRect;
    FRectTabLast_Scrolled:= GetRectScrolled(FRectTabLast_NotScrolled);
  end
  else
  begin
    FRectTabLast_NotScrolled:= cRect0;
    FRectTabLast_Scrolled:= cRect0;
  end;

  UpdateRectPlus(FRectTabPlus_NotScrolled);
  FRectTabPlus_Scrolled:= GetRectScrolled(FRectTabPlus_NotScrolled);
end;

procedure TATTabs.UpdateTabPropsX;
var
  D: TATTabData;
  i: integer;
begin
  for i:= 0 to TabCount-1 do
  begin
    D:= GetTabData(i);
    if D=nil then Continue;
    D.TabVisibleX:= GetTabVisibleX(i, D);
    if D.TabVisibleX and (D.TabRect<>cRect0) then
      D.TabRectX:= GetTabRect_X(D.TabRect)
    else
      D.TabRectX:= cRect0;
  end;
end;

procedure TATTabs.UpdateRectPlus(var R: TRect);
var
  bTabsVisible: boolean;
begin
  bTabsVisible:= GetTabLastVisibleIndex>=0;
  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        if bTabsVisible then
        begin
          R:= FRectTabLast_NotScrolled;
          if R=cRect0 then exit;
          R.Left:= R.Right + DoScale(FOptSpaceBetweenTabs);
          R.Right:= R.Left + GetTabRectWidth(true);
        end
        else
        begin
          R.Top:= DoScale(FOptSpacer);
          R.Bottom:= R.Top + DoScale(FOptTabHeight);
          R.Left:= FRealIndentLeft + FLastSpaceSide;
          R.Right:= R.Left + GetTabRectWidth(true);
        end;
      end;
    else
      begin
        if bTabsVisible then
        begin
          R:= FRectTabLast_NotScrolled;
          if R=cRect0 then exit;
          R.Top:= R.Bottom + DoScale(FOptSpaceBetweenTabs);
          R.Bottom:= R.Top + DoScale(FOptTabHeight);
        end
        else
        begin
          R.Left:= IfThen(FOptPosition=atpLeft, DoScale(FOptSpacer), DoScale(FOptSpacer2));
          R.Right:= IfThen(FOptPosition=atpLeft, Width-DoScale(FOptSpacer2), Width-DoScale(FOptSpacer));
          R.Top:= GetInitialVerticalIndent;
          R.Bottom:= R.Top + DoScale(FOptTabHeight);
        end;
      end;
  end;
end;

function TATTabs.GetTabRect_X(const ARect: TRect): TRect;
var
  X, Y, W: integer;
begin
  if ARect=cRect0 then
  begin
    Result:= cRect0;
    Exit;
  end;
  X:= ARect.Right-DoScale(FOptSpaceXRight);
  Y:= (ARect.Top+ARect.Bottom) div 2 + 1;
  W:= DoScale(FOptSpaceXSize);
  Dec(X, W div 2);
  Dec(Y, W div 2);
  Result:= Rect(X, Y, X+W, Y+W);
end;

function TATTabs._IsDrag: boolean;
begin
  Result:= Dragging and FMouseDragBegins;
end;

procedure TATTabs.GetTabXColors(AIndex: integer;
  AMouseOverX: boolean;
  out AColorXBg, AColorXBorder, AColorXMark: TColor);
begin
  if (FTabIndex = AIndex) or (FTabIndexOver=AIndex) then
    AColorXBg := FColorTabActive
  else
    AColorXBg := FColorBg;

  AColorXBorder:= AColorXBg;
  AColorXMark:= FColorCloseX;

  if AMouseOverX then
  begin
    AColorXBg:= FColorCloseBgOver;
    AColorXBorder:= FColorCloseBorderOver;
    AColorXMark:= FColorCloseXOver;
  end;
end;

procedure TATTabs.GetTabXProps(AIndex: integer; const ARect: TRect;
  out AMouseOverX: boolean;
  out ARectX: TRect);
var
  Data: TATTabData;
begin
  AMouseOverX:= false;
  ARectX:= cRect0;

  Data:= GetTabData(AIndex);
  if Data=nil then Exit;
  ARectX:= GetRectScrolled(Data.TabRectX);

  if _IsDrag then Exit;

  if Data.TabVisibleX then
    if AIndex=FTabIndexOver then
    begin
      AMouseOverX:= PtInRect(ARectX, ScreenToClient(Mouse.CursorPos));
    end;
end;

function TATTabs.IsPaintNeeded(AElemType: TATTabElemType;
  AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= ARect.Right>ARect.Left;
  if Result then
    if Assigned(FOnTabDrawBefore) then
      FOnTabDrawBefore(Self, AElemType, AIndex, ACanvas, ARect, Result);
end;

function TATTabs.DoPaintAfter(AElemType: TATTabElemType;
  AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnTabDrawAfter) then
    FOnTabDrawAfter(Self, AElemType, AIndex, ACanvas, ARect, Result);
end;

procedure TATTabs.DoPaintBgTo(C: TCanvas; const ARect: TRect);
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ARect);
end;

procedure TATTabs.DoPaintSpacerRect(C: TCanvas);
var
  ElemType: TATTabElemType;
  RBottom: TRect;
  NLineX1, NLineY1, NLineX2, NLineY2: integer;
begin
  if FOptMultiline and FScrollingNeeded then exit;

  case FOptPosition of
    atpTop:
      begin
        if FOptMultiline then
          RBottom:= Rect(0, Height-DoScale(FOptSpacer2), Width, Height)
        else
          RBottom:= Rect(0, DoScale(FOptSpacer)+DoScale(FOptTabHeight), Width, Height);
        NLineX1:= RBottom.Left;
        NLineY1:= RBottom.Top;
        NLineX2:= RBottom.Right;
        NLineY2:= RBottom.Top;
      end;
    atpBottom:
      begin
        RBottom:= Rect(0, 0, Width, DoScale(FOptSpacer));
        NLineX1:= RBottom.Left;
        NLineY1:= RBottom.Bottom;
        NLineX2:= RBottom.Right;
        NLineY2:= RBottom.Bottom;
      end;
    atpLeft:
      begin
        RBottom:= Rect(Width-DoScale(FOptSpacer2), 0, Width, Height);
        NLineX1:= RBottom.Left;
        NLineY1:= RBottom.Top;
        NLineX2:= RBottom.Left;
        NLineY2:= RBottom.Bottom;
      end;
    atpRight:
      begin
        RBottom:= Rect(0, 0, DoScale(FOptSpacer2), Height);
        NLineX1:= RBottom.Right;
        NLineY1:= RBottom.Top;
        NLineX2:= RBottom.Right;
        NLineY2:= RBottom.Bottom;
      end;
    else
      raise Exception.Create('Unknown tab pos');
  end;

  ElemType:= aeSpacerRect;
  if IsPaintNeeded(ElemType, -1, C, RBottom) then
  begin
    C.Brush.Color:= FColorTabActive;
    C.FillRect(RBottom);
    DrawLine(C, NLineX1, NLineY1, NLineX2, NLineY2, FColorBorderActive);
    DoPaintAfter(ElemType, -1, C, RBottom);
  end;
end;

procedure TATTabs.DoPaintTo(C: TCanvas);
var
  RRect, RRectXMark: TRect;
  NColorFont: TColor;
  ElemType: TATTabElemType;
  Data: TATTabData;
  NFontStyle: TFontStyles;
  bMouseOver, bMouseOverX: boolean;
  Info: TATTabPaintInfo;
  i: integer;
begin
  FActualMultiline:= (FOptPosition in [atpLeft, atpRight]) or FOptMultiline;

  ElemType:= aeBackground;
  RRect:= ClientRect;

  if not ATTabsStretchDrawEnabled then
    FLastSpaceSide:= 0
  else
  if Width>=DoScale(FOptMinimalWidthForSides) then
    FLastSpaceSide:= FOptSpaceSide
  else
    FLastSpaceSide:= 0;

  //update index here, because user can add/del tabs by keyboard
  with ScreenToClient(Mouse.CursorPos) do
    FTabIndexOver:= GetTabAt(X, Y, bMouseOverX);

  FLastOverIndex:= FTabIndexOver;
  FLastOverX:= bMouseOverX;

  FRealIndentLeft:= DoScale(FOptSpaceInitial) + GetButtonsWidth(FButtonsLeft);
  FRealIndentRight:= DoScale(FOptSpaceInitial) + GetButtonsWidth(FButtonsRight);

  FRectArrowLeft:= GetRectOfButton(atbScrollLeft);
  FRectArrowRight:= GetRectOfButton(atbScrollRight);
  FRectArrowDown:= GetRectOfButton(atbDropdownMenu);
  FRectButtonPlus:= GetRectOfButton(atbPlus);
  FRectButtonClose:= GetRectOfButton(atbClose);
  FRectButtonUser0:= GetRectOfButton(atbUser0);
  FRectButtonUser1:= GetRectOfButton(atbUser1);
  FRectButtonUser2:= GetRectOfButton(atbUser2);
  FRectButtonUser3:= GetRectOfButton(atbUser3);
  FRectButtonUser4:= GetRectOfButton(atbUser4);

  //painting of BG is little different then other elements:
  //paint fillrect anyway, then maybe paint ownerdraw
  DoPaintBgTo(C, RRect);
  if IsPaintNeeded(ElemType, -1, C, RRect) then
  begin
    DoPaintAfter(ElemType, -1, C, RRect);
  end;

  C.Font.Assign(Self.Font);
  UpdateTabWidths;
  UpdateTabRects(C);
  UpdateTabRectsSpecial;
  UpdateTabPropsX;
  FScrollingNeeded:= GetScrollMarkNeeded;

  //paint spacer rect
  if not FOptShowFlat then
    DoPaintSpacerRect(C);

  //paint "plus" tab
  if FOptShowPlusTab then
  begin
    DoPaintPlus(C, FRectTabPlus_Scrolled);
  end;

  //paint passive tabs
  for i:= TabCount-1 downto 0 do
    if i<>FTabIndex then
    begin
      Data:= GetTabData(i);
      if Data=nil then Continue;

      RRect:= GetRectScrolled(Data.TabRect);
      if RRect=cRect0 then Continue;

      GetTabXProps(i, RRect, bMouseOverX, RRectXMark);
      bMouseOver:= i=FTabIndexOver;

      if bMouseOver then
        ElemType:= aeTabPassiveOver
      else
        ElemType:= aeTabPassive;

      Info.Clear;
      Info.Rect:= RRect;
      Info.RectX:= RRectXMark;
      Info.Caption:= GetTabCaptionFinal(Data, i);
      Info.Modified:= Data.TabModified;
      Info.Modified2:= Data.TabModified2;
      Info.ExtModified:= Data.TabExtModified;
      Info.ExtModified2:= Data.TabExtModified2;
      Info.ExtDeleted:= Data.TabExtDeleted;
      Info.ExtDeleted2:= Data.TabExtDeleted2;
      Info.Pinned:= Data.TabPinned;
      Info.TabIndex:= i;
      Info.ImageIndex:= Data.TabImageIndex;
      Info.TwoDocuments:= Data.TabTwoDocuments;
      Info.TabMouseOver:= bMouseOver;
      Info.TabMouseOverX:= bMouseOverX;

      if IsPaintNeeded(ElemType, i, C, RRect) then
      begin
        if not Data.TabVisible then Continue;

        if FOptHotFontStyleUsed and bMouseOver then
          NFontStyle:= FOptHotFontStyle
        else
          NFontStyle:= Data.TabFontStyle;

        if (FColorFontHot<>clNone) and bMouseOver then
          NColorFont:= FColorFontHot
        else
        if Data.TabModified or Data.TabModified2 then
          NColorFont:= FColorFontModified
        else
        if Data.TabFontColor<>clNone then
          NColorFont:= Data.TabFontColor
        else
          NColorFont:= FColorFont;

        Info.FontStyle:= NFontStyle;
        Info.ColorFont:= NColorFont;

        DoPaintTabTo(C, Info);
        DoPaintAfter(ElemType, i, C, RRect);
      end;

      if Data.TabVisibleX then
      begin
        DoPaintX(C, Info);
      end;
    end;

  //paint active tab
  i:= FTabIndex;
  if IsIndexOk(i) then
  begin
    Data:= GetTabData(i);
    if Assigned(Data) and Data.TabVisible then
    begin
      RRect:= GetRectScrolled(Data.TabRect);
      GetTabXProps(i, RRect, bMouseOverX, RRectXMark);

      bMouseOver:= i=FTabIndexOver;

      Info.Clear;
      Info.Rect:= RRect;
      Info.RectX:= RRectXMark;
      Info.Caption:= GetTabCaptionFinal(Data, i);
      Info.Modified:= Data.TabModified;
      Info.Modified2:= Data.TabModified2;
      Info.ExtModified:= Data.TabExtModified;
      Info.ExtModified2:= Data.TabExtModified2;
      Info.ExtDeleted:= Data.TabExtDeleted;
      Info.ExtDeleted2:= Data.TabExtDeleted2;
      Info.Pinned:= Data.TabPinned;
      Info.TabIndex:= i;
      Info.ImageIndex:= Data.TabImageIndex;
      Info.TwoDocuments:= Data.TabTwoDocuments;
      Info.TabActive:= true;
      Info.TabMouseOver:= bMouseOver;
      Info.TabMouseOverX:= bMouseOverX;
      Info.TabActive:= true;
      Info.TabMouseOverX:= bMouseOverX;

      if IsPaintNeeded(aeTabActive, i, C, RRect) then
      begin
        if FOptActiveFontStyleUsed then
          NFontStyle:= FOptActiveFontStyle
        else
          NFontStyle:= Data.TabFontStyle;

        if FColorFontActive<>clNone then
          NColorFont:= FColorFontActive
        else
        if Data.TabModified or Data.TabModified2 then
          NColorFont:= FColorFontModified
        else
        if Data.TabFontColor<>clNone then
          NColorFont:= Data.TabFontColor
        else
          NColorFont:= FColorFont;

        Info.FontStyle:= NFontStyle;
        Info.ColorFont:= NColorFont;

        DoPaintTabTo(C, Info);
        DoPaintAfter(aeTabActive, i, C, RRect);
      end;

      if Data.TabVisibleX then
      begin
        DoPaintX(C, Info);
      end;
    end;
  end;

  //button back
  DoPaintButtonsBG(C);
  //buttons
  DoPaintArrowLeft(C);
  DoPaintArrowRight(C);
  DoPaintArrowDown(C);
  DoPaintButtonPlus(C);
  DoPaintButtonClose(C);
  DoPaintUserButtons(C, FButtonsLeft, true);
  DoPaintUserButtons(C, FButtonsRight, false);

  if FOptShowDropMark then
    if _IsDrag then
      if PtInControl(Self, Mouse.CursorPos) then
        DoPaintDropMark(C);

  if FOptShowScrollMark then
    DoPaintScrollMark(C);
end;

procedure TATTabs.DoTextOut(C: TCanvas; AX, AY: integer;
  const AClipRect: TRect; const AText: string);
{$ifdef WIDE}
var
  Str: WideString;
begin
  Str:= UTF8Decode(AText);
  ExtTextOutW(C.Handle, AX, AY, ETO_CLIPPED, @AClipRect,
    PWideChar(Str), Length(Str), nil);
end;
{$else}
begin
  ExtTextOut(C.Handle, AX, AY, ETO_CLIPPED, @AClipRect,
    PChar(AText), Length(AText), nil);
end;
{$endif}

procedure TATTabs.DoPaintDropMark(C: TCanvas);
var
  D: TATTabData;
  R: TRect;
  N: integer;
begin
  N := FTabIndexDrop;
  if (N < 0) or (N = TabIndex) then
    Exit;
  //if N<>FTabIndex then
  begin
    D:= GetTabData(N);
    if D=nil then Exit;
    R:= GetRectScrolled(D.TabRect);

    case FOptPosition of
      atpTop,
      atpBottom:
        begin
          R.Left:= IfThen(N<=FTabIndex, R.Left, R.Right);
          R.Left:= R.Left - DoScale(FOptDropMarkSize) div 2;
          R.Right:= R.Left + DoScale(FOptDropMarkSize);
        end;
      else
        begin
          R.Top:= IfThen(N<=FTabIndex, R.Top, R.Bottom);
          R.Top:= R.Top  - DoScale(FOptDropMarkSize) div 2;
          R.Bottom:= R.Top + DoScale(FOptDropMarkSize);
        end;
    end;

    C.Brush.Color:= FColorDropMark;
    C.FillRect(R);
  end;
end;


function TATTabs.GetScrollMarkNeeded: boolean;
begin
  if TabCount=0 then
    Result:= false
  else
  if FScrollPos>0 then
    Result:= true
  else
  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        if not FOptVarWidth and not FOptMultiline then
          Result:= FTabWidth<=DoScale(FOptTabWidthMinimal)
        else
          Result:= GetMaxScrollPos>0;
      end;
    else
      begin
        Result:= GetMaxScrollPos>0;
      end;
  end;
end;

procedure TATTabs.DoPaintScrollMark(C: TCanvas);
var
  NPos, NSize, NIndent: integer;
  R: TRect;
begin
  if not FScrollingNeeded then exit;

  if not FActualMultiline then
  begin
    NPos:= GetMaxScrollPos;
    NSize:= Width - FRealIndentLeft - FRealIndentRight;

    if NPos>0 then
    begin
      R.Top:= IfThen(FOptPosition=atpBottom, DoScale(FOptTabHeight) + DoScale(FOptSpacer), 0);
      R.Bottom:= R.Top + DoScale(FOptScrollMarkSizeY);

      R.Left:= FRealIndentLeft +
        Max(0, Min(
          NSize-DoScale(FOptScrollMarkSizeX),
          Int64(FScrollPos) * (NSize-DoScale(FOptScrollMarkSizeX)) div NPos
        ));
      R.Right:= R.Left + DoScale(FOptScrollMarkSizeX);

      C.Brush.Color:= FColorScrollMark;
      C.FillRect(R);
    end;
  end
  else
  begin
    NIndent:= GetInitialVerticalIndent;
    NPos:= GetMaxScrollPos;
    NSize:= Height-NIndent;

    if NPos>0 then
    begin
      R.Top:= NIndent +
        Max(0, Min(
          NSize - DoScale(FOptScrollMarkSizeX),
          Int64(FScrollPos) * (NSize-DoScale(FOptScrollMarkSizeX)) div NPos
          ));
      R.Bottom:= R.Top + DoScale(FOptScrollMarkSizeX);

      if FOptPosition=atpLeft then
      begin
        R.Left:= 0;
        R.Right:= R.Left + DoScale(FOptScrollMarkSizeY);
      end
      else
      begin
        R.Right:= Width;
        R.Left:= R.Right - DoScale(FOptScrollMarkSizeY);
      end;

      C.Brush.Color:= FColorScrollMark;
      C.FillRect(R);
    end;
  end;
end;

procedure TATTabs.SetOptButtonLayout(const AValue: string);
begin
  //if FOptButtonLayout=AValue then Exit;
  FOptButtonLayout:= AValue;
  ApplyButtonLayout;
  Invalidate;
end;

procedure TATTabs.SetOptScalePercents(AValue: integer);
begin
  if FOptScalePercents=AValue then Exit;
  FOptScalePercents:= AValue;
  ApplyButtonLayout;
  Invalidate;
end;

procedure TATTabs.SetOptVarWidth(AValue: boolean);
begin
  if FOptVarWidth=AValue then Exit;
  FOptVarWidth:= AValue;
  if not AValue then
    FScrollPos:= 0;
  Invalidate;
end;


function TATTabs.GetTabAt(AX, AY: integer; out APressedX: boolean): integer;
var
  Pnt: TPoint;
  RectTab: TRect;
  D: TATTabData;
  ok: boolean;
  i: integer;
begin
  Result:= cTabIndexNone;
  APressedX:= false;
  Pnt:= Point(AX, AY);

  if PtInRect(FRectArrowLeft, Pnt) then
  begin
    Result:= cTabIndexArrowScrollLeft;
    Exit
  end;

  if PtInRect(FRectArrowRight, Pnt) then
  begin
    Result:= cTabIndexArrowScrollRight;
    Exit
  end;

  if PtInRect(FRectArrowDown, Pnt) then
  begin
    Result:= cTabIndexArrowMenu;
    Exit
  end;

  if PtInRect(FRectButtonPlus, Pnt) then
  begin
    Result:= cTabIndexPlusBtn;
    Exit
  end;

  if PtInRect(FRectButtonClose, Pnt) then
  begin
    Result:= cTabIndexCloseBtn;
    Exit
  end;

  if PtInRect(FRectButtonUser0, Pnt) then
  begin
    Result:= cTabIndexUser0;
    Exit
  end;

  if PtInRect(FRectButtonUser1, Pnt) then
  begin
    Result:= cTabIndexUser1;
    Exit
  end;

  if PtInRect(FRectButtonUser2, Pnt) then
  begin
    Result:= cTabIndexUser2;
    Exit
  end;

  if PtInRect(FRectButtonUser3, Pnt) then
  begin
    Result:= cTabIndexUser3;
    Exit
  end;

  if PtInRect(FRectButtonUser4, Pnt) then
  begin
    Result:= cTabIndexUser4;
    Exit
  end;

  //normal tab?
  for i:= 0 to TabCount-1 do
  begin
    D:= GetTabData(i);
    if D=nil then Continue;
    if not D.TabVisible then Continue;

    RectTab:= GetRectScrolled(D.TabRect);
    if RectTab=cRect0 then Continue;

    //support drag-drop into area between tabs
    //we need to increase RectTab, because it doesn't contain inter-tab area
    if FOptPosition in [atpTop, atpBottom] then
      Dec(RectTab.Left, DoScale(FOptSpaceBetweenTabs))
    else
      Dec(RectTab.Top, DoScale(FOptSpaceBetweenTabs));

    if FActualMultiline then
    begin
      if RectTab.Top>Pnt.Y then Break;
    end
    else
    begin
      if RectTab.Left>Pnt.X then Break;
    end;

    if PtInRect(RectTab, Pnt) then
    begin
      Result:= i;
      APressedX:= D.TabVisibleX and PtInRect(GetRectScrolled(D.TabRectX), Pnt);
      Exit;
    end;
  end;

  //plus tab?
  if FOptShowPlusTab then
    if PtInRect(FRectTabPlus_Scrolled, Pnt) then
    begin
      Result:= cTabIndexPlus;
      Exit
    end;

  //empty area after last tab?
  RectTab:= FRectTabLast_Scrolled;
  if RectTab<>cRect0 then
  begin
    if FOptPosition in [atpTop, atpBottom] then
      ok:= (AX>=RectTab.Right) and (AY>=RectTab.Top) and (AY<RectTab.Bottom)
    else
      ok:= (AY>=RectTab.Bottom) and (AX>=RectTab.Left) and (AX<RectTab.Right);
    if ok then
    begin
      Result:= cTabIndexEmptyArea;
      Exit;
    end;
  end;
end;

procedure TATTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  bClick, bDblClick: boolean;
begin
  inherited;
  bClick:= FMouseDown and
    (Abs(X-FMouseDownPnt.X) < cTabsMouseMaxDistanceToClick) and
    (Abs(Y-FMouseDownPnt.Y) < cTabsMouseMaxDistanceToClick);
  bDblClick:= bClick and FMouseDownDbl;
  //bRightClick:= FMouseDownRightBtn and
  //  (Abs(X-FMouseDownPnt.X) < cTabsMouseMaxDistanceToClick) and
  //  (Abs(Y-FMouseDownPnt.Y) < cTabsMouseMaxDistanceToClick);

  FMouseDown:= false;
  FMouseDownDbl:= false;
  FMouseDownRightBtn:= false;
  FMouseDragBegins:= false;
  Cursor:= crDefault;
  Screen.Cursor:= crDefault;
  FTabIndexDrop:= -1;
  FTabIndexDropOld:= -1;

  if bDblClick then
  begin
    if Assigned(FOnTabDblClick) and (FTabIndexOver>=0) then
      FOnTabDblClick(Self, FTabIndexOver);

    if FOptMouseDoubleClickClose and (FTabIndexOver>=0) then
      DeleteTab(FTabIndexOver, true, true)
    else
    if FOptMouseDoubleClickPlus and (FTabIndexOver=cTabIndexEmptyArea) then
      if Assigned(FOnTabPlusClick) then
        FOnTabPlusClick(Self);
    Exit
  end;

  if bClick then
  begin
    DoHandleClick;
    Invalidate;
    Exit
  end;
end;

procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  bOverX: boolean;
begin
  inherited;
  FMouseDown:= Button in [mbLeft, mbMiddle]; //but not mbRight
  FMouseDownRightBtn:= (Button = mbRight);
  FMouseDownPnt:= Point(X, Y);
  FMouseDownButton:= Button;
  FMouseDownShift:= Shift;
  FMouseDragBegins:= false;

  FTabIndexOver:= GetTabAt(X, Y, bOverX);

  if Button=mbLeft then
    //activate tab only if not X clicked
    if not bOverX then
      //if TabIndex<>FTabIndexOver then //with this check, CudaText cannot focus active tab in passive tab-group
        TabIndex:= FTabIndexOver;

  Invalidate;
end;


procedure TATTabs.DoHandleClick;
var
  Action: TATTabActionOnClose;
  D: TATTabData;
  R: TRect;
begin
  if FMouseDownButton=mbMiddle then
  begin
    if FOptMouseMiddleClickClose then
      if FTabIndexOver>=0 then
        DeleteTab(FTabIndexOver, true, true);
    Exit;
  end;

  if FMouseDownButton=mbLeft then
  begin
    case FTabIndexOver of
      cTabIndexArrowMenu:
        begin
          EndDrag(false);
          FTabIndexOver:= -1;
          Invalidate;
          ShowTabMenu;
        end;

      cTabIndexArrowScrollLeft:
        DoScrollLeft;

      cTabIndexArrowScrollRight:
        DoScrollRight;

      cTabIndexUser0:
        DoClickUser(0);
      cTabIndexUser1:
        DoClickUser(1);
      cTabIndexUser2:
        DoClickUser(2);
      cTabIndexUser3:
        DoClickUser(3);
      cTabIndexUser4:
        DoClickUser(4);

      cTabIndexPlus,
      cTabIndexPlusBtn:
        begin
          EndDrag(false);
          FTabIndexOver:= -1;
          if Assigned(FOnTabPlusClick) then
            FOnTabPlusClick(Self);
        end;

      cTabIndexCloseBtn:
        begin
          Action:= aocDefault;
          if Assigned(FOnTabGetCloseAction) then
            FOnTabGetCloseAction(Self, Action);
          DeleteTab(FTabIndex, true, true, Action);
        end

      else
        begin
          D := GetTabData(FTabIndexOver);
          if Assigned(D) and D.TabVisibleX then
          begin
            R := GetRectScrolled(D.TabRectX);

            if PtInRect(R, FMouseDownPnt) then
            begin
              FIsClickingCloseButton := True;
              try
                EndDrag(false);
                DeleteTab(FTabIndexOver, true, true);
              finally
                FIsClickingCloseButton := False;
              end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TATTabs.DoHandleRightClick;
var
  P: TPoint;
  D: TATTabData;
begin
  if (FTabIndex=FTabIndexOver) then // to check if click was processed as a valid click on a tab
  begin
    D:= GetTabData(FTabIndex);
    if Assigned(D) and Assigned(D.TabPopupMenu) then
    begin
      P:= ClientToScreen(FMouseDownPnt);
      D.TabPopupMenu.PopUp(P.X, P.Y);

      //fixing ATFlatControls #80
      FTabIndexDrop:= cTabIndexNone;
    end;
  end;
end;

type
  TControlHack = class(TControl);

procedure TATTabs.MouseMove(Shift: TShiftState; X, Y: integer);
var
  bOverX: boolean;
  Data: TATTabData;
begin
  inherited;

  if TabCount=0 then
  begin
    Invalidate; //cleans up <> v and x highlights if no tabs
    exit;
  end;

  // LCL dragging with DragMode=automatic is started too early.
  // so use DragMode=manual and DragStart.
  if OptMouseDragEnabled and FMouseDown and (FMouseDownButton=mbLeft) and (not _IsDrag) and (GetTabAt(FMouseDownPnt.X,FMouseDownPnt.Y,bOverX) >= 0) then
  begin
    BeginDrag(false, Mouse.DragThreshold);
    {$ifdef fpc}
    //needed for Lazarus, when dragging tab below the control to another ATTabs
    //but it's bad for Delphi
    Screen.Cursor:= crDrag;
    {$endif}
    Exit
  end;

  FTabIndexOver:= GetTabAt(X, Y, bOverX);
  if (FTabIndexOver <> cTabIndexEmptyArea) then
    FTabIndexDrop := FTabIndexOver;
  if FTabIndexOver=cTabIndexNone then exit;
  Data:= nil;

  if ShowHint then
  begin
    if bOverX then
      FTabIndexHinted:= cTabIndexCloseBtn
    else
      FTabIndexHinted:= FTabIndexOver;

    if FTabIndexHinted<>FTabIndexHintedPrev then
    begin
      FTabIndexHintedPrev:= FTabIndexHinted;
      Hint:= '';
      case FTabIndexHinted of
        cTabIndexPlus,
        cTabIndexPlusBtn:
          Hint:= FHintForPlus;
        cTabIndexArrowScrollLeft:
          Hint:= FHintForArrowLeft;
        cTabIndexArrowScrollRight:
          Hint:= FHintForArrowRight;
        cTabIndexArrowMenu:
          Hint:= FHintForArrowMenu;
        cTabIndexCloseBtn:
          Hint:= FHintForX;
        cTabIndexUser0:
          Hint:= FHintForUser0;
        cTabIndexUser1:
          Hint:= FHintForUser1;
        cTabIndexUser2:
          Hint:= FHintForUser2;
        cTabIndexUser3:
          Hint:= FHintForUser3;
        cTabIndexUser4:
          Hint:= FHintForUser4;
        0..10000:
          begin
            Data:= GetTabData(FTabIndexOver);
            if Assigned(Data) and (Data.TabHint<>'') then
              Hint:= Data.TabHint;
          end;
      end; //case

      if Hint<>'' then
        Application.ActivateHint(Mouse.CursorPos)
      else
      begin
        Application.HideHint;
        FTabIndexHintedPrev:= cTabIndexNone;
      end;
    end;
  end; //if ShowHint

  if Assigned(Data) then
    if Assigned(FOnTabOver) then
      FOnTabOver(Self, FTabIndexOver);

  //repaint only if really needed
  //use {$define tab_paint_counter} to debug it
  if (FTabIndexOver<>FLastOverIndex) or (bOverX<>FLastOverX) then
  begin
    Invalidate;
  end;
end;

function TATTabs.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  bToRight: boolean;
  bSwitchTab: boolean;
begin
  Result:= false;
  bSwitchTab:= false;

  case FOptMouseWheelMode of
    amwIgnoreWheel:
      exit;
    amwNormalScroll:
      begin
        bSwitchTab:= ssShift in Shift;
        if bSwitchTab then exit;
      end;
    amwNormalScrollAndShiftSwitch:
      begin
        bSwitchTab:= ssShift in Shift;
      end;
    amwNormalSwitch:
      begin
        bSwitchTab:= not (ssShift in Shift);
        if not bSwitchTab then exit;
      end;
    amwNormalSwitchAndShiftScroll:
      begin
        bSwitchTab:= not (ssShift in Shift);
      end;
  end;

  bToRight:= WheelDelta<0;

  if bSwitchTab then
  begin
    SwitchTab(bToRight, false{LoopAtEdge});
  end
  else
  begin
    if bToRight then
      DoScrollRight
    else
      DoScrollLeft;
  end;

  Result:= true;
end;

procedure TATTabs.Resize;
begin
  inherited;
  FTabsResized:= true;

  if Assigned(FBitmap) then
    BitmapResizeBySteps(FBitmap, Width, Height);

  if FOptActiveVisibleOnResize then
    if FTabIndex>=0 then
      MakeVisible(FTabIndex);

  //auto-scroll tabs to right when width is shrinked
  if FScrollPos>0 then
    FScrollPos:= Min(FScrollPos, GetMaxScrollPos);

  Invalidate;
end;


function TATTabs.AddTab(
  AIndex: integer;
  const ACaption: TATTabString;
  AObject: TObject = nil;
  AModified: boolean = false;
  AColor: TColor = clNone;
  AImageIndex: TImageIndex = -1): TATTabData;
var
  Data: TATTabData;
begin
  FTabsChanged:= true;

  Data:= TATTabData(FTabList.Add);
  if IsIndexOk(AIndex) then
    Data.Index:= AIndex
  else
    AIndex:= TabCount-1;

  Data.TabCaption:= ACaption;
  //Data.TabHint:= AHint;
  Data.TabObject:= AObject;
  Data.TabModified:= AModified;
  Data.TabColor:= AColor;
  Data.TabImageIndex:= AImageIndex;
  //Data.TabPopupMenu:= APopupMenu;
  //Data.TabFontStyle:= AFontStyle;
  //Data.TabSpecial:= ASpecial;

  FTabIndexHinted:= cTabIndexNone;
  FTabIndexHintedPrev:= cTabIndexNone;

  Invalidate;

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, -1, AIndex);

  if FTabIndex<0 then
    FTabIndex:= 0;

  Result:= Data;
end;

procedure TATTabs.AddTab(AIndex: integer; AData: TATTabData);
var
  Data: TATTabData;
begin
  Data:= TATTabData(FTabList.Add);
  Data.Assign(AData);
  if IsIndexOk(AIndex) then
    Data.Index:= AIndex
  else
  begin
    AIndex:= TabCount-1;
    Data.Index:= AIndex;
  end;

  FTabIndexHinted:= cTabIndexNone;
  FTabIndexHintedPrev:= cTabIndexNone;

  Invalidate;

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, -1, AIndex);
end;

function TATTabs.DeleteTab(AIndex: integer;
  AAllowEvent, AWithCancelBtn: boolean;
  AAction: TATTabActionOnClose=aocDefault): boolean;
  //
  procedure _ActivateRightTab;
  begin
    if FTabIndex>AIndex then
      SetTabIndexEx(FTabIndex-1, true)
    else
    if (FTabIndex=AIndex) and (FTabIndex>0) and (FTabIndex>=TabCount) then
      SetTabIndex(FTabIndex-1)
    else
    if FTabIndex=AIndex then
      SetTabIndex(FTabIndex);
  end;
  //
  procedure _ActivateRecentTab;
  var
    Idx, i: integer;
    Tick, TickMax: Int64;
  begin
    TickMax:= 0;
    Idx:= -1;
    for i:= 0 to TabCount-1 do
    begin
      Tick:= GetTabTick(i);
      if Tick>TickMax then
      begin
        TickMax:= Tick;
        Idx:= i;
      end;
    end;
    if Idx>=0 then
      SetTabIndex(Idx)
    else
      _ActivateRightTab;
  end;
  //
var
  CanClose, CanContinue: boolean;
  NMax: integer;
begin
  FTabsChanged:= true;
  FMouseDown:= false;

  if AAllowEvent then
  begin
    CanClose:= true;
    CanContinue:= AWithCancelBtn;

    if Assigned(FOnTabClose) then
      FOnTabClose(Self, AIndex, CanClose, CanContinue);

    if AWithCancelBtn and not CanContinue then
      begin Result:= false; Exit end;
    if not CanClose then
      begin Result:= true; Exit end;
  end;

  if IsIndexOk(AIndex) then
  begin
    FTabIndexHinted:= cTabIndexNone;
    FTabIndexHintedPrev:= cTabIndexNone;

    FTabList.Delete(AIndex);

    if AAction=aocDefault then
      AAction:= FOptWhichActivateOnClose;

    case AAction of
      aocNone:
        begin end;
      aocRight:
        _ActivateRightTab;
      aocRecent:
        _ActivateRecentTab;
      else
        _ActivateRightTab;
    end;

    //if lot of tabs were opened, and closed last tab, need to scroll all tabs righter
    NMax:= GetMaxScrollPos;
    if ScrollPos>NMax then
      ScrollPos:= NMax;

    Invalidate;

    if (TabCount=0) then
    begin
      FTabIndex:= -1;
      if Assigned(FOnTabEmpty) then
        FOnTabEmpty(Self);
    end;

    if Assigned(FOnTabMove) then
      FOnTabMove(Self, AIndex, -1);
  end;

  Result:= true;
end;

procedure TATTabs.SetTabIndex(AIndex: integer);
begin
  SetTabIndexEx(AIndex, false);
end;

procedure TATTabs.SetTabIndexEx(AIndex: integer; ADisableEvent: boolean);
//note: check "if AIndex=FTabIndex" must not be here, must be in outer funcs.
//Sometimes SetTabIndex(TabIndex) is used in CudaText: do focus of clicked tab, and in DeleteTab.
var
  bCanChange, bDisableEvent, bTabChanged: boolean;
  PrevMousePos, NextMousePos: TPoint;
begin
  if csLoading in ComponentState then
    FTabIndexLoaded:= AIndex;
  bDisableEvent:= (csLoading in ComponentState) or ADisableEvent;
  bTabChanged:= AIndex<>FTabIndex;

  if IsIndexOk(AIndex) then
  begin
    if Assigned(FOnTabChangeQuery) then
    begin
      bCanChange:= true;
      PrevMousePos:= Mouse.CursorPos;
      FOnTabChangeQuery(Self, AIndex, bCanChange);

      NextMousePos:= Mouse.CursorPos;
      if Abs(NextMousePos.X-PrevMousePos.X) + Abs(NextMousePos.Y-PrevMousePos.Y) >= 4 then
      begin
        if FMouseDown then
          MouseUp(mbLeft, [], 0, 0);
      end;

      if not bCanChange then Exit;
    end;

    FTabIndex:= AIndex;

    MakeVisible(AIndex);
    Invalidate;

    if not bDisableEvent then
    begin
      if Assigned(FOnTabClick) then
        FOnTabClick(Self);

      if Assigned(FOnTabChanged) and bTabChanged then
        FOnTabChanged(Self);
    end;
  end;
end;

function TATTabs.HideTab(AIndex: integer): boolean;
begin
  Result:= IsIndexOk(AIndex);
  if Result then
  begin
    GetTabData(AIndex).TabVisible:= false;
    // if the deleted tab has focus then this needs to shift to the next
    // tab or - if there are none - to the first
    if AIndex=TabIndex then
    begin
      if IsIndexOk(AIndex+1) then
        SetTabIndex(AIndex+1)
      else
        SetTabIndex(0);
    end;
  end;
end;

function TATTabs.ShowTab(AIndex: integer): boolean;
begin
  Result:= IsIndexOk(AIndex);
  if Result then
  begin
    GetTabData(AIndex).TabVisible:= true;
    SetTabIndex(AIndex);
  end;
end;

function TATTabs.GetTabData(AIndex: integer): TATTabData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATTabData(FTabList.Items[AIndex])
  else
    Result:= nil;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATTabs.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATTabs.DoPaintArrowTo(C: TCanvas; ATyp: TATTabTriangle; ARect: TRect;
  AActive, AEnabled: boolean);
var
  NColor: TColor;
begin
  if not AEnabled then
    NColor:= ColorBlendHalf(FColorArrow, FColorBg)
  else
  if AActive and not _IsDrag then
    NColor:= FColorArrowOver
  else
    NColor:= FColorArrow;

  DrawTriangleType(C, ATyp, ARect, NColor, DoScale(FOptArrowSize) div 2);
end;


function TATTabs.GetIndexOfButton(const AButtons: TATTabButtons; ABtn: TATTabButton): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Length(AButtons)-1 do
    if AButtons[i].Id=ABtn then
      begin Result:= i; exit; end;
end;

function TATTabs.GetButtonsEdgeCoord(AtLeft: boolean): integer;
begin
  if AtLeft then
  begin
    Result:= DoScale(FOptSpaceInitial);
    case FOptPosition of
      atpLeft:
        Inc(Result, DoScale(FOptSpacer));
      atpRight:
        Inc(Result, DoScale(FOptSpacer2));
    end;
  end
  else
  begin
    Result:= Width;
    case FOptPosition of
      atpLeft:
        Dec(Result, DoScale(FOptSpacer2));
      atpRight:
        Dec(Result, DoScale(FOptSpacer));
    end;
  end;
end;

function TATTabs.GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
var
  NPos, i: integer;
begin
  NPos:= GetButtonsEdgeCoord(AtLeft);
  if AtLeft then
  begin
    for i:= 0 to AIndex do
    begin
      Result.Left:= NPos;
      Result.Right:= Result.Left+FButtonsLeft[i].Size;
      NPos:= Result.Right;
    end;
  end
  else
  begin
    for i:= 0 to AIndex do
    begin
      Result.Right:= NPos;
      Result.Left:= Result.Right-FButtonsRight[i].Size;
      NPos:= Result.Left;
    end;
  end;

  if FOptPosition in [atpTop, atpBottom] then
    Result.Top:= DoScale(FOptSpacer)
  else
    Result.Top:= 0;

  Result.Bottom:= Result.Top+DoScale(FOptTabHeight);

  if FOptPosition=atpBottom then
    Inc(Result.Top);
end;

function TATTabs.GetRectOfButton(AButton: TATTabButton): TRect;
var
  N: integer;
begin
  Result:= cRect0;

  N:= GetIndexOfButton(FButtonsLeft, AButton);
  if N>=0 then
    Result:= GetRectOfButtonIndex(N, true)
  else
  begin
    N:= GetIndexOfButton(FButtonsRight, AButton);
    if N>=0 then
      Result:= GetRectOfButtonIndex(N, false);
  end;
end;


procedure TATTabs.ShowTabMenu;
var
  mi: TMenuItem;
  P: TPoint;
  i: integer;
  bShow: boolean;
  Data: TATTabData;
begin
  if Assigned(TabMenuExternal) then
  begin
    P:= Point(FRectArrowDown.Left, FRectArrowDown.Bottom);
    P:= ClientToScreen(P);
    TabMenuExternal.Popup(P.X, P.Y);
    exit;
  end;

  if TabCount=0 then Exit;

  bShow:= true;
  if Assigned(FOnTabMenu) then
    FOnTabMenu(Self, bShow);
  if not bShow then Exit;

  if not Assigned(FTabMenu) then
    FTabMenu:= TPopupMenu.Create(Self);
  FTabMenu.Items.Clear;

  for i:= 0 to TabCount-1 do
  begin
    Data:= GetTabData(i);
    if Data.TabVisible then
    begin
      mi:= TMenuItem.Create(Self);
      mi.Tag:= i;
      mi.Caption:= GetTabData(i).TabCaptionFull;
      mi.OnClick:= TabMenuClick;
      mi.RadioItem:= true;
      mi.Checked:= i=FTabIndex;
      FTabMenu.Items.Add(mi);
    end;
  end;
  P:= Point(FRectArrowDown.Left, FRectArrowDown.Bottom);
  P:= ClientToScreen(P);
  FTabMenu.Popup(P.X, P.Y);
end;

procedure TATTabs.TabMenuClick(Sender: TObject);
begin
  SetTabIndex((Sender as TComponent).Tag);
end;

procedure TATTabs.UpdateTabWidths;
var
  NValue, NCount: integer;
begin
  if FOptVarWidth then Exit;

  NCount:= TabCount;
  if NCount=0 then Exit;

  if FOptPosition in [atpLeft, atpRight] then
  begin
    FTabWidth:= Width-DoScale(FOptSpacer);
    exit
  end;

  //tricky formula: calculate auto-width
  NValue:= (Width
    - IfThen(FOptShowPlusTab, GetTabWidth_Plus_Raw + DoScale(FOptSpaceBeforeText+FOptSpaceAfterText))
    - FRealIndentLeft
    - FRealIndentRight
    - FOptSpaceSide
    ) div NCount
      - DoScale(FOptSpaceBetweenTabs);

  NValue:= Max(NValue, DoScale(FOptTabWidthMinimal));
  NValue:= Min(NValue, DoScale(FOptTabWidthNormal));

  FTabWidth:= NValue;
end;

function TATTabs.GetTabVisibleX(AIndex: integer; const D: TATTabData): boolean;
begin
  if Width<FOptMinimalWidthForSides then
    exit(false);

  case FOptShowXButtons of
    atbxShowNone:
      Result:= false;
    atbxShowAll:
      Result:= true;
    atbxShowActive:
      Result:= AIndex=FTabIndex;
    atbxShowMouseOver:
      Result:= AIndex=FTabIndexOver;
    atbxShowActiveAndMouseOver:
      Result:= (AIndex=FTabIndex) or (AIndex=FTabIndexOver);
    else
      Result:= false;
  end;

  if Result then
  begin
    if D.TabHideXButton then
    begin
      Result:= false;
      Exit
    end;

    if not FOptVarWidth then
      if FOptPosition in [atpTop, atpBottom] then
        if FTabWidth<DoScale(FOptTabWidthMinimalHidesX) then
        begin
          Result:= false;
          Exit
        end;
  end;
end;

function TATTabs.IsDraggingAllowed: boolean;
var
  NFrom, NTo: integer;
begin
  Result:= false;
  NFrom:= FTabIndex;
  if not IsIndexOk(NFrom) then Exit;
  NTo:= FTabIndexDrop;
  if not IsIndexOk(NTo) then
    NTo:= TabCount-1;
  //if NFrom=NTo then Exit;

  Result:= true;
  if NTo<>FTabIndexDropOld then
  begin
    FTabIndexDropOld:= NTo;
    if Assigned(FOnTabDragging) then
      FOnTabDragging(Self, NFrom, NTo, Result);
  end;
end;

procedure TATTabs.DoTabDrop;
var
  NFrom, NTo: integer;
  ACanDrop: boolean;
begin
  NFrom:= FTabIndex;
  if not IsIndexOk(NFrom) then Exit;
  NTo:= FTabIndexDrop;
  if not IsIndexOk(NTo) then
    NTo:= TabCount-1;
  if NFrom=NTo then Exit;

  ACanDrop:= true;
  if Assigned(FOnTabDropQuery) then
    FOnTabDropQuery(Self, NFrom, NTo, ACanDrop);
  if not ACanDrop then Exit;

  FTabList.Items[NFrom].Index:= NTo;
  SetTabIndex(NTo);

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, NFrom, NTo);
end;

procedure TATTabs.MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
begin
  if not IsIndexOk(AFrom) then exit;
  if not IsIndexOk(ATo) then exit;
  if AFrom=ATo then exit;

  FTabList.Items[AFrom].Index:= ATo;
  if AActivateThen then
    SetTabIndex(ATo);
end;

function TATTabs.FindTabByObject(AObject: TObject): integer;
var
  D: TATTabData;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to TabCount-1 do
  begin
    D:= GetTabData(i);
    if D<>nil then
      if D.TabObject=AObject then
      begin
        Result:= i;
        exit;
      end;
  end;
end;

procedure TATTabs.DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
var
  ATabs: TATTabs;
  NTab, NTabTo: integer;
  Data: TATTabData;
  P: TPoint;
  bOverX: boolean;
begin
  if not (ATarget is TATTabs) then
  begin
    if Assigned(TControlHack(ATarget).OnDragDrop) then
    begin
      P:= APnt;
      Data:= GetTabData(FTabIndex);
      if Data<>nil then
        TControlHack(ATarget).OnDragDrop(ATarget, Data.TabObject, P.X, P.Y);
    end;
    Exit;
  end;  

  ATabs:= ATarget as TATTabs;
  if not ATabs.OptMouseDragEnabled then Exit;

  NTab:= FTabIndex;
  NTabTo:= ATabs.GetTabAt(APnt.X, APnt.Y, bOverX); //-1 is allowed

  Data:= GetTabData(NTab);
  if Data=nil then Exit;

  ATabs.AddTab(NTabTo, Data);

  //correct TabObject parent
  if Data.TabObject is TWinControl then
    if (Data.TabObject as TWinControl).Parent = Self.Parent then
      (Data.TabObject as TWinControl).Parent:= ATabs.Parent;

  //delete old tab (don't call OnTabClose)
  DeleteTab(NTab, false{AllowEvent}, false);

  //activate dropped tab
  if NTabTo<0 then
    ATabs.TabIndex:= ATabs.TabCount-1
  else
    ATabs.TabIndex:= NTabTo;
end;

function TATTabs.GetTabTick(AIndex: integer): Int64;
var
  D: TATTabData;
begin
  Result:= 0;
  if Assigned(FOnTabGetTick) then
  begin
    D:= GetTabData(AIndex);
    if Assigned(D) then
      Result:= FOnTabGetTick(Self, D.TabObject);
  end;
end;

procedure TATTabs.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FTabIndexOver:= -1;
  FTabIndexHinted:= -1;
  Invalidate;
end;

procedure TATTabs.SwitchTab(ANext: boolean; ALoopAtEdge: boolean=true);
begin
  if TabCount>1 then
    if ANext then
    begin
      if TabIndex=TabCount-1 then
      begin
        if ALoopAtEdge then
          TabIndex:= 0;
      end
      else
        TabIndex:= TabIndex+1;
    end
    else
    begin
      if TabIndex=0 then
      begin
        if ALoopAtEdge then
          TabIndex:= TabCount-1;
      end
      else
        TabIndex:= TabIndex-1;
    end;
end;

procedure TATTabs.DblClick;
begin
  inherited;
  FMouseDownDbl:= true;
end;

procedure TATTabs.DragOver(Source: TObject; X, Y: integer; State: TDragState;
  var Accept: Boolean);
var
  Limit: integer;
  IsX: Boolean;
begin
  //this is workaround for too early painted drop-mark (vertical red line)
  if (not FMouseDragBegins) and (GetTabAt(FMouseDownPnt.X,FMouseDownPnt.Y, IsX) >= 0) then
  begin
    Limit:= Mouse.DragThreshold;
    FMouseDragBegins:= (Abs(X-FMouseDownPnt.X)>=Limit) or (Abs(Y-FMouseDownPnt.Y)>=Limit);
  end;

  if Source is TATTabs then
  begin
    Accept:=
      FOptMouseDragEnabled and
      FOptMouseDragOutEnabled and
      ((Source<>Self) or IsDraggingAllowed);

    // Delphi 7 don't call MouseMove during dragging
    {$ifndef fpc}
    if Accept then
    begin
      FTabIndexDrop:= GetTabAt(X, Y, IsX);
    end;
    {$endif}

    if Accept then
      Invalidate;
  end
  else
  begin
    if not FOptMouseDragFromNotATTabs then
      Accept:= false
    else
      inherited;
  end;
end;

procedure TATTabs.DragDrop(Source: TObject; X, Y: integer);
begin
  if not (Source is TATTabs) then
  begin
    inherited;
    exit;
  end;

  if (Source=Self) then
  begin
    //drop to itself
    if (FTabIndexDrop>=0) or
      (FTabIndexDrop=cTabIndexPlus) or
      (FTabIndexDrop=cTabIndexEmptyArea) then
    begin
      DoTabDrop;
      Invalidate;
    end;
  end
  else
  begin
    //drop to anoter control
    (Source as TATTabs).DoTabDropToOtherControl(Self, Point(X, Y));
  end;
end;

function TATTabs.DoScale(AValue: integer): integer;
begin
  Result:= AValue * FOptScalePercents div 100;
end;

function TATTabs.DoScaleFont(AValue: integer): integer;
begin
  Result:= AValue * FOptFontScale div 100;
end;

function TATTabs.GetScrollPageSize: integer;
begin
  if not FActualMultiline then
    Result:= Width * FOptScrollPagesizePercents div 100
  else
  if FOptPosition in [atpLeft, atpRight] then
    Result:= Height * FOptScrollPagesizePercents div 100
  else
    Result:= FOptTabHeight+FOptSpacer;
end;

function TATTabs.GetMaxEdgePos: integer;
var
  R: TRect;
begin
  Result:= 0;
  if TabCount=0 then Exit;

  R:= FRectTabPlus_NotScrolled;
  if FActualMultiline then
    Result:= R.Bottom
  else
    Result:= R.Right;
end;

function TATTabs.GetMaxScrollPos: integer;
begin
  Result:= GetMaxEdgePos;
  if Result=0 then exit;

  if not FActualMultiline then
    Result:= Max(0, Result - Width + FRealIndentRight)
  else
    Result:= Max(0, Result - Height);
end;

procedure TATTabs.DoScrollLeft;
var
  NPos: integer;
begin
  NPos:= Max(0, FScrollPos-GetScrollPageSize);
  if NPos<>FScrollPos then
  begin
    FScrollPos:= NPos;
    Invalidate;
  end;
end;

procedure TATTabs.DoScrollRight;
var
  NPos: integer;
begin
  NPos:= Min(GetMaxScrollPos, FScrollPos+GetScrollPageSize);
  if NPos<>FScrollPos then
  begin
    FScrollPos:= NPos;
    Invalidate;
  end;
end;

procedure TATTabs.DoPaintButtonPlus(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
  NColor: TColor;
begin
  bOver:= FTabIndexOver=cTabIndexPlusBtn;
  if bOver then
    ElemType:= aeButtonPlusOver
  else
    ElemType:= aeButtonPlus;

  R:= FRectButtonPlus;

  if IsPaintNeeded(ElemType, -1, C, R) then
    begin
      NColor:= IfThen(
        bOver and not _IsDrag,
        FColorArrowOver,
        FColorArrow);

      DoPaintBgTo(C, R);
      DrawPlusSign(C, R, DoScale(FOptArrowSize), DoScale(1), NColor);
      DoPaintAfter(ElemType, -1, C, R);
    end;
end;

procedure TATTabs.DoPaintButtonClose(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
  NColor: TColor;
  NIndent: integer;
begin
  bOver:= FTabIndexOver=cTabIndexCloseBtn;
  if bOver then
    ElemType:= aeButtonCloseOver
  else
    ElemType:= aeButtonClose;

  R:= FRectButtonClose;

  if IsPaintNeeded(ElemType, -1, C, R) then
    begin
      NColor:= IfThen(
        bOver and not _IsDrag,
        FColorArrowOver,
        FColorArrow);

      NIndent:= (R.Right-R.Left) div 2 - DoScale(FOptArrowSize);

      DoPaintBgTo(C, R);
      CanvasPaintXMark(C, R, NColor, NIndent, NIndent, 1);
      DoPaintAfter(ElemType, -1, C, R);
    end;
end;


procedure TATTabs.DoPaintArrowDown(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
begin
  bOver:= FTabIndexOver=cTabIndexArrowMenu;
  if bOver then
    ElemType:= aeArrowDropdownOver
  else
    ElemType:= aeArrowDropdown;

  if IsPaintNeeded(ElemType, -1, C, FRectArrowDown) then
    begin
      DoPaintBgTo(C, FRectArrowDown);
      DoPaintArrowTo(C, atriDown, FRectArrowDown, bOver, true);
      DoPaintAfter(ElemType, -1, C, FRectArrowDown);
    end;
end;

procedure TATTabs.DoPaintArrowLeft(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= (TabCount > 0) and (FTabIndexOver=cTabIndexArrowScrollLeft);
  if bOver then
    ElemType:= aeArrowScrollLeftOver
  else
    ElemType:= aeArrowScrollLeft;

  if IsPaintNeeded(ElemType, -1, C, FRectArrowLeft) then
    begin
      R:= FRectArrowLeft;
      if FOptShowArrowsNear then
        R.Left:= R.Left * 2 div 3 + R.Right div 3;

      DoPaintBgTo(C, FRectArrowLeft);
      DoPaintArrowTo(C, atriLeft, R, bOver, FScrollingNeeded);
      DoPaintAfter(ElemType, -1, C, FRectArrowLeft);
    end;
end;

procedure TATTabs.DoPaintArrowRight(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= (TabCount > 0) and (FTabIndexOver=cTabIndexArrowScrollRight);
  if bOver then
    ElemType:= aeArrowScrollRightOver
  else
    ElemType:= aeArrowScrollRight;

  if IsPaintNeeded(ElemType, -1, C, FRectArrowRight) then
    begin
      R:= FRectArrowRight;
      if FOptShowArrowsNear then
        R.Right:= R.Left div 3 + R.Right * 2 div 3;

      DoPaintBgTo(C, FRectArrowRight);
      DoPaintArrowTo(C, atriRight, R, bOver, FScrollingNeeded);
      DoPaintAfter(ElemType, -1, C, FRectArrowRight);
    end;
end;


function SwapString(const S: string): string;
var
  i: integer;
begin
  Result:= '';
  SetLength(Result, Length(S));
  for i:= 1 to Length(S) do
    Result[Length(S)+1-i]:= S[i];
end;

procedure TATTabs.ApplyButtonLayout;
  //
  procedure UpdateBtns(var Btns: TATTabButtons; const S: string);
  var
    i: integer;
  begin
    SetLength(Btns, 0);
    for i:= 1 to Length(S) do
      case S[i] of
        '<': AddTabButton(Btns, atbScrollLeft,   DoScale(FOptButtonSize));
        '>': AddTabButton(Btns, atbScrollRight,  DoScale(FOptButtonSize));
        'v': AddTabButton(Btns, atbDropdownMenu, DoScale(FOptButtonSize));
        '+': AddTabButton(Btns, atbPlus,         DoScale(FOptButtonSize));
        'x': AddTabButton(Btns, atbClose,        DoScale(FOptButtonSize));
        '0': AddTabButton(Btns, atbUser0,        DoScale(FOptButtonSize));
        '1': AddTabButton(Btns, atbUser1,        DoScale(FOptButtonSize));
        '2': AddTabButton(Btns, atbUser2,        DoScale(FOptButtonSize));
        '3': AddTabButton(Btns, atbUser3,        DoScale(FOptButtonSize));
        '4': AddTabButton(Btns, atbUser4,        DoScale(FOptButtonSize));
        '_': AddTabButton(Btns, atbSpace,        DoScale(FOptButtonSizeSpace));
        '|': AddTabButton(Btns, atbSeparator,    DoScale(FOptButtonSizeSeparator));
      end;
  end;
  //
var
  S, SLeft, SRight: string;
  N: integer;
begin
  S:= FOptButtonLayout;
  N:= Pos(',', S);
  if N=0 then N:= Length(S)+1;
  SLeft:= Copy(S, 1, N-1);
  SRight:= Copy(S, N+1, MaxInt);

  UpdateBtns(FButtonsLeft, SLeft);
  UpdateBtns(FButtonsRight, SwapString(SRight));
end;

procedure TATTabs.DoClickUser(AIndex: integer);
begin
  if Assigned(FOnTabClickUserButton) then
    FOnTabClickUserButton(Self, AIndex);
end;

procedure TATTabs.DoPaintSeparator(C: TCanvas; const R: TRect);
begin
  DoPaintBgTo(C, R);
  C.Pen.Color:= FColorSeparator;
  C.MoveTo(R.Left, R.Top+DoScale(FOptSpaceSeparator));
  C.LineTo(R.Left, R.Bottom-DoScale(FOptSpaceSeparator));
end;


function TATTabs.ConvertButtonIdToTabIndex(Id: TATTabButton): integer;
begin
  case Id of
    atbUser0: Result:= cTabIndexUser0;
    atbUser1: Result:= cTabIndexUser1;
    atbUser2: Result:= cTabIndexUser2;
    atbUser3: Result:= cTabIndexUser3;
    atbUser4: Result:= cTabIndexUser4;
    else
      raise Exception.Create('Unknown button id');
  end;
end;

procedure TATTabs.DoPaintSpaceInital(C: TCanvas);
var
  R: TRect;
begin
  R.Left:= 0;
  R.Top:= 0;
  R.Right:= DoScale(FOptSpaceInitial);
  R.Bottom:= DoScale(FOptTabHeight)+DoScale(FOptSpacer);
  DoPaintBgTo(C, R);
end;

procedure TATTabs.DoPaintUserButtons(C: TCanvas; const AButtons: TATTabButtons; AtLeft: boolean);
var
  BtnId: TATTabButton;
  ElemType: TATTabElemType;
  NIndex, i: integer;
  R: TRect;
begin
  //If we have an OptSpaceInitial > 0 then this "hides" scrolled buttons
  //in that small area before the first userbutton:
  if FOptPosition in [atpTop, atpBottom] then
    if FOptSpaceInitial>0 then
      DoPaintSpaceInital(C);

  for i:= 0 to Length(AButtons)-1 do
  begin
    BtnId:= AButtons[i].Id;
    R:= GetRectOfButtonIndex(i, AtLeft);

    case BtnId of
      atbUser0..atbUser4:
        begin
          NIndex:= ConvertButtonIdToTabIndex(BtnId);

          if FTabIndexOver=NIndex then
            ElemType:= aeButtonUserOver
          else
            ElemType:= aeButtonUser;

          DoPaintBgTo(C, R);
          DoPaintAfter(ElemType, Ord(BtnId)-Ord(atbUser0), C, R);
        end;
      atbSpace:
        begin
          DoPaintBgTo(C, R);
        end;
      atbSeparator:
        begin
          DoPaintSeparator(C, R);
        end
    end;
  end;
end;

procedure TATTabs.Loaded;
begin
  inherited;
  TabIndex:= FTabIndexLoaded;
end;

procedure TATTabs.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  {$ifdef FPC}
  if not IsEnabled then //prevent popup menu if form is disabled, needed for CudaText plugins dlg_proc API on Qt5
  begin
    Handled:= true;
    exit;
  end;
  {$endif}

  inherited;
  if not Handled then
  begin
    DoHandleRightClick;
    Handled:= true;
  end;
end;

function TATTabs.GetButtonsWidth(const B: TATTabButtons): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to Length(B)-1 do
    Inc(Result, B[i].Size);
end;

function TATTabs.GetButtonsEmpty: boolean;
begin
  Result:=
    (Length(FButtonsLeft)=0) and
    (Length(FButtonsRight)=0);
end;

function TATTabs.GetInitialVerticalIndent: integer;
begin
  if GetButtonsEmpty then
    Result:= DoScale(FOptSpaceInitial)
  else
    Result:= DoScale(FOptTabHeight);
end;

procedure TATTabs.DoPaintColoredBand(C: TCanvas; const ARect: TRect; AColor: TColor;
  APos: TATTabPosition);
var
  NColor: TColor;
  R: TRect;
begin
  case APos of
    atpTop:
      begin
        R.Left:= ARect.Left+1;
        R.Right:= ARect.Right-1;
        R.Top:= ARect.Top+1-Ord(FOptShowFlat);
        R.Bottom:= R.Top+DoScale(FOptColoredBandSize);
        if FOptShowFlat then
          Inc(R.Right);
      end;
    atpBottom:
      begin
        R.Left:= ARect.Left+1;
        R.Right:= ARect.Right-1;
        R.Bottom:= ARect.Bottom;
        R.Top:= R.Bottom-DoScale(FOptColoredBandSize);
        if FOptShowFlat then
          Inc(R.Right);
      end;
    atpLeft:
      begin
        R.Left:= ARect.Left+1-Ord(FOptShowFlat);
        R.Right:= R.Left+DoScale(FOptColoredBandSize);
        R.Top:= ARect.Top+1;
        R.Bottom:= ARect.Bottom-1;
      end;
    atpRight:
      begin
        R.Right:= ARect.Right-1+Ord(FOptShowFlat);
        R.Left:= R.Right-DoScale(FOptColoredBandSize);
        R.Top:= ARect.Top+1;
        R.Bottom:= ARect.Bottom-1;
      end;
  end;

  NColor:= C.Brush.Color;
  C.Brush.Color:= AColor;
  C.FillRect(R);
  C.Brush.Color:= NColor;
end;

procedure TATTabs.DoPaintButtonsBG(C: TCanvas);
var
  X1, X2: integer;
begin
  if FOptPosition in [atpLeft, atpRight] then
    if not GetButtonsEmpty then
    begin
      X1:= GetButtonsEdgeCoord(true);
      X2:= GetButtonsEdgeCoord(false);
      DoPaintBgTo(C, Rect(X1, 0, X2, DoScale(FOptTabHeight)));
    end;
end;

procedure TATTabs.UpdateCanvasAntialiasMode(C: TCanvas);
{$ifdef fpc}
begin
  // https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39416
  {$if not defined(LCLQt5) and not defined(LCLQt6)}
  C.AntialiasingMode:= amOn;
  {$endif}
end;
{$else}
var
  p: TPoint;
begin
  GetBrushOrgEx(C.Handle, p);
  SetStretchBltMode(C.Handle, HALFTONE);
  SetBrushOrgEx(C.Handle, p.x, p.y, @p);
end;
{$endif}

procedure TATTabs.UpdateTabRectsToFillLine(AIndexFrom, AIndexTo: integer; ALastLine: boolean);
var
  NDelta, NWidthOfPlus, i: integer;
  D: TATTabData;
  R: TRect;
begin
  D:= GetTabData(AIndexTo);
  if D=nil then exit;

  if ALastLine and FOptShowPlusTab then
    NWidthOfPlus:= GetTabRectWidth(true)
  else
    NWidthOfPlus:= 0;

  NDelta:=
    (Width - FRealIndentRight - D.TabRect.Right - NWidthOfPlus)
    div (AIndexTo-AIndexFrom+1);

  for i:= AIndexFrom to AIndexTo do
  begin
    D:= GetTabData(i);
    if D=nil then Continue;
    R:= D.TabRect;
    if R=cRect0 then Continue;

    Inc(R.Left, (i-AIndexFrom)*NDelta);
    Inc(R.Right, (i+1-AIndexFrom)*NDelta);

    //width of last tab is not precise (+-2pixels). fix it.
    if i=AIndexTo then
      R.Right:= Width - FRealIndentRight - NWidthOfPlus;

    D.TabRect:= R;
  end;
end;

procedure TATTabs.UpdateCaptionProps(C: TCanvas; const ACaption: TATTabString;
  out ALineHeight: integer; out ATextSize: TSize);
  //
  procedure _GetExtent(const S: string; var Ex: TSize);
  {$ifdef WIDE}
  var
    StrW: WideString;
  {$endif}
  begin
    Ex.cx:= 0;
    Ex.cy:= 0;
    {$ifdef WIDE}
    StrW:= UTF8Decode(S);
    Windows.GetTextExtentPoint32W(C.Handle, PWideChar(StrW), Length(StrW), Ex);
    {$else}
    Ex:= C.TextExtent(S);
    {$endif}
  end;
  //
var
  Sep: TATStringSeparator;
  SepItem: string;
  Ex: TSize;
begin
  ALineHeight:= 0;
  ATextSize.cx:= 0;
  ATextSize.cy:= 0;

  if Pos(#10, ACaption)=0 then
  begin
    _GetExtent(ACaption, Ex);
    ATextSize.CY:= Ex.CY;
    ALineHeight:= Ex.CY;
    ATextSize.CX:= Ex.CX;
    exit;
  end;

  Sep.Init({$ifdef WIDE}UTF8Encode{$endif}(ACaption), #10);
  while Sep.GetItemStr(SepItem) do
  begin
    _GetExtent(SepItem, Ex);
    Inc(ATextSize.CY, Ex.CY);
    ALineHeight:= Max(ALineHeight, Ex.CY);
    ATextSize.CX:= Max(ATextSize.CX, Ex.CX);
  end;
end;

function TATTabs.IsTabVisible(AIndex: integer): boolean;
var
  D: TATTabData;
  R: TRect;
begin
  D:= GetTabData(AIndex);
  if D=nil then
  begin
    Result:= false;
    exit
  end;

  R:= D.TabRect;
  if R=cRect0 then
  begin
    Result:= false;
    exit
  end;

  if not FScrollingNeeded then
  begin
    Result:= true;
    exit
  end;

  R:= GetRectScrolled(R);
  if not FActualMultiline then
    Result:=
      (R.Left >= FRealIndentLeft) and
      (R.Right < Width-FRealIndentRight)
  else
    Result:=
      (R.Top >= FRealIndentLeft) and
      (R.Bottom < Height-FRealIndentRight);
end;

procedure TATTabs.MakeVisible(AIndex: integer);
var
  D: TATTabData;
  R: TRect;
begin
  //sometimes new tab has not updated Data.TabRect
  if FTabsChanged or FTabsResized then
  begin
    FTabsChanged:= false;
    FTabsResized:= false;
    if Assigned(FBitmap) then
      UpdateTabRects(FBitmap.Canvas);
  end;

  if not FScrollingNeeded then exit;

  if IsTabVisible(AIndex) then exit;

  //simulate repaint, otherwise cannot scroll to the actual end for the last index :(
  if AIndex=TabCount-1 then
    PaintSimulated;

  D:= GetTabData(AIndex);
  if D=nil then exit;
  R:= D.TabRect;

  if not FActualMultiline then
    FScrollPos:= Min(GetMaxScrollPos, Max(0, R.Left - Width div 2))
  else
    FScrollPos:= Min(GetMaxScrollPos, Max(0, R.Top - Height div 2));

  Invalidate;
end;

procedure TATTabs.SetScrollPos(AValue: integer);
begin
  //user suggested to not limit ScrollPos
  //AValue:= Max(0, Min(GetMaxScrollPos, AValue) );
  if FScrollPos=AValue then exit;
  FScrollPos:= AValue;
  Invalidate;
end;


function TATTabs.GetTabFlatEffective(AIndex: integer): boolean;
begin
  Result:= FOptShowFlat and not (FOptShowFlatMouseOver and (FTabIndexOver=AIndex));
end;

function TATTabs.GetTabBgColor_Passive(AIndex: integer): TColor;
var
  Data: TATTabData;
begin
  if GetTabFlatEffective(AIndex) then
    Result:= FColorBg
  else
  if (FTabIndexOver=AIndex) and not _IsDrag then
    Result:= FColorTabOver
  else
    Result:= FColorTabPassive;

  if FOptShowEntireColor then
  begin
    Data:= GetTabData(AIndex);
    if (FTabIndexOver=AIndex) and not _IsDrag and Assigned(Data) and (Data.TabColorOver<>clNone) then
      Result:= Data.TabColorOver
    else
    if Assigned(Data) and (Data.TabColor<>clNone) then
      Result:= Data.TabColor;
  end;
end;

function TATTabs.GetTabBgColor_Active(AIndex: integer): TColor;
var
  Data: TATTabData;
begin
  Result := FColorTabActive;
  //if GetTabFlatEffective(AIndex) then
  //  Result:= FColorBg
  //else
  //  Result:= FColorTabActive;

  if FOptShowEntireColor then
  begin
    Data:= GetTabData(AIndex);
    if Assigned(Data) and (Data.TabColorActive<>clNone) then
      Result:= Data.TabColorActive
    else
    if Assigned(Data) and (Data.TabColor<>clNone) then
      Result:= Data.TabColor;
  end;
end;

function TATTabs.GetPositionInverted(APos: TATTabPosition): TATTabPosition;
begin
  case APos of
    atpTop:
      Result:= atpBottom;
    atpBottom:
      Result:= atpTop;
    atpLeft:
      Result:= atpRight;
    atpRight:
      Result:= atpLeft;
    else
      raise Exception.Create('Unknown tab pos');
  end;
end;

function TATTabs.GetTabCaptionFinal(AData: TATTabData; ATabIndex: integer): TATTabString;
begin
  Result:= '';
  if AData.TabCaption<>'' then
  begin
    if AData.TabPinned then
      Result:= Result+FOptShowPinnedText;
    {
    if AData.TabModified then
      Result:= Result+FOptShowModifiedText;
      }
    if FOptShowNumberPrefix<>'' then
      Result:= Result+Format(FOptShowNumberPrefix, [ATabIndex+1]);
    Result:= Result+AData.TabCaptionFull;
  end
  else
  begin
    {
    if AData.TabModified then
      Result:= Result+FOptShowModifiedText;
      }
  end;
end;

procedure TATTabs.UpdateTabTooltip;
begin
  FTabIndexHintedPrev:= -1;
end;

initialization
  {$if defined(LCLQt5) or defined(LCLQt6) or defined(darwin)}
  ATTabsStretchDrawEnabled:= false;
  ATTabsCircleDrawEnabled:= false;
  ATTabsPixelsDrawEnabled:= false;
  {$endif};

end.
