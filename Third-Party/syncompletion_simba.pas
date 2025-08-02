// Simba changes: Use custom drawn scrollbar & support TSynBaseCompletionFormSizeDrag.OnPaint

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.
Portions created by Cyrille de Brebisson are Copyright (C) 1999
Cyrille de Brebisson. All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynCompletion_Simba;

{$I SynEdit.inc}

{$DEFINE HintClickWorkaround} // Workaround for issue 21952

interface

uses
  Classes, SysUtils, Types, Character,
  // LCL
  LCLIntf, LCLType, LMessages, Graphics, Forms,
  Controls, StdCtrls, ExtCtrls, Menus, Themes,
  // LazUtils
  LazUTF8, LazLoggerBase,
  // SynEdit
  SynEditMiscProcs, SynEditKeyCmds, SynEdit, SynEditTypes, SynEditPlugins,
  simba.component_scrollbar;

type
  TSynBaseCompletionPaintItem = function(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean of object;
  TCodeCompletionEvent = procedure(var Value: string; SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState) of object;
  TValidateEvent = procedure(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState) of object;
  TSynBaseCompletionSearchPosition = procedure(var APosition :integer) of object;

  TSynBaseCompletion = class;

  TSynCompletionHint = class(THintWindow)
  private
    FCompletion: TSynBaseCompletion;
    FIndex: Integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect: TRect; reintroduce; virtual;

    property Index: Integer read FIndex;
    property Completion: TSynBaseCompletion read FCompletion;
  end;
  TSynCompletionHintClass = class of TSynCompletionHint;

  TSynBaseCompletionFormSizeDrag = class(TPanel)
  private
    FMouseDownPos, FMouseLastPos, FWinSize: TPoint;
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  end;

  TSynBaseCompletionFormScrollBar = TSimbaScrollBar;

  { TSynCompletionForm }

  TSynCompletionForm = class(TForm)
  protected
    FItemCount: Integer;
    FCurrentString: string;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDelete: TNotifyEvent;
    FOnPaintItem: TSynBaseCompletionPaintItem;
    FPosition: Integer;
    FNbLinesInWindow: Integer;
    FFontHeight: integer;
    FResizeLock: Integer;
    FScrollBar: TSynBaseCompletionFormScrollBar;
    FSizeDrag: TSynBaseCompletionFormSizeDrag;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FSelectedColor: TColor;
    FCaseSensitive: boolean;
    FBackgroundColor: TColor;
    FDrawBorderColor: TColor;
    FOnSearchPosition: TSynBaseCompletionSearchPosition;
    FOnKeyCompletePrefix: TNotifyEvent;
    FOnKeyNextChar: TNotifyEvent;
    FOnKeyPrevChar: TNotifyEvent;
    FTextColor: TColor;
    FTextSelectedColor: TColor;
    FHint: TSynCompletionHint;
    FMouseWheelAccumulator: Integer;

    procedure KeyDownProxy(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressProxy(Sender: TObject; var Key: char);
    procedure Utf8KeyPressProxy(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEditorKeyPress(Sender: TObject; var Key: char);
    procedure DoEditorUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure SetCurrentString(const Value: string);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure AddCharAtCursor(AUtf8Char: TUTF8Char); virtual;
    procedure DeleteCharAfterCursor; virtual;
    procedure DeleteCharBeforeCursor; virtual;
    procedure Paint; override;
    procedure AppDeactivated(Sender: TObject); // Because FForm.Deactivate isn't called
    procedure Deactivate; override;
    procedure SelectPrec;
    procedure SelectNext;
    procedure ScrollChange(Sender: TObject);
    procedure SetPosition(Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    {$IFDEF HintClickWorkaround}
    procedure HintWindowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    {$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DoOnResize; override;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure FontChanged(Sender: TObject); override;
    procedure WMMouseWheel(var Msg: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure RegisterHandlers(EditOnly: Boolean = False);
    procedure UnRegisterHandlers(EditOnly: Boolean = False);
    procedure SetVisible(Value: Boolean); override;
    procedure IncHintLock;
    procedure DecHintLock;
    procedure DoOnDragResize(Sender: TObject);
    procedure ClearCurrentString;
  private
    FCurrentEditor: TCustomSynEdit; // Must only be set via TSynCompletion.SetEditor
    FDoubleClickSelects: Boolean;
    FDrawBorderWidth: Integer;
    FOnDragResized: TNotifyEvent;
    FOnPositionChanged: TNotifyEvent;
    FShowSizeDrag: Boolean;
    FHintLock: Integer;
    FCompletion: TSynBaseCompletion;

    procedure SetCurrentEditor(const AValue: TCustomSynEdit);
    procedure SetDrawBorderWidth(const AValue: Integer);
    procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetItemCount(AValue: Integer);
    procedure SetSelectedColor(AValue: TColor);
    procedure SetShowSizeDrag(const AValue: Boolean);
  protected

  public
    constructor Create(ACompletion: TSynBaseCompletion; AHint: TSynCompletionHint); reintroduce;
    destructor Destroy; override;
    function Focused: Boolean; override;
    procedure ShowItemHint(AIndex: Integer);
    property CurrentEditor: TCustomSynEdit read FCurrentEditor; // Must only be set via TSynCompletion.SetEditor
  published
    property Completion: TSynBaseCompletion read FCompletion;
    property Hint: TSynCompletionHint read FHint;
    property SizeDrag: TSynBaseCompletionFormSizeDrag read FSizeDrag;
    property ScrollBar: TSynBaseCompletionFormScrollBar read FScrollBar;
    property ItemCount: Integer read FItemCount write SetItemCount;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDelete: TNotifyEvent read FOnKeyDelete write FOnKeyDelete;
    property OnPaintItem: TSynBaseCompletionPaintItem read FOnPaintItem write FOnPaintItem;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property Position: Integer read FPosition write SetPosition;
    property NbLinesInWindow: Integer read FNbLinesInWindow write SetNbLinesInWindow;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property FontHeight:integer read FFontHeight;
    property OnSearchPosition:TSynBaseCompletionSearchPosition read FOnSearchPosition write FOnSearchPosition;
    property OnKeyCompletePrefix: TNotifyEvent read FOnKeyCompletePrefix write FOnKeyCompletePrefix; // e.g. Tab
    property OnKeyNextChar: TNotifyEvent read FOnKeyNextChar write FOnKeyNextChar; // e.g. arrow right
    property OnKeyPrevChar: TNotifyEvent read FOnKeyPrevChar write FOnKeyPrevChar; // e.g. arrow left
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property DrawBorderColor: TColor read FDrawBorderColor write FDrawBorderColor;
    property DrawBorderWidth: Integer read FDrawBorderWidth write SetDrawBorderWidth;
    property TextColor: TColor read FTextColor write FTextColor;
    property TextSelectedColor: TColor read FTextSelectedColor write FTextSelectedColor;
    property DoubleClickSelects: Boolean read FDoubleClickSelects write FDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read FShowSizeDrag write SetShowSizeDrag default False;
    property OnDragResized: TNotifyEvent read FOnDragResized write FOnDragResized;
  end;

  TSynCompletionFormClass = class of TSynCompletionForm;

  { TSynBaseCompletion }

  TOnBeforeExeucteFlag = (befAbort);
  TOnBeforeExeucteFlags = set of TOnBeforeExeucteFlag;

  TOnBeforeExecuteEvent = procedure(
    ASender: TSynBaseCompletion;
    var ACurrentString: String;
    var APosition: Integer; // Defaults to -1. If left at -1 position will be calculated from CurrentString
    var AnX, AnY: Integer;        // Coordinates for the FForm
    var AnResult: TOnBeforeExeucteFlags
  ) of object;

  TSynBaseCompletion = class(TLazSynMultiEditPlugin)
  private
    FAutoUseSingleIdent: Boolean;
    FOnBeforeExecute: TOnBeforeExecuteEvent;
    FForm: TSynCompletionForm;
    FAddedPersistentCaret, FChangedNoneBlink: boolean;
    FOnExecute: TNotifyEvent;
    FWidth: Integer;

    function GetCaseSensitive: boolean;
    function GetSelectedColor: TColor;
    function GetDoubleClickSelects: Boolean;
    function GetItemCount: Integer;
    function GetOnKeyDown: TKeyEvent;
    function GetOnPositionChanged: TNotifyEvent;
    function GetShowSizeDrag: Boolean;
    procedure SetCaseSensitive(const AValue: boolean);
    procedure SetSelectedColor(const Value: TColor);
    function GetCurrentString: string;
    function GetNbLinesInWindow: Integer;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionPaintItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetDoubleClickSelects(const AValue: Boolean);
    procedure SetItemCount(AValue: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyDown(const AValue: TKeyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnPositionChanged(const AValue: TNotifyEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionPaintItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    function GetOnKeyDelete: TNotifyEvent;
    procedure SetOnKeyDelete(const Value: TNotifyEvent);
    procedure SetShowSizeDrag(const AValue: Boolean);
    procedure SetWidth(Value: Integer);
    function GetOnUTF8KeyPress: TUTF8KeyPressEvent;
    procedure SetOnUTF8KeyPress(const AValue: TUTF8KeyPressEvent);
    function GetFontHeight:integer;
    function GetOnSearchPosition:TSynBaseCompletionSearchPosition;
    procedure SetOnSearchPosition(NewValue :TSynBaseCompletionSearchPosition);
    function GetOnKeyCompletePrefix: TNotifyEvent;
    procedure SetOnKeyCompletePrefix(const AValue: TNotifyEvent);
    function GetOnKeyNextChar: TNotifyEvent;
    procedure SetOnKeyNextChar(const AValue: TNotifyEvent);
    function GetOnKeyPrevChar: TNotifyEvent;
    procedure SetOnKeyPrevChar(const AValue: TNotifyEvent);
  protected
    function GetCompletionFormClass: TSynCompletionFormClass; virtual;
    function GetCompletionHintClass: TSynCompletionHintClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer); overload;
    procedure Execute(s: string; TopLeft: TPoint); overload;
    procedure Execute(s: string; TokenRect: TRect); overload; // Excute below or above the token // may be extended to adjust left corner too
    procedure Deactivate;
    function IsActive: boolean;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnUTF8KeyPress
                                                write SetOnUTF8KeyPress;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyDelete: TNotifyEvent read GetOnKeyDelete write SetOnKeyDelete;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
    property FontHeight: integer read GetFontHeight;
    property NbLinesInWindow: Integer read GetNbLinesInWindow write SetNbLinesInWindow; deprecated;
  published
    property OnBeforeExecute: TOnBeforeExecuteEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnPaintItem: TSynBaseCompletionPaintItem read GetOnPaintItem write SetOnPaintItem;
    property Position: Integer read GetPosition write SetPosition;
    property LinesInWindow: Integer read GetNbLinesInWindow
                                      write SetNbLinesInWindow;
    property OnSearchPosition: TSynBaseCompletionSearchPosition
                             read GetOnSearchPosition write SetOnSearchPosition;
    property OnKeyCompletePrefix: TNotifyEvent read GetOnKeyCompletePrefix
                                               write SetOnKeyCompletePrefix;// e.g. Tab
    property OnKeyNextChar: TNotifyEvent read GetOnKeyNextChar
                                         write SetOnKeyNextChar;// e.g. arrow right
    property OnKeyPrevChar: TNotifyEvent read GetOnKeyPrevChar
                                         write SetOnKeyPrevChar;// e.g. arrow left
    property OnPositionChanged: TNotifyEvent read GetOnPositionChanged
                                             write SetOnPositionChanged;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    property Width: Integer read FWidth write SetWidth;
    property DoubleClickSelects: Boolean read GetDoubleClickSelects write SetDoubleClickSelects default True;
    property ShowSizeDrag: Boolean read GetShowSizeDrag write SetShowSizeDrag default False;
    property AutoUseSingleIdent: Boolean read FAutoUseSingleIdent write FAutoUseSingleIdent;
    property ItemCount: Integer read GetItemCount write SetItemCount;
    property Form: TSynCompletionForm read FForm;
  end;

  TSynCompletion = class(TSynBaseCompletion)
  private
    FShortCut: TShortCut;
    FExecCommandID: TSynEditorCommand;
    FEndOfTokenChr: string;
    FOnCodeCompletion: TCodeCompletionEvent;
    FToggleReplacesWhole: boolean;
    procedure Cancel(Sender: TObject);
    procedure Validate(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
    function GetPreviousToken(FEditor: TCustomSynEdit): string;
  protected
    procedure SetEditor(const Value: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure SetShortCut(Value: TShortCut);
    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
    function GetCompletionFormClass: TSynCompletionFormClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    function EditorsCount: integer; deprecated; // use EditorCount
    procedure AddCharAtCursor(AUtf8Char: TUTF8Char);
    procedure DeleteCharBeforeCursor;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property OnCodeCompletion: TCodeCompletionEvent
      read FOnCodeCompletion write FOnCodeCompletion;
    property ExecCommandID: TSynEditorCommand read FExecCommandID write FExecCommandID;
    property Editor;
    property ToggleReplaceWhole: boolean read FToggleReplacesWhole write FToggleReplacesWhole;// false=shift replaces left side, true=shift replaces whole word
  end;

const
  ecSynCompletionExecute     = ecPluginFirstCompletion +  0;
  ecSynAutoCompletionExecute = ecPluginFirstCompletion +  1;

  // If extending the list, reserve space in SynEditKeyCmds
  ecSynCompletionCount = 2;

implementation

function IsIdentifierChar(p: PChar): boolean; inline;
var
  u: UnicodeString;
  i: Integer;
  L: SizeUInt;
begin
  Result := p^ in ['a'..'z','A'..'Z','0'..'9','_'];
  if Result then exit;
  if p^ <= #127 then exit;
  i := UTF8CodepointSize(p);
  SetLength(u, i);
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(u), i + 1, p, i, [toInvalidCharToSymbol], L) = trNoError
  then begin
    SetLength(u, L - 1);
    if L > 1 then
      Result := TCharacter.IsLetterOrDigit(u, 1);
  end;
end;

{ TSynBaseCompletionFormSizeDrag }

procedure TSynBaseCompletionFormSizeDrag.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FMouseDownPos.x := x + Left;
  FMouseDownPos.y := y + Top;
  FMouseLastPos.x := x + Left;
  FMouseLastPos.y := y + Top;
  FWinSize.x := TSynCompletionForm(Owner).Width;
  FWinSize.y := TSynCompletionForm(Owner).Height;
  TSynCompletionForm(Owner).IncHintLock;
  MouseCapture := True;
end;

procedure TSynBaseCompletionFormSizeDrag.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: TSynCompletionForm;
begin
  inherited MouseMove(Shift, X, Y);
  x := x + Left;
  y := y + Top;
  if (FMouseDownPos.y < 0) or ((FMouseLastPos.x = x) and (FMouseLastPos.y = y)) then
    exit;
  FMouseLastPos.x := x;
  FMouseLastPos.y := y;

  F := TSynCompletionForm(Owner);
  F.Width :=
    Max(FWinSize.x + x - FMouseDownPos.x, 100);
  F.NbLinesInWindow :=
    Max((FWinSize.y + y - FMouseDownPos.y) div F.FontHeight, 3);
end;

procedure TSynBaseCompletionFormSizeDrag.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseDownPos.y := -1;
  MouseCapture := False;
  TSynCompletionForm(Owner).DecHintLock;

  if (FWinSize.x <> TSynCompletionForm(Owner).Width) or
     (FWinSize.y <> TSynCompletionForm(Owner).Height)
  then
    TSynCompletionForm(Owner).DoOnDragResize(Owner);
end;

constructor TSynBaseCompletionFormSizeDrag.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMouseDownPos.y := -1;
end;

procedure TSynBaseCompletionFormSizeDrag.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
begin
  // do nothing
end;

procedure TSynBaseCompletionFormSizeDrag.Paint;
var
  I: Integer;
  D: TThemedElementDetails;
begin
  if Assigned(OnPaint) then
  begin
    OnPaint(Self);
    Exit;
  end;

  Canvas.Brush.Color := clBtnFace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clBtnShadow;

  D := ThemeServices.GetElementDetails(tsUpperTrackVertNormal);
  ThemeServices.DrawElement(Canvas.Handle, D, ClientRect);

  I := 2;
  while (I < Height) do
  begin
    Canvas.MoveTo(ClientRect.Right-I, ClientRect.Bottom-1-1);
    Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-I-1);
    Inc(I, 3);
  end;
end;

{ TSynCompletionForm }

constructor TSynCompletionForm.Create(ACompletion: TSynBaseCompletion; AHint: TSynCompletionHint);
begin
  ControlStyle := ControlStyle + [csNoDesignVisible];
  FResizeLock := 1; // prevent DoResize (on Handle Creation) do reset LinesInWindow
  FDoubleClickSelects := True;
  FHintLock := 0;
  BeginFormUpdate;
  KeyPreview := True;
  FCompletion := ACompletion;

  // we have no resource => must be constructed using CreateNew
  inherited CreateNew(nil, 1);

  FScrollBar := TSynBaseCompletionFormScrollBar.Create(self);
  FScrollBar.Kind := sbVertical;
  FScrollBar.OnChange := @ScrollChange;
  FScrollBar.Parent := Self;
  FScrollBar.TabStop := False;
  FScrollBar.Visible := True;

  FScrollBar.OnKeyPress := @KeyPressProxy;
  FScrollBar.OnKeyDown := @KeyDownProxy;
  FScrollBar.OnUTF8KeyPress := @Utf8KeyPressProxy;

  FSizeDrag := TSynBaseCompletionFormSizeDrag.Create(Self);
  FSizeDrag.Parent := Self;
  FSizeDrag.BevelInner := bvNone;
  FSizeDrag.BevelOuter := bvNone;
  FSizeDrag.Caption := '';
  FSizeDrag.AutoSize := False;
  FSizeDrag.BorderStyle := bsNone;
  FSizeDrag.Anchors := [akBottom, akRight, akLeft];
  FSizeDrag.AnchorSideLeft.Side := asrTop;
  FSizeDrag.AnchorSideLeft.Control := FScrollBar;
  FSizeDrag.AnchorSideRight.Side := asrBottom;
  FSizeDrag.AnchorSideRight.Control := Self;
  FSizeDrag.AnchorSideBottom.Side := asrBottom;
  FSizeDrag.AnchorSideBottom.Control := Self;
  FSizeDrag.Cursor := crSizeNWSE;
  FSizeDrag.Visible := False;
  FSizeDrag.OnKeyPress := @KeyPressProxy;
  FSizeDrag.OnKeyDown := @KeyDownProxy;
  FSizeDrag.OnUTF8KeyPress := @Utf8KeyPressProxy;

  FScrollBar.Anchors:=[akTop,akRight, akBottom];
  FScrollBar.AnchorSide[akTop].Side := asrTop;
  FScrollBar.AnchorSide[akTop].Control := self;
  FScrollBar.AnchorSide[akRight].Side := asrBottom;
  FScrollBar.AnchorSide[akRight].Control := Self;
  FScrollBar.AnchorSide[akBottom].Side := asrTop;
  FScrollBar.AnchorSide[akBottom].Control := FSizeDrag;

  FHint := AHint;
  FHint.FormStyle := fsSystemStayOnTop;
  {$IFDEF HintClickWorkaround}
  FHint.OnMouseDown := @HintWindowMouseDown;
  {$ENDIF}
  FHint.FCompletion := FCompletion;

  DrawBorderWidth := 1;
  FTextColor:=clBlack;
  FTextSelectedColor:=clWhite;
  Caption:='Completion';
  Color:=clNone;
  FBackgroundColor:=clWhite;
  FDrawBorderColor:=clBlack;
  FSelectedColor := clHighlight;
  Visible := false;
  FNbLinesInWindow := 6;
  FontChanged(Font);
  ShowHint := False;
  EndFormUpdate;
  FResizeLock := 0;

  BorderStyle := bsNone;
  FormStyle := fsSystemStayOnTop;
end;

procedure TSynCompletionForm.Deactivate;
begin
  {$IFDEF VerboseFocus}
  DebugLnEnter(['>> TSynCompletionForm.Deactivate ']);
  try
  {$ENDIF}
  // completion box lost focus
  // this can happen when a hint window is clicked => ToDo
  Visible := False;
  FHint.Visible := False;
  if Assigned(OnCancel) then OnCancel(Self);
  if (FCurrentEditor<>nil) and (TCustomSynEdit(fCurrentEditor).HandleAllocated)
  then
    SetCaretRespondToFocus(TCustomSynEdit(FCurrentEditor).Handle,true);
  {$IFDEF VerboseFocus}
  finally
    DebugLnExit(['<< TSynCompletionForm.Deactivate ']);
  end
  {$ENDIF}
end;

destructor TSynCompletionForm.Destroy;
begin
  UnRegisterHandlers;

  inherited destroy;
end;

procedure TSynCompletionForm.ShowItemHint(AIndex: Integer);
var
  R: TRect;
  MaxRight: Integer;
begin
  if Visible and (AIndex >= 0) and (AIndex < ItemCount) and (FHintLock = 0) then
  begin
    // CalcHintRect uses the current index
    FHint.FIndex := AIndex;

    // Calculate the size
    R := FHint.CalcHintRect();
    if (R.Width = 0) or (R.Height = 0) then
    begin
      FHint.Visible := False;
      Exit;
    end;

    // Clip to screen right
    MaxRight := Max(Application.MainForm.BoundsRect.Right, Monitor.BoundsRect.Right);
    if (R.Right > MaxRight) then
      R.Right := MaxRight;

    FHint.Visible := False;
    FHint.HintRect := R;
    FHint.ActivateSub();
  end else
    FHint.Visible := False;
end;

procedure TSynCompletionForm.KeyDown(var Key: Word; Shift: TShiftState);
var
  i: integer;
  Handled: Boolean;
begin
  {$IFDEF VerboseKeys}
  DebugLnEnter(['TSynCompletionForm.KeyDown ',Key,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift]);
  try
  {$ENDIF}
  inherited KeyDown(Key,Shift);
  if Key=VK_UNKNOWN then exit;
  Handled:=true;
  case Key of
// added the VK_XXX codes to make it more readable / maintainable
    VK_RETURN:
      if Assigned(OnValidate) then
        OnValidate(Self, '', Shift);
    VK_ESCAPE:
      if Assigned(OnCancel) then OnCancel(Self);
    // I do not think there is a worst way to do this, but laziness rules :-)
    VK_PRIOR:
      for i := 1 to NbLinesInWindow do
        SelectPrec;
    VK_NEXT:
      for i := 1 to NbLinesInWindow do
        SelectNext;
    VK_END:
      Position := FItemCount - 1;
    VK_HOME:
      Position := 0;
    VK_UP:
      if ssCtrl in Shift then
        Position := 0
      else
        SelectPrec;
    VK_DOWN:
      if ssCtrl in Shift then
        Position := FItemCount - 1
      else
        SelectNext;
    VK_BACK:
      if (Shift = []) and (Length(CurrentString) > 0) then begin
        if Assigned(OnKeyDelete) then OnKeyDelete(Self);
        DeleteCharBeforeCursor;
      end;
    VK_DELETE:
      begin
        if Assigned(OnKeyDelete) then OnKeyDelete(Self);
        DeleteCharAfterCursor;
      end;
    VK_TAB:
      begin
        if Assigned(OnKeyCompletePrefix) then OnKeyCompletePrefix(Self);
      end;
    VK_LEFT:
      begin
        if (Shift = []) and (Length(CurrentString) > 0) then begin
          if Assigned(OnKeyPrevChar) then OnKeyPrevChar(Self);
        end;
      end;
    VK_Right:
      begin
        if Assigned(OnKeyNextChar) then OnKeyNextChar(Self);
      end;
  else
    Handled:=false;
  end;
  if Handled then Key:=VK_UNKNOWN;
  Invalidate;
  {$IFDEF VerboseKeys}
  finally
    DebugLnExit(['TSynCompletionForm.KeyDown ',Key,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift]);
  end;
  {$ENDIF}
end;

procedure TSynCompletionForm.KeyPress(var Key: char);
begin
  {$IFDEF VerboseKeys}
  DebugLn('TSynCompletionForm.KeyPress A Key="',DbgStr(Key),'"');
  {$ENDIF}
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
  {$IFDEF VerboseKeys}
    DebugLn('TSynCompletionForm.KeyPress B Key="',DbgStr(Key),'"');
  {$ENDIF}
  if Key=#0 then exit;
  case key of //
    #33..'z':
      begin
        if Key<>#0 then
          AddCharAtCursor(key);
        Key:=#0;
      end;
    #8: ;
  else
    if (ord(key)>=32) and Assigned(OnValidate) then begin
      OnValidate(Self, Key, []);
      Key:=#0;
    end else begin
      if Assigned(OnCancel) then OnCancel(Self);
      Key:=#0;
    end;
  end; // case
  Invalidate;
end;

procedure TSynCompletionForm.AddCharAtCursor(AUtf8Char: TUTF8Char);
begin
  CurrentString := CurrentString + AUtf8Char;
  if CurrentEditor <> nil then
    (CurrentEditor as TCustomSynEdit).CommandProcessor(ecChar, AUtf8Char, nil);
end;

procedure TSynCompletionForm.DeleteCharBeforeCursor;
begin
  if CurrentEditor <> nil then
    (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0, nil);
  CurrentString := UTF8Copy(CurrentString, 1, UTF8Length(CurrentString) - 1);
end;

{$IFDEF HintClickWorkaround}
procedure TSynCompletionForm.HintWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p := ScreenToClient(FHint.ClientToScreen(Point(X, Y)));
  MouseDown(Button, Shift, p.X, p.Y);
end;
{$ENDIF}

procedure TSynCompletionForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  y := (y - 1) div FFontHeight;
  Position := FScrollBar.Position + y;
  if DoubleClickSelects and (ssDouble in Shift) and (Position = OldPosition) and
     Assigned(OnValidate)
  then
    OnValidate(Self, '', Shift);
end;

procedure TSynCompletionForm.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if ((FScrollBar.Visible) and (x > FScrollBar.Left)) or
     (y  < DrawBorderWidth) or (y >= ClientHeight - DrawBorderWidth)
  then
    exit;
  Y := (Y - DrawBorderWidth) div FFontHeight;
  ShowItemHint(FScrollBar.Position + Y);
end;

procedure TSynCompletionForm.Paint;
begin
end;

function TSynCompletionForm.Focused: Boolean;
begin
  Result := (inherited Focused) or FSizeDrag.Focused or FScrollBar.Focused;
end;

procedure TSynCompletionForm.AppDeactivated(Sender: TObject);
begin
  {$IFDEF VerboseFocus}
  DebugLn(['>> TSynCompletionForm.AppDeactivated ']);
  {$ENDIF}
  Deactivate;
end;

procedure TSynCompletionForm.ScrollChange(Sender: TObject);
begin
  if Position < FScrollBar.Position then
    Position := FScrollBar.Position
  else 
  if Position > FScrollBar.Position + NbLinesInWindow - 1 then
    Position := FScrollBar.Position + NbLinesInWindow - 1;
  Invalidate;
end;

procedure TSynCompletionForm.SelectNext;
begin
  if Position < FItemCount - 1 then
    Position := Position + 1;
end;

procedure TSynCompletionForm.SelectPrec;
begin
  if Position > 0 then
    Position := Position - 1;
end;

procedure TSynCompletionForm.DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  KeyDown(Key, Shift);
  Key := 0;
end;

procedure TSynCompletionForm.DoEditorKeyPress(Sender: TObject; var Key: char);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  KeyPress(Key);
  Key := #0;
end;

procedure TSynCompletionForm.DoEditorUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if (not Visible) or (FCurrentEditor = nil) or (Sender <> FCurrentEditor) then exit;
  UTF8KeyPress(UTF8Key);
  UTF8Key := '';
end;

procedure TSynCompletionForm.KeyDownProxy(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyDown(key,shift);
end;

procedure TSynCompletionForm.KeyPressProxy(Sender: TObject; var Key: char);
begin
  KeyPress(key);
end;

procedure TSynCompletionForm.Utf8KeyPressProxy(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

procedure TSynCompletionForm.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  {$IFDEF VerboseKeys}
  DebugLn('TSynCompletionForm.UTF8KeyPress A UTF8Key="',DbgStr(UTF8Key),'" ',dbgsName(TObject(TMethod(OnUTF8KeyPress).Data)));
  {$ENDIF}
  if Assigned(OnUTF8KeyPress) then
    OnUTF8KeyPress(Self, UTF8Key);
  if UTF8Key='' then
    exit;

  if UTF8Key=#8 then
  begin
    // backspace
  end
  else
  if (Length(UTF8Key)>=1) and (not IsIdentifierChar(@UTF8Key[1])) then
  begin
    // non identifier character
    // if it is special key then eat it
    if (Length(UTF8Key) = 1) and (UTF8Key[1] < #32) then
    begin
      if Assigned(OnCancel) then
        OnCancel(Self);
    end
    else
    if Assigned(OnValidate) then
      OnValidate(Self, UTF8Key, []);
    UTF8Key := '';
  end
  else
  if (UTF8Key<>'') then
  begin
    // identifier character
    AddCharAtCursor(UTF8Key);
    UTF8Key := '';
  end;
  {$IFDEF VerboseKeys}
  DebugLn('TSynCompletionForm.UTF8KeyPress END UTF8Key="',DbgStr(UTF8Key),'"');
  {$ENDIF}
end;

procedure TSynCompletionForm.SetCurrentString(const Value: string);
var
  i: integer;
begin
  FCurrentString := Value;
  //DebugLn('TSynCompletionForm.SetCurrentString FCurrentString=',FCurrentString);
  if Assigned(FOnSearchPosition) then
  begin
    i:=Position;
    FOnSearchPosition(i);
    Position:=i;
  end else
  begin
    //if FCaseSensitive then
    //begin
    //  for i := 0 to Pred(FItemCount) do
    //    if CompareStr(fCurrentString, Copy(ItemList[i], 1, Length(fCurrentString))) = 0 then
    //    begin
    //      Position := i;
    //      Break;
    //    end;
    //end else
    //begin
    //  for i := 0 to Pred(FItemCount) do
    //    if AnsiCompareText(fCurrentString, Copy(ItemList[i], 1, Length(fCurrentString))) = 0 then
    //     begin
    //      Position := i;
    //      Break;
    //    end;
    //end;
  end;
end;

procedure TSynCompletionForm.DoOnResize;
begin
  inherited DoOnResize;
  if ([csLoading,csDestroying]*ComponentState<>[]) or (FScrollBar=nil) then exit;
  if (fFontHeight > 0) and (FResizeLock = 0) then
  begin
    FNbLinesInWindow := (Height-2*DrawBorderWidth+(fFontHeight-1)) div fFontHeight;
    Invalidate;
  end;
end;

procedure TSynCompletionForm.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Color := AValue;
    FHint.Color := AValue;
  end;
end;

procedure TSynCompletionForm.FontChanged(Sender: TObject);
var
  TextMetric: TTextMetric;
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    inherited;
    FillChar(TextMetric{%H-},SizeOf(TextMetric),0);
    GetTextMetrics(Canvas.Handle, TextMetric);
    FFontHeight := TextMetric.tmHeight+2;
    SetNblinesInWindow(FNbLinesInWindow);
    if FSizeDrag<>nil then
      FSizeDrag.Height := Max(7, FFontHeight * 2 div 3);
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynCompletionForm.WMMouseWheel(var Msg: TLMMouseEvent);
const
  WHEEL_DELTA = 120;
var
  WheelClicks: Integer;
begin
  Inc(FMouseWheelAccumulator, Msg.WheelDelta);
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator - WheelClicks * WHEEL_DELTA;
  WheelClicks := WheelClicks * Mouse.WheelScrollLines;
  FScrollBar.Position := Max(0, Min(FItemCount - NbLinesInWindow, FScrollBar.Position - WheelClicks));
end;

procedure TSynCompletionForm.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scTopLine in Changes) and Assigned(OnCancel) then
    OnCancel(Self);
end;

procedure TSynCompletionForm.SetItemCount(AValue: Integer);
begin
  if (FItemCount = AValue) then
    Exit;
  FItemCount := AValue;

  if FItemCount - NbLinesInWindow < 0 then
    FScrollBar.Max := 0
  else
    FScrollBar.Max := FItemCount - NbLinesInWindow;

  Position := Position;
end;

procedure TSynCompletionForm.SetSelectedColor(AValue: TColor);
begin
  if FSelectedColor=AValue then Exit;
  FSelectedColor:=AValue;
end;

procedure TSynCompletionForm.SetShowSizeDrag(const AValue: Boolean);
begin
  if FShowSizeDrag = AValue then exit;
  FShowSizeDrag := AValue;
  FSizeDrag.Visible := AValue;
  if FSizeDrag.Visible then
    FScrollBar.BorderSpacing.Bottom := 0
  else
    FScrollBar.BorderSpacing.Bottom := FDrawBorderWidth;
end;

procedure TSynCompletionForm.RegisterHandlers(EditOnly: Boolean);
begin
  if FCurrentEditor <> nil then begin
    FCurrentEditor.RegisterStatusChangedHandler(@EditorStatusChanged, [scTopLine]);
    // Catch Editor events. Some Widgetset may report keys to the editor,
    // if the user types faster, then the app can open the FForm
    FCurrentEditor.RegisterBeforeKeyDownHandler(@DoEditorKeyDown);
    FCurrentEditor.RegisterBeforeKeyPressHandler(@DoEditorKeyPress);
    FCurrentEditor.RegisterBeforeUtf8KeyPressHandler(@DoEditorUtf8KeyPress);
  end;
  if not EditOnly then
    Application.AddOnDeactivateHandler(@AppDeactivated);
end;

procedure TSynCompletionForm.UnRegisterHandlers(EditOnly: Boolean);
begin
  if FCurrentEditor <> nil then begin
    FCurrentEditor.UnRegisterStatusChangedHandler(@EditorStatusChanged);
    FCurrentEditor.UnregisterBeforeKeyDownHandler(@DoEditorKeyDown);
    FCurrentEditor.UnregisterBeforeKeyPressHandler(@DoEditorKeyPress);
    FCurrentEditor.UnregisterBeforeUtf8KeyPressHandler(@DoEditorUtf8KeyPress);
  end;
  if not EditOnly then
    Application.RemoveOnDeactivateHandler(@AppDeactivated);
end;

procedure TSynCompletionForm.SetCurrentEditor(const AValue: TCustomSynEdit);
begin
  if FCurrentEditor = AValue then exit;
  UnRegisterHandlers(True);
  FCurrentEditor := AValue;
  if Visible then
    RegisterHandlers(True);
end;

procedure TSynCompletionForm.SetDrawBorderWidth(const AValue: Integer);
begin
  if FDrawBorderWidth = AValue then exit;
  FDrawBorderWidth := AValue;
  NbLinesInWindow := NbLinesInWindow;
  FScrollBar.BorderSpacing.Top := FDrawBorderWidth;
  FScrollBar.BorderSpacing.Right := FDrawBorderWidth;
  if FSizeDrag.Visible then
    FScrollBar.BorderSpacing.Bottom := 0
  else
    FScrollBar.BorderSpacing.Bottom := FDrawBorderWidth;
  FSizeDrag.BorderSpacing.Right := FDrawBorderWidth;
  FSizeDrag.BorderSpacing.Bottom := FDrawBorderWidth;
end;

procedure TSynCompletionForm.SetVisible(Value: Boolean);
begin
  if Visible = Value then exit;

  if Value then
    RegisterHandlers
  else
    UnRegisterHandlers;

  inherited SetVisible(Value);
end;

procedure TSynCompletionForm.IncHintLock;
begin
  inc(FHintLock);
  FHint.Hide;
end;

procedure TSynCompletionForm.DecHintLock;
begin
  dec(FHintLock);
  if FHintLock = 0 then
    ShowItemHint(Position);
end;

procedure TSynCompletionForm.DeleteCharAfterCursor;
begin
  if CurrentEditor <> nil then
    (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteChar, #0, nil);
end;

procedure TSynCompletionForm.DoOnDragResize(Sender: TObject);
begin
  if assigned(FOnDragResized) then
    FOnDragResized(Sender);
end;

procedure TSynCompletionForm.ClearCurrentString;
begin
  FCurrentString := '';
  FPosition := 0;
end;

procedure TSynCompletionForm.SetNbLinesInWindow(const Value: Integer);
begin
  inc(FResizeLock);   // prevent DoResize from recalculating NbLinesInWindow
  try
    FNbLinesInWindow := Value;
    Height := fFontHeight * NbLinesInWindow + 2*DrawBorderWidth;
  finally
    dec(FResizeLock);
  end;
end;

procedure TSynCompletionForm.SetPosition(Value: Integer);
begin
  Value := MinMax(Value, 0, FItemCount - 1);
  if FPosition <> Value then begin
    FPosition := Value;
    if Position < FScrollBar.Position then
      FScrollBar.Position := Position
    else if FScrollBar.Position < Position - NbLinesInWindow + 1 then
      FScrollBar.Position := Position - NbLinesInWindow + 1;
    Invalidate;
    if Assigned(OnPositionChanged) then OnPositionChanged(Self);
  end;
  if Showing then
    ShowItemHint(Position);
end;

{ TSynBaseCompletion }

constructor TSynBaseCompletion.Create(AOwner: TComponent);
begin
  FWidth := 262;
  inherited Create(AOwner);
  FForm := GetCompletionFormClass().Create(Self, GetCompletionHintClass.Create(Self));
  FForm.Width := FWidth;

  FAutoUseSingleIdent := True;
end;

destructor TSynBaseCompletion.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FForm);
end;

function TSynBaseCompletion.GetOnUTF8KeyPress: TUTF8KeyPressEvent;
begin
  Result:=FForm.OnUTF8KeyPress;
end;

procedure TSynBaseCompletion.SetOnUTF8KeyPress(
  const AValue: TUTF8KeyPressEvent);
begin
  FForm.OnUTF8KeyPress:=AValue;
end;

function TSynBaseCompletion.GetFontHeight:integer;
begin
  Result:=FForm.FontHeight;
end;

function TSynBaseCompletion.GetOnSearchPosition:TSynBaseCompletionSearchPosition;
begin
  Result:=FForm.OnSearchPosition;
end;

procedure TSynBaseCompletion.SetOnSearchPosition(
  NewValue :TSynBaseCompletionSearchPosition);
begin
  FForm.OnSearchPosition:=NewValue;
end;

function TSynBaseCompletion.GetOnKeyCompletePrefix: TNotifyEvent;
begin
  Result:=FForm.OnKeyCompletePrefix;
end;

procedure TSynBaseCompletion.SetOnKeyCompletePrefix(const AValue: TNotifyEvent);
begin
  FForm.OnKeyCompletePrefix:=AValue;
end;

function TSynBaseCompletion.GetOnKeyNextChar: TNotifyEvent;
begin
  Result:=FForm.OnKeyNextChar;
end;

procedure TSynBaseCompletion.SetOnKeyNextChar(const AValue: TNotifyEvent);
begin
  FForm.OnKeyNextChar:=AValue;
end;

function TSynBaseCompletion.GetOnKeyPrevChar: TNotifyEvent;
begin
  Result:=FForm.OnKeyPrevChar;
end;

procedure TSynBaseCompletion.SetOnKeyPrevChar(const AValue: TNotifyEvent);
begin
  FForm.OnKeyPrevChar:=AValue;
end;

function TSynBaseCompletion.GetCompletionFormClass: TSynCompletionFormClass;
begin
  Result := TSynCompletionForm;
end;

function TSynBaseCompletion.GetCompletionHintClass: TSynCompletionHintClass;
begin
  Result := TSynCompletionHint;
end;

procedure TSynBaseCompletion.Execute(s: string; x, y: integer);
var
  CurSynEdit: TCustomSynEdit;
  p: Integer;
  r: TOnBeforeExeucteFlags;
begin
  //Todo: This is dangerous, if other plugins also change/changed the flag.
  FAddedPersistentCaret := False;
  FChangedNoneBlink := False;

  FForm.ClearCurrentString;
  p := -1;
  r := [];
  if Assigned(OnBeforeExecute) then
    OnBeforeExecute(Self, s, p, x, y, r);
  if befAbort in r then
    exit;

  CurrentString := s;
  FForm.Position := 0;

  if Assigned(OnExecute) then
    OnExecute(Self);
  if (FForm.FItemCount=1) and Assigned(OnValidate) and FAutoUseSingleIdent then begin
    OnValidate(FForm, '', []);
    exit;
  end;
  if (FForm.FItemCount=0) and Assigned(OnCancel) then begin
    OnCancel(FForm);
    exit;
  end;

  if (FForm.CurrentEditor is TCustomSynEdit) then begin
    CurSynEdit:=TCustomSynEdit(FForm.CurrentEditor);
    FAddedPersistentCaret := not(eoPersistentCaret in CurSynEdit.Options);
    FChangedNoneBlink := (eoPersistentCaretStopBlink in CurSynEdit.Options2);
    if FAddedPersistentCaret then
      CurSynEdit.Options:=CurSynEdit.Options+[eoPersistentCaret];
    if FChangedNoneBlink then
      CurSynEdit.Options2:=CurSynEdit.Options2-[eoPersistentCaretStopBlink];
  end;
  FForm.SetBounds(x,y,FForm.Width,FForm.Height);
  FForm.Show;
  FForm.Position := FForm.Position;
end;

procedure TSynBaseCompletion.Execute(s: string; TopLeft: TPoint);
begin
  Execute(s, TopLeft.x, TopLeft.y);
end;

procedure TSynBaseCompletion.Execute(s: string; TokenRect: TRect);
var
  SpaceBelow, SpaceAbove: Integer;
  Mon: TMonitor;
  MRect: TRect;
begin
  Mon := Screen.MonitorFromPoint(TokenRect.TopLeft);
  if Mon = nil then begin
    Execute(s, TokenRect.Left, TokenRect.Bottom);
    exit;
  end;

  MRect := Mon.WorkareaRect; // BoundsRect on Windows, if overlap with Taskbar is desired
  TokenRect.Left := Max(MRect.Left, Min(TokenRect.Left, MRect.Right - FForm.Width));

  SpaceBelow := MRect.Bottom - TokenRect.Bottom;
  SpaceAbove := TokenRect.Top - MRect.Top;
  if FForm.Height < SpaceBelow then
    Execute(s, TokenRect.Left, TokenRect.Bottom)
  else
  if FForm.Height < SpaceAbove then
    Execute(s, TokenRect.Left, TokenRect.Top - FForm.Height)
  else
  begin
    if SpaceBelow > SpaceAbove then begin
      FForm.NbLinesInWindow := Max(SpaceBelow div FForm.FontHeight, 3); // temporary height
    Execute(s, TokenRect.Left, TokenRect.Bottom);
    end else begin
      FForm.NbLinesInWindow := Max(SpaceAbove div FForm.FontHeight, 3); // temporary height
      Execute(s, TokenRect.Left, TokenRect.Top - FForm.Height);
    end;
  end;
end;

function TSynBaseCompletion.GetCurrentString: string;
begin
  result := FForm.CurrentString;
end;

function TSynBaseCompletion.GetNbLinesInWindow: Integer;
begin
  Result := FForm.NbLinesInWindow;
end;

function TSynBaseCompletion.GetOnCancel: TNotifyEvent;
begin
  Result := FForm.OnCancel;
end;

function TSynBaseCompletion.GetOnKeyPress: TKeyPressEvent;
begin
  Result := FForm.OnKeyPress;
end;

function TSynBaseCompletion.GetOnPaintItem: TSynBaseCompletionPaintItem;
begin
  Result := FForm.OnPaintItem;
end;

function TSynBaseCompletion.GetOnValidate: TValidateEvent;
begin
  Result := FForm.OnValidate;
end;

function TSynBaseCompletion.GetPosition: Integer;
begin
  Result := FForm.Position;
end;

procedure TSynBaseCompletion.SetCurrentString(const Value: string);
begin
  FForm.CurrentString := Value;
end;

procedure TSynBaseCompletion.SetDoubleClickSelects(const AValue: Boolean);
begin
  FForm.DoubleClickSelects := AValue;
end;

procedure TSynBaseCompletion.SetItemCount(AValue: Integer);
begin
  FForm.ItemCount := AValue;
end;

procedure TSynBaseCompletion.SetNbLinesInWindow(const Value: Integer);
begin
  FForm.NbLinesInWindow := Value;
end;

procedure TSynBaseCompletion.SetOnCancel(const Value: TNotifyEvent);
begin
  FForm.OnCancel := Value;
end;

procedure TSynBaseCompletion.SetOnKeyDown(const AValue: TKeyEvent);
begin
  FForm.OnKeyDown:=AValue;
end;

procedure TSynBaseCompletion.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  FForm.OnKeyPress := Value;
end;

procedure TSynBaseCompletion.SetOnPositionChanged(const AValue: TNotifyEvent);
begin
  FForm.OnPositionChanged :=  AValue;
end;

procedure TSynBaseCompletion.SetOnPaintItem(const Value: TSynBaseCompletionPaintItem);
begin
  FForm.OnPaintItem := Value;
end;

procedure TSynBaseCompletion.SetPosition(const Value: Integer);
begin
  FForm.Position := Value;
end;

procedure TSynBaseCompletion.SetOnValidate(const Value: TValidateEvent);
begin
  FForm.OnValidate := Value;
end;

function TSynBaseCompletion.GetSelectedColor: TColor;
begin
  Result := FForm.SelectedColor;
end;

function TSynBaseCompletion.GetDoubleClickSelects: Boolean;
begin
  Result := FForm.DoubleClickSelects;
end;

function TSynBaseCompletion.GetItemCount: Integer;
begin
  Result := FForm.ItemCount;
end;

function TSynBaseCompletion.GetOnKeyDown: TKeyEvent;
begin
  Result := FForm.OnKeyDown;
end;

function TSynBaseCompletion.GetCaseSensitive: boolean;
begin
  Result := FForm.CaseSensitive;
end;

function TSynBaseCompletion.GetOnPositionChanged: TNotifyEvent;
begin
  Result := FForm.OnPositionChanged;
end;

function TSynBaseCompletion.GetShowSizeDrag: Boolean;
begin
  Result := FForm.ShowSizeDrag;
end;

procedure TSynBaseCompletion.SetCaseSensitive(const AValue: boolean);
begin
  FForm.CaseSensitive := AValue;
end;

procedure TSynBaseCompletion.SetSelectedColor(const Value: TColor);
begin
  FForm.SelectedColor := Value;
end;

function TSynBaseCompletion.GetOnKeyDelete: TNotifyEvent;
begin
  result := FForm.OnKeyDelete;
end;

procedure TSynBaseCompletion.SetOnKeyDelete(const Value: TNotifyEvent);
begin
  FForm.OnKeyDelete := Value;
end;

procedure TSynBaseCompletion.SetShowSizeDrag(const AValue: Boolean);
begin
  FForm.ShowSizeDrag := AValue;
end;

procedure TSynBaseCompletion.SetWidth(Value: Integer);
begin
  FWidth := Value;
  FForm.Width := FWidth;
  FForm.SetNbLinesInWindow(FForm.FNbLinesInWindow);
end;

procedure TSynBaseCompletion.Deactivate;
var
  CurSynEdit: TCustomSynEdit;
begin
  if (FForm<>nil) and (FForm.CurrentEditor is TCustomSynEdit)
  then begin
    CurSynEdit:=TCustomSynEdit(FForm.CurrentEditor);
    if FAddedPersistentCaret then
      CurSynEdit.Options:=CurSynEdit.Options-[eoPersistentCaret];
    if FChangedNoneBlink then
      CurSynEdit.Options2:=CurSynEdit.Options2+[eoPersistentCaretStopBlink];
  end;
  if Assigned(FForm) then FForm.Deactivate;
end;

function TSynBaseCompletion.IsActive: boolean;
begin
  Result:= (FForm <> nil) and (FForm.Visible);
end;

{ TSynCompletion }

procedure TSynCompletion.Cancel(Sender: TObject);
var
  F: TSynCompletionForm;
begin
  F := Sender as TSynCompletionForm;
  if F.CurrentEditor <> nil then begin
    if (F.CurrentEditor as TCustomSynEdit).Owner is TWinControl then
      TWinControl((F.CurrentEditor as TCustomSynEdit).Owner).SetFocus;
    (F.CurrentEditor as TCustomSynEdit).SetFocus;
  end;
end;

procedure TSynCompletion.Validate(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
var
  F: TSynCompletionForm;
  Value, CurLine: string;
  NewBlockBegin, NewBlockEnd: TPoint;
  LogCaret: TPoint;
  HighlighterIdentChars: TSynIdentChars;
begin
  //DebugLn('TSynCompletion.Validate ',dbgsName(Sender),' ',dbgs(Shift),' Position=',dbgs(Position));
  F := Sender as TSynCompletionForm;
  // Note: FForm.Visible can be false, for example when completion only contains one item
  if F.CurrentEditor is TCustomSynEdit then
    with TCustomSynEdit(F.CurrentEditor) do begin
      BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSynCompletion.Validate'){$ENDIF};
      BeginUpdate;
      try
        if Editor.Highlighter<>nil then
          HighlighterIdentChars := Editor.Highlighter.IdentChars
        else
          HighlighterIdentChars := [];
        LogCaret := LogicalCaretXY;
        NewBlockBegin:=LogCaret;
        CurLine:=Lines[NewBlockBegin.Y - 1];
        while (NewBlockBegin.X>1) and (NewBlockBegin.X-1<=length(CurLine))
        and ((IsIdentifierChar(@CurLine[NewBlockBegin.X-1]))
             or (CurLine[NewBlockBegin.X-1] in HighlighterIdentChars))
        do
          dec(NewBlockBegin.X);
        //BlockBegin:=NewBlockBegin;
        if (ssShift in Shift)=ToggleReplaceWhole then begin
          // replace the whole word
          NewBlockEnd := LogCaret;
          CurLine:=Lines[NewBlockEnd.Y - 1];
          while (NewBlockEnd.X<=length(CurLine))
          and ((IsIdentifierChar(@CurLine[NewBlockEnd.X]))
               or (CurLine[NewBlockEnd.X] in HighlighterIdentChars))
          do
            inc(NewBlockEnd.X);
        end else begin
          // replace only prefix
          NewBlockEnd := LogCaret;
        end;
        //DebugLn('TSynCompletion.Validate B Position=',dbgs(Position));
        if Position>=0 then begin
          if Assigned(FOnCodeCompletion) then
          begin
            Value := '';
            FOnCodeCompletion(Value, TextBetweenPoints[NewBlockBegin, NewBlockEnd],
                              NewBlockBegin, NewBlockEnd, KeyChar, Shift);
            if (CompareCarets(NewBlockBegin, NewBlockEnd) <> 0) or (Value <> '') then
            begin
              TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := Value;
              TCustomSynEdit(F.CurrentEditor).SetFocus;
            end;
          end else begin
            //TextBetweenPointsEx[NewBlockBegin, NewBlockEnd, scamEnd] := ItemList[Position];
            //TCustomSynEdit(F.CurrentEditor).SetFocus;
          end;
        end
        else
        if (ItemCount = 0) then
          Cancel(Sender);
      finally
        EndUpdate;
        EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSynCompletion.Validate'){$ENDIF};
      end;
    end;
end;

constructor TSynCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm.OnValidate := @Validate;
  FForm.OnCancel := @Cancel;
  FEndOfTokenChr := '()[].';
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
  FExecCommandID := ecSynCompletionExecute;
end;

procedure TSynCompletion.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletion.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
  var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean;
  var ComboKeyStrokes: TSynEditKeyStrokes);
var
  i: integer;
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  if (Code = VK_UNKNOWN) or Handled or FinishComboOnly or (FExecCommandID = ecNone) then exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);
    if (SState = ShortCutShift) and (Code = ShortCutKey) then begin
      Command := FExecCommandID;
      Handled := True;
    end;
  end;
end;

procedure TSynCompletion.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  p: TPoint;
  i: integer;
begin
  if Handled or (Command <> FExecCommandID) then
    exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    with sender as TCustomSynEdit do begin
      if not ReadOnly then begin
        p := ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight + 1));
        Editor := Sender as TCustomSynEdit; // Will set FForm.SetCurrentEditor
        Execute(GetPreviousToken(Sender as TCustomSynEdit), p.x, p.y);
        Handled := True;
      end;
    end;
  end;

end;

function TSynCompletion.GetCompletionFormClass: TSynCompletionFormClass;
begin
  Result := TSynCompletionForm;
end;

function TSynCompletion.GetPreviousToken(FEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  if FEditor <> nil then begin
    s := FEditor.LineText;
    i := FEditor.LogicalCaretXY.X - 1;
    if i > length(s) then
      result := ''
    else begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Begin
          dec(i);
        end;
      result := copy(s, i + 1, FEditor.LogicalCaretXY.X - i - 1);
    end;
  end
  else
    result := '';
end;

procedure TSynCompletion.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);

  AValue.RegisterCommandHandler(@ProcessSynCommand, nil);
  AValue.RegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynCompletion.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  if FForm.CurrentEditor = AValue then
    FForm.SetCurrentEditor(nil);

  AValue.UnregisterCommandHandler(@ProcessSynCommand);
  AValue.UnRegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynCompletion.SetEditor(const Value: TCustomSynEdit);
begin
  inherited SetEditor(Value);
  FForm.SetCurrentEditor(Value);
end;

function TSynCompletion.EditorsCount: integer;
begin
  result := EditorCount;
end;

procedure TSynCompletion.AddCharAtCursor(AUtf8Char: TUTF8Char);
begin
  FForm.AddCharAtCursor(AUtf8Char);
end;

procedure TSynCompletion.DeleteCharBeforeCursor;
begin
  FForm.DeleteCharBeforeCursor;
end;

{ TSynCompletionHint }

procedure TSynCompletionHint.Paint;
begin
end;

constructor TSynCompletionHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoHide := False;
  Visible := False;
end;

function TSynCompletionHint.CalcHintRect: TRect;
begin
  Result := TRect.Empty;
end;

const
  SynComplutionCommandStrs: array[0..1] of TIdentMapEntry = (
    (Value: ecSynCompletionExecute;       Name: 'ecSynCompletionExecute'),
    (Value: ecSynAutoCompletionExecute;   Name: 'ecSynAutoCompletionExecute')
  );

function IdentToSynCompletionCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, SynComplutionCommandStrs);
end;

function SynCompletionCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := (Cmd >= ecPluginFirstCompletion) and (Cmd - ecPluginFirstCompletion < ecSynCompletionCount);
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, SynComplutionCommandStrs);
end;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(SynComplutionCommandStrs) to High(SynComplutionCommandStrs) do
    Proc(SynComplutionCommandStrs[I].Name);
end;

initialization
  RegisterKeyCmdIdentProcs(@IdentToSynCompletionCommand, @SynCompletionCommandToIdent);
  RegisterExtraGetEditorCommandValues(@GetEditorCommandValues);

end.

