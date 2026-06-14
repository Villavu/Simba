{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_output_components;

{$I simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, Menus,
  simba.base,
  simba.containers,
  simba.component_synedit;

const
  CC_SET_BACKGROUND = UInt8(1);
  CC_RESET_BACKGROUND = UInt8(2);
  CC_CLEAR = UInt8(3);
  CC_FOCUS = UInt8(4);
  CC_RESET_AT_EOL = UInt8(5);

type
  // the actual sequence that is encoded in the string
  // Example for red background: #0#0#1'000000FF'
  // Data must be encoded as hex because stdout expecting printable chars (0..127) not binary
  PControlCode = ^TControlCode;
  TControlCode = packed record
    Sig: array[0..1] of Char; // #0#0
    Typ: UInt8;
    Data: array[0..7] of Char;
  end;

// Exposing TSynEdit is just a ton unrelated mess so just wrap it.
type
  TCheckLinkableEvent = function(Sender: TObject; var Link: String; X: Integer; out X1, X2: Integer): Boolean of object;
  TLinkClickEvent = procedure(Sender: TObject; Link: String) of object;

  TOutputListComponentReal = class(TCustomControl)
  private
    FListComponent: TComponent;

    function GetMemo: TSimbaMemo;
    function GetCheckLinkable: TCheckLinkableEvent;
    function GetLinkClick: TLinkClickEvent;
    function GetContextMenu: TPopupMenu;
    procedure SetCheckLinkable(AValue: TCheckLinkableEvent);
    procedure SetLinkClick(AValue: TLinkClickEvent);
    procedure SetContextMenu(AValue: TPopupMenu);
  public
    constructor Create(AOwner: TComponent); override;

    property Memo: TSimbaMemo read GetMemo;
    property OnCheckLinkable: TCheckLinkableEvent read GetCheckLinkable write SetCheckLinkable;
    property OnLinkClick: TLinkClickEvent read GetLinkClick write SetLinkClick;
    property ContextMenu: TPopupMenu read GetContextMenu write SetContextMenu;

    procedure Add(const S: String); overload;
    procedure Add(Buf: PChar; Len: SizeInt); overload;
    procedure Flush;
    procedure Clear;

    procedure CopyAll;
    procedure CopySelection;
    procedure CopyLine;
    procedure SelectAll;
  end;

implementation

uses
  syncobjs,
  SynEdit,
  SynEditHighlighter,
  SynEditMarkup,
  SynEditMiscClasses,
  SynEditMarkupBracket,
  SynEditMarkupWordGroup,
  SynEditMouseCmds,
  ATCanvasPrimitives,
  simba.component_theme;

type
  TLineControlCode = record
    Index: Int32; // column where this control code activates
    Typ: UInt8;
    Data: Int32;
  end;
  TLineControlCodeArray = array of TLineControlCode;
  TLineControlCodes = specialize TSimbaList<TLineControlCodeArray>;

type
  TOutputListComponent = class(TSimbaMemo)
  private type
    TPending = record
      Text: String;
      Codes: TLineControlCodeArray;
    end;
    TPendingList = specialize TSimbaList<TPending>;
  protected
    FNeedFlush: Boolean; // to exit early without acquiring a lock
    FLock: TCriticalSection;
    FBuffer: String; // string buffer written to until a line ending exists
    FControlCodes: TLineControlCodes; // all line attributes, kept in sync with .Lines
    FControlCodeBuffer: TLineControlCodeArray; // buffer to write into when parsing a line
    FPending: TPendingList;
    FCheckLinkable: TCheckLinkableEvent;
    FLinkClick: TLinkClickEvent;
    FWasLinkable: Boolean;
    FLink: String;

    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeave; override;

    procedure ParseAndAddLine(const S: String);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property OnCheckLinkable: TCheckLinkableEvent read FCheckLinkable write FCheckLinkable;
    property OnLinkClick: TLinkClickEvent read FLinkClick write FLinkClick;

    procedure Add(const S: String); overload;
    procedure Add(Buf: PChar; Len: SizeInt); overload;
    procedure Flush;

    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: Integer); override;
    function IsLinkable(Y, X1, X2: Integer): Boolean; override;
  end;

  TOutputHighlighter = class(TSynCustomHighlighter)
  private
    FTokenPos: SizeInt;
    FTokenEnd: SizeInt;
    FLineText: String;
    FLineLength: SizeInt;
    FControlCodes: TLineControlCodeArray;
    FNextIndex: SizeInt;
    FColor: TColor;
    FSpecialAttri: TSynHighlighterAttributesModifier;
  public
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
  public
    constructor Create(AOwner: TComponent); override;

    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
  end;

  // Handles coloring the whitespace after line text has finished.
  // Wasnt possible to do in highlighter to my knowledge
  // Note: In theory the highlighter could be removed and everything handled here
  //       however I did not know that at the time :)
  TEndOfLineWhitespaceMarkup = class(TSynEditMarkup)
  private
    FControlCodes: TLineControlCodes;
    FStart: Integer; // -1 will mean no markup
  public
    constructor Create(ASynEdit: TSynEditBase);

    procedure PrepareMarkupForRow(aRow: Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer); override;
    function GetMarkupAttributeAtWrapEnd(const aRow: Integer; const aWrapCol: TLazSynDisplayTokenBound): TSynSelectedColor; override;
  end;

constructor TEndOfLineWhitespaceMarkup.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);

  FControlCodes := TOutputListComponent(ASynEdit).FControlCodes;
end;

procedure TEndOfLineWhitespaceMarkup.PrepareMarkupForRow(aRow: Integer);
var
  ControlCodesForLine: TLineControlCodeArray;
  LastControlCode: TLineControlCode;
  i: Integer;
begin
  Assert(aRow-1 >= 0);
  Assert(aRow-1 < FControlCodes.Count);

  FStart := -1;

  ControlCodesForLine := FControlCodes[aRow-1];
  if (ControlCodesForLine <> nil) then
  begin
    LastControlCode := ControlCodesForLine[High(ControlCodesForLine)];

    if (LastControlCode.Typ = CC_RESET_AT_EOL) then
    begin
      for i := High(ControlCodesForLine) - 1 downto 0 do
        if (ControlCodesForLine[i].Typ = CC_SET_BACKGROUND) then
        begin
          MarkupInfo.Background := ColorBlend(UInt32(ControlCodesForLine[i].Data), SimbaComponentTheme.ColorBackground, 80);
          FStart := Length(Lines[aRow - 1]) + 1;
          Exit;
        end;
    end else
    if (LastControlCode.Typ = CC_SET_BACKGROUND) then
    begin
      MarkupInfo.Background := ColorBlend(UInt32(LastControlCode.Data), SimbaComponentTheme.ColorBackground, 80);
      FStart := Length(Lines[aRow - 1]) + 1;
    end;
  end;
end;

function TEndOfLineWhitespaceMarkup.GetMarkupAttributeAtRowCol(const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  if (FStart > -1) and (FStart <= aStartCol.Logical) then
    Result := MarkupInfo
  else
    Result := nil;
end;

function TEndOfLineWhitespaceMarkup.GetMarkupAttributeAtWrapEnd(const aRow: Integer; const aWrapCol: TLazSynDisplayTokenBound): TSynSelectedColor;
begin
  if (FStart > -1) and (FStart <= aWrapCol.Logical) then
    Result := MarkupInfo
  else
    Result := nil;
end;

procedure TEndOfLineWhitespaceMarkup.GetNextMarkupColAfterRowCol(const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (FStart > -1) and (FStart > aStartCol.Logical) then
    ANextLog := FStart;
end;

procedure TOutputListComponent.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: Integer);
var
  Line: String;
  X1, X2: Integer;
begin
  inherited GetWordBoundsAtRowCol(XY, StartX, EndX);

  FWasLinkable := False;

  if MouseInClient and Assigned(FCheckLinkable) then
  begin
    Line := TextView[XY.Y - 1];
    if (XY.X > Length(Line)) or (Length(Line) <= 1) then
      Exit;

    FWasLinkable := FCheckLinkable(Self, Line, XY.X, X1, X2);
    if FWasLinkable then
    begin
      FLink := Line;
      StartX := X1;
      EndX   := X2;
    end;
  end;
end;

function TOutputListComponent.IsLinkable(Y, X1, X2: Integer): Boolean;
begin
  Result := MouseInClient and FWasLinkable;
end;

procedure TOutputListComponent.DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := FWasLinkable;
end;

procedure TOutputListComponent.DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FLinkClick) and FWasLinkable then
    FLinkClick(Sender, FLink);
end;

procedure TOutputListComponent.MouseLeave;
begin
  LastMouseCaret := TPoint.Create(-1, -1);

  inherited MouseLeave();
end;

procedure TOutputListComponent.ParseAndAddLine(const S: String);

  procedure AddControlCode(var Count: Integer; const Index: UInt32; const Typ: UInt8; const Data: Int32); inline;
  begin
    if (Count >= Length(FControlCodeBuffer)) then
      SetLength(FControlCodeBuffer, Length(FControlCodeBuffer) * 2);
    FControlCodeBuffer[Count].Typ := Typ;
    FControlCodeBuffer[Count].Data := Data;
    FControlCodeBuffer[Count].Index := Index;
    Inc(Count);
  end;

  procedure ParseAndAddControlCode(var Count: Integer; const Index: UInt32; ControlCode: PControlCode); inline;

    function HexToInt32(P: PAnsiChar): Int32;
    var
      N, I: Integer;
      Val: AnsiChar;
    begin
      Result := 0;

      for I := 1 to 8 do
      begin
        Val := P^;
        case Val of
          '0'..'9': N := Ord(Val) - Ord('0');
          'a'..'f': N := Ord(Val) - (Ord('a') - 10);
          'A'..'F': N := Ord(Val) - (Ord('A') - 10);
          else
            Exit(0);
        end;
        Result := (Result shl 4) or Int32(N);

        Inc(P);
      end;
    end;

  begin
    AddControlCode(Count, Index, ControlCode^.Typ, HexToInt32(@ControlCode^.Data[0]));
  end;

  function HasControlCodeSignature: Boolean; inline;
  begin
    Result := (Length(S) >= SizeOf(TControlCode)) and (Pos(#0#0, S) > 0);
  end;

var
  I, LastPos, Len, Stop: Integer;
  CleanTextLen: Integer;
  CleanText: AnsiString;
  ControlCodeCount: Integer;
  Last: TLineControlCode;
  PendingItem: TPending;
begin
  Len := Length(S);

  ControlCodeCount := 0;
  Stop := Len - SizeOf(TControlCode) + 1;

  // carry-over last color from previous line
  if (FPending.Count = 0) then
  begin
    if (Lines.Count > 0) and (FControlCodes[Lines.Count - 1] <> nil) then
    begin
      Last := FControlCodes[Lines.Count - 1][High(FControlCodes[Lines.Count - 1])];
      if (Last.Typ = CC_SET_BACKGROUND) then
        AddControlCode(ControlCodeCount, 0, Last.Typ, Last.Data);
    end;
  end else
  begin
    if (FPending.Last.Codes <> nil) then
    begin
      Last := FPending.Last.Codes[High(FPending.Last.Codes)];
      if (Last.Typ = CC_SET_BACKGROUND) then
        AddControlCode(ControlCodeCount, 0, Last.Typ, Last.Data);
    end;
  end;

  if HasControlCodeSignature() then
  begin
    SetLength(CleanText, Len);
    CleanTextLen := 0;
    LastPos := 1;
    I := 1;

    while (I <= Stop) do
    begin
      if (S[I] = #0) and (S[I+1] = #0) then
      begin
        if (I > LastPos) then
        begin
          Move(S[LastPos], CleanText[CleanTextLen + 1], I - LastPos);
          Inc(CleanTextLen, I - LastPos);
        end;
        ParseAndAddControlCode(ControlCodeCount, CleanTextLen, PControlCode(@S[I]));
        I := I + SizeOf(TControlCode);
        LastPos := I;
      end
      else
        Inc(I);
    end;

    // Remaining tail of the string
    if (LastPos <= Len) then
    begin
      Move(S[LastPos], CleanText[CleanTextLen + 1], Len - LastPos + 1);
      Inc(CleanTextLen, Len - LastPos + 1);
    end;
    SetLength(CleanText, CleanTextLen);
  end else
    CleanText := S;

  if (ControlCodeCount > 0) then
    PendingItem.Codes := Copy(FControlCodeBuffer, 0, ControlCodeCount)
  else
    PendingItem.Codes := nil;
  PendingItem.Text := CleanText;
  FPending.Add(PendingItem);
end;

// Unoptimized version
//procedure TOutputListComponent.Add(const S: String);
//var
//  Arr: TStringArray;
//  I: Integer;
//begin
//  FLock.Enter();
//  try
//    FNeedFlush := True;
//    Arr := String(FBuffer + S).Split(LineEnding, False);
//    if (Length(Arr) = 0) then
//      FBuffer := ''
//    else if S.EndsWith(LineEnding) then
//    begin
//      FBuffer := '';
//      for I := 0 to High(Arr) do
//        ParseAndAddLine(Arr[I]);
//    end else
//    begin
//      FBuffer := Arr[High(Arr)];
//      for I := 0 to High(Arr) - 1 do
//        ParseAndAddLine(Arr[I]);
//    end;
//  finally
//    FLock.Leave();
//  end;
//end;

procedure TOutputListComponent.Add(const S: String);
begin
  Add(PChar(S), Length(S));
end;

// Optimized version (String.SplitLines style)
procedure TOutputListComponent.Add(Buf: PChar; Len: SizeInt);
var
  P, StartP, EndP: PChar;
  LineLen: SizeInt;
  Line: String;
begin
  if (Buf = nil) or (Len <= 0) then
    Exit;

  FLock.Enter();
  try
    FNeedFlush := True;

    P := Buf;
    StartP := Buf;
    EndP := Buf + Len;
    while (P < EndP) do
    begin
      if (P^ = #10) then
      begin
        LineLen := P - StartP;
        if (LineLen > 0) and ((P - 1)^ = #13) then
          Dec(LineLen);

        SetString(Line, StartP, LineLen);

        // prepend if has incomplete data from the last chunk
        if (FBuffer <> '') then
        begin
          Line := FBuffer + Line;
          FBuffer := '';
        end;
        ParseAndAddLine(Line);

        Inc(P);
        StartP := P;
      end
      else
        Inc(P);
    end;

    // chunk did not end with a newline, store for next time
    if (StartP < EndP) then
    begin
      SetString(Line, StartP, EndP - StartP);
      FBuffer := FBuffer + Line;
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TOutputListComponent.Flush;
var
  I, J: Integer;
  WasFullyScrolled: Boolean;
  Item: TPending;
  HasVisualCode: Boolean;
begin
  if not FNeedFlush then
    Exit;

  // continue automatically scrolling if already scrolled to bottom.
  WasFullyScrolled := (Lines.Count < LinesInWindow) or ((Lines.Count + 1) = (TopLine + LinesInWindow));

  FLock.Enter();
  try
    FNeedFlush := False;

    IncPaintLock();
    try
      for I := 0 to FPending.Count - 1 do
      begin
        Item := FPending[I];

        if (Item.Codes <> nil) then
        begin
          HasVisualCode := False;
          for J := 0 to High(Item.Codes) do
          begin
            case Item.Codes[J].Typ of
              CC_CLEAR:
                begin
                  Lines.Clear();
                  FControlCodes.Clear();
                end;
              CC_FOCUS:
                begin
                  Parent.Show();
                  TopLine := Lines.Count;
                end;
              else
                HasVisualCode := True;
            end;
          end;

          if (Item.Text = '') and (not HasVisualCode) then
            Continue;
        end;

        Lines.Add(Item.Text);
        FControlCodes.Add(Item.Codes);
      end;

      FPending.Clear();
    finally
      DecPaintLock();
    end;
  finally
    FLock.Leave();
  end;

  if WasFullyScrolled then
    TopLine := Lines.Count;
  Invalidate();
end;

constructor TOutputListComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, False);

  FLock := TCriticalSection.Create();
  FControlCodes := TLineControlCodes.Create();
  FBuffer := '';
  FPending := TPendingList.Create();

  SetLength(FControlCodeBuffer, 128);

  MarkupManager.AddMarkUp(TEndOfLineWhitespaceMarkup.Create(Self));
  Highlighter := TOutputHighlighter.Create(Self);
  ReadOnly := True;
  TabStop := False;
  OnMouseLink := @DoAllowMouseLink;
  OnClickLink := @DoMouseLinkClick;

  MouseLinkColor.Style := [fsUnderline];
  MouseLinkColor.Foreground := RGBToColor(80, 160, 240);

  MarkupByClass[TSynEditMarkupBracket].Enabled := False;
  MarkupByClass[TSynEditMarkupWordGroup].Enabled := False;

  MouseOptions := [emUseMouseActions, emShowCtrlMouseLinks];
  ResetMouseActions();
end;

destructor TOutputListComponent.Destroy;
begin
  inherited Destroy();

  FreeAndNil(FLock);
  FreeAndNil(FControlCodes);
  FreeAndNil(FPending);
end;

constructor TOutputHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSpecialAttri := TSynHighlighterAttributesModifier.Create('special');
  FSpecialAttri.OnChange := nil;

  AddAttribute(FSpecialAttri);
end;

procedure TOutputHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;

  if (LineNumber < 0) or (LineNumber >= TOutputListComponent(Owner).FControlCodes.Count) then
    raise Exception.Create('TOutputHighlighter.SetLine out of range?');

  FControlCodes := TOutputListComponent(Owner).FControlCodes[LineNumber];
  FNextIndex := 0;
  FLineText := NewValue;
  FLineLength := Length(FLineText);
  FTokenEnd := 1;
  FColor := -1;
  Next();
end;

procedure TOutputHighlighter.Next;
begin
  FTokenPos := FTokenEnd;
  if (FTokenPos > FLineLength) then
  begin
    FTokenEnd := FLineLength + 1;
    Exit;
  end;

  if (FControlCodes <> nil) and (FNextIndex <= High(FControlCodes)) then
  begin
    FTokenEnd := FControlCodes[FNextIndex].Index + 1;
    if (FTokenPos = FTokenEnd) then
    begin
      if (FControlCodes[FNextIndex].Typ = CC_SET_BACKGROUND) then
        FColor := FControlCodes[FNextIndex].Data
      else
        FColor := -1;

      Inc(FNextIndex);
      if (FNextIndex <= High(FControlCodes)) then
        FTokenEnd := FControlCodes[FNextIndex].Index + 1
      else
        FTokenEnd := FLineLength + 1;
    end;
  end else
    FTokenEnd := FLineLength + 1;
end;

function TOutputHighlighter.GetEol: Boolean;
begin
  Result := FTokenPos > FLineLength;
end;

procedure TOutputHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TOutputHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if (FColor = -1) then
    Result := nil
  else
  begin
    Result := FSpecialAttri;
    Result.Background := ColorBlend(UInt32(FColor), SimbaComponentTheme.ColorBackground, 80);
  end;
end;

function TOutputHighlighter.GetToken: String;
begin
  Result := Copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TOutputHighlighter.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TOutputHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

function TOutputHighlighter.GetTokenKind: integer;
begin
  Result := -1;
end;

function TOutputListComponentReal.GetMemo: TSimbaMemo;
begin
  Result := TSimbaMemo(FListComponent);
end;

function TOutputListComponentReal.GetCheckLinkable: TCheckLinkableEvent;
begin
  Result := TOutputListComponent(FListComponent).OnCheckLinkable;
end;

function TOutputListComponentReal.GetLinkClick: TLinkClickEvent;
begin
  Result := TOutputListComponent(FListComponent).OnLinkClick;
end;

function TOutputListComponentReal.GetContextMenu: TPopupMenu;
begin
  Result := TOutputListComponent(FListComponent).PopupMenu;
end;

procedure TOutputListComponentReal.SetCheckLinkable(AValue: TCheckLinkableEvent);
begin
  TOutputListComponent(FListComponent).OnCheckLinkable := AValue;
end;

procedure TOutputListComponentReal.SetLinkClick(AValue: TLinkClickEvent);
begin
  TOutputListComponent(FListComponent).OnLinkClick := AValue;
end;

procedure TOutputListComponentReal.SetContextMenu(AValue: TPopupMenu);
begin
  TOutputListComponent(FListComponent).PopupMenu := AValue;
end;

constructor TOutputListComponentReal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FListComponent := TOutputListComponent.Create(Self);
  TOutputListComponent(FListComponent).Parent := Self;
  TOutputListComponent(FListComponent).Align := alClient;
end;

procedure TOutputListComponentReal.Add(const S: String);
begin
  TOutputListComponent(FListComponent).Add(S);
end;

procedure TOutputListComponentReal.Add(Buf: PChar; Len: SizeInt);
begin
  TOutputListComponent(FListComponent).Add(Buf, Len);
end;

procedure TOutputListComponentReal.Flush;
begin
  TOutputListComponent(FListComponent).Flush();
end;

procedure TOutputListComponentReal.Clear;
begin
  TOutputListComponent(FListComponent).Add(DEBUG_CLEAR + LineEnding);
end;

procedure TOutputListComponentReal.CopyAll;
begin
  TOutputListComponent(FListComponent).DoCopyToClipboard(TOutputListComponent(FListComponent).Text);
end;

procedure TOutputListComponentReal.CopySelection;
begin
  TOutputListComponent(FListComponent).CopyToClipboard();
end;

procedure TOutputListComponentReal.CopyLine;
var
  Line: Integer;
begin
  with TOutputListComponent(FListComponent) do
  begin
    Line := PixelsToRowColumn(ScreenToClient(ContextMenu.PopupPoint), []).Y;
    if (Line > 0) and (Line <= Lines.Count) then
      DoCopyToClipboard(Lines[Line - 1]);
  end;
end;

procedure TOutputListComponentReal.SelectAll;
begin
  TOutputListComponent(FListComponent).SelectAll();
end;

end.


