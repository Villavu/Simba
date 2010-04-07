unit v_AutoCompleteForm;

interface

{$I ValistusDefines.inc}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,  SynEdit, SynEditKeyCmds, v_ideCodeParser, v_ideCodeInsight,

  {$IFDEF FPC}
  LMessages,
  lcltype,
  mPasLex
  {$ELSE}
  Windows,
  Messages
  {$ENDIF};

type
  TInsertProc = procedure(Str: string) of object;

  TAutoCompleteListBox = class(TListBox)
  protected
    fIndexList: array of Integer;
    fList: TStrings;
    fInsertList: TStrings;
    fFilter: string;

    procedure setItemList(List: TStrings);
    procedure setInsertList(List: TStrings);
    procedure setFilter(Filter: string);

    procedure DblClick; override;
    {$IFDEF ccFORMCAPTION}
    procedure DoSelectionChange(User: Boolean); override;
    {$ENDIF}
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    {$IFDEF FPC}
    procedure WMEraseBkgnd(var message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMVScroll(var message: TLMVScroll); message LM_VSCROLL;
    procedure CNChar(var message: TLMessage); message CN_CHAR;
    {$ELSE}
    procedure WMEraseBkgnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var message: TWMVScroll); message WM_VSCROLL;
    procedure CNChar(var message: TMessage); message CN_CHAR;
    {$ENDIF}
  public
    ColumnSizes: array of Integer;
    Redirect: TWinControl;
    InsertProc: TInsertProc;

    procedure setLists(ItemList, InsertList: TStrings);
    function getInsert: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ItemList: TStrings read fList write setItemList;
    property InsertList: TStrings read fInsertList write setInsertList;
    property Filter: string read fFilter write setFilter;
  end;

  TAutoCompletePopup = class(TForm)
  protected
    l: TAutoCompleteListBox;
    procedure DoShow; override;

    function getRedirect: TWinControl;
    procedure setRedirect(Control: TWinControl);
    function getInsertProc: TInsertProc;
    procedure setInsertProc(Proc: TInsertProc);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Show(Pos: TPoint; ItemList, InsertList: TStrings; Filter: string = ''; Editor: TWinControl = nil); reintroduce;
    procedure DoHide; override;

    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleKeyPress(Sender: TObject; var Key: char);
  published
    property ListBox: TAutoCompleteListBox read l;
    property Redirect: TWinControl read getRedirect write setRedirect;
    property InsertProc: TInsertProc read getInsertProc write setInsertProc;
  end;

  { TParamHint }

  TParamHint = class(THintWindow)
  private
    fPreparedString : string;
    LastParameterIndex : integer;
    FSynEdit : TSynedit;
    FStartPoint : TPoint;
    FBracketPoint : TPoint;
    FMP : TCodeInsight;
    FDecl : TciProcedureDeclaration;
    FParameters : TDeclarationArray;
    procedure ParamHintHide(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure DrawHints(var MaxWidth, MaxHeight: Integer; Draw: boolean);
    function PrepareParamString(out Str : string; out MustHide : boolean) : integer;
  public
    destructor Destroy; override;
    constructor Create(TheOwner: TComponent); override;
    procedure CalculateBounds;
    procedure UpdateHint;
    procedure Paint; override;
    procedure Show(StartPoint,BracketPoint : TPoint;Decl : TciProcedureDeclaration; Editor : TSynedit; mp : TCodeInsight); reintroduce;
  end;

implementation

uses
  StrUtils {$IFDEF FPC}, lclintf{$ENDIF},math, Themes;

procedure TAutoCompleteListBox.setItemList(List: TStrings);
begin
  fList.Assign(List);
end;

procedure TAutoCompleteListBox.setInsertList(List: TStrings);
begin
  fInsertList.Assign(List);
end;

procedure TAutoCompleteListBox.setLists(ItemList, InsertList: TStrings);
var
  i: Integer;
begin
  //fList.Assign(ItemList);
  //fInsertList.Assign(InsertList);
  {$IFDEF FPC}LockSelectionChange;{$ENDIF}

  fList.BeginUpdate;
  try
    fList.Clear;
    fList.AddStrings(ItemList);
  finally
    fList.EndUpdate;
  end;

  fInsertList.BeginUpdate;
  try
    fInsertList.Clear;
    fInsertList.AddStrings(InsertList);
  finally
    fInsertList.EndUpdate;
  end;

  Items.BeginUpdate;
  try
    Items.Clear;
    Items.AddStrings(ItemList);
  finally
    Items.EndUpdate;
  end;

  fFilter := '';

  SetLength(fIndexList, fList.Count);
  for i := 0 to fList.Count - 1 do
    fIndexList[i] := i;

  if (fList.Count > 0) then
    ItemIndex := 0
  else
    ItemIndex := -1;

  {$IFDEF FPC}UnlockSelectionChange;{$ENDIF}
end;

procedure TAutoCompleteListBox.setFilter(Filter: string);
var
  i, c, l, del: Integer;
begin
  Filter := LowerCase(Filter);
  if (Filter = fFilter) then
    Exit;

  {$IFDEF FPC}LockSelectionChange;{$ENDIF}

  c := 0;
  if (LeftStr(Filter, Length(fFilter)) = fFilter) then
  begin
    fFilter := Filter;

    Items.BeginUpdate;
    try
      l := Length(fFilter);
      del := 0;

      for i := 0 to Items.Count - 1 do
        if (LowerCase(LeftStr(fInsertList[fIndexList[i]], l)) <> fFilter) then
        begin
          Items.Delete(i - del);
          Inc(del);
        end
        else
        begin
          if (ItemIndex = i) or (LowerCase(fInsertList[fIndexList[i]]) = fFilter) then
            ItemIndex := c;

          fIndexList[c] := fIndexList[i];
          Inc(c);
        end;

      SetLength(fIndexList, c + 1);
    finally
      Items.EndUpdate;
    end;
  end
  else
  begin
    fFilter := Filter;

    Items.BeginUpdate;
    try
      Items.Clear;
      l := Length(fFilter);
      SetLength(fIndexList, fInsertList.Count);

      for i := 0 to fInsertList.Count - 1 do
        if (LowerCase(LeftStr(fInsertList[i], l)) = fFilter) then
        begin
          Items.Append(fList[i]);

          if (LowerCase(fInsertList[i]) = fFilter) then
            ItemIndex := i;

          fIndexList[c] := i;
          Inc(c);
        end;
      SetLength(fIndexList, c + 1);
    finally
      Items.EndUpdate;
    end;
  end;

  {$IFDEF FPC}UnlockSelectionChange;{$ENDIF}
end;

function TAutoCompleteListBox.getInsert: string;
begin
  if (ItemIndex < 0) or (ItemIndex > Length(fIndexList)) then
    Result := ''
  else
    Result := fInsertList[fIndexList[ItemIndex]];
end;

{$IFDEF FPC}
procedure TAutoCompleteListBox.WMEraseBkgnd(var message: TLMEraseBkgnd);
{$ELSE}
procedure TAutoCompleteListBox.WMEraseBkgnd(var message: TWMEraseBkgnd);
{$ENDIF}
begin
  if (Count < Round(ClientHeight / ItemHeight)) then
  begin
    Canvas.Brush.Color := clYellow;
    FillRect(message.DC, Rect(0, Count * ItemHeight, ClientWidth, ClientHeight), HBRUSH({$IFDEF FPC}Brush.Reference.Handle{$ELSE}Parent.Brush.Handle{$ENDIF}));
  end;

  message.Result := 1;
end;

{$IFDEF FPC}
procedure TAutoCompleteListBox.WMVScroll(var message: TLMVScroll);
{$ELSE}
procedure TAutoCompleteListBox.WMVScroll(var message: TWMVScroll);
{$ENDIF}
var
  c: Integer;
begin
  {$IFDEF FPC}LockSelectionChange;{$ENDIF}
  if (ItemIndex < TopIndex) then
    ItemIndex := TopIndex
  else
  begin
    c := Round(ClientHeight / ItemHeight) - 1;
    if (ItemIndex > TopIndex + c) then
      ItemIndex := TopIndex + c;
  end;
  {$IFDEF FPC}UnlockSelectionChange;{$ENDIF}
end;

{$IFDEF FPC}
procedure TAutoCompleteListBox.CNChar(var message: TLMessage);
{$ELSE}
procedure TAutoCompleteListBox.CNChar(var message: TMessage);
{$ENDIF}
begin
  inherited;

  {$IFDEF FPC}
  if (message.Result = 0) and (Redirect <> nil) and (TLMChar(message).CharCode <> VK_DOWN) and (TLMChar(message).CharCode <> VK_UP) and (TLMChar(message).CharCode <> VK_RETURN) then
  {$ELSE}
  if (message.Result = 0) and (Redirect <> nil) and (TWMChar(message).CharCode <> VK_DOWN) and (TWMChar(message).CharCode <> VK_UP) and (TWMChar(message).CharCode <> VK_RETURN) then
  {$ENDIF}
  begin
    Redirect.SetFocus;
    Application.ProcessMessages;
    if (Redirect is TSynEdit) then
      TSynEdit(Redirect).CommandProcessor(ecChar, TUTF8Char(Chr(TLMChar(message).CharCode)), nil)
    else
      SendMessage(Redirect.Handle, CN_Char, message.wParam, message.lParam);
  end;
end;

procedure TAutoCompleteListBox.DblClick;
begin
  if (Assigned(InsertProc)) then
    InsertProc(GetInsert);
  if (Owner is TForm) then
    TForm(Owner).Hide;
end;

{$IFDEF ccFORMCAPTION}
procedure TAutoCompleteListBox.DoSelectionChange(User: Boolean);
begin
  if (Owner is TForm) then
    TForm(Owner).Caption := getInsert;
end;
{$ENDIF}

procedure TAutoCompleteListBox.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  p1, p2, p3, tl, col: Integer;
  s, c: string;
begin
  if (ItemIndex = -1) and (Count > 0) then
  begin
    {$IFDEF FPC}LockSelectionChange;{$ENDIF}
    ItemIndex := TopIndex;
    {$IFDEF FPC}UnlockSelectionChange;{$ENDIF}
  end;

  if (not Visible) or (ARect.Left > ClientRect.Right) or (ARect.Top > ClientRect.Bottom) or (Index < 0) or (Index >= Items.Count) then
    Exit;

  tl := ARect.Left;
  col := 0;
  with Canvas do
  begin
    Font.Style := [];
    Font.Color := clBlack;
    if (odSelected in State) then
      Brush.Color := clHighlight
    else if (odHotLight in State) then
      Brush.Color := clHotLight
    else if Odd(Index) then
      Brush.Color := $F0F0F0
    else
      Brush.Color := clWhite;
    FillRect(ARect);

    s := Items[Index];
    p1 := Pos('{', s);
    p3 := 1;
    while (p1 > 0) do
    begin
      p2 := PosEx('}', s, p1 + 1);
      if (p2 > 0) then
      begin
        if (s[p2 - 1] <> '\') then
        begin
          c := Copy(s, p3, p1 - p3);
          TextOut(tl, ARect.Top + Round((ItemHeight - TextHeight(c) + 0.001) / 2) , c);
          tl := tl + TextWidth(c) + 1;

          p3 := p2 + 1;
          if ((p2 - p1 - 2) > 0) then
          begin
            c := LowerCase(Copy(s, p1 + 2, p2 - p1 - 2));
            case Char(CharUpper({$IFNDEF FPC}PChar{$ELSE}Char{$ENDIF}(s[p1 + 1]))) of
              '#', 'C': Font.Color := StringToColor(c);
              'B':
                if (c = '+') then
                  Font.Style := Font.Style + [fsBold]
                else
                  Font.Style := Font.Style - [fsBold];
              'I':
                if (c = '+') then
                  Font.Style := Font.Style + [fsItalic]
                else
                  Font.Style := Font.Style - [fsItalic];
              'U':
                if (c = '+') then
                  Font.Style := Font.Style + [fsUnderline]
                else
                  Font.Style := Font.Style - [fsUnderline];
            end;
          end
          else if (s[p1 + 1] = '|') then
          begin
            if (ColumnSizes[col] = -1) then
              ColumnSizes[col] := Canvas.TextWidth('constructor') + 5;
            tl := ColumnSizes[col];
            Inc(Col);
          end;
          p1 := PosEx('{', s, p2 + 1);
        end;
      end
      else
        Break;
    end;

    if ((Length(s) - p3 + 1) > 0) then
    begin
      c := Copy(s, p3, Length(s) - p3 + 1);
      TextOut(tl, ARect.Top + Round((ItemHeight - TextHeight(c) + 0.001) / 2), c);
    end;
  end;
end;

constructor TAutoCompleteListBox.Create(TheOwner: TComponent);
begin
  inherited;

  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];
  BorderStyle := bsNone;
  Style := lbOwnerDrawFixed;

  IntegralHeight := True;
  {$IFDEF FPC}
  ItemHeight := CalculateStandardItemHeight + 4;
  {$ELSE}
  ItemHeight := 19;
  {$ENDIF}
  Constraints.MinHeight := ItemHeight;

  SetLength(ColumnSizes, 1);
  ColumnSizes[0] := -1;
  Redirect := nil;
  InsertProc := nil;

  Items.Clear;
  fList := TStringList .Create;
  fInsertList := TStringList.Create;
  fFilter := '';
end;

destructor TAutoCompleteListBox.Destroy;
begin
  FreeAndNil(fList);
  FreeAndNil(fInsertList);

  inherited;
end;

procedure TAutoCompletePopup.DoShow;
begin
  //ClientHeight := Max(Min(Round(l.ClientHeight / l.ItemHeight), l.Count), 1) * l.ItemHeight;
  //ClientHeight := Max(Round(l.ClientHeight / l.ItemHeight), 1) * l.ItemHeight;
end;

function TAutoCompletePopup.getRedirect: TWinControl;
begin
  Result := ListBox.Redirect;
end;

procedure TAutoCompletePopup.setRedirect(Control: TWinControl);
begin
  ListBox.Redirect := Control;
end;

function TAutoCompletePopup.getInsertProc: TInsertProc;
begin
  Result := ListBox.InsertProc;
end;

procedure TAutoCompletePopup.setInsertProc(Proc: TInsertProc);
begin
  ListBox.InsertProc := Proc;
end;

constructor TAutoCompletePopup.Create(TheOwner: TComponent);
begin
  inherited;

  l := TAutoCompleteListBox.Create(Self);
  with l do
  begin
    Parent := Self;
    Align := alClient;
    OnKeyDown := {$IFDEF FPC}@{$ENDIF}HandleKeyDown;
    OnKeyPress := {$IFDEF FPC}@{$ENDIF}HandleKeyPress;
  end;

  DefaultMonitor := dmMainForm;
  FormStyle := fsStayOnTop;

  {$IFDEF FPC}
  ShowInTaskBar := stNever;
  {$ENDIF}

  {$IFDEF ccFORMCAPTION}
    {$IFDEF ccFORMRESIZE}
      BorderStyle := bsSizeToolWin;
      BorderIcons := [biSystemMenu];
    {$ELSE}
      BorderStyle := bsToolWindow;
      BorderIcons := [biSystemMenu];
    {$ENDIF}
  {$ELSE}
    {$IFDEF ccFORMRESIZE}
      BorderStyle := bsSizeToolWin;
      BorderIcons := [];
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and (not WS_CAPTION) or WS_BORDER);
      Height := Height - GetSystemMetrics(SM_CYCAPTION);
    {$ELSE}
      BorderStyle := bsNone;
      BorderIcons := [];
      l.BorderStyle := bsSingle;
    {$ENDIF}
  {$ENDIF}

  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];

  ClientHeight := (Round(ClientHeight / l.ItemHeight) * l.ItemHeight);
  Constraints.MinHeight := l.ItemHeight;
  Constraints.MinWidth := 100;
end;

procedure TAutoCompletePopup.DoHide;
begin
  ListBox.Clear;
  inherited;
end;

procedure TAutoCompletePopup.HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Visible then
  begin
    case Key of
      VK_UP:
        if (ListBox.Count > 0) and (ListBox.ItemIndex > 0) then
          ListBox.ItemIndex := ListBox.ItemIndex - 1;
      VK_DOWN:
        if (ListBox.Count > 0) and (ListBox.ItemIndex + 1 < ListBox.Count) then
          ListBox.ItemIndex := ListBox.ItemIndex + 1;
      VK_RETURN:
        ListBox.DblClick;
      VK_ESCAPE:
        Hide;
      else
        Exit;
    end;
    Key := 0;
  end;
end;

procedure TAutoCompletePopup.HandleKeyPress(Sender: TObject; var Key: char);
begin
  if Visible and (not (Key in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) then
    if (Key in ['.', '(', '[', ';', ':']) then
      ListBox.DblClick
    else
      Hide;
end;

procedure TAutoCompletePopup.Show(Pos: TPoint; ItemList, InsertList: TStrings; Filter: string = ''; Editor: TWinControl = nil);
begin
  ListBox.setLists(ItemList, InsertList);
  ListBox.Redirect := Editor;
  ListBox.Filter := Filter;
  Left := Pos.x;
  Top := Pos.y;

  inherited Show;

  if (Editor <> nil) then
    Editor.SetFocus;
end;

function StringListPartToText(BeginPos, EndPos : TPoint; Strings :TStrings) : string;
var
  i : integer;
begin;
  result := '';
  if endpos.y < beginpos.y then
    exit;
  if endpos.y >= strings.Count then
    exit;
  if beginpos.x > length(strings[beginpos.y]) then
    exit;
  if endpos.x > (length(strings[endpos.y])+1) then
    exit;
  if EndPos.y = beginpos.y then
  begin
    result := copy(strings[beginpos.y],beginpos.x, endpos.x - beginpos.x + 1);
    exit;
  end;
  result := copy(strings[beginpos.y],beginpos.x, length(strings[beginpos.y]) - beginpos.x + 1);
  for i := beginpos.y + 1 to endpos.y-1 do
    result := result + strings[i];
  result := result +  copy(strings[endpos.y],0,endpos.x-1); //Position <> count!
end;

function TParamHint.PrepareParamString(out Str: string; out MustHide : boolean): Integer;
var
  Parser : TmwPasLex;
  bracketcount, parameterindex,ParamC : integer;
  ParamNames : TDeclarationArray;
  typedecl : TDeclaration;
  s,TypeStr,Params : string;//
  i,ii :integer;
  CursorXY : TPoint;
begin
  result := -1;
  MustHide := True;
  Parser := TmwPasLex.Create;                 //The position of the bracket
  parser.Origin:= PChar(StringListPartToText(Point(FBracketPoint.x,FBracketPoint.y-1),
                                             point(min(FSynEdit.CaretX,length(FSynEdit.Lines[FSynEdit.CaretY - 1])+1),FSynEdit.CaretY-1),
                                             FSynEdit.lines));
  bracketcount := 0;
  ParameterIndex := -1;
  while parser.TokenID <> tkNull do
  begin
    case parser.tokenID of
      tkRoundOpen,tkSquareOpen:
        begin
          inc(BracketCount);
          if BracketCount = 1 then
            ParameterIndex := 0;
        end;
      tkRoundClose, tkSquareClose:
        begin
          dec(BracketCount);
          if bracketcount =0 then
            exit;
        end;
      tkComma:
        begin
          if bracketcount = 1 then
            inc(parameterIndex);
        end;
      end;
    parser.NextNoJunk;
  end;
  if parameterindex = -1 then
    exit;
  if parameterindex = LastParameterIndex then
  begin
    mustHide := false;
    str := fPreparedString;
    result := parameterindex;
    exit;
  end;
  str := '';
  ParamC := 0;
  typedecl := FDecl.Name;
  if typedecl = nil then
    exit;
  if typedecl.shorttext = '' then
    exit;
  for i := 0 to high(FParameters) do
  begin
    if (FParameters[i] is TciConstParameter) then
      s := 'const '
    else if (FParameters[i] is TciOutParameter) then
      s := 'out '
    else if (FParameters[i] is TciInParameter) then
      s := 'in '
    else if (FParameters[i] is TciVarParameter) then
      s := 'var '
    else
      s := '';
    ParamNames:= FParameters[i].Items.GetItemsOfClass(TciParameterName);
    TypeDecl := FParameters[i].Items.GetFirstItemOfClass(TciParameterType);
    if TypeDecl <> nil then
      TypeStr := ': ' + typedecl.ShortText
    else
      TypeStr := '';
    Params := '';
    for ii := 0 to high(ParamNames) do
    begin;
      if parameterindex = ParamC then //Found the current parameter index in the parameterdecl!
      begin;
        if s <> '' then
          s := '\' + s + '\'; //If it has a const/var/in/out thingy, bold this as well
        if TypeStr <> '' then        //If has a type then bold the type
          TypeStr := '\' + TypeStr + '\';
        if Params <> '' then
          Params := Params +', \' + ParamNames[ii].ShortText + '\'
        else
          Params := '\' + ParamNames[ii].ShortText + '\';
      end else
      begin;
        if Params <> '' then
          Params := Params +', ' +  ParamNames[ii].ShortText
        else
          Params := ParamNames[ii].ShortText;
      end;
      inc(ParamC);
    end;
    if str <> '' then
      str := str + ';' + s + Params + typestr
    else
      str := s + params + typestr;
  end;
  TypeDecl := FDecl.Items.GetFirstItemOfClass(TciReturnType);
  if TypeDecl <> nil then
    TypeStr := ': ' + typedecl.ShortText
  else
    TypeStr := '';
  str := FDecl.Name.ShortText + '(' +  str + ')' + TypeStr + ';';
  str := StringReplace(str,'\\','',[rfReplaceAll]); //Delete all the \\, something like \const \\x\ is the same as \const x\
  MustHide := False;
  Result := parameterindex;
  fPreparedString := str;
  Parser.Free;
end;

destructor TParamHint.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationIdle);
  inherited Destroy;
end;


constructor TParamHint.Create(TheOwner: TComponent);
begin
  inherited;
  {$IFDEF FPC}
  AutoHide := False;
  {$ENDIF}
  OnHide:=@ParamHintHide;
  LastParameterIndex:= -1;
  Application.AddOnIdleHandler(@ApplicationIdle);
end;

procedure TParamHint.CalculateBounds;
var
  DrawWidth: LongInt;
  DrawHeight: LongInt;
  ScreenTextXY: TPoint;
  ClientXY: TPoint;
  ScreenXY: TPoint;
begin
  ScreenTextXY := FSynEdit.LogicalToPhysicalPos(FStartPoint);
  ClientXY := FSynEdit.RowColumnToPixels(ScreenTextXY);
  DrawWidth := FSynEdit.ClientWidth;  //Maximum width it can have..
  DrawHeight := ClientXY.y; //Maximum height it can have..
  DrawHints(DrawWidth,DrawHeight,false); //Calculate the max size we need!
  if DrawWidth<20 then DrawWidth:=20; //Some default values!
  if DrawHeight<5 then DrawHeight:=5;

  if ClientXY.X+DrawWidth>FSynedit.ClientWidth then //If we go out of bounds, lets put it to the left a bit.
    ClientXY.X:=FSynedit.ClientWidth-DrawWidth;
  if ClientXY.X<0 then //If we go to the left a lil bit to much, go to the right!
    ClientXY.X:=0;
  dec(ClientXY.Y,DrawHeight); //Move this a lil bit up!
  if ClientXY.y < 0 then
    ClientXY.y := 0;

  ScreenXY:=FSynedit.ClientToScreen(ClientXY); //Position on the screen
  dec(ScreenXY.Y,4); //Move it up a lilttle bit above your text, to make the shade come out better?

  //Set the new position
  BoundsRect:=Bounds(ScreenXY.X,ScreenXY.Y,DrawWidth,DrawHeight);
end;

procedure TParamHint.UpdateHint;
var
  MustHide : boolean;
  CursorXY : TPoint;
  Line : string;
begin
  if not self.Visible then
    exit;
  try
    MustHide := True;
    if not Assigned(FSynEdit) then
      exit;
    if FSynEdit.Focused = false then //No focus, hide this hint
      exit;          //Exits to the finally statement ;)
    CursorXY := FSynEdit.LogicalCaretXY;
    if (CursorXY.x <= FBracketPoint.x) and (CursorXY.y <= FBracketPoint.y) then //Cursor moved in front of the bracket
      exit;
    Line:=FSynEdit.Lines[FBracketPoint.Y-1];
    if (length(Line)<FBracketPoint.X) or (not (Line[FBracketPoint.X] in ['(','['])) then
      exit;
    if PrepareParamString(Line,MustHide) = LastParameterIndex then
      exit
    else if not MustHide then
      Self.Invalidate;
  finally
    if MustHide then
      Self.hide;
  end;
end;

procedure TParamHint.ParamHintHide(Sender: TObject);
begin
  if FMP <> nil then
    freeandnil(Fmp);
end;

procedure TParamHint.DrawHints(var MaxWidth, MaxHeight: Integer;
  Draw: boolean);
var
  HorizontalSpace: Integer;
  VerticalSpace: Integer;
  BackgroundColor, TextGrayColor, TextColor, PenColor: TColor;
  TextGrayStyle, TextStyle: TFontStyles;

  procedure DrawHint(const Line: string; var AHintRect: TRect);
  var
    ATextRect: TRect; //The area we can use
    TokenRect: TRect; //The area the text takes up
    TokenSize: TPoint; //The W/H the text takes up
    TokenPos: TPoint; //The position where the text is drawn
    UsedWidth: Integer; // maximum right token position
    LineHeight: Integer; // Current line height
    Bolding : boolean; //If we are in a bolding part.
    Pos : integer;
    StartPos : integer;
    //Text takes up it's own Width/Height + the space around the text.
  begin
    ATextRect:=Rect(AHintRect.Left+HorizontalSpace,
                    AHintRect.Top+VerticalSpace,
                    AHintRect.Right-HorizontalSpace,
                    AHintRect.Bottom-VerticalSpace);//Possible area!
    UsedWidth:=0;
    LineHeight:=0;
    TokenPos:=Point(ATextRect.Left,ATextRect.Top); //StartPoint like (0,0)
    Bolding := False;
    Pos := 0;
    //Split the drawing up in words, that way we can split the function if it gets to long ;).
    while (Pos < Length(Line)) do
    begin
      inc(Pos);
      if (Line[Pos] = '\') then  //Bold from now
      begin;
        if Draw then
        begin
          if not Bolding then
          begin
            Canvas.Font.Color := TextColor;
            Canvas.Font.Style := TextStyle;
          end else
          begin
            Canvas.Font.Color := TextGrayColor;
            Canvas.Font.Style := TextGrayStyle;
          end;
          Bolding := not Bolding;
        end;
        continue;
      end;
      StartPos := Pos;
      if (Line[Pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then //We are in a word, lets draw that completely ;)
      begin
        while ((Pos < length(line)) and (Line[Pos + 1] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) do
          inc(pos);
      end else
        while ((Pos < length(line)) and not(Line[Pos + 1] in ['a'..'z', 'A'..'Z', '0'..'9', '_','\'])) do
          inc(pos);
      TokenRect:=Bounds(0,0,12345,1234); //Random rect
      DrawText(Canvas.Handle,@Line[StartPos],Pos-StartPos + 1,TokenRect,
               DT_SINGLELINE+DT_CALCRECT+DT_NOCLIP); //Calculate the size it takes to draw this text
      TokenSize:=Point(TokenRect.Right,TokenRect.Bottom); //The size it takes to draw this text
      if (LineHeight>0) and (TokenPos.X+TokenSize.X>ATextRect.Right) then  //It doesn't fit.. Text = 2 long
      begin
        if Draw and (TokenPos.X<AHintRect.Right) then //Fill the rest of the area with blank info,
                                                     //since we are going to draw this text on the next line
          Canvas.FillRect(Rect(TokenPos.X,TokenPos.Y-VerticalSpace,
                               AHintRect.Right,TokenPos.Y+LineHeight+VerticalSpace));
        TokenPos:=Point(ATextRect.Left,TokenPos.y+LineHeight+VerticalSpace);//Lets start on the left side, one row below ;)
        LineHeight:=0;
      end;
      OffsetRect(TokenRect,TokenPos.x,TokenPos.y); //Move the tokenrectangle to the tokenposition
      if Draw then
      begin
        Canvas.FillRect(Rect(TokenRect.Left,TokenRect.Top-VerticalSpace,
                             TokenRect.Right,TokenRect.Bottom+VerticalSpace));//Fill the entire rect (means including the spaces above and below
        DrawText(Canvas.Handle,@Line[StartPos],Pos-StartPos + 1,
                 TokenRect,DT_SINGLELINE+DT_NOCLIP);  //Draw the text!
      end;
      if LineHeight<TokenSize.y then
        LineHeight:=TokenSize.y;   //the line has a bigger height than before.. The text H is bigger.
      inc(TokenPos.X,TokenSize.x);  //Move the tokenposition text-width to the right
      if UsedWidth<TokenPos.X then //Calculate the max-width we've used!
        UsedWidth:=TokenPos.X;
    end;
    if Draw and (TokenPos.X<AHintRect.Right) and (LineHeight>0) then  //Fill the rest of the unused area
      Canvas.FillRect(Rect(TokenPos.X,TokenPos.Y-VerticalSpace,
                      AHintRect.Right,TokenPos.Y+LineHeight+VerticalSpace));
    if (not Draw) and (UsedWidth>0) then
      AHintRect.Right:=UsedWidth+HorizontalSpace; //Calculate the width we actually need
    AHintRect.Bottom:=TokenPos.Y+LineHeight+VerticalSpace;
  end;

var
  CurHintRect: TRect;
  MustHide : boolean;
  hintstr: string;
begin
  if Draw then
  begin
    BackgroundColor:=clInfoBk;
    TextGrayColor:=clInfoText;
    TextGrayStyle:=[];
    TextColor:=clInfoText;
    TextStyle:=[fsBold];
    PenColor:=clBlack;
  end;
  HorizontalSpace:=2; //The spaces around the text
  VerticalSpace:=2;

  if Draw then begin
    Canvas.Brush.Color:=BackgroundColor;
    Canvas.Font.Color:=TextGrayColor;
    Canvas.Font.Style:=TextGrayStyle;
    Canvas.Pen.Color:=PenColor;
  end else begin
    Canvas.Font.Style:=[fsBold]; //Let us calculate the maximum width we need :)
  end;
  CurHintRect:=Rect(0,0,MaxWidth,MaxHeight);
  PrepareParamString(HintStr,MustHide);
  if MustHide then
  begin;
    Self.Hide;
    exit;
  end;
  DrawHint(HintStr, CurHintRect);

  if Draw then //Fill the rest if needed.. (Possible if we calculated we need 2 rows, but turns out we need only 1 this time).
  begin
    if CurHintRect.Bottom<MaxHeight then
      Canvas.FillRect(Rect(0,CurHintRect.Bottom,MaxWidth,MaxHeight));
    // draw frame around window
    Canvas.Frame(Rect(0,0,MaxWidth-1,MaxHeight-1));
  end;
  if not Draw then //Adjust the maxwidth/maxheight needed to draw this thingy!
  begin
    if CurHintRect.right<MaxWidth then
      MaxWidth:=CurHintRect.right;
    if CurHintRect.Bottom<MaxHeight then
      MaxHeight:=CurHintRect.Bottom;
  end;
end;

procedure TParamHint.Paint;
var
  MaxWidth,MaxHeight : integer;
begin
  MaxWidth:= ClientWidth;
  MaxHeight := ClientHeight;
  DrawHints(MaxWidth,MaxHeight,True);
end;

procedure TParamHint.Show(StartPoint,BracketPoint: TPoint;Decl : TciProcedureDeclaration; Editor: TSynedit; mp : TCodeInsight);
begin
  if self.Visible then
    self.hide;
  FDecl := Decl;
  Fmp := mp;
  FParameters:= Decl.GetParamDeclarations;
  if Length(FParameters) = 0 then //Method has no Parameters
    exit;
  FSynEdit := Editor;
  FStartPoint:= StartPoint;
  FBracketPoint:= BracketPoint;
  CalculateBounds;  //Calculate the size we need!
  self.Visible := true;
end;


procedure TParamHint.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if not Visible then exit;
  UpdateHint;
end;

end.

