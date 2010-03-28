unit v_AutoCompleteForm;

interface

{$I ValistusDefines.inc}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,  SynEdit, SynEditKeyCmds,

  {$IFDEF FPC}
  LMessages,
  lcltype
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

  TParamHint = class(THintWindow)
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
  end;

implementation

uses
  StrUtils {$IFDEF FPC}, lclintf{$ENDIF}, Themes;

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

  if {$IFDEF FPC}(odPainted in State) or{$ENDIF} (not Visible) or (ARect.Left > ClientRect.Right) or (ARect.Top > ClientRect.Bottom) or (Index < 0) or (Index >= Items.Count) then
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

constructor TParamHint.Create(TheOwner: TComponent);
begin
  inherited;

  {$IFDEF FPC}
  AutoHide := False;
  {$ENDIF}
end;

procedure TParamHint.Paint;

  function GetDrawTextFlags: Cardinal;
  var
    EffectiveAlignment: TAlignment;
  begin
    Result := DT_NOPREFIX or DT_VCENTER or DT_WORDBREAK;
    EffectiveAlignment := Alignment;
    if BiDiMode <> bdLeftToRight then
    begin
      Result := Result or DT_RTLREADING;
      //change alignment if is RTL
      if BiDiMode = bdRightToLeft then
      begin
        case Alignment of
          taLeftJustify: EffectiveAlignment := taRightJustify;
          taRightJustify: EffectiveAlignment := taLeftJustify;
        end;
      end;
    end;
    case EffectiveAlignment of
      taLeftJustify: Result := Result or DT_LEFT;
      taCenter: Result := Result or DT_CENTER;
      taRightJustify: Result := Result or DT_RIGHT;
    end;
end;

var
  ARect: TRect;
  Details: TThemedElementDetails;
begin
  ARect := ClientRect;
  if Color = clInfoBk then // draw using themes
  begin
    Details := ThemeServices.GetElementDetails(tttStandardLink);
    ThemeServices.DrawElement(Canvas.Handle, Details, ARect);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Width := 1;
    Canvas.FillRect(ARect);
    DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
  end;
  InflateRect(ARect, - 4, - 4);
  Canvas.TextOut(ARect.Left, ARect.Top, Caption);
end;

end.

