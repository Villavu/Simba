unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, SynHighlighterPas, SynEdit, SynEditHighlighter, SynEditKeyCmds,

  lclintf, ComCtrls, lcltype,

  v_ideCodeInsight, v_ideCodeParser,  v_AutoCompleteForm,
  CastaliaPasLexTypes, CastaliaSimplePasPar;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnParams: TButton;
    btnParse: TButton;
    btnComplete: TButton;
    e: TSynEdit;
    hlPas: TSynPasSyn;
    lstDump: TListBox;
    txtDebug: TMemo;
    pnlLeft: TPanel;
    procedure btnCompleteClick(Sender: TObject);
    procedure btnParamsClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure eClickLink(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure eCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure eKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eKeyPress(Sender: TObject; var Key: char);
    procedure eMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure eProcessUserCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure eStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
  protected
    acp_start: TPoint;
    acp: TAutoCompletePopup;

    procedure OnM(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    procedure CompleteCode(Str: string);
    function OnFindInclude(Sender: TObject; var FileName: string): Boolean;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  ecAutoComplete = ecUserFirst + 1;
  ecParamHint = ecUserFirst + 2;

function WordAtCaret(e: TSynEdit; var sp, ep: Integer; Start: Integer = -1): string;
var
  s: string;
  l: Integer;
begin
  Result := '';
  if (Start = -1) then
    Start := e.CaretX;
  sp := Start - 1;
  ep := Start - 1;
  s := e.Lines[e.CaretY - 1];
  l := Length(s);
  //if (sp > l) then
  //  Dec(sp);

  if (sp < 1) or (sp > l) or (not (s[sp] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) then
  begin
    Inc(sp);
    Inc(ep);
    if (sp < 1) or (sp > l) or (not (s[sp] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) then
      Exit('');
  end;

  while (sp > 1) and (sp <= l) and (s[sp - 1] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
    Dec(sp);
  while (ep >= 1) and (ep < l) and (s[ep + 1] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
    Inc(ep);

  Result := Copy(s, sp, ep - sp + 1);
end;

{ TMainForm }

procedure TMainForm.OnM(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  if (Typ = meNotSupported) then
    Exit;
  if (Sender is TmwSimplePasPar) then
    if (TmwSimplePasPar(Sender).Lexer.TokenID = tok_DONE) then
      Exit;
  txtDebug.Lines.Add('ERROR: '+Format('%d:%d %s', [Y + 1, X, Msg])+' in '+TCodeInsight(Sender).FileName);
end;

procedure TMainForm.CompleteCode(Str: string);
var
  sp, ep: Integer;
  s: string;
begin
  if (Str <> '') then
  begin
    s := WordAtCaret(e, sp, ep);
    if (s <> '') then
    begin
      e.SelStart := e.SelStart + (sp - e.CaretX);
      e.SelEnd := e.SelStart + (ep - e.CaretX) + 1;
      e.SelText := Str;
    end
    else
      e.InsertTextAtCaret(Str);
  end;
end;

function TMainForm.OnFindInclude(Sender: TObject; var FileName: string): Boolean;
begin
  if FileExists('C:\Program Files\SCAR 3.20\includes\'+FileName) then
  begin
    FileName := 'C:\Program Files\SCAR 3.20\includes\'+FileName;
    Result := True;
  end
  else
    Result := False;
end;

procedure TMainForm.btnParseClick(Sender: TObject);

  procedure PrintDeclaration(Item: TDeclaration; Strings: TStrings; Prefix: string);
  var
    i: Integer;
  begin
    Strings.Add(Prefix + '(' + IntToStr(Item.StartPos) +',' + IntToStr(Item.EndPos) + ') ' + Item.CleanText + '(' + Item.ClassName + ') ');
    for i := 0 to Item.Items.Count - 1 do
      PrintDeclaration(Item.Items[i], Strings, Prefix + '-');
  end;

  procedure PrintCodeInsight(Item: TCodeInsight; Strings: TStrings);
  var
    i: Integer;
  begin
    Strings.Add('*START***START***START*');
    for i := 0 to Item.Items.Count - 1 do
      PrintDeclaration(Item.Items[i], Strings, '');
    Strings.Add('*INCLUDES***INCLUDES*');
    for i := 0 to High(Item.Includes) do
      PrintCodeInsight(Item.Includes[i], Strings);
    Strings.Add('*END***END***END***END*');
  end;

var
  mp: TCodeInsight;
  ms: TMemoryStream;
  t: Integer;
begin
  mp := TCodeInsight.Create;
  mp.OnMessage := @OnM;
  mp.OnFindInclude := @OnFindInclude;

  ms := TMemoryStream.Create;

  t := GetTickCount;
  e.Lines.SaveToStream(ms);

  try
    lstDump.Items.BeginUpdate;
    lstDump.Clear;

    t := GetTickCount;
    mp.Run(ms);
    txtDebug.Lines.Add(IntToStr(GetTickCount - t) + 'ms');

    PrintCodeInsight(mp, lstDump.Items);
    //mp.FillSynCompletionProposal(lstDump.Items, InsertList);
  finally
    FreeAndNil(ms);
    FreeAndNil(mp);
    lstDump.Items.EndUpdate;
  end;
end;

procedure TMainForm.eClickLink(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  mp: TCodeInsight;
  ms: TMemoryStream;
  d: TDeclaration;
  sp, ep: Integer;
begin
  mp := TCodeInsight.Create;
  mp.OnMessage := @OnM;
  mp.OnFindInclude := @OnFindInclude;

  ms := TMemoryStream.Create;
  e.Lines.SaveToStream(ms);

  try
    e.GetWordBoundsAtRowCol(e.CaretXY, sp, ep);
    mp.Run(ms);
    mp.Position := e.SelStart + (ep - e.CaretX) - 1;

    d := mp.FindVarBase(mp.GetExpressionAtPos);
    if (d <> nil) then
    begin
      if (TCodeInsight(d.Parser).FileName <> mp.FileName) then
        ShowMessage('Declared in "' + TCodeInsight(d.Parser).FileName  + '" at ' + IntToStr(d.StartPos))
      else
      begin
        e.SelStart := d.StartPos + 1;
        e.SelEnd := d.StartPos + Length(TrimRight(d.RawText)) + 1;
      end;
    end;

  finally
    FreeAndNil(ms);
    FreeAndNil(mp);
  end;
end;

procedure TMainForm.eCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
var
  sp, ep: Integer;
  s: string;
begin
  if acp.Visible then
    case Command of
      ecDeleteChar, ecDeleteWord, ecDeleteEOL:
        begin
          if (e.CaretY = acp_start.y) then
          begin
            //e.GetWordBoundsAtRowCol(acp_start, sp, ep);
            s := WordAtCaret(e, sp, ep, acp_start.x);
            if (e.CaretX >= acp_start.x) and (e.CaretX <= ep) then
            begin
              acp.ListBox.Filter := s;
              Exit;
            end;
          end;

          acp.Hide;
        end;
    end;
end;

procedure TMainForm.eKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  acp.HandleKeyDown(Sender, Key, Shift);
end;

procedure TMainForm.eKeyPress(Sender: TObject; var Key: char);
begin
  acp.HandleKeyPress(Sender, Key);
end;

procedure TMainForm.eMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
var
  s: string;
  Attri: TSynHighlighterAttributes;
begin
  AllowMouseLink := e.GetHighlighterAttriAtRowCol(Point(X, Y), s, Attri) and (Attri.Name = 'Identifier');
end;

procedure TMainForm.eProcessUserCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
var
  s: string;
  Attri: TSynHighlighterAttributes;
begin
  if (Command = ecAutoComplete) then
  begin
    if (not e.GetHighlighterAttriAtRowCol(e.CaretXY, s, Attri)) or (Attri.Name = 'Identifier') then
    begin
      btnCompleteClick(nil);
      Command := ecNone;
    end;
  end
  else if (Command = ecParamHint) then
  begin
    btnParamsClick(nil);
    Command := ecNone;
  end;
end;

procedure TMainForm.eStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  sp, ep: Integer;
  s: string;
begin
  if acp.Visible then
    if (scAll in Changes) or (scTopLine in Changes) then
      acp.Visible := False
    else if (scCaretX in Changes) or (scCaretY in Changes) or (scSelection in Changes) or (scModified in Changes) then
    begin
      if (e.CaretY = acp_start.y) then
      begin
        //e.GetWordBoundsAtRowCol(acp_start, sp, ep);
        s := WordAtCaret(e, sp, ep, acp_start.x);
        if (e.CaretX >= acp_start.x) and (e.CaretX - 1 <= ep) then
        begin
          acp.ListBox.Filter := s;
          Exit;
        end;
      end;

      acp.Hide;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  acp_start := Point(-1, -1);
  acp := TAutoCompletePopup.Create(Self);
  acp.InsertProc := @CompleteCode;

  e.AddKey(ecAutoComplete, VK_SPACE, [ssCtrl], VK_UNKNOWN, []);
  e.AddKey(ecParamHint, VK_SPACE, [ssShift, ssCtrl], VK_UNKNOWN, []);
end;

procedure TMainForm.btnCompleteClick(Sender: TObject);
var
  mp: TCodeInsight;
  ms: TMemoryStream;
  ItemList, InsertList: TStringList;
  sp, ep, t: Integer;
  p: TPoint;
  s, Filter: string;
begin
  mp := TCodeInsight.Create;
  mp.OnMessage := @OnM;
  mp.OnFindInclude := @OnFindInclude;

  ms := TMemoryStream.Create;
  ItemList := TStringList.Create;
  InsertList := TStringList.Create;
  InsertList.Sorted := True;

  e.Lines.SaveToStream(ms);

  try
    //e.GetWordBoundsAtRowCol(e.CaretXY, sp, ep);
    Filter := WordAtCaret(e, sp, ep);
    acp_start := Point(sp, e.CaretY);
    //mp.Position := e.SelStart + (ep - e.CaretX) - 1;

    mp.Run(ms, nil, e.SelStart + (ep - e.CaretX) - 1);

    s := mp.GetExpressionAtPos;
    if (s <> '') then
    begin
      sp := LastDelimiter('.', s);
      if (sp > 0) then
        Delete(s, sp, Length(s) - sp + 1)
      else
        s := '';
    end;

    t := GetTickCount;
    mp.FillSynCompletionProposal(ItemList, InsertList, s);
    txtDebug.Lines.Add('Fill: '+IntToStr(GetTickCount - t)+'ms');

    p := e.ClientToScreen(e.RowColumnToPixels(Point(ep, e.CaretY)));
    p.y := p.y + e.LineHeight;

    t := GetTickCount;
    acp.Show(p, ItemList, InsertList, Filter, e);
    txtDebug.Lines.Add('Show: '+IntToStr(GetTickCount - t)+'ms');
  finally
    FreeAndNil(ms);
    FreeAndNil(mp);
    ItemList.Free;
    InsertList.Free;
  end;
end;

procedure TMainForm.btnParamsClick(Sender: TObject);
var
  mp: TCodeInsight;
  ms: TMemoryStream;
  d, dd: TDeclaration;
  sp, ep, bcc, bck, cc: Integer;
  s: string;
begin
  mp := TCodeInsight.Create;
  mp.OnMessage := @OnM;
  mp.OnFindInclude := @OnFindInclude;

  ms := TMemoryStream.Create;
  e.Lines.SaveToStream(ms);

  try
    e.GetWordBoundsAtRowCol(e.CaretXY, sp, ep);
    mp.Run(ms, nil, e.SelStart + (ep - e.CaretX) - 1);
    //mp.Position := e.SelStart + (ep - e.CaretX) - 1;

    bcc := 1;
    bck := 0;
    cc := 0;
    s := mp.GetExpressionAtPos(bcc, bck, cc, True);
    if (s <> '') then
      Delete(s, Length(s), 1);

    d := mp.FindVarBase(s);
    dd := nil;
    while (d <> nil) and (d <> dd) and (d.Owner <> nil) and (not ((d is TciProcedureDeclaration) or (d.Owner is TciProcedureDeclaration))) do
    begin
      dd := d;
      d := d.Owner.Items.GetFirstItemOfClass(TciTypeKind);
      if (d <> nil) then
      begin
        d := TciTypeKind(d).GetRealType;
        if (d is TciReturnType) then
          d := d.Owner;
      end;
      if (d <> nil) and (d.Owner <> nil) and (not ((d is TciProcedureDeclaration) or (d.Owner is TciProcedureDeclaration))) then
        d := mp.FindVarBase(d.CleanText)
      else
        Break;
    end;
    if (d <> nil) and (d <> dd) and (d.Owner <> nil) and ((d is TciProcedureDeclaration) or (d.Owner is TciProcedureDeclaration)) then
    begin
      if (d.Owner is TciProcedureDeclaration) and (not (d is TciProcedureDeclaration)) then
        d := d.Owner;
      with TParamHint.Create(Self) do
      begin
      if (TciProcedureDeclaration(d).SynParams <> '') then
        //txtDebug.Lines.Add(TciProcedureDeclaration(d).SynParams)
        Caption := TciProcedureDeclaration(d).SynParams
      else
        //txtDebug.Lines.Add('<no parameters expected>');
        Caption := '<no parameters expected>';
      end;
    end;
  finally
    FreeAndNil(ms);
    FreeAndNil(mp);
  end;
end;

end.

