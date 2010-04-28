{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    framescript for the Mufasa Macro Library
}  
unit framescript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, SynHighlighterPas, SynEdit,   SynEditMarkupHighAll,
   mmlpsthread,ComCtrls, SynEditKeyCmds, LCLType,MufasaBase, SynEditMarkupSpecialLine, Graphics, Controls,    SynEditStrConst,
  v_ideCodeInsight, v_ideCodeParser, CastaliaPasLexTypes, CastaliaSimplePasPar, SynEditHighlighter,synedittextbase;
const
   ecCodeCompletion = ecUserFirst;
   ecCodeHints = ecUserFirst + 1;
type
  TScriptState = (ss_None,ss_Running,ss_Paused,ss_Stopping);
  {
    ss_None: Means the script either hasn't been run yet, or it has ended (Succesfully or terminated)
    ss_Running: Means the script is running as we speak :-)
    ss_Paused: Means the script is currently in pause modus.
    ss_Stopping: Means we've asked PS-Script politely to stop the script (next time we press the stop button we won't be that nice).
  }

  { TScriptFrame }

  TScriptFrame = class(TFrame)
    SynEdit: TSynEdit;
    procedure SynEditChange(Sender: TObject);
    procedure SynEditClickLink(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEditCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure SynEditDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynEditDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SynEditKeyPress(Sender: TObject; var Key: char);
    procedure SynEditMouseLink(Sender: TObject; X, Y: Integer;
      var AllowMouseLink: Boolean);
    procedure SynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure SynEditProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    OwnerPage  : TPageControl;
    OwnerSheet : TTabSheet;//The owner TTabsheet -> For title setting
  public
    ErrorData : TErrorData;  //For threadsafestuff
    ScriptErrorLine : integer; //Highlight the error line!
    ScriptFile : string;//The path to the saved/opened file currently in the SynEdit
    StartText : string;//The text synedit holds upon start/open/save
    ScriptName : string;//The name of the currently opened/saved file.
    ScriptDefault : string;//The default script e.g. program new; begin end.
    ScriptChanged : boolean;//We need this for that little * (edited star).
    ScriptThread : TMThread;//Just one thread for now..
    FScriptState : TScriptState;//Stores the ScriptState, if you want the Run/Pause/Start buttons to change accordingly, acces through Form1
    procedure undo;
    procedure redo;
    procedure HandleErrorData;
    procedure MakeActiveScriptFrame;
    procedure ScriptThreadTerminate(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    { public declarations }
  end;

  function WordAtCaret(e: TSynEdit; var sp, ep: Integer; Start: Integer = -1): string;

implementation
uses
  TestUnit, SynEditTypes, LCLIntF, StrUtils,framefunctionlist;

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

function PosToCaretXY(e : TSynEdit; pos : integer) : TPoint;
  function llen(const data: string): integer;
  begin
    result := length(Data) + length(LineEnding);
  end;

var
  loop: integer;
  count: integer;
  Lines : TStrings;
begin
  loop := 0;
  count := 0;
  Lines := e.Lines;
  while (loop < Lines.Count) and (count + llen(Lines[loop]) < pos) do begin
    count := count + llen(Lines[loop]);
    inc(loop);
  end;
  result.x := pos - count;
  result.y := loop + 1;
end;

{ TScriptFrame }

procedure TScriptFrame.SynEditChange(Sender: TObject);
begin
  ScriptErrorLine:= -1;
  if not ScriptChanged then
  begin;
    ScriptChanged:= True;
    Form1.Caption:= Format(WindowTitle,[ScriptName + '*']);
    OwnerSheet.Caption:=ScriptName + '*';
  end;
end;

procedure TScriptFrame.SynEditClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  mp: TCodeInsight;
  ms: TMemoryStream;
  d: TDeclaration;
  sp, ep: Integer;
  s : string;
begin
  mp := TCodeInsight.Create;
  mp.FileName := ScriptFile;
  mp.OnMessage := @Form1.OnCCMessage;
  mp.OnFindInclude := @Form1.OnCCFindInclude;

  ms := TMemoryStream.Create;
  SynEdit.Lines.SaveToStream(ms);

  try
    SynEdit.GetWordBoundsAtRowCol(SynEdit.CaretXY, sp, ep);
    s := SynEdit.Lines[SynEdit.Carety-1];
    if ep > length(s) then //We are outside the real text, go back to the last char
      mp.Run(ms, nil, Synedit.SelStart - ep + length(s),false)
    else
      mp.Run(ms, nil, Synedit.SelStart + (ep - Synedit.CaretX) - 1,false);
    mp.Position := SynEdit.SelStart + (ep - SynEdit.CaretX) - 1;

    d := mp.FindVarBase(mp.GetExpressionAtPos);
    if (d <> nil) then
    begin
      if (TCodeInsight(d.Parser).FileName <> mp.FileName) then
      begin
        if FileExists(TCodeInsight(d.Parser).FileName) then
        begin;
          if Form1.LoadScriptFile(TCodeInsight(d.Parser).FileName,true,true) then
          begin;
            Form1.CurrScript.SynEdit.SelStart:= d.StartPos + 1;
            Form1.CurrScript.SynEdit.SelEnd := d.StartPos + Length(TrimRight(d.RawText)) + 1;
          end;
        end
        else
          mDebugLn('Declared in "' + TCodeInsight(d.Parser).FileName  + '" at ' + IntToStr(d.StartPos));
      end else
      begin
        SynEdit.SelStart := d.StartPos + 1;
        SynEdit.SelEnd := d.StartPos + Length(TrimRight(d.RawText)) + 1;
      end;
    end;

  finally
    FreeAndNil(ms);
    FreeAndNil(mp);
  end;
end;

procedure TScriptFrame.SynEditCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
var
  Command2 : TSynEditorCommand;
begin
  if (Command = ecChar) and (AChar = '(') and (Form1.ParamHint.Visible = false) and (Form1.ShowHintAuto) then
  begin
    Command2:= ecCodeHints;
    SynEditProcessUserCommand(sender,command2,achar,nil);
  end;
end;

procedure TScriptFrame.SynEditDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source is TFunctionListFrame then
    if TFunctionListFrame(Source).DraggingNode.Data <> nil then
      SynEdit.InsertTextAtCaret( GetMethodName(PMethodInfo(TFunctionListFrame(Source).DraggingNode.Data)^.MethodStr,true));
end;

procedure TScriptFrame.SynEditDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Form1.frmFunctionList;
  if(Accept)then
  begin
    SynEdit.CaretXY := SynEdit.PixelsToLogicalPos(point(x, y));
    if(not(Form1.Active))then Form1.BringToFront;
    if(Form1.ActiveControl <> SynEdit)then Form1.ActiveControl := SynEdit;
  end;
end;

procedure TScriptFrame.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_F3 then
  begin;
    Form1.ActionFindNextExecute(Sender);
    key := 0;
  end;
  if key = VK_ESCAPE then
    Form1.ParamHint.Hide;

  Form1.CodeCompletionForm.HandleKeyDown(Sender, Key, Shift);
end;

procedure TScriptFrame.SynEditKeyPress(Sender: TObject; var Key: char);
begin
  Form1.CodeCompletionForm.HandleKeyPress(Sender, Key);
end;

procedure TScriptFrame.SynEditMouseLink(Sender: TObject; X, Y: Integer;
  var AllowMouseLink: Boolean);
var
  s: string;
  Attri: TSynHighlighterAttributes;
begin
  AllowMouseLink := SynEdit.GetHighlighterAttriAtRowCol(Point(X, Y), s, Attri) and (Attri.Name = SYNS_AttrIdentifier);
end;

procedure TScriptFrame.SynEditProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  case Command of
    ecUndo :  begin
                Command:= ecNone;
                Self.Undo;
              end;
    ecRedo :  begin
                Command := ecNone;
                self.Redo;
              end;
  end;
end;

procedure TScriptFrame.SynEditProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
{var
  LineText,SearchText : string;
  Caret : TPoint;
  i,endI : integer;}
var
  mp: TCodeInsight;
  ms: TMemoryStream;
  ItemList, InsertList: TStringList;
  sp, ep,bcc,cc,bck,posi,bracketpos: Integer;
  p: TPoint;
  s, Filter: string;
  Attri: TSynHighlighterAttributes;
  d: TDeclaration;
  dd: TDeclaration;
begin
  if (Command = ecCodeCompletion) and ((not SynEdit.GetHighlighterAttriAtRowCol(SynEdit.CaretXY, s, Attri)) or
                                      ((Attri.Name <> SYNS_AttrComment) and (Attri.name <> SYNS_AttrString) and (Attri.name <> SYNS_AttrDirective))) then
  begin
      {form1.FunctionListShown(True);
      with form1.frmFunctionList do
        if editSearchList.CanFocus then
        begin;
          editSearchList.SetFocus;
          LineText := SynEdit.LineText;
          Caret:=SynEdit.LogicalCaretXY;
          i := Caret.X - 1;
          endi := caret.x;
          if (i > length(LineText)) or ((i = 0) and (length(lineText) = 0)) then
          begin
            SearchText:= '';
            CompletionLine := PadRight(linetext,caret.x);
          end
          else begin
            while (i > 0) and (LineText[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
              dec(i);
            while LineText[endi] in ['a'..'z','A'..'Z','0'..'9','_'] do
              inc(endi);
            SearchText := Trim(copy(LineText, i + 1, Caret.X - i - 1));
            CompletionLine := LineText;
          end;
          CompletionStart:= LineText;
          Delete(CompletionLine,i+1,endi - i - 1);
          Insert('%s',CompletionLine,i+1);
          CompletionCaret := Point(endi,Caret.y);
          StartWordCompletion:= Point(i+1,caret.y);
          mDebugLn(CompletionLine);
          mDebugLn(CompletionStart);
          InCodeCompletion := true;
          editSearchList.Text:= SearchText;
          editSearchList.SelStart:= Length(searchText);
          SynEdit.SelectedColor.Style:= [fsUnderline];
          SynEdit.SelectedColor.Foreground:= clBlack;
          SynEdit.SelectedColor.Background:= clWhite;
          Synedit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable;
        end;}
    mp := TCodeInsight.Create;
    mp.FileName := ScriptFile;
    mp.OnMessage := @Form1.OnCCMessage;
    mp.OnFindInclude := @Form1.OnCCFindInclude;

    ms := TMemoryStream.Create;
    ItemList := TStringList.Create;
    InsertList := TStringList.Create;
    InsertList.Sorted := True;

    Synedit.Lines.SaveToStream(ms);

    try
      Filter := WordAtCaret(Synedit, sp, ep);
      Form1.CodeCompletionStart := Point(sp, Synedit.CaretY);

      //mp.Run(ms, nil, Synedit.SelStart + (ep - Synedit.CaretX) - 1);
      s := SynEdit.Lines[SynEdit.Carety-1];
      if ep > length(s) then //We are outside the real text, go back to the last char
        mp.Run(ms, nil, Synedit.SelStart - ep + length(s))
      else
        mp.Run(ms, nil, Synedit.SelStart + (ep - Synedit.CaretX) - 1);

      s := mp.GetExpressionAtPos;
      if (s <> '') then
      begin
        sp := LastDelimiter('.', s);
        if (sp > 0) then
          Delete(s, sp, Length(s) - sp + 1)
        else
          s := '';
      end;

      mp.FillSynCompletionProposal(ItemList, InsertList, s);
      p := SynEdit.ClientToScreen(SynEdit.RowColumnToPixels(Point(ep, SynEdit.CaretY)));
      p.y := p.y + SynEdit.LineHeight;
      Form1.CodeCompletionForm.Show(p, ItemList, InsertList, Filter, SynEdit);
    finally
      FreeAndNil(ms);
      FreeAndNil(mp);
      ItemList.Free;
      InsertList.Free;
    end;
  end;
  if command = ecCodeHints then
  begin
    if Form1.ParamHint.Visible = true then
      form1.ParamHint.hide;
    mp := TCodeInsight.Create;
    mp.OnMessage := @form1.OnCCMessage;
    mp.OnFindInclude := @form1.OnCCFindInclude;

    ms := TMemoryStream.Create;
    synedit.Lines.SaveToStream(ms);
    try
      Synedit.GetWordBoundsAtRowCol(Synedit.CaretXY, sp, ep);
      s := SynEdit.Lines[SynEdit.Carety-1];
      if ep > length(s) then //We are outside the real text, go back to the last char
        mp.Run(ms, nil, Synedit.SelStart - ep + length(s),true)
      else
        mp.Run(ms, nil, Synedit.SelStart + (ep - Synedit.CaretX) - 1,true);
      bcc := 1;bck := 0;cc := 0;
      s := mp.GetExpressionAtPos(bcc, bck, cc,posi, true);

      bracketpos := posi + length(s);
      if pos('(',s) > 0 then
      begin;
        bracketpos := pos('(',s) + posi;
        delete(s,pos('(',s),length(s) - pos('(',s) + 1);
      end;
      d := mp.FindVarBase(s);
      dd := nil;
      //Find the declaration -> For example if one uses var x : TNotifyEvent..
      //You have to get the owner of x, to find the declaration of TNotifyEvent etc..
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
      //Yeah, we should have found the procedureDeclaration now!
      if (d <> nil) and (d <> dd) and (d.Owner <> nil) and ((d is TciProcedureDeclaration) or (d.Owner is TciProcedureDeclaration)) then
      begin
        if (not (d is TciProcedureDeclaration)) and (d.Owner is TciProcedureDeclaration) then
          d := d.Owner;
        if (TciProcedureDeclaration(d).Params <> '') then
          Form1.ParamHint.Show(PosToCaretXY(synedit,posi + 1), PosToCaretXY(synedit,bracketpos),
                              TciProcedureDeclaration(d), synedit,mp)
        else
          FormWriteln('<no parameters expected>');
      end;
    except
      on e : exception do
        mDebugLn(e.message);
      //Do not free the MP, we need to use this.
    end;
  end;
  if Form1.CodeCompletionForm.Visible then
    case Command of
      ecDeleteChar, ecDeleteWord, ecDeleteEOL:
        begin
          if (SynEdit.CaretY = Form1.CodeCompletionStart.y) then
          begin
            //e.GetWordBoundsAtRowCol(acp_start, sp, ep);
            s := WordAtCaret(SynEdit, sp, ep, Form1.CodeCompletionStart.x);
            if (SynEdit.CaretX >= Form1.CodeCompletionStart.x) and (SynEdit.CaretX <= ep) then
            begin
              Form1.CodeCompletionForm.ListBox.Filter := s;
              Exit;
            end;
          end;
          Form1.CodeCompletionForm.Hide;
        end;
    end;
end;

procedure TScriptFrame.SynEditSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if line = ScriptErrorLine then
  begin;
    Special := true;
    BG := $50a0ff;
    FG := 0;
  end;
end;

procedure TScriptFrame.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  sp, ep: Integer;
  s: string;
begin
  {$IFDEF UpdateEditButtons}
  if scSelection in changes then
  begin;
    Form1.TT_Cut.Enabled := SynEdit.SelAvail;
    form1.TT_Copy.Enabled:= Form1.TT_Cut.Enabled;
    form1.TT_Paste.Enabled:= SynEdit.CanPaste;
  end;
  {$ENDIF}

  if Form1.CodeCompletionForm.Visible then
    if (scAll in Changes) or (scTopLine in Changes) then
      Form1.CodeCompletionForm.Visible := False
    else if (scCaretX in Changes) or (scCaretY in Changes) or (scSelection in Changes) or (scModified in Changes) then
    begin
      if (SynEdit.CaretY = Form1.CodeCompletionStart.y) then
      begin
        s := WordAtCaret(SynEdit, sp, ep, Form1.CodeCompletionStart.x);
        if (SynEdit.CaretX >= Form1.CodeCompletionStart.x) and (SynEdit.CaretX - 1 <= ep) then
        begin
          Form1.CodeCompletionForm.ListBox.Filter := s;
          Exit;
        end;
      end;

      Form1.CodeCompletionForm.Hide;
    end;
end;

procedure TScriptFrame.undo;
begin
  SynEdit.Undo;
  if ScriptChanged then
    if SynEdit.Lines.Text = StartText then
    begin;
      Form1.Caption:= format(WindowTitle,[ScriptName]);
      OwnerSheet.Caption:= ScriptName;
      ScriptChanged := false;
    end;
end;

procedure TScriptFrame.redo;
begin
  SynEdit.Redo;
  if ScriptChanged then
    if SynEdit.Lines.Text = StartText then
    begin;
      Form1.Caption:= format(WindowTitle,[ScriptName]);
      OwnerSheet.Caption := ScriptName;
      ScriptChanged := false;
    end;
end;

procedure TScriptFrame.HandleErrorData;
var
  i : integer;
begin
  if ErrorData.Module <> '' then
  begin;
    if not FileExists(ErrorData.Module) then
      formWriteln(Format('ERROR comes from a non-existing file (%s)',[ErrorData.Module]))
    else
    begin
      ErrorData.Module:= SetDirSeparators(ErrorData.Module);// Set it right ;-)
      Form1.LoadScriptFile(ErrorData.Module,true,true);//Checks if the file is already open!
      ErrorData.Module:= '';
      Form1.CurrScript.ErrorData := Self.ErrorData;
      Form1.CurrScript.HandleErrorData;
      exit;
    end;
  end;
  MakeActiveScriptFrame;
  ScriptErrorLine:= ErrorData.Row;
  SynEdit.Invalidate;
  if ErrorData.Col = -1 then
    SynEdit.SelStart:= ErrorData.Position
  else
    SynEdit.LogicalCaretXY := Point(ErrorData.Col,ErrorData.Row);
  if pos('error',lowercase(ErrorData.Error)) > 0 then
    formWriteln(Format('%s at line %d',[ErrorData.Error,ErrorData.Row]))
  else
    formWriteln(Format('Error: %s at line %d',[ErrorData.Error,ErrorData.Row]));
end;

procedure TScriptFrame.MakeActiveScriptFrame;
var
  i : integer;
begin
  if Form1.Visible then
  for i := 0 to OwnerPage.PageCount - 1 do
    if OwnerPage.Pages[i] = OwnerSheet then
    begin;
      OwnerPage.TabIndex := i;
      if OwnerSheet.CanFocus then
        OwnerSheet.SetFocus;
      exit;
    end;
end;

procedure TScriptFrame.ScriptThreadTerminate(Sender: TObject);
begin
  FScriptState:= ss_None;
  Form1.RefreshTab;
end;
procedure AddKey(const SynEdit : TSynEdit; const ACmd: TSynEditorCommand; const AKey: word;const AShift: TShiftState);
begin
  with SynEdit.KeyStrokes.Add do
  begin
    Key := AKey;
    Shift := AShift;
    Command := ACmd;
  end;
end;
constructor TScriptFrame.Create(TheOwner: TComponent);
var
  MarkCaret : TSynEditMarkupHighlightAllCaret;
begin
  inherited Create(TheOwner);
  OwnerSheet := TTabSheet(TheOwner);
  OwnerPage := TPageControl(OwnerSheet.Owner);
  StartText:= SynEdit.Lines.text;
  ScriptDefault:= StartText;
  ScriptName:= 'Untitled';
  ScriptChanged := false;
  FScriptState:= ss_None;
  ScriptErrorLine:= -1;
  OwnerSheet.Caption:= ScriptName;
  SynEdit.Highlighter := Form1.CurrHighlighter;
  SynEdit.Options:= SynEdit.Options + [eoTabIndent] - [eoSmartTabs];
  SynEdit.IncrementColor.Background := $30D070;
  SynEdit.HighlightAllColor.Background:= clYellow;
  SynEdit.HighlightAllColor.Foreground:= clDefault;
  SynEdit.TabWidth := 2;
  SynEdit.BlockIndent := 2;
  MarkCaret := TSynEditMarkupHighlightAllCaret(SynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  if assigned(MarkCaret) then
  begin
    with MarkCaret.MarkupInfo do
    begin;
      Background :=$E6E6E6;
      FrameColor := clGray;
    end;
    MarkCaret.Enabled := True;
    MarkCaret.FullWord:= True;
    MarkCaret.FullWordMaxLen:= 3;
    MarkCaret.WaitTime := 1500;
    MarkCaret.IgnoreKeywords := true;
  end;
  AddKey(SynEdit,ecCodeCompletion,VK_SPACE,[ssCtrl]);
  AddKey(SynEdit,ecCodeHints,VK_SPACE,[ssCtrl,ssShift]);
//  TSynPasSyn(SynEdit.Highlighter).NestedComments:= false; Does not work :(
end;

destructor TScriptFrame.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$R *.lfm}

end.

