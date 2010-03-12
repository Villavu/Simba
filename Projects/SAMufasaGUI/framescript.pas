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
   mmlpsthread,ComCtrls, SynEditKeyCmds, LCLType,MufasaBase, SynEditMarkupSpecialLine, Graphics, Controls;
const
   ecCodeCompletion = ecUserFirst;
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
    SynFreePascalSyn1: TSynFreePascalSyn;
    procedure SynEditChange(Sender: TObject);
    procedure SynEditDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynEditDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
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

implementation
uses
  TestUnit, SynEditTypes, LCLIntF, StrUtils,framefunctionlist;

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
var
  LineText,SearchText : string;
  Caret : TPoint;
  i,endI : integer;
begin
  if Command = ecCodeCompletion then
  begin
    form1.FunctionListShown(True);
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
begin
  {$IFDEF UpdateEditButtons}
  if scSelection in changes then
  begin;
    Form1.TT_Cut.Enabled := SynEdit.SelAvail;
    form1.TT_Copy.Enabled:= Form1.TT_Cut.Enabled;
    form1.TT_Paste.Enabled:= SynEdit.CanPaste;
  end;
  {$ENDIF}
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
      for i := 0 to Form1.Tabs.Count - 1 do
        if lowercase(TMufasaTab(Form1.Tabs[i]).ScriptFrame.ScriptFile) = lowercase(ErrorData.Module) then
        begin;
          ErrorData.Module:= '';
          TMufasaTab(Form1.Tabs[i]).ScriptFrame.ErrorData := Self.ErrorData;
          TMufasaTab(Form1.Tabs[i]).ScriptFrame.HandleErrorData;
          Exit;
        end;
      Form1.LoadScriptFile(ErrorData.Module,true);
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
  SynEdit.Options:= SynEdit.Options- [eoGroupUndo];
  SynEdit.Options:= SynEdit.Options+ [eoGroupUndo,eoPersistentCaret];
  SynEdit.IncrementColor.Background := $30D070;
  SynEdit.HighlightAllColor.Background:= clYellow;
  SynEdit.HighlightAllColor.Foreground:= clDefault;
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
//  TSynPasSyn(SynEdit.Highlighter).NestedComments:= false; Does not work :(
end;

destructor TScriptFrame.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I framescript.lrs}

end.

