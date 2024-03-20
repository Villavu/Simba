{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_commentblock;

{$i simba.inc}

interface

uses
  Classes, SysUtils, lcltype,
  SynEdit, SynEditTypes, SynEditKeyCmds;

type
  TSimbaEditorPlugin_CommentBlock = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);

    procedure CommentBlock;
  public
    class var EditorCommand: TSynEditorCommand;
    class constructor Create;
  end;

implementation

uses
  math,
  simba.editor;

procedure TSimbaEditorPlugin_CommentBlock.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  with Value.Keystrokes.Add() do
  begin
    Key := VK_LCL_SLASH;
    Shift := [ssCtrl];
    Command := EditorCommand;
  end;

  Value.RegisterCommandHandler(@DoCommand, Pointer(nil), [hcfPostExec]);
end;

procedure TSimbaEditorPlugin_CommentBlock.DoCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);
begin
  if (Command = EditorCommand) then
  begin
    CommentBlock();

    Handled := True;
  end;
end;

// Copied from Lazarus source
procedure TSimbaEditorPlugin_CommentBlock.CommentBlock;
var
  OldCaretPos, OldBlockStart, OldBlockEnd: TPoint;
  WasSelAvail: Boolean;
  WasSelMode: TSynSelectionMode;
  BlockBeginLine: Integer;
  BlockEndLine: Integer;
  CommonIndent: Integer;

  function FirstNonBlankPos(const Text: String; Start: Integer = 1): Integer;
  var
    i: Integer;
  begin
    for i := Start to Length(Text) do
      if (Text[i] <> #32) and (Text[i] <> #9) then
        Exit(i);
    Result := -1;
  end;

  function MinCommonIndent: Integer;
  var
    i, j: Integer;
  begin
    If CommonIndent = 0 then begin
      CommonIndent := Max(FirstNonBlankPos(Editor.Lines[BlockBeginLine - 1]), 1);
      for i := BlockBeginLine + 1 to BlockEndLine do begin
        j := FirstNonBlankPos(Editor.Lines[i - 1]);
        if (j < CommonIndent) and (j > 0) then
          CommonIndent := j;
      end;
    end;
    Result := CommonIndent;
  end;

  function InsertPos(ALine: Integer): Integer;
  begin
    if not WasSelAvail then
      Result := MinCommonIndent
    else case WasSelMode of
      smColumn: // CommonIndent is not used otherwise
        begin
          if CommonIndent = 0 then
            CommonIndent := Min(Editor.LogicalToPhysicalPos(OldBlockStart).X, Editor.LogicalToPhysicalPos(OldBlockEnd).X);
          Result := Editor.PhysicalToLogicalPos(Point(CommonIndent, ALine)).X;
        end;
      smNormal:
        begin
          Result := MinCommonIndent;
        end;
       else
         Result := 1;
    end;
  end;

  function DeletePos(ALine: Integer): Integer;
  var
    Line: String;
  begin
    Line := Editor.Lines[ALine - 1];
    Result := FirstNonBlankPos(Line, InsertPos(ALine));
    if (WasSelMode = smColumn) and((Result < 1) or (Result > length(Line) - 1)) then
      Result := length(Line) - 1;
    Result := Max(1, Result);
    if (Length(Line) < Result +1) or
       (Line[Result] <> '/') or (Line[Result+1] <> '/') then
      Result := -1;
  end;

var
  i: Integer;
  NonBlankStart: Integer;
  CommentOn: Boolean;
begin
  with Editor as TSimbaEditor do
  begin
    if ReadOnly then
      Exit;

    OldCaretPos   := CaretXY;
    OldBlockStart := BlockBegin;
    OldBlockEnd   := BlockEnd;
    WasSelAvail := SelAvail;
    WasSelMode  := SelectionMode;
    CommonIndent := 0;

    BlockBeginLine := OldBlockStart.Y;
    BlockEndLine := OldBlockEnd.Y;
    if (OldBlockEnd.X = 1) and (BlockEndLine > BlockBeginLine) and (SelectionMode <> smLine) then
      Dec(BlockEndLine);

    CommentOn := False;
    for i := BlockBeginLine to BlockEndLine do
      if DeletePos(i) < 0 then
      begin
        CommentOn := True;
        Break;
      end;

    BeginUpdate();
    BeginUndoBlock();

    SelectionMode := smNormal;

    if CommentOn then
    begin
      for i := BlockEndLine downto BlockBeginLine do
        TextBetweenPoints[Point(InsertPos(i), i), Point(InsertPos(i), i)] := '//';
      if OldCaretPos.X > InsertPos(OldCaretPos.Y) then
        OldCaretPos.x := OldCaretPos.X + 2;
      if OldBlockStart.X > InsertPos(OldBlockStart.Y) then
        OldBlockStart.X := OldBlockStart.X + 2;
      if OldBlockEnd.X > InsertPos(OldBlockEnd.Y) then
        OldBlockEnd.X := OldBlockEnd.X + 2;
    end else
    begin
      for i := BlockEndLine downto BlockBeginLine do
      begin
        NonBlankStart := DeletePos(i);
        if NonBlankStart < 1 then Continue;
        TextBetweenPoints[Point(NonBlankStart, i), Point(NonBlankStart + 2, i)] := '';
        if (OldCaretPos.Y = i) and (OldCaretPos.X > NonBlankStart) then
          OldCaretPos.x := Max(OldCaretPos.X - 2, NonBlankStart);
        if (OldBlockStart.Y = i) and (OldBlockStart.X > NonBlankStart) then
          OldBlockStart.X := Max(OldBlockStart.X - 2, NonBlankStart);
        if (OldBlockEnd.Y = i) and (OldBlockEnd.X > NonBlankStart) then
          OldBlockEnd.X := Max(OldBlockEnd.X - 2, NonBlankStart);
      end;
    end;

    EndUndoBlock();
    EndUpdate();

    CaretXY := OldCaretPos;
    BlockBegin := OldBlockStart;
    BlockEnd := OldBlockEnd;
    SelectionMode := WasSelMode;
  end;
end;

class constructor TSimbaEditorPlugin_CommentBlock.Create;
begin
  EditorCommand := AllocatePluginKeyRange(1);
end;

end.

