{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_blockcompletion;

{$i simba.inc}

interface

uses
  Classes, SysUtils, LCLType,
  SynEdit, SynEditTypes, SynEditKeyCmds, SynEditHighlighterFoldBase, SynHighlighterPas_Simba;

type
  TSimbaEditorPlugin_BlockCompletion = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoBeforeCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);

    function BeginHasEnd(LogCaret: TPoint): Boolean;
  end;

implementation

uses
  simba.editor, simba.settings;

procedure TSimbaEditorPlugin_BlockCompletion.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  Value.RegisterCommandHandler(@DoBeforeCommand, Pointer(nil), [hcfPreExec]);
end;

procedure TSimbaEditorPlugin_BlockCompletion.DoBeforeCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);
var
  Token: String;
  Caret: TPoint;
  InsertText: String;
begin
  with Editor as TSimbaEditor do
  begin
    if ReadOnly or SelAvail then
      Exit;

    InsertText := '';

    case Command of
      ecLineBreak:
        if SimbaSettings.Editor.AutomaticallyCompleteBegin.Value then
        begin
          if IsHighlighterAttribute(['String', 'Comment']) or IsTextAhead(['BEGIN']) then
            Exit;

          Token := GetWordAtRowCol(CaretXY);

          if (CompareText(Token, 'BEGIN') = 0) and (not Self.BeginHasEnd(LogicalCaretXY)) then
          begin
            Caret := CaretXY;
            Caret.X -= 3;
            Caret.Y += 1;

            InsertText := LineEnding +
                          StringOfChar(' ', CaretX - 4) + LineEnding +
                          StringOfChar(' ', CaretX - 6) + 'end;';

            Handled := True;
          end;
        end;

      ecChar:
        begin
          if IsHighlighterAttribute(['String', 'Comment']) then
            Exit;

          Token := UTF8Encode(AChar);
          Caret := CaretXY;

          if (CompareText(Token, '(') = 0) and (not IsTextAhead(['(', ')'])) and SimbaSettings.Editor.AutomaticallyCompleteParentheses.Value then
            InsertText := ')'
          else
          if (CompareText(Token, '[') = 0) and (not IsTextAhead(['[', ']'])) and SimbaSettings.Editor.AutomaticallyCompleteIndex.Value then
            InsertText := ']';
        end;
    end;

    if (InsertText <> '') then
    begin
      BeginUndoBlock();

      try
        InsertTextAtCaret(InsertText, scamIgnore);

        CaretXY := Caret;
      finally
        EndUndoBlock();
      end;
    end;
  end;
end;

// Yoinked and modified from SynEditMarkupWordGroup
function TSimbaEditorPlugin_BlockCompletion.BeginHasEnd(LogCaret: TPoint): Boolean;
var
  NodeList: TLazSynFoldNodeInfoList;

  function FindEndNode(StartNode: TSynFoldNodeInfo; var YIndex, NIndex: Integer): TSynFoldNodeInfo;

    function SearchLine(ALineIdx: Integer; var ANodeIdx: Integer): TSynFoldNodeInfo;
    begin
      NodeList.Line := ALineIdx;
      repeat
        Inc(ANodeIdx);
        Result := NodeList[ANodeIdx];
      until (sfaInvalid in Result.FoldAction) or (Result.NestLvlEnd <= StartNode.NestLvlStart);
    end;

  begin
    Result := SearchLine(YIndex, NIndex);
    if not (sfaInvalid in Result.FoldAction) then
      Exit;

    inc(YIndex);
    while (YIndex < Editor.Lines.Count) and
          (TSynFreePascalSyn(Editor.Highlighter).FoldBlockMinLevel(YIndex, StartNode.FoldGroup, [sfbIncludeDisabled])
           > StartNode.NestLvlStart)
    do
      Inc(YIndex);
    if YIndex = Editor.Lines.Count then
      Exit;

    NIndex := -1;
    Result := SearchLine(YIndex, NIndex);

    if (Result.LogXEnd = 0) or (sfaLastLineClose in Result.FoldAction) then
      Result.FoldAction := [sfaInvalid]; // LastLine closed Node(maybe force-closed?)
  end;

var
  I, Y: Integer;
  StartNode, CloseNode, TmpNode: TSynFoldNodeInfo;
begin
  Result := False;
  if (not Assigned(Editor.Lines)) or (LogCaret.Y < 1) or (LogCaret.Y > Editor.Lines.Count) or (LogCaret.X < 1) then
    Exit;
  if not (Editor.Highlighter is TSynCustomFoldHighlighter) then
    Exit;

  I := 0;
  Y := LogCaret.Y-1;
  TSynFreePascalSyn(Editor.Highlighter).FoldNodeInfo[Y].ClearFilter; // only needed once, in case the line was already used

  NodeList := TSynFreePascalSyn(Editor.Highlighter).FoldNodeInfo[Y];
  NodeList.AddReference;
  NodeList.ActionFilter := [sfaMarkup];

  TmpNode := NodeList[i];
  while not (sfaInvalid in TmpNode.FoldAction) and (TmpNode.LogXEnd < LogCaret.X-1) do
  begin
    Inc(I);
    TmpNode := NodeList[I];
  end;
  if (TmpNode.LogXStart > LogCaret.X - 1) or (sfaInvalid in TmpNode.FoldAction) then
    Exit;

  if TmpNode.FoldAction * [sfaOpenFold, sfaOneLineOpen] <> [] then
  begin
    StartNode := TmpNode;
    CloseNode := FindEndNode(StartNode, Y, i);
    if (sfaInvalid in CloseNode.FoldAction) then
      Exit;

    {
    Writeln(StartNode.NestLvlStart);
    Writeln(StartNode.NestLvlEnd);
    Writeln(CloseNode.NestLvlStart);
    Writeln(CloseNode.NestLvlEnd);
    }

    Result := CloseNode.LogXStart = StartNode.LogXStart;
  end;
end;

end.

