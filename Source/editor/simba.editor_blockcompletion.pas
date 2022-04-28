{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_blockcompletion;

{$i simba.inc}

interface

uses
  Classes, SysUtils, lcltype,
  SynEdit, SynEditTypes, SynEditKeyCmds;

type
  TSimbaEditorPlugin_BlockCompletion = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoBeforeCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);
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
        begin
          if IsHighlighterAttribute(['String', 'Comment']) or IsTextAhead(['BEGIN']) then
            Exit;

          Token := GetWordAtRowCol(CaretXY);

          if (CompareText(Token, 'BEGIN') = 0) and SimbaSettings.Editor.AutomaticallyCompleteBegin.Value then
          begin
            Caret := CaretXY;
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

end.

