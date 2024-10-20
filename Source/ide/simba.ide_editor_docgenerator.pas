{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor_docgenerator;

{$i simba.inc}

interface

uses
  Classes, SysUtils, LCLType,
  SynEdit, SynEditTypes, SynEditKeyCmds;

const
  DEFAULT_DOCUMENTATION_COMMENT =
    '(*'   + LineEnding +
    '%s'   + LineEnding +
    '%s'   + LineEnding +
    '```'  + LineEnding +
    '%s'   + LineEnding +
    '```'  + LineEnding +
    '*)'   + LineEnding;

type
  TSimbaEditorPlugin_DocGenerator = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);

    procedure InsertDocumentation;
  public
    class var EditorCommand: TSynEditorCommand;
    class constructor Create;
  end;

implementation

uses
  simba.ide_codetools_base, simba.ide_codetools_parser, simba.settings,
  simba.dialog;

procedure TSimbaEditorPlugin_DocGenerator.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  with Value.Keystrokes.Add() do
  begin
    Key := VK_D;
    Shift := [ssCtrl];
    Command := EditorCommand;
  end;

  Value.RegisterCommandHandler(@DoCommand, Pointer(nil), [hcfPostExec]);
end;

procedure TSimbaEditorPlugin_DocGenerator.DoCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: Pointer; HandlerData: Pointer);
begin
  if (Command = EditorCommand) then
  begin
    InsertDocumentation();

    Handled := True;
  end;
end;

procedure TSimbaEditorPlugin_DocGenerator.InsertDocumentation;

  procedure InsertDocAtMethod(Decl: TDeclaration);
  begin
    if (Decl is TDeclaration_Method) then
      with TDeclaration_Method(Decl) do
      begin
        Editor.CaretXY := Editor.CharIndexToRowCol(StartPos - 1);
        Editor.InsertTextAtCaret(
          Format(SimbaSettings.Editor.DocumentationComment.Value, [Decl.FullName, StringOfChar('-', Length(Decl.FullName)), Header])
        );
      end;
  end;

var
  Parser: TCodeParser;
  Decl: TDeclaration;
begin
  if Editor.ReadOnly then
    Exit;

  Parser := TCodeParser.Create();
  try
    Parser.SetScript(Editor.Text);
    Parser.Run();

    Decl := Parser.Items.GetByPosition(Editor.SelStart - 1);
    if (Decl <> nil) then
    begin
      if (Decl is TDeclaration_Method) then
        InsertDocAtMethod(Decl)
      else
        InsertDocAtMethod(Decl.ParentByClass[TDeclaration_Method]);
    end;
  except
    on E: Exception do
      SimbaErrorDlg('Simba', 'Insert documentation error: %s', [E.Message]);
  end;
  if (Parser <> nil) then
    Parser.Free();
end;

class constructor TSimbaEditorPlugin_DocGenerator.Create;
begin
  EditorCommand := AllocatePluginKeyRange(1);
end;

end.

