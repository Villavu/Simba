{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_docgenerator;

{$i simba.inc}

interface

uses
  Classes, SysUtils, lcltype,
  SynEdit, SynEditTypes, SynEditKeyCmds;

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
  simba.editor, simba.codeparser;

const
  DOCUMENTATION_COMMENT =
    '(*'             + LineEnding +
    '%s'             + LineEnding +
    '%s'             + LineEnding +
    '.. pascal:: %s' + LineEnding +
    ''               + LineEnding +
    'DESC'           + LineEnding +
    ''               + LineEnding +
    'Example'        + LineEnding +
    '-------'        + LineEnding +
    ''               + LineEnding +
    '  EXAMPLE'      + LineEnding +
    '*)'             + LineEnding;

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
var
  Parser: TCodeParser;
  Decl: TDeclaration;
  FullName: String;
begin
  with Editor as TSimbaEditor do
  begin
    if ReadOnly then
      Exit;

    Parser := TCodeParser.Create();

    try
      Parser.Run(Text, '');

      Decl := Parser.Items.GetItemInPosition(SelStart - 1);
      if (Decl <> nil) and Decl.HasOwnerClass(TciProcedureDeclaration, Decl, True) then
        with Decl as TciProcedureDeclaration do
        begin
          CaretXY := CharIndexToRowCol(StartPos);

          if IsMethodOfType then
            FullName := ObjectName + '.' + Name
          else
            FullName := Name;

          InsertTextAtCaret(
            Format(DOCUMENTATION_COMMENT, [FullName, StringOfChar('~', Length(FullName)), Header])
          );
        end;
    except
    end;

    if (Parser <> nil) then
      Parser.Free();
  end;
end;

class constructor TSimbaEditorPlugin_DocGenerator.Create;
begin
  EditorCommand := AllocatePluginKeyRange(1);
end;

end.

