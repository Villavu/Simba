{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor_codecomplete;

{$i simba.inc}

interface

uses
  Classes, SysUtils, LCLType,
  SynEdit, SynEditTypes, SynEditKeyCmds, SynEditHighlighterFoldBase;

type
  TSimbaCodeComplete = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoEditorRemoving(Value: TCustomSynEdit); override;
    procedure DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
  end;

implementation

uses
  simba.ide_editor, simba.ide_codetools_insight, simba.ide_codetools_parser, simba.vartype_string,
  simba.ide_editor_commands;

procedure TSimbaCodeComplete.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  Value.RegisterCommandHandler(@DoEditorCommand, nil, [hcfPostExec]);
end;

procedure TSimbaCodeComplete.DoEditorRemoving(Value: TCustomSynEdit);
begin
  Value.UnregisterCommandHandler(@DoEditorCommand);

  inherited DoEditorRemoving(Value);
end;

procedure TSimbaCodeComplete.DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
var
  ci: TCodeinsight;
  Decls: TDeclarationArray;
  Decl: TDeclaration;
  MethodToInsert: String;
  StartX, EndX, NameX: Integer;
begin
  if (Command = ecCodeComplete) then
  begin
    ci := TCodeinsight.Create();
    try
      ci.SetScript(Editor.Text, TSimbaEditor(Editor).FileName);
      ci.Run();

      Decls := ci.Get(Editor.GetWordAtRowCol(Editor.CaretXY));
      if (Length(Decls) > 0) then
        Decl := Decls[0].Items.GetByClassFirst(TDeclaration_Method, False, True)
      else
        Decl := nil;

      if (Decl <> nil) then
      begin
        MethodToInsert :=
          Decl.Header + ';' + LineEnding +
          'begin'           + LineEnding +
          ''                + LineEnding +
          'end;'            + LineEnding;

        // insert placeholder name
        NameX := MethodToInsert.IndexOf(' ') + 1;
        MethodToInsert.Insert('MyEvent', NameX);

        Editor.GetWordBoundsAtRowCol(Editor.CaretXY, StartX, EndX);
        Editor.TextBetweenPoints[Point(StartX, Editor.CaretY), Point(EndX, Editor.CaretY)] := MethodToInsert;

        Editor.CaretX := NameX;
        Editor.BlockBegin := Point(NameX, Editor.CaretY);
        Editor.BlockEnd := Point(NameX + Length('MyEvent'), Editor.CaretY);
      end;
    finally
      ci.Free();
    end;
  end;
end;

end.

