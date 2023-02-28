unit codeinsighttool.main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit, SynHighlighterPas;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    SynEdit1: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  simba.ide_codetools_parser, simba.ide_codetools_insight, mPasLexTypes;

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Decl: TDeclaration;
begin
  SetupKeywordDictionary();

  with TCodeinsight.Create() do
  try
    SetScript(SynEdit1.Text, '', SynEdit1.SelStart, SynEdit1.SelStart);
    Run();

    Decl := ParseExpression(Edit1.Text, [EParseExpressionFlag.WantMethodResult]);
    if (Decl = nil) then
      Memo1.Text := 'Failed'
    else
      Memo1.Text := 'Found: ' + Decl.Dump() + ' "' + Decl.Text + '"';

    Memo1.Lines.Add('');
    Memo1.Lines.Add('Locals: %d', [ScriptParser.Locals.Count]);
    for Decl in ScriptParser.Locals.ToArray() do
      Memo1.Lines.Add(Decl.ClassName + ' "' + Decl.Name + '"');
  finally
    Free();
  end;
end;

end.

