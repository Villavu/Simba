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
  simba.ide_codetools_parser_new, simba.ide_codetools_insight_new, mPasLexTypes;

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Decl: TDeclaration;
begin
  SetupKeywordDictionary();

  with TNewCodeinsight.Create() do
  try
    SetScript(SynEdit1.Text, '');
    Run();

    Decl := ParseExpression(Edit1.Text);
    //if (Decl = nil) then
    //  Memo1.Text := 'Base var type not found'
    //else
    //  Memo1.Text := 'Base var type: ' + Decl.Dump + ' :: ' + Decl.Text;
  finally
    Free();
  end;
end;

end.

