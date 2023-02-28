unit codeparsertool.main;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, SynEdit, SynHighlighterPas;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Timer1: TTimer;

    procedure Timer1Timer(Sender: TObject);
  public
    ChangeStamp: Int64;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  simba.mufasatypes, simba.ide_codetools_parser;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if (ChangeStamp = SynEdit1.ChangeStamp) then
    Exit;

  with TCodeParser.Create() do
  try
    SetScript(SynEdit1.Text);
    Run();

    Memo1.Text := DebugTree();
  finally
    Free();
  end;

  ChangeStamp := SynEdit1.ChangeStamp;
end;

end.

