{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_finder;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.ide_codetools_parser, simba.ide_codetools_insight;

// Get declaration at codeinsight.caretpos
function FindDeclaration(Codeinsight: TCodeInsight; Expr: String): TDeclarationArray;

implementation

function FindDeclaration(Codeinsight: TCodeInsight; Expr: String): TDeclarationArray;
var
  Insight: TCodeinsight;
  Decl: TDeclaration;
begin
  Insight := TCodeinsight.Create();
  Insight.SetScript('{$I SRL/osr.simba} procedure abs(wtf, olly: Integer); begin end; begin end;', 'Script', -1, -1);
  Insight.Run();



  Decl := Insight.ParseExpression('TSRL.Setup', False);

  Result := Codeinsight.GetOverloads(Codeinsight.ParseExpression(Expr, False));
end;

end.

