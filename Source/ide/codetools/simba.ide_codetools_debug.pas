{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_debug;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

procedure DebugSymbolTable(Script: String; ScriptName: String);

implementation

uses
  Controls, ComCtrls, Forms,
  simba.component_treeview,
  simba.ide_codetools_insight,
  simba.ide_codetools_parser;

procedure DebugSymbolTable(Script: String; ScriptName: String);
var
  Form: TForm;
  TreeView: TSimbaTreeView;
  Codeinsight: TCodeinsight;
  I,J: Integer;
  n: TTreeNode;
begin
  Form := TForm.Create(nil);
  Form.Caption := 'Symbol Table';
  Form.Position := poScreenCenter;
  Form.Width := 800;
  Form.Height := 800;

  TreeView := TSimbaTreeView.Create(Form);
  TreeView.Parent := Form;
  TreeView.Align := alClient;

  Codeinsight := TCodeinsight.Create();
  Codeinsight.SetScript(Script, ScriptName);
  Codeinsight.Run();

  for I := 0 to Codeinsight.SymbolTable.Count - 1 do
    with Codeinsight.SymbolTable.Items[I] do
    begin
      n := TreeView.AddNode(Decls[0].Name);
      for J := 0 to Count - 1 do
        TreeView.AddNode(n, Decls[J].Dump, DeclarationImage(Decls[J]));
    end;

  Codeinsight.Free();

  Form.ShowModal();
  Form.Free();
end;

end.

