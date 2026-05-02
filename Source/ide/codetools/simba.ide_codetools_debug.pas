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
procedure DebugCache;

implementation

uses
  Controls, ComCtrls, Forms, fgl,
  simba.component_treeview,
  simba.component_images,
  simba.ide_codetools_insight,
  simba.ide_codetools_parser,
  simba.ide_codetools_includes,
  simba.fs;

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
  //TreeView.Images := SimbaMainForm.Images;

  Codeinsight := TCodeinsight.Create();
  Codeinsight.SetScript(Script, ScriptName);
  Codeinsight.Run();

  for I := 0 to Codeinsight.SymbolTable.Count - 1 do
    with Codeinsight.SymbolTable.Items[I] do
    begin
      n := TreeView.AddNode(Decls[0].Name);
      //for J := 0 to Count - 1 do
      //  TreeView.AddNode(n, Decls[J].Dump, DeclarationImage(Decls[J]));
    end;

  Codeinsight.Free();

  Form.ShowModal();
  Form.Free();
end;

procedure DebugCache;

  procedure DebugClassTypes(Parser: TCodeParser; TreeView: TSimbaTreeView; n: TTreeNode);
  var
    List: specialize TFPGMap<Pointer, Int64>;
    i, j: Integer;
    idx: Integer;
  begin
    List := specialize TFPGMap<Pointer, Int64>.Create();
    for i := 0 to parser.Garbage.Count-1 do
    begin
      idx := List.IndexOf(Parser.Garbage[i].ClassType);
      if (idx > -1) then
        List.Data[idx] := List.Data[idx] + 1
      else
        List.Add(Parser.Garbage[i].ClassType, 1);
    end;

    for i := 0 to List.Count-1 do
      for j := 0 to List.Count-1 do
        if (List.Data[i] > List.Data[j]) then
          List.Exchange(i, j);

    n := TreeView.AddNode(n, 'Class counts', SimbaImages.TYPE_DECL);
    for i := 0 to List.Count - 1 do
      TreeView.AddNode(n, TDeclarationClass(List.Keys[i]).ClassName + ': ' + IntToStr(List.Data[i]));

    List.Free();
  end;

var
  Parser: TCodeParser;
  Form: TForm;
  TreeView: TSimbaTreeView;
  n: TTreeNode;
begin
  Form := TForm.Create(nil);
  Form.Caption := 'Cache';
  Form.Position := poScreenCenter;
  Form.Width := 800;
  Form.Height := 800;

  TreeView := TSimbaTreeView.Create(Form);
  TreeView.Parent := Form;
  TreeView.Align := alClient;
  TreeView.Images := SimbaImages;

  CodetoolsIncludes.DebugEnter();
  for Parser in CodetoolsIncludes.Debug.ToArray do
  begin
    n := TreeView.AddNode(TSimbaPath.PathExtractName(Parser.Lexer.FileName), SimbaImages.DOCUMENT);

    TreeView.AddNode(n, 'Size: ' + FormatFloat('0.00', Parser.SizeInBytes / (1024*1024)) + ' MB', SimbaImages.INFO);
    TreeView.AddNode(n, 'Declaration Count: ' + IntToStr(Parser.Garbage.Count), SimbaImages.INFO);
    TreeView.AddNode(n, 'Symbol Count: ' + IntToStr(Parser.SymbolTable.Count), SimbaImages.INFO);
    TreeView.AddNode(n, 'Max Stack Depth: ' + IntToStr(Parser.Stack.Max), SimbaImages.INFO);
    TreeView.AddNode(n, 'Ref Count: ' + IntToStr(TCodetoolsInclude(Parser).RefCount), SimbaImages.INFO);
    TreeView.AddNode(n, 'Last Used: ' + IntToStr(TCodetoolsInclude(Parser).LastUsed), SimbaImages.INFO);
    TreeView.AddNode(n, 'InDefines: ' + TCodetoolsInclude(Parser).InDefines.ToString);

    DebugClassTypes(Parser, TreeView, n);
  end;
  CodetoolsIncludes.DebugLeave();

  Form.ShowModal();
  Form.Free();
end;

end.

