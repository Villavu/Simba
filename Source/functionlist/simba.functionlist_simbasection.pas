unit simba.functionlist_simbasection;

{$i simba.inc}

interface

uses
  classes, sysutils, comctrls,
  simba.codeparser, simba.ci_includecache, simba.functionlistform;

type
  TSimbaFunctionList_SimbaSection = class(TSimbaFunctionListStaticSection)
  protected
    FSections: array of TCodeParser;

    function Sort(A, B: TTreeNode): Integer;
  public
    constructor Create(Includes: TCodeInsight_IncludeArray);

    procedure Add(FunctionList: TSimbaFunctionList); override;
  end;

implementation

uses
  simba.main, simba.functionlist_nodes;

function TSimbaFunctionList_SimbaSection.Sort(A, B: TTreeNode): Integer;
begin
  Result := CompareStr(A.Text, B.Text);

  case A.ImageIndex of
    IMAGE_TYPE:     Dec(Result, 2000);
    IMAGE_CONSTANT: Dec(Result, 1500);
    IMAGE_VARIABLE: Dec(Result, 1000);
  end;

  case B.ImageIndex of
    IMAGE_TYPE:     Inc(Result, 2000);
    IMAGE_CONSTANT: Inc(Result, 1500);
    IMAGE_VARIABLE: Inc(Result, 1000);
  end;
end;

constructor TSimbaFunctionList_SimbaSection.Create(Includes: TCodeInsight_IncludeArray);
var
  Include: TCodeInsight_Include;
begin
  inherited Create();

  for Include in Includes do
  begin
    if (Include.Lexer = nil) or (Include.Lexer.FileName.StartsWith('!')) then
      Continue;

    FSections := FSections + [Include];
  end;
end;

procedure TSimbaFunctionList_SimbaSection.Add(FunctionList: TSimbaFunctionList);
var
  Section: TCodeParser;
  SimbaNode, SectionNode: TTreeNode;
  FileName: String;
begin
  SimbaNode := FunctionList.TreeView.Items.Add(nil, 'Simba');
  SimbaNode.ImageIndex := IMAGE_DIRECTORY;
  SimbaNode.SelectedIndex := IMAGE_DIRECTORY;

  for Section in FSections do
  begin
    if (Section.Items.Count = 0) then
      Continue;

    FileName := Section.Lexer.FileName;
    if FileName.StartsWith('https://') or FileName.StartsWith('http://') then
      SectionNode := FunctionList.AddNode(SimbaNode, TFunctionList_URLNode.Create(FunctionList, FileName))
    else
      SectionNode := FunctionList.AddNode(SimbaNode, TFunctionList_InternalFileNode.Create(FunctionList, FileName));

    FunctionList.AddInternalDecls(SectionNode, Section.Items);

    SectionNode.CustomSort(@Sort);
  end;

  SimbaNode.AlphaSort();
  SimbaNode.Expanded := True;
end;

end.

