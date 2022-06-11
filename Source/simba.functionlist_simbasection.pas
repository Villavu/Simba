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
  simba.main, simba.mufasatypes;

function TSimbaFunctionList_SimbaSection.Sort(A, B: TTreeNode): Integer;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  Result := 0;

  Weights := [IMAGE_TYPE, IMAGE_CONSTANT, IMAGE_VARIABLE, IMAGE_PROCEDURE, IMAGE_FUNCTION];
  for I := 0 to High(Weights) do
  begin
    if (Weights[I] = A.ImageIndex) then
      Inc(Result, I+1);
    if (Weights[I] = B.ImageIndex) then
      Dec(Result, I+1);
  end;

  if (A.ImageIndex = B.ImageIndex) then
    Inc(Result, CompareText(A.Text, B.Text));
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
begin
  SimbaNode := FunctionList.TreeView.Items.Add(nil, 'Simba');
  SimbaNode.ImageIndex := IMAGE_DIRECTORY;
  SimbaNode.SelectedIndex := IMAGE_DIRECTORY;

  FunctionList.BeginAddingInternal();

  for Section in FSections do
  begin
    SectionNode := FunctionList.AddFile(SimbaNode, Section.Lexer.FileName);
    FunctionList.AddDeclarations(
      Section.Items, SectionNode, False, False, False
    );

    SectionNode.CustomSort(@Sort);
  end;

  FunctionList.EndAdddingInternal();

  SimbaNode.AlphaSort();
  SimbaNode.Expanded := True;
end;

end.

