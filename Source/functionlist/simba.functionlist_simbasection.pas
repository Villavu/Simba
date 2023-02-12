unit simba.functionlist_simbasection;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ComCtrls,
  simba.mufasatypes, simba.ide_codetools_parser, simba.functionlistform;

const
  SimbaSectionDocLinks: TStringArray = (
    'TPoint        -> https://villavu.github.io/Simba/TPoint.html',
    'TPointArray   -> https://villavu.github.io/Simba/TPointArray.html',
    'TBox          -> https://villavu.github.io/Simba/TBox.html',
    'TBoxArray     -> https://villavu.github.io/Simba/TBoxArray.html',
    'TQuad         -> https://villavu.github.io/Simba/TQuad.html',
    'Random        -> https://villavu.github.io/Simba/Random.html',
    'TWindowHandle -> https://villavu.github.io/Simba/Window Handle.html',
    'T2DPointArray -> https://villavu.github.io/Simba/T2DPointArray.html',
    'Debug Image   -> https://villavu.github.io/Simba/Debug Image.html',
    'Script        -> https://villavu.github.io/Simba/Script.html',
    'Variant       -> https://villavu.github.io/Simba/Variant.html',
    'TWindowHandle -> https://villavu.github.io/Simba/TWindowHandle.html',
    'TMufasaBitmap -> https://villavu.github.io/Simba/TMufasaBitmap.html'
  );

function GetSectionDocLink(Section: String): String;

type
  TSimbaFunctionList_SimbaSection = class(TSimbaFunctionListStaticSection)
  protected
    FSections: array of TCodeParser;

    function Sort(A, B: TTreeNode): Integer;
  public
    function Added(FunctionList: TSimbaFunctionList): Boolean; override;
    procedure Load(Includes: TCodeParserList);
    procedure Add(FunctionList: TSimbaFunctionList); override;
  end;

var
  SimbaFunctionList_SimbaSection: TSimbaFunctionList_SimbaSection;

implementation

uses
  strutils,
  simba.main, simba.functionlist_nodes;

function GetSectionDocLink(Section: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to High(SimbaSectionDocLinks) do
    if (SimbaSectionDocLinks[I].Before('->').Trim() = Section) then
    begin
      Result := SimbaSectionDocLinks[I].After('->').Trim();
      Exit;
    end;
end;

function TSimbaFunctionList_SimbaSection.Sort(A, B: TTreeNode): Integer;
begin
  Result := NaturalCompareText(A.Text, B.Text);

  case A.ImageIndex of
    IMAGE_TYPE:      Dec(Result, 2000);
    IMAGE_CONSTANT:  Dec(Result, 1500);
    IMAGE_VARIABLE:  Dec(Result, 1000);
    IMAGE_PROCEDURE: Dec(Result, 500);
    IMAGE_FUNCTION:  Dec(Result, 500);
  end;

  case B.ImageIndex of
    IMAGE_TYPE:      Inc(Result, 2000);
    IMAGE_CONSTANT:  Inc(Result, 1500);
    IMAGE_VARIABLE:  Inc(Result, 1000);
    IMAGE_PROCEDURE: Inc(Result, 500);
    IMAGE_FUNCTION:  Inc(Result, 500);
  end;
end;

function TSimbaFunctionList_SimbaSection.Added(FunctionList: TSimbaFunctionList): Boolean;
begin
  Result := FunctionList.TreeView.Items.FindTopLvlNode('Simba') <> nil;
end;

procedure TSimbaFunctionList_SimbaSection.Load(Includes: TCodeParserList);
var
  Parser: TCodeParser;
begin
  for Parser in Includes do
  begin
    if (Parser.Lexer = nil) or (Parser.Lexer.FileName.StartsWith('!')) then
      Continue;

    FSections := FSections + [Parser];
  end;

  Loaded := True;
end;

procedure TSimbaFunctionList_SimbaSection.Add(FunctionList: TSimbaFunctionList);
var
  Section: TCodeParser;
  SimbaNode, SectionNode: TTreeNode;
  FileName, URL: String;
begin
  SimbaNode := FunctionList.TreeView.Items.Add(nil, 'Simba');
  SimbaNode.ImageIndex    := IMAGE_DIRECTORY;
  SimbaNode.SelectedIndex := IMAGE_DIRECTORY;

  for Section in FSections do
  begin
    if (Section.Items.Count = 0) then
      Continue;

    FileName := Section.Lexer.FileName;
    URL := GetSectionDocLink(FileName);
    if (URL <> '') then
      SectionNode := FunctionList.AddNode(SimbaNode, TFunctionList_URLNode.Create(FunctionList, URL))
    else
      SectionNode := FunctionList.AddNode(SimbaNode, TFunctionList_InternalFileNode.Create(FunctionList, FileName));

    FunctionList.AddInternalDecls(SectionNode, Section.Items);

    SectionNode.CustomSort(@Sort);
  end;

  SimbaNode.AlphaSort();
  SimbaNode.Expanded := True;
end;

initialization
  SimbaFunctionList_SimbaSection := TSimbaFunctionList_SimbaSection.Create();

finalization
  if (SimbaFunctionList_SimbaSection <> nil) then
    FreeAndNil(SimbaFunctionList_SimbaSection);

end.

