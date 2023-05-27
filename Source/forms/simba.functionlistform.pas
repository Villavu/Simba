{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlistform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls,
  simba.ide_codetools_parser, simba.ide_codetools_insight, simba.component_treeview;

type
  TSimbaFunctionListForm = class(TForm)
  protected
    FTreeView: TSimbaTreeView;
    FScriptNode: TTreeNode;
    FSimbaNode: TTreeNode;

    procedure DoCodetoolsSetup(Sender: TObject);
    function DoGetNodeHint(Node: TTreeNode): String;

    function AddDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
    // Sort so order is: Types, Constants, Variables, Methods
    function CompareDecl(A, B: TTreeNode): Integer;
  public
    procedure AddSimbaNodes;

    constructor Create(TheOwner: TComponent); override;
  end;

var
  SimbaFunctionListForm: TSimbaFunctionListForm;

implementation

{$R *.lfm}

uses
  StrUtils,
  simba.main, simba.ide_mainstatusbar, simba.ide_events, simba.threading;

function GetImage(const Decl: TDeclaration): Integer;
begin
  if (Decl is TDeclaration_Method) and Decl.isFunction       then Result := IMAGE_FUNCTION  else
  if (Decl is TDeclaration_Method) and Decl.isProcedure      then Result := IMAGE_PROCEDURE else
  if (Decl is TDeclaration_Method) and Decl.isOperatorMethod then Result := IMAGE_OPERATOR  else
  if (Decl is TDeclaration_Type)                             then Result := IMAGE_TYPE      else
  if (Decl is TDeclaration_Const)                            then Result := IMAGE_CONSTANT  else
  if (Decl is TDeclaration_Var)                              then Result := IMAGE_VARIABLE  else Result := -1;
end;

function GetText(const Decl: TDeclaration): String;
begin
  if (Decl is TDeclaration_Method) and Decl.isObjectMethod then
    Result := TDeclaration_Method(Decl).ObjectName + '.' + TDeclaration_Method(Decl).Name
  else
    Result := Decl.Name;
end;

function GetHint(const Decl: TDeclaration): String;
begin
  if (Decl is TDeclaration_Method) then Result := TDeclaration_Method(Decl).HeaderString                                                                else
  if (Decl is TDeclaration_Type)   then Result := 'type '  + Decl.Name + ' = ' + Decl.TextNoCommentsSingleLine                                          else
  if (Decl is TDeclaration_Const)  then Result := 'const ' + Decl.Name + TDeclaration_Var(Decl).VarTypeString + TDeclaration_Var(Decl).VarDefaultString else
  if (Decl is TDeclaration_Var)    then Result := 'var '   + Decl.Name + TDeclaration_Var(Decl).VarTypeString + TDeclaration_Var(Decl).VarDefaultString else Result := '';
end;

function GetURL(const Section: String): String;
begin
  Result := '';

  case Section of
    'TPoint':        Result := 'https://villavu.github.io/Simba/TPoint.html';
    'TPointArray':   Result := 'https://villavu.github.io/Simba/TPointArray.html';
    'TBox':          Result := 'https://villavu.github.io/Simba/TBox.html';
    'TBoxArray':     Result := 'https://villavu.github.io/Simba/TBoxArray.html';
    'TQuad':         Result := 'https://villavu.github.io/Simba/TQuad.html';
    'Random':        Result := 'https://villavu.github.io/Simba/Random.html';
    'T2DPointArray': Result := 'https://villavu.github.io/Simba/T2DPointArray.html';
    'Debug Image':   Result := 'https://villavu.github.io/Simba/Debug Image.html';
    'Script':        Result := 'https://villavu.github.io/Simba/Script.html';
    'Variant':       Result := 'https://villavu.github.io/Simba/Variant.html';
    'TWindowHandle': Result := 'https://villavu.github.io/Simba/TWindowHandle.html';
    'TMufasaBitmap': Result := 'https://villavu.github.io/Simba/TMufasaBitmap.html';
    'Finder':        Result := 'https://villavu.github.io/Simba/Finder.html';
    'Input':         Result := 'https://villavu.github.io/Simba/Input.html';
    'Target':        Result := 'https://villavu.github.io/Simba/Target.html';
    'Internet':      Result := 'https://villavu.github.io/Simba/Internet.html';
  end;
end;

type
  TSimbaFunctionListNode = class(TTreeNode)
  public
    IsInternal: Boolean;
    Hint: String;
    Line: Integer;
    StartPos: Integer;
    EndPos: Integer;
    FileName: String;
  end;

procedure TSimbaFunctionListForm.DoCodetoolsSetup(Sender: TObject);
begin
  TThread.ExecuteInThread(@AddSimbaNodes);
end;

function TSimbaFunctionListForm.DoGetNodeHint(Node: TTreeNode): String;
begin
  if (Node is TSimbaFunctionListNode) then
    Result := TSimbaFunctionListNode(Node).Hint
  else
    Result := '';
end;

function TSimbaFunctionListForm.AddDecl(ParentNode: TTreeNode; Decl: TDeclaration): TTreeNode;
begin
  Result := FTreeView.AddNode(ParentNode, Decl.Name);
  with TSimbaFunctionListNode(Result) do
  begin
    FileName := Decl.Lexer.FileName;
    StartPos := Decl.StartPos;
    EndPos   := Decl.EndPos;
    Line     := Decl.Line;

    Text := GetText(Decl);
    Hint := GetHint(Decl);
    ImageIndex := GetImage(Decl);
    SelectedIndex := GetImage(Decl);
  end;
end;

function TSimbaFunctionListForm.CompareDecl(A, B: TTreeNode): Integer;
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

procedure TSimbaFunctionListForm.AddSimbaNodes;

  procedure BeginUpdate;
  begin
    FTreeView.BeginUpdate();
  end;

  procedure EndUpdate;
  begin
    FSimbaNode.AlphaSort();
    FSimbaNode.Expanded := True; // This needs to be on main thread it seems?

    FTreeView.EndUpdate();
  end;

var
  I, J: Integer;
  Parser: TCodeParser;
  ParentNode: TTreeNode;
begin
  ExecuteOnMainThread(@BeginUpdate);

  for I := 0 to TCodeinsight.BaseIncludes.Count - 1 do
  begin
    Parser := TCodeinsight.BaseIncludes[I];
    if (Parser = nil) or (Parser.Items.Count = 0) or (Parser.FileName.StartsWith('!')) then
      Continue;

    ParentNode := FTreeView.AddNode(FSimbaNode, Parser.FileName, IMAGE_FILE);
    with TSimbaFunctionListNode(ParentNode) do
    begin
      FileName := GetURL(Parser.FileName);
      if (FileName <> '') then
        Hint := Text + ' (double click to open online documentation)';
    end;

    for J := 0 to Parser.Items.Count - 1 do
      if (Parser.Items[J].Name <> '') and (not Parser.Items[J].isOverrideMethod) then
        AddDecl(ParentNode, Parser.Items[J]);

    ParentNode.CustomSort(@CompareDecl);
  end;

  ExecuteOnMainThread(@EndUpdate);
end;

constructor TSimbaFunctionListForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FTreeView := TSimbaTreeView.Create(Self, TSimbaFunctionListNode);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.Images := SimbaForm.Images;
  //FTreeView.OnDoubleClick := @DoTreeViewDoubleClick;
  //FTreeView.OnSelectionChange := @DoTreeViewSelectionChanged;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;

  FScriptNode  := FTreeView.AddNode('Script', IMAGE_DIRECTORY);
  FSimbaNode   := FTreeView.AddNode('Simba',  IMAGE_DIRECTORY);

  SimbaIDEEvents.RegisterOnCodetoolsSetup(@DoCodetoolsSetup);
end;

end.

