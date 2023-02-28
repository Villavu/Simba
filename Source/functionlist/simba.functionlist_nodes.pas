unit simba.functionlist_nodes;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ComCtrls,
  simba.ide_codetools_parser, simba.functionlistform;

type
  TFunctionListNode = class(TTreeNode)
  public
    Hint: String;

    procedure Open; virtual;
  end;

  TFunctionList_URLNode = class(TFunctionListNode)
  public
    URL: String;

    constructor Create(FunctionList: TSimbaFunctionList; AURL: String); reintroduce;
    procedure Open; override;
  end;

  TFunctionList_DeclNode = class(TFunctionListNode)
  protected
    FLine: Integer;
    FStartPos: Integer;
    FEndPos: Integer;
    FFileName: String;
  public
    constructor Create(FunctionList: TSimbaFunctionList; ADecl: TDeclaration); reintroduce;
    procedure Open; override;
  end;

  TFunctionList_InternalDeclNode = class(TFunctionList_DeclNode)
  public
    procedure Open; override;
  end;

  TFunctionList_FileNode = class(TFunctionListNode)
  public
    constructor Create(FunctionList: TSimbaFunctionList; AFileName: String); reintroduce;
    procedure Open; override;
  end;

  TFunctionList_InternalFileNode = class(TFunctionListNode)
  public
    constructor Create(FunctionList: TSimbaFunctionList; AFileName: String); reintroduce;
  end;

implementation

uses
  lazfileutils, lclintf,
  simba.mufasatypes, simba.main, simba.scripttabsform, simba.functionlist_formatter,
  simba.ide_showdeclaration;

procedure TFunctionList_InternalDeclNode.Open;
begin
  ShowInternalDeclaration(Hint, FFileName);
end;

constructor TFunctionList_InternalFileNode.Create(FunctionList: TSimbaFunctionList; AFileName: String);
begin
  inherited Create(FunctionList.TreeView.Items);

  Text := ExtractFileNameOnly(AFileName);

  ImageIndex    := IMAGE_FILE;
  SelectedIndex := IMAGE_FILE;
end;

procedure TFunctionListNode.Open;
begin
  { nothing }
end;

procedure TFunctionList_FileNode.Open;
begin
  SimbaScriptTabsForm.Open(Hint);
end;

constructor TFunctionList_FileNode.Create(FunctionList: TSimbaFunctionList; AFileName: String);
begin
  inherited Create(FunctionList.TreeView.Items);

  Text := ExtractFileNameOnly(AFileName);
  Hint := AFileName;

  ImageIndex    := IMAGE_FILE;
  SelectedIndex := IMAGE_FILE;
end;

procedure TFunctionList_URLNode.Open;
begin
  OpenURL(URL);
end;

constructor TFunctionList_URLNode.Create(FunctionList: TSimbaFunctionList; AURL: String);
begin
  inherited Create(FunctionList.TreeView.Items);

  URL  := AURL;
  Text := ExtractFileNameOnly(AURL);
  Hint := Text + ' (double click to open online documentation)';

  ImageIndex    := IMAGE_FILE;
  SelectedIndex := IMAGE_FILE;
end;

procedure TFunctionList_DeclNode.Open;
begin
  ShowDeclaration(FStartPos, FEndPos, FLine, FFileName);
end;

constructor TFunctionList_DeclNode.Create(FunctionList: TSimbaFunctionList; ADecl: TDeclaration);
begin
  inherited Create(FunctionList.TreeView.Items);

  FFileName := ADecl.Lexer.FileName;
  FStartPos := ADecl.StartPos;
  FEndPos   := ADecl.EndPos;
  FLine     := ADecl.Line;

  Text := FunctionListFormatter.GetName(ADecl);
  Hint := FunctionListFormatter.GetHint(ADecl);

  ImageIndex    := FunctionListFormatter.GetImage(ADecl);
  SelectedIndex := ImageIndex;
end;

end.

