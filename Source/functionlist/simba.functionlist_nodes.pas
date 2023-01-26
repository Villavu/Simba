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
  simba.mufasatypes, simba.main, simba.scripttabsform;

procedure TFunctionList_InternalDeclNode.Open;
begin
  SimbaScriptTabsForm.OpenInternalDeclaration(Hint, FFileName);
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
  SimbaScriptTabsForm.OpenDeclaration(FStartPos, FEndPos, FLine, FFileName);
end;

constructor TFunctionList_DeclNode.Create(FunctionList: TSimbaFunctionList; ADecl: TDeclaration);
begin
  inherited Create(FunctionList.TreeView.Items);

  FFileName := ADecl.Lexer.FileName;
  FStartPos := ADecl.StartPos;
  FEndPos   := ADecl.EndPos;
  FLine     := ADecl.Line;

  Text := ADecl.Name;

  case ADecl.ClassName of
    'TciProcedureDeclaration':
      begin
        if TciProcedureDeclaration(ADecl).IsMethodOfType then
          Text := TciProcedureDeclaration(ADecl).ObjectName + '.' + Text;
        Hint := TciProcedureDeclaration(ADecl).Header;

        if TciProcedureDeclaration(ADecl).IsOperator then
        begin
          ImageIndex    := IMAGE_OPERATOR;
          SelectedIndex := IMAGE_OPERATOR;
        end else
        begin
          case TciProcedureDeclaration(ADecl).IsFunction of
            True:
              begin
                ImageIndex    := IMAGE_FUNCTION;
                SelectedIndex := IMAGE_FUNCTION;
              end;

            False:
              begin
                ImageIndex    := IMAGE_PROCEDURE;
                SelectedIndex := IMAGE_PROCEDURE;
              end;
          end;
        end;
      end;

    'TciTypeDeclaration':
      begin
        Hint := ADecl.ShortText;

        ImageIndex    := IMAGE_TYPE;
        SelectedIndex := IMAGE_TYPE;
      end;

    'TciConstantDeclaration':
      begin
        Hint := ADecl.ShortText;

        ImageIndex    := IMAGE_CONSTANT;
        SelectedIndex := IMAGE_CONSTANT;
      end;

    'TciVarDeclaration':
      begin
        Hint := ADecl.ShortText;

        ImageIndex    := IMAGE_VARIABLE;
        SelectedIndex := IMAGE_VARIABLE;
      end;
  end;
end;

end.

