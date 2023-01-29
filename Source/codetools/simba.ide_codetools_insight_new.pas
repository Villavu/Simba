unit simba.ide_codetools_insight_new;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.ide_codetools_parser_new;

type
  TNewCodeinsight = class(TObject)
  protected
    FIncludes: array of TCodeParser;
    FScriptParser: TCodeParser;
  public
    procedure SetScript(Script: String; FileName: String);
    procedure Reset;
    procedure Run;

    // Return var type of a "starting" variable
    function FindBase(S: String): TDeclaration;

    // if `TDeclaration_VarType` resolve to type.
    // if `TDeclaration_Identifier` resolve to type "TPoint" > "record X,Y: Integer; end"
    function EnsureTypeDeclaration(Decl: TDeclaration): TDeclaration;

    function ParseExpression(S: String): TDeclaration;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.mufasatypes;

procedure TNewCodeinsight.SetScript(Script: String; FileName: String);
begin
  FScriptParser.SetScript(Script, FileName);
end;

procedure TNewCodeinsight.Reset;
begin
  FScriptParser.Reset();
  // FScriptParser.Lexer.Defines.AddStrings();
end;

procedure TNewCodeinsight.Run;
begin
  //Reset();

  FScriptParser.Run();
end;

function TNewCodeinsight.FindBase(S: String): TDeclaration;
begin
  Result := FScriptParser.Globals.Get(S);

  if (Result is TDeclaration_Method) then Result := EnsureTypeDeclaration(TDeclaration_Method(Result).ResultType) else
  if (Result is TDeclaration_Var)    then Result := EnsureTypeDeclaration(TDeclaration_Var(Result).VarType)       else Result := nil;
end;

function TNewCodeinsight.EnsureTypeDeclaration(Decl: TDeclaration): TDeclaration;
begin
  Result := Decl;
  if (Result is TDeclaration_VarType) then
  begin
    if (Result.Items.Count > 0) then
      Result := Result.Items.First()
    else
      Result := nil;
  end;

  if (Result is TDeclaration_Identifier) then
    Result := FScriptParser.Globals.Get(Result.Text);
end;

function TNewCodeinsight.ParseExpression(S: String): TDeclaration;

  function GetMembersByName(Decl: TDeclaration; Name: String): TDeclarationArray;

    procedure CheckRecord(Decl: TDeclaration);
    begin
      Result := Result + Decl.Items.GetByClassAndName(TDeclaration_Field, Name)
                       + Decl.Items.GetByClassAndName(TDeclaration_Var, Name); // class fields
    end;

    procedure CheckMethods(Decl: TDeclaration);
    var
      Decls: TDeclarationArray;
      I: Integer;
    begin
      if (Decl.Name <> '') then
      begin
        Decls := FScriptParser.GlobalTypeMethods[Decl.Name];
        for I := 0 to High(Decls) do
          if TDeclaration_Method(Decls[I]).IsName(Name) then
            Result := Result + [Decls[I]];
      end;
    end;

  begin
    Result := nil;

    if (Decl is TDeclaration_TypeRecord) then
      CheckRecord(Decl);
    CheckMethods(Decl);
  end;

  function Next(Decl: TDeclaration; Name: String): TDeclaration;
  var
    I: Integer;
    Members: TDeclarationArray;
  begin
    Result := nil;

    Members := GetMembersByName(Decl, Name);
    for I := 0 to High(Members) do
    begin
      if (Members[I] is TDeclaration_Var) and (TDeclaration_Var(Members[I]).VarType <> nil) then
      begin
        Result := EnsureTypeDeclaration(TDeclaration_Var(Members[I]).VarType);
        Break;
      end;

      if (Members[I] is TDeclaration_Method) and (TDeclaration_Method(Members[I]).ResultType <> nil) then
      begin
        Result := EnsureTypeDeclaration(TDeclaration_Method(Members[I]).ResultType);
        Break;
      end;
    end;
  end;

var
  Base: TDeclaration;
  Expr: TStringArray;
  I: Integer;
begin
  Result := nil;

  Expr := S.Split('.');
  Base := FindBase(Expr[0]);
  if (Base = nil) then
  begin
    Writeln('no base');
    Exit;


  // VarType
  // P.Something
  end;

  Writeln('Base is: ', Base.ClassName);

  for I := 1 to High(Expr) do
  begin
    Writeln('Looking for: ', Expr[I], ' in ', Base.Text);
    Base := Next(Base, Expr[I]);
    if (Base = nil) then
    begin
      Writeln('Not found');
      Base := nil;
      Break;
    end;
  end;

  Result := Base;
  if (Result <> nil) then
  begin
    Writeln(S,'=',Base.Text, '(' + Base.ClassName + ')');
  end;
end;

constructor TNewCodeinsight.Create;
begin
  inherited Create();

  FScriptParser := TCodeParser.Create();
end;

destructor TNewCodeinsight.Destroy;
begin
  if (FScriptParser <> nil) then
    FreeAndNil(FScriptParser);

  inherited Destroy();
end;

end.

