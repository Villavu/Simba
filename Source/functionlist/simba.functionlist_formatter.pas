unit simba.functionlist_formatter;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.ide_codetools_parser;

type
  TFunctionListFormatter = class(TObject)
  protected
    FImageFormatters: array[0..3] of record
      ClassType: TDeclarationClass;
      Func: function(Decl: TDeclaration): Integer;
    end;

    FHintFormatters: array[0..3] of record
      ClassType: TDeclarationClass;
      Func: function(Decl: TDeclaration): String;
    end;

    FNameFormatters: array[0..3] of record
      ClassType: TDeclarationClass;
      Func: function(Decl: TDeclaration): String;
    end;
  public
    constructor Create;

    function GetImage(Decl: TDeclaration): Integer;
    function GetHint(Decl: TDeclaration): String;
    function GetName(Decl: TDeclaration): String;
  end;

var
  FunctionListFormatter: TFunctionListFormatter = nil;

implementation

uses
  simba.main, simba.ide_initialization;

function GetImage_Method(Decl: TDeclaration): Integer;
begin
  if Decl.isFunction then
    Result := IMAGE_FUNCTION
  else
  if Decl.isProcedure then
    Result := IMAGE_PROCEDURE
  else
  if Decl.isOperatorMethod then
    Result := IMAGE_OPERATOR;
end;

function GetImage_Type(Decl: TDeclaration): Integer;
begin
  Result := IMAGE_TYPE;
end;

function GetImage_Const(Decl: TDeclaration): Integer;
begin
  Result := IMAGE_CONSTANT;
end;

function GetImage_Var(Decl: TDeclaration): Integer;
begin
  Result := IMAGE_VARIABLE;
end;

function GetHint_Method(Decl: TDeclaration): String;
begin
  Result := TDeclaration_Method(Decl).HeaderString;
end;

function GetHint_Type(Decl: TDeclaration): String;
begin
  Result := 'type ' + Decl.Name + ' = ' + Decl.TextNoCommentsSingleLine;
end;

function GetHint_Const(Decl: TDeclaration): String;
begin
  Result := 'const ' + Decl.Name + TDeclaration_Var(Decl).VarTypeString + TDeclaration_Var(Decl).VarDefaultString;
end;

function GetHint_Var(Decl: TDeclaration): String;
begin
  Result := 'var ' + Decl.Name + TDeclaration_Var(Decl).VarTypeString + TDeclaration_Var(Decl).VarDefaultString;
end;

function GetName_Method(Decl: TDeclaration): String;
begin
  if Decl.isObjectMethod then
    Result := TDeclaration_Method(Decl).ObjectName + '.' + TDeclaration_Method(Decl).Name
  else
    Result := Decl.Name;
end;

function GetName_Type(Decl: TDeclaration): String;
begin
  Result := Decl.Name;
end;

function GetName_Const(Decl: TDeclaration): String;
begin
  Result := Decl.Name;
end;

function GetName_Var(Decl: TDeclaration): String;
begin
  Result := Decl.Name;
end;

function TFunctionListFormatter.GetImage(Decl: TDeclaration): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FImageFormatters) do
    if (Decl is FImageFormatters[I].ClassType) then
    begin
      Result := FImageFormatters[I].Func(Decl);
      Exit;
    end;

  Result := -1;
end;

function TFunctionListFormatter.GetHint(Decl: TDeclaration): String;
var
  I: Integer;
begin
  for I := 0 to High(FHintFormatters) do
    if (Decl is FHintFormatters[I].ClassType) then
    begin
      Result := FHintFormatters[I].Func(Decl);
      Exit;
    end;

  Result := '';
end;

function TFunctionListFormatter.GetName(Decl: TDeclaration): String;
var
  I: Integer;
begin
  for I := 0 to High(FNameFormatters) do
    if (Decl is FNameFormatters[I].ClassType) then
    begin
      Result := FNameFormatters[I].Func(Decl);
      Exit;
    end;

  Result := '';
end;

constructor TFunctionListFormatter.Create;
begin
  inherited Create();

  // image
  FImageFormatters[0].ClassType := TDeclaration_Method;
  FImageFormatters[0].Func := @GetImage_Method;

  FImageFormatters[1].ClassType := TDeclaration_Type;
  FImageFormatters[1].Func := @GetImage_Type;

  FImageFormatters[2].ClassType := TDeclaration_Const;
  FImageFormatters[2].Func := @GetImage_Const;

  FImageFormatters[3].ClassType := TDeclaration_Var;
  FImageFormatters[3].Func := @GetImage_Var;

  // hint
  FHintFormatters[0].ClassType := TDeclaration_Method;
  FHintFormatters[0].Func := @GetHint_Method;

  FHintFormatters[1].ClassType := TDeclaration_Type;
  FHintFormatters[1].Func := @GetHint_Type;

  FHintFormatters[2].ClassType := TDeclaration_Const;
  FHintFormatters[2].Func := @GetHint_Const;

  FHintFormatters[3].ClassType := TDeclaration_Var;
  FHintFormatters[3].Func := @GetHint_Var;

  // name
  FNameFormatters[0].ClassType := TDeclaration_Method;
  FNameFormatters[0].Func := @GetName_Method;

  FNameFormatters[1].ClassType := TDeclaration_Type;
  FNameFormatters[1].Func := @GetName_Type;

  FNameFormatters[2].ClassType := TDeclaration_Const;
  FNameFormatters[2].Func := @GetName_Const;

  FNameFormatters[3].ClassType := TDeclaration_Var;
  FNameFormatters[3].Func := @GetName_Var;
end;

procedure CreateFunctionListFormatter;
begin
  FunctionListFormatter := TFunctionListFormatter.Create();
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnCreate(@CreateFunctionListFormatter, 'FunctionListFormatter');

finalization
  if (FunctionListFormatter <> nil) then
    FreeAndNil(FunctionListFormatter);

end.

