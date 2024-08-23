{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Base class for generating generic methods.
}
unit simba.script_genericbase;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lptree, lpcompiler, lpmessages;

type
  TLapeMethodBuilder = class
  protected type
    TLapeCompilerProtectedAccess = class(TLapeCompiler);
  protected
    FCompiler: TLapeCompilerBase;
    FParams: array of record
      Name: lpString;
      VarType: TLapeType;
      ParamType: ELapeParameterType;
    end;
    FLocalTypes: TLapeTypeArray;
    FResultType: TLapeType;
    FObjectType: TLapeType;
    FName: lpString;
    FBody: TStringArray;
    FProperty: Boolean;
    FOverload: Boolean;
  public
    constructor Create(AObjectType: TLapeType; ALocalTypes: TLapeTypeArray = nil); reintroduce;

    procedure addParam(ParamName: lpString; VarType: TLapeType; ParamType: ELapeParameterType = lptNormal);

    procedure Reset;
    procedure Build(DoReset: Boolean = True);

    property ResultType: TLapeType read FResultType write FResultType;
    property ObjectType: TLapeType read FObjectType write FObjectType;
    property Name: lpString read FName write FName;
    property Body: TStringArray read FBody write FBody;

    property isProperty: Boolean read FProperty write FProperty;
    property isOverload: Boolean read FOverload write FOverload;
  end;

  TGenericMethod = class(TLapeTree_InternalMethod)
  protected
    procedure addToStringOverride;

    function getParamType(Index: Integer): TLapeType;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;

    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
  end;

implementation

constructor TLapeMethodBuilder.Create(AObjectType: TLapeType; ALocalTypes: TLapeTypeArray);
begin
  inherited Create();

  FCompiler := AObjectType.Compiler;
  FObjectType := AObjectType;
  FLocalTypes := ALocalTypes;
end;

procedure TLapeMethodBuilder.addParam(ParamName: lpString; VarType: TLapeType; ParamType: ELapeParameterType);
begin
  SetLength(FParams, Length(FParams) + 1);
  FParams[High(FParams)].Name := ParamName;
  FParams[High(FParams)].VarType := VarType;
  FParams[High(FParams)].ParamType := ParamType;
end;

procedure TLapeMethodBuilder.Reset;
begin
  FName := '';
  FBody := nil;
  FParams := nil;
  FResultType := nil;
  FProperty := False;
  FOverload := False;
end;

procedure TLapeMethodBuilder.Build(DoReset: Boolean);

  function Add(FuncHeader: TLapeType_Method; FuncName: lpString; FuncBody: lpString): TLapeTree_Method;
  var
    OldState: Pointer;
    i: Integer;
  begin
    Result := nil;

    with TLapeCompilerProtectedAccess(FCompiler) do
    begin
      OldState := getTempTokenizerState(FuncBody, '!' + FObjectType.ClassName + '::' + FuncName);
      Options := [lcoShortCircuit, lcoAlwaysInitialize, lcoLooseSyntax, lcoRangeCheck];

      IncStackInfo();
      try
        if (FuncHeader is TLapeType_MethodOfType) then
          FStackInfo.addSelfVar(TLapeType_MethodOfType(FuncHeader).SelfParam, TLapeType_MethodOfType(FuncHeader).ObjectType);
        for i := 0 to FuncHeader.Params.Count - 1 do
          FStackInfo.addVar(FuncHeader.Params[i].ParType, FuncHeader.Params[i].VarType, FParams[i].Name);
        if (FuncHeader.Res <> nil) then
          FStackInfo.addVar(lptOut, FuncHeader.Res, 'Result');
        for i := 0 to High(FLocalTypes) do
          FStackInfo.addDeclaration(FLocalTypes[i]);

        Result := ParseMethod(nil, FuncHeader, FuncName, False);
        CheckAfterCompile();
        addDelayedExpression(Result, True, True);
      finally
        DecStackInfo(True, False, (Result = nil));
      end;

      resetTokenizerState(OldState);
    end;
  end;

var
  Header: TLapeType_MethodOfType;
  i: Integer;
  FuncBody: TStringList;
  Param: TLapeParameter;
begin
  Header := FCompiler.addManagedDecl(TLapeType_MethodOfType.Create(FCompiler, FObjectType, nil, FResultType, Name)) as TLapeType_MethodOfType;
  if FProperty then
    Header.MethodDef := mdProperty;

  for i := 0 to High(FParams) do
  begin
    Param := NullParameter;
    Param.VarType := FParams[i].VarType;
    Param.ParType := FParams[i].ParamType;

    Header.Params.Add(Param);
  end;

  FuncBody := TStringList.Create();
  if FOverload then
    FuncBody.Add('overload;');

  FuncBody.Add('begin');
  FuncBody.Add('  try');
  for i := 0 to High(FBody) do
    FuncBody.Add('    ' + FBody[i]);
  FuncBody.Add('  except');
  FuncBody.Add('    raise at GetCallerLocation();');
  FuncBody.Add('  end;');
  FuncBody.Add('end;');

  Add(Header, Name, FuncBody.Text);

  FuncBody.Free();

  if DoReset then
    Reset();
end;

procedure TGenericMethod.addToStringOverride;
var
  Header: TLapeType;
begin
  with FCompiler as TLapeCompiler do
  begin
    Header := addManagedType(
      TLapeType_Method.Create(
        FCompiler,
        [FResType],
        [lptConstRef],
        [TLapeGlobalVar(nil)],
        getBaseType(ltString))
      );

    TLapeType_OverloadedMethod(Globals['ToString'].VarType).addMethod(
      addGlobalFunc(TLapeType_Method(Header), '_ToString', 'begin Result := Param0.ToString(); end;').Method
    );
  end;
end;

function TGenericMethod.getParamType(Index: Integer): TLapeType;
var
  Typ: TLapeType;
begin
  if (Index < 0) or (Index >= FParams.Count) then
    LapeExceptionFmt(lpeInvalidIndex, [Index], DocPos);
  Typ := FParams[Index].resType();
  if (not (Typ is TLapeType_Type)) then
    LapeException(lpeImpossible, DocPos);

  Result := TLapeType_Type(Typ).TType;
end;

constructor TGenericMethod.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FConstant := bTrue;
end;

// "inherited FoldConstants()" but dont hide exception if raised
function TGenericMethod.FoldConstants(DoFree: Boolean): TLapeTree_Base;
var
  Replacement: TLapeTree_ExprBase;
begin
  Result := Self;

  if (not isEmpty(Self)) and isConstant() then
  begin
    Replacement := TLapeTree_VarType.Create(resType(), Self);
    Replacement.Parent := FParent;

    FParent := nil;
    if DoFree then
      Free();
    Result := Replacement;
  end;
end;

end.

