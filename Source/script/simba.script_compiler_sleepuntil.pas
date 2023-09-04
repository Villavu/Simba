{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler_sleepuntil;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lpcompiler;

procedure InitializeSleepUntil(Compiler: TLapeCompiler);

implementation

uses
  lptypes, lptree, lpvartypes, lpmessages, lpinternalmethods;

type
  TLapeTree_SleepUntil_Operator = class(TLapeTree_Operator) // Dont take ownership of FLeft
  protected
    procedure setLeft(Node: TLapeTree_ExprBase); override;
  end;

  TLapeTree_InternalMethod_SleepUntil = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

procedure TLapeTree_SleepUntil_Operator.setLeft(Node: TLapeTree_ExprBase);
begin
  FLeft := Node;
end;

function TLapeTree_InternalMethod_SleepUntil.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltEvalBool);

  Result := inherited;
end;

function TLapeTree_InternalMethod_SleepUntil.Compile(var Offset: Integer): TResVar;
var
  Loop: TLapeTree_While;
  Assignment: TLapeTree_Operator;
  Condition: TLapeTree_Operator;
  Interval, Timeout, Limit: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 3) then
    LapeException('Expected three parameters (Condition, Interval, Timeout)', DocPos);
  if isEmpty(FParams[0]) or (FParams[0].resType() = nil) or (not (FParams[0].resType().BaseType in LapeBoolTypes)) then
    LapeException('Condition parameter is invalid: Boolean expression expected.', DocPos);
  if isEmpty(FParams[1]) or (FParams[1].resType() = nil) or (not (FParams[1].resType().BaseType in LapeIntegerTypes)) then
    LapeException('Interval parameter is invalid: Integer expected.', DocPos);
  if isEmpty(FParams[2]) or (FParams[2].resType() = nil) or (not (FParams[2].resType().BaseType in LapeIntegerTypes)) then
    LapeException('Timeout parameter is invalid: Integer expected.', DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(resType()));

  FCompiler.VarToDefault(Result, Offset, @Self._DocPos);

  Interval := FParams[1].Compile(Offset);
  Timeout := FParams[2].Compile(Offset);

  Condition := TLapeTree_SleepUntil_Operator.Create(op_cmp_Equal, Self);
  Condition.Left := FParams[0];
  Condition.Right := TLapeTree_GlobalVar.Create('True', ltEvalBool, Self);

  // Limit := GetTickCount();
  with TLapeTree_Invoke.Create('GetTickCount', Self) do
  try
    Limit := Compile(Offset);
    Limit.Writeable := True;
  finally
    Free();
  end;

  // Limit := Limit + Timeout;
  with TLapeTree_Operator.Create(op_AssignPlus, Self) do
  try
    Left := TLapeTree_ResVar.Create(Limit.IncLock(), Self);
    Right := TLapeTree_ResVar.Create(Timeout.IncLock(), Self);
    Limit := Compile(Offset);
  finally
    Free();
  end;

  Loop := TLapeTree_While.Create(Self);

  try
    // while GetTickCount() < Limit do
    Loop.Condition := TLapeTree_Operator.Create(op_cmp_LessThan, Self);
    with TLapeTree_Operator(Loop.Condition) do
    begin
      Left := TLapeTree_Invoke.Create('GetTickCount', Self);
      Right := TLapeTree_ResVar.Create(Limit.IncLock(), Self);
    end;

    Loop.Body := TLapeTree_If.Create(Self);

    TLapeTree_If(Loop.Body).Condition := Condition;
    TLapeTree_If(Loop.Body).Body := TLapeTree_StatementList.Create(Self);

    // If Condition then
    // Result := True
    // Break;
    with TLapeTree_If(Loop.Body) do
    begin
      Assignment := TLapeTree_Operator.Create(op_Assign, Self);
      Assignment.Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Assignment.Right := TLapeTree_GlobalVar.Create('True', ltEvalBool, Self);

      TLapeTree_StatementList(Body).addStatement(Assignment);
      TLapeTree_StatementList(Body).addStatement(TLapeTree_InternalMethod_Break.Create(Body));
    end;

    // Else Sleep(Interval)
    TLapeTree_If(Loop.Body).ElseBody := TLapeTree_Invoke.Create('Sleep', Self);
    with TLapeTree_If(Loop.Body) do
      TLapeTree_Invoke(ElseBody).addParam(TLapeTree_ResVar.Create(Interval.IncLock(), Self));

    Loop.Compile(Offset).Spill(1);
  finally
    Loop.Free();
  end;

  Limit.Spill(2);
  Interval.Spill(1);
  Timeout.Spill(1);
end;

procedure InitializeSleepUntil(Compiler: TLapeCompiler);
begin
  Compiler.InternalMethodMap['SleepUntil'] := TLapeTree_InternalMethod_SleepUntil;
end;

end.

