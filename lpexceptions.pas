{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Lape exceptions.
}
unit lpexceptions;

{$I lape.inc}

interface

uses
  SysUtils,
  lptypes;

type
  lpException = class(Exception);

resourcestring
  lpeArrayLengthsDontMatch = 'Length of arrays (%s) don''t match';
  lpeBlockExpected = 'Block expected';
  lpeCannotAssign = 'Target cannot be assigned to';
  lpeCannotBreak = 'Cannot break out of this statement';
  lpeCannotContinue = 'Cannot use continue in this context';
  lpeCannotEvalConst = 'Cannot be evaluated as constant';
  lpeCannotEvalConstProc = 'Procedures cannot be used for constant evaluation';
  lpeCannotEvalRunTime = 'Cannot be evaluated at runtime';
  lpeCannotInvoke = 'Cannot invoke identifier';
  lpeCannotOverload = 'Cannot overload function';
  lpeClosingParenthesisExpected = 'Closing parenthesis expected';
  lpeConditionalNotClosed = 'Conditional statement not properly closed';
  lpeConstantExpected = 'Constant expression expected';
  lpeDefaultToMoreThanOne = 'Runtime default value can only be assigned to one variable';
  lpeDuplicateDeclaration = 'Duplicate declaration "%s"';
  lpeExceptionAt = '%s at line %d, column %d';
  lpeExceptionIn = '%s in file "%s"';
  lpeExpected = '%s expected';
  lpeExpectedOther = 'Found unexpected token "%s", expected "%s"';
  lpeExpressionExpected = 'Expression expected';
  lpeFileNotFound = 'File "%s" not found';
  lpeImpossible = 'It''s impossible!';
  lpeIncompatibleAssignment = 'Can''t assign "%s" to "%s"';
  lpeIncompatibleOperator = 'Operator "%s" not compatible with types';
  lpeIncompatibleOperator1 = 'Operator "%s" not compatible with "%s"';
  lpeIncompatibleOperator2 = 'Operator "%s" not compatible with types "%s" and "%s"';
  lpeInvalidAssignment = 'Invalid assignment';
  lpeInvalidCast = 'Invalid cast';
  lpeInvalidCondition = 'Invalid condition';
  lpeInvalidEvaluation = 'Invalid evaluation';
  lpeInvalidForward = 'Forwarded declaration "%s" not resolved';
  lpeInvalidIndex = 'Invalid index "%s"';
  lpeInvalidIterator = 'Variable cannot be used for iteration';
  lpeInvalidJump = 'Invalid jump';
  lpeInvalidRange = 'Expression is not a valid range';
  lpeInvalidValueForType = 'Invalid value for type "%s"';
  lpeInvalidWithReference = 'Invalid with-reference';
  lpeLostClosingParenthesis = 'Found closing parenthesis without matching opening parenthesis';
  lpeLostConditional = 'Found conditional without matching opening statement';
  lpeNoDefaultForParam = 'No default value for parameter %d found';
  lpeNoForwardMatch = 'Forwarded declaration doesn''t match';
  lpeNoOverloadedMethod = 'Don''t know which overloaded method to call with params (%s)';
  lpeOperatorExpected = 'Operator expected';
  lpeOutOfStackRange = 'Out of stack range';
  lpeOutOfTypeRange = 'Out of type range';
  lpeRuntime = 'Runtime error: "%s"';
  lpeStatementNotAllowed = 'Statement not allowed here';
  lpeTooMuchParameters = 'Too much parameters found';
  lpeTypeExpected = 'Type expected';
  lpeUnexpectedToken = 'Found unexpected token "%s"';
  lpeUnknownDeclaration = 'Unknown declaration "%s"';
  lpeUnknownOC = 'Unknown opcode';
  lpeUnknownParent = 'Cannot find parent declaration';
  lpeVariableExpected = 'Variable expected';
  lpeVariableOfTypeExpected = 'Expected variable of type "%s", got "%s"';
  lpeWrongNumberParams = 'Wrong number of parameters found, expected %d';

procedure LapeException(Msg: string); overload;
procedure LapeException(Msg: string; DocPos: TDocPos); overload;
procedure LapeException(Msg: string; DocPos: array of TLapeBaseDeclClass); overload;
procedure LapeExceptionFmt(Msg: string; Args: array of const); overload;
procedure LapeExceptionFmt(Msg: string; Args: array of const; DocPos: TDocPos); overload;
procedure LapeExceptionFmt(Msg: string; Args: array of const; DocPos: array of TLapeBaseDeclClass); overload;

implementation

procedure LapeException(Msg: string);
begin
  raise lpException.Create(Msg);
end;

procedure LapeException(Msg: string; DocPos: TDocPos);
begin
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Msg := Format(lpeExceptionAt, [Msg, DocPos.Line, DocPos.Col]);
  if (DocPos.FileName <> '') then
    Msg := Format(lpeExceptionIn, [Msg, DocPos.FileName]);
  LapeException(Msg);
end;

procedure LapeException(Msg: string; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
begin
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      LapeException(Msg, DocPos[i].DocPos);
      Exit;
    end;
  LapeException(Msg);
end;

procedure LapeExceptionFmt(Msg: string; Args: array of const);
begin
  LapeException(Format(Msg, Args));
end;

procedure LapeExceptionFmt(Msg: string; Args: array of const; DocPos: TDocPos);
begin
  Msg := Format(Msg, Args);
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Msg := Format(lpeExceptionAt, [Msg, DocPos.Line, DocPos.Col]);
  if (DocPos.FileName <> '') then
    Msg := Format(lpeExceptionIn, [Msg, DocPos.FileName]);
  LapeException(Msg);
end;

procedure LapeExceptionFmt(Msg: string; Args: array of const; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
begin
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      LapeExceptionFmt(Msg, Args, DocPos[i].DocPos);
      Exit;
    end;
  LapeExceptionFmt(Msg, Args);
end;

end.

