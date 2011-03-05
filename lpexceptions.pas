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
  lpeBlockExpected = 'Block Expected';
  lpeCannotAssign = 'Target cannot be assigned to';
  lpeCannotEvalConstProc = 'Procedures cannot be used for constant evaluation';
  lpeCannotInvoke = 'Cannot invoke identifier';
  lpeCannotOverload = 'Cannot overload function';
  lpeClosingParenthesisExpected = 'Closing parenthesis expected';
  lpeDefaultToMoreThanOne = 'Runtime default value can only be assigned to one variable';
  lpeDuplicateDeclaration = 'Duplicate declaration "%s"';
  lpeExceptionAt = '%s at line %d, column %d';
  lpeExceptionIn = '%s in file "%s"';
  lpeExpected = '%s Expected';
  lpeExpectedOther = 'Found unexpected token "%s", expected "%s"';
  lpeExpressionExpected = 'Expression Expected';
  lpeImpossible = 'It''s impossible!';
  lpeIncompatibleAssignment = 'Can''t assign "%s" to "%s"';
  lpeIncompatibleOperator = 'Operator "%s" not compatible with types';
  lpeIncompatibleOperator1 = 'Operator "%s" not compatible with "%s"';
  lpeIncompatibleOperator2 = 'Operator "%s" not compatible with types "%s" and "%s"';
  lpeInvalidAssignment = 'Invalid Assignment';
  lpeInvalidCast = 'Invalid Cast';
  lpeInvalidCondition = 'Invalid Condition';
  lpeInvalidEvaluation = 'Invalid Evaluation';
  lpeInvalidForward = 'Forwarded declaration "%s" not resolved';
  lpeInvalidIndex = 'Invalid Index "%s"';
  lpeInvalidIterator = 'Variable cannot be used for iteration';
  lpeInvalidRange = 'Expression is not a valid range';
  lpeInvalidValueForType = 'Invalid value for type "%s"';
  lpeLostClosingParenthesis = 'Found closing parenthesis without matching opening parenthesis';
  lpeNoDefaultForParam = 'No default value for parameter %d found';
  lpeNoOverloadedMethod = 'Don''t know which overloaded method to call';
  lpeOperatorExpected = 'Operator Expected';
  lpeOutOfStackRange = 'Out of Stack Range';
  lpeRuntime = 'Runtime Error: "%s"';
  lpeTooMuchParameters = 'Too much parameters found';
  lpeTypeExpected = 'Type Expected';
  lpeUnexpectedToken = 'Found unexpected token "%s"';
  lpeUnknownDeclaration = 'Unknown declaration "%s"';
  lpeUnknownOC = 'Unknown OpCode';
  lpeUnknownParent = 'Cannot find parent declaration';
  lpeVariableExpected = 'Variable Expected';
  lpeVariableOfTypeExpected = 'Expected variable of type "%s", got "%s"';

procedure LapeException(Msg: string); overload; inline;
procedure LapeException(Msg: string; DocPos: TDocPos); overload; inline;
procedure LapeException(Msg: string; Args: array of const); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
procedure LapeException(Msg: string; Args: array of const; DocPos: TDocPos); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

implementation

procedure LapeException(Msg: string);
begin
  raise lpException.Create(Msg);
end;

procedure LapeException(Msg: string; DocPos: TDocPos);
begin
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Msg := Format(lpeExceptionAt, [Msg, DocPos.Line, DocPos.Col]);
  if (lpString(DocPos.FileName) <> '') then
    Msg := Format(lpeExceptionIn, [Msg, DocPos.FileName]);
  LapeException(Msg);
end;

procedure LapeException(Msg: string; Args: array of const);
begin
  LapeException(Format(Msg, Args));
end;

procedure LapeException(Msg: string; Args: array of const; DocPos: TDocPos);
begin
  Msg := Format(Msg, Args);
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Msg := Format(lpeExceptionAt, [Msg, DocPos.Line, DocPos.Col]);
  if (lpString(DocPos.FileName) <> '') then
    Msg := Format(lpeExceptionIn, [Msg, DocPos.FileName]);
  LapeException(Msg);
end;

end.

