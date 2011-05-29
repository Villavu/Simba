{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Evaluation functions.
}
unit lpeval;

{$I lape.inc}

interface

uses
  SysUtils,
  lptypes;

type
  TLapeEvalArr = array[EOperator, ELapeBaseType, ELapeBaseType] of TLapeEvalProc;
  TLapeEvalRes = array[EOperator, ELapeBaseType, ELapeBaseType] of ELapeBaseType;
  TGetEvalRes = function(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
  TGetEvalProc = function(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;

function ValidEvalFunction(p: Pointer): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ValidEvalFunction(p: TLapeEvalProc): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

procedure LapeEval_Error(const Dest, Left, Right: Pointer);
procedure ClearEvalRes(var Arr: TLapeEvalRes);
procedure ClearEvalArr(var Arr: TLapeEvalArr);
procedure LoadEvalRes(var Arr: TLapeEvalRes);
procedure LoadEvalArr(var Arr: TLapeEvalArr);
function LapeEval_GetRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
function LapeEval_GetProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;

var
  LapeEvalErrorProc: TLapeEvalProc = {$IFDEF FPC}@{$ENDIF}LapeEval_Error;
  getEvalRes: TGetEvalRes = {$IFDEF FPC}@{$ENDIF}LapeEval_GetRes;
  getEvalProc: TGetEvalProc = {$IFDEF FPC}@{$ENDIF}LapeEval_GetProc;

  LapeEvalRes: TLapeEvalRes;
  LapeEvalArr: TLapeEvalArr;

implementation

uses
  lpexceptions;

function ValidEvalFunction(p: Pointer): Boolean;
begin
   Result := (p <> nil) and (p <> @LapeEvalErrorProc);
end;

function ValidEvalFunction(p: TLapeEvalProc): Boolean;
begin
   Result := ValidEvalFunction(@p);
end;

procedure LapeEval_Error(const Dest, Left, Right: Pointer);
begin
  LapeException(lpeInvalidEvaluation);
end;

procedure ClearEvalRes(var Arr: TLapeEvalRes);
var
  op: EOperator;
  t1, t2: ELapeBaseType;
begin
  for op := Low(EOperator) to High(EOperator) do
    for t1 := Low(ELapeBaseType) to High(ELapeBaseType) do
      for t2 := Low(ELapeBaseType) to High(ELapeBaseType) do
        Arr[op][t1][t2] := ltUnknown;
end;

procedure ClearEvalArr(var Arr: TLapeEvalArr);
var
  op: EOperator;
  BaseTypeLeft, BaseTypeRight: ELapeBaseType;
begin
  for op := Low(EOperator) to High(EOperator) do
    for BaseTypeLeft := Low(ELapeBaseType) to High(ELapeBaseType) do
      for BaseTypeRight := Low(ELapeBaseType) to High(ELapeBaseType) do
        Arr[op][BaseTypeLeft][BaseTypeRight] := @LapeEval_Error;
end;

procedure LoadEvalRes(var Arr: TLapeEvalRes);
begin
  {$I lpeval_res.inc}
end;

{$I lpeval_functions.inc}

procedure LoadEvalArr(var Arr: TLapeEvalArr);
begin
  {$I lpeval_arr.inc}
end;

procedure lpeAddr(const Dest, Left, Right: Pointer);
begin
  PPointer(Dest)^ := Left;
end;

procedure lpeDeref(const Dest, Left, Right: Pointer);
begin
  PPointer(Dest)^ := PPointer(Left)^;
end;

function LapeEval_GetRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
begin
  if (Op = op_Addr) then
    Result := ltPointer
  else
    Result := LapeEvalRes[op, Left, Right];
end;

function LapeEval_GetProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;
begin
  if (Op = op_Addr) then
    Result := @lpeAddr
  else if (Op = op_Deref) then
    Result := @lpeDeref
  else
    Result := LapeEvalArr[op, Left, Right];
end;

initialization
  ClearEvalRes(LapeEvalRes);
  ClearEvalArr(LapeEvalArr);
  LoadEvalRes(LapeEvalRes);
  LoadEvalArr(LapeEvalArr);

end.

