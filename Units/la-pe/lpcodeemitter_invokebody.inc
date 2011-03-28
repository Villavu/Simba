{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Invoke bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

{
function TLapeCodeEmitterBase._IncCall_Stk(ASOffset: TStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncCall_Stk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Stk));
  with POC_Invoke_Stk(@FCode[Offset])^ do begin
    SOffset := ASOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Stk));
end;

function TLapeCodeEmitterBase._IncCall_PStk(AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncCall_PStk, Offset, Pos);
  _UInt16(AParamSize, Offset);
end;
}

function TLapeCodeEmitterBase._IncCall_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncCall_Var, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Var));
  with POC_Invoke_Var(@FCode[Offset])^ do begin
    VOffset := AVOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Var));
end;

function TLapeCodeEmitterBase._IncCall_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncCall_PVar, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_PVar));
  with POC_Invoke_PVar(@FCode[Offset])^ do begin
    VOffset := AVOffset;
    POffset := APOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_PVar));
end;

function TLapeCodeEmitterBase._IncCall_Ptr(APtr: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncCall_Ptr, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Ptr));
  with POC_Invoke_Ptr(@FCode[Offset])^ do begin
    Ptr := APtr;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Ptr));
end;

{
function TLapeCodeEmitterBase._InvokeImported_Stk(ASOffset: TStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  o: Integer;
begin
  o := _IncCall_Stk(ASOffset, AParamSize, Offset, Pos);
  Result := _op(ocInvokeImported_Stk, o, Pos);
end;

function TLapeCodeEmitterBase._InvokeImported_PStk(AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  o: Integer;
begin
  o := _IncCall_PStk(AParamSize, Offset, Pos);
  Result := _op(ocInvokeImported_PStk, o, Pos);
end;
}

function TLapeCodeEmitterBase._InvokeImported_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  o: Integer;
begin
  o := _IncCall_Var(AVOffset, AParamSize, Offset, Pos);
  Result := _op(ocInvokeImported_Var, o, Pos);
end;

function TLapeCodeEmitterBase._InvokeImported_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  o: Integer;
begin
  o := _IncCall_PVar(AVOffset, APOffset, AParamSize, Offset, Pos);
  Result := _op(ocInvokeImported_PVar, o, Pos);
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr(APtr: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  o: Integer;
begin
  o := _IncCall_Ptr(APtr, AParamSize, Offset, Pos);
  Result := _op(ocInvokeImported_Ptr, o, Pos);
end;

{
function TLapeCodeEmitterBase._InvokeImported_Stk_Stk(ASOffset: TStackOffset; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Stk_Stk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Stk_Stk));
  with POC_Invoke_Stk_Stk(@FCode[Offset])^ do begin
    SOffset := ASOffset;
    ParamSize := AParamSize;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Stk_Stk));
end;

function TLapeCodeEmitterBase._InvokeImported_Stk_PStk(ASOffset: TStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Stk_PStk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Stk_PStk));
  with POC_Invoke_Stk_PStk(@FCode[Offset])^ do begin
    SOffset := ASOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Stk_PStk));
end;

function TLapeCodeEmitterBase._InvokeImported_Stk_Var(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Stk_Var, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Stk_Var));
  with POC_Invoke_Stk_Var(@FCode[Offset])^ do begin
    SOffset := ASOffset;
    VOffsetD := AVOffsetD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Stk_Var));
end;

function TLapeCodeEmitterBase._InvokeImported_Stk_PVar(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Stk_PVar, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Stk_PVar));
  with POC_Invoke_Stk_PVar(@FCode[Offset])^ do begin
    SOffset := ASOffset;
    VOffsetD := AVOffsetD;
    POffset := APOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Stk_PVar));
end;

function TLapeCodeEmitterBase._InvokeImported_Stk_Ptr(ASOffset: TStackOffset; APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Stk_Ptr, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Stk_Ptr));
  with POC_Invoke_Stk_Ptr(@FCode[Offset])^ do begin
    SOffset := ASOffset;
    PtrD := APtrD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Stk_Ptr));
end;
}

function TLapeCodeEmitterBase._InvokeImported_Var_Stk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Var_Stk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Var_Stk));
  with POC_Invoke_Var_Stk(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    ParamSize := AParamSize;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Var_Stk));
end;

function TLapeCodeEmitterBase._InvokeImported_Var_PStk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Var_PStk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Var_PStk));
  with POC_Invoke_Var_PStk(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Var_PStk));
end;

function TLapeCodeEmitterBase._InvokeImported_Var_Var(AVOffsetS, AVOffsetD: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Var_Var, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Var_Var));
  with POC_Invoke_Var_Var(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    VOffsetD := AVOffsetD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Var_Var));
end;

function TLapeCodeEmitterBase._InvokeImported_Var_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Var_PVar, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Var_PVar));
  with POC_Invoke_Var_PVar(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    VOffsetD := AVOffsetS;
    POffset := APOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Var_PVar));
end;

function TLapeCodeEmitterBase._InvokeImported_Var_Ptr(AVOffsetS: TVarStackOffset; APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Var_Ptr, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Var_Ptr));
  with POC_Invoke_Var_Ptr(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    PtrD := APtrD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Var_Ptr));
end;


function TLapeCodeEmitterBase._InvokeImported_PVar_Stk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_PVar_Stk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_PVar_Stk));
  with POC_Invoke_PVar_Stk(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    POffsetS := APOffsetS;
    ParamSize := AParamSize;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_PVar_Stk));
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_PStk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_PVar_PStk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_PVar_PStk));
  with POC_Invoke_PVar_PStk(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    POffsetS := APOffsetS;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_PVar_PStk));
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_Var(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_PVar_Var, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_PVar_Var));
  with POC_Invoke_PVar_Var(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    VOffsetD := AVOffsetD;
    POffsetS := APOffsetS;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_PVar_Var));
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS, APOffsetD: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_PVar_PVar, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_PVar_PVar));
  with POC_Invoke_PVar_PVar(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    VOffsetD := AVOffsetD;
    POffsetS := APOffsetS;
    POffsetD := APOffsetD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_PVar_PVar));
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_Ptr(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_PVar_Ptr, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_PVar_Ptr));
  with POC_Invoke_PVar_Ptr(@FCode[Offset])^ do begin
    VOffsetS := AVOffsetS;
    POffsetS := APOffsetS;
    PtrD := APtrD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_PVar_Ptr));
end;


function TLapeCodeEmitterBase._InvokeImported_Ptr_Stk(APtrS: Pointer; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_Stk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Ptr_Stk));
  with POC_Invoke_Ptr_Stk(@FCode[Offset])^ do begin
    PtrS := APtrS;
    ParamSize := AParamSize;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Ptr_Stk));
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_PStk(APtrS: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_PStk, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Ptr_PStk));
  with POC_Invoke_Ptr_PStk(@FCode[Offset])^ do begin
    PtrS := APtrS;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Ptr_PStk));
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_Var(APtrS: Pointer; AVOffsetD: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_Var, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Ptr_Var));
  with POC_Invoke_Ptr_Var(@FCode[Offset])^ do begin
    PtrS := APtrS;
    VOffsetD := AVOffsetD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Ptr_Var));
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_PVar(APtrS: Pointer; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_PVar, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Ptr_PVar));
  with POC_Invoke_Ptr_PVar(@FCode[Offset])^ do begin
    PtrS := APtrS;
    VOffsetD := AVOffsetD;
    POffset := APOffset;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Ptr_PVar));
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_Ptr(APtrS, APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_Ptr, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Invoke_Ptr_Ptr));
  with POC_Invoke_Ptr_Ptr(@FCode[Offset])^ do begin
    PtrS := APtrS;
    PtrD := APtrD;
    ParamSize := AParamSize;
  end;
  Inc(Offset, SizeOf(TOC_Invoke_Ptr_Ptr));
end;


//function TLapeCodeEmitterBase._IncCall_Stk(ASOffset: TStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
//  var o: Integer; begin o := -1; Result := _IncCall_Stk(ASOffset, AParamSize, o, Pos); end;
//function TLapeCodeEmitterBase._IncCall_PStk(AParamSize: UInt16; Pos: PDocPos = nil): Integer;
//  var o: Integer; begin o := -1; Result := _IncCall_PStk(AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._IncCall_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _IncCall_Var(AVOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._IncCall_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _IncCall_PVar(AVOffset, APOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._IncCall_Ptr(APtr: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _IncCall_Ptr(APtr, AParamSize, o, Pos); end;

//function TLapeCodeEmitterBase._InvokeImported_Stk(ASOffset: TStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
//  var o: Integer; begin o := -1; Result := _InvokeImported_Stk(ASOffset, AParamSize, o, Pos); end;
//function TLapeCodeEmitterBase._InvokeImported_PStk(AParamSize: UInt16; Pos: PDocPos = nil): Integer;
//  var o: Integer; begin o := -1; Result := _InvokeImported_PStk(AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Var(AVOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_PVar(AVOffset, APOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Ptr(APtr: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Ptr(APtr, AParamSize, o, Pos); end;

{
function TLapeCodeEmitterBase._InvokeImported_Stk_Stk(ASOffset: TStackOffset; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Stk_Stk(ASOffset, AParamSize, AStackInc, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Stk_PStk(ASOffset: TStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Stk_PStk(ASOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Stk_Var(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Stk_Var(ASOffset, AVOffsetD, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Stk_PVar(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Stk_PVar(ASOffset, AVOffsetD, APOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Stk_Ptr(ASOffset: TStackOffset; APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Stk_Ptr(ASOffset, APtrD, AParamSize, o, Pos); end;
}

function TLapeCodeEmitterBase._InvokeImported_Var_Stk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Var_Stk(AVOffsetS, AParamSize, AStackInc, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Var_PStk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Var_PStk(AVOffsetS, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Var_Var(AVOffsetS, AVOffsetD: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Var_Var(AVOffsetS, AVOffsetD, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Var_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Var_PVar(AVOffsetS, AVOffsetD, APOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Var_Ptr(AVOffsetS: TVarStackOffset; APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Var_Ptr(AVOffsetS, APtrD, AParamSize, o, Pos); end;

function TLapeCodeEmitterBase._InvokeImported_PVar_Stk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_PVar_Stk(AVOffsetS, APOffsetS, AParamSize, AStackInc, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_PVar_PStk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_PVar_PStk(AVOffsetS, APOffsetS, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_PVar_Var(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_PVar_Var(AVOffsetS, AVOffsetD, APOffsetS, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_PVar_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS, APOffsetD: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_PVar_PVar(AVOffsetS, AVOffsetD, APOffsetS, APOffsetD, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_PVar_Ptr(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_PVar_Ptr(AVOffsetS, APOffsetS, APtrD, AParamSize, o, Pos); end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_Stk(APtrS: Pointer; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Ptr_Stk(APtrS, AParamSize, AStackInc, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Ptr_PStk(APtrS: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Ptr_PStk(APtrS, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Ptr_Var(APtrS: Pointer; AVOffsetD: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Ptr_Var(APtrS, AVOffsetD, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Ptr_PVar(APtrS: Pointer; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Ptr_PVar(APtrS, AVOffsetD, APOffset, AParamSize, o, Pos); end;
function TLapeCodeEmitterBase._InvokeImported_Ptr_Ptr(APtrS, APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InvokeImported_Ptr_Ptr(APtrS, APtrD, AParamSize, o, Pos); end;

