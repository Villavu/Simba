{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Jump bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function TLapeCodeEmitterBase._Jmp(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmp, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpR(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpR, Offset, Pos);
  _Int32(Jmp, Offset);
end;

{
function TLapeCodeEmitterBase._Jmp_Var(ATarget: UInt32; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_Jmp_Var));
  with POC_Jmp_Var(@FCode[Offset])^ do begin
    Target := ATarget;
    VarStackOffset := AVarStackOffset;
  end;
  Inc(Offset, SizeOf(TOC_Jmp_Var));
end;

function TLapeCodeEmitterBase._Jmp_Ptr(ATarget: UInt32; APtr: Pointer; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_Jmp_Ptr));
  with POC_Jmp_Ptr(@FCode[Offset])^ do begin
    Target := ATarget;
    Ptr := APtr;
  end;
  Inc(Offset, SizeOf(TOC_Jmp_Ptr));
end;

function TLapeCodeEmitterBase._Jmp_PVar(ATarget: UInt32; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_Jmp_PVar));
  with POC_Jmp_PVar(@FCode[Offset])^ do begin
    Target := ATarget;
    VarStackOffset := AVarStackOffset;
    PointerOffset := APointerOffset;
  end;
  Inc(Offset, SizeOf(TOC_Jmp_PVar));
end;
}

function TLapeCodeEmitterBase._JmpR_Var(Jmp: Int32; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_JmpR_Var));
  with POC_JmpR_Var(@FCode[Offset])^ do begin
    Offset := Jmp;
    VarStackOffset := AVarStackOffset;
  end;
  Inc(Offset, SizeOf(TOC_JmpR_Var));
end;

function TLapeCodeEmitterBase._JmpR_Ptr(Jmp: Int32; APtr: Pointer; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_JmpR_Ptr));
  with POC_JmpR_Ptr(@FCode[Offset])^ do begin
    Offset := Jmp;
    Ptr := APtr;
  end;
  Inc(Offset, SizeOf(TOC_JmpR_Ptr));
end;

function TLapeCodeEmitterBase._JmpR_PVar(Jmp: Int32; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_JmpR_PVar));
  with POC_JmpR_PVar(@FCode[Offset])^ do begin
    Offset := Jmp;
    VarStackOffset := AVarStackOffset;
    PointerOffset := APointerOffset;
  end;
  Inc(Offset, SizeOf(TOC_JmpR_PVar));
end;

//jump if
{
function TLapeCodeEmitterBase._JmpIf8_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf8_Stk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf16_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf16_Stk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf32_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf32_Stk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf64_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf64_Stk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf8_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf8_Var, Offset, Pos);
  _Jmp_Var(Target, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf16_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf16_Var, Offset, Pos);
  _Jmp_Var(Target, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf32_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf32_Var, Offset, Pos);
  _Jmp_Var(Target, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf64_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf64_Var, Offset, Pos);
  _Jmp_Var(Target, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf8_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf8_Ptr, Offset, Pos);
  _Jmp_Ptr(Target, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpIf16_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf16_Ptr, Offset, Pos);
  _Jmp_Ptr(Target, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpIf32_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf32_Ptr, Offset, Pos);
  _Jmp_Ptr(Target, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpIf64_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf64_Ptr, Offset, Pos);
  _Jmp_Ptr(Target, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpIf8_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf8_PStk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf16_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf16_PStk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf32_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf32_PStk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf64_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf64_PStk, Offset, Pos);
  _UInt32(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpIf8_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf8_PVar, Offset, Pos);
  _Jmp_PVar(Target, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf16_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf16_PVar, Offset, Pos);
  _Jmp_PVar(Target, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf32_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf32_PVar, Offset, Pos);
  _Jmp_PVar(Target, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpIf64_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpIf64_PVar, Offset, Pos);
  _Jmp_PVar(Target, VarStackOffset, PointerOffset, Offset);
end;
}

//jump relative if
function TLapeCodeEmitterBase._JmpRIf8_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf8_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf8_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

//jump relative if not
function TLapeCodeEmitterBase._JmpRIfNot8_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_Stk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_PStk, Offset, Pos);
  _Int32(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._Jmp(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _PopVar(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpR(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _PopVar(Jmp, o, Pos); end;

//jump if
{
function TLapeCodeEmitterBase._JmpIf8_Stk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf8_Stk(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf16_Stk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf16_Stk(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf32_Stk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf32_Stk(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf64_Stk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf64_Stk(Target, o, Pos); end;


function TLapeCodeEmitterBase._JmpIf8_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf8_Var(Target, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf16_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf16_Var(Target, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf32_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf32_Var(Target, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf64_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf64_Var(Target, VarStackOffset, o, Pos); end;

function TLapeCodeEmitterBase._JmpIf8_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf8_Ptr(Target, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf16_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf16_Ptr(Target, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf32_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf32_Ptr(Target, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf64_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf64_Ptr(Target, Ptr, o, Pos); end;

function TLapeCodeEmitterBase._JmpIf8_PStk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf8_PStk(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf16_PStk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf16_PStk(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf32_PStk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf32_PStk(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf64_PStk(Target: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf64_PStk(Target, o, Pos); end;

function TLapeCodeEmitterBase._JmpIf8_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf8_PVar(Target, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf16_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf16_PVar(Target, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf32_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf32_PVar(Target, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpIf64_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpIf64_PVar(Target, VarStackOffset, PointerOffset, o, Pos); end;
}

//jump relative if
function TLapeCodeEmitterBase._JmpRIf8_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_Stk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_Var(Jmp, VarStackOffset, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_Ptr(Jmp, Ptr, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_PStk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;

//jump relative if not
function TLapeCodeEmitterBase._JmpRIfNot8_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_Stk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_Var(Jmp, VarStackOffset, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_Ptr(Jmp, Ptr, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_PStk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
