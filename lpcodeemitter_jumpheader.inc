{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Jump headers for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function _Jmp(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _Jmp(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;

{
function _Jmp_Var(ATarget: TCodePos; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
function _Jmp_Ptr(ATarget: TCodePos; APtr: Pointer; var Offset: Integer): Integer;
function _Jmp_PVar(ATarget: TCodePos; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;
}
function _JmpR_Var(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
function _JmpR_Ptr(Jmp: TCodeOffset; APtr: Pointer; var Offset: Integer): Integer;
function _JmpR_PVar(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;

//jump if
{
function _JmpIf8_Stk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Stk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Stk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Stk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Ptr(Target: TCodePos; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Ptr(Target: TCodePos; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Ptr(Target: TCodePos; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Ptr(Target: TCodePos; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PStk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PStk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PStk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PStk(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
}
//jump relative if
function _JmpRIf8_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
//jump relative if not
function _JmpRIfNot8_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
{
function _JmpIf8_Stk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Stk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Stk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Stk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Var(Target: TCodePos; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Ptr(Target: TCodePos; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Ptr(Target: TCodePos; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Ptr(Target: TCodePos; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Ptr(Target: TCodePos; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PStk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PStk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PStk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PStk(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PVar(Target: TCodePos; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
}
function _JmpRIf8_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;

function _JmpRIfNot8_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;

