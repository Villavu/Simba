{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Jump headers for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function _Jmp(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _Jmp(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpR(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpR(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;

{
function _Jmp_Var(ATarget: UInt32; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
function _Jmp_Ptr(ATarget: UInt32; APtr: Pointer; var Offset: Integer): Integer;
function _Jmp_PVar(ATarget: UInt32; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;
}
function _JmpR_Var(Jmp: Int32; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
function _JmpR_Ptr(Jmp: Int32; APtr: Pointer; var Offset: Integer): Integer;
function _JmpR_PVar(Jmp: Int32; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;

//jump if
{
function _JmpIf8_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Stk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Var(Target: UInt32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Ptr(Target: UInt32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PStk(Target: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
}
//jump relative if
function _JmpRIf8_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
//jump relative if not
function _JmpRIfNot8_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Stk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Ptr(Jmp: Int32; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PStk(Jmp: Int32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
{
function _JmpIf8_Stk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Stk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Stk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Stk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Var(Target: UInt32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_Ptr(Target: UInt32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PStk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PStk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PStk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PStk(Target: UInt32; Pos: PDocPos = nil): Integer; overload;
function _JmpIf8_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf16_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf32_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpIf64_PVar(Target: UInt32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
}
function _JmpRIf8_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;

function _JmpRIfNot8_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Stk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Var(Jmp: Int32; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Ptr(Jmp: Int32; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PStk(Jmp: Int32; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PVar(Jmp: Int32; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;

