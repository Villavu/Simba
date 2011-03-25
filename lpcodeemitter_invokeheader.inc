{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Invoke headers for emitter class.
}
{%MainUnit lpcodeemitter.pas}

//function _IncCall_Stk(ASOffset: TStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
//function _IncCall_PStk(AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _IncCall_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _IncCall_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _IncCall_Ptr(APtr: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;

//function _InvokeImported_Stk(ASOffset: TStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
//function _InvokeImported_PStk(AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr(APtr: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;

{
function _InvokeImported_Stk_Stk(ASOffset: TStackOffset; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_PStk(ASOffset: TStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_Var(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_PVar(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_Ptr(ASOffset: TStackOffset; APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
}

function _InvokeImported_Var_Stk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_PStk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_Var(AVOffsetS, AVOffsetD: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_Ptr(AVOffsetS: TVarStackOffset; APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;

function _InvokeImported_PVar_Stk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_PStk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_Var(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS, APOffsetD: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_Ptr(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;

function _InvokeImported_Ptr_Stk(APtrS: Pointer; AParamSize: UInt16; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_PStk(APtrS: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_Var(APtrS: Pointer; AVOffsetD: TVarStackOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_PVar(APtrS: Pointer; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_Ptr(APtrS, APtrD: Pointer; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;


//function _IncCall_Stk(ASOffset: TStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
//function _IncCall_PStk(AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _IncCall_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _IncCall_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _IncCall_Ptr(APtr: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;

//function _InvokeImported_Stk(ASOffset: TStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
//function _InvokeImported_PStk(AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var(AVOffset: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr(APtr: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;

{
function _InvokeImported_Stk_Stk(ASOffset: TStackOffset; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_PStk(ASOffset: TStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_Var(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_PVar(ASOffset: TStackOffset; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Stk_Ptr(ASOffset: TStackOffset; APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
}

function _InvokeImported_Var_Stk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_PStk(AVOffsetS: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_Var(AVOffsetS, AVOffsetD: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Var_Ptr(AVOffsetS: TVarStackOffset; APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;

function _InvokeImported_PVar_Stk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_PStk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_Var(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS, APOffsetD: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_PVar_Ptr(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;

function _InvokeImported_Ptr_Stk(APtrS: Pointer; AParamSize: UInt16; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_PStk(APtrS: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_Var(APtrS: Pointer; AVOffsetD: TVarStackOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_PVar(APtrS: Pointer; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
function _InvokeImported_Ptr_Ptr(APtrS, APtrD: Pointer; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
