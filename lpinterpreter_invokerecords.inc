{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	These are the invoke related records for the bytecode.
}
{%MainUnit lpinterpreter.pas}

POC_Invoke_Stk = ^TOC_Invoke_Stk;
TOC_Invoke_Stk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  SOffset: TStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Var = ^TOC_Invoke_Var;
TOC_Invoke_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffset: TVarStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_PVar = ^TOC_Invoke_PVar;
TOC_Invoke_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffset: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Ptr = ^TOC_Invoke_Ptr;
TOC_Invoke_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Ptr: Pointer;
  ParamSize: UInt16;
end;

POC_Invoke_Stk_Stk = ^TOC_Invoke_Stk_Stk;
TOC_Invoke_Stk_Stk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  SOffset: TStackOffset;
  ParamSize: UInt16;
  StackInc: TStackInc;
end;

POC_Invoke_Stk_PStk = ^TOC_Invoke_Stk_PStk;
TOC_Invoke_Stk_PStk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  SOffset: TStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Stk_Var = ^TOC_Invoke_Stk_Var;
TOC_Invoke_Stk_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  SOffset: TStackOffset;
  VOffsetD: TVarStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Stk_Ptr = ^TOC_Invoke_Stk_Ptr;
TOC_Invoke_Stk_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  SOffset: TStackOffset;
  PtrD: Pointer;
  ParamSize: UInt16;
end;

POC_Invoke_Stk_PVar = ^TOC_Invoke_Stk_PVar;
TOC_Invoke_Stk_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  SOffset: TStackOffset;
  VOffsetD: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Var_Stk = ^TOC_Invoke_Var_Stk;
TOC_Invoke_Var_Stk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS: TVarStackOffset;
  ParamSize: UInt16;
  StackInc: TStackInc;
end;

POC_Invoke_Var_PStk = ^TOC_Invoke_Var_PStk;
TOC_Invoke_Var_PStk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS: TVarStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Var_Var = ^TOC_Invoke_Var_Var;
TOC_Invoke_Var_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS, VOffsetD: TVarStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Var_PVar = ^TOC_Invoke_Var_PVar;
TOC_Invoke_Var_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS, VOffsetD: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Var_Ptr = ^TOC_Invoke_Var_Ptr;
TOC_Invoke_Var_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS: TVarStackOffset;
  PtrD: Pointer;
  ParamSize: UInt16;
end;

POC_Invoke_PVar_Stk = ^TOC_Invoke_PVar_Stk;
TOC_Invoke_PVar_Stk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS: TVarStackOffset;
  POffsetS: TPointerOffset;
  ParamSize: UInt16;
  StackInc: TStackInc;
end;

POC_Invoke_PVar_PStk = ^TOC_Invoke_PVar_PStk;
TOC_Invoke_PVar_PStk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS: TVarStackOffset;
  POffsetS: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_PVar_Var = ^TOC_Invoke_PVar_Var;
TOC_Invoke_PVar_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS, VOffsetD: TVarStackOffset;
  POffsetS: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_PVar_PVar = ^TOC_Invoke_PVar_PVar;
TOC_Invoke_PVar_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS, VOffsetD: TVarStackOffset;
  POffsetS, POffsetD: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_PVar_Ptr = ^TOC_Invoke_PVar_Ptr;
TOC_Invoke_PVar_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  VOffsetS: TVarStackOffset;
  POffsetS: TPointerOffset;
  PtrD: Pointer;
  ParamSize: UInt16;
end;

POC_Invoke_Ptr_Stk = ^TOC_Invoke_Ptr_Stk;
TOC_Invoke_Ptr_Stk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  PtrS: Pointer;
  ParamSize: UInt16;
  StackInc: TStackInc;
end;

POC_Invoke_Ptr_PStk = ^TOC_Invoke_Ptr_PStk;
TOC_Invoke_Ptr_PStk = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  PtrS: Pointer;
  ParamSize: UInt16;
end;

POC_Invoke_Ptr_Var = ^TOC_Invoke_Ptr_Var;
TOC_Invoke_Ptr_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  PtrS: Pointer;
  VOffsetD: TVarStackOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Ptr_PVar = ^TOC_Invoke_Ptr_PVar;
TOC_Invoke_Ptr_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  PtrS: Pointer;
  VOffsetD: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: UInt16;
end;

POC_Invoke_Ptr_Ptr = ^TOC_Invoke_Ptr_Ptr;
TOC_Invoke_Ptr_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  PtrS, PtrD: Pointer;
  ParamSize: UInt16;
end;
