{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	These are the jump related records for the bytecode.
}
{%MainUnit lpinterpreter.pas}

POC_Jmp_Var = ^TOC_Jmp_Var;
TOC_Jmp_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Target: UInt32;
  VarStackOffset: TVarStackOffset;
end;

POC_Jmp_Ptr = ^TOC_Jmp_Ptr;
TOC_Jmp_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Target: UInt32;
  Ptr: Pointer;
end;

POC_Jmp_PVar = ^TOC_Jmp_PVar;
TOC_Jmp_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Target: UInt32;
  VarStackOffset: TVarStackOffset;
  PointerOffset: TPointerOffset;
end;

POC_JmpR_Var = ^TOC_JmpR_Var;
TOC_JmpR_Var = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Offset: Int32;
  VarStackOffset: TVarStackOffset;
end;

POC_JmpR_Ptr = ^TOC_JmpR_Ptr;
TOC_JmpR_Ptr = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Offset: Int32;
  Ptr: Pointer;
end;

POC_JmpR_PVar = ^TOC_JmpR_PVar;
TOC_JmpR_PVar = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
  Offset: Int32;
  VarStackOffset: TVarStackOffset;
  PointerOffset: TPointerOffset;
end;
