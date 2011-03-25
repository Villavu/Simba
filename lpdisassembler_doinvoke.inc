{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include prints all the invoke related opcodes in the bytecode.
}
{%MainUnit lpdisassembler.pas}

procedure DoIncCall_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('IncCall Stack[StackPos - '+IntToStr(SOffset)+'] '+IntToStr(ParamSize));
    _WriteLn('StackDec '+IntToStr(SOffset - ParamSize));
  end;
  Inc(Code, SizeOf(TOC_Invoke_Stk) + ocSize);
end;

procedure DoIncCall_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
var
  ParamSize: UInt16;
begin
  ParamSize := PUInt16(PtrUInt(Code) + ocSize)^;
  _WriteLn('IncCall (Stack[StackPos - '+IntToStr(ParamSize - SizeOf(Pointer))+'])^ '+IntToStr(ParamSize));
  _WriteLn('StackDec '+IntToStr(SizeOf(Pointer)));
  Inc(Code, SizeOf(UInt16) + ocSize);
end;

procedure DoIncCall_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var(PtrUInt(Code) + ocSize)^ do
    _WriteLn('IncCall VarStack[VarStackPos + '+IntToStr(VOffset)+'] '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Var) + ocSize);
end;

procedure DoIncCall_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar(PtrUInt(Code) + ocSize)^ do
    _WriteLn('IncCall (VarStack[VarStackPos + '+IntToStr(VOffset)+'] + '+IntToStr(POffset)+')^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_PVar) + ocSize);
end;

procedure DoIncCall_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr(PtrUInt(Code) + ocSize)^ do
    _WriteLn('IncCall '+IntToStr(Ptr)+' '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Ptr) + ocSize);
end;

procedure DoInvokeImported_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('InvokeImportedProc Stack[StackPos - '+IntToStr(SOffset)+'] '+IntToStr(ParamSize));
    _WriteLn('StackDec '+IntToStr(SOffset - ParamSize));
  end;
  Inc(Code, SizeOf(TOC_Invoke_Stk) + ocSize);
end;

procedure DoInvokeImported_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
var
  ParamSize: UInt16;
begin
  ParamSize := PUInt16(PtrUInt(Code) + ocSize)^;
  _WriteLn('InvokeImportedProc (Stack[StackPos - '+IntToStr(ParamSize - SizeOf(Pointer))+'])^ '+IntToStr(ParamSize));
  _WriteLn('StackDec '+IntToStr(SizeOf(Pointer)));
  Inc(Code, SizeOf(UInt16) + ocSize);
end;

procedure DoInvokeImported_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedProc VarStack[VarStackPos + '+IntToStr(VOffset)+'] '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Var) + ocSize);
end;

procedure DoInvokeImported_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedProc (VarStack[VarStackPos + '+IntToStr(VOffset)+'] + '+IntToStr(POffset)+')^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_PVar) + ocSize);
end;

procedure DoInvokeImported_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedProc '+IntToStr(Ptr)+' '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Ptr) + ocSize);
end;

procedure DoInvokeImported_Stk_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Stk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('InvokeImportedFunc Stack[StackPos - '+IntToStr(SOffset)+'] Stack[StackPos - '+IntToStr(SOffset)+'] '+IntToStr(ParamSize));
    _WriteLn('StackInc '+IntToStr(ParamSize - SOffset + StackInc));
  end;
  Inc(Code, SizeOf(TOC_Invoke_Stk_Stk) + ocSize);
end;

procedure DoInvokeImported_Stk_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_PStk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('InvokeImportedFunc Stack[StackPos - '+IntToStr(SOffset)+'] (Stack[StackPos - '+IntToStr(SOffset)+'])^ '+IntToStr(ParamSize));
    _WriteLn('StackInc '+IntToStr(ParamSize - SOffset));
  end;
  Inc(Code, SizeOf(TOC_Invoke_Stk_PStk) + ocSize);
end;

procedure DoInvokeImported_Stk_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Var(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc Stack[StackPos - '+IntToStr(SOffset)+'] VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Stk_Var) + ocSize);
end;

procedure DoInvokeImported_Stk_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_PVar(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc Stack[StackPos - '+IntToStr(SOffset)+'] (VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] + '+IntToStr(POffset)+')^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Stk_PVar) + ocSize);
end;

procedure DoInvokeImported_Stk_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Ptr(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc Stack[StackPos - '+IntToStr(SOffset)+'] '+IntToStr(PtrD)+' '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Stk_Ptr) + ocSize);
end;

procedure DoInvokeImported_Var_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Stk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('InvokeImportedFunc VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] Stack[StackPos - '+IntToStr(ParamSize)+'] '+IntToStr(ParamSize));
    _WriteLn('StackInc '+IntToStr(StackInc));
  end;
  Inc(Code, SizeOf(TOC_Invoke_Var_Stk) + ocSize);
end;

procedure DoInvokeImported_Var_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_PStk(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] (Stack[StackPos - '+IntToStr(ParamSize)+'])^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Var_PStk) + ocSize);
end;

procedure DoInvokeImported_Var_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Var(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc VarStack[VarStackPos + '+IntToStr(VOffsetS)+']) VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Var_Var) + ocSize);
end;

procedure DoInvokeImported_Var_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_PVar(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] (VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] + '+IntToStr(POffset)+')^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Var_PVar) + ocSize);
end;

procedure DoInvokeImported_Var_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Ptr(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] '+IntToStr(PtrD)+' '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Var_Ptr) + ocSize);
end;

procedure DoInvokeImported_PVar_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Stk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('InvokeImportedFunc (VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] + '+IntToStr(POffsetS)+')^ Stack[StackPos - '+IntToStr(ParamSize)+'] '+IntToStr(ParamSize));
    _WriteLn('StackInc '+IntToStr(StackInc));
  end;
  Inc(Code, SizeOf(TOC_Invoke_PVar_Stk) + ocSize);
end;

procedure DoInvokeImported_PVar_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_PStk(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc (VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] + '+IntToStr(POffsetS)+')^ (Stack[StackPos - '+IntToStr(ParamSize)+'])^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_PVar_PStk) + ocSize);
end;

procedure DoInvokeImported_PVar_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Var(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc (VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] + '+IntToStr(POffsetS)+')^ VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_PVar_Var) + ocSize);
end;

procedure DoInvokeImported_PVar_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_PVar(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc (VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] + '+IntToStr(POffsetS)+')^ (VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] + '+IntToStr(POffsetD)+')^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_PVar_PVar) + ocSize);
end;

procedure DoInvokeImported_PVar_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Ptr(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc (VarStack[VarStackPos + '+IntToStr(VOffsetS)+'] + '+IntToStr(POffsetS)+')^ '+IntToStr(PtrD)+' '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_PVar_Ptr) + ocSize);
end;

procedure DoInvokeImported_Ptr_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Stk(PtrUInt(Code) + ocSize)^ do
  begin
    _WriteLn('InvokeImportedFunc '+IntToStr(PtrS)+' Stack[StackPos - '+IntToStr(ParamSize)+'] '+IntToStr(ParamSize));
    _WriteLn('StackInc '+IntToStr(StackInc));
  end;
  Inc(Code, SizeOf(TOC_Invoke_Ptr_Stk) + ocSize);
end;

procedure DoInvokeImported_Ptr_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_PStk(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc '+IntToStr(PtrS)+' (Stack[StackPos - '+IntToStr(ParamSize)+'])^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Ptr_PStk) + ocSize);
end;

procedure DoInvokeImported_Ptr_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Var(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc '+IntToStr(PtrS)+' VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Ptr_Var) + ocSize);
end;

procedure DoInvokeImported_Ptr_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_PVar(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc '+IntToStr(PtrS)+' (VarStack[VarStackPos + '+IntToStr(VOffsetD)+'] + '+IntToStr(POffset)+')^ '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Ptr_PVar) + ocSize);
end;

procedure DoInvokeImported_Ptr_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Ptr(PtrUInt(Code) + ocSize)^ do
    _WriteLn('InvokeImportedFunc '+IntToStr(PtrS)+' '+IntToStr(PtrD)+' '+IntToStr(ParamSize));
  Inc(Code, SizeOf(TOC_Invoke_Ptr_Ptr) + ocSize);
end;
