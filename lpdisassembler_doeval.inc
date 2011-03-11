{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include prints all the evaluation related opcodes in the bytecode.
}
{%MainUnit lpdisassembler.pas}

procedure DoEval__PtrD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), IntToStr(PtrL), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), IntToStr(PtrL), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), IntToStr(PtrL), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), IntToStr(PtrL), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), IntToStr(PtrL), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_VarR) + ocSize);
end;
procedure DoEval__PtrD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_StkR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_VarR) + ocSize);
end;
procedure DoEval__PtrD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_StkR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrR)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), IntToStr(PtrD), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [2*SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', IntToStr(PtrL), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', IntToStr(PtrL), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_VarR) + ocSize);
end;
procedure DoEval__VarD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', IntToStr(PtrL), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_StkR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', IntToStr(PtrL), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', IntToStr(PtrL), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__VarD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PtrR) + ocSize);
end;
procedure DoEval__VarD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_VarR) + ocSize);
end;
procedure DoEval__VarD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_StkR) + ocSize);
end;
procedure DoEval__VarD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PVarR) + ocSize);
end;
procedure DoEval__VarD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PStkR) + ocSize);
end;
procedure DoEval__VarD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PtrR) + ocSize);
end;
procedure DoEval__VarD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_VarR) + ocSize);
end;
procedure DoEval__VarD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_StkR) + ocSize);
end;
procedure DoEval__VarD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PVarR) + ocSize);
end;
procedure DoEval__VarD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_VarR) + ocSize);
end;
procedure DoEval__VarD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_StkR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrR)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_VarR) + ocSize);
end;
procedure DoEval__VarD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_StkR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'VarStack[VarStackPos + '+IntToStr(VOffsetD)+']', '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [2*SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', IntToStr(PtrL), IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', IntToStr(PtrL), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_VarR) + ocSize);
end;
procedure DoEval__StkD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetR)+']', IntToStr(PtrL), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_StkR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', IntToStr(PtrL), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SizeOf(Pointer))+']', IntToStr(PtrL), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__StkD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PtrR) + ocSize);
end;
procedure DoEval__StkD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_VarR) + ocSize);
end;
procedure DoEval__StkD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetR)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_StkR) + ocSize);
end;
procedure DoEval__StkD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PVarR) + ocSize);
end;
procedure DoEval__StkD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SizeOf(Pointer))+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PStkR) + ocSize);
end;
procedure DoEval__StkD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PtrR) + ocSize);
end;
procedure DoEval__StkD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_VarR) + ocSize);
end;
procedure DoEval__StkD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_StkR) + ocSize);
end;
procedure DoEval__StkD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PVarR) + ocSize);
end;
procedure DoEval__StkD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_VarR) + ocSize);
end;
procedure DoEval__StkD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetR)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_StkR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos]', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SizeOf(Pointer))+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SizeOf(Pointer))+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SizeOf(Pointer))+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_VarR) + ocSize);
end;
procedure DoEval__StkD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_StkR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(SizeOf(Pointer))+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), 'Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+']', '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), IntToStr(PtrL), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), IntToStr(PtrL), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), IntToStr(PtrL), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), IntToStr(PtrL), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), IntToStr(PtrL), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_VarR) + ocSize);
end;
procedure DoEval__PVarD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_StkR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_VarR) + ocSize);
end;
procedure DoEval__PVarD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_StkR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrR)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(VarStack[VarStackPos + '+IntToStr(VOffsetD)+'])^ + '+IntToStr(PDestOffset), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [2*SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PStkR) + ocSize);
end;
{$IFDEF Lape_PStkD}
procedure DoEval__PStkD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrL), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrL), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetR)+'])^', IntToStr(PtrL), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrL), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', IntToStr(PtrL), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_VarR) + ocSize);
end;
procedure DoEval__PStkD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetR)+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_StkR) + ocSize);
end;
procedure DoEval__PStkD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', IntToStr(PtrR)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_VarR) + ocSize);
end;
procedure DoEval__PStkD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_StkR) + ocSize);
end;
procedure DoEval__PStkD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetL)+']', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), IntToStr(PtrR)]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetR)+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetL)+'])^ + '+IntToStr(PLeftOffset), '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', IntToStr(PtrR)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', 'VarStack[VarStackPos + '+IntToStr(VOffsetR)+']']);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', '(Stack[StackPos - '+IntToStr(SOffsetL)+'])^', 'Stack[StackPos - '+IntToStr(SOffsetR)+']']);
    _WriteLn('IncStackPos %d', [StackInc]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^', '(VarStack[VarStackPos + '+IntToStr(VOffsetR)+'])^ + '+IntToStr(PRightOffset)]);
    _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    _WriteLn('Eval %s %s %s %s', [IntToStr(Pointer({$IFNDEF FPC}@{$ENDIF}Proc)), '(Stack[StackPos - '+IntToStr(3*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(2*SizeOf(Pointer))+'])^', '(Stack[StackPos - '+IntToStr(SizeOf(Pointer))+'])^']);
    _WriteLn('DecStackPos %d', [2*SizeOf(Pointer)]);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_PStkR) + ocSize);
end;
{$ENDIF}
