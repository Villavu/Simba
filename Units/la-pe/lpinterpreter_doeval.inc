{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	All the evaluation related handling functions.
}
{%MainUnit lpinterpreter.pas}

procedure DoEval__PtrD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, PtrL, @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PtrL, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, PtrL, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PtrL, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, @VarStack[VarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, @VarStack[VarStackPos + VOffsetL], @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_VarR) + ocSize);
end;
procedure DoEval__PtrD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @VarStack[VarStackPos + VOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_StkR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, @VarStack[VarStackPos + VOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @VarStack[VarStackPos + VOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @Stack[StackPos - SOffsetL], PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @Stack[StackPos - SOffsetL], @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_VarR) + ocSize);
end;
procedure DoEval__PtrD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_StkR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @Stack[StackPos - SOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @Stack[StackPos - SOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrR);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetR]);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], PtrL, @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_VarR) + ocSize);
end;
procedure DoEval__VarD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PtrL, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_StkR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], PtrL, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PtrL, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__VarD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], @VarStack[VarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PtrR) + ocSize);
end;
procedure DoEval__VarD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], @VarStack[VarStackPos + VOffsetL], @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_VarR) + ocSize);
end;
procedure DoEval__VarD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @VarStack[VarStackPos + VOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_StkR) + ocSize);
end;
procedure DoEval__VarD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], @VarStack[VarStackPos + VOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PVarR) + ocSize);
end;
procedure DoEval__VarD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @VarStack[VarStackPos + VOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PStkR) + ocSize);
end;
procedure DoEval__VarD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @Stack[StackPos - SOffsetL], PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PtrR) + ocSize);
end;
procedure DoEval__VarD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @Stack[StackPos - SOffsetL], @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_VarR) + ocSize);
end;
procedure DoEval__VarD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_StkR) + ocSize);
end;
procedure DoEval__VarD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @Stack[StackPos - SOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PVarR) + ocSize);
end;
procedure DoEval__VarD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], @Stack[StackPos - SOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_VarR) + ocSize);
end;
procedure DoEval__VarD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_StkR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(@VarStack[VarStackPos + VOffsetD], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrR);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetR]);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_VarR) + ocSize);
end;
procedure DoEval__VarD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_StkR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@VarStack[VarStackPos + VOffsetD], PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], PtrL, PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], PtrL, @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_VarR) + ocSize);
end;
procedure DoEval__StkD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetR], PtrL, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_StkR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], PtrL, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SizeOf(Pointer)], PtrL, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__StkD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], @VarStack[VarStackPos + VOffsetL], PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PtrR) + ocSize);
end;
procedure DoEval__StkD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], @VarStack[VarStackPos + VOffsetL], @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_VarR) + ocSize);
end;
procedure DoEval__StkD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetR], @VarStack[VarStackPos + VOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_StkR) + ocSize);
end;
procedure DoEval__StkD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], @VarStack[VarStackPos + VOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PVarR) + ocSize);
end;
procedure DoEval__StkD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SizeOf(Pointer)], @VarStack[VarStackPos + VOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PStkR) + ocSize);
end;
procedure DoEval__StkD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetL], PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PtrR) + ocSize);
end;
procedure DoEval__StkD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetL], @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_VarR) + ocSize);
end;
procedure DoEval__StkD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_StkR) + ocSize);
end;
procedure DoEval__StkD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PVarR) + ocSize);
end;
procedure DoEval__StkD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_VarR) + ocSize);
end;
procedure DoEval__StkD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetR], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_StkR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SizeOf(Pointer)], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SizeOf(Pointer)], PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SizeOf(Pointer)], PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_VarR) + ocSize);
end;
procedure DoEval__StkD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SOffsetL], PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_StkR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - SizeOf(Pointer)], PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@Stack[StackPos - 2*SizeOf(Pointer)], PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PtrL, @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PtrL, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PtrL, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PtrL, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @VarStack[VarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @VarStack[VarStackPos + VOffsetL], @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_VarR) + ocSize);
end;
procedure DoEval__PVarD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @VarStack[VarStackPos + VOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_StkR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @VarStack[VarStackPos + VOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @VarStack[VarStackPos + VOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @Stack[StackPos - SOffsetL], PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @Stack[StackPos - SOffsetL], @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_VarR) + ocSize);
end;
procedure DoEval__PVarD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_StkR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @Stack[StackPos - SOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), @Stack[StackPos - SOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrR);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetR]);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + PDestOffset), PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PStkR) + ocSize);
end;
{$IFDEF Lape_PStkD}
procedure DoEval__PStkD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrL, @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetR])^, PtrL, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrL, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PtrL, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetL], @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_VarR) + ocSize);
end;
procedure DoEval__PStkD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetR])^, @VarStack[VarStackPos + VOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_StkR) + ocSize);
end;
procedure DoEval__PStkD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetL], PtrR);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetL], @VarStack[VarStackPos + VOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_VarR) + ocSize);
end;
procedure DoEval__PStkD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetL], @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_StkR) + ocSize);
end;
procedure DoEval__PStkD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetL], Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetL], PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @VarStack[VarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetR])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^, PtrR);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^, @VarStack[VarStackPos + VOffsetR]);
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - SOffsetL])^, PPointer(@Stack[StackPos - SOffsetL])^, @Stack[StackPos - SOffsetR]);
    Inc(StackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^, Pointer(PtrUInt(PPointer(@VarStack[VarStackPos + VOffsetR])^) + PRightOffset));
    Dec(StackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PStkD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PStkD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PPointer(@Stack[StackPos - 3*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - 2*SizeOf(Pointer)])^, PPointer(@Stack[StackPos - SizeOf(Pointer)])^);
    Dec(StackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PStkD_PStkL_PStkR) + ocSize);
end;
{$ENDIF}
