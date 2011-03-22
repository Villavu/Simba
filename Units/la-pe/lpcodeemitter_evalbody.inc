{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Evaluation bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PtrR(AProc: TLapeEvalProc; APtrD, APtrL, APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PtrL_PtrR));
  with POC_Eval__PtrD_PtrL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    PtrL := APtrL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PtrL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PtrR(AProc: TLapeEvalProc; APtrD, APtrL, APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PtrL_PtrR(AProc,APtrD,APtrL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_VarR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PtrL_VarR));
  with POC_Eval__PtrD_PtrL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PtrL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_VarR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PtrL_VarR(AProc,APtrD,APtrL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_StkR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PtrL_StkR));
  with POC_Eval__PtrD_PtrL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    PtrL := APtrL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PtrL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_StkR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PtrL_StkR(AProc,APtrD,APtrL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PVarR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PtrL_PVarR));
  with POC_Eval__PtrD_PtrL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PtrL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PVarR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PtrL_PVarR(AProc,APtrD,APtrL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PStkR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PtrL_PStkR));
  with POC_Eval__PtrD_PtrL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    PtrL := APtrL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PtrL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PStkR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PtrL_PStkR(AProc,APtrD,APtrL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_VarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_VarL_PtrR));
  with POC_Eval__PtrD_VarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_VarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_VarL_PtrR(AProc,APtrD,AVOffsetL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL, AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_VarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_VarL_VarR));
  with POC_Eval__PtrD_VarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_VarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL, AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_VarL_VarR(AProc,APtrD,AVOffsetL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_VarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_VarL_StkR));
  with POC_Eval__PtrD_VarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_VarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_VarL_StkR(AProc,APtrD,AVOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_VarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_VarL_PVarR));
  with POC_Eval__PtrD_VarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_VarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_VarL_PVarR(AProc,APtrD,AVOffsetL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_VarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_VarL_PStkR));
  with POC_Eval__PtrD_VarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_VarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_VarL_PStkR(AProc,APtrD,AVOffsetL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_StkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_StkL_PtrR));
  with POC_Eval__PtrD_StkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    SOffsetL := ASOffsetL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_StkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_StkL_PtrR(AProc,APtrD,ASOffsetL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_StkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_StkL_VarR));
  with POC_Eval__PtrD_StkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_StkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_StkL_VarR(AProc,APtrD,ASOffsetL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_StkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_StkL_StkR));
  with POC_Eval__PtrD_StkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_StkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_StkL_StkR(AProc,APtrD,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_StkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_StkL_PVarR));
  with POC_Eval__PtrD_StkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_StkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_StkL_PVarR(AProc,APtrD,ASOffsetL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_StkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_StkL_PStkR));
  with POC_Eval__PtrD_StkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    SOffsetL := ASOffsetL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_StkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_StkL_PStkR(AProc,APtrD,ASOffsetL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PVarL_PtrR));
  with POC_Eval__PtrD_PVarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PVarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PVarL_PtrR(AProc,APtrD,AVOffsetL,APLeftOffset,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PVarL_VarR));
  with POC_Eval__PtrD_PVarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PVarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PVarL_VarR(AProc,APtrD,AVOffsetL,APLeftOffset,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PVarL_StkR));
  with POC_Eval__PtrD_PVarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PVarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PVarL_StkR(AProc,APtrD,AVOffsetL,APLeftOffset,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PVarL_PVarR));
  with POC_Eval__PtrD_PVarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PVarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PVarL_PVarR(AProc,APtrD,AVOffsetL,APLeftOffset,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PVarL_PStkR));
  with POC_Eval__PtrD_PVarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PVarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PVarL_PStkR(AProc,APtrD,AVOffsetL,APLeftOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PtrR(AProc: TLapeEvalProc; APtrD, APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PStkL_PtrR));
  with POC_Eval__PtrD_PStkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PStkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PtrR(AProc: TLapeEvalProc; APtrD, APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PStkL_PtrR(AProc,APtrD,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PStkL_VarR));
  with POC_Eval__PtrD_PStkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PStkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PStkL_VarR(AProc,APtrD,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PStkL_StkR));
  with POC_Eval__PtrD_PStkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PStkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PStkL_StkR(AProc,APtrD,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PStkL_PVarR));
  with POC_Eval__PtrD_PStkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PStkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PStkL_PVarR(AProc,APtrD,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PtrD_PStkL_PStkR));
  with POC_Eval__PtrD_PStkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrD := APtrD;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PtrD_PStkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PtrD_PStkL_PStkR(AProc,APtrD,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL, APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PtrL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PtrL_PtrR));
  with POC_Eval__VarD_PtrL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PtrL := APtrL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PtrL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL, APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PtrL_PtrR(AProc,AVOffsetD,APtrL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PtrL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PtrL_VarR));
  with POC_Eval__VarD_PtrL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PtrL_VarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PtrL_VarR(AProc,AVOffsetD,APtrL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PtrL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PtrL_StkR));
  with POC_Eval__VarD_PtrL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PtrL := APtrL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PtrL_StkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PtrL_StkR(AProc,AVOffsetD,APtrL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PtrL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PtrL_PVarR));
  with POC_Eval__VarD_PtrL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PtrL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PtrL_PVarR(AProc,AVOffsetD,APtrL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PtrL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PtrL_PStkR));
  with POC_Eval__VarD_PtrL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PtrL := APtrL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PtrL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PtrL_PStkR(AProc,AVOffsetD,APtrL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_VarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_VarL_PtrR));
  with POC_Eval__VarD_VarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_VarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_VarL_PtrR(AProc,AVOffsetD,AVOffsetL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL, AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_VarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_VarL_VarR));
  with POC_Eval__VarD_VarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_VarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL, AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_VarL_VarR(AProc,AVOffsetD,AVOffsetL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_VarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_VarL_StkR));
  with POC_Eval__VarD_VarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_VarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_VarL_StkR(AProc,AVOffsetD,AVOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_VarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_VarL_PVarR));
  with POC_Eval__VarD_VarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_VarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_VarL_PVarR(AProc,AVOffsetD,AVOffsetL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_VarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_VarL_PStkR));
  with POC_Eval__VarD_VarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_VarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_VarL_PStkR(AProc,AVOffsetD,AVOffsetL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_StkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_StkL_PtrR));
  with POC_Eval__VarD_StkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    SOffsetL := ASOffsetL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_StkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_StkL_PtrR(AProc,AVOffsetD,ASOffsetL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_StkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_StkL_VarR));
  with POC_Eval__VarD_StkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_StkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_StkL_VarR(AProc,AVOffsetD,ASOffsetL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_StkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_StkL_StkR));
  with POC_Eval__VarD_StkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_StkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_StkL_StkR(AProc,AVOffsetD,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_StkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_StkL_PVarR));
  with POC_Eval__VarD_StkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_StkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_StkL_PVarR(AProc,AVOffsetD,ASOffsetL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_StkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_StkL_PStkR));
  with POC_Eval__VarD_StkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    SOffsetL := ASOffsetL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_StkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_StkL_PStkR(AProc,AVOffsetD,ASOffsetL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PVarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PVarL_PtrR));
  with POC_Eval__VarD_PVarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PVarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PVarL_PtrR(AProc,AVOffsetD,AVOffsetL,APLeftOffset,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PVarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PVarL_VarR));
  with POC_Eval__VarD_PVarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PVarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PVarL_VarR(AProc,AVOffsetD,AVOffsetL,APLeftOffset,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PVarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PVarL_StkR));
  with POC_Eval__VarD_PVarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PVarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PVarL_StkR(AProc,AVOffsetD,AVOffsetL,APLeftOffset,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PVarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PVarL_PVarR));
  with POC_Eval__VarD_PVarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PVarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PVarL_PVarR(AProc,AVOffsetD,AVOffsetL,APLeftOffset,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PVarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PVarL_PStkR));
  with POC_Eval__VarD_PVarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PVarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PVarL_PStkR(AProc,AVOffsetD,AVOffsetL,APLeftOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PStkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PStkL_PtrR));
  with POC_Eval__VarD_PStkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PStkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PStkL_PtrR(AProc,AVOffsetD,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PStkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PStkL_VarR));
  with POC_Eval__VarD_PStkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PStkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PStkL_VarR(AProc,AVOffsetD,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PStkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PStkL_StkR));
  with POC_Eval__VarD_PStkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PStkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PStkL_StkR(AProc,AVOffsetD,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PStkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PStkL_PVarR));
  with POC_Eval__VarD_PStkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PStkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PStkL_PVarR(AProc,AVOffsetD,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_VarD_PStkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__VarD_PStkL_PStkR));
  with POC_Eval__VarD_PStkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
  end;
  Inc(Offset, SizeOf(TOC_Eval__VarD_PStkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__VarD_PStkL_PStkR(AProc,AVOffsetD,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PtrR(AProc: TLapeEvalProc; APtrL, APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PtrL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PtrL_PtrR));
  with POC_Eval__StkD_PtrL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PtrL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PtrR(AProc: TLapeEvalProc; APtrL, APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PtrL_PtrR(AProc,APtrL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_VarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PtrL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PtrL_VarR));
  with POC_Eval__StkD_PtrL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PtrL_VarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_VarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PtrL_VarR(AProc,APtrL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_StkR(AProc: TLapeEvalProc; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PtrL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PtrL_StkR));
  with POC_Eval__StkD_PtrL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PtrL_StkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_StkR(AProc: TLapeEvalProc; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PtrL_StkR(AProc,APtrL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PVarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PtrL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PtrL_PVarR));
  with POC_Eval__StkD_PtrL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PtrL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PVarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PtrL_PVarR(AProc,APtrL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PStkR(AProc: TLapeEvalProc; APtrL: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PtrL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PtrL_PStkR));
  with POC_Eval__StkD_PtrL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PtrL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PStkR(AProc: TLapeEvalProc; APtrL: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PtrL_PStkR(AProc,APtrL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_VarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_VarL_PtrR));
  with POC_Eval__StkD_VarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_VarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_VarL_PtrR(AProc,AVOffsetL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_VarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_VarL_VarR));
  with POC_Eval__StkD_VarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_VarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_VarL_VarR(AProc,AVOffsetL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_VarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_VarL_StkR));
  with POC_Eval__StkD_VarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_VarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_VarL_StkR(AProc,AVOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_VarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_VarL_PVarR));
  with POC_Eval__StkD_VarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_VarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_VarL_PVarR(AProc,AVOffsetL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_VarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_VarL_PStkR));
  with POC_Eval__StkD_VarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_VarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_VarL_PStkR(AProc,AVOffsetL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PtrR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_StkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_StkL_PtrR));
  with POC_Eval__StkD_StkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_StkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PtrR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_StkL_PtrR(AProc,ASOffsetL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_VarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_StkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_StkL_VarR));
  with POC_Eval__StkD_StkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_StkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_VarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_StkL_VarR(AProc,ASOffsetL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_StkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_StkL_StkR));
  with POC_Eval__StkD_StkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_StkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_StkL_StkR(AProc,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PVarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_StkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_StkL_PVarR));
  with POC_Eval__StkD_StkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_StkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PVarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_StkL_PVarR(AProc,ASOffsetL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PStkR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_StkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_StkL_PStkR));
  with POC_Eval__StkD_StkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_StkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PStkR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_StkL_PStkR(AProc,ASOffsetL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PVarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PVarL_PtrR));
  with POC_Eval__StkD_PVarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PVarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PVarL_PtrR(AProc,AVOffsetL,APLeftOffset,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PVarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PVarL_VarR));
  with POC_Eval__StkD_PVarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PVarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PVarL_VarR(AProc,AVOffsetL,APLeftOffset,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PVarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PVarL_StkR));
  with POC_Eval__StkD_PVarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PVarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PVarL_StkR(AProc,AVOffsetL,APLeftOffset,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PVarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PVarL_PVarR));
  with POC_Eval__StkD_PVarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PVarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PVarL_PVarR(AProc,AVOffsetL,APLeftOffset,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PVarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PVarL_PStkR));
  with POC_Eval__StkD_PVarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PVarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PVarL_PStkR(AProc,AVOffsetL,APLeftOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PtrR(AProc: TLapeEvalProc; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PStkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PStkL_PtrR));
  with POC_Eval__StkD_PStkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PStkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PtrR(AProc: TLapeEvalProc; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PStkL_PtrR(AProc,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PStkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PStkL_VarR));
  with POC_Eval__StkD_PStkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PStkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PStkL_VarR(AProc,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PStkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PStkL_StkR));
  with POC_Eval__StkD_PStkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PStkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PStkL_StkR(AProc,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PStkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PStkL_PVarR));
  with POC_Eval__StkD_PStkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PStkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PStkL_PVarR(AProc,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PStkR(AProc: TLapeEvalProc; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_StkD_PStkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__StkD_PStkL_PStkR));
  with POC_Eval__StkD_PStkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__StkD_PStkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PStkR(AProc: TLapeEvalProc; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__StkD_PStkL_PStkR(AProc,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL, APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PtrL_PtrR));
  with POC_Eval__PVarD_PtrL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    PtrL := APtrL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PtrL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL, APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PtrL_PtrR(AProc,AVOffsetD,APDestOffset,APtrL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PtrL_VarR));
  with POC_Eval__PVarD_PtrL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PtrL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PtrL_VarR(AProc,AVOffsetD,APDestOffset,APtrL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PtrL_StkR));
  with POC_Eval__PVarD_PtrL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    PtrL := APtrL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PtrL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PtrL_StkR(AProc,AVOffsetD,APDestOffset,APtrL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PtrL_PVarR));
  with POC_Eval__PVarD_PtrL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PtrL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PtrL_PVarR(AProc,AVOffsetD,APDestOffset,APtrL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PtrL_PStkR));
  with POC_Eval__PVarD_PtrL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    PtrL := APtrL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PtrL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PtrL_PStkR(AProc,AVOffsetD,APDestOffset,APtrL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_VarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_VarL_PtrR));
  with POC_Eval__PVarD_VarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_VarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_VarL_PtrR(AProc,AVOffsetD,APDestOffset,AVOffsetL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL, AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_VarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_VarL_VarR));
  with POC_Eval__PVarD_VarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_VarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL, AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_VarL_VarR(AProc,AVOffsetD,APDestOffset,AVOffsetL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_VarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_VarL_StkR));
  with POC_Eval__PVarD_VarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_VarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_VarL_StkR(AProc,AVOffsetD,APDestOffset,AVOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_VarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_VarL_PVarR));
  with POC_Eval__PVarD_VarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_VarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_VarL_PVarR(AProc,AVOffsetD,APDestOffset,AVOffsetL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_VarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_VarL_PStkR));
  with POC_Eval__PVarD_VarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_VarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_VarL_PStkR(AProc,AVOffsetD,APDestOffset,AVOffsetL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_StkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_StkL_PtrR));
  with POC_Eval__PVarD_StkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    SOffsetL := ASOffsetL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_StkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_StkL_PtrR(AProc,AVOffsetD,APDestOffset,ASOffsetL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_StkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_StkL_VarR));
  with POC_Eval__PVarD_StkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_StkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_StkL_VarR(AProc,AVOffsetD,APDestOffset,ASOffsetL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_StkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_StkL_StkR));
  with POC_Eval__PVarD_StkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_StkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_StkL_StkR(AProc,AVOffsetD,APDestOffset,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_StkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_StkL_PVarR));
  with POC_Eval__PVarD_StkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_StkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_StkL_PVarR(AProc,AVOffsetD,APDestOffset,ASOffsetL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_StkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_StkL_PStkR));
  with POC_Eval__PVarD_StkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    SOffsetL := ASOffsetL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_StkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_StkL_PStkR(AProc,AVOffsetD,APDestOffset,ASOffsetL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PVarL_PtrR));
  with POC_Eval__PVarD_PVarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PVarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PVarL_PtrR(AProc,AVOffsetD,APDestOffset,AVOffsetL,APLeftOffset,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PVarL_VarR));
  with POC_Eval__PVarD_PVarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PVarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PVarL_VarR(AProc,AVOffsetD,APDestOffset,AVOffsetL,APLeftOffset,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PVarL_StkR));
  with POC_Eval__PVarD_PVarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PVarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PVarL_StkR(AProc,AVOffsetD,APDestOffset,AVOffsetL,APLeftOffset,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PVarL_PVarR));
  with POC_Eval__PVarD_PVarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PVarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PVarL_PVarR(AProc,AVOffsetD,APDestOffset,AVOffsetL,APLeftOffset,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PVarL_PStkR));
  with POC_Eval__PVarD_PVarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PVarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PVarL_PStkR(AProc,AVOffsetD,APDestOffset,AVOffsetL,APLeftOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PStkL_PtrR));
  with POC_Eval__PVarD_PStkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PStkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PStkL_PtrR(AProc,AVOffsetD,APDestOffset,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PStkL_VarR));
  with POC_Eval__PVarD_PStkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PStkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PStkL_VarR(AProc,AVOffsetD,APDestOffset,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PStkL_StkR));
  with POC_Eval__PVarD_PStkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PStkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PStkL_StkR(AProc,AVOffsetD,APDestOffset,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PStkL_PVarR));
  with POC_Eval__PVarD_PStkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PStkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PStkL_PVarR(AProc,AVOffsetD,APDestOffset,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PVarD_PStkL_PStkR));
  with POC_Eval__PVarD_PStkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetD := AVOffsetD;
    PDestOffset := APDestOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PVarD_PStkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PVarD_PStkL_PStkR(AProc,AVOffsetD,APDestOffset,o,Pos); end;
{$IFDEF Lape_PStkD}
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_PtrR(AProc: TLapeEvalProc; APtrL, APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PtrL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PtrL_PtrR));
  with POC_Eval__PStkD_PtrL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PtrL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_PtrR(AProc: TLapeEvalProc; APtrL, APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PtrL_PtrR(AProc,APtrL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_VarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PtrL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PtrL_VarR));
  with POC_Eval__PStkD_PtrL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PtrL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_VarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PtrL_VarR(AProc,APtrL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_StkR(AProc: TLapeEvalProc; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PtrL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PtrL_StkR));
  with POC_Eval__PStkD_PtrL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PtrL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_StkR(AProc: TLapeEvalProc; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PtrL_StkR(AProc,APtrL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_PVarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PtrL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PtrL_PVarR));
  with POC_Eval__PStkD_PtrL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PtrL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_PVarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PtrL_PVarR(AProc,APtrL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_PStkR(AProc: TLapeEvalProc; APtrL: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PtrL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PtrL_PStkR));
  with POC_Eval__PStkD_PtrL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrL := APtrL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PtrL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PtrL_PStkR(AProc: TLapeEvalProc; APtrL: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PtrL_PStkR(AProc,APtrL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_VarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_VarL_PtrR));
  with POC_Eval__PStkD_VarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_VarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_VarL_PtrR(AProc,AVOffsetL,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_VarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_VarL_VarR));
  with POC_Eval__PStkD_VarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_VarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_VarL_VarR(AProc,AVOffsetL,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_VarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_VarL_StkR));
  with POC_Eval__PStkD_VarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_VarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_VarL_StkR(AProc,AVOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_VarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_VarL_PVarR));
  with POC_Eval__PStkD_VarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_VarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_VarL_PVarR(AProc,AVOffsetL,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_VarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_VarL_PStkR));
  with POC_Eval__PStkD_VarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_VarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_VarL_PStkR(AProc,AVOffsetL,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_PtrR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_StkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_StkL_PtrR));
  with POC_Eval__PStkD_StkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    PtrR := APtrR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_StkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_PtrR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_StkL_PtrR(AProc,ASOffsetL,APtrR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_VarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_StkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_StkL_VarR));
  with POC_Eval__PStkD_StkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_StkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_VarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_StkL_VarR(AProc,ASOffsetL,AVOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_StkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_StkL_StkR));
  with POC_Eval__PStkD_StkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_StkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_StkL_StkR(AProc,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_PVarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_StkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_StkL_PVarR));
  with POC_Eval__PStkD_StkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_StkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_PVarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_StkL_PVarR(AProc,ASOffsetL,AVOffsetR,APRightOffset,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_PStkR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_StkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_StkL_PStkR));
  with POC_Eval__PStkD_StkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_StkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_StkL_PStkR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_StkL_PStkR(AProc,ASOffsetL,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PVarL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PVarL_PtrR));
  with POC_Eval__PStkD_PVarL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PVarL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PVarL_PtrR(AProc,AVOffsetL,APLeftOffset,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PVarL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PVarL_VarR));
  with POC_Eval__PStkD_PVarL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PVarL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PVarL_VarR(AProc,AVOffsetL,APLeftOffset,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PVarL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PVarL_StkR));
  with POC_Eval__PStkD_PVarL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PVarL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PVarL_StkR(AProc,AVOffsetL,APLeftOffset,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PVarL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PVarL_PVarR));
  with POC_Eval__PStkD_PVarL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PVarL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PVarL_PVarR(AProc,AVOffsetL,APLeftOffset,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PVarL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PVarL_PStkR));
  with POC_Eval__PStkD_PVarL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetL := AVOffsetL;
    PLeftOffset := APLeftOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PVarL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PVarL_PStkR(AProc,AVOffsetL,APLeftOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_PtrR(AProc: TLapeEvalProc; APtrR: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PStkL_PtrR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PStkL_PtrR));
  with POC_Eval__PStkD_PStkL_PtrR(@FCode[Offset])^ do begin
    Proc := AProc;
    PtrR := APtrR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PStkL_PtrR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_PtrR(AProc: TLapeEvalProc; APtrR: Pointer; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PStkL_PtrR(AProc,APtrR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PStkL_VarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PStkL_VarR));
  with POC_Eval__PStkD_PStkL_VarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetR := AVOffsetR;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PStkL_VarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PStkL_VarR(AProc,AVOffsetR,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PStkL_StkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PStkL_StkR));
  with POC_Eval__PStkD_PStkL_StkR(@FCode[Offset])^ do begin
    Proc := AProc;
    SOffsetL := ASOffsetL;
    SOffsetR := ASOffsetR;
    StackInc := AStackInc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PStkL_StkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PStkL_StkR(AProc,ASOffsetL,ASOffsetR,AStackInc,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PStkL_PVarR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PStkL_PVarR));
  with POC_Eval__PStkD_PStkL_PVarR(@FCode[Offset])^ do begin
    Proc := AProc;
    VOffsetR := AVOffsetR;
    PRightOffset := APRightOffset;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PStkL_PVarR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PStkL_PVarR(AProc,AVOffsetR,APRightOffset,o,Pos); end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_PStkR(AProc: TLapeEvalProc; var Offset: Integer; Pos: PDocPos = nil): Integer; begin
  Result := _op(ocEval_PStkD_PStkL_PStkR, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_Eval__PStkD_PStkL_PStkR));
  with POC_Eval__PStkD_PStkL_PStkR(@FCode[Offset])^ do begin
    Proc := AProc;
  end;
  Inc(Offset, SizeOf(TOC_Eval__PStkD_PStkL_PStkR));
end;
function TLapeCodeEmitterBase._Eval__PStkD_PStkL_PStkR(AProc: TLapeEvalProc; Pos: PDocPos = nil): Integer; var o: Integer; begin o := -1; Result := _Eval__PStkD_PStkL_PStkR(AProc,o,Pos); end;
{$ENDIF}

