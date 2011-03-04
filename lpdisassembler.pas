{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
	
	Bytecode disassembler.
}
unit lpdisassembler;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpinterpreter;

procedure DisassembleCode(Code: PByte; PointerNames: TLapeDeclArray = nil); {$IFDEF Lape_Inline}inline;{$ENDIF}

implementation

uses
  lpexceptions, lpeval, lpvartypes;

type
  TPMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<string>;
procedure DisassembleCode(Code: PByte; PointerNames: TLapeDeclArray = nil);
var
  pMap: TPMap;
  op: EOperator;
  t1, t2: ELapeBaseType;
  proc: TLapeEvalProc;
  i: Integer;
  {$IFDEF Lape_EmitPos}p: TDocPos;{$ENDIF}

  function IntToStr(i: Int64): string; overload;
  begin
    Result := SysUtils.IntToStr(i);
  end;

  function IntToStr(p: Pointer): string; overload;
  var s: string;
  begin
    if (p = nil) then
      Result := 'nil'
    else
    begin
      s := IntToStr(PtrUInt(p));
      if (pMap <> nil) and (pMap.ExistsItemI(s)) then
        Result := pMap[s]
      else
        Result := '$' + s;
    end;
  end;

  procedure _WriteLn(s: string); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    WriteLn(IntToStr(Code) + ' :: ' + s);
  end;

  procedure _WriteLn(s: string; args: array of const); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn(Format(s, args));
  end;

  function getTIMemPosPtr(p: TIMemPos): string; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    case p.MemPos of
      mpMem: Result := IntToStr(p.Ptr);
      mpVar: Result := 'VarStack[VarStackPos + ' + IntToStr(p.VOffset) + ']';
      mpStack: Result := 'Stack[StackPos - ' + IntToStr(p.SOffset) + ']';
      else LapeException(lpeImpossible);
    end;
    if p.isPointer then
      Result := '(' + Result + ')^+'+IntToStr(p.POffset);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitStackLen %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoInitVarLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitVarStackLen %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitStack %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('ExpandVarStack %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('ExpandVarStackAndInit %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('GrowVarStack %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('GrowVarStackAndInit %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoPopVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('PopVarStack %d', [PUInt16(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('IncTry %d', [PInt32(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(Int32) + ocSize);
  end;

  procedure DoDecTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DecTry');
    Inc(Code, ocSize);
  end;

  procedure DoEndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('EndTry');
    Inc(Code, ocSize);
  end;

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('CatchException');
    Inc(Code, ocSize);
  end;

  procedure DoIncCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_IncCall(PtrUInt(Code) + ocSize)^ do
      _WriteLn('IncCall %s %d', [GetTIMemPosPtr(CodePos), ParamSize]);
    Inc(Code, SizeOf(TOC_IncCall) + ocSize);
  end;

  procedure DoDecCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DecCall');
    Inc(Code, ocSize);
  end;

  procedure DoDecCall_EndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DecCall_EndTry');
    Inc(Code, ocSize);
  end;

  procedure DoInvokeExternalProc; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_InvokeExternalProc(PtrUInt(Code) + ocSize)^ do
    begin
      _WriteLn('InvokeExternalProc %s %d', [GetTIMemPosPtr(MemPos), ParamLen]);
      _WriteLn('DecStackPos %d', [ParamLen]);
    end;
    Inc(Code, SizeOf(TOC_InvokeExternalProc) + ocSize)
  end;

  procedure DoInvokeExternalFunc; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_InvokeExternalFunc(PtrUInt(Code) + ocSize)^ do
    begin
      _WriteLn('InvokeExternalFunc %s %d %s', [GetTIMemPosPtr(MemPos), ParamLen,getTIMemPosPtr(ResPos)]);
      _WriteLn('DecStackPos %d', [ParamLen]);
    end;
    Inc(Code, SizeOf(TOC_InvokeExternalFunc) + ocSize)
  end;

  {$I lpdisassembler_dojump.inc}
  {$I lpdisassembler_doeval.inc}

begin
  {$IFDEF Lape_EmitPos}
  p.Line := 0;
  p.Col := 0;
  {$ENDIF}

  pMap := TPMap.Create('');
  try
    for op := Low(EOperator) to High(EOperator) do
    begin
      if (op_name[op] = '') then
        Continue;

      for t1 := Low(ELapeBaseType) to High(ELapeBaseType) do
        for t2 := Low(ELapeBaseType) to High(ELapeBaseType) do
        begin
          proc := getEvalProc(op, t1, t2);
          if ({$IFNDEF FPC}@{$ENDIF}proc <> nil) and ({$IFNDEF FPC}@{$ENDIF}proc <>{$IFNDEF FPC}@{$ENDIF}LapeEvalErrorProc) then
            if (t2 = ltUnknown) then
              pMap[IntToStr(PtrUInt(proc))] := 'lpe'+LapeTypeToString(t1)+'_'+op_name[op]
            else
              pMap[IntToStr(PtrUInt(proc))] := 'lpe'+LapeTypeToString(t1)+'_'+op_name[op]+'_'+LapeTypeToString(t2);
        end;
    end;
    pMap[IntToStr(PtrUInt(getEvalProc(op_Addr, ltUnknown, ltUnknown)))] := 'lpeAddr';
    pMap[IntToStr(PtrUInt(getEvalProc(op_Deref, ltUnknown, ltUnknown)))] := 'lpeDeref';


    for i := 0 to High(PointerNames) do
      if (PointerNames[i].Name = '') and (PointerNames[i] is TLapeGlobalVar) then
        pMap[IntToStr(PtrUInt(TLapeGlobalVar(PointerNames[i]).Ptr))] := TLapeGlobalVar(PointerNames[i]).AsString
      else if (PointerNames[i] is TLapeGlobalVar) then
        pMap[IntToStr(PtrUInt(TLapeGlobalVar(PointerNames[i]).Ptr))] := PointerNames[i].Name
      else
        pMap[IntToStr(PtrUInt(PointerNames[i]))] := PointerNames[i].Name;


    while True do
    begin
      {$IFDEF Lape_EmitPos}
      with PDocPos(PtrUInt(Code) + SizeOf(opCodeType))^ do
        if (p.FileName <> FileName) or (p.Line <> Line) or (p.Col <> Col) then
        begin
          p.FileName := FileName;
          p.Line := Line;
          p.Col := Col;
          _WriteLn('--> File "'+FileName+'", Line '+intToStr(Line)+', Col '+IntToStr(Col)+'   ');
        end;
      {$ENDIF}
      {$I lpinterpreter_opcodecase.inc}
    end;
  except
    on E: Exception do
      LapeException(lpeRuntime, [E.Message] {$IFDEF Lape_EmitPos}, PDocPos(PtrUInt(Code) + SizeOf(opCodeType))^ {$ENDIF});
  end;
  pMap.Free();
end;

end.

