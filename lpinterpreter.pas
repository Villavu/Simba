{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Opcodes and bytecode interpreter.
}
unit lpinterpreter;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

type
  opCode = (
    ocNone,
    ocInitStackLen,                                            //InitStackLen TStackOffset
    ocInitVarLen,                                              //InitVarLen TStackOffset
    ocInitStack,                                               //InitStack TStackOffset
    ocGrowStack,                                               //GrowStack TStackOffset
    ocExpandVar,                                               //ExpandVar TStackOffset
    ocExpandVarAndInit,                                        //ExpandVarAndInit TStackOffset
    ocGrowVar,                                                 //GrowVar TStackOffset
    ocGrowVarAndInit,                                          //GrowVarAndInit TStackOffset
    ocPopVar,                                                  //PopVar TStackOffset
    ocJmpSafe,                                                 //JmpSafe TCodePos
    ocJmpSafeR,                                                //JmpSafeR TCodeOffset

    ocIncTry,                                                  //IncTry TCodeOffset UInt32
    ocDecTry,                                                  //DecTry
    ocEndTry,                                                  //EndTry
    ocCatchException,                                          //CatchException

    ocDecCall,                                                 //DecCall
    ocDecCall_EndTry,                                          //DecCall_EndTry

    {$I lpinterpreter_invokeopcodes.inc}
    {$I lpinterpreter_jumpopcodes.inc}
    {$I lpinterpreter_evalopcodes.inc}
  );
  opCodeTypeP = ^opCodeType;
  {$IFDEF Lape_SmallCode}
  opCodeType = Byte;
  {$ELSE}
  opCodeType = Integer; //Better alignment
  {$ENDIF}

  TCallRec = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    CalledFrom: PByte;
    StackP, VarStackP: UInt32;
  end;

  POC_IncTry = ^TOC_IncTry;
  TOC_IncTry = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Jmp: TCodeOffset;
    JmpFinally: UInt32;
  end;

  {$I lpinterpreter_invokerecords.inc}
  {$I lpinterpreter_jumprecords.inc}
  {$I lpinterpreter_evalrecords.inc}

const
  Try_NoFinally: UInt32 = UInt32(-1);
  Try_NoExcept: UInt32 = UInt32(-2);
  EndJump: TCodePos = TCodePos(-1);


  {$IFDEF Lape_UnlimitedStackSize}
  StackSize = 512 * SizeOf(Pointer); //bytes
  {$ELSE}
  StackSize = 2048 * SizeOf(Pointer); //bytes
  {$ENDIF}

  {$IFDEF Lape_UnlimitedVarStackSize}
  VarStackSize = 256 * SizeOf(Pointer); //bytes
  {$ELSE}
  VarStackSize = 4096 * SizeOf(Pointer); //bytes
  {$ENDIF}

  {$IFDEF Lape_UnlimitedTryStackSize}
  TryStackSize = 128; //pointers
  {$ELSE}
  TryStackSize = 1024; //pointers
  {$ENDIF}

  {$IFDEF Lape_UnlimitedCallStackSize}
  CallStackSize = 128; //TCallRecs
  {$ELSE}
  CallStackSize = 512; //TCallRecs
  {$ENDIF}

  ocSize = SizeOf(opCodeType) {$IFDEF Lape_EmitPos}+SizeOf(TDocPos){$ENDIF};

procedure _LapeHigh(Params: PParamArray; Result: Pointer);
procedure _LapeLength(Params: PParamArray; Result: Pointer);
procedure _LapeStrLen(Params: PParamArray; Result: Pointer);
procedure RunCode(Code: PByte); {$IFDEF Lape_Inline}inline;{$ENDIF}

implementation

uses
  lpexceptions;

procedure _LapeHigh(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := High(PCodeArray(Params^[0])^);
end;

procedure _LapeLength(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := Length(PCodeArray(Params^[0])^);
end;

procedure _LapeStrLen(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := Length(PlpString(Params^[0])^);
end;

procedure RunCode(Code: PByte);
const
  opNone: opCodeType = opCodeType(ocNone);
var
  CodeBase: PByte;
  Stack: array of Byte;
  StackPos: UInt32;

  VarStack: array of Byte;
  VarStackPos, VarStackLen: UInt32;

  TryStack: array of record
    Jmp: PByte;
    JmpFinally: PByte;
  end;
  TryStackPos: UInt32;
  InException: Exception;
  InSafeJump: PByte;

  CallStack: array of TCallRec;
  CallStackPos: UInt32;

  procedure JumpTo(const Target: TCodePos); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Code := PByte(PtrUInt(CodeBase) + Target);
  end;

  procedure JumpToRelative(const Offset: TCodeOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Inc(Code, Offset);
  end;

  procedure HandleException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (TryStackPos > 0) then
    begin
      Dec(TryStackPos);
      Code := TryStack[TryStackPos].Jmp;
    end
    else
      raise InException;
  end;

  procedure HandleSafeJump; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    while (TryStackPos > 0) and (TryStack[TryStackPos - 1].Jmp < InSafeJump) and (TryStack[TryStackPos - 1].JmpFinally = nil) do
      Dec(TryStackPos);

    if (TryStackPos > 0) and (TryStack[TryStackPos - 1].Jmp < InSafeJump) and  (TryStack[TryStackPos - 1].JmpFinally <> nil) then
    begin
      Assert(TryStack[TryStackPos - 1].JmpFinally >= Code);
      Dec(TryStackPos);
      Code := TryStack[TryStackPos].JmpFinally;
    end
    else if (CodeBase = PByte(PtrUInt(InSafeJump) - EndJump)) then
      Code := @opNone
    else
    begin
      Code := InSafeJump;
      InSafeJump := nil;
    end;
  end;

  procedure PushToVar(const Size: TStackOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(StackPos, Size);
    VarStackPos := VarStackLen;
    Inc(VarStackLen, Size);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    Move(Stack[StackPos], VarStack[VarStackPos], Size);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    SetLength(Stack, PStackOffset(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitVarLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    SetLength(VarStack, PStackOffset(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    o: TStackOffset;
  begin
    o := PStackOffset(PtrUInt(Code) + ocSize)^;
    {$IFDEF Lape_UnlimitedStackSize}
    if (StackPos + o > Length(VarStack)) then
      SetLength(Stack, StackPos + o + (StackSize div 2));
    {$ENDIF}
    FillChar(Stack[StackPos], o, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    o: TStackOffset;
  begin
    o := PStackOffset(PtrUInt(Code) + ocSize)^;
    {$IFDEF Lape_UnlimitedStackSize}
    if (StackPos + o > Length(VarStack)) then
      SetLength(Stack, StackPos + o + (StackSize div 2));
    {$ENDIF}
    Inc(StackPos, o);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    VarStackPos := VarStackLen;
    Inc(VarStackLen, PStackOffset(PtrUInt(Code) + ocSize)^);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    a: Integer;
  begin
    a := PStackOffset(PtrUInt(Code) + ocSize)^;
    VarStackPos := VarStackLen;
    Inc(VarStackLen, a);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    FillChar(VarStack[VarStackPos], a, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Inc(VarStackLen, PStackOffset(PtrUInt(Code) + ocSize)^);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    a, b: UInt32;
  begin
    a := PStackOffset(PtrUInt(Code) + ocSize)^;
    b := VarStackLen;
    Inc(VarStackLen, a);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    FillChar(VarStack[b], a, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoPopVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    a: Integer;
  begin
    a := PStackOffset(PtrUInt(Code) + ocSize)^;
    Dec(VarStackPos, a);
    Dec(VarStackLen, a);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoJmpSafe; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    InSafeJump := PByte(PtrUInt(CodeBase) + PCodePos(PtrUInt(Code) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafeR; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    InSafeJump := PByte(PtrInt(Code) + PCodeOffset(PtrUInt(Code) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    {$IFDEF Lape_UnlimitedTryStackSize}
    if (TryStackPos >= Length(TryStack)) then
      SetLength(TryStack, TryStackPos + (TryStackSize div 2));
    {$ENDIF}

    with POC_IncTry(PtrUInt(Code) + ocSize)^ do
    begin
      TryStack[TryStackPos].Jmp := PByte(PtrInt(Code) + Jmp);
      if (JmpFinally = Try_NoFinally) then
        TryStack[TryStackPos].JmpFinally := nil
      else if (JmpFinally = Try_NoExcept) then
        TryStack[TryStackPos].JmpFinally := TryStack[TryStackPos].Jmp
      else
        TryStack[TryStackPos].JmpFinally := PByte(PtrUInt(Code) + JmpFinally + Jmp);
    end;

    Inc(TryStackPos);
    Inc(Code, ocSize + SizeOf(TOC_IncTry));
  end;

  procedure DoDecTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(TryStackPos);
    Inc(Code, ocSize);
  end;

  procedure DoEndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (InException <> nil) then
      HandleException()
    else if (InSafeJump <> nil) then
      HandleSafeJump()
    else
      Inc(Code, ocSize);
  end;

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    InException.Free();
    InException := nil;
    Inc(Code, ocSize);
  end;

  procedure DoIncCall(RecSize: Integer; Jmp: TCodePos; ParamSize: TParamSize; StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    {$IFDEF Lape_UnlimitedCallStackSize}
    if (CallStackPos >= Length(CallStack)) then
      SetLength(CallStack, CallStackPos + (CallStackSize div 2));
    {$ENDIF}
    with CallStack[CallStackPos] do
    begin
      CalledFrom := PByte(PtrUInt(Code) + ocSize + RecSize);
      VarStackP := VarStackPos;
      PushToVar(ParamSize);
      StackP := StackPos + StackPosOffset;
      JumpTo(Jmp);
      Inc(CallStackPos);
    end;
  end;

  procedure DoDecCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (CallStackPos <= 0) then
      Code := @opNone
    else
    begin
      Dec(CallStackPos);
      with CallStack[CallStackPos] do
      begin
        Code := CalledFrom;
        VarStackPos := VarStackP;
        Assert(StackPos = StackP);
        Assert(VarStackPos = VarStackP);
      end;
    end;
  end;

  procedure DoDecCall_EndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    DoDecCall();
    if (InException <> nil) then
      HandleException()
    else if (InSafeJump <> nil) then
      HandleSafeJump();
  end;

  procedure DoInvokeImportedProc(RecSize: Integer; Ptr: Pointer; ParamSize: UInt16; StackPosOffset: Integer = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedProc(Ptr)(@Stack[StackPos - ParamSize]);
    Dec(StackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  procedure DoInvokeImportedFunc(RecSize: Integer; Ptr, Res: Pointer; ParamSize: UInt16; StackPosOffset: Integer = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedFunc(Ptr)(@Stack[StackPos - ParamSize], Res);
    Dec(StackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  {$I lpinterpreter_doinvoke.inc}
  {$I lpinterpreter_dojump.inc}
  {$I lpinterpreter_doeval.inc}

  procedure DaLoop; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    try
      while True do {$I lpinterpreter_opcodecase.inc}
    except
      InException := Exception(AcquireExceptionObject());
      HandleException();
      DaLoop();
    end;
  end;

begin
  CodeBase := Code;
  SetLength(Stack, StackSize);
  SetLength(VarStack, VarStackSize);
  SetLength(TryStack, TryStackSize);
  SetLength(CallStack, CallStackSize);

  try
    Code := CodeBase;
    StackPos := 0;
    VarStackPos := 0;
    VarStackLen := 0;
    TryStackPos := 0;
    CallStackpos := 0;
    InException := nil;
    InSafeJump := nil;

    DaLoop();
  except
    on E: Exception do
      LapeException(lpeRuntime, [E.Message] {$IFDEF Lape_EmitPos}, PDocPos(PtrUInt(Code) + SizeOf(opCodeType))^ {$ENDIF});
  end;
end;

end.

