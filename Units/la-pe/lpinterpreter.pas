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
    ocInitStackLen,                                            //InitStackLen UInt16
    ocInitVarLen,                                              //InitVarLen UInt16
    ocInitStack,                                               //InitStack UInt16
    ocExpandVar,                                               //ExpandVar UInt16
    ocExpandVarAndInit,                                        //ExpandVarAndInit UInt16
    ocGrowVar,                                                 //GrowVar UInt16
    ocGrowVarAndInit,                                          //GrowVarAndInit UInt16
    ocPopVar,                                                  //PopVar UInt16

    ocIncTry,                                                  //IncTry Int32
    ocDecTry,                                                  //DecTry
    ocEndTry,                                                  //EndTry
    ocCatchException,                                          //CatchException

    ocIncCall,                                                 //IncCall TIMemPos UInt16
    ocDecCall,                                                 //DecCall
    ocDecCall_EndTry,                                          //DecCall_EndTry

    ocInvokeExternalProc,                                      //InvokeExternalProc TIMemPos UInt16
    ocInvokeExternalFunc,                                      //InvokeExternalFunc TIMemPos TIMemPos UInt16

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

  POC_IncCall = ^TOC_IncCall;
  TOC_IncCall = record
    CodePos: TIMemPos;
    ParamSize: UInt16;
  end;

  POC_InvokeExternalProc = ^TOC_InvokeExternalProc;
  TOC_InvokeExternalProc = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    MemPos: TIMemPos;
    ParamLen: UInt16;
  end;

  POC_InvokeExternalFunc = ^TOC_InvokeExternalFunc;
  TOC_InvokeExternalFunc = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    MemPos: TIMemPos;
    ResPos: TIMemPos;
    ParamLen: UInt16;
  end;

  {$I lpinterpreter_jumprecords.inc}
  {$I lpinterpreter_evalrecords.inc}

const
  StackSize = 2048 * SizeOf(Pointer); //bytes

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

procedure RunCode(Code: PByte); {$IFDEF Lape_Inline}inline;{$ENDIF}

implementation

uses
  lpexceptions;

procedure RunCode(Code: PByte);
const
  opNone: opCodeType = opCodeType(ocNone);
var
  CodeBase: PByte;
  Stack: array of Byte;
  StackPos: UInt32;

  VarStack: array of Byte;
  VarStackPos, VarStackLen: UInt32;

  TryStack: array of PByte;
  TryStackPos: UInt32;
  InException: Exception;

  CallStack: array of TCallRec;
  CallStackPos: UInt32;

  procedure JumpTo(const Target: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Code := PByte(PtrUInt(CodeBase) + Target);
  end;

  procedure JumpToRelative(const Offset: Int32); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Inc(Code, Offset);
  end;

  procedure HandleException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (TryStackPos > 0) then
    begin
      Dec(TryStackPos);
      Code := TryStack[TryStackPos];
    end
    else
      raise InException;
  end;

  procedure PushToVar(const Size: UInt16); {$IFDEF Lape_Inline}inline;{$ENDIF}
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

  function getTIMemPosPtr(const p: TIMemPos): Pointer; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    case p.MemPos of
      mpMem: Result := p.Ptr;
      mpVar: Result := @VarStack[VarStackPos + p.VOffset];
      mpStack: Result := @Stack[StackPos - p.SOffset];
      else LapeException(lpeImpossible);
    end;
    if p.isPointer then
      Result := Pointer(PtrUInt(PPointer(Result)^) + p.POffset);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    SetLength(Stack, PUInt16(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoInitVarLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    SetLength(VarStack, PUInt16(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FillChar(Stack[StackPos], PUInt16(PtrUInt(Code) + ocSize)^, 0);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    VarStackPos := VarStackLen;
    Inc(VarStackLen, PUInt16(PtrUInt(Code) + ocSize)^);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    a: Integer;
  begin
    a := PUInt16(PtrUInt(Code) + ocSize)^;
    VarStackPos := VarStackLen;
    Inc(VarStackLen, a);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    FillChar(VarStack[VarStackPos], a, 0);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Inc(VarStackLen, PUInt16(PtrUInt(Code) + ocSize)^);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    a, b: UInt32;
  begin
    a := PUInt16(PtrUInt(Code) + ocSize)^;
    b := VarStackLen;
    Inc(VarStackLen, a);
    {$IFDEF Lape_UnlimitedVarStackSize}
    if (VarStackLen > Length(VarStack)) then
      SetLength(VarStack, VarStackLen + (VarStackSize div 2));
    {$ENDIF}
    FillChar(VarStack[b], a, 0);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoPopVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    a: Integer;
  begin
    a := PUInt16(PtrUInt(Code) + ocSize)^;
    Dec(VarStackPos, a);
    Dec(VarStackLen, a);
    Inc(Code, SizeOf(UInt16) + ocSize);
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    {$IFDEF Lape_UnlimitedTryStackSize}
    if (TryStackPos >= Length(TryStack)) then
      SetLength(TryStack, TryStackPos + (TryStackSize div 2));
    {$ENDIF}
    TryStack[TryStackPos] := PByte(PtrUInt(Code) + PInt32(PtrUInt(Code) + ocSize)^);
    Inc(TryStackPos);
    Inc(Code, SizeOf(Int32) + ocSize);
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
    else
      Inc(Code, ocSize);
  end;

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    InException.Free();
    InException := nil;
    Inc(Code, ocSize);
  end;

  procedure DoIncCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    {$IFDEF Lape_UnlimitedCallStackSize}
    if (CallStackPos >= Length(CallStack)) then
      SetLength(CallStack, CallStackPos + (CallStackSize div 2));
    {$ENDIF}
    with CallStack[CallStackPos], POC_IncCall(PtrUInt(Code) + ocSize)^ do
    begin
      CalledFrom := PByte(PtrUInt(Code) + ocSize + SizeOf(TOC_IncCall));
      VarStackP := VarStackPos;
      PushToVar(ParamSize);
      StackP := StackPos;
      JumpTo(PUInt32(getTIMemPosPtr(CodePos))^);
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
      HandleException();
  end;

  procedure DoInvokeExternalProc; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_InvokeExternalProc(PtrUInt(Code) + ocSize)^ do
    begin
      TLapeCallbackProc(PPointer(getTIMemPosPtr(MemPos))^)(@Stack[StackPos - ParamLen]);
      Dec(StackPos, ParamLen);
    end;
    Inc(Code, SizeOf(TOC_InvokeExternalProc) + ocSize);
  end;

  procedure DoInvokeExternalFunc; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_InvokeExternalFunc(PtrUInt(Code) + ocSize)^ do
    begin
      TLapeCallbackFunc(PPointer(getTIMemPosPtr(MemPos))^)(@Stack[StackPos - ParamLen], getTIMemPosPtr(ResPos));
      Dec(StackPos, ParamLen);
    end;
    Inc(Code, SizeOf(TOC_InvokeExternalFunc) + ocSize);
  end;

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

    DaLoop();
  except
    on E: Exception do
      LapeException(lpeRuntime, [E.Message] {$IFDEF Lape_EmitPos}, PDocPos(PtrUInt(Code) + SizeOf(opCodeType))^ {$ENDIF});
  end;
end;

end.

