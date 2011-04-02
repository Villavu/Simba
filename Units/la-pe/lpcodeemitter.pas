{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Bytecode emitter.
}
unit lpcodeemitter;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpinterpreter, lpparser;

type
  TLapeCodePointers = {$IFDEF FPC}specialize{$ENDIF} TLapeList<PCodePos>;
  TLapeCodeEmitterBase = class(TLapeBaseClass)
  protected
    FCode: TCodeArray;
    FCodeStart: Integer;
    FCodeCur: Integer;
    FCodeSize: Integer;
    FCodePointers: TLapeCodePointers;
    function getCode: Pointer;
   public
    CodeGrowSize: Word;
    Tokenizer: TLapeTokenizerBase;

    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; virtual;
    procedure Delete(StartOffset, Len: Integer); overload; virtual;
    procedure Delete(StartOffset, Len: Integer; var Offset: Integer); overload; virtual;

    function addCodePointer(p: PCodePos): Integer; virtual;
    procedure deleteCodePointer(i: Integer); overload; virtual;
    procedure deleteCodePointer(p: PCodePos); overload; virtual;
    procedure adjustCodePointers(Pos, Offset: Integer); virtual;

    procedure EnsureCodeGrowth(Len: Word); virtual;
    function getCodeOffset(Offset: Integer): Integer; virtual;
    function CheckOffset(var Offset: Integer; Len: Word = 0): Integer; overload; virtual;
    function CheckOffset(Len: Word = 0): Integer; overload; virtual;

    function _Int8(v: Int8; var Offset: Integer): Integer;
    function _UInt8(v: UInt8; var Offset: Integer): Integer;
    function _Int16(v: Int16; var Offset: Integer): Integer;
    function _UInt16(v: UInt16; var Offset: Integer): Integer;
    function _Int32(v: Int32; var Offset: Integer): Integer;
    function _UInt32(v: UInt32; var Offset: Integer): Integer;
    function _Int64(v: Int64; var Offset: Integer): Integer;
    function _UInt64(v: UInt64; var Offset: Integer): Integer;

    function _StackOffset(v: TStackOffset; var Offset: Integer): Integer;
    function _ParamSize(v: TParamSize; var Offset: Integer): Integer;
    function _CodePos(v: TCodePos; var Offset: Integer): Integer;
    function _CodeOffset(v: TCodeOffset; var Offset: Integer): Integer;

    function _DocPos(Pos: TDocPos; var Offset: Integer): Integer; overload;
    function _DocPos(Pos: TDocPos): Integer; overload;
    function _DocPos(Pos: PDocPos; var Offset: Integer): Integer; overload;
    function _DocPos(Pos: PDocPos): Integer; overload;
    function _DocPos(var Offset: Integer): Integer; overload;
    function _DocPos: Integer; overload;

    function _op(op: opCode; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _op(op: opCode; Pos: PDocPos = nil): Integer; overload;

    function _InitStackLen(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InitStackLen(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _InitVarLen(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InitVarLen(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _InitStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InitStack(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _GrowStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GrowStack(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVar(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _GrowVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GrowVar(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _GrowVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GrowVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _PopVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _PopVar(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafe(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafe(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafeR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafeR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;

    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; Pos: PDocPos = nil): Integer; overload;
    function _DecTry(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DecTry(Pos: PDocPos = nil): Integer; overload;
    function _EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _EndTry(Pos: PDocPos = nil): Integer; overload;
    function _CatchException(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _CatchException(Pos: PDocPos = nil): Integer; overload;

    function _DecCall(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DecCall(Pos: PDocPos = nil): Integer; overload;
    function _DecCall_EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DecCall_EndTry(Pos: PDocPos = nil): Integer; overload;

    {$I lpcodeemitter_invokeheader.inc}
    {$I lpcodeemitter_jumpheader.inc}
    {$I lpcodeemitter_evalheader.inc}

    property Code: Pointer read getCode;
    property CodeLen: Integer read FCodeCur;
  end;

implementation

function TLapeCodeEmitterBase.getCode: Pointer;
begin
  Result := @FCode[FCodeStart];
end;

constructor TLapeCodeEmitterBase.Create;
begin
  inherited;

  FCodePointers := TLapeCodePointers.Create(nil, dupIgnore);
  CodeGrowSize := 256;
  Reset();
end;

destructor TLapeCodeEmitterBase.Destroy;
begin
  FCodePointers.Free();
  inherited;
end;

procedure TLapeCodeEmitterBase.Reset;
begin
  FCodeSize := CodeGrowSize;
  SetLength(FCode, FCodeSize);
  FillChar(FCode[0], FCodeSize, 0);
  FCodePointers.Clear();

  {$IFDEF Lape_SmallCode}
  FCodeStart := 0;
  {$ELSE}
  //16 byte alignment
  FCodeStart := (16 - (PtrUInt(FCode) mod 16)) mod 16;
  {$ENDIF}
  FCodeCur := FCodeStart;
end;

procedure TLapeCodeEmitterBase.Delete(StartOffset, Len: Integer);
var
  i: Integer;
begin
  if (Len <= 0) or (StartOffset < 0) or (StartOffset + Len > FCodeCur) then
    Exit;
  for i := StartOffset + Len to FCodeCur - 1 do
    FCode[i - Len] := FCode[i];

  Dec(FCodeCur, Len);
  FillChar(FCode[FCodeCur], Len, 0);
  adjustCodePointers(StartOffset, -Len);
end;

procedure TLapeCodeEmitterBase.Delete(StartOffset, Len: Integer; var Offset: Integer);
begin
  Delete(StartOffset, Len);
  if (Offset > StartOffset) then
    Dec(Offset, Len);
end;

function TLapeCodeEmitterBase.addCodePointer(p: PCodePos): Integer;
begin
  if (p <> nil) then
    Result := FCodePointers.add(p)
  else
    Result := -1;
end;

procedure TLapeCodeEmitterBase.deleteCodePointer(i: Integer);
begin
  FCodePointers.Delete(i);
end;

procedure TLapeCodeEmitterBase.deleteCodePointer(p: PCodePos);
begin
  FCodePointers.DeleteItem(p);
end;

procedure TLapeCodeEmitterBase.adjustCodePointers(Pos, Offset: Integer);
var
  i: Integer;
begin
  for i := 0 to FCodePointers.Count - 1 do
    if (FCodePointers[i]^ > Pos) then
      Inc(FCodePointers[i]^, Offset);
end;

procedure TLapeCodeEmitterBase.EnsureCodeGrowth(Len: Word);
begin
  if (FCodeCur + Len >= FCodeSize) then
  begin
    if (Len > CodeGrowSize) then
      FCodeSize := FCodeSize + Len
    else
      FCodeSize := FCodeSize + CodeGrowSize;

    SetLength(FCode, FCodeSize);
    FillChar(FCode[FCodeCur], FCodeSize - FCodeCur, 0);
  end;
end;

function TLapeCodeEmitterBase.getCodeOffset(Offset: Integer): Integer;
begin
  if (Offset < 0) then
    Result := 0
  else
    Result := Offset - FCodeStart;
end;

function TLapeCodeEmitterBase.CheckOffset(var Offset: Integer; Len: Word = 0): Integer;
begin
  if (Offset < 0) or (Offset + Len > FCodeCur) then
  begin
    Offset := FCodeCur;
    EnsureCodeGrowth(Len);
    Inc(FCodeCur, Len);
  end;

  Result := Offset;
end;

function TLapeCodeEmitterBase._Int8(v: Int8; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int8));
  PInt8(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(Int8));
end;

function TLapeCodeEmitterBase._UInt8(v: UInt8; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt8));
  PUInt8(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(UInt8));
end;

function TLapeCodeEmitterBase._Int16(v: Int16; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int16));
  PInt16(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(Int16));
end;

function TLapeCodeEmitterBase._UInt16(v: UInt16; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt16));
  PUInt16(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(UInt16));
end;

function TLapeCodeEmitterBase._Int32(v: Int32; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int32));
  PInt32(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(Int32));
end;

function TLapeCodeEmitterBase._UInt32(v: UInt32; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt32));
  PUInt32(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(UInt32));
end;

function TLapeCodeEmitterBase._Int64(v: Int64; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int64));
  PInt64(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(Int64));
end;

function TLapeCodeEmitterBase._UInt64(v: UInt64; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt64));
  PUInt64(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(UInt64));
end;

function TLapeCodeEmitterBase._StackOffset(v: TStackOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TStackOffset));
  PStackOffset(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(TStackOffset));
end;

function TLapeCodeEmitterBase._ParamSize(v: TParamSize; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TParamSize));
  PParamSize(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(TParamSize));
end;

function TLapeCodeEmitterBase._CodePos(v: TCodePos; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TCodePos));
  PCodePos(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(TCodePos));
end;

function TLapeCodeEmitterBase._CodeOffset(v: TCodeOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TCodeOffset));
  PCodeOffset(@FCode[Offset])^ := v;
  Inc(Offset, SizeOf(TCodeOffset));
end;

function TLapeCodeEmitterBase._DocPos(Pos: TDocPos; var Offset: Integer): Integer;
begin
{$IFDEF Lape_EmitPos}
  Result := CheckOffset(Offset, SizeOf(TDocPos));
  PDocPos(@FCode[Offset])^ := Pos;
  Inc(Offset, SizeOf(TDocPos));
{$ENDIF}
end;

function TLapeCodeEmitterBase._DocPos(Pos: PDocPos; var Offset: Integer): Integer;
begin
  if (Pos = nil) then
    Result := _DocPos(Offset)
  else
    Result := _DocPos(Pos^, Offset);
end;

function TLapeCodeEmitterBase._DocPos(var Offset: Integer): Integer;
begin
  Result := _DocPos(NullDocPos, Offset);
end;

function TLapeCodeEmitterBase._op(op: opCode; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(opCodeType));
  opCodeTypeP(@FCode[Offset])^ := opCodeType(op);
  Inc(Offset, SizeOf(opCodeType));
  _DocPos(Pos, Offset);
end;

function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInitStackLen, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._InitVarLen(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInitVarLen, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._InitStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInitStack, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGrowStack, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocExpandVar, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocExpandVarAndInit, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGrowVar, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGrowVarAndInit, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._PopVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocPopVar, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpSafe, Offset, Pos);
  _CodePos(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpSafeR, Offset, Pos);
  _CodeOffset(Jmp, Offset);
end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncTry, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_IncTry));
  with POC_IncTry(@FCode[Offset])^ do
  begin
    Jmp := AJmp;
    JmpFinally := AJmpFinally;
  end;
  Inc(Offset, SizeOf(TOC_IncTry));
end;

function TLapeCodeEmitterBase._DecTry(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDecTry, Offset, Pos);
end;

function TLapeCodeEmitterBase._EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocEndTry, Offset, Pos);
end;

function TLapeCodeEmitterBase._CatchException(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocCatchException, Offset, Pos);
end;

function TLapeCodeEmitterBase._DecCall(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDecCall, Offset, Pos);
end;

function TLapeCodeEmitterBase._DecCall_EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDecCall_EndTry, Offset, Pos);
end;

function TLapeCodeEmitterBase.CheckOffset(Len: Word = 0): Integer;
  var o: Integer; begin o := -1; Result := CheckOffset(o, Len); end;
function TLapeCodeEmitterBase._DocPos(Pos: TDocPos): Integer;
  var o: Integer; begin o := -1; Result := _DocPos(Pos, o); end;
function TLapeCodeEmitterBase._DocPos(Pos: PDocPos): Integer;
  var o: Integer; begin o := -1; Result := _DocPos(Pos, o); end;
function TLapeCodeEmitterBase._DocPos(): Integer;
  var o: Integer; begin o := -1; Result := _DocPos(o); end;
function TLapeCodeEmitterBase._op(op: opCode; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _op(op, o, Pos); end;
function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InitStackLen(Len, o, Pos); end;
function TLapeCodeEmitterBase._InitVarLen(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InitVarLen(Len, o, Pos); end;
function TLapeCodeEmitterBase._InitStack(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InitStack(Len, o, Pos); end;
function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GrowStack(Len, o, Pos); end;
function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVar(Len, o, Pos); end;
function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVarAndInit(Len, o, Pos); end;
function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVar(Len, o, Pos); end;
function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVarAndInit(Len, o, Pos); end;
function TLapeCodeEmitterBase._PopVar(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _PopVar(Len, o, Pos); end;
function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpSafe(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpSafeR(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _IncTry(AJmp, AJmpFinally, o, Pos); end;
function TLapeCodeEmitterBase._DecTry(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DecTry(o, Pos); end;
function TLapeCodeEmitterBase._EndTry(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _EndTry(o, Pos); end;
function TLapeCodeEmitterBase._CatchException(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _CatchException(o, Pos); end;

function TLapeCodeEmitterBase._DecCall(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DecCall(o, Pos); end;
function TLapeCodeEmitterBase._DecCall_EndTry(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DecCall_EndTry(o, Pos); end;

{$I lpcodeemitter_invokebody.inc}
{$I lpcodeemitter_jumpbody.inc}
{$I lpcodeemitter_evalbody.inc}

end.

