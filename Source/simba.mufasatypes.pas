{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.mufasatypes;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics;

type
  {$SCOPEDENUMS ON}
  ESimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING, STATE_NONE);

  ESimbaCommunicationMessage = (
    STATUS, DISGUSE,
    SIMBA_PID, SIMBA_TARGET_PID, SIMBA_TARGET_WINDOW,
    SCRIPT_ERROR, SCRIPT_STATE_CHANGE,
    BALLOON_HINT,
    DEBUGIMAGE_ZOOM, DEBUGIMAGE_MOVETO, DEBUGIMAGE_MAXSIZE, DEBUGIMAGE_DRAW, DEBUGIMAGE_SHOW, DEBUGIMAGE_HIDE,
    DEBUGIMAGE_DISPLAY, DEBUGIMAGE_CLEAR,
    DEBUG_METHOD_NAME, DEBUG_EVENTS
  );

  PSimbaScriptDebuggerEvent = ^TSimbaScriptDebuggerEvent;
  TSimbaScriptDebuggerEvent = packed record
    Method: Int16;
    Depth: Int16;
    Exception: Boolean;
  end;
  TSimbaScriptDebuggerEvents = array of TSimbaScriptDebuggerEvent;
  {$SCOPEDENUMS OFF}

  TRGB32 = packed record
  case Byte of
    0: (B, G, R, A: Byte);
    1: (AsInteger: Integer);
  end;

  PRGB32 = ^TRGB32;
  PRGB32Array = ^TRGB32Array;
  TRGB32Array = array of TRGB32;

  PPRGB32 = ^PRGB32;
  PPRGB32Array = ^TPRGB32Array;
  TPRGB32Array = array of PRGB32;

  TRetData = record
    Ptr: PRGB32;
    IncPtrWith: Integer;
    RowLen: Integer;
  end;
  PRetData = ^TRetData;

  PWindowHandle = ^TWindowHandle;
  PWindowHandleArray = ^TWindowHandleArray;

  TWindowHandle = type PtrUInt;
  TWindowHandleArray = array of TWindowHandle;

type
  PClickType = ^TClickType;
  TClickType = (
    MOUSE_RIGHT,
    MOUSE_LEFT,
    MOUSE_MIDDLE,
    MOUSE_EXTRA_1,
    MOUSE_EXTRA_2,
    MOUSE_SCROLL_UP,
    MOUSE_SCROLL_DOWN
  );

  PStringArray = ^TStringArray;
  TStringArray = array of String;

  PPoint = ^TPoint;

  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;
  P2DPointArray = ^T2DPointArray;
  T2DPointArray = array of TPointArray;

  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array of Integer;

  PIntegerMatrix = ^TIntegerMatrix;
  TIntegerMatrix = array of TIntegerArray;

  PByteArray = ^TByteArray;
  TByteArray = array of Byte;

  PByteMatrix = ^TByteMatrix;
  TByteMatrix = array of TByteArray;

  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array of Boolean;

  PBooleanMatrix = ^TBooleanMatrix;
  TBooleanMatrix = array of TBooleanArray;

  PSingleArray  = ^TSingleArray;
  TSingleArray  = array of Single;
  PSingleMatrix = ^TSingleMatrix;
  TSingleMatrix = array of TSingleArray;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array of Double;

  PDoubleMatrix = ^TDoubleMatrix;
  TDoubleMatrix = array of TDoubleArray;

  TInt64Array = array of Int64;
  PInt64Array = ^TInt64Array;

  EComparator = (__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__);
  PComparator = ^EComparator;

  PBox = ^TBox;
  TBox = record
  case Integer of
    0: (X1, Y1, X2, Y2: Integer);
    1: (TopLeft, BottomRight: TPoint);
  end;
  PBoxArray = ^TBoxArray;
  TBoxArray = array of TBox;

  {$DEFINE HEADER}
    {$i generics.inc}
    {$i box.inc}
    {$i boxarray.inc}
    {$i point.inc}
    {$i rgb32.inc}
  {$UNDEF HEADER}

function Min(const A, B: Integer): Integer; inline; overload;
function Max(const A, B: Integer): Integer; inline; overload;
function Min(const A, B: Int64): Int64; inline; overload;
function Max(const A, B: Int64): Int64; inline; overload;
function Min(const A, B: Single): Single; inline; overload;
function Max(const A, B: Single): Single; inline; overload;
function Min(const A, B: Double): Double; inline; overload;
function Max(const A, B: Double): Double; inline; overload;

procedure Swap(var A, B: Byte); overload;
procedure Swap(var A, B: Integer); overload;
procedure Swap(var A, B: Extended); overload;
procedure Swap(var A, B: TPoint); overload;
procedure Swap(var A, B: Pointer); overload;
procedure Swap(var A, B: TRGB32); overload;

function Unique(const Arr: TIntegerArray): TIntegerArray; overload;
function Unique(const Arr: TStringArray): TStringArray; overload;

type
  TProc       = procedure of object;
  TProcArray  = array of TProc;

  TNestedProc      = procedure is nested;
  TNestedProcArray = array of TNestedProc;

procedure Sync(Method: TProc); overload;
procedure Sync(Method: TNestedProc); overload;

function Threaded(Method: TNestedProc): TThread; overload;
procedure Threaded(Methods: TProcArray; Interval: Integer = 0); overload;

implementation

uses
  math,
  simba.math, simba.overallocatearray, simba.geometry;

{$DEFINE BODY}
  {$i generics.inc}
  {$i box.inc}
  {$i boxarray.inc}
  {$i point.inc}
  {$i rgb32.inc}
{$UNDEF BODY}

function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Int64): Int64;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Int64): Int64;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Double): Double;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

procedure Swap(var A, B: Byte);
begin
  specialize Swap<Byte>(A, B);
end;

procedure Swap(var A, B: Integer);
begin
  specialize Swap<Integer>(A, B);
end;

procedure Swap(var A, B: Extended);
begin
  specialize Swap<Extended>(A, B);
end;

procedure Swap(var A, B: TPoint);
begin
  specialize Swap<TPoint>(A, B);
end;

procedure Swap(var A, B: Pointer);
begin
  specialize Swap<Pointer>(A, B);
end;

procedure Swap(var A, B: TRGB32);
begin
  specialize Swap<TRGB32>(A, B);
end;

function Unique(const Arr: TIntegerArray): TIntegerArray;
var
  I, J, Size: Integer;
  Value: Integer;
  Table: array of record
    Bucket: TIntegerArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<Integer>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Value and Size] do
    begin
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

function Unique(const Arr: TStringArray): TStringArray;
var
  I, J, Size: Integer;
  Value: String;
  Table: array of record
    Bucket: TStringArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<String>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Hash(Value) and Size] do
    begin
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

type
  TSyncObject = object
    Proc: TProc;
    NestedProc: TNestedProc;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
begin
  if Assigned(Proc) then Proc();
  if Assigned(NestedProc) then NestedProc();
end;

procedure Sync(Method: TProc);
var
  SyncObject{%H-}: TSyncObject;
begin
  SyncObject.Proc := Method;

  TThread.Synchronize(nil, @SyncObject.Execute);
end;

procedure Sync(Method: TNestedProc);
var
  SyncObject{%H-}: TSyncObject;
begin
  SyncObject.NestedProc := Method;

  TThread.Synchronize(nil, @SyncObject.Execute);
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TProc;
    FNestedProc: TNestedProc;

    procedure Execute; override;
  public
    constructor Create(Proc: TNestedProc); reintroduce;
    constructor Create(Proc: TProc); reintroduce;
  end;

procedure TThreaded.Execute;
begin
  if Assigned(FNestedProc) then FNestedProc();
  if Assigned(FProc)       then FProc();
end;

constructor TThreaded.Create(Proc: TNestedProc);
begin
  inherited Create(False, 512*512);

  FNestedProc := Proc;
end;

constructor TThreaded.Create(Proc: TProc);
begin
  inherited Create(False, 512*512);

  FProc := Proc;
end;

function Threaded(Method: TNestedProc): TThread;
begin
  Result := TThreaded.Create(Method);
end;

procedure Threaded(Methods: TProcArray; Interval: Integer);
var
  Threads: array of TThread;
  I: Integer;
begin
  SetLength(Threads, Length(Methods));
  for I := 0 to High(Threads) do
  begin
    Threads[I] := TThreaded.Create(Methods[I]);
    if (Interval > 0) then
      Sleep(Interval);
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor();
    Threads[I].Free();
  end;
end;

end.

