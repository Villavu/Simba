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

const
  IsScriptProcess: Boolean = False;
  IsScriptProcessWithCommunication: Boolean = False;

type
  {$SCOPEDENUMS ON}
  ESimbaScriptState = (STATE_PAUSED, STATE_STOP, STATE_RUNNING, STATE_NONE);

  ESimbaCommunicationMessage = (
    STATUS, DISGUSE,
    SIMBA_PID, SIMBA_TARGET_PID, SIMBA_TARGET_WINDOW,
    SCRIPT_ERROR, SCRIPT_STATE_CHANGE,
    TRAY_NOTIFICATION,
    DEBUGIMAGE_UPDATE,
    DEBUGIMAGE_MOVETO, DEBUGIMAGE_MAXSIZE, DEBUGIMAGE_SHOW, DEBUGIMAGE_HIDE,
    DEBUGIMAGE_DISPLAY, DEBUGIMAGE_DISPLAY_XY,
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
  TRGB32Array = array of TRGB32;

  PPRGB32 = ^PRGB32; // Pointer to PRGB32
  PRGB32 = ^TRGB32;
  TPRGB32Array = array of PRGB32;

  TRetData = record
    Ptr: PRGB32;
    IncPtrWith: Integer;
    RowLen: Integer;
  end;
  PRetData = ^TRetData;

  PWindowHandle = ^TWindowHandle;
  PWindowHandleArray = ^TWindowHandleArray;

  TWindowHandle = type UInt64;
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
    {$i quad.inc}
    {$i box.inc}
    {$i boxarray.inc}
    {$i point.inc}
    {$i rgb32.inc}
    {$i integermatrix.inc}
    {$i singlematrix.inc}
    {$i matrix.inc}
    {$i string.inc}
  {$UNDEF HEADER}

var
  DebugLnFunc: procedure(const Msg: String);

{$PUSH}
{$SCOPEDENUMS ON}
type
  ESimbaDebugLn = (
    NONE,
    CLEAR,
    YELLOW,
    RED,
    GREEN,
    SHOW
  );
{$POP}

function ToStr(Typ: ESimbaDebugLn): String; overload;

procedure DebugLn(const Msg: String); overload;
procedure DebugLn(const Msg: String; Args: array of const); overload;

procedure SimbaDebugLn(const Msg: String); overload;
procedure SimbaDebugLn(const Msg: String; Args: array of const); overload;

procedure SimbaDebugLn(const Typ: ESimbaDebugLn; const Msg: String); overload;
procedure SimbaDebugLn(const Typ: ESimbaDebugLn; const Msg: String; Args: array of const); overload;

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
procedure Swap(var A, B: Double); overload;
procedure Swap(var A, B: TPoint); overload;
procedure Swap(var A, B: Pointer); overload;
procedure Swap(var A, B: TRGB32); overload;

type
  TProc       = procedure of object;
  TProcArray  = array of TProc;

  TNestedProc      = procedure is nested;
  TNestedProcArray = array of TNestedProc;

procedure Sync(Method: TProc); overload;
procedure Sync(Method: TNestedProc); overload;

function Threaded(Method: TNestedProc): TThread; overload;
function Threaded(Method: TProc): TThread; overload;
procedure Threaded(Methods: TProcArray; Interval: Integer = 0); overload;

procedure ThreadedAndForget(Method: TNestedProc);

implementation

uses
  math, forms, lazloggerbase, uregexpr, strutils,
  simba.math, simba.overallocatearray, simba.geometry, simba.heaparray,
  simba.algo_sort, simba.tpa, simba.random;

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

{$DEFINE BODY}
  {$i generics.inc}
  {$i quad.inc}
  {$i box.inc}
  {$i boxarray.inc}
  {$i point.inc}
  {$i rgb32.inc}
  {$i integermatrix.inc}
  {$i singlematrix.inc}
  {$i matrix.inc}
  {$i string.inc}
{$UNDEF BODY}

function ToStr(Typ: ESimbaDebugLn): String;
begin
  Result := #0+#0+'$'+IntToHex(Ord(Typ), 2);
end;

procedure DebugLn(const Msg: String);
begin
  {%H-}lazloggerbase.DebugLn(Msg);
end;

procedure DebugLn(const Msg: String; Args: array of const);
begin
  {%H-}lazloggerbase.DebugLn(Msg, Args);
end;

procedure SimbaDebugLn(const Msg: String);
begin
  DebugLnFunc(Msg);
end;

procedure SimbaDebugLn(const Msg: String; Args: array of const);
begin
  SimbaDebugLn(Msg.Format(Args));
end;

procedure SimbaDebugLn(const Typ: ESimbaDebugLn; const Msg: String);
begin
  if (not IsScriptProcess) or IsScriptProcessWithCommunication then
    SimbaDebugLn(ToStr(Typ) + Msg)
  else
    SimbaDebugLn(Msg);
end;

procedure SimbaDebugLn(const Typ: ESimbaDebugLn; const Msg: String; Args: array of const);
begin
  SimbaDebugLn(Typ, Msg.Format(Args));
end;

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

procedure Swap(var A, B: Double);
begin
  specialize Swap<Double>(A, B);
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

type
  TSyncObject = object
    Proc: TProc;
    NestedProc: TNestedProc;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
begin
  try
    if Assigned(Proc)       then Proc();
    if Assigned(NestedProc) then NestedProc();
  except
    on E: Exception do
      DebugLn('Sync :: ' + E.Message);
  end;
end;

procedure Sync(Method: TProc);
var
  SyncObject: TSyncObject;
begin
  if (GetCurrentThreadID() <> MainThreadID) then
  begin
    SyncObject.Proc       := Method;
    SyncObject.NestedProc := nil;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

procedure Sync(Method: TNestedProc);
var
  SyncObject: TSyncObject;
begin
  if (GetCurrentThreadID() <> MainThreadID) then
  begin
    SyncObject.Proc       := nil;
    SyncObject.NestedProc := Method;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TProc;
    FNestedProc: TNestedProc;

    procedure DoTerminated(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create(Proc: TNestedProc; Forget: Boolean); reintroduce;
    constructor Create(Proc: TProc; Forget: Boolean); reintroduce;
  end;

procedure TThreaded.DoTerminated(Sender: TObject);
begin
  Flush(Output);
end;

procedure TThreaded.Execute;
begin
  if Assigned(FNestedProc) then FNestedProc();
  if Assigned(FProc)       then FProc();
end;

constructor TThreaded.Create(Proc: TNestedProc; Forget: Boolean);
begin
  inherited Create(False, 512*512);

  if Forget then
  begin
    FreeOnTerminate := True;
    OnTerminate := @DoTerminated;
  end;

  FNestedProc := Proc;
end;

constructor TThreaded.Create(Proc: TProc; Forget: Boolean);
begin
  inherited Create(False, 512*512);

  if Forget then
  begin
    FreeOnTerminate := True;
    OnTerminate := @DoTerminated;
  end;

  FProc := Proc;
end;

function Threaded(Method: TNestedProc): TThread;
begin
  Result := TThreaded.Create(Method, False);
end;

function Threaded(Method: TProc): TThread;
begin
  Result := TThreaded.Create(Method, False);
end;

procedure Threaded(Methods: TProcArray; Interval: Integer);
var
  Threads: array of TThread;
  I: Integer;
begin
  SetLength(Threads, Length(Methods));
  for I := 0 to High(Threads) do
  begin
    Threads[I] := TThreaded.Create(Methods[I], False);
    if (Interval > 0) then
      Sleep(Interval);
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor();
    Threads[I].Free();
  end;
end;

procedure ThreadedAndForget(Method: TNestedProc);
begin
  TThreaded.Create(Method, True);
end;

initialization
  DebugLnFunc := @DebugLn;

end.

