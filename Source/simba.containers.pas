{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Very simple containers:
    - List
    - Stack
    - ArrayBuffer
    - StringBuilder
}
unit simba.containers;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, syncobjs,
  simba.base;

type
  generic TSimbaList<_T> = class(TObject)
  private
    function GetFirst: _T;
    function GetLast: _T;
  public type
    TArr = array of _T;
  protected
    FArr: TArr;
    FCount: Integer;

    function GetItem(Index: Integer): _T; virtual;
    procedure SetItem(Index: Integer; AValue: _T); virtual;
  public
    procedure Add(Item: _T); virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    function ToArray: TArr; virtual;

    property Count: Integer read FCount;
    property Items[Index: Integer]: _T read GetItem write SetItem; default;

    property First: _T read GetFirst;
    property Last: _T read GetLast;
  end;

  generic TSimbaObjectList<_T: class> = class(specialize TSimbaList<_T>)
  protected
    FFreeObjects: Boolean;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Delete(Item: _T); overload;
    function IndexOf(Item: _T): Integer;

    property FreeObjects: Boolean read FFreeObjects write FFreeObjects;

    constructor Create(AFreeObjects: Boolean = False); reintroduce;
    destructor Destroy; override;
  end;

  generic TSimbaThreadsafeObjectList<_T: class> = class(TObject)
  protected type
    TList = specialize TSimbaObjectList<_T>;
  protected
    FList: TList;
    FLock: TCriticalSection;

    function GetCount: Integer;
    function GetFirst: _T;
    function GetLast: _T;
    function GetItem(Index: Integer): _T;
  public
    procedure Add(Item: _T);
    procedure Delete(Item: _T);

    procedure Lock;
    procedure UnLock;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: _T read GetItem; default;
    property First: _T read GetFirst;
    property Last: _T read GetLast;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TSimbaStringPair = record
    Name: String;
    Value: String;
  end;
  TSimbaStringPairList = specialize TSimbaList<TSimbaStringPair>;

  generic TSimbaStack<_T> = class(TObject)
  public type
    TArr = array of _T;
  protected
    FArr: TArr;
    FLen: Integer;
    FCount: Integer;

    function GetTop: _T;
  public
    property Top: _T read GetTop;
    property Count: Integer read FCount;
    procedure Clear;
    procedure Push(Item: _T);
    function Pop: _T;
  end;

  generic TSimbaArrayBuffer<_T> = record
  private
  type
    TSelf = specialize TSimbaArrayBuffer<_T>;
    TArr = specialize TArray<_T>;
  private
    FLength: Integer;
    FCount: Integer;
    FArr: TArr;

    procedure Grow(const Len: Integer = 1);
    function GetItem(const Index: Integer): _T; inline;
  public
    property Size: Integer read FLength;
    property Count: Integer read FCount;
    property Item[Index: Integer]: _T read GetItem; default;

    procedure Clear;
    procedure Init(const InitialSize: Integer = 1024);
    procedure InitWith(const Values: TArr);

    procedure Add(const Value: _T); overload; inline;
    procedure Add(const Values: TArr); overload; inline;

    function First: _T; inline;
    function Last: _T; inline;
    function Pop: _T; inline;

    function ToArray(Copy: Boolean = True): TArr;

    class operator Initialize(var Self: TSelf);
  end;

  TSimbaIntegerBuffer    = specialize TSimbaArrayBuffer<Integer>;
  TSimbaStringBuffer     = specialize TSimbaArrayBuffer<String>;
  TSimbaPointBuffer      = specialize TSimbaArrayBuffer<TPoint>;
  TSimbaPointArrayBuffer = specialize TSimbaArrayBuffer<TPointArray>;

  TSimbaPointBufferHelper = record helper for TSimbaPointBuffer
  public
    procedure Add(const X, Y: Integer); overload; inline;
  end;

  TSimbaStringBuilder = record
  private
    FData: String;
    FCount: Integer;

    procedure EnsureGrowth(const Len: Integer);

    function GetString: String;
    function GetPeek: Char;
  public
    class operator Initialize(var Self: TSimbaStringBuilder);

    procedure AppendBuf(const Buffer; Count: Integer);
    procedure Append(const Str: String);
    procedure AppendLine(const Str: String = '');

    property Str: String read GetString;
    property Count: Integer read FCount;
    property Peek: Char read GetPeek;
  end;

implementation

// List
procedure TSimbaList.SetItem(Index: Integer; AValue: _T);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('%s.SetItem: Index %d out of bounds', [ClassName, Index]);

  FArr[Index] := AValue;
end;

function TSimbaList.GetFirst: _T;
begin
  if (FCount = 0) then
    SimbaException('%s.GetItem: Index %d out of bounds', [ClassName, 0]);
  Result := FArr[0];
end;

function TSimbaList.GetLast: _T;
begin
  if (FCount = 0) then
    SimbaException('%s.GetItem: Index %d out of bounds', [ClassName, 0]);
  Result := FArr[FCount - 1];
end;

function TSimbaList.GetItem(Index: Integer): _T;
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('%s.GetItem: Index %d out of bounds', [ClassName, Index]);

  Result := FArr[Index];
end;

procedure TSimbaList.Add(Item: _T);
begin
  if (FCount >= Length(FArr)) then
    SetLength(FArr, 32 + (Length(FArr) * 2));

  FArr[FCount] := Item;
  Inc(FCount);
end;

procedure TSimbaList.Clear;
begin
  FCount := 0;
end;

procedure TSimbaList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('%s.Delete: Index %d out of bounds', [ClassName, Index]);

  if (Index < FCount - 1) then
    Move(FArr[Index + 1], FArr[Index], (FCount - (Index + 1)) * SizeOf(_T));
  FCount := FCount - 1;
end;

function TSimbaList.ToArray: TArr;
begin
  SetLength(Result, FCount);
  if (FCount > 0) then
    Move(FArr[0], Result[0], FCount * SizeOf(_T));
end;

// Object list
procedure TSimbaObjectList.Clear;
var
  I: Integer;
begin
  if FFreeObjects then
    for I := 0 to FCount - 1 do
      FArr[I].Free();

  inherited Clear();
end;

procedure TSimbaObjectList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('%s.Delete: Index %d out of bounds', [ClassName, Index]);
  if FFreeObjects then
    FArr[Index].Free();

  inherited Delete(Index);
end;

procedure TSimbaObjectList.Delete(Item: _T);
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if (Index > -1) then
    Delete(Index);
end;

function TSimbaObjectList.IndexOf(Item: _T): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if (FArr[I] = Item) then
      Exit(I);
  Result := -1;
end;

constructor TSimbaObjectList.Create(AFreeObjects: Boolean);
begin
  inherited Create();

  FFreeObjects := AFreeObjects;
end;

destructor TSimbaObjectList.Destroy;
begin
  Clear();

  inherited Destroy();
end;

function TSimbaThreadsafeObjectList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSimbaThreadsafeObjectList.GetFirst: _T;
begin
  FLock.Enter();
  try
    Result := FList.First;
  finally
    FLock.Leave();
  end;
end;

function TSimbaThreadsafeObjectList.GetLast: _T;
begin
  FLock.Enter();
  try
    Result := FList.Last;
  finally
    FLock.Leave();
  end;
end;

function TSimbaThreadsafeObjectList.GetItem(Index: Integer): _T;
begin
  FLock.Enter();
  try
    Result := FList[Index];
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaThreadsafeObjectList.Add(Item: _T);
begin
  FLock.Enter();
  try
    FList.Add(Item);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaThreadsafeObjectList.Delete(Item: _T);
begin
  FLock.Enter();
  try
    FList.Delete(Item);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaThreadsafeObjectList.Lock;
begin
  FLock.Enter();
end;

procedure TSimbaThreadsafeObjectList.UnLock;
begin
  FLock.Leave();
end;

constructor TSimbaThreadsafeObjectList.Create;
begin
  FList := TList.Create();
  FLock := TCriticalSection.Create();
end;

destructor TSimbaThreadsafeObjectList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FLock);

  inherited Destroy();
end;

function TSimbaStack.GetTop: _T;
begin
  if (FCount <= 0) then
    SimbaException('TSimbaStack.GetTop: Stack is empty');

  Result := FArr[FCount - 1];
end;

procedure TSimbaStack.Clear;
begin
  FCount := 0;
end;

procedure TSimbaStack.Push(Item: _T);
begin
  if (FCount >= Length(FArr)) then
    SetLength(FArr, 32 + (Length(FArr) * 2));

  FArr[FCount] := Item;
  Inc(FCount);
end;

function TSimbaStack.Pop: _T;
begin
  if (FCount <= 0) then
    SimbaException('TSimbaStack.GetTop: Stack is empty');

  Dec(FCount);

  Result := FArr[FCount];
end;

procedure TSimbaArrayBuffer.Grow(const Len: Integer);
begin
  FLength := FLength + Len;
  if (FLength < 32) then
    FLength := 32
  else
  if (FLength > 256000) then
    FLength := FLength * 4
  else
    FLength := FLength * 2;

  SetLength(FArr, FLength);
end;

procedure TSimbaPointBufferHelper.Add(const X, Y: Integer);
begin
  if (FCount+1 >= FLength) then
    Grow();

  FArr[FCount].X := X;
  FArr[FCount].Y := Y;

  Inc(FCount);
end;

function TSimbaArrayBuffer.GetItem(const Index: Integer): _T;
begin
  Result := FArr[Index];
end;

procedure TSimbaArrayBuffer.Clear;
begin
  FCount := 0;
end;

procedure TSimbaArrayBuffer.Init(const InitialSize: Integer);
begin
  FLength := InitialSize;
  FCount := 0;

  if (FLength > 0) then
    SetLength(FArr, FLength);
end;

procedure TSimbaArrayBuffer.InitWith(const Values: TArr);
begin
  FArr := Values;
  FLength := Length(FArr);
  FCount := FLength;
end;

procedure TSimbaArrayBuffer.Add(const Value: _T);
begin
  if (FCount >= FLength) then
    Grow();

  FArr[FCount] := Value;
  Inc(FCount);
end;

procedure TSimbaArrayBuffer.Add(const Values: TArr);
var
  Len: Integer;
begin
  Len := Length(Values);

  if (Len > 0) then
  begin
    if (FCount + Len >= FLength) then
      Grow(Len);
    Move(Values[0], FArr[FCount], Len * SizeOf(_T));
    Inc(FCount, Len);
  end;
end;

function TSimbaArrayBuffer.First: _T;
begin
  Result := FArr[0];
end;

function TSimbaArrayBuffer.Last: _T;
begin
  Result := FArr[FCount - 1];
end;

function TSimbaArrayBuffer.Pop: _T;
begin
  Result := FArr[FCount - 1];
  Dec(FCount);
end;

function TSimbaArrayBuffer.ToArray(Copy: Boolean): TArr;
begin
  if Copy then
    Result := System.Copy(FArr, 0, FCount)
  else
  begin
    FLength := FCount;
    SetLength(FArr, FLength);

    Result := FArr;
  end;
end;

class operator TSimbaArrayBuffer.Initialize(var Self: TSelf);
begin
  Self := Default(TSelf);
end;

class operator TSimbaStringBuilder.Initialize(var Self: TSimbaStringBuilder);
begin
  Self := Default(TSimbaStringBuilder);
end;

procedure TSimbaStringBuilder.AppendBuf(const Buffer; Count: Integer);
begin
  if (Count > 0) then
  begin
    EnsureGrowth(Count);

    Move(Buffer, FData[1 + FCount], Count);
    Inc(FCount, Count);
  end;
end;

procedure TSimbaStringBuilder.EnsureGrowth(const Len: Integer);
var
  NewLen: Integer;
begin
  if (FCount + Len >= Length(FData)) then
  begin
    NewLen := Length(FData) + Len;
    if (NewLen < 256) then
      NewLen := 256
    else
    if (NewLen > 256000) then
      NewLen := NewLen * 4
    else
      NewLen := NewLen * 2;

    SetLength(FData, NewLen);
  end;
end;

function TSimbaStringBuilder.GetString: String;
begin
  Result := Copy(FData, 1, FCount);
end;

function TSimbaStringBuilder.GetPeek: Char;
begin
  if (FCount = 0) then
    Exit(#0);
  Result := FData[FCount + 1];
end;

procedure TSimbaStringBuilder.Append(const Str: String);
begin
  if (Length(Str) > 0) then
    AppendBuf(Str[1], Length(Str));
end;

procedure TSimbaStringBuilder.AppendLine(const Str: String);
begin
  Append(Str + LineEnding);
end;

end.

