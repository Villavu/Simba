unit simba.container_dict;
{==============================================================================]
  Hashmap copyright (c) 2022, Jarl `slacky` Holta
[==============================================================================}
interface
(*
 Some very basic hashing.. You need to write your own hash function if you want
 the Key to be anything else then Integer/string/TPoint/TBox.
*)
{$mode delphi}{$H+}
{$inline on}

uses
  Classes, SysUtils,
  simba.base;


const
  // Minimum size of the dictionary - must be a power of 2
  DICT_MIN_SIZE = 512;

  // The basic growth strategy
  // EG: 2 means that it will double the size every time it resizes
  DICT_GROWTH_STRAT = 2;

  // The growth strategy when the dict grows beyond 50,000 items
  // EG: 4 means that it will quadruple the size every time it resizes
  DICT_GROWTH_STRAT_50K = 4;


type
  THashIndex = record
    hash,idx:UInt32; 
  end;

  TDictionary<K,V> = class
  public type
    THashFunc    = function(constref key: K): UInt32;
    TCompareFunc = function(constref A,B: K): Boolean;
    THashElement = record key:K; val:V; end;
    THashBucket  = array of THashElement;
    TMap         = array of THashBucket;
  private
    FData: TMap;   //map
    FSize: UInt32; //num items
    FHigh: UInt32; //real size
    FResizable: Boolean;

    FHashFunc: THashFunc;
    FHashData: Boolean;

    FCompareFunc: TCompareFunc;
    FCompareData: Boolean;

    procedure _growRebuild();
    function _addItem(h:UInt32; key:K; value:V; checkResize:Boolean=True): Boolean;
    function _delItem(pos:THashIndex; key:K): Boolean;

    procedure DetermineHashFunc;
    procedure DetermineCompareFunc;
  public
    // create
    constructor Create(AHashFunc: THashFunc = nil; ACompareFunc: TCompareFunc = nil);

    // Access hashmap-items directly [r]
    // value := Dict.Items[hash,idx]
    property Items:TMap read FData;

    // Sets whether the map can resize, or not (Default = True);
    property Resizable:Boolean read FResizable write FResizable;

    // Sets the base-size of the dictionary
    // Can be used to reduce the number of rebuilds.
    // Can't be used after items has been added to the dict.
    procedure SetSize(k:SizeInt);

    // Clear the dictionary - removes all elements, and sizes it down to 0.
    procedure Clear; inline;

    // function used to hash the key
    function Hash(constref key:K): UInt32;
    function Compare(constref A,B: K): Boolean;

    // Returns position `pos` of they item `key`
    // it can then be used with the the read-only property `Items`
    function Find(constref key:K; out pos:THashIndex): Boolean; inline;

    // Add a key-value pair to the hashmap. If the key already exists it's value
    // will be changed to `value`.
    // Same as `dict[key] := value`.
    procedure AddFast(key:K; value:V); inline;

    // Look up a key. Will raise an exception if it's not found.
    // Same as `value := dict[key]`
    function GetFast(key:K): V; inline;
    
    // Add a key-value pair to the hashmap. Will not modify existing keys
    // instead return False.
    function Add(key:K; value:V): Boolean; inline;

    // Add a key-value pair to the hashmap. If the key already exists it's value
    // will be changed to `value`.
    // Returns True it already existed.
    function AddOrModify(key:K; value:V): Boolean; inline;

    // Look up a key. Returns False if it's not found.
    function Get(key:K; out value:V): Boolean; inline;

    // Look up a key. Returns the given default value if not found.
    function GetDef(key:K; default:V): V; inline;

    // Removes the given key, will return False if it doesn't exist.
    //
    // Will never reduce the hashmap-size, only the bucket-size.
    // This is because with a number of removal, there will often come
    // a nearly identical number of a new additions. And so we avoid expensive
    // resizing.
    function Remove(key:K): Boolean; inline;

    // Check if a key exists.
    function Contains(key:K): Boolean; inline;

    // property item (default) - used to index the dictionary as if it's
    // a regualar array like structure. Can be used to add new key-value pairs.
    //
    // > mydict['hello world'] := 123;
    // > value := mydict['hello world'];
    property Item[key:K]: V read GetFast write AddFast; default;
    property Size:UInt32 read FSize;
    property Data:TMap read FData;

    class function HashBool(constref k: Boolean): UInt32; static;
    class function HashInt8(constref k: Byte): UInt32; static;
    class function HashInt16(constref k: Int16): UInt32; static;
    class function HashInt32(constref k: Int32): UInt32; static;
    class function HashInt64(constref k: Int64): UInt32; static;
    class function HashString(constref k: String): UInt32; static;

    class function CompareBool(constref A,B: Boolean): Boolean; static;
    class function CompareInt8(constref A,B: Byte): Boolean; static;
    class function CompareInt16(constref A,B: Int16): Boolean; static;
    class function CompareInt32(constref A,B: Int32): Boolean; static;
    class function CompareInt64(constref A,B: Int64): Boolean; static;
    class function CompareString(constref A,B: String): Boolean; static;
  end;

implementation

uses
  TypInfo,
  simba.math, simba.hash_murmur;

class function TDictionary<K,V>.HashBool(constref k: Boolean): UInt32;
begin
  Result := UInt32(k);
end;

class function TDictionary<K,V>.HashInt8(constref k: Byte): UInt32;
begin
  Result := UInt32(k);
end;

class function TDictionary<K,V>.HashInt16(constref k: Int16): UInt32;
begin
  Result := UInt32(k);
end;

class function TDictionary<K,V>.HashInt32(constref k: Int32): UInt32;
begin
  Result := UInt32(k);
end;

class function TDictionary<K,V>.HashInt64(constref k: Int64): UInt32;
begin
  Result := UInt32(k);
end;

// FNV
class function TDictionary<K, V>.HashString(constref k: String): UInt32;
var
  I: Int32;
begin
  {$PUSH}
  {$Q-}{$R-}
  Result := 2166136261;
  for I := 1 to Length(k) do
  begin
    Result := Result xor Byte(k[I]);
    Result := Result * 16777619;
  end;
  {$POP}
end;

class function TDictionary<K, V>.CompareBool(constref A, B: Boolean): Boolean;
begin
  Result := A = B;
end;

class function TDictionary<K, V>.CompareInt8(constref A, B: Byte): Boolean;
begin
  Result := A = B;
end;

class function TDictionary<K, V>.CompareInt16(constref A, B: Int16): Boolean;
begin
  Result := A = B;
end;

class function TDictionary<K, V>.CompareInt32(constref A, B: Int32): Boolean;
begin
  Result := A = B;
end;

class function TDictionary<K, V>.CompareInt64(constref A, B: Int64): Boolean;
begin
  Result := A = B;
end;

class function TDictionary<K, V>.CompareString(constref A, B: String): Boolean;
begin
  Result := A = B;
end;

(******************************************************************************)
{
  TDictionary<K,V>

  A simple datastructure that allows for efficient indexing using "keys".
  The key can be just about any datatype, as long as it can be hashed in a
  useful manner:
  Hashing a key should always give the same result, and preferably be fast
  as the hash-value will be computed every time you index.
}

procedure TDictionary<K, V>.DetermineHashFunc;
begin
  if IsManagedType(K) then
  begin
    if (GetTypeKind(K) = tkAString) then
      FHashFunc := @HashString;
  end else
  begin
    if (GetTypeKind(K) in [tkInteger, tkInt64, tkQWord, tkBool, tkPointer, tkRecord]) then
      case SizeOf(K) of
        1: FHashFunc := @HashInt8;
        2: FHashFunc := @HashInt16;
        4: FHashFunc := @HashInt32;
        8: FHashFunc := @HashInt64;
        else
          FHashData := True;
      end;
  end;

  if (@FHashFunc = nil) and (not FHashData) then
    SimbaException('HashFunc required');
end;

procedure TDictionary<K, V>.DetermineCompareFunc;
begin
  if IsManagedType(K) then
  begin
    if (GetTypeKind(K) = tkAString) then
      FCompareFunc := @HashString;
  end else
  begin
    if (GetTypeKind(K) in [tkInteger, tkInt64, tkQWord, tkBool, tkPointer, tkRecord]) then
      case SizeOf(K) of
        1: FCompareFunc := @CompareInt8;
        2: FCompareFunc := @CompareInt16;
        4: FCompareFunc := @CompareInt32;
        8: FCompareFunc := @CompareInt64;
        else
          FCompareData := True;
      end;
  end;

  if (@FCompareFunc = nil) and (not FCompareData) then
    SimbaException('CompareFunc required');
end;

constructor TDictionary<K,V>.Create(AHashFunc: THashFunc; ACompareFunc: TCompareFunc);
begin
  inherited Create();

  FHashFunc := AHashFunc;
  FCompareFunc := ACompareFunc;

  if (@FHashFunc = nil) then DetermineHashFunc();
  if (@FCompareFunc = nil) then DetermineCompareFunc();

  FHigh := 0;
  FSize := DICT_MIN_SIZE - 1;
  SetLength(FData, DICT_MIN_SIZE);
  FResizable := True;
end;


procedure TDictionary<K,V>.SetSize(k:SizeInt);
begin
  if FHigh <> 0 then
    SimbaException('Can''t set size after dictionary has been filled. Call `clear` first');
  FSize := Max(DICT_MIN_SIZE-1, NextPower2(k-1));
  SetLength(FData, FSize+1);
end;


procedure TDictionary<K,V>.Clear;
begin
  SetLength(FData, 0);
  FHigh := 0;
  FSize := DICT_MIN_SIZE-1;
  SetLength(FData, DICT_MIN_SIZE);
end;


function TDictionary<K,V>.Hash(constref key: K): UInt32;
begin
  if FHashData then
    Result := UInt32(TMurmur2aLE.HashBuf(@key, SizeOf(K)) and FSize)
  else
    Result := UInt32(FHashFunc(key) and FSize);
end;

function TDictionary<K, V>.Compare(constref A, B: K): Boolean;
begin
  if FCompareData then
    Result := CompareMem(@A, @B, SizeOf(K))
  else
    Result := FCompareFunc(A, B);
end;


procedure TDictionary<K,V>._growRebuild();
var
  i,j,k,hi: Int32;
  temp: array of THashElement;
  hval,strategy: UInt32;
begin
  SetLength(temp, FHigh);
  k := 0;
  for i:=0 to FSize do
  begin
    for j:=0 to High(FData[i]) do
    begin
      temp[k] := FData[i][j];
      inc(k);
    end;
    SetLength(FData[i], 0);
  end;

  strategy := DICT_GROWTH_STRAT;
  if FHigh >= 50000 then strategy := DICT_GROWTH_STRAT_50K;

  FSize := ((FSize+1) * strategy)-1;
  SetLength(FData, FSize+1);
  hi := FHigh;
  FHigh := 0;
  for i:=0 to hi-1 do
  begin
    hval := self.hash(temp[i].key);
    self._addItem(hval, temp[i].key, temp[i].val, False);
  end;
end;


function TDictionary<K,V>._addItem(h:UInt32; key: K; value:V; checkResize:Boolean=True): Boolean;
var l: Int32;
begin
  l := Length(FData[h]);
  SetLength(FData[h], l+1);
  FData[h][l].key := key;
  FData[h][l].val := value;
  Inc(FHigh);

  if FResizable and checkResize and (FHigh > FSize div 2) then
    self._growRebuild();

  Result := True;
end;


function TDictionary<K,V>._delItem(pos:THashIndex; key: K): Boolean;
var
  l: Int32;
begin
  l := High(FData[pos.hash]);
  if pos.idx <> l then
    FData[pos.hash][pos.idx] := FData[pos.hash][l];
  SetLength(FData[pos.hash], l);
  Dec(FHigh);
  Result := True;
end;

function TDictionary<K,V>.Find(constref key: K; out pos:THashIndex): Boolean;
var
  l: Int32;
begin
  pos.hash := Hash(key);
  l := High(FData[pos.hash]);
  pos.idx := 0;
  while pos.idx <= l do
  begin
    if Compare(FData[pos.hash][pos.idx].key, key) then
      Exit(True);
    Inc(pos.idx);
  end;
  Result := False;
end;

procedure TDictionary<K,V>.AddFast(key: K; value:V);
var pos: THashIndex;
begin
  if Find(key, pos) then
    FData[pos.hash][pos.idx].val := value
  else
    _addItem(pos.hash, key, value);
end;


function TDictionary<K,V>.GetFast(key: K): V;
var pos: THashIndex;
begin
  if not Find(key, pos) then
    SimbaException('The key does not exist');
  Result := FData[pos.hash][pos.idx].val;
end;


function TDictionary<K,V>.Add(key: K; value:V): Boolean;
var pos: THashIndex;
begin
  if Find(key, pos) then Exit(False);
  Result := _addItem(pos.hash, key, value);
end;


function TDictionary<K,V>.AddOrModify(key: K; value:V): Boolean;
var pos: THashIndex;
begin
  if Find(key, pos) then
  begin
    FData[pos.hash][pos.idx].val := value;
    Result := True;
  end else
  begin
    _addItem(pos.hash, key, value);
    Result := False;
  end;
end;


function TDictionary<K,V>.Get(key: K; out value: V): Boolean;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(False);
  Value := FData[pos.hash][pos.idx].val;
  Result := True;
end;


function TDictionary<K,V>.GetDef(key: K; default:V): V;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(default);
  Result := FData[pos.hash][pos.idx].val;
end;


function TDictionary<K,V>.Remove(key: K): Boolean;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(False);

  Result := _delItem(pos, key);
end;


function TDictionary<K,V>.Contains(key: K): Boolean;
var idx: THashIndex;
begin
  Result := Find(key, idx);
end;


end.
