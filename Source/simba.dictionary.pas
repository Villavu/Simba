unit simba.dictionary;
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
  simba.mufasatypes;


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
  THashIndex = packed record 
    hash,idx:UInt32; 
  end;

  TDictionary<K,V> = class
  public type
    THashFunc    = function(constref key:K): UInt32;
    THashElement = record key:K; val:V; end;
    THashBucket  = array of THashElement;
    TMap         = array of THashBucket;
  private
    FData: TMap;   //map
    FSize: UInt32; //num items
    FHigh: UInt32; //real size
    FResizable: Boolean;
    HashFunc: THashFunc;

    procedure _growRebuild();
    function _addItem(h:UInt32; key:K; value:V; checkResize:Boolean=True): Boolean;
    function _delItem(pos:THashIndex; key:K): Boolean;
  public
    // create
    constructor Create(AHashFunc:THashFunc);

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
    function Hash(constref key:K): UInt32; inline;

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
  end;

// hash-functions to go with the hashtable.
function HashBool(constref k: Boolean): UInt32; inline;
function HashUInt8(constref k: Byte): UInt32; inline;
function HashInt32(constref k: Int32): UInt32; inline;
function HashInt64(constref k: Int64): UInt32; inline;
function HashNative(constref k: NativeInt): UInt32; inline;
function HashPoint(constref k: TPoint): UInt32; inline;
function HashBox(constref k: TBox): UInt32; inline;
function HashAStr(constref k: AnsiString): UInt32; inline;

implementation

uses
  simba.math;

(******************************* Hash Functions *******************************)
function HashBool(constref k: Boolean): UInt32;
begin
  Result := Ord(k);
end;

function HashUInt8(constref k: Byte): UInt32;
begin
  Result := k;
end;

function HashInt32(constref k: Int32): UInt32;
begin
  Result := k;
end;

function HashInt64(constref k: Int64): UInt32;
begin
  Result := k;
end;

function HashNative(constref k: NativeInt): UInt32;
begin
  Result := k;
end;

function HashPoint(constref k: TPoint): UInt32;
begin
  Result := UInt32((k.y * $0f0f1f1f) xor k.x);
end;

function HashBox(constref k: TBox): UInt32;
var a,b:UInt32;
begin
  a := (k.y1 * $0f0f1f1f) xor k.x1;
  b := (k.y2 * $0f0f1f1f) xor k.x2;
  Result := UInt32((a * $0f0f1f1f) xor b);
end;

function HashAStr(constref k: AnsiString): UInt32;
var i: Int32;
begin
  Result := 2166136261;
  for i:=1 to Length(k) do
  begin
    Result := Result xor Byte(k[I]);
    Result := Result * 16777619;
  end;
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


constructor TDictionary<K,V>.Create(AHashFunc: THashFunc);
begin
  FHigh := 0;
  FSize := DICT_MIN_SIZE-1;
  SetLength(FData, DICT_MIN_SIZE);
  
  HashFunc := AHashFunc;
  FResizable := True;
end;


procedure TDictionary<K,V>.SetSize(k:SizeInt);
begin
  if FHigh <> 0 then
    raise Exception.Create('Can''t set size after dictionary has been filled. Call `clear` first');
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
  Result := HashFunc(key) and FSize;
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
    if FData[pos.hash][pos.idx].key = key then
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
    raise Exception.Create('The key does not exist');
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
