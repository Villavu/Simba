{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple managed string map.
  Can be sorted for fast binary-search lookups using the keys hash.
}
unit simba.stringmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  generic TSimbaStringMap<_T> = record
  public type
    PPair = ^TPair;
    TPair = record Key: String; Value: _T; end;
    TValueArray = array of _T;
  private type
    TItem = record
      KeyHash: UInt32;
      Key: String;
      Value: _T;
    end;
    TItemArray = array of TItem;
  private
    FItems: TItemArray;
    FCount: Integer;
    FSorted: Boolean;
    FCaseSens: Boolean;

    class function _StrToValue(Str: String; Default: String): String; overload; static;
    class function _StrToValue(Str: String; Default: Int64): Int64; overload; static;
    class function _StrToValue(Str: String; Default: Pointer): Pointer; overload; static;

    class function _ValueToStr(Val: String): String; overload; static;
    class function _ValueToStr(Val: Int64): String; overload; static;
    class function _ValueToStr(Val: Pointer): String; overload; static;

    class function _BSearch32(var Arr: TItemArray; Hash: UInt32; Lo, Hi: Integer): Integer; static;
    class procedure _Insert32(var Arr: TItemArray; var Index: Integer); static;

    procedure CheckIndex(const Index: Integer); inline;
    function EqualKeys(const Key1, Key2: String): Boolean;

    procedure Sort;

    function GetPair(Index: Integer): TPair;
    function GetKey(Index: Integer): String;
    function GetValue(AKey: String): _T;
    function GetValues(AKey: String): TValueArray;

    procedure SetKey(Index: Integer; NewKey: String);
    procedure SetValue(AKey: String; AValue: _T);
    procedure SetSorted(AValue: Boolean);
  public
    InvalidVal: _T;

    property Count: Integer read FCount;
    property CaseSens: Boolean read FCaseSens write FCaseSens;
    property Sorted: Boolean read FSorted write SetSorted;

    property Pair[Index: Integer]: TPair read GetPair;
    property Key[Index: Integer]: String read GetKey write SetKey;
    property Value[AKey: String]: _T read GetValue write SetValue;
    property Values[AKey: String]: TValueArray read GetValues;

    function Exists(AKey: String): Boolean;
    function Add(AKey: String; AValue: _T): Integer;
    function IndexOf(AKey: String): Integer;
    function IndicesOf(AKey: String): TIntegerArray;
    procedure Delete(Index: Integer); overload;
    procedure Delete(AKey: String); overload;
    procedure DeleteAll(AKey: String);
    procedure Clear;

    procedure Load(FileName: String; Sep: String = '=');
    procedure Save(FileName: String; Sep: String = '=');

    function ToString: String;

    class operator Initialize(var Self: TSimbaStringMap);
  end;

  PStringMap = ^TStringMap;
  PStringPointerMap = ^TStringPointerMap;
  PStringIntMap = ^TStringIntMap;

  TStringMap = specialize TSimbaStringMap<String>;
  TStringPointerMap = specialize TSimbaStringMap<Pointer>;
  TStringIntMap = specialize TSimbaStringMap<Int64>;

implementation

uses
  simba.arraybuffer, simba.files, simba.algo_sort, simba.stringbuilder;

class function TSimbaStringMap._StrToValue(Str: String; Default: String): String; overload;
begin
  Result := Str;
end;

class function TSimbaStringMap._StrToValue(Str: String; Default: Int64): Int64; overload;
begin
  Result := StrToInt64Def(Str, Default);
end;

class function TSimbaStringMap._StrToValue(Str: String; Default: Pointer): Pointer; overload;
begin
  Result := Pointer(StrToInt64Def(Str.Replace('0x', '$'), Int64(Default)));
end;

class function TSimbaStringMap._ValueToStr(Val: String): String; overload;
begin
  Result := Val;
end;

class function TSimbaStringMap._ValueToStr(Val: Int64): String; overload;
begin
  Result := IntToStr(Val);
end;

class function TSimbaStringMap._ValueToStr(Val: Pointer): String; overload;
begin
  Result := '0x' + IntToHex(PtrUInt(Val), 1);
end;

class function TSimbaStringMap._BSearch32(var Arr: TItemArray; Hash: UInt32; Lo, Hi: Integer): Integer;
var
  mVal: UInt32;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex].KeyHash;
    if (Hash = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1].KeyHash = Hash) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Hash < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

class procedure TSimbaStringMap._Insert32(var Arr: TItemArray; var Index: Integer);
var
  Item: TItem;
  Hi: Integer;
  I: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item.KeyHash < Arr[Index - 1].KeyHash) then
  begin
    Index := _BSearch32(Arr, Item.KeyHash, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    for i := Hi downto Index + 1 do
      Arr[i] := Arr[i-1];
    Arr[Index] := Item;
  end;
end;

procedure TSimbaStringMap.SetSorted(AValue: Boolean);
begin
  if (FSorted = AValue) then
    Exit;
  FSorted := AValue;
  if FSorted then
    Sort();
end;

function TSimbaStringMap.EqualKeys(const Key1, Key2: String): Boolean;
begin
  if CaseSens then
    Result := (Key1 = Key2)
  else
    Result := SameText(Key1, Key2);
end;

function TSimbaStringMap.GetPair(Index: Integer): TPair;
begin
  CheckIndex(Index);

  Result.Key := FItems[Index].Key;
  Result.Value := FItems[Index].Value;
end;

function TSimbaStringMap.GetValue(AKey: String): _T;
var
  Index: Integer;
begin
  Index := IndexOf(AKey);
  if (Index > -1) then
    Result := FItems[Index].Value
  else
    Result := InvalidVal;
end;

procedure TSimbaStringMap.SetValue(AKey: String; AValue: _T);
var
  Index: Integer;
begin
  Index := IndexOf(AKey);
  if (Index > -1) then
    FItems[Index].Value := AValue;
end;

function TSimbaStringMap.GetValues(AKey: String): TValueArray;
var
  Indices: TIntegerArray;
  I: Integer;
begin
  Indices := Self.IndicesOf(AKey);

  SetLength(Result, Length(Indices));
  for I := 0 to High(Result) do
    Result[I] := FItems[Indices[I]].Value;
end;

function TSimbaStringMap.Add(AKey: String; AValue: _T): Integer;
begin
  if (Self.Count >= Length(FItems)) then
    SetLength(FItems, 4 + (FCount * 2));

  Result := FCount;

  Inc(FCount);
  if CaseSens then
    FItems[Result].KeyHash := AKey.Hash()
  else
    FItems[Result].KeyHash := AKey.ToUpper().Hash();
  FItems[Result].Key := AKey;
  FItems[Result].Value := AValue;

  if FSorted then
    _Insert32(FItems, Result);
end;

function TSimbaStringMap.IndexOf(AKey: String): Integer;
var
  Hash: UInt32;
  I: Integer;
begin
  if CaseSens then
    Hash := AKey.Hash()
  else
    Hash := AKey.ToUpper().Hash();

  if FSorted then
  begin
    Result := _BSearch32(FItems, Hash, 0, FCount - 1);
    if (Result < 0) or (not EqualKeys(FItems[Result].Key, AKey)) then
      Result := -1;
  end else
  begin
    for I := 0 to FCount - 1 do
      if (FItems[I].KeyHash = Hash) and EqualKeys(FItems[I].Key, AKey) then
        Exit(I);

    Result := -1;
  end;
end;

function TSimbaStringMap.IndicesOf(AKey: String): TIntegerArray;
var
  Hash: UInt32;
  Index: Integer;
  Buffer: TSimbaIntegerBuffer;
begin
  if CaseSens then
    Hash := AKey.Hash()
  else
    Hash := AKey.ToUpper().Hash();

  if FSorted then
  begin
    Index := _BSearch32(FItems, Hash, 0, FCount - 1);
    if (Index >= 0) then
      while (Index < FCount) and (FItems[Index].KeyHash = Hash) do
      begin
        if EqualKeys(FItems[Index].Key, AKey) then
          Buffer.Add(Index);
        Inc(Index);
      end;
  end else
  begin
    for Index := 0 to FCount - 1 do
      if (FItems[Index].KeyHash = Hash) and EqualKeys(FItems[Index].Key, AKey) then
        Buffer.Add(Index);
  end;

  Result := Buffer.ToArray(False);
end;

procedure TSimbaStringMap.Delete(Index: Integer);
var
  I: Integer;
begin
  CheckIndex(Index);
  for I := Index+1 to FCount - 1 do
    FItems[I-1] := FItems[I];

  Dec(FCount);
end;

procedure TSimbaStringMap.Delete(AKey: String);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if (I > -1) then
    Delete(I);
end;

procedure TSimbaStringMap.DeleteAll(AKey: String);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  while (I > -1) do
  begin
    Delete(I);
    I := IndexOf(AKey);
  end;
end;

procedure TSimbaStringMap.Clear;
begin
  FCount := 0;
end;

function TSimbaStringMap.Exists(AKey: String): Boolean;
begin
  Result := IndexOf(AKey) > -1;
end;

procedure TSimbaStringMap.Sort;
var
  Weights: array of UInt32;
  I: Integer;
begin
  SetLength(Weights, Length(FItems));
  for I := 0 to FCount - 1 do
    Weights[I] := FItems[I].KeyHash;

  specialize QuickSort<TItem, UInt32>(FItems, Weights, 0, FCount - 1, True);
end;

function TSimbaStringMap.GetKey(Index: Integer): String;
begin
  CheckIndex(Index);

  Result := FItems[Index].Key;
end;

procedure TSimbaStringMap.SetKey(Index: Integer; NewKey: String);
begin
  CheckIndex(Index);

  if Sorted then
    SimbaException('Cannot change a key of a sorted map');

  FItems[Index].Key := NewKey;
  FItems[Index].KeyHash := NewKey.Hash();
end;

procedure TSimbaStringMap.CheckIndex(const Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('StringPairList: Index %d is out of range (%d..%d)', [Index, 0, FCount-1]);
end;

procedure TSimbaStringMap.Load(FileName: String; Sep: String = '=');
var
  Line: String;
  Pieces: TStringArray;
  wasSorted: Boolean;
begin
  Clear();

  wasSorted := Sorted;
  if wasSorted then
    Sorted := False;

  for Line in TSimbaFile.FileReadLines(FileName) do
  begin
    Pieces := Line.Partition(Sep);
    if (Pieces[0] <> '') and (Pieces[1] <> '') and (Pieces[2] <> '') then
      Add(Pieces[0], _StrToValue(Pieces[2], InvalidVal));
  end;

  if wasSorted then
    Sorted := True;
end;

procedure TSimbaStringMap.Save(FileName: String; Sep: String);
var
  I: Integer;
  Builder: TSimbaStringBuilder;
begin
  for I := 0 to FCount - 1 do
    Builder.AppendLine(FItems[I].Key + Sep + _ValueToStr(FItems[I].Value));

  TSimbaFile.FileWrite(FileName, Builder.Str);
end;

function TSimbaStringMap.ToString: String;
var
  Builder: TSimbaStringBuilder;
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if (I > 0) then
      Builder.Append(', ');
    Builder.Append('[Key=' + FItems[I].Key + ', Value=' + _ValueToStr(FItems[I].Value) + ']');
  end;

  Result := 'Count=%d, CaseSens=%s, Sorted=%s, Pairs=[%s]'.Format([FCount, FCaseSens.ToString(), FSorted.ToString(), Builder.Str]);
end;

class operator TSimbaStringMap.Initialize(var Self: TSimbaStringMap);
begin
  Self := Default(TSimbaStringMap);
end;

end.

