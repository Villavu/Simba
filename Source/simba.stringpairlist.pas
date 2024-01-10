{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple managed list of string pairs (key,value).
  Can be sorted for fast binary-search lookups.
}
unit simba.stringpairlist;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  TStringPairItem = record
    KeyHash: UInt32;
    Key, Value: String;
  end;
  TStringPairItemArray = array of TStringPairItem;

  PStringPair = ^TStringPair;
  TStringPair = record Key, Value: String; end;

  PStringPairList = ^TStringPairList;
  TStringPairList = record
  private
    FItems: TStringPairItemArray;
    FCount: Integer;
    FSorted: Boolean;
    FCaseSens: Boolean;

    procedure CheckIndex(const Index: Integer); inline;
    function EqualKeys(const Key1, Key2: String): Boolean;

    procedure Sort;

    function GetPair(Index: Integer): TStringPair;
    function GetKey(Index: Integer): String;
    function GetValue(AKey: String): String;
    function GetValues(AKey: String): TStringArray;

    procedure SetKey(Index: Integer; NewKey: String);
    procedure SetValue(AKey: String; AValue: String);
    procedure SetSorted(AValue: Boolean);
  public
    property Count: Integer read FCount;
    property CaseSens: Boolean read FCaseSens write FCaseSens;
    property Sorted: Boolean read FSorted write SetSorted;

    property Pair[Index: Integer]: TStringPair read GetPair;
    property Key[Index: Integer]: String read GetKey write SetKey;
    property Value[AKey: String]: String read GetValue write SetValue;
    property Values[AKey: String]: TStringArray read GetValues;

    function Exists(AKey: String): Boolean;
    function Add(AKey, AValue: String): Integer;
    function IndexOf(AKey: String): Integer;
    function IndicesOf(AKey: String): TIntegerArray;
    procedure Delete(Index: Integer); overload;
    procedure Delete(AKey: String); overload;
    procedure DeleteAll(AKey: String);
    procedure Clear;

    procedure Load(FileName: String; Sep: String = '=');
    procedure Save(FileName: String; Sep: String = '=');

    function ToString: String;

    class operator Initialize(var Self: TStringPairList);
  end;

implementation

uses
  simba.arraybuffer, simba.files, simba.algo_sort, simba.stringbuilder;

function _BSearch32(var Arr: TStringPairItemArray; Hash: UInt32; Lo, Hi: Integer): Integer;
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

procedure _Insert32(var Arr: TStringPairItemArray; var Index: Integer);
var
  Item: TStringPairItem;
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

procedure TStringPairList.SetSorted(AValue: Boolean);
begin
  if (FSorted = AValue) then
    Exit;
  FSorted := AValue;
  if FSorted then
    Sort();
end;

function TStringPairList.EqualKeys(const Key1, Key2: String): Boolean;
begin
  if CaseSens then
    Result := (Key1 = Key2)
  else
    Result := SameText(Key1, Key2);
end;

function TStringPairList.GetPair(Index: Integer): TStringPair;
begin
  CheckIndex(Index);

  Result.Key := FItems[Index].Key;
  Result.Value := FItems[Index].Value;
end;

function TStringPairList.GetValue(AKey: String): String;
var
  Index: Integer;
begin
  Index := IndexOf(AKey);
  if (Index > -1) then
    Result := FItems[Index].Value
  else
    Result := '';
end;

procedure TStringPairList.SetValue(AKey: String; AValue: String);
var
  Index: Integer;
begin
  Index := IndexOf(AKey);
  if (Index > -1) then
    FItems[Index].Value := AValue;
end;

function TStringPairList.GetValues(AKey: String): TStringArray;
var
  Indices: TIntegerArray;
  I: Integer;
begin
  Indices := Self.IndicesOf(AKey);

  SetLength(Result, Length(Indices));
  for I := 0 to High(Result) do
    Result[I] := FItems[Indices[I]].Value;
end;

function TStringPairList.Add(AKey, AValue: String): Integer;
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

function TStringPairList.IndexOf(AKey: String): Integer;
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

function TStringPairList.IndicesOf(AKey: String): TIntegerArray;
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

procedure TStringPairList.Delete(Index: Integer);
var
  I: Integer;
begin
  CheckIndex(Index);
  for I := Index+1 to FCount - 1 do
    FItems[I-1] := FItems[I];

  Dec(FCount);
end;

procedure TStringPairList.Delete(AKey: String);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if (I > -1) then
    Delete(I);
end;

procedure TStringPairList.DeleteAll(AKey: String);
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

procedure TStringPairList.Clear;
begin
  FCount := 0;
end;

function TStringPairList.Exists(AKey: String): Boolean;
begin
  Result := IndexOf(AKey) > -1;
end;

procedure TStringPairList.Sort;
var
  Weights: array of UInt32;
  I: Integer;
begin
  SetLength(Weights, Length(FItems));
  for I := 0 to FCount - 1 do
    Weights[I] := FItems[I].KeyHash;

  specialize QuickSortWeighted<TStringPairItem, UInt32>(FItems, Weights, 0, FCount - 1, True);
end;

function TStringPairList.GetKey(Index: Integer): String;
begin
  CheckIndex(Index);

  Result := FItems[Index].Key;
end;

procedure TStringPairList.SetKey(Index: Integer; NewKey: String);
var
  Val: String;
begin
  CheckIndex(Index);

  if Sorted then
  begin
    Val := FItems[Index].Value;
    Delete(Index);
    Add(NewKey, Val);
  end else
  begin
    FItems[Index].Key := NewKey;
    FItems[Index].KeyHash := NewKey.Hash();
  end;
end;

procedure TStringPairList.CheckIndex(const Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('StringPairList: Index %d is out of range (%d..%d)', [Index, 0, FCount-1]);
end;

procedure TStringPairList.Load(FileName: String; Sep: String = '=');
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
      Add(Pieces[0], Pieces[2]);
  end;

  if wasSorted then
    Sorted := True;
end;

procedure TStringPairList.Save(FileName: String; Sep: String);
var
  I: Integer;
  Builder: TSimbaStringBuilder;
begin
  for I := 0 to FCount - 1 do
    Builder.AppendLine(FItems[I].Key + Sep + FItems[I].Value);

  TSimbaFile.FileWrite(FileName, Builder.Str);
end;

function TStringPairList.ToString: String;
var
  Builder: TSimbaStringBuilder;
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if (I > 0) then
      Builder.Append(', ');
    Builder.Append('"' + FItems[I].Key + '"="' + FItems[I].Value + '"');
  end;

  Result := 'Count=%d, CaseSens=%s, Sorted=%s, Pairs=[%s]'.Format([FCount, FCaseSens.ToString(TUseBoolStrs.True), FSorted.ToString(TUseBoolStrs.True), Builder.Str]);
end;

class operator TStringPairList.Initialize(var Self: TStringPairList);
begin
  Self := Default(TStringPairList);
end;

end.

