{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple managed string map.
  Can be sorted for fast binary-search lookups using the keys hash.
}
unit simba.container_stringmap;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Variants,
  simba.base;

type
  TSimbaStringMap = record
  private type
    TItem = record
      KeyHash: UInt32;
      Key: String;
      Value: Variant;
    end;
    TItemArray = array of TItem;
  private
    FItems: TItemArray;
    FCount: Integer;
    FSorted: Boolean;
    FCaseSens: Boolean;
    FAllowDuplicates: Boolean;

    class function _BSearch32(var Arr: TItemArray; const Hash: UInt32; Lo, Hi: Integer): Integer; static;
    class procedure _Insert32(var Arr: TItemArray; var Index: Integer); static;

    procedure CheckIndex(const Index: Integer); inline;
    function EqualKeys(const Key1, Key2: String): Boolean;

    procedure Sort;

    function GetValue(Key: String): Variant;
    function GetValues(Key: String): TVariantArray;
    function GetKeyFromIndex(Index: Integer): String;
    function GetValueFromIndex(Index: Integer): Variant;

    procedure SetKeyFromIndex(Index: Integer; AValue: String);
    procedure SetValueFromIndex(Index: Integer; AValue: Variant);
    procedure SetValue(Key: String; AValue: Variant);
    procedure SetSorted(AValue: Boolean);
  public
    property Count: Integer read FCount;
    property CaseSens: Boolean read FCaseSens write FCaseSens;
    property Sorted: Boolean read FSorted write SetSorted;
    property AllowDuplicates: Boolean read FAllowDuplicates write FAllowDuplicates;

    property Value[Key: String]: Variant read GetValue write SetValue;
    property Values[Key: String]: TVariantArray read GetValues;
    property KeyFromIndex[Index: Integer]: String read GetKeyFromIndex write SetKeyFromIndex;
    property ValueFromIndex[Index: Integer]: Variant read GetValueFromIndex write SetValueFromIndex;

    function IndexOf(AKey: String): Integer;
    function IndicesOf(AKey: String): TIntegerArray;

    function Exists(AKey: String): Boolean;
    procedure Add(AKey: String; AValue: Variant);
    procedure Delete(Index: Integer); overload;
    procedure Delete(AKey: String); overload;
    procedure Clear;

    procedure Load(FileName: String; VarType: EVariantType; Sep: String = '=');
    procedure Save(FileName: String; Sep: String = '=');

    function ToString: String;

    class operator Initialize(var Self: TSimbaStringMap);
  end;

implementation

uses
  simba.containers, simba.fs, simba.array_algorithm, simba.vartype_string;

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

class function TSimbaStringMap._BSearch32(var Arr: TItemArray; const Hash: UInt32; Lo, Hi: Integer): Integer;
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

    for I := Hi downto Index + 1 do
      Arr[I] := Arr[I-1];
    Arr[Index] := Item;
  end;
end;

function TSimbaStringMap.GetValue(Key: String): Variant;
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);
  if (Index = -1) then
    SimbaException('Key "%s" does not exist', [Key]);

  Result := FItems[Index].Value;
end;

function TSimbaStringMap.GetValues(Key: String): TVariantArray;
var
  Indices: TIntegerArray;
  I: Integer;
begin
  Indices := Self.IndicesOf(Key);

  SetLength(Result, Length(Indices));
  for I := 0 to High(Result) do
    Result[I] := FItems[Indices[I]].Value;
end;

function TSimbaStringMap.GetKeyFromIndex(Index: Integer): String;
begin
  CheckIndex(Index);

  Result := FItems[Index].Key;
end;

function TSimbaStringMap.GetValueFromIndex(Index: Integer): Variant;
begin
  CheckIndex(Index);

  Result := FItems[Index].Value;
end;

procedure TSimbaStringMap.SetKeyFromIndex(Index: Integer; AValue: String);
begin
  if FSorted then
    SimbaException('Cannot change the key of a sorted map');
  CheckIndex(Index);

  FItems[Index].Key := AValue;
end;

procedure TSimbaStringMap.SetValueFromIndex(Index: Integer; AValue: Variant);
begin
  CheckIndex(Index);

  FItems[Index].Value := AValue;
end;

procedure TSimbaStringMap.SetValue(Key: String; AValue: Variant);
var
  Index: Integer;
begin
  Index := IndexOf(Key);
  if (Index = -1) then
    SimbaException('Key "%s" does not exist', [Key]);

  FItems[Index].Value := AValue;
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

procedure TSimbaStringMap.Add(AKey: String; AValue: Variant);
var
  Index: Integer;
begin
  if (FCount >= Length(FItems)) then
    SetLength(FItems, 4 + (FCount * 2));

  Index := FCount;

  Inc(FCount);
  if CaseSens then
    FItems[Index].KeyHash := AKey.Hash()
  else
    FItems[Index].KeyHash := AKey.ToUpper().Hash();
  FItems[Index].Key := AKey;
  FItems[Index].Value := AValue;

  if FSorted then
    _Insert32(FItems, Index);
end;

procedure TSimbaStringMap.Sort;
var
  Weights: array of UInt32;
  I: Integer;
begin
  SetLength(Weights, Length(FItems));
  for I := 0 to FCount - 1 do
    Weights[I] := FItems[I].KeyHash;

  specialize TArraySortWeighted<TItem, UInt32>.QuickSort(FItems, Weights, 0, FCount - 1, True);
end;

procedure TSimbaStringMap.CheckIndex(const Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Index %d is out of range (%d..%d)', [Index, 0, FCount-1]);
end;

procedure TSimbaStringMap.Load(FileName: String; VarType: EVariantType; Sep: String);
var
  Line: String;
  Pieces: TStringArray;
  wasSorted: Boolean;
  Val: Variant;
begin
  if (not (VarType in [EVariantType.Boolean, EVariantType.AString..EVariantType.WString, EVariantType.Single..EVariantType.Double, EVariantType.Int8..EVariantType.UInt64])) then
    SimbaException('Cannot load vartype');

  Clear();

  wasSorted := Sorted;
  if wasSorted then
    Sorted := False;

  for Line in TSimbaFile.FileReadLines(FileName) do
  begin
    Pieces := Line.Partition(Sep);
    if (Pieces[0] <> '') and (Pieces[1] <> '') and (Pieces[2] <> '') then
    try
      case VarType of
        EVariantType.Boolean: Val := Pieces[2].ToBoolean();

        EVariantType.AString: Val := AnsiString(Pieces[2]);
        EVariantType.UString: Val := UnicodeString(Pieces[2]);
        EVariantType.WString: Val := WideString(Pieces[2]);

        EVariantType.Single:  Val := Pieces[2].ToSingle();
        EVariantType.Double:  Val := Pieces[2].ToDouble();

        EVariantType.Int8:    Val := Int8(Pieces[2].ToInt64());
        EVariantType.Int16:   Val := Int16(Pieces[2].ToInt64());
        EVariantType.Int32:   Val := Int32(Pieces[2].ToInt64());
        EVariantType.Int64:   Val := Int64(Pieces[2].ToInt64());

        EVariantType.UInt8:   Val := UInt8(Pieces[2].ToInt64());
        EVariantType.UInt16:  Val := UInt16(Pieces[2].ToInt64());
        EVariantType.UInt32:  Val := UInt32(Pieces[2].ToInt64());
        EVariantType.UInt64:  Val := UInt64(Pieces[2].ToInt64());
      end;

      Add(Pieces[0], Val);
    except
    end;
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
    Builder.AppendLine(FItems[I].Key + Sep + VarToStr(FItems[I].Value));

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
      Builder.Append(LineEnding);
    Builder.Append('  [Key=' + FItems[I].Key + ', Value=' + VarToStr(FItems[I].Value) + ']');
  end;

  Result := Format('Count=%d, CaseSens=%s, Sorted=%s, AllowDuplicates=%s', [FCount, FCaseSens.ToString(), FSorted.ToString(), FAllowDuplicates.ToString()]);
  if (FCount > 0) then
    Result := Result + LineEnding + 'Items:' + LineEnding + Builder.Str;
end;

class operator TSimbaStringMap.Initialize(var Self: TSimbaStringMap);
begin
  Self := Default(TSimbaStringMap);
end;

end.
