{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.container_imagemap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

type
  TSimbaImageMap = record
  private type
    TItem = record
      KeyHash: UInt32;
      Key: String;
      Value: TSimbaImage;
    end;
    TItemArray = array of TItem;
  private
    FItems: TItemArray;
    FCount: Integer;

    class function _BSearch32(var Arr: TItemArray; const Hash: UInt32; Lo, Hi: Integer): Integer; static;
    class procedure _Insert32(var Arr: TItemArray; var Index: Integer); static;

    function IndexOf(AKey: String): Integer;
    function IndicesOf(AKey: String): TIntegerArray;

    function GetImage(Key: String): TSimbaImage;
    function GetImages(Key: String): TSimbaImageArray;
    procedure SetImage(Key: String; AValue: TSimbaImage);
  public
    property Count: Integer read FCount;
    property Image[Key: String]: TSimbaImage read GetImage write SetImage;
    property Images[Key: String]: TSimbaImageArray read GetImages;

    procedure Clear;
    procedure Delete(Key: String);
    procedure Add(Key: String; Img: TSimbaImage);

    function Exists(Key: String): Boolean;
    function ToString: String;
  end;

implementation

uses
  simba.containers, simba.vartype_string;

class function TSimbaImageMap._BSearch32(var Arr: TItemArray; const Hash: UInt32; Lo, Hi: Integer): Integer;
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

class procedure TSimbaImageMap._Insert32(var Arr: TItemArray; var Index: Integer);
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

function TSimbaImageMap.IndexOf(AKey: String): Integer;
var
  Hash: UInt32;
begin
  Hash := AKey.ToUpper().Hash();

  Result := _BSearch32(FItems, Hash, 0, FCount - 1);
  if (Result < 0) or (not SameText(FItems[Result].Key, AKey)) then
    Result := -1;
end;

function TSimbaImageMap.IndicesOf(AKey: String): TIntegerArray;
var
  Hash: UInt32;
  Index: Integer;
  Buffer: TSimbaIntegerBuffer;
begin
  Hash := AKey.ToUpper().Hash();

  Index := _BSearch32(FItems, Hash, 0, FCount - 1);
  if (Index >= 0) then
    while (Index < FCount) and (FItems[Index].KeyHash = Hash) do
    begin
      if SameText(FItems[Index].Key, AKey) then
        Buffer.Add(Index);
      Inc(Index);
    end;

  Result := Buffer.ToArray(False);
end;

function TSimbaImageMap.GetImage(Key: String): TSimbaImage;
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);
  if (Index = -1) then
    SimbaException('Image "%s" does not exist', [Key]);

  Result := FItems[Index].Value;
end;

function TSimbaImageMap.GetImages(Key: String): TSimbaImageArray;
var
  Indices: TIntegerArray;
  I: Integer;
begin
  Indices := Self.IndicesOf(Key);

  SetLength(Result, Length(Indices));
  for I := 0 to High(Result) do
    Result[I] := FItems[Indices[I]].Value;
end;

procedure TSimbaImageMap.SetImage(Key: String; AValue: TSimbaImage);
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);
  if (Index = -1) then
    SimbaException('Image "%s" does not exist', [Key]);

  FItems[Index].Value := AValue;
end;

function TSimbaImageMap.Exists(Key: String): Boolean;
begin
  Result := IndexOf(Key) > -1;
end;

function TSimbaImageMap.ToString: String;
var
  Builder: TSimbaStringBuilder;
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if (I > 0) then
      Builder.Append(LineEnding);
    Builder.Append('  [Key=' + FItems[I].Key + ', Image=' + HexStr(FItems[I].Value) + ']');
  end;

  Result := Format('Count=%d', [FCount]);
  if (FCount > 0) then
    Result := Result + LineEnding + 'Items:' + LineEnding + Builder.Str;
end;

procedure TSimbaImageMap.Delete(Key: String);
var
  Index, I: Integer;
begin
  Index := IndexOf(Key);

  if (Index > -1) then
  begin
    for I := Index + 1 to FCount - 1 do
      FItems[I-1] := FItems[I];

    Dec(FCount);
  end;
end;

procedure TSimbaImageMap.Add(Key: String; Img: TSimbaImage);
var
  Index: Integer;
begin
  if (FCount >= Length(FItems)) then
    SetLength(FItems, 4 + (FCount * 2));

  Index := FCount;

  Inc(FCount);
  FItems[Index].KeyHash := Key.ToUpper().Hash();
  FItems[Index].Key := Key;
  FItems[Index].Value := Img;

  _Insert32(FItems, Index);
end;

procedure TSimbaImageMap.Clear;
begin
  FCount := 0;
  FItems := [];
end;

end.

