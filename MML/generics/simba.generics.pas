{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Modified for Simba.
}
unit simba.generics;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, simba.mufasatypes;

type
  ESimbaGenericsException = class(Exception);

  generic TSimbaStack<_T> = class
  public type
    TTArray = array of _T;
  protected
    FArr: TTArray;
    FLen: Integer;
    FCur: Integer;

    procedure Grow(AGrowSize: Integer); virtual;
    procedure CheckIndex(Index: Integer; GrowIfNeeded: Boolean = False); virtual;
    function getItem(Index: Integer): _T; virtual;
    function getCurItem: _T; virtual;
    procedure setCurItem(Item: _T); virtual;
    function getCount: Integer; virtual;
  public
    GrowSize: Word;

    constructor Create(StartBufLen: Cardinal = 32); reintroduce; virtual;
    procedure Reset; virtual;
    function Pop: _T; virtual;
    function Push(Item: _T): Integer; virtual;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem; default;
    property Top: _T read getCurItem write setCurItem;
    property Size: Integer read FLen;
    property Count: Integer read getCount;
    property Cur: Integer read FCur;
  end;

  generic TSimbaList<_T> = class
  public type
    TTArray = array of _T;
  protected
    FDuplicates: TDuplicates;
    FSorted: Boolean;
    FItems: TTArray;
    FCount: Integer;

    function getSorted: Boolean; virtual;
    procedure setSorted(Sort: Boolean; DoUpdate: Boolean); overload; virtual;
    procedure setSorted(Sort: Boolean); overload; virtual;
    function getItem(Index: Integer): _T; virtual;
    procedure setItem(Index: Integer; Item: _T); virtual;
  public
    InvalidVal: _T;

    constructor Create(InvalidValue: _T; ADuplicates: TDuplicates; ASort: Boolean); reintroduce; virtual;
    constructor Create; reintroduce; virtual;
    procedure Clear; virtual;

    procedure Extend(Items: TTArray); virtual;
    function Add(Item: _T): Integer; virtual;
    function Insert(Item: _T; Index: Integer): Integer; virtual;

    function Delete(Index: Integer): _T; virtual;
    function DeleteItem(Item: _T): _T; virtual;

    procedure MoveItem(AFrom, ATo: Integer); virtual;
    procedure SwapItem(AFrom, ATo: Integer); virtual;

    function IndicesOf(Item: _T): TIntegerArray; virtual;
    function IndexOf(Item: _T; Lo, Hi: Integer): Integer; overload; virtual;
    function IndexOf(Item: _T): Integer; overload; virtual;
    function ExistsItem(Item: _T): Boolean;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem write setItem; default;
    property Count: Integer read FCount;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read getSorted write setSorted;
  end;

  generic TSimbaObjectList<_T> = class(specialize TSimbaList<_T>)
  protected
    FManageObjects: Boolean;
  public
    procedure Clear; override;

    function Delete(Index: Integer): _T; override;

    constructor Create(ManageObjects: Boolean); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaList_String = specialize TSimbaList<String>;
  TSimbaList_UInt32 = specialize TSimbaList<UInt32>;

  TSimbaStringList = class(TSimbaList_String)
  protected
    FHashList: TSimbaList_UInt32;
    FCaseSensitive: Boolean;

    function getSorted: Boolean; override;
    procedure setSorted(Sort: Boolean; DoUpdate: Boolean = True); override;
    procedure setItem(Index: Integer; Item: String); override;
  public
    constructor Create(InvalidValue: String; ADuplicates: TDuplicates; ACaseSensitive, ASort: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    function CaseSens(const Item: String): String; inline;
    function Add(Item: String): Integer; override;
    function Delete(Index: Integer): String; override;

    procedure MoveItem(AFrom, ATo: Integer); override;
    procedure SwapItem(AFrom, ATo: Integer); override;

    function IndexOf(Item: String; Lo, Hi: Integer): Integer; override;
    procedure ImportFromArray(Arr: TSimbaList_String.TTArray); override;

    function Implode(ASep: String): String; virtual;

    property CaseSensitive: Boolean read FCaseSensitive;
  end;

  generic TSimbaStringMap<_T> = class
  public type
    TTItems = specialize TSimbaList<_T>;
    TTArray = array of _T;
    TTArrays = record
      Keys: TSimbaStringList.TTArray;
      Items: TTArray;
    end;
  protected
    FStringList: TSimbaStringList;
    FItems: TTItems;
    FCount: Integer;

    function getItem(Key: String): _T; virtual;
    procedure setItem(Key: String; Item: _T); virtual;
    function getItemI(Index: Integer): _T; virtual;
    procedure setItemI(Index: Integer; Item: _T); virtual;
    function getKey(Index: Integer): String; virtual;

    function getSorted: Boolean; virtual;
    procedure setSorted(Sort: Boolean); virtual;
  public
    constructor Create(InvalidValue: _T; Duplicates: TDuplicates; Sort: Boolean; InvalidKey: String = ''; ACaseSensitive: Boolean = False); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    function Insert(Key: String; Item: _T; Index: Integer): Integer; virtual;
    function Add(Key: String; Item: _T): Integer; virtual;

    function Delete(Key: String): _T; overload; virtual;
    function Delete(Index: Integer): _T; overload; virtual;
    function DeleteItem(Item: _T): _T; virtual;

    function IndicesOfItemI(Item: _T): TIntegerArray; virtual;
    function IndicesOfItem(Item: _T): TStringArray; virtual;
    function IndicesOfKey(Key: String): TIntegerArray; virtual;

    function ItemsOfKey(Key: String): TTArray; virtual;

    function IndexOfItemI(Item: _T): Integer; virtual;
    function IndexOfItem(Item: _T): String; virtual;
    function IndexOfKey(Key: String): Integer; virtual;
    function ExistsItem(Item: _T): Boolean;
    function ExistsKey(Key: String): Boolean;

    procedure setKeyI(Index: Integer; NewKey: String); virtual;
    procedure setKey(Item: _T; NewKey: String); virtual;

    procedure ImportFromArrays(Arr: TTArrays); virtual;
    function ExportToArrays: TTArrays; virtual;

    property Items[Index: String]: _T read getItem write setItem; default;
    property ItemsI[Index: Integer]: _T read getItemI write setItemI;
    property Key[Index: Integer]: String read getKey write setKeyI;
    property Count: Integer read FCount;
    property Sorted: Boolean read getSorted write setSorted;
  end;

  function HashString(const Value: String): UInt32; inline;

  function _Compare8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer; inline;
  function _Compare16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer; inline;
  function _Compare32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer; inline;
  function _Compare64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer; inline;
  function _BSearch8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer; inline;
  function _BSearch16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer; inline;
  function _BSearch32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer; inline;
  function _BSearch64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer; inline;
  procedure _Insert8(Arr: PUInt8; var Index: Integer); inline;
  procedure _Insert16(Arr: PUInt16; var Index: Integer); inline;
  procedure _Insert32(Arr: PUInt32; var Index: Integer); inline;
  procedure _Insert64(Arr: PUInt64; var Index: Integer); inline;

implementation

//MurMurHas2 by Tommi Prami & optimizations by Patrick van Logchem
function HashString(const Value: String): UInt32; inline;
const
  Seed: UInt32 = $c58f1a7b;
  cM:   UInt32 = $5bd1e995;
  cR:   UInt32 = 24;
var
  Data: Pointer;
  i, k, Len: UInt32;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Data := @Value[1];
  Len := Length(Value) * SizeOf(Char);

  Result := Seed xor Len;
  for i := 1 to Len div SizeOf(UInt32) do
  begin
    k := PUInt32(Data)^ * cM;
    Result := (Result * cM) xor ((k xor (k shr cR)) * cM);
    Inc(PtrUInt(Data), SizeOf(UInt32));
  end;

  Len := Len and 3;
  if (Len > 0) then
  begin
    if (Len >= SizeOf(UInt16)) then
    begin
      k := PUInt16(Data)^;
      if (Len > SizeOf(UInt16)) then
        k := k + (UInt32(PUInt8(PtrUInt(Data) + SizeOf(UInt16))^) shl 16);
    end
    else
      k := PUInt8(Data)^;
    Result := (Result xor k) * cM;
  end;

  Result := (Result xor (Result shr 13)) * cM;
  Result := (Result xor (Result shr 15));
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

function _Compare8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer; inline;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer; inline;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer; inline;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer; inline;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _BSearch8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer; inline;
var
  mVal: UInt8;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

function _BSearch16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer; inline;
var
  mVal: UInt16;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

function _BSearch32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer; inline;
var
  mVal: UInt32;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

function _BSearch64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer; inline;
var
  mVal: UInt64;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

procedure _Insert8(Arr: PUInt8; var Index: Integer); inline;
var
  Item: UInt8;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch8(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt8));
    Arr[Index] := Item;
  end;
end;

procedure _Insert16(Arr: PUInt16; var Index: Integer); inline;
var
  Item: UInt16;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch16(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt16));
    Arr[Index] := Item;
  end;
end;

procedure _Insert32(Arr: PUInt32; var Index: Integer); inline;
var
  Item: UInt32;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch32(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt32));
    Arr[Index] := Item;
  end;
end;

procedure _Insert64(Arr: PUInt64; var Index: Integer); inline;
var
  Item: UInt64;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch64(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt64));
    Arr[Index] := Item;
  end;
end;

{$INCLUDE simbastack.inc}
{$INCLUDE simbalist.inc}
{$INCLUDE simbaobjectlist.inc}
{$INCLUDE simbastringlist.inc}
{$INCLUDE simbastringmap.inc}

end.

