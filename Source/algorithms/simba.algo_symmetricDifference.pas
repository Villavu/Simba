{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.algo_symmetricDifference;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

function Algo_Box_SymmetricDifference(x,y: TBoxArray): TBoxArray;
function Algo_Point_SymmetricDifference(x,y: TPointArray): TPointArray;
function Algo_UInt8_SymmetricDifference(x,y: TByteArray): TByteArray;
function Algo_Int32_SymmetricDifference(x,y: TIntegerArray): TIntegerArray;
function Algo_Int64_SymmetricDifference(x,y: TInt64Array): TInt64Array;


implementation

uses
  math, simba.generics_dict;

{$DEFINE MACRO_SET_SYMDIFF :=
  for i:=0 to High(x) do dict[x[i]] := 1;
  for i:=0 to High(y) do
  begin
    if dupes.GetDef(y[i], 0) = 1 then continue;
    dict[y[i]] := dict.GetDef(y[i], 0) + 1;
    dupes[y[i]] := 1;
  end;

  c := 0;
  SetLength(Result, length(x)+Length(y));
  for i:=0 to High(x) do
    if dict.GetDef(x[i], 0) = 1 then
    begin
      dict[x[i]] := $FFFFFF;
      Result[c] := x[i];
      Inc(c);
    end;

  for i:=0 to High(y) do
    if dict.GetDef(y[i], 0) = 1 then
    begin
      dict[y[i]] := $FFFFFF;
      Result[c] := y[i];
      Inc(c);
    end;

  SetLength(Result, c);
}

function Algo_Box_SymmetricDifference(x,y: TBoxArray): TBoxArray;
type
  TDict = specialize TDictionary<TBox, Int32>;
var
  dict, dupes: TDict;
  i,c, tmp: Int32;
begin
  Result := nil;
  dict := TDict.Create(@HashBox);
  dupes := TDict.Create(@HashBox);
  MACRO_SET_SYMDIFF;
  dict.Free();
  dupes.Free();
end;

function Algo_Point_SymmetricDifference(x,y: TPointArray): TPointArray;
type
  TDict = specialize TDictionary<TPoint, Int32>;
var
  dict, dupes: TDict;
  i,c, tmp: Int32;
begin
  Result := nil;
  dict := TDict.Create(@HashPoint);
  dupes := TDict.Create(@HashPoint);
  MACRO_SET_SYMDIFF;
  dict.Free(); 
  dupes.Free();
end;

function Algo_UInt8_SymmetricDifference(x,y: TByteArray): TByteArray;
type
  TDict = specialize TDictionary<UInt8, Int32>;
var
  dict, dupes: TDict;
  i,c, tmp: Int32;
begin
  Result := nil;
  dict := TDict.Create(@HashUInt8);
  dupes := TDict.Create(@HashUInt8);
  MACRO_SET_SYMDIFF;
  dict.Free();
  dupes.Free();
end;

function Algo_Int32_SymmetricDifference(x,y: TIntegerArray): TIntegerArray;
type
  TDict = specialize TDictionary<Int32, Int32>;
var
  dict, dupes: TDict;
  i,c, tmp: Int32;
begin
  Result := nil;
  dict := TDict.Create(@HashInt32);
  dupes := TDict.Create(@HashInt32);
  MACRO_SET_SYMDIFF;
  dict.Free();
  dupes.Free();
end;

function Algo_Int64_SymmetricDifference(x,y: TInt64Array): TInt64Array;
type
  TDict = specialize TDictionary<Int64, Int32>;
var
  dict, dupes: TDict;
  i,c, tmp: Int32;
begin
  Result := nil;
  dict  := TDict.Create(@HashInt64);
  dupes := TDict.Create(@HashInt64);
  MACRO_SET_SYMDIFF;
  dict.Free();
  dupes.Free();
end;


end.

