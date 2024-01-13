{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.algo_difference;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

function Algo_Box_Difference(x,y: TBoxArray): TBoxArray;
function Algo_Point_Difference(x,y: TPointArray): TPointArray;
function Algo_UInt8_Difference(x,y: TByteArray): TByteArray;
function Algo_Int32_Difference(x,y: TIntegerArray): TIntegerArray;
function Algo_Int64_Difference(x,y: TInt64Array): TInt64Array;


implementation

uses
  simba.dictionary;

{$DEFINE MACRO_SET_DIFF :=
  for i:=0 to High(x) do dict[x[i]] := True;
  for i:=0 to High(y) do dict[y[i]] := False;

  c := 0;
  SetLength(Result, length(x));
  for i:=0 to High(x) do
    if dict.Get(x[i], tmp) and (tmp <> False) then
    begin
      dict[x[i]] := False;
      Result[c] := x[i];
      Inc(c);
    end;
  SetLength(Result, c);
}

function Algo_Box_Difference(x,y: TBoxArray): TBoxArray;
type
  TDict = specialize TDictionary<TBox, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := nil;
  dict := TDict.Create(@HashBox);
  MACRO_SET_DIFF;
  dict.Free();
end;

function Algo_Point_Difference(x,y: TPointArray): TPointArray;
type
  TDict = specialize TDictionary<TPoint, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := nil;
  dict := TDict.Create(@HashPoint);
  MACRO_SET_DIFF;
  dict.Free();
end;

function Algo_UInt8_Difference(x,y: TByteArray): TByteArray;
type
  TDict = specialize TDictionary<UInt8, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := nil;
  dict := TDict.Create(@HashUInt8);
  MACRO_SET_DIFF;
  dict.Free();
end;

function Algo_Int32_Difference(x,y: TIntegerArray): TIntegerArray;
type
  TDict = specialize TDictionary<Int32, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := nil;
  dict := TDict.Create(@HashInt32);
  MACRO_SET_DIFF;
  dict.Free();
end;

function Algo_Int64_Difference(x,y: TInt64Array): TInt64Array;
type
  TDict = specialize TDictionary<Int64, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := nil;
  dict := TDict.Create(@HashInt64);
  MACRO_SET_DIFF;
  dict.Free();
end;


end.

