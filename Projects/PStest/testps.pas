unit TestPS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringArray = array of string;
  w_TPoint = record
    x, y:integer;
  end;

function MakeArr : TStringArray;
procedure TestParameters(Int1,Int2,Int3,Int4,Int5,Int6 : integer);
function TestResult(Int1,Int2,Int3,Int4,Int5,Int6 : integer): Integer;
function TestString(Str1,Str2,Str3 : string) : string;
function TestStringEdit(var Str : string) : String;
procedure TestArrayPassing( Arr : TStringArray);
Procedure TestArrayEdit(var Arr : TStringArray);
function TestArrayFull(var Arr1: TStringArray; Arr2 : TStringArray): TStringArray;
function makePoint(x, y: integer): w_Tpoint;

implementation

uses
  Unit1;

procedure Writeln(s : string);
begin;
  Form1.Memo2.Lines.add(s);
end;

procedure TestParameters(Int1,Int2,Int3,Int4,Int5,Int6 : integer);
begin;
  Writeln('Parameters are: ' + format('%d %d %d %d %d %d',[int1,int2,int3,int4,int5,int6]));
  if (int1 <> 1) or (int2 <> 2) or (int3 <> 3) or (int4 <> 4) or (int5 <> 5) or (int6 <> 6) then
    Writeln('WRONG.')
  else
    Writeln('Passed this test');
end;

function TestResult(Int1,Int2,Int3,Int4,Int5,Int6 : integer): Integer;
begin;
  Result := int1 + int2 + int3 + int4 + int5 + int6;
  Writeln('Result should be:' + inttostr(int1+int2+int3+int4+int5+int6));
end;

function TestString(Str1,Str2,Str3 : string) : string;
begin;
  if Str1+Str2+str3 = 'lol' then
    Writeln('Strings got passed over correctly.');
  Str1 := 'a';
  Str2 := 'b';
  Str3 := 'c';
  Result := str1;
  Writeln('Result should be a');
end;

function TestStringEdit(var Str : string) : String;
begin;
  Result := 'False';
  if Str <> 'Test' then
    Writeln('String didn''t get passed on right');
  Str := 'tseT';
  Result := Str;
  Result := 'hopla';
  Writeln('Result should be: hopla; Input str should be tseT');
end;

procedure TestArrayPassing(Arr : TStringArray);
var
  i : integer;
begin;
  Writeln('Length of arr is: ' + inttostr(length(arr)));
  for i := 0 to high(arr) do
    Writeln(format('Arr[%d] := %s',[i,arr[i]]));
end;


Procedure TestArrayEdit(var Arr : TStringArray);
begin;
  Writeln('Arr size is ' + inttostr(length(arr)));
  SetLength(arr,0);
  SetLength(arr,5);
  arr[0] := 'a';
  arr[1] := 'b';
  arr[2] := 'c';
  arr[3] := 'd';
  arr[4] := '!';
end;

function ConvStrArr( Arr : Array of String): TStringArray;
var
  Len : Integer;
begin;
  Len := Length(Arr);
  SetLength(Result, Len);
  Move(Arr[Low(Arr)], Result[0], Len*SizeOf(String));
end;

function TestArrayFull(var Arr1: TStringArray; Arr2 : TStringArray): TStringArray;
begin
  Writeln(Format('Len(Arr1) = %d - Len(Arr2) = %d',[Length(Arr1),length(arr2)]));
  SetLength(Result,0);
  Setlength(Arr1,0);
  SetLength(Arr2,0);
  SetLength(result,3);
  SetLength(Arr1,4);
  SetLength(arr2,5);
  Arr1 := ConvStrArr(['Hoi','Hoe','Gaat','Het?']);
  Arr2 := ConvStrArr(['Ik','ben','een','geest!']);
  Result := ConvStrArr(['Waza?','Gaat','Alles','Goed']);
end;

function MakeArr : TStringArray;
begin;
  setlength(result,2);
  result[0] := 'Test0';
  Result[1] := 'Wow!';
end;

function makePoint(x, y: integer): w_Tpoint;
begin
  result.x := x;
  result.y := y;
end;

end.

