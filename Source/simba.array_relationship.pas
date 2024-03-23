unit simba.array_relationship;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.hash;

type
  generic TArrayRelationship<_T> = class
  public type
    TArr = array of _T;
    THashFunc = function(constref k: _T): UInt32;
  public
    class function Difference(x,y: TArr; HashFunc: THashFunc): TArr; static;
    class function SymmetricDifference(x,y: TArr; HashFunc: THashFunc): TArr; static;
    class function Intersection(x,y: TArr; HashFunc: THashFunc): TArr; static;
  end;

implementation

uses
  simba.container_dict;

class function TArrayRelationship.Difference(x, y: TArr; HashFunc: THashFunc): TArr;
type
  TDict = specialize TDictionary<_T, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := [];
  dict := TDict.Create(HashFunc);

  for i:=0 to High(x) do dict[x[i]] := True;
  for i:=0 to High(y) do dict[y[i]] := False;

  c := 0;
  SetLength(Result, Length(x));
  for i:=0 to High(x) do
    if dict.Get(x[i], tmp) and (tmp <> False) then
    begin
      dict[x[i]] := False;
      Result[c] := x[i];
      Inc(c);
    end;
  SetLength(Result, c);

  dict.Free();
end;

class function TArrayRelationship.SymmetricDifference(x, y: TArr; HashFunc: THashFunc): TArr;
begin

end;

class function TArrayRelationship.Intersection(x, y: TArr; HashFunc: THashFunc): TArr;
begin

end;

end.

