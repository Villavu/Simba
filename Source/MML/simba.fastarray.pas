{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

  Generic which overallocates an array type. Initial growth is 768 then `n = n * 1.5`
  Automatically initalized and managed.

  var FastArray: specialize TSimbaFastArray<Integer>;
  var Arr: TIntegerArray;
  begin
    FastArray.Add(123);
    FastArray.Add(456);

    Arr := FastArray; // Arr is now [123, 456]
  end;
}
unit simba.fastarray;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaFastArray<_T> = record
  private
  type
    TArr = array of _T;
    TSelf = specialize TSimbaFastArray<_T>;
  private
    FLength: Int32;
    FArr: TArr;
    FCount: Int32;
  public
    class operator Initialize(var Self: TSelf); inline;
    class operator :=(var Self: TSelf): TArr; inline;

    procedure Add(constref Value: _T); inline;
  end;

implementation

class operator TSimbaFastArray.Initialize(var Self: TSelf);
begin
  with Self do
  begin
    FCount := 0;
    FLength := 768;

    SetLength(FArr, FLength);
  end;
end;

class operator TSimbaFastArray.:=(var Self: TSelf): TArr;
begin
  with Self do
  begin
    FLength := FCount;
    SetLength(FArr, FLength);

    Result := FArr;
  end;
end;

procedure TSimbaFastArray.Add(constref Value: _T);
begin
  FArr[FCount] := Value;
  FCount += 1;

  if (FCount = FLength) then
  begin
    FLength := FLength + (FLength div 2);
    SetLength(FArr, FLength);
  end;
end;

end.

