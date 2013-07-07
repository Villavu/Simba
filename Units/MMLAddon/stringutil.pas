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

    String Util for Mufasa Macro Library
}

unit stringutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  StrUtils, mufasatypes;

type
  StrExtr =(Numbers, Letters, Others);
  PStrExtr = ^StrExtr;

function ExtractFromStr( Str : string; Extract : StrExtr) : string;
function Capitalize(str : string) : string;
function Implode(Glue : string; Pieces: TStringArray): string;
function Explode(del, str: string): TStringArray;
function CompressString(const Str : string) : string;
function DecompressString(const Compressed : string) : string;
function Base64Encode(const str : string) : string;
function Base64Decode(const str : string) : string;
function LevDistance(src, target: string): Integer;
function NormLevDistance(src, target: string): Extended;
function StringMatch(checkCompare, goalCompare: string): extended;
function MultiBetween(str, s1, s2: string): TStringArray;

implementation
uses
  paszlib,  DCPbase64, math;

function Implode(Glue: string;Pieces: TStringArray): string;
var
  I, Len : integer;
begin
  Result := '';
  Len := high(Pieces);
  if (Len < 0) then
    exit;
  Result := Pieces[0];
  for i := 1 to len do
    result := result + Glue + Pieces[i];
end;

function Explode(del, str: string): TStringArray;
var
  i,ii : integer;
  lastpos : integer;
  lenstr : integer;
  lendel : integer;
  lenres : integer;
  matches : boolean;
begin;
  lastpos := 1;
  lenres := 0;
  setlength(result,lenres);
  lendel := length(del);
  lenstr := length(str);
  //  for i := 1 to lenstr do
  i := 1;
  while i <= lenstr do
  begin;
    if not ((i + lendel - 1) > lenstr) then
    begin
      matches := true;
      for ii := 1 to lendel do
        if str[i + ii - 1] <> del[ii] then
        begin
          matches := false;
          break;
        end;
      if matches then
      begin;
        inc(lenres);
        setlength(result,lenres);
        result[lenres-1] := Copy(str,lastpos,i-lastpos);
        lastpos := i+lendel;
        i := i + lendel-1;//Dirty
        if i = lenstr then //This was the trailing delimiter
          exit;
      end;
    end else //We cannot possibly find a delimiter anymore, thus copy the rest of the string and exit
      Break;
    inc(i);
  end;
  //Copy the rest of the string (if it's not a delimiter)
  inc(lenres);
  setlength(result,lenres);
  result[lenres-1] := Copy(str,lastpos,lenstr - lastpos + 1);
end;

function CompressString(const Str: string): string;
var
  Destlen:longword;
begin
  result := '';
  Destlen :=BufferLen;
  if length(str) <  1 then
    exit;
  if compress(BufferString,destlen,PChar(Str),length(str)) = Z_OK then
  begin
    setlength(result,Destlen + SizeOf(Integer));
    PInteger(@result[1])^ := Length(str);
    Move(bufferstring[0],result[5],Destlen);
  end;
end;

function DecompressString(const Compressed: string): string;
var
  destlen : Longword;
  len,dest : integer;
  Compress : PChar;
begin
  result := '';
  len := Length(Compressed);
  Compress := PChar(Compressed);
  if len < 5 then
    exit;
  dest := PInteger(@compress[0])^;
  Inc(Compress,sizeof(integer));
  if dest < 1 then
    exit;
  destlen := dest;
  setlength(result,destlen);
  if uncompress(PChar(result),destlen,Compress,len) <> z_OK then
    result := '';
end;

function Base64Encode(const str: string): string;
begin
  result := Base64EncodeStr(str);
end;

function Base64Decode(const str: string): string;
begin
  result := Base64DecodeStr(str);
end;

function Capitalize(str : string) : string;
var
  i , l : integer;
  cap : boolean;
  Range : set of char;
begin;
  result := str;
  l := length(str);
  cap := true;
  Range :=  ['a'..'z','A'..'Z'];
  for i := 1 to l do
    if cap and (str[i] in Range) then
    begin;
      result[i] := UpperCase(str[i])[1];
      cap := false;
    end else if not (str[i] in Range) then
      cap := true;
end;

function ExtractFromStr( Str : string; Extract : StrExtr) : string;
var
  Range : set of char;
  i : integer;
begin;
  case Extract of
    Numbers : Range := ['0'..'9'];
    Letters : Range := ['A'..'Z','a'..'z'];
    Others  : Range := [#0..#255] - ['0'..'9','A'..'Z','a'..'z'];
  end;
  Result := '';
  for i := length(str) downto 1 do
    if str[i] in Range then
      result := str[i] + result;
end;

(*
LevDistance
===========

Number of deletions, insertions, or substitutions to transform src into target.

Uses Levenshtein, original code taken from:
www.merriampark.com/lddelphi.htm
*)
function LevDistance(src, target: string): Integer;

  function min3(a, b, c: Integer): Integer;
  begin
    Result := a;
    if (Result > b) then
      Result := b;
    if (Result > c) then
      Result := c;
  end;

var
  d: array of array of Integer;
  n, m, i, j, cost: Integer;
  s_i, t_j: char;
begin
  n := Length(src);
  m := Length(target);
  if (n = 0) then
  begin
    Result := m;
    Exit;
  end;
  if (m = 0) then
  begin
    Result := n;
    Exit;
  end;

  setLength(d, n+1, m+1);

  for i := 0 to n do
    d[i, 0] := i;
  for j := 0 to m do
    d[0, j] := j;

  for i := 1 to n do
  begin
    s_i := src[i];
    for j := 1 to m do
    begin
      t_j := target[j];
      if (s_i = t_j) then
        cost := 0
      else
        cost := 1;

      d[i, j] := min3(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i -1][j - 1] + cost);
    end;
  end;

  Result := d[n, m];
end;

function NormLevDistance(src, target: String): Extended;
begin
  Result := Max(Length(src), Length(target)) * 1.0;
  Result := LevDistance(src, target) / Result;
end;

{/\
  Uses Levenshtein to work out the match % between the two strings.
/\}
function StringMatch(checkCompare, goalCompare: string): extended;
var
  mismatch, len: extended;
begin
  result := 0.00;

  if ((length(checkCompare)=0) or (length(goalCompare)=0)) then
    exit;

  mismatch := LevDistance(checkCompare, goalCompare);

  len := max(length(checkCompare), length(goalCompare));
  result := (len-mismatch) / len;
end;

{/\
  Splits a string into an array of strings by giving it an begin and an end
  tag. Useful for data reading.
/\}
function MultiBetween(str, s1, s2: string): TStringArray;

  function Between(s1, s2, str: string): string;
  var
    I,J : integer;
  begin;
    Result := '';
    I := pos(s1,str);
    if I > 0 then
    begin;
      i := i + length(s1);
      j := posex(s2,str,i);
      if j > 0 then
        Result := copy(str,i,j-i);
    end;
  end;

var
  strL, s1L, s2L, o, n, e, r: Integer;
begin
  s1L := Length(s1);
  s2L := Length(s2);
  strL := Length(str);

  r := 0;
  o := 0;
  n := 0;
  e := 0;

  if ((s1 = '') or (s2 = '') or (strL <= (s1L + s2L))) then
    exit;

  SetLength(Result, (strL - (s1L + s2L)));

  repeat
    n := PosEx(s1, str, (n + 1));
    if (n < 1) then
      Break;
    e := PosEx(s2, str, (n + 1));
    if (e < 1) then
      Break;
    repeat
      o := n;
      n := PosEx(s1, str, (n + 1));
    until ((n >= e) or (n < 1));
    n := o;
    Result[r] := Between(s1, s2, Copy(str, n, (s1L + (e + s2L))));
    if (Result[r] <> '') then
      Inc(r);
  until ((e < 1) or (n < 1));

  SetLength(Result, r);
end;

end.

