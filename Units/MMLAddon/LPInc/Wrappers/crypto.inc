{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

    crypto.inc for the Mufasa Macro Library
}

procedure lphaval(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_haval;
  Digest: array[0..31] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_haval.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 31 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpmd4(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_md4;
  Digest: array[0..15] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_md4.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 15 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end; 

procedure lpmd5(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_md5;
  Digest: array[0..15] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_md5.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 15 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpripemd128(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_ripemd128;
  Digest: array[0..15] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_ripemd128.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 15 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpripemd160(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_ripemd160;
  Digest: array[0..19] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_ripemd160.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 19 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpsha1(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_sha1;
  Digest: array[0..19] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha1.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 19 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpsha256(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_sha256;
  Digest: array[0..31] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 31 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpsha384(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_sha384;
  Digest: array[0..47] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha384.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 47 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lpsha512(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_sha512;
  Digest: array[0..63] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha512.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 63 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

procedure lptiger(Params: PParamArray; Result: Pointer);
var
  HASH: TDCP_tiger;
  Digest: array[0..23] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_tiger.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(PlpString(Params^[0])^);
    Hash.Final(Digest);
    PlpString(Result)^ := '';
    for I := 0 to 23 do
      PlpString(Result)^ := PlpString(Result)^ + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;
