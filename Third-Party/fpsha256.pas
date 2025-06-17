{
  This file is part of the Free Component Library.
  Copyright (c) 2021 by the Free Pascal team.

  SHA256 and HMACSha256 routines.

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// Normally, if an optimized version is available for OS/CPU, that will be used
// Define to force to use implementation in pascal
{$ifdef OLD_ASSEMBLER}
  {$DEFINE SHA256PASCAL}
{$endif OLD_ASSEMBLER}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpsha256;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$MODESWITCH advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TContextBuffer = array[0..7] of UInt32;
  PContextBuffer = ^TContextBuffer;

  THashBuffer = array[0..63] of Byte;

  TSHA256Base = record
    Context: TContextBuffer;
    HashBuffer : THashBuffer;
    Index: UInt32;
    TotalLength: Int64;
    procedure Compress; inline;
    procedure Final;
    procedure Init(Use224 : Boolean = False);
    procedure Update(PBuf: PByte; Size: UInt32); overload;
    procedure Update(const Value: TBytes); overload;
{$IFDEF DEBUGSHA}
  private
    procedure DumpBuffer;
    procedure DumpHash;
{$ENDIF DEBUGSHA}
  end;

  TSHA256Digest = packed array[0..31] of Byte;
  PSHA256Digest = ^TSHA256Digest;
  PSHA256 = ^TSHA256;
  TSHA256 = record
    Base : TSHA256Base;
    Digest: TSHA256Digest;
    procedure Final;
    procedure Init;
    function IsEqual(const ADigest: TSHA256Digest): Boolean;
    procedure OutputHexa(out Result: AnsiString);
    procedure Update(PBuf: PByte; Size: UInt32); overload;
    procedure Update(const Value: TBytes); overload;

    // Calculate SHA256, return digest as bytes.
    class procedure DigestBytes(const Value: TBytes; out Result: TBytes) ; static;
    // Calculate  SHA256, return digest as base64(url) string
    class procedure DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString); static;
    // Calculate  SHA256, return digest as HEX encoded string
    class procedure DigestHexa(const Value: TBytes; out Result: AnsiString); static;
    // HMAC using SHA256 as hash
    Class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA256Digest): Boolean; overload; static;
    class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA256Digest): Boolean; overload; static;
    // Calculate HMacSHA256, return digest as hex string.
    class function HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload; static;
    // Calculate SHA256 from a stream, return digest.
    class procedure Stream(aStream: TStream; out aDigest: TSHA256Digest); static; overload;
    class function Stream(aStream: TStream): TSHA256Digest; static; overload;
    // Digest Stream, result as HexaDecimal string.
    class procedure StreamHexa(aStream: TStream; out Result: AnsiString); static; overload;
    class Function StreamHexa(aStream: TStream): AnsiString; static overload;
    // Digest Stream, result as Base64-encoded string
    class procedure StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString); static; overload;
    class Function StreamBase64(aStream: TStream; isURL : Boolean): AnsiString; static; overload;
    // HKDF : Derive key of desired length from a salt,input key and info  (RF5869, using HMACSHA256) .
    class function HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean; static;
  end;

  TSHA224Digest = packed array[0..27] of Byte;
  PSHA224Digest = ^TSHA224Digest;
  PSHA224 = ^TSHA224;
  TSHA224 = record
    Base : TSHA256Base;
    Digest: TSHA224Digest;
    procedure Final;
    procedure Init;
    function IsEqual(const ADigest: TSHA224Digest): Boolean;
    procedure OutputHexa(out Result: AnsiString);
    procedure Update(PBuf: PByte; Size: UInt32); overload;
    procedure Update(const Value: TBytes); overload;

    // Calculate SHA224, return digest as bytes.
    class procedure DigestBytes(const Value: TBytes; out Result: TBytes) ; static;
    // Calculate  SHA224, return digest as base64(url) string
    class procedure DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString); static;
    // Calculate  SHA224, return digest as HEX encoded string
    class procedure DigestHexa(const Value: TBytes; out Result: AnsiString); static;
    // HMAC using SHA224 as hash
    Class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA224Digest): Boolean; overload; static;
    class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA224Digest): Boolean; overload; static;
    // Calculate HMacSHA224, return digest as hex string.
    class function HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload; static;
    // Calculate SHA224 from a stream, return digest.
    class procedure Stream(aStream: TStream; out aDigest: TSHA224Digest); static; overload;
    class function Stream(aStream: TStream): TSHA224Digest; static; overload;
    // Digest Stream, result as HexaDecimal string.
    class procedure StreamHexa(aStream: TStream; out Result: AnsiString); static; overload;
    class Function StreamHexa(aStream: TStream): AnsiString; static overload;
    // Digest Stream, result as Base64-encoded string
    class procedure StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString); static; overload;
    class Function StreamBase64(aStream: TStream; isURL : Boolean): AnsiString; static; overload;
    // HKDF : Derive key of desired length from a salt,input key and info  (RF5869, using HMACSHA224) .
    class function HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean; static;
  end;


Const
  SHA256_DIGEST_SIZE = SizeOf(TSHA256Digest); // 32
  SHA224_DIGEST_SIZE = SizeOf(TSHA224Digest); // 28

implementation

{$R-}
{$Q-}

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Utils
{$if defined(x86_64) or defined(CPU386)},System.CPU{$endif};
{$ELSE FPC_DOTTEDUNITS}
uses fphashutils
{$if defined(x86_64) or defined(CPU386)},cpu{$endif};
{$ENDIF FPC_DOTTEDUNITS}

//------------------------------------------------------------------------------
// SHA256Base
//------------------------------------------------------------------------------

const
  Seed256Hash : TContextBuffer = (
    $6a09e667,
    $bb67ae85,
    $3c6ef372,
    $a54ff53a,
    $510e527f,
    $9b05688c,
    $1f83d9ab,
    $5be0cd19
  );
  Seed224Hash : TContextBuffer = (
   $c1059ed8,
   $367cd507,
   $3070dd17,
   $f70e5939,
   $ffc00b31,
   $68581511,
   $64f98fa7,
   $befa4fa4
  );

procedure TSHA256Base.Init(Use224 : Boolean = False);

var
  P : PContextBuffer;
  I : Integer;

begin
  Self.Index := 0;
  Self.TotalLength := 0;
  FillChar(Self.HashBuffer, Sizeof(Self.HashBuffer), 0);
  if Use224 then
    P:=@Seed224Hash
  else
    P:=@Seed256Hash;
  For I:=0 to 7 do
    Context[i]:=P^[i];
end;

const
  K: array[0..63] of UInt32 = (
   $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
   $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
   $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
   $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
   $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
   $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
   $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
   $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
   $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
   $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
   $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

procedure sha256PascalCompress(var Context:TContextBuffer;var HashBuffer:THashBuffer; aK:pointer; Mask:pointer);
// Actual hashing function
Type
  TBuf64 =  array[0..63] of UInt32;
var
  A, B, C, D, E, F, G, H: UInt32;
  W: TBuf64;
  I: UInt32;
  t1, t2: UInt32;
begin
  w:=Default(TBuf64);
  // Calculate "expanded message blocks"
  Move(HashBuffer, W, Sizeof(HashBuffer));
  for I := 0 to 15 do
    W[I] := SwapEndian(W[I]);

  A := Context[0]; B := Context[1]; C := Context[2]; D := Context[3]; E := Context[4]; F := Context[5];  G := Context[6];  H := Context[7];
  for I := 0 to 15 do
  begin
    t1 := H+(((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7)))+((E and F) xor (not E and G))+K[I]+W[I];
    t2 := (((A shr 2) or (A shl 30)) xor ((A shr 13) or (A shl 19)) xor ((A shr 22) xor (A shl 10)))+((A and B) xor (A and C) xor (B and C));
    H := G; G := F; F := E; E := D+t1;
    D := C; C := B; B := A; A := t1+t2;
  end;

  for I := 16 to 63 do
  begin
    W[I] := (((W[I-2] shr 17) or(W[I-2] shl 15)) xor ((W[I-2] shr 19) or (W[I-2] shl 13))
      xor (W[I-2] shr 10))+W[I-7]+(((W[I-15] shr 7) or (W[I-15] shl 25))
      xor ((W[I-15] shr 18) or (W[I-15] shl 14)) xor (W[I-15] shr 3))+W[I-16];

    t1 := H+(((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl 7)))+((E and F) xor (not E and G))+K[I]+W[I];
    t2 := (((A shr 2) or (A shl 30)) xor ((A shr 13) or (A shl 19)) xor ((A shr 22) xor (A shl 10)))+((A and B) xor (A and C) xor (B and C));
    H := G; G := F; F := E; E := D+t1;
    D := C; C := B; B := A; A := t1+t2;
  end;

  Inc(Context[0], A);
  Inc(Context[1], B);
  Inc(Context[2], C);
  Inc(Context[3], D);
  Inc(Context[4], E);
  Inc(Context[5], F);
  Inc(Context[6], G);
  Inc(Context[7], H);
end;

procedure TSHA256Base.Compress;
begin
  sha256PascalCompress(Context,HashBuffer,@K,nil);
end;

type
  TInt64Rec = packed record
    case Integer of
      0: (Lo, Hi: UInt32);
      1: (QuadPart: Int64);
  end;

procedure TSHA256Base.Final;
begin
  // 1. append bit '1' after Buffer
  HashBuffer[Self.Index] := $80;
  FillChar(HashBuffer[Self.Index+1], SizeOf(HashBuffer)-Self.Index-1, 0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length)
  if Self.Index >= 56 then
  begin
    Compress;
    FillChar(HashBuffer, SizeOf(HashBuffer), 0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PUInt32(@HashBuffer[56])^ := SwapEndian(TInt64Rec(TotalLength).Hi);
  PUInt32(@HashBuffer[60])^ := SwapEndian(TInt64Rec(TotalLength).Lo);
  Compress;
  Context[0] := SwapEndian(Context[0]);
  Context[1] := SwapEndian(Context[1]);
  Context[2] := SwapEndian(Context[2]);
  Context[3] := SwapEndian(Context[3]);
  Context[4] := SwapEndian(Context[4]);
  Context[5] := SwapEndian(Context[5]);
  Context[6] := SwapEndian(Context[6]);
  Context[7] := SwapEndian(Context[7]);
//  Self.Init; // uncomment if you need security protection against memory inspection
end;

procedure TSHA256Base.Update(PBuf: PByte; Size: UInt32);
var
  Len: UInt32;
begin
  Inc(TotalLength, Int64(UInt32(Size)) * 8);
  while Size > 0 do
  begin
    if (Sizeof(HashBuffer)-Self.Index) <= UInt32(Size) then
    begin
      Len := Sizeof(HashBuffer)-Self.Index;
      Move(PBuf^, HashBuffer[Self.Index], Len);
      Dec(Size, Len);
      Inc(PBuf, Len);
      Compress;
      Self.Index := 0;
    end else
    begin
      Move(PBuf^, HashBuffer[Self.Index], Size);
      Inc(Self.Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TSHA256Base.Update(const Value: TBytes);

begin
  Update(PByte(Value), System.Length(Value));
end;


//------------------------------------------------------------------------------
// SHA256
//------------------------------------------------------------------------------


procedure TSHA256.Init;
begin
  Base.Init(False);
end;



procedure TSHA256.Final;

begin
  Base.Final;
  Move(Base.Context,Digest,Sizeof(Digest));
end;

function TSHA256.IsEqual(const ADigest: TSHA256Digest): Boolean;

var
  Left, Right: TBytes;
begin
  Left:=BytesFromVar(@ADigest, SizeOf(ADigest));
  Right:=BytesFromVar(@Self.Digest, SizeOf(Self.Digest));
  Result:=CompareMem(Pointer(Left), Pointer(Right),Length(Left));
end;


procedure TSHA256.Update(PBuf: PByte; Size: UInt32);

begin
  Base.Update(PBuf,Size);
end;


procedure TSHA256.Update(const Value: TBytes);
begin
  Base.Update(Value);
end;

// @Result[64]
// 'abc' -> 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
procedure TSHA256.OutputHexa(out Result: AnsiString);

begin
  BytesToHexStr(Result,PByte(@Self.Digest),SizeOf(Self.Digest));
end;

// @Result[32]
class procedure TSHA256.DigestBytes(const Value: TBytes; out Result: TBytes);
var
  lSHA256: TSHA256;
begin
  lSHA256.Init;
  lSHA256.Update(Value);
  lSHA256.Final;
  BytesFromVar(Result, @lSHA256.Digest[0], SizeOf(lSHA256.Digest));
end;

class procedure TSHA256.DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString);
var
  S : TBytes;
  lSHA256: TSHA256;
begin
  lSHA256.Init;
  lSHA256.Update(Value);
  lSHA256.Final;
  BytesFromVar(S, @lSHA256.Digest[0], SizeOf(lSHA256.Digest));
  BytesEncodeBase64(S, Result, IsURL, False, False);
end;

// @Result[64]
Class procedure TSHA256.DigestHexa(const Value: TBytes; out Result: AnsiString);
var
  SHA256: TSHA256;
begin
  SHA256.Init;
  SHA256.Update(Value);
  SHA256.Final;
  SHA256.OutputHexa(Result);
end;

class function TSHA256.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA256Digest): Boolean;
begin
  Result := HMAC(Key, KeySize, Data, DataSize, nil, 0, nil, 0, aDigest);
end;

{Generate a SHA256 HMAC (Hashed Message Authentication Code) using the Key and Data
The SHA256 HMAC algorithm is:
 SHA256(Key xor oPad, SHA256(Key xor iPad, Data))
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
 If Key is more than 64 bytes it will be hashed to Key = SHA256(Key) instead
 If Key is less than 64 bytes it will be padded with zeros }
 class function TSHA256.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA256Digest): Boolean;

Type
  TBuf64 = array[0..63] of Byte;

var
  Count: UInt32;
  KeyBuffer, PadBuffer: TBuf64;
  SHA256, SHA256_: TSHA256;
begin
  Result:=False;
  if Key = nil then
    Exit;
  if Data = nil then
    Exit;
  KeyBuffer:=Default(TBuf64);
  SHA256.Init;
  if KeySize > 64 then
  begin
    SHA256.Update(Key, KeySize);
    SHA256.Final;
    System.Move(SHA256.Digest[0], KeyBuffer[0], SizeOf(SHA256.Digest));
  end else
    System.Move(Key^, KeyBuffer[0], KeySize);
  // XOR the key buffer with the iPad value
  for Count := 0 to 63 do
    PadBuffer[Count] := KeyBuffer[Count] xor $36;
  SHA256.Init;
  SHA256.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA256.Update(Data, DataSize);
  if Data2 <> nil then
    SHA256.Update(Data2, DataSize2);
  if Data3 <> nil then
    SHA256.Update(Data3, DataSize3);
  SHA256.Final;
  // XOR the key buffer with the oPad value
  for Count := 0 to 63 do
    PadBuffer[Count] := KeyBuffer[Count] xor $5C;
  // SHA256 the key buffer and the result of the inner SHA256 (Outer)
  SHA256_.Init;
  SHA256_.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA256_.Update(@SHA256.Digest, SizeOf(SHA256.Digest));
  SHA256_.Final;
  System.Move(SHA256_.Digest, aDigest, SizeOf(aDigest));
  Result:=True;
end;

// @Result[64]
class function TSHA256.HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload;

var
  aDigest: TSHA256Digest;
  S: TBytes;
begin
  aDigest:=Default(TSHA256Digest);
  Result := HMAC(PByte(Key),Length(Key), PByte(Data), Length(Data), aDigest);
  BytesFromVar(S, @aDigest[0], SizeOf(aDigest));
  BytesToHexStr(SignatureHexa,S);
end;


class procedure TSHA256.Stream(aStream: TStream; out aDigest: TSHA256Digest);

var
  aLen : LongInt;
  Buffer: array[0 .. 64*1024 - 1] of Byte;
  SHA256: TSHA256;

begin
  SHA256.Init;
  repeat
     aLen:=aStream.Read(Buffer, Length(Buffer));
     if aLen>0 then
       SHA256.Update(PByte(Buffer),aLen);
  until aLen=0;
  SHA256.Final;
  aDigest:=SHA256.Digest;
end;

class function TSHA256.Stream(aStream: TStream): TSHA256Digest;

begin
  Stream(aStream,Result);
end;


class procedure TSHA256.StreamHexa(aStream: TStream; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA256Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA256Digest));
  BytesToHexStr(Result,B);
end;

class function TSHA256.StreamHexa(aStream: TStream): AnsiString;

begin
  Result:='';
  StreamHexa(aStream,Result);
end;


class procedure TSHA256.StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA256Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA256Digest));
  BytesEncodeBase64(B,Result,isUrl,False,False);
end;

class Function TSHA256.StreamBase64(aStream: TStream; isURL : Boolean): AnsiString;

begin
  Result:='';
  StreamBase64(aStream,isURL,Result);
end;

class function TSHA256.HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean;

var
  PRK, T: TSHA256Digest;
  Round: Byte;

begin
  PRK:=Default(TSHA256Digest);
  T:=Default(TSHA256Digest);
  Result := HMAC(PByte(Salt), Length(Salt), PByte(IKM), Length(IKM), PRK);
  if not Result then
    Exit;
  Round := 1;
  while Length(Output) < DesiredLen do
  begin
    if Length(Output) = 0 then
      Result := HMAC(@PRK, SizeOf(PRK), PByte(Info), Length(Info), @Round, SizeOf(Round), nil, 0, T)
    else
      Result := HMAC(@PRK, SizeOf(PRK), @T, SizeOf(T), PByte(Info), Length(Info), @Round, SizeOf(Round), T);
    if not Result then
      Exit;
    Inc(Round);
    Output:=Concat(OutPut,BytesFromVar(@T,SizeOf(T)));
    if Length(Output) >= DesiredLen then
      Break;
  end;
  SetLength(Output,DesiredLen);
end;

//------------------------------------------------------------------------------
// SHA224
//------------------------------------------------------------------------------


procedure TSHA224.Init;
begin
  Base.Init(True);
end;

procedure TSHA224.Final;

begin
  Base.Final;
  Move(Base.Context,Digest,Sizeof(Digest));
end;

function TSHA224.IsEqual(const ADigest: TSHA224Digest): Boolean;

var
  Left, Right: TBytes;
begin
  Left:=BytesFromVar(@ADigest, SizeOf(ADigest));
  Right:=BytesFromVar(@Self.Digest, SizeOf(Self.Digest));
  Result:=CompareMem(Pointer(Left), Pointer(Right),Length(Left));
end;


procedure TSHA224.Update(PBuf: PByte; Size: UInt32);

begin
  Base.Update(PBuf,Size);
end;


procedure TSHA224.Update(const Value: TBytes);
begin
  Base.Update(Value);
end;

// @Result[64]
// 'abc' -> 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
procedure TSHA224.OutputHexa(out Result: AnsiString);

begin
  BytesToHexStr(Result,PByte(@Self.Digest),SizeOf(Self.Digest));
end;

// @Result[32]
class procedure TSHA224.DigestBytes(const Value: TBytes; out Result: TBytes);
var
  lSHA224: TSHA224;
begin
  lSHA224.Init;
  lSHA224.Update(Value);
  lSHA224.Final;
  BytesFromVar(Result, @lSHA224.Digest[0], SizeOf(lSHA224.Digest));
end;

class procedure TSHA224.DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString);
var
  S : TBytes;
  lSHA224: TSHA224;
begin
  lSHA224.Init;
  lSHA224.Update(Value);
  lSHA224.Final;
  BytesFromVar(S, @lSHA224.Digest[0], SizeOf(lSHA224.Digest));
  BytesEncodeBase64(S, Result, IsURL, False, False);
end;

// @Result[64]
Class procedure TSHA224.DigestHexa(const Value: TBytes; out Result: AnsiString);
var
  SHA224: TSHA224;
begin
  SHA224.Init;
  SHA224.Update(Value);
  SHA224.Final;
  SHA224.OutputHexa(Result);
end;

class function TSHA224.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA224Digest): Boolean;
begin
  Result := HMAC(Key, KeySize, Data, DataSize, nil, 0, nil, 0, aDigest);
end;

{Generate a SHA224 HMAC (Hashed Message Authentication Code) using the Key and Data
The SHA224 HMAC algorithm is:
 SHA224(Key xor oPad, SHA224(Key xor iPad, Data))
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
 If Key is more than 64 bytes it will be hashed to Key = SHA224(Key) instead
 If Key is less than 64 bytes it will be padded with zeros }
 class function TSHA224.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA224Digest): Boolean;

Type
  TBuf64 = array[0..63] of Byte;

var
  Count: UInt32;
  KeyBuffer, PadBuffer: TBuf64;
  SHA224, SHA224_: TSHA224;
begin
  Result:=False;
  if Key = nil then
    Exit;
  if Data = nil then
    Exit;
  KeyBuffer:=Default(TBuf64);
  SHA224.Init;
  if KeySize > 64 then
  begin
    SHA224.Update(Key, KeySize);
    SHA224.Final;
    System.Move(SHA224.Digest[0], KeyBuffer[0], SizeOf(SHA224.Digest));
  end else
    System.Move(Key^, KeyBuffer[0], KeySize);
  // XOR the key buffer with the iPad value
  for Count := 0 to 63 do
    PadBuffer[Count] := KeyBuffer[Count] xor $36;
  SHA224.Init;
  SHA224.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA224.Update(Data, DataSize);
  if Data2 <> nil then
    SHA224.Update(Data2, DataSize2);
  if Data3 <> nil then
    SHA224.Update(Data3, DataSize3);
  SHA224.Final;
  // XOR the key buffer with the oPad value
  for Count := 0 to 63 do
    PadBuffer[Count] := KeyBuffer[Count] xor $5C;
  // SHA224 the key buffer and the result of the inner SHA224 (Outer)
  SHA224_.Init;
  SHA224_.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA224_.Update(@SHA224.Digest, SizeOf(SHA224.Digest));
  SHA224_.Final;
  System.Move(SHA224_.Digest, aDigest, SizeOf(aDigest));
  Result:=True;
end;

// @Result[64]
class function TSHA224.HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload;

var
  aDigest: TSHA224Digest;
  S: TBytes;
begin
  aDigest:=Default(TSHA224Digest);
  Result := HMAC(PByte(Key),Length(Key), PByte(Data), Length(Data), aDigest);
  BytesFromVar(S, @aDigest[0], SizeOf(aDigest));
  BytesToHexStr(SignatureHexa,S);
end;


class procedure TSHA224.Stream(aStream: TStream; out aDigest: TSHA224Digest);

var
  aLen : LongInt;
  Buffer: array[0 .. 64*1024 - 1] of Byte;
  SHA224: TSHA224;

begin
  SHA224.Init;
  repeat
     aLen:=aStream.Read(Buffer, Length(Buffer));
     if aLen>0 then
       SHA224.Update(PByte(Buffer),aLen);
  until aLen=0;
  SHA224.Final;
  aDigest:=SHA224.Digest;
end;

class function TSHA224.Stream(aStream: TStream): TSHA224Digest;

begin
  Stream(aStream,Result);
end;


class procedure TSHA224.StreamHexa(aStream: TStream; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA224Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA224Digest));
  BytesToHexStr(Result,B);
end;

class function TSHA224.StreamHexa(aStream: TStream): AnsiString;

begin
  Result:='';
  StreamHexa(aStream,Result);
end;


class procedure TSHA224.StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA224Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA224Digest));
  BytesEncodeBase64(B,Result,isUrl,False,False);
end;

class Function TSHA224.StreamBase64(aStream: TStream; isURL : Boolean): AnsiString;

begin
  Result:='';
  StreamBase64(aStream,isURL,Result);
end;

class function TSHA224.HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean;

var
  PRK, T: TSHA224Digest;
  Round: Byte;

begin
  PRK:=Default(TSHA224Digest);
  T:=Default(TSHA224Digest);
  Result := HMAC(PByte(Salt), Length(Salt), PByte(IKM), Length(IKM), PRK);
  if not Result then
    Exit;
  Round := 1;
  while Length(Output) < DesiredLen do
  begin
    if Length(Output) = 0 then
      Result := HMAC(@PRK, SizeOf(PRK), PByte(Info), Length(Info), @Round, SizeOf(Round), nil, 0, T)
    else
      Result := HMAC(@PRK, SizeOf(PRK), @T, SizeOf(T), PByte(Info), Length(Info), @Round, SizeOf(Round), T);
    if not Result then
      Exit;
    Inc(Round);
    Output:=Concat(OutPut,BytesFromVar(@T,SizeOf(T)));
    if Length(Output) >= DesiredLen then
      Break;
  end;
  SetLength(Output,DesiredLen);
end;


end.


