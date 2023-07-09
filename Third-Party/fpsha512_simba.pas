{
  This file is part of the Free Component Library.
  Copyright (c) 2021 by the Free Pascal team.

  SHA512/SHA384 and HMACSha512/HMACSha384 routines.

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fpsha512_simba;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{ $define debugsha}

interface

uses
  Classes, SysUtils;

Type
  THashBuffer = array[0..127] of Byte;

  { TSHA512 }
  TSHA512Base = record
    Context: array[0..7] of QWord;
    Buffer : THashBuffer;
    Index: UInt32;
    TotalLength: Int64;
    procedure Compress;
    procedure Final;
    procedure Init(Use384 : Boolean = False);
    procedure Update(PBuf: PByte; Size: UInt32); overload;
    procedure Update(const Value: TBytes); overload;
{$IFDEF DEBUGSHA}
  private
    procedure DumpBuffer;
    procedure DumpHash;
{$ENDIF DEBUGSHA}
  end;
  PSHA512Base = ^TSHA512Base;

  TSHA512Digest = packed array[0..63] of Byte;
  PSHA512Digest = ^TSHA512Digest;
  PSHA512 = ^TSHA512;
  TSHA512 = record
    Base : TSHA512Base;
    Digest: TSHA512Digest;
    function IsEqual(const ADigest: TSHA512Digest): Boolean;
    procedure OutputHexa(out Result: AnsiString);
    procedure Compress;
    procedure Final;
    procedure Init;
    procedure Update(PBuf: PByte; Size: UInt32); overload;
    procedure Update(const Value: TBytes); overload;
    // Calculate SHA512, return digest as bytes.
    class procedure DigestBytes(const Value: TBytes; out Result: TBytes) ; static;
    // Calculate  SHA512, return digest as base64(url) string
    class procedure DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString); static;
    // Calculate  SHA512, return digest as HEX encoded string
    class procedure DigestHexa(const Value: TBytes; out Result: AnsiString); static;
    // HMAC using SHA512 as hash
    Class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA512Digest): Boolean; overload; static;
    class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA512Digest): Boolean; overload; static;
    // Calculate HMacSHA512, return digest as hex string.
    class function HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload; static;
    // Calculate SHA512 from a stream, return digest.
    class procedure Stream(aStream: TStream; out aDigest: TSHA512Digest); static; overload;
    class function Stream(aStream: TStream): TSHA512Digest; static; overload;
    // Digest Stream, result as HexaDecimal string.
    class procedure StreamHexa(aStream: TStream; out Result: AnsiString); static; overload;
    class Function StreamHexa(aStream: TStream): AnsiString; static overload;
    // Digest Stream, result as Base64-encoded string
    class procedure StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString); static; overload;
    class Function StreamBase64(aStream: TStream; isURL : Boolean): AnsiString; static; overload;
    // HKDF : Derive key of desired length from a salt,input key and info  (RF5869, using HMACSHA512) .
    class function HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean; static;
  end;

  TSHA384Digest = packed array[0..47] of Byte;
  PSHA384Digest = ^TSHA384Digest;
  PSHA384 = ^TSHA384;

  TSHA384 = record
    Base : TSHA512Base;
    Digest: TSHA384Digest;
    function IsEqual(const ADigest: TSHA384Digest): Boolean;
    procedure OutputHexa(out Result: AnsiString);
    procedure Compress;
    procedure Final;
    procedure Init;
    procedure Update(PBuf: PByte; Size: UInt32); overload;
    procedure Update(const Value: TBytes); overload;
    // Calculate SHA384, return digest as bytes.
    class procedure DigestBytes(const Value: TBytes; out Result: TBytes) ; static;
    // Calculate  SHA384, return digest as base64(url) string
    class procedure DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString); static;
    // Calculate  SHA384, return digest as HEX encoded string
    class procedure DigestHexa(const Value: TBytes; out Result: AnsiString); static;
    // HMAC using SHA384 as hash
    Class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA384Digest): Boolean; overload; static;
    class function HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA384Digest): Boolean; overload; static;
    // Calculate HMacSHA384, return digest as hex string.
    class function HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload; static;
    // Calculate SHA384 from a stream, return digest.
    class procedure Stream(aStream: TStream; out aDigest: TSHA384Digest); static; overload;
    class function Stream(aStream: TStream): TSHA384Digest; static; overload;
    // Digest Stream, result as HexaDecimal string.
    class procedure StreamHexa(aStream: TStream; out Result: AnsiString); static; overload;
    class Function StreamHexa(aStream: TStream): AnsiString; static overload;
    // Digest Stream, result as Base64-encoded string
    class procedure StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString); static; overload;
    class Function StreamBase64(aStream: TStream; isURL : Boolean): AnsiString; static; overload;
    // HKDF : Derive key of desired length from a salt,input key and info  (RF5869, using HMACSHA384) .
    class function HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean; static;
  end;

Const
  SHA512_DIGEST_SIZE = SizeOf(TSHA512Digest); // 64
  SHA384_DIGEST_SIZE = SizeOf(TSHA384Digest); // 48

implementation

uses fphashutils_simba;

Const
  Seed512Hash : Array[0..7] of QWord
              = (QWord($6A09E667F3BCC908),
                 QWord($BB67AE8584CAA73B),
                 QWord($3C6EF372FE94F82B),
                 QWord($A54FF53A5F1D36F1),
                 QWord($510E527FADE682D1),
                 QWord($9B05688C2B3E6C1F),
                 QWord($1F83D9ABFB41BD6B),
                 QWord($5BE0CD19137E2179)
                 );
  Seed384Hash : Array[0..7] of QWord
              = (QWord($cbbb9d5dc1059ed8),
                 QWord($629a292a367cd507),
                 QWord($9159015a3070dd17),
                 QWord($152fecd8f70e5939),
                 QWord($67332667ffc00b31),
                 QWord($8eb44a8768581511),
                 QWord($db0c2e0d64f98fa7),
                 QWord($47b5481dbefa4fa4)
                 );

{ ----------------------------------------------------------------------
  TSHA512Base
  ----------------------------------------------------------------------}


procedure TSHA512Base.Init(Use384 : Boolean = False);

Var
  I : Integer ;

begin
  Self.Index := 0;
  Self.TotalLength := 0;
  FillChar(Buffer, Sizeof(Buffer), 0);
  if Not Use384 then
    begin
    For I:=0 to 7 do
      Context[i]:=Seed512Hash[i];
    end
  else
    begin
    For I:=0 to 7 do
      Context[i]:=Seed384Hash[i];
    end;
end;


{$IFDEF DEBUGSHA}
procedure Tsha512Base.DumpBuffer;

Var
 i : Integer;

begin
 For I:=0 to SizeOf(Buffer)-1 do
   Write(IntToStr(Buffer[i]),',');
  Writeln;
end;

procedure TSHA512Base.DumpHash;

Var
 i : Integer;

begin
 For I:=0 to 7 do
   Write(IntToStr(Context[i]),' ');
  Writeln;
end;
{$ENDIF}

procedure TSHA512Base.Update(const Value: TBytes);
begin
  Update(PByte(Value), System.Length(Value));
end;

procedure TSHA512Base.Update(PBuf: PByte; Size: UInt32);
var
  Len: UInt32;
begin
  Inc(TotalLength, Int64(UInt32(Size)) * 8);
  while Size > 0 do
  begin
    Len:=Sizeof(Buffer)-Index;
    if Len <= UInt32(Size) then
    begin
      Move(PBuf^, Buffer[Index], Len);
      Dec(Size, Len);
      Inc(PBuf, Len);
      Compress;
      Self.Index := 0;
    end else
    begin
      Move(PBuf^, Buffer[Index], Size);
      Inc(Self.Index, Size);
      Size := 0;
    end;
  end;
end;

procedure TSHA512Base.Final;

Var
  I : Integer;

begin
  Buffer[Index] := $80;
  FillChar(Buffer[Index+1], SizeOf(Buffer)-Index-1, 0);
  if Index >= 112 then
    Compress;
  PQWord(@Buffer[112])^ := 0;
  PQWord(@Buffer[120])^ := SwapEndian(TotalLength);
  Compress;
  For I:=0 to 7 do
    Context[i] := NtoBE(Context[i]);
end;

procedure TSHA512Base.Compress;

const
  K: array[0..79] of QWord = (
     QWord($428A2F98D728AE22),QWord($7137449123EF65CD),QWord($B5C0FBCFEC4D3B2F),QWord($E9B5DBA58189DBBC),
     QWord($3956C25BF348B538),QWord($59F111F1B605D019),QWord($923F82A4AF194F9B),QWord($AB1C5ED5DA6D8118),
     QWord($D807AA98A3030242),QWord($12835B0145706FBE),QWord($243185BE4EE4B28C),QWord($550C7DC3D5FFB4E2),
     QWord($72BE5D74F27B896F),QWord($80DEB1FE3B1696B1),QWord($9BDC06A725C71235),QWord($C19BF174CF692694),
     QWord($E49B69C19EF14AD2),QWord($EFBE4786384F25E3),QWord($0FC19DC68B8CD5B5),QWord($240CA1CC77AC9C65),
     QWord($2DE92C6F592B0275),QWord($4A7484AA6EA6E483),QWord($5CB0A9DCBD41FBD4),QWord($76F988DA831153B5),
     QWord($983E5152EE66DFAB),QWord($A831C66D2DB43210),QWord($B00327C898FB213F),QWord($BF597FC7BEEF0EE4),
     QWord($C6E00BF33DA88FC2),QWord($D5A79147930AA725),QWord($06CA6351E003826F),QWord($142929670A0E6E70),
     QWord($27B70A8546D22FFC),QWord($2E1B21385C26C926),QWord($4D2C6DFC5AC42AED),QWord($53380D139D95B3DF),
     QWord($650A73548BAF63DE),QWord($766A0ABB3C77B2A8),QWord($81C2C92E47EDAEE6),QWord($92722C851482353B),
     QWord($A2BFE8A14CF10364),QWord($A81A664BBC423001),QWord($C24B8B70D0F89791),QWord($C76C51A30654BE30),
     QWord($D192E819D6EF5218),QWord($D69906245565A910),QWord($F40E35855771202A),QWord($106AA07032BBD1B8),
     QWord($19A4C116B8D2D0C8),QWord($1E376C085141AB53),QWord($2748774CDF8EEB99),QWord($34B0BCB5E19B48A8),
     QWord($391C0CB3C5C95A63),QWord($4ED8AA4AE3418ACB),QWord($5B9CCA4F7763E373),QWord($682E6FF3D6B2B8A3),
     QWord($748F82EE5DEFB2FC),QWord($78A5636F43172F60),QWord($84C87814A1F0AB72),QWord($8CC702081A6439EC),
     QWord($90BEFFFA23631E28),QWord($A4506CEBDE82BDE9),QWord($BEF9A3F7B2C67915),QWord($C67178F2E372532B),
     QWord($CA273ECEEA26619C),QWord($D186B8C721C0C207),QWord($EADA7DD6CDE0EB1E),QWord($F57D4F7FEE6ED178),
     QWord($06F067AA72176FBA),QWord($0A637DC5A2C898A6),QWord($113F9804BEF90DAE),QWord($1B710B35131C471B),
     QWord($28DB77F523047D84),QWord($32CAAB7B40C72493),QWord($3C9EBE0A15C9BEBC),QWord($431D67C49C100D4C),
     QWord($4CC5D4BECB3E42B6),QWord($597F299CFC657E2A),QWord($5FCB6FAB3AD6FAEC),QWord($6C44198C4A475817)
   );

type
  TQWordArray = Array[0..79] of QWord;

var
  A, B, C, D, E, F, G, H: QWord;
  I: UInt32;
  t1, t2: QWord;
  W : TQWordArray;

begin
  {$IFDEF DEBUGSHA}  DumpHash; DumpBuffer; {$ENDIF}
  W:=Default(TQWordArray);
  a:= Context[0];
  b:= Context[1];
  c:= Context[2];
  d:= Context[3];
  e:= Context[4];
  f:= Context[5];
  g:= Context[6];
  h:= Context[7];
  // Fill first 16 QWords, swap endianness.
  Move(Buffer,W,SizeOf(THashBuffer));
  for i:= 0 to 15 do
    W[i]:= BeTON(W[i]);
  for i:= 0 to 79 do
    begin
    if I>=16 then
      W[i]:= W[i-16]
             // SIGMA4(x) = (ROR64(x, 19) ^ ROR64(x, 61) ^ SHR64(x, 6))
             + (((W[i-2] shr 19) or (W[i-2] shl 45))
               xor ((W[i-2] shr 61) or (W[i-2] shl 3))
               xor (W[i-2] shr 6))
             + W[i-7]
             // Sigma3 (x) = (ROR64(x, 1) ^ ROR64(x, 8) ^ SHR64(x, 7));
             + (((W[i-15] shr 1) or (W[i-15] shl 63))
                xor ((W[i-15] shr 8) or (W[i-15] shl 56))
                xor (W[i-15] shr 7));


    t1:= h
           // Sigma2(x) =  (ROR64(x, 14) ^ ROR64(x, 18) ^ ROR64(x, 41))
         + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23)))
           // CH(x, y, z) = (((x) & (y)) | (~(x) & (z)))
         + ((e and f) or (not e and g))
         + K[i] + W[i];
    t2:= // SIGMA1(x) = (ROR64(x, 28) ^ ROR64(x, 34) ^ ROR64(x, 39))
         (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) +
         // MAJ(x,y,z) = (((x) & (y)) | ((x) & (z)) | ((y) & (z)))
         ((a and b) or (a and c) or (b and c));
    h:= g;
    g:= f;
    f:= e;
    e:= d + t1;
    d:= c;
    c:= b;
    b:= a;
    a:= t1 + t2;
    end;

  Inc(Context[0], A);
  Inc(Context[1], B);
  Inc(Context[2], C);
  Inc(Context[3], D);
  Inc(Context[4], E);
  Inc(Context[5], F);
  Inc(Context[6], G);
  Inc(Context[7], H);
  FillChar(Buffer,Sizeof(Buffer),0);
  {$IFDEF DEBUGSHA} DumpHash;{$ENDIF}
end;


{ ----------------------------------------------------------------------
  TSHA512
  ----------------------------------------------------------------------}


procedure TSHA512.Init;

begin
  Base.Init(False);
end;


procedure TSHA512.Compress;

begin
  Base.Compress;
end;


procedure TSHA512.Final;

begin
  Base.Final;
  Move(Base.Context, Digest, Sizeof(Digest));
end;


function TSHA512.IsEqual(const ADigest: TSHA512Digest): Boolean;
var
  Left, Right: TBytes;
begin
  Left:=BytesFromVar(@ADigest, SizeOf(ADigest));
  Right:=BytesFromVar(@Self.Digest, SizeOf(Self.Digest));
  Result:=CompareMem(Pointer(Left), Pointer(Right),System.Length(Left));
end;

procedure TSHA512.Update(PBuf: PByte; Size: UInt32);

begin
  Base.Update(PBuf,Size);
end;

procedure TSHA512.Update(const Value: TBytes);

begin
  Base.Update(Value);
end;


class procedure TSHA512.DigestBytes(const Value: TBytes; out Result: TBytes);

var
  lSHA512: TSHA512;

begin
  lSHA512.Init;
  lSHA512.Update(Value);
  lSHA512.Final;
  BytesFromVar(Result, @lSHA512.Digest[0], SizeOf(lSHA512.Digest));
end;

class procedure TSHA512.DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString);

var
  S : TBytes;
  lSHA512: TSHA512;

begin
  lSHA512.Init;
  lSHA512.Update(Value);
  lSHA512.Final;
  BytesFromVar(S, @lSHA512.Digest[0], SizeOf(lSHA512.Digest));
  BytesEncodeBase64(S, Result, IsURL, False, False);
end;

class procedure TSHA512.DigestHexa(const Value: TBytes; out Result: AnsiString);
var
  SHA512: TSHA512;
begin
  SHA512.Init;
  SHA512.Update(Value);
  SHA512.Final;
  SHA512.OutputHexa(Result);
end;

procedure TSHA512.OutputHexa(out Result: AnsiString);

begin
  BytesToHexStr(Result,PByte(@Self.Digest),SizeOf(Self.Digest));
end;

class function TSHA512.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA512Digest): Boolean;
begin
  Result := HMAC(Key, KeySize, Data, DataSize, nil, 0, nil, 0, aDigest);
end;

{Generate a SHA512 HMAC (Hashed Message Authentication Code) using the Key and Data
The SHA512 HMAC algorithm is:
 SHA512(Key xor oPad, SHA512(Key xor iPad, Data))
 Where iPad is the byte $36 repeated 128 times
       oPad is the byte $5c repeated 128 times
 If Key is more than 128 bytes it will be hashed to Key = SHA512(Key) instead
 If Key is less than 128 bytes it will be padded with zeros }
class function TSHA512.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA512Digest): Boolean;

Type
  TBuf128 = array[0..127] of Byte;

var
  Count: UInt32;
  KeyBuffer, PadBuffer: TBuf128;
  SHA512, SHA512_: TSHA512;
begin
  Result:=False;
  if Key = nil then
    Exit;
  if Data = nil then
    Exit;
  KeyBuffer:=Default(TBuf128);
  SHA512.Init;
  if KeySize > 128 then
  begin
    SHA512.Update(Key, KeySize);
    SHA512.Final;
    System.Move(SHA512.Digest[0], KeyBuffer[0], SizeOf(SHA512.Digest));
  end else
    System.Move(Key^, KeyBuffer[0], KeySize);
  // XOR the key buffer with the iPad value
  for Count := 0 to 127 do
    PadBuffer[Count] := KeyBuffer[Count] xor $36;
  SHA512.Init;
  SHA512.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA512.Update(Data, DataSize);
  if Data2 <> nil then
    SHA512.Update(Data2, DataSize2);
  if Data3 <> nil then
    SHA512.Update(Data3, DataSize3);
  SHA512.Final;
  // XOR the key buffer with the oPad value
  for Count := 0 to 127 do
    PadBuffer[Count] := KeyBuffer[Count] xor $5C;
  // SHA512 the key buffer and the result of the inner SHA512 (Outer)
  SHA512_.Init;
  SHA512_.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA512_.Update(@SHA512.Digest, SizeOf(SHA512.Digest));
  SHA512_.Final;
  System.Move(SHA512_.Digest, aDigest, SizeOf(aDigest));
// FillChar(KeyDigest, SizeOf(TSHA1Digest),0);
// FillChar(KeyBuffer, SizeOf(TSHA1ByteBuffer),0);
// FillChar(PadBuffer, SizeOf(TSHA1ByteBuffer),0);
  Result:=True;
end;

// @Result[64]
class function TSHA512.HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload;
var
  aDigest: TSHA512Digest;
  S: TBytes;
begin
  aDigest:=Default(TSHA512Digest);
  Result := HMAC(PByte(Key),System.Length(Key), PByte(Data), System.Length(Data), aDigest);
  BytesFromVar(S, @aDigest[0], SizeOf(aDigest));
  BytesToHexStr(SignatureHexa,S);
end;


class procedure TSHA512.Stream(aStream: TStream; out aDigest: TSHA512Digest);

const
  BUFFER_SIZE = 64*1024;

var
  aLen : LongInt;
  lBuffer: TBytes;
  SHA512: TSHA512;

begin
  lBuffer:=Nil;
  SHA512.Init;
  SetLength(lBuffer,BUFFER_SIZE);
  repeat
     aLen:=aStream.Read(lBuffer, BUFFER_SIZE);
     if aLen = 0 then
       Break;
     SHA512.Update(PByte(lBuffer),aLen);
  until aLen=0;
  SHA512.Final;
  aDigest:=SHA512.Digest;
end;

class function TSHA512.Stream(aStream: TStream): TSHA512Digest;

begin
  Stream(aStream,Result);
end;


class procedure TSHA512.StreamHexa(aStream: TStream; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA512Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA512Digest));
  BytesToHexStr(Result,B);
end;

class function TSHA512.StreamHexa(aStream: TStream): AnsiString;

begin
  Result:='';
  StreamHexa(aStream,Result);
end;


class procedure TSHA512.StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA512Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA512Digest));
  BytesEncodeBase64(B,Result,isUrl,False,False);
end;

Class Function TSHA512.StreamBase64(aStream: TStream; isURL : Boolean): AnsiString;

begin
  Result:='';
  StreamBase64(aStream,isURL,Result);
end;

class function TSHA512.HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean;

var
  PRK, T: TSHA512Digest;
  Round: Byte;

begin
  PRK:=Default(TSHA512Digest);
  T:=Default(TSHA512Digest);
  Result := HMAC(PByte(Salt), System.Length(Salt), PByte(IKM), Length(IKM), PRK);
  if not Result then
    Exit;
  Round := 1;
  while System.Length(Output) < DesiredLen do
  begin
    if System.Length(Output) = 0 then
      Result := HMAC(@PRK, SizeOf(PRK), PByte(Info), System.Length(Info), @Round, SizeOf(Round), nil, 0, T)
    else
      Result := HMAC(@PRK, SizeOf(PRK), @T, SizeOf(T), PByte(Info), System.Length(Info), @Round, SizeOf(Round), T);
    if not Result then
      Exit;
    Inc(Round);
    Output:=Concat(OutPut,BytesFromVar(@T,SizeOf(T)));
    if Length(Output) >= DesiredLen then
      Break;
  end;
  SetLength(Output,DesiredLen);
end;

{ ----------------------------------------------------------------------
  TSHA384
  ----------------------------------------------------------------------}

procedure TSHA384.Init;

begin
  Base.Init(True);
end;


procedure TSHA384.Compress;

begin
  Base.Compress;
end;


procedure TSHA384.Final;

begin
  Base.Final;
  Move(Base.Context, Digest, Sizeof(Digest));
end;


function TSHA384.IsEqual(const ADigest: TSHA384Digest): Boolean;
var
  Left, Right: TBytes;
begin
  Left:=BytesFromVar(@ADigest, SizeOf(ADigest));
  Right:=BytesFromVar(@Self.Digest, SizeOf(Self.Digest));
  Result:=CompareMem(Pointer(Left), Pointer(Right),System.Length(Left));
end;

procedure TSHA384.Update(PBuf: PByte; Size: UInt32);

begin
  Base.Update(PBuf,Size);
end;

procedure TSHA384.Update(const Value: TBytes);

begin
  Base.Update(Value);
end;


class procedure TSHA384.DigestBytes(const Value: TBytes; out Result: TBytes);

var
  lSHA384: TSHA384;

begin
  lSHA384.Init;
  lSHA384.Update(Value);
  lSHA384.Final;
  BytesFromVar(Result, @lSHA384.Digest[0], SizeOf(lSHA384.Digest));
end;

class procedure TSHA384.DigestBase64(const Value: TBytes; const IsURL: Boolean; out Result: AnsiString);

var
  S : TBytes;
  lSHA384: TSHA384;

begin
  lSHA384.Init;
  lSHA384.Update(Value);
  lSHA384.Final;
  BytesFromVar(S, @lSHA384.Digest[0], SizeOf(lSHA384.Digest));
  BytesEncodeBase64(S, Result, IsURL, False, False);
end;

class procedure TSHA384.DigestHexa(const Value: TBytes; out Result: AnsiString);
var
  SHA384: TSHA384;
begin
  SHA384.Init;
  SHA384.Update(Value);
  SHA384.Final;
  SHA384.OutputHexa(Result);
end;

procedure TSHA384.OutputHexa(out Result: AnsiString);

begin
  BytesToHexStr(Result,PByte(@Self.Digest),SizeOf(Self.Digest));
end;

class function TSHA384.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; var aDigest: TSHA384Digest): Boolean;
begin
  Result := HMAC(Key, KeySize, Data, DataSize, nil, 0, nil, 0, aDigest);
end;

{Generate a SHA384 HMAC (Hashed Message Authentication Code) using the Key and Data
The SHA384 HMAC algorithm is:
 SHA384(Key xor oPad, SHA384(Key xor iPad, Data))
 Where iPad is the byte $36 repeated 128 times
       oPad is the byte $5c repeated 128 times
 If Key is more than 128 bytes it will be hashed to Key = SHA384(Key) instead
 If Key is less than 128 bytes it will be padded with zeros }
class function TSHA384.HMAC(Key: PByte; KeySize: UInt32; Data: PByte; DataSize: UInt32; Data2: PByte; DataSize2: UInt32; Data3: PByte; DataSize3: UInt32; var aDigest: TSHA384Digest): Boolean;

Type
  TBuf128 = array[0..127] of Byte;

var
  Count: UInt32;
  KeyBuffer, PadBuffer: TBuf128;
  SHA384, SHA384_: TSHA384;
begin
  Result:=False;
  if Key = nil then
    Exit;
  if Data = nil then
    Exit;
  KeyBuffer:=Default(TBuf128);
  SHA384.Init;
  if KeySize > 128 then
  begin
    SHA384.Update(Key, KeySize);
    SHA384.Final;
    System.Move(SHA384.Digest[0], KeyBuffer[0], SizeOf(SHA384.Digest));
  end else
    System.Move(Key^, KeyBuffer[0], KeySize);
  // XOR the key buffer with the iPad value
  for Count := 0 to 127 do
    PadBuffer[Count] := KeyBuffer[Count] xor $36;
  SHA384.Init;
  SHA384.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA384.Update(Data, DataSize);
  if Data2 <> nil then
    SHA384.Update(Data2, DataSize2);
  if Data3 <> nil then
    SHA384.Update(Data3, DataSize3);
  SHA384.Final;
  // XOR the key buffer with the oPad value
  for Count := 0 to 127 do
    PadBuffer[Count] := KeyBuffer[Count] xor $5C;
  // SHA384 the key buffer and the result of the inner SHA384 (Outer)
  SHA384_.Init;
  SHA384_.Update(@PadBuffer, SizeOf(PadBuffer));
  SHA384_.Update(@SHA384.Digest, SizeOf(SHA384.Digest));
  SHA384_.Final;
  System.Move(SHA384_.Digest, aDigest, SizeOf(aDigest));
// FillChar(KeyDigest, SizeOf(TSHA1Digest),0);
// FillChar(KeyBuffer, SizeOf(TSHA1ByteBuffer),0);
// FillChar(PadBuffer, SizeOf(TSHA1ByteBuffer),0);
  Result:=True;
end;

// @Result[64]
class function TSHA384.HMACHexa(const Key, Data: TBytes; out SignatureHexa: AnsiString): Boolean; overload;
var
  aDigest: TSHA384Digest;
  S: TBytes;
begin
  aDigest:=Default(TSHA384Digest);
  Result := HMAC(PByte(Key),System.Length(Key), PByte(Data), System.Length(Data), aDigest);
  BytesFromVar(S, @aDigest[0], SizeOf(aDigest));
  BytesToHexStr(SignatureHexa,S);
end;


class procedure TSHA384.Stream(aStream: TStream; out aDigest: TSHA384Digest);

const
  BUFFER_SIZE = 64*1024;

var
  aLen : LongInt;
  lBuffer: TBytes;
  SHA384: TSHA384;

begin
  lBuffer:=Nil;
  SHA384.Init;
  SetLength(lBuffer,BUFFER_SIZE);
  repeat
     aLen:=aStream.Read(lBuffer, BUFFER_SIZE);
     if aLen = 0 then
       Break;
     SHA384.Update(PByte(lBuffer),aLen);
  until aLen=0;
  SHA384.Final;
  aDigest:=SHA384.Digest;
end;

class function TSHA384.Stream(aStream: TStream): TSHA384Digest;

begin
  Stream(aStream,Result);
end;


class procedure TSHA384.StreamHexa(aStream: TStream; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA384Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA384Digest));
  BytesToHexStr(Result,B);
end;

class function TSHA384.StreamHexa(aStream: TStream): AnsiString;

begin
  Result:='';
  StreamHexa(aStream,Result);
end;


class procedure TSHA384.StreamBase64(aStream: TStream; isURL : Boolean; out Result: AnsiString);

Var
  B : TBytes;
  aDigest : TSHA384Digest;

begin
  Stream(aStream,aDigest);
  BytesFromVar(B,@aDigest,SizeOf(TSHA384Digest));
  BytesEncodeBase64(B,Result,isUrl,False,False);
end;

Class Function TSHA384.StreamBase64(aStream: TStream; isURL : Boolean): AnsiString;

begin
  Result:='';
  StreamBase64(aStream,isURL,Result);
end;

class function TSHA384.HKDF(const Salt, IKM, Info: TBytes; var Output: TBytes; const DesiredLen: Integer): Boolean;

var
  PRK, T: TSHA384Digest;
  Round: Byte;

begin
  PRK:=Default(TSHA384Digest);
  T:=Default(TSHA384Digest);
  Result := HMAC(PByte(Salt), System.Length(Salt), PByte(IKM), Length(IKM), PRK);
  if not Result then
    Exit;
  Round := 1;
  while System.Length(Output) < DesiredLen do
  begin
    if System.Length(Output) = 0 then
      Result := HMAC(@PRK, SizeOf(PRK), PByte(Info), System.Length(Info), @Round, SizeOf(Round), nil, 0, T)
    else
      Result := HMAC(@PRK, SizeOf(PRK), @T, SizeOf(T), PByte(Info), System.Length(Info), @Round, SizeOf(Round), T);
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


