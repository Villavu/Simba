{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by Michael Van Canneyt,
    member of the Free Pascal development team

    Base16,Base32,Base32-hex,Base32-crockford, Base64,Base64url encoding/decoding, with or without padding

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit basenenc_simba;

{$mode ObjFPC}{$H+}

interface

uses SysUtils;

Type
  { TAlphabetEncoder }
  TReverseAlphabet = Array[0..255] of Byte;
  TStandardEncoder = (seBase16,
                      seBase32,seBase32hex,seBase32CrockFord,
                      seBase64,seBase64URL);
  TAlphabetEncoder = Class (TObject)
  protected
    Const
      StdBits : Array[TStandardEncoder] of Byte  = (4,5,5,5,6,6);
      StdPads : Array[TStandardEncoder] of Byte  = (0,8,8,8,4,4);
      StdAlpha : Array[TStandardEncoder] of String = (
        '0123456789ABCDEF',
        'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567',
        '0123456789ABCDEFGHIJKLMNOPQRSTUV',
        '0123456789ABCDEFGHJKMNPQRSTVWXYZ',
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/',
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_');
  Private
    FBits : Byte;
    FAlphabet : TBytes;
    FReverse : TReverseAlphabet;
    FPadding : Integer;
    class var StdEncoders : Array[TStandardEncoder] of TAlphabetEncoder;
    class function GetStdEncoder(AIndex: Integer): TAlphabetEncoder; static;
  public
    // Construct an encoder with alphabet, bits per letter, padding size in bits
    Constructor Create(Const aAlphabet : AnsiString; aBits : Byte; aPadding : Integer); virtual;
    // Destroy all standard encoders
    Class Destructor Done;
    // Create a standard encoder. You must free the result
    Class Function CreateStdEncoder(Std: TStandardEncoder): TAlphabetEncoder;
    // Encode data in buffer aBuffer with length aLen. If doPad is true, add padding if needed.
    Function Encode(aBuffer : PByte; aLen : Cardinal; doPad : Boolean = True) : AnsiString; virtual; overload;
    // Encode data in buffer aBuffer. If doPad is true, add padding if needed.
    Function Encode(aBuffer : TBytes; doPad : Boolean = True) : AnsiString; overload;
    // Encode data in string aBuffer. If doPad is true, add padding if needed.
    Function Encode(const aBuffer : AnsiString; doPad : Boolean = True) : AnsiString; overload;
    // Decode aSrcBuffer with length aLen.
    // Buffer must have enough room. Calculate maximum needed room with GetDecodeLen
    Function Decode(const aSrcBuffer : PByte; aLen : Integer; ABuffer : PByte; SkipWhiteSpace : Boolean = True) : Integer; virtual; overload;
    // Buffer must have enough room. Calculate maximum needed room with GetDecodeLen
    Function Decode(const S : AnsiString; ABuffer : PByte; SkipWhiteSpace : Boolean = True) : Integer; overload;
    // Return a buffer with decoded data.
    Function Decode(const S : AnsiString; SkipWhiteSpace : Boolean = True) : TBytes; overload;
    // Return a buffer with decoded data, starting with buffer.
    Function Decode(const aBuffer: PByte; aLen : Integer) : TBytes; overload;
    // Get a decoding length for the encoded string S. May be oversized due to padding.
    Function GetDecodeLen(const S : AnsiString) : Integer;
    // Bits per characters
    Property Bits : Byte Read FBits;
    // ASCII value of characters
    Property Alphabet : TBytes Read FAlphabet;
    // Reverse byte->character map
    Property Reverse : TReverseAlphabet Read FReverse;
    // Bits of padding
    Property Padding : Integer Read FPadding;
    // Standard encoders.
    Class Property Base16 : TAlphabetEncoder Index Ord(seBase16) Read GetStdEncoder;
    Class Property Base32 : TAlphabetEncoder Index Ord(seBase32) Read GetStdEncoder;
    Class Property Base32Hex : TAlphabetEncoder Index Ord(seBase32Hex)  Read GetStdEncoder;
    Class Property Base32Crockford : TAlphabetEncoder Index Ord(seBase32Crockford)  Read GetStdEncoder;
    Class Property Base64 : TAlphabetEncoder  Index Ord(seBase64)  Read GetStdEncoder;
    Class Property Base64URL : TAlphabetEncoder  Index Ord(seBase64Url)  Read GetStdEncoder;
  end;

// Shortcut access to standard encoders.
// Do not free the results !
Function Base16 : TAlphabetEncoder;
Function Base32 : TAlphabetEncoder;
Function Base32Hex : TAlphabetEncoder;
Function Base32Crockford : TAlphabetEncoder;
Function Base64 : TAlphabetEncoder;
Function Base64URL : TAlphabetEncoder;
Function GetStandardEncoder(aEncoder : TStandardEncoder): TAlphabetEncoder;

Function GetRawStringBytes(const S : AnsiString) : TBytes;
Function GetRawStringFromBytes(B : TBytes) : RawByteString;

implementation

Function GetRawStringFromBytes(B : TBytes) : RawByteString;

Var
  L : Integer;

begin
  Result:='';
  L:=Length(B);
  SetLength(Result,L);
  If L>0 then
    Move(B[0],Result[1],L);
end;

Function GetRawStringBytes(Const S : AnsiString) : TBytes;

Var
  L : Integer;

begin
  Result:=[];
  L:=Length(S);
  SetLength(Result,L);
  If L>0 then
    Move(S[1],Result[0],L);
end;


Function TAlphabetEncoder.Encode(aBuffer : TBytes; doPad : Boolean = True) : AnsiString;

begin
  Result:=Encode(PByte(aBuffer),Length(aBuffer),DoPad);
end;


function TAlphabetEncoder.Encode(Const aBuffer: AnsiString; doPad : Boolean = True): AnsiString;

begin
  Result:=Encode(GetRawStringBytes(aBuffer),DoPad);
end;


Constructor TAlphabetEncoder.Create(const aAlphabet: AnsiString; aBits: Byte; aPadding: Integer);

Var
  I : Integer;

begin
  if (Length(aAlphabet)<2) or (Length(aAlphabet)>255) then
    Raise Exception.Create('Invalid alphabet length');
  FBits:=ABits;
  FPadding:=aPadding;
  SetLength(FAlphaBet,Length(aAlphabet));
  Move(aAlphabet[1],FAlphaBet[0],Length(aAlphabet));
  for I:=1 to Length(aAlphabet) do
    FReverse[Ord(aAlphaBet[i])]:=I;
end;


class destructor TAlphabetEncoder.Done;

Var
  Std : TStandardEncoder;

begin
  For Std in TStandardEncoder do
    FreeAndNil(StdEncoders[Std]);
end;


class function TAlphabetEncoder.CreateStdEncoder(Std : TStandardEncoder) : TAlphabetEncoder;

begin
  Result:=TAlphaBetEncoder.Create(StdAlpha[Std],StdBits[Std],StdPads[Std]);
end;


class function TAlphabetEncoder.GetStdEncoder(AIndex: Integer): TAlphabetEncoder; static;

Var
  Std : TStandardEncoder;

begin
  Std:=TStandardEncoder(aIndex);
  if (StdEncoders[Std]=Nil) then
    StdEncoders[Std]:=CreateStdEncoder(Std);
  Result:=StdEncoders[Std];
end;


function TAlphabetEncoder.Encode(aBuffer: PByte; Alen : Cardinal; doPad : Boolean = True): Ansistring;

var
  pSrc, pDest: pByte;
  I, Reg, lBits, PadLen,OutLen: integer;


begin
  Result:='';
  Reg:=0;
  lBits:=0;
  PadLen:=0;
  OutLen:=aLen*8;
  OutLen:=(OutLen div Bits)+Ord((OutLen mod Bits) > 0 );
  if DoPad and (Padding>0) then
    begin
    PadLen:=OutLen mod Padding;
    if PadLen>0 then
      Inc(OutLen,(Padding-PadLen));
    end;
  SetLength(Result,OutLen);
  pSrc:=aBuffer;
  pDest:=@Result[1];
  for i:=1 to aLen do
    begin
    Reg:=Reg shl 8;
    Reg:=Reg or pSrc^;
    Inc(lBits,8);
    inc(pSrc);
    while (lBits>=Bits) do
      begin
      Dec(lBits,Bits);
      pDest^:=Alphabet[(Reg shr lBits)];
      Reg:= Reg-((Reg shr lBits) shl lBits);
      inc(pDest);
      end;
    end;
  if (lBits>0) then
    begin
    pDest^:=Alphabet[Reg shl (Bits-lBits)];
    inc(pDest);
    end;
  if DoPad and (PadLen>0) then
    FillChar(pDest^,Padding-PadLen,'=');
end;


Function TAlphabetEncoder.Decode(const aSrcBuffer : PByte; aLen : Integer; ABuffer : PByte;SkipWhiteSpace : Boolean = True) : Integer;

Const
  WhiteSpace = [Ord(' '),10,13,9];

var
  i, Reg, lBits : Integer;
  pSrc, pDest: pByte;

begin
  Reg:=0;
  lBits:=0;
  Result:=0;
  while (aLen>0) and (aSrcBuffer[aLen-1]=Ord('=')) do
    Dec(aLen);
  if Alen=0 then exit;
  pSrc:=@aSrcBuffer[0];
  pDest:=aBuffer;
  I:=1;
  While (i<=aLen) do
    begin
    if SkipWhiteSpace then
      begin
      While (PSrc^ in WhiteSpace) and (I<=aLen) do
        begin
        Inc(PSrc);
        Inc(I);
        end;
      if I>aLen then
        break;
      end;
    if Reverse[pSrc^] <= 0 then
      break;
    Reg:=Reg shl Bits;
    Reg:=Reg or (Reverse[pSrc^]-1);
    Inc(lBits,Bits);
    while (lBits>=8) do
      begin
      Dec(lBits,8);
      pDest^:=byte(Reg shr lBits);
      inc(pDest);
      end;
    inc(pSrc);
    Inc(i);
    end;
  Result:=pDest-aBuffer;
end;


Function TAlphabetEncoder.GetDecodeLen(const S : AnsiString) : Integer;

begin
  Result:=(length(s)*Bits) div 8;
end;


function TAlphabetEncoder.Decode(const S: AnsiString; SkipWhiteSpace : Boolean = True): TBytes;

begin
  Result:=[];
  SetLength(Result,GetDecodeLen(S));
  SetLength(Result,Decode(S,PByte(Result),SkipWhiteSpace));
end;


function TAlphabetEncoder.Decode(const aBuffer: PByte; aLen: Integer): TBytes;

begin
  Result:=[];
  SetLength(Result,(aLen*Bits) div 8);
  SetLength(Result,Decode(aBuffer,aLen,PByte(Result)));
end;


Function TAlphabetEncoder.Decode(const S : AnsiString; ABuffer : PByte;SkipWhiteSpace : Boolean = True) : Integer; overload;

begin
  Result:=Decode(PByte(S),Length(S),ABuffer,SkipWhiteSpace);
end;


Function Base16 : TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.Base16;
end;


Function Base32 : TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.Base32;
end;


Function Base32Hex : TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.Base32Hex;
end;


Function Base32CrockFord : TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.Base32CrockFord;
end;


Function Base64 : TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.Base64;
end;


Function Base64URL : TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.Base64URL;
end;


Function GetStandardEncoder(aEncoder : TStandardEncoder): TAlphabetEncoder;

begin
  Result:=TAlphabetEncoder.GetStdEncoder(Ord(aEncoder));
end;

end.


