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

    DTM class for the Mufasa Macro Library
}

unit dtm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes;

type

    { TMDTM }

    { TMDTM }

    TMDTM = class(TObject)
    private
      FPoints : TMDTMPointArray;
      FLen    : integer;
      function GetPointerPoints: PMDTMPoint;
      procedure SetPointCount(const AValue: integer);
    public
      Name : string;
      Index : integer;
      function ToString : string;
      function Valid : boolean;
      procedure DeletePoint( Point : integer);
      procedure SwapPoint(p1,p2 : integer);
      procedure MovePoint(fromIndex,toIndex : integer);
      procedure AddPoint( Point : TMDTMPoint);
      property PPoints : PMDTMPoint read GetPointerPoints;
      property Count : integer read FLen write SetPointCount;
      property Points : TMDTMPointArray read FPoints;
    end;
    TMDTMS = class(TObject) //Manages the DTMs   TMufasaDTMs
    private
      Client: TObject;
      DTMList: Array Of TMDTM;
      FreeSpots: Array Of Integer;
      procedure CheckIndex(index : integer);
    public
      function AddDTM(const d: TSDTM): Integer;overload;
      function AddDTM(const d: TMDTM): Integer;overload;
      function GetDTM(index: Integer) :TMDTM;
      procedure FreeDTM(DTM: Integer);
      function StringToDTM(const S: String): Integer;
      property DTM[Index : integer]: TMDTM read GetDTM; default;
      constructor Create(Owner: TObject);
      destructor Destroy; override;
    end;

implementation
uses
    dtmutil, paszlib,
    client,
    DCPbase64,
    graphics, // for TColor
    math // for max
    ;




constructor TMDTMS.Create(Owner: TObject);
begin
  inherited Create;
  Self.Client := Owner;

  SetLength(DTMList, 0);
  SetLength(FreeSpots, 0);
end;

{$DEFINE DTM_DEBUG}
destructor TMDTMS.Destroy;
var
  i, j: integer;
  b:boolean;
  WriteStr : string;
begin
  WriteStr := '[';
  for i := 0 to high(DTMList) do
  begin
    b := false;
    for j := 0 to high(freespots) do
      if i = freespots[j] then
      begin
        b := true;
        break;
      end;
      if not b then
      begin;
        if DTMList[i].name <> '' then
          WriteStr := WriteStr + DTMList[i].name + ', '
        else
          WriteStr := WriteStr + inttostr(i) + ', ';
        FreeDTM(i);
      end;
  end;
  if WriteStr <> '[' then  //Has unfreed DTMs
  begin
    SetLength(WriteStr,length(WriteStr)-1);
    WriteStr[Length(writeStr)] := ']';
    TClient(Client).Writeln(Format('The following DTMs were not freed: %s',[WriteStr]));
  end;
  SetLength(DTMList, 0);
  SetLength(FreeSpots, 0);

  inherited Destroy;
end;

//  Rotates the given point (p) by A (in radians) around the point defined by cx, cy.

function RotatePoint(const p: TPoint;const angle, mx, my: Extended): TPoint; inline;
begin
  Result.X := Round(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Round(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
end;

function HexToInt(const HexNum: string): LongInt;inline;
begin
   Result:=StrToInt('$' + HexNum);
end;

function TMDTMS.StringToDTM(const S: String): Integer;
var
  b: PBufferByteArray;
  MDTM : TMDTM;
  Source : String;
  DestLen : longword;
  i,ii,c : integer;
  DPoints : PMDTMPoint;
  Ptr : Pointer;
function ReadInteger : integer;
begin
  Result := PInteger(ptr)^;
  inc(ptr,sizeof(integer));
end;
function ReadBoolean : boolean;
begin
  result := PBoolean(ptr)^;
  inc(ptr,sizeof(boolean));
end;

begin
  MDTM := TMDTM.Create;
  Result := AddDTM(MDTM);
  ii := Length(S);
  if (ii = 0) then
    exit;
  if S[1] = 'm' then
  begin
    if ii < 9 then
      raise Exception.CreateFMT('Invalid DTM-String passed to StringToDTM: %s',[s]);
    Writeln(copy(s,2,ii-1));
    Source := Base64DecodeStr(copy(s,2,ii-1));
    i:= PLongint(@source[1])^; //The 4 four bytes should contain the dest len!
    if i < 1 then
      raise Exception.CreateFMT('Invalid DTM-String passed to StringToDTM: %s',[s]);
    DestLen := BufferLen;
    ptr := @Source[1 + sizeof(longint)];
    if uncompress(BufferString,DestLen,ptr,length(source)-sizeof(integer)) = Z_OK then
    begin
      ptr := BufferString;
      MDTM.Count:= ReadInteger;
      ii := MDTM.Count;
      if (MDTM.Count * TMDTMPointSize) <> (Destlen - SizeOf(integer)) then
        raise Exception.CreateFMT('Invalid DTM-String passed to StringToDTM: %s',[s]);
      DPoints := MDTM.PPoints;
      for i := 0 to ii-1 do
        DPoints[i].x := ReadInteger;
      for i := 0 to ii-1 do
        DPoints[i].y := ReadInteger;
      for i := 0 to ii-1 do
        DPoints[i].c := ReadInteger;
      for i := 0 to ii-1 do
        DPoints[i].t := ReadInteger;
      for i := 0 to ii-1 do
        DPoints[i].asz := ReadInteger;
      for i := 0 to ii-1 do
        DPoints[i].bp := ReadBoolean;
    end;
  end else
  begin
    if (ii mod 2 <> 0) then
      exit;
    ii := ii div 2;
    SetLength(Source,ii);
    for i := 1 to ii do
      Source[i] := Chr(HexToInt(S[i * 2 - 1] + S[i * 2]));
    DestLen := BufferLen;
    if uncompress(Bufferstring,Destlen,pchar(Source), ii) = Z_OK then
    begin;
      if (Destlen mod 36) > 0 then
        raise Exception.CreateFMT('Invalid DTM-String passed to StringToDTM: %s',[s]);
      DestLen := DestLen div 36;
      MDTM.Count:= DestLen;
      DPoints := MDTM.PPoints;
      b := PBufferByteArray(BufferString);
      for i := 0 to DestLen - 1 do
      begin;
        c := i * 36;
        DPoints[i].x := PInteger(@b^[c+1])^;
        DPoints[i].y := PInteger(@b^[c+5])^;
        DPoints[i].asz := PInteger(@b^[c+12])^;
  //    DPoints.ash[i] := PInteger(@b^[c+16])^;
        DPoints[i].c := PInteger(@b^[c+20])^;
        DPoints[i].t := PInteger(@b^[c+24])^;
        DPoints[i].bp := False;
      end;
    end;
  end;
end;


procedure TMDTMS.CheckIndex(index: integer);
begin
  if (index < 0) or (index >= Length(DTMList)) or (DTMList[Index] = nil) then
    raise Exception.CreateFmt('The given DTM Index[%d] doesn''t exist',[index]);
end;

function TMDTMS.AddDTM(const d: TSDTM): Integer;
begin
  Result := AddDTM(SDTMToMDTM(d));
end;

{/\
  Adds the given pDTM to the DTM Array, and returns it's index.
/\}

function TMDTMS.AddDTM(const d: TMDTM): Integer;
begin

  if Length(FreeSpots) > 0 then
  begin
    Result := FreeSpots[High(FreeSpots)];
    SetLength(FreeSpots, High(FreeSpots));
  end
  else
  begin
    SetLength(DTMList, Length(DTMList) + 1);
    Result := High(DTMList);
  end;
  DTMList[Result] := d;
  DTMList[Result].Index:= Result;
  NormalizeDTM(DTMList[result]);
end;

{/\
   Returns the DTM (pDTM type) in the variable dtm at the given index.
   Returns true is succesfull, false if the dtm does not exist.
/\}

function TMDTMS.GetDTM(index: Integer) :TMDTM;
begin
  CheckIndex(index);
  result := DTMList[index];
end;

{/\
  Unloads the DTM at the given index from the DTM Array.
  Notes:
  Will keep track of not used index, so it is very memory efficient.
/\}

procedure TMDTMS.FreeDTM(DTM: Integer);
begin
  CheckIndex(DTM);
  DTMList[DTM].Free;
  DTMList[DTM] := nil;
  SetLength(FreeSpots, Length(FreeSpots) + 1);
  FreeSpots[High(FreeSpots)] := DTM;
end;

{ TMDTM }

function TMDTM.GetPointerPoints: PMDTMPoint;
begin
  if count < 1 then
    result := nil
  else
    result := @FPoints[0];
end;

procedure TMDTM.SetPointCount(const AValue: integer);
begin
  SetLength(FPoints,AValue);
  FLen := AValue;
end;

//We save the data as arrays for each point (so we can add features to DTMs without having to change
//the way the ToString is done..
//E.G. A DTM with 3 points would become
//LenXXXYYYCCCTTTASZASZASZBPBPBP

function TMDTM.ToString: string;
var
  i,len : integer;
  Ptr,Start : Pointer;
  Destlen : Longword;
procedure WriteInteger(int : integer);
begin
  PLongInt(Ptr)^ := int;
  Inc(ptr,sizeof(int));
end;
procedure WriteBool(bool : boolean);
begin;
  PBoolean(Ptr)^ := bool;
  inc(ptr,sizeof(boolean));
end;

begin
  result := '';
  if Count < 1 then
    exit;
  len := Count * TMDTMPointSize + SizeOf(Integer);
  Start:= GetMem(len);
  Ptr := Start;
  WriteInteger(FLen);
  for i := 0 to FLen-1 do
    WriteInteger(FPoints[i].x);
  for i := 0 to FLen-1 do
    WriteInteger(FPoints[i].y);
  for i := 0 to FLen-1 do
    WriteInteger(FPoints[i].c);
  for i := 0 to FLen-1 do
    WriteInteger(FPoints[i].t);
  for i := 0 to FLen-1 do
    WriteInteger(FPoints[i].asz);
  for i := 0 to FLen-1 do
    WriteBool(FPoints[i].bp);
  Destlen :=BufferLen;
  if compress(BufferString,destlen,pchar(start),len) = Z_OK then
  begin
    setlength(result,Destlen + SizeOf(Integer));
    PInteger(@result[1])^ := len;
    Writeln(len);
    Move(bufferstring[0],result[1 + sizeof(integer)],Destlen);
    //We now have Size + Compressed data.. Lets Base64Encrypt it!
    Result := 'm' + Base64EncodeStr(result);
    //It now looks like m + base64encoded data! The 'm' is to indicate we used this encryption method.
  end;
  Freemem(start,len);
end;

function TMDTM.Valid: boolean;
begin
  result := false;
  if Count < 1 then
    exit;
  NormalizeDTM(self);
  result := true;
end;

procedure TMDTM.DeletePoint(Point: integer);
begin
  MovePoint(Point,FLen-1);
  Count := Count - 1;
end;

procedure TMDTM.SwapPoint(p1, p2: integer);
var
  Temp : TMDTMPoint;
begin
  Temp := FPoints[p1];
  FPoints[p1] := FPoints[p2];
  FPoints[p2] := Temp;
end;

procedure TMDTM.MovePoint(fromIndex, toIndex: integer);
var
  i : integer;
begin
  if fromIndex > toIndex then //We are going down
  begin
    for i := fromindex downto Toindex+1 do
      SwapPoint(i,i-1);
  end else if fromIndex < toIndex then
    for i := fromindex to toindex - 1 do
      SwapPoint(i,i+1);
end;

procedure TMDTM.AddPoint(Point: TMDTMPoint);
begin
  Count:= Count + 1;
  FPoints[FLen-1] := Point;
end;

end.

