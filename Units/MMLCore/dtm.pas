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
      property PPoints : PMDTMPoint read GetPointerPoints;
      property Count : integer read FLen write SetPointCount;
      property Points : TMDTMPointArray read FPoints;
    end;

    { TMDTMS }

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
      function StringToDTM(const S: String): TMDTM;
      property DTM[Index : integer]: TMDTM read GetDTM; default;
      constructor Create(Owner: TObject);
      destructor Destroy; override;
    end;

implementation
uses
    dtmutil, paszlib,
    client,
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

function TMDTMS.StringToDTM(const S: String): TMDTM;
var
  b: PBufferByteArray;
  Source : String;
  DestLen : longword;
  i,ii,c : integer;
  DPoints : PMDTMPoint;
begin
  Result := TMDTM.Create;
  ii := Length(S);
  if (ii = 0) or (ii mod 2 <> 0) then
    Exit;
  ii := ii div 2;
  SetLength(Source,ii);
  for i := 1 to ii do
    Source[i] := Chr(HexToInt(S[i * 2 - 1] + S[i * 2]));
  DestLen := BufferLen;
  if uncompress(Bufferstring,Destlen,pchar(Source), ii) = Z_OK then
  begin;
    if (Destlen mod 36) > 0 then
      raise Exception.CreateFmt('Invalid DTM passed to StringToDTM: %s',[s]);
    DestLen := DestLen div 36;
    Result.Count:= DestLen;
    DPoints := result.PPoints;
    b := PBufferByteArray(BufferString);
    for i := 0 to DestLen - 1 do
    begin;
      c := i * 36;
      DPoints[i].x := PInteger(@b^[c+1])^;
      DPoints[i].y := PInteger(@b^[c+5])^;
      DPoints[i].asz := PInteger(@b^[c+12])^;
//      Result.ash[i] := PInteger(@b^[c+16])^;
      DPoints[i].c := PInteger(@b^[c+20])^;
      DPoints[i].t := PInteger(@b^[c+24])^;
      DPoints[i].bp := False;
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

function TMDTM.ToString: string;
begin
  Result := '';
end;

function TMDTM.Valid: boolean;
begin
  Result := Count > 0;
end;

end.

