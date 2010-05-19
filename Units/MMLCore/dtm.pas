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
      Client: TObject;
      DTMList: Array Of PpDTM;
      FreeSpots: Array Of Integer;
      procedure CheckIndex(index : integer);
    public
      function AddDTM(const d: TDTM): Integer;
      function AddpDTM(const d: pDTM): Integer;
      function GetDTM(index: Integer) :ppDTM;
      procedure FreeDTM(DTM: Integer);
      function StringToDTM(const S: String): pDTM;
      function DTMToString(const DTM : PDTM) : string;
      procedure SetDTMName(DTM: Integer;const S: String);
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




constructor TMDTM.Create(Owner: TObject);
begin
  inherited Create;
  Self.Client := Owner;

  SetLength(DTMList, 0);
  SetLength(FreeSpots, 0);
end;

{$DEFINE DTM_DEBUG}
destructor TMDTM.Destroy;
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
        if DTMList[i]^.n <> '' then
          WriteStr := WriteStr + DTMList[i]^.n + ', '
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

function TMDTM.StringToDTM(const S: String): pDTM;
var
  b: PBufferByteArray;
  Source : String;
  DestLen : longword;
  i,ii,c : integer;
begin
  SetLength(Result.p,0);
  SetLength(Result.c,0);
  SetLength(Result.t,0);
  SetLength(Result.asz,0);
  SetLength(Result.ash,0);
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
    SetLength(Result.p,DestLen);
    SetLength(Result.c,DestLen);
    SetLength(Result.t,DestLen);
    SetLength(Result.asz,DestLen);
    SetLength(Result.ash,DestLen);
    SetLength(Result.bp,DestLen);
    b := PBufferByteArray(BufferString);
    for i := 0 to DestLen - 1 do
    begin;
      c := i * 36;
      Result.p[i].x := PInteger(@b^[c+1])^;
      Result.p[i].y := PInteger(@b^[c+5])^;
      Result.asz[i] := PInteger(@b^[c+12])^;
      Result.ash[i] := PInteger(@b^[c+16])^;
      Result.c[i] := PInteger(@b^[c+20])^;
      Result.t[i] := PInteger(@b^[c+24])^;
      Result.bp[i] := False;
    end;
  end;
  result.l := length(result.p);
end;


function TMDTM.DTMToString(const DTM: PDTM): string;
var
  i : integer;
begin
  if DTM.l = 0 then
    exit;
end;

procedure TMDTM.CheckIndex(index: integer);
begin
  if (index < 0) or (index >= Length(DTMList)) or (DTMList[Index] = nil) then
    raise Exception.CreateFmt('The given DTM Index[%d] doesn''t exist',[index]);
end;

function TMDTM.AddDTM(const d: TDTM): Integer;
begin
  Result := AddpDTM(tDTMTopDTM(d));
end;

{/\
  Adds the given pDTM to the DTM Array, and returns it's index.
/\}

function TMDTM.AddpDTM(const d: pDTM): Integer;
var
  NewDTM : PpDTM;
begin
  New(NewDTM);
  NewDTM^ := d;

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
  DTMList[Result] := NewDTM;
  NormalizeDTM(DTMList[result]^);
end;

{/\
   Returns the DTM (pDTM type) in the variable dtm at the given index.
   Returns true is succesfull, false if the dtm does not exist.
/\}

function TMDTM.GetDTM(index: Integer) :ppDTM;
begin
  CheckIndex(index);
  result := DTMList[index];
end;

procedure TMDTM.SetDTMName(DTM: Integer;const s: string);
begin
  CheckIndex(DTM);
  DTMList[DTM]^.n := s;
end;

{/\
  Unloads the DTM at the given index from the DTM Array.
  Notes:
  Will keep track of not used index, so it is very memory efficient.
/\}

procedure TMDTM.FreeDTM(DTM: Integer);
begin
  CheckIndex(DTM);
  with DTMList[DTM]^ do
  begin
    SetLength(p, 0);
    SetLength(c, 0);
    SetLength(t, 0);
    SetLength(asz, 0);
    SetLength(ash, 0);
    SetLength(bp,0);
    l := 0;
    n := '';
  end;
  Dispose(DTMList[DTM]);
  DTMList[DTM] := nil;
  SetLength(FreeSpots, Length(FreeSpots) + 1);
  FreeSpots[High(FreeSpots)] := DTM;
end;

{wat}
// Then, first find all occurances of all colours on the given client.
// Each point has a colour, and we call them C_0...C_n.
// MP denotes the points of the main point colour on the client.
// P_i denotes the points on the client for C_i
// O_i denotes the point offset, and possible area shape and size.
// B_i denotes a boolean representation of P_i for C_i, for C_1...C_n.
// B_0 and O_0 are the merry exception here, as we don't need them for C_0,
// which we will show later.

// I hope it is clear how this will be respresented in computer data
// structures.

// Now, we iterate for i in range(1, n),
  // We use MP_i, and iterate for j in range(0, dtm_points),
    // Calculate the B_j indices (with MP_i and O_j) for each j, and
    // see if B_j is not true, go on with MP_i + 1.
    // Possible using areasize/shape.

    // else, if B_j is true, continue with this inner loop.
  // If B_{0...dtm_points} were all true, the point is valid.

{/\
  Tries to find the given DTM (index). If found will put the point the dtm has
  been found at in x, y and result to true.
  Will rotate the DTM starting at sAngle, increasing by aStep until eAngle has been reached, or when the DTM has been found.
  Returns all Angles in an Extended array.
/\}

{function TMDTM.FindDTMRotated(DTM: Integer; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;
Var
   temp: pDTM;
Begin
  If GetDTM(DTM, temp) Then
    Result := pFindDTMRotated(temp, x, y, x1, y1, x2, y2, sAngle, eAngle, aStep, aFound)
  else
  Begin
    x := 0;
    y := 0;
    aFound := 0.0;
    Result := False;
  end;
end;  }

{/\
  Tries to find the given pDTM. If found will put the point the dtm has
  been found at in x, y and result to true.
  Will rotate the DTM starting at sAngle, increasing by aStep until eAngle has been reached, or when the DTM has been found.
  Returns all Angles in an Extended array.
/\}

{function TMDTM.pFindDTMRotated(DTM: pDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;

Begin

end;   }

{/\
  Tries to find the given DTM (index). Will return true if it has found one or more
  DTM's. All the occurances are stored in the Points (TPointArray)
  Will rotate the DTM starting at sAngle, increasing by aStep until eAngle has been reached.
  Does not stop rotating when one occurance of a DTM has been found.
  Returns all Angles in a Two Dimensional Extended array.
/\}

{function TMDTM.FindDTMsRotated(DTM: Integer; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray): Boolean;
Var
   temp: pDTM;
Begin
  If GetDTM(DTM, temp) Then
    Result := pFindDTMsRotated(temp, Points, x1, y1, x2, y2, sAngle, eAngle, aStep, aFound)
  else
  Begin
    SetLength(Points, 0);
    SetLength(aFound, 0);
    Result := False;
  end;
end;    }

{/\
  Tries to find the given pDTM. Will return true if it has found one or more
  DTM's. All the occurances are stored in the Points (TPointArray)
  Will rotate the DTM starting at sAngle, increasing by aStep until eAngle has been reached.
  Does not stop rotating when one occurance of a DTM has been found.
  Returns all Angles in a Two Dimensional Extended array.
/\}

end.

