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

    DTM class for the Mufasa Macro Library
}

unit simba.dtm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simba.mufasatypes;

type

    { TMDTM }

    PMDTM = ^TMDTM;
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
      function SaveToFile(const FileName : string) : boolean;
      function LoadFromString(const s : string) : boolean;
      procedure Normalize;
      function Valid : boolean;
      procedure DeletePoint(Point : integer);
      procedure SwapPoint(p1, p2: integer);
      procedure MovePoint(fromIndex, toIndex: integer);
      function AddPoint(Point: TMDTMPoint): integer;
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
	  function ExistsDTM(index : integer) : boolean;
      function GetDTM(index: Integer) :TMDTM;
      procedure FreeDTM(DTM: Integer);
      function StringToDTM(const S: String): Integer;
      property DTM[Index : integer]: TMDTM read GetDTM; default;
      constructor Create(Owner: TObject);
      destructor Destroy; override;
    end;

implementation
uses
    simba.dtmutil, paszlib,
    simba.client, simba.stringutil,
    base64,
    graphics,
    math;

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
  aDTM : TMDTM;
begin
  aDTM := TMDTM.Create;
  aDTM.LoadFromString(s);
  Result := AddDTM(aDTM);
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
  DTMList[result].Normalize;
end;

function TMDTMS.ExistsDTM(index : integer) : boolean;
begin
  result := false;
  if (index >= 0) and (index <= High(DTMList)) then
    result := Assigned(DTMList[index]);
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
  i: Int32;
  Ptr: Pointer;

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

var
  Data: String;
begin
  Result := '';
  if Count < 1 then
    Exit;

  SetLength(Data, Count * TMDTMPointSize + SizeOf(Integer));
  Ptr := @Data[1];

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

  Result := 'm' + Base64Encode(CompressString(Data));
end;

function TMDTM.SaveToFile(const FileName: string): boolean;
begin
  Result := False;
end;

function TMDTM.LoadFromString(const s: string): boolean;
var
  Source : String;
  i,c : integer;
  Ptr : Pointer;

  function ReadInteger : integer;
  begin
    Result := PInteger(ptr)^;
    WRiteln(Result);
    inc(ptr,sizeof(integer));
  end;
  function ReadBoolean : boolean;
  begin
    result := PBoolean(ptr)^;
    WRiteln(Result);
    inc(ptr,sizeof(boolean));
  end;

begin
  Result := False;
  if (Length(S) = 0) then
    Exit;

  if S[1] = 'm' then
  begin
    Source := DecompressString(Base64Decode(s.Remove(0, 1)), True);
    if (Source <> '') then
    begin
      ptr := @Source[1];
      Self.Count:= ReadInteger();
      for i := 0 to Self.Count-1 do
        Self.PPoints[i].x := ReadInteger();
      for i := 0 to Self.Count-1 do
        Self.PPoints[i].y := ReadInteger();
      for i := 0 to Self.Count-1 do
        Self.PPoints[i].c := ReadInteger();
      for i := 0 to Self.Count-1 do
        Self.PPoints[i].t := ReadInteger();
      for i := 0 to Self.Count-1 do
        Self.PPoints[i].asz := ReadInteger();
      for i := 0 to Self.Count-1 do
        Self.PPoints[i].bp := ReadBoolean();

      Result := True;
    end;
  end;

  if not Result then
    raise Exception.Create('Invalid DTM string: "' + S + '"');

  Normalize();
end;

procedure TMDTM.Normalize;
var
   i:integer;
begin
  if (self = nil) or (Self.count < 1) or ((Self.Points[0].x = 0) and (Self.Points[0].y = 0)) then  //Already normalized
    exit;
  for i := 1 to Self.Count - 1 do
  begin
    Self.Points[i].x := Self.Points[i].x - Self.Points[0].x;
    Self.Points[i].y := Self.Points[i].y - Self.Points[0].y;
  end;
  Self.Points[0].x := 0;
  Self.Points[0].y := 0;
end;

function TMDTM.Valid: boolean;
begin
  result := false;
  if Count < 1 then
    exit;
  Normalize;
  result := true;
end;

procedure TMDTM.DeletePoint(Point: integer);
begin
  MovePoint(Point, FLen - 1);
  Count := Count - 1;
end;

procedure TMDTM.SwapPoint(p1, p2: integer);
var
  tempP: TMDTMPoint;
begin
  tempP := FPoints[p1];

  FPoints[p1] := FPoints[p2];
  FPoints[p2] := tempP;
end;

procedure TMDTM.MovePoint(fromIndex, toIndex: integer);
var
  i: integer;
begin
  if fromIndex > toIndex then //We are going down
  begin
    for i := fromindex downto Toindex + 1 do
      SwapPoint(i, i - 1);
  end else if fromIndex < toIndex then
    for i := fromindex to toindex - 1 do
      SwapPoint(i, i + 1);
end;

function TMDTM.AddPoint(Point: TMDTMPoint): integer;
begin
  Count := Count + 1;
  Result := FLen - 1;
  FPoints[Result] := Point;
end;

end.

