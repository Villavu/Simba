{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dtm;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.baseclass;

const
  TMDTMPointSize = 5*SizeOf(integer)+Sizeof(boolean);
type
  TMDTMPoint = record
    x,y,c,t,asz : integer;
    bp : boolean;
  end;

  PMDTMPoint = ^TMDTMPoint;
  TMDTMPointArray = array of TMDTMPoint;

  TSDTMPointDef = record
    x, y, Color, Tolerance, AreaSize, AreaShape: integer;
  end;

  TSDTMPointDefArray = array of TSDTMPointDef;

  PSDTM = ^TSDTM;
  TSDTM = record
    MainPoint: TSDTMPointDef;
    SubPoints: TSDTMPointDefArray;
  end;

  PMDTMPointArray = ^TMDTMPointArray;
  PPMDTMPoint = ^PMDTMPoint;
  PMDTM = ^TMDTM;
  TMDTM = class(TSimbaBaseClass)
  private
    FPoints : TMDTMPointArray;
    FLen    : integer;
    function GetPointerPoints: PMDTMPoint;
    procedure SetPointCount(const AValue: integer);
  public
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

  PMDTMS = ^TMDTMS;
  TMDTMS = class(TObject)
  private
    FClient: TObject;
    FList: TList;
  public
    function AddDTM(const DTM: TSDTM): Integer; overload;
    function AddDTM(const DTM: TMDTM): Integer; overload;
    function ExistsDTM(Index: Integer): Boolean;
    function GetDTM(Index: Integer): TMDTM;
    procedure FreeDTM(Index: Integer);
    function StringToDTM(const S: String): Integer;
    property DTM[Index: Integer]: TMDTM read GetDTM; default;
    constructor Create(Owner: TObject);
    destructor Destroy; override;
  end;

implementation

uses
  simba.dtmutil, simba.stringutil;

constructor TMDTMS.Create(Owner: TObject);
begin
  inherited Create;

  FClient := Owner;
  FList := TList.Create();
end;

destructor TMDTMS.Destroy;
begin
  if (FList <> nil) then
    FreeAndNil(FList);

  inherited Destroy();
end;

function TMDTMS.StringToDTM(const S: String): Integer;
var
  aDTM : TMDTM;
begin
  aDTM := TMDTM.Create;
  aDTM.LoadFromString(s);
  Result := AddDTM(aDTM);
end;

function TMDTMS.AddDTM(const DTM: TSDTM): Integer;
begin
  Result := AddDTM(SDTMToMDTM(DTM));
end;

{/\
  Adds the given pDTM to the DTM Array, and returns it's index.
/\}

function TMDTMS.AddDTM(const DTM: TMDTM): Integer;
begin
  Result := FList.IndexOf(nil);
  if (Result = -1) then
    Result := FList.Add(nil);

  FList[Result] := DTM;

  DTM.Index := Result;
  DTM.Name := 'DTM[' + IntToStr(Result) + ']';
  DTM.Normalize();
end;

function TMDTMS.ExistsDTM(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FList.Count) and (FList[Index] <> nil);
end;

{/\
   Returns the DTM (pDTM type) in the variable dtm at the given index.
   Returns true is succesfull, false if the dtm does not exist.
/\}

function TMDTMS.GetDTM(Index: Integer): TMDTM;
begin
  if not ExistsDTM(Index) then
    raise Exception.CreateFmt('The DTM[%d] does not exist', [Index]);

  Result := TMDTM(FList[Index]);
end;

{/\
  Unloads the DTM at the given index from the DTM Array.
/\}

procedure TMDTMS.FreeDTM(Index: Integer);
begin
  GetDTM(Index).Free();

  FList[Index] := nil;
end;

function TMDTM.GetPointerPoints: PMDTMPoint;
begin
  if count < 1 then
    result := nil
  else
    result := @FPoints[0];
end;

procedure TMDTM.SetPointCount(const AValue: Integer);
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

  procedure WriteInteger(int : Integer);
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
  i: Integer;
  Ptr : Pointer;

  function ReadInteger : Integer;
  begin
    Result := PInteger(ptr)^;
    inc(ptr,sizeof(Integer));
  end;
  function ReadBoolean : boolean;
  begin
    result := PBoolean(ptr)^;
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
   i:Integer;
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
  Result := Count > 1;
  if Result then
    Normalize();
end;

procedure TMDTM.DeletePoint(Point: Integer);
begin
  MovePoint(Point, FLen - 1);
  Count := Count - 1;
end;

procedure TMDTM.SwapPoint(p1, p2: Integer);
var
  tempP: TMDTMPoint;
begin
  tempP := FPoints[p1];

  FPoints[p1] := FPoints[p2];
  FPoints[p2] := tempP;
end;

procedure TMDTM.MovePoint(fromIndex, toIndex: Integer);
var
  i: Integer;
begin
  if fromIndex > toIndex then //We are going down
  begin
    for i := fromindex downto Toindex + 1 do
      SwapPoint(i, i - 1);
  end else if fromIndex < toIndex then
    for i := fromindex to toindex - 1 do
      SwapPoint(i, i + 1);
end;

function TMDTM.AddPoint(Point: TMDTMPoint): Integer;
begin
  Count := Count + 1;
  Result := FLen - 1;
  FPoints[Result] := Point;
end;

end.

