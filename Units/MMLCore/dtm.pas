unit dtm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes;

type
    TMDTM = class(TObject)

           function AddDTM(d: TDTM): Integer;
           function AddpDTM(d: pDTM): Integer;
           function GetDTM(index: Integer; var dtm: pDTM): Boolean;
           procedure FreeDTM(DTM: Integer);
           Function StringToDTM(S: String): pDTM;

           function FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2,
                           y2: Integer): Boolean;
        {  function FindDTMs(DTM: Integer; var Points: TPointArray; x1, y1, x2,
                            y2: Integer): Boolean;
          function FindDTMRotated(DTM: Integer; var x, y: Integer; x1, y1, x2,
                                  y2: Integer; sAngle, eAngle, aStep: Extended;
                                  var aFound: Extended): Boolean;
          function FindDTMsRotated(DTM: Integer; var Points: TPointArray; x1,
                                  y1, x2, y2: Integer; sAngle, eAngle,
                                  aStep: Extended; var aFound: T2DExtendedArray)
                                  : Boolean;  }
           function pFindDTM(DTM: pDTM; var x, y: Integer; x1, y1, x2, y2:
           Integer): Boolean;

           constructor Create(Owner: TObject);
           destructor Destroy; override;
    private
           function AreaShape(Color, Tolerance, Size, Shape: Integer; P: TPoint) : Boolean; inline;
    private
           Client: TObject;

           // For decompressing.
           BufferString: String;

           DTMList: Array Of pDTM;
           FreeSpots: Array Of Integer;
    end;
const
    dtm_Rectangle = 0;
    dtm_Cross = 1;
    dtm_DiagonalCross = 2;
    dtm_Circle = 3;
    dtm_Triangle = 4;

{
 I am not sure wether I should simply copy and paste the old DTM implementation,
 or rewrite it from scratch.

 I recall there was something partially wrong with SCAR-alike DTM conversions
 to Mufasa DTM's...

 The old DTM system problaby doesn't perform that well, but seems to be quite
 stable and complete.

 If I would rewrite it from scratch, it would probably be faster, and
 hopefully more efficient.That won't be too hard, especially since I have
 direct data access now. (TClient FTW!)

 Rewrite from scratch it will be, I guess.
 And AreaShape will be turned into a {$I }, inline simply doesn't cut it.

 ~Wizz
}


implementation
uses
    Client, dtmutil, paszlib, finder,
    graphics, // for TColor
    math // for max
    ;

type
   TBufferByteArray = Array[0..524287] of Byte;
   PBufferByteArray = ^TBufferByteArray;

constructor TMDTM.Create(Owner: TObject);
begin
  inherited Create;
  Self.Client := Owner;

  SetLength(DTMList, 0);
  SetLength(FreeSpots, 0);
  SetLength(BufferString, 524288);
end;
{$DEFINE DTM_DEBUG}
destructor TMDTM.Destroy;

{$IFDEF DTM_DEBUG}
var
   i, j: integer;
   b:boolean;
{$ENDIF}
begin
  {$IFDEF DTM_DEBUG}
  writeln(inttostr(high(dtmlist)));
  writeln(inttostr(high(freespots)));
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
        writeln('DTM Number ' + inttostr(i) + ' was not freed');
  end;
  {$ENDIF}
  SetLength(DTMList, 0);
  SetLength(FreeSpots, 0);
  SetLength(BufferString, 0);

  inherited Destroy;
end;

type
    PMSimColor = function (Color1,Color2,Tolerance : Integer) : boolean of object;
    PMGetCol = function (x, y: integer): TColor of object;

Function TMDTM.AreaShape(Color, Tolerance, Size, Shape: Integer; P: TPoint) : Boolean; inline;

Var
   X, Y, S: Integer;
   SimCol: PMSimColor;
   GetCol: PMGetCol;

Begin
  writeln('areashape');
  SimCol := @TClient(Client).MFinder.SimilarColors;
  GetCol := @TClient(Client).MWindow.GetColor;
  Case Shape Of
    dtm_Rectangle:
    Begin
      {
        Example:
        3x3
        X X X
        X X X
        X X X
      }
      For X := P.X - Size To P.X + Size Do
        For Y := P.Y - Size To P.Y + Size Do
          If SimCol(GetCol(X, Y), Color, Tolerance) Then
          Begin
            Result := True;
	          Exit;
          End;
    End;

    dtm_Cross:
      {
        Example:
        3x3
          X
        X X X
          X
      }
    Begin
      For X := P.X - Size To P.X + Size Do
        If SimCol(GetCol(X, P.Y), Color, Tolerance) Then
        Begin
          Result := True;
          Exit;
        End;
      For Y := P.Y - Size To P.Y + Size Do
        If SimCol(GetCol(P.X, Y), Color, Tolerance) Then
        Begin
          Result := True;
          Exit;
        End;
    End;

    dtm_DiagonalCross:
      {
        Example:
        3x3
        X   X
          X
        X   X

      }
      Begin
      For S := -Size To Size Do
      Begin
        If SimCol(GetCol(P.X + S, P.Y + S), Color, Tolerance) Then
        Begin
          Result := True;
          Exit;
        End;
        If SimCol(GetCol(P.X + S, P.Y - S), Color, Tolerance) Then
        Begin
          Result := True;
          Exit;
        End;
      End;
    End;

    {4:
    Begin
      D := Ceil(Sqrt(Pow(Size, 2) + Pow(Size, 2)));
      //Will finish later

    End;   }

    Else
      WriteLn('Incorrect Shape');
  End;
  Result := False;
End;


function HexToInt(HexNum: string): LongInt;inline;
begin
   Result:=StrToInt('$' + HexNum);
end;

function TMDTM.StringToDTM(S: String): pDTM;
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
  DestLen := Length(Self.BufferString);
  if uncompress(PChar(Self.Bufferstring),Destlen,pchar(Source), ii) = Z_OK then
  begin;
    if (Destlen mod 36) > 0 then
    begin;
      Writeln('Invalid DTM');
      Exit;
    end;
    DestLen := DestLen div 36;
    SetLength(Result.p,DestLen);
    SetLength(Result.c,DestLen);
    SetLength(Result.t,DestLen);
    SetLength(Result.asz,DestLen);
    SetLength(Result.ash,DestLen);
    b := @Self.Bufferstring[1];
    for i := 0 to DestLen - 1 do
    begin;
      c := i * 36;
      Result.p[i].x := PInteger(@b^[c+1])^;
      Result.p[i].y := PInteger(@b^[c+5])^;
      Result.asz[i] := PInteger(@b^[c+12])^;
      Result.ash[i] := PInteger(@b^[c+16])^;
      Result.c[i] := PInteger(@b^[c+20])^;
      Result.t[i] := PInteger(@b^[c+24])^;
    end;
  end;
end;

function TMDTM.AddDTM(d: TDTM): Integer;

begin
  if Length(FreeSpots) > 0 then
  begin
    DTMList[FreeSpots[High(FreeSpots)]] := TDTMTopDTM(d);
    Result := FreeSpots[High(FreeSpots)];
    SetLength(FreeSpots, High(FreeSpots));
  end
  else
  begin
    SetLength(DTMList, Length(DTMList) + 1);
    DTMList[High(DTMList)] := TDTMTopDTM(d);
    Result := High(DTMList);
  end;
end;

{/\
  Adds the given pDTM to the DTM Array, and returns it's index.
/\}

function TMDTM.AddpDTM(d: pDTM): Integer;

begin
  if Length(FreeSpots) > 0 then
  begin
    DTMList[FreeSpots[High(FreeSpots)]] := d;
    Result := FreeSpots[High(FreeSpots)];
    SetLength(FreeSpots, High(FreeSpots));
  end
  Else
  begin
    SetLength(DTMList, Length(DTMList) + 1);
    DTMList[High(DTMList)] := d;
    Result := High(DTMList);
  end;
end;

{/\
   Returns the DTM (pDTM type) in the variable dtm at the given index.
   Returns true is succesfull, false if the dtm does not exist.
/\}

function TMDTM.GetDTM(index: Integer; var dtm: pDTM): Boolean;
begin
  Result := True;
  try
    dtm := DTMList[index];
  except
    begin
      raise Exception.CreateFmt('The given DTM Index ([%d]) is invalid.',
      [index]);
      //WriteLn('DTM Index ' + IntToStr(index) + ' does not exist');
      Result := False;
    end;
  end
end;

{/\
  Unloads the DTM at the given index from the DTM Array.
  Notes:
  Will keep track of not used index, so it is very memory efficient.
/\}

Procedure TMDTM.FreeDTM(DTM: Integer);
begin
  try
    SetLength(DTMList[DTM].p, 0);
    SetLength(DTMList[DTM].c, 0);
    SetLength(DTMList[DTM].t, 0);
    SetLength(DTMList[DTM].asz, 0);
    SetLength(DTMList[DTM].ash, 0);
  except
    //WriteLn('Invalid DTM');
  end;
  SetLength(FreeSpots, Length(FreeSpots) + 1);
  FreeSpots[High(FreeSpots)] := DTM;
end;

{
  Tries to find the given DTM (index). If found will put the point the dtm has
  been found at in x, y and result to true.
}
function TMDTM.FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
var
   temp: pDTM;
begin
  if GetDTM(DTM, temp) then
    Result := pFindDTM(temp, x, y, x1, y1, x2, y2)
  else
  begin
    x := 0;
    y := 0;
    Result := False;
  end;
end;

{
  Tries to find the given pDTM. If found will put the point the dtm has
  been found at in x, y and result to true.
}

function TMDTM.pFindDTM(DTM: pDTM; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;

var
   mP: TPointArray;
   I, J, H, dH: Integer;
   Found: Boolean;
   TempTP: TPoint;
   RetData: TRetData;
   MaxSubPointDist: TPoint;

begin
  MaxSubPointDist := Point(0,0);

  for I := 1 to High(DTM.p) do
  begin
    DTM.p[I].x := DTM.p[I].x - DTM.p[0].x;
    DTM.p[I].y := DTM.p[I].y - DTM.p[0].y;
    MaxSubPointDist.X := Max(DTM.p[I].x, MaxSubPointDist.X);
    MaxSubPointDist.Y := Max(DTM.p[I].y, MaxSubPointDist.Y);
  end;

   X2 := X2 - MaxSubPointDist.X;
   Y2 := Y2 - MaxSubPointDist.Y;
   X1 := X1 + MaxSubPointDist.X;
   Y1 := Y1 + MaxSubPointDist.Y;
   {If X2 > X1 then
     //Exit;
   If Y2 > Y1 then  }
     //Exit;
  // Will make sure there are no out of bounds exceptions, and will make it faster
  TClient(Client).MWindow.Freeze();

  TClient(Client).MFinder.FindColorsTolerance(mP, DTM.c[Low(DTM.c)],
          x1, y1, x2, y2, DTM.t[Low(DTM.t)]);

  TClient(Client).MWindow.GetDimensions(H, dH);
  RetData := TClient(Client).MWindow.ReturnData(0, 0, H, dH);

  H := High(mP);
  dH := High(DTM.p);
  for I := 0 to H do
  begin
    // Use MainPoint's AreaSize and Shape.
    // for Loop on mP, depending on the AreaShape. then on all the code beneath
    // this point, use the var that is retrieved from the for loop.
    Found := True;
    for J := 1 to dH do
    begin
      TempTP.X := DTM.p[J].X + mP[I].X;
      TempTP.Y := DTM.p[J].Y + mP[I].Y;
      //Now would be the time to Rotate TempTP
      if not AreaShape(DTM.c[J], DTM.t[J], DTM.asz[J], DTM.ash[J], TempTP) then
      begin
        Found := False;
        Break;
      end;
    end;

    if Found then
    begin
      Result := True;
      x := mP[I].X;
      y := mP[I].Y;
      TClient(Client).MWindow.UnFreeze();
      Exit;
    end;
  end;
  TClient(Client).MWindow.UnFreeze();
  Result := False;
end;

end.

