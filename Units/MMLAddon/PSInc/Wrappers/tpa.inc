procedure SplitTPAExWrap(arr: TPointArray; w, h: Integer; out res : T2DPointArray);
begin
  res := SplitTPAEx(arr,w,h);
end;

procedure SplitTPAWrap(arr: TPointArray; Dist: Integer; out res: T2DPointArray);
begin
  res := SplitTPA(arr,dist);
end;

procedure FindGapsTPAWrap(TPA: TPointArray; MinPixels: Integer; out Res : T2DPointArray); extdecl;
begin
  Res := FindGapsTPA(TPA,MinPixels);
end;
procedure RemoveDistTPointArrayWrap(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean; out Res :  TPointArray);extdecl;
begin
  Res :=  RemoveDistTPointArray(x,y,dist,thepoints,removehigher);
end;
procedure CombineTPAWrap(Ar1, Ar2: TPointArray; out Res :  TPointArray);extdecl;
begin
  Res := CombineTPA(Ar1,Ar2);
end;
procedure ReArrangeandShortenArrayExWrap(a: TPointArray; w, h: Integer; out Res :  TPointArray);extdecl;
begin
  Res := ReArrangeandShortenArrayEx(a,w,h);
end;
procedure ReArrangeandShortenArrayWrap(a: TPointArray; Dist: Integer; out Res :  TPointArray);extdecl;
begin
  Res := ReArrangeandShortenArray(a,dist);
end;
procedure TPAtoATPAExWrap(TPA: TPointArray; w, h: Integer; out Res :  T2DPointArray);extdecl;
begin
  Res := TPAtoATPAEx(TPA,w,h);
end;
procedure TPAtoATPAWrap(TPA: TPointArray; Dist: Integer; out Res :  T2DPointArray);extdecl;
begin
  Res := TPAtoATPA(TPA,Dist);
end;
procedure CombineIntArrayWrap(Ar1, Ar2: TIntegerArray; out Res :  TIntegerArray);extdecl;
begin
  Res := CombineIntArray(Ar1,Ar2);
end;
procedure MergeATPAWrap(ATPA : T2DPointArray; out Res: TPointArray); extdecl;
begin
  Res := MergeATPA(ATPA);
end;
procedure TPAFromBoxWrap(const Box : TBox; out Res : TPointArray);extdecl;
begin
  Res := TPAFromBox(Box);
end;
procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; out Res :  TPointArray);extdecl;
begin
  Res := RotatePoints(P,a,cx,cy);
end;
procedure FindTPAEdgesWrap(p: TPointArray; out Res :  TPointArray);extdecl;
begin
  Res := FindTPAEdges(p);
end;
procedure ClearTPAFromTPAWrap(arP, ClearPoints: TPointArray; out Res :  TPointArray);extdecl;
begin
  Res := ClearTPAFromTPA(arP, clearpoints);
end;
procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; out Res :  TPointArray);extdecl;
begin
  Res := ReturnPointsNotInTPA(TotalTPA,box);
end;
