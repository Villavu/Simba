procedure ps_Quicksort(var Arr : TIntegerArray); extdecl;
begin
  QuickSort(Arr,low(arr),high(arr));
end;

procedure ps_tSwap(var a, b: TPoint); extdecl;
begin
  tswap(a,b);
end;

procedure ps_tpaSwap(var a, b: TPointArray);extdecl;
begin
  tpaSwap(a,b);
end;

procedure ps_SwapE(var a, b: Extended); extdecl;
begin
  SwapE(a,b);
end;

procedure ps_RAaSTPAEx(var a: TPointArray; const w, h: Integer); extdecl;
begin
  RAaSTPAEx(a,w,h);
end;

procedure ps_RAaSTPA(var a: TPointArray; const Dist: Integer); extdecl;
begin
 RAaSTPA(a,dist);
end;

function ps_NearbyPointInArrayEx(const P: TPoint; w, h:Integer;const a: TPointArray): Boolean;extdecl;
begin
  result := NearbyPointInArrayEx(p,w,h,a);
end;

function ps_NearbyPointInArray(const P: TPoint; Dist:Integer;const a: TPointArray): Boolean; extdecl;
begin
  result := NearbyPointInArray(p,dist,a);
end;

procedure ps_QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);extdecl;
begin
  QuickTPASort(a,b,ilo,ihi,SortUp);
end;

procedure ps_QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);extdecl;
begin
  QuickATPASort(a,b,ilo,ihi,sortup);
end;

procedure ps_SortTPAByX(var a: TPointArray; const LowToHi: Boolean);
begin
  SortTPAByX(a, LowToHi);
end;

procedure ps_SortTPAByY(var a: TPointArray; const LowToHi: Boolean);
begin
  SortTPAByY(a, LowToHi);
end;

function ps_FindTPARows(a: TPointArray): T2DPointArray;
begin
  Result := FindTPARows(a);
end;

function ps_FindTPAColumns(a: TPointArray): T2DPointArray;
begin
  Result := FindTPAColumns(a);
end;


procedure ps_SortTPAFrom(var a: TPointArray; const From: TPoint); extdecl;
begin
  SortTPAFrom(a,from);
end;

procedure ps_SortATPAFrom(var a: T2DPointArray; const From: TPoint); extdecl;
begin
  SortATPAFrom(a,from);
end;

procedure ps_SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint); extdecl;
begin
  SortATPAFromFirstPoint(a,from);
end;

procedure ps_SortATPAFromMidPoint(var a: T2DPointArray; const From: TPoint); extdecl;
begin
  SortATPAFromMidPoint(a,from);
end;

procedure ps_InvertTPA(var a: TPointArray); extdecl;
begin
  InvertTPA(a);
end;

procedure ps_InvertATPA(var a: T2DPointArray); extdecl;
begin
  InvertATPA(a);
end;

function ps_MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean; extdecl;
begin
  Result := MiddleTPAEx(tpa,x,y);
end;

function ps_MiddleTPA(const tpa: TPointArray): TPoint;  extdecl;
begin
  result := MiddleTPA(tpa);
end;

procedure ps_SortATPASize(var a: T2DPointArray; const BigFirst: Boolean); extdecl;
begin
  SortATPASize(a,bigfirst);
end;

procedure ps_SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);extdecl;
begin
  SortATPAFromSize(a,size,closefirst);
end;

function ps_InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;extdecl;
begin
  result := InIntArrayEx(a,where,number);
end;

function ps_InIntArray(const a: TIntegerArray; Number: Integer): Boolean; extdecl;
begin
  result := InIntArray(a,number);
end;

procedure ps_ClearSameIntegers(var a: TIntegerArray);extdecl;
begin
  ClearSameIntegers(a);
end;

procedure ps_ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);extdecl;
begin
  ClearSameIntegersAndTPA(a,p);
end;

function ps_SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;  extdecl;
begin
  result := SplitTPAEx(arr,w,h);
end;

function ps_SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;  extdecl;
begin
  result := SplitTPA(arr,dist);
end;

function ps_FloodFillTPA(const TPA : TPointArray) : T2DPointArray; extdecl;
begin
  result := FloodFillTPA(TPA);
end;

procedure ps_FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);extdecl;
begin
  FilterPointsPie(points,sd,ed,minr,maxr,mx,my);
end;

procedure ps_FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Integer); extdecl;
begin
  FilterPointsDist(points,mindist,maxdist,mx,my);
end;

procedure ps_FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);extdecl;
begin
  FilterPointsLine(points,radial,radius,mx,my);
end;

procedure ps_FilterTPADist(var TPA: TPointArray; maxDist: integer);
begin
  FilterTPADist(TPA, maxDist);
end;

function ps_GetATPABounds(const ATPA: T2DPointArray): TBox;extdecl;
begin
  result := GetATPABounds(ATPA);
end;

function ps_GetTPABounds(const TPA: TPointArray): TBox; extdecl;
begin
  result := GetTPABounds(TPA);
end;

function ps_FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean; extdecl;
begin
  Result := FindTPAinTPA(searchTPA,totaltpa,matches);
end;

function ps_GetSamePointsATPA(const ATPA : T2DPointArray; var Matches : TPointArray) : boolean;extdecl;
begin
  result := GetSamePointsATPA(ATPA,Matches);
end;

function ps_FindTextTPAinTPA(Height : integer;const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;extdecl;
begin
  result := FindTextTPAinTPA(height,searchtpa,totaltpa,matches);
end;

procedure ps_SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);extdecl;
begin
  SortCircleWise(tpa,cx,cy,startdegree,sortup,clockwise);
end;

procedure ps_LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);  extdecl;
begin
  LinearSort(tpa,cx,cy,sd,sortup);
end;

function ps_RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;  extdecl;
begin
  result := RotatePoint(p,angle,mx,my);
end;

function ps_ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;extdecl;
begin
  result := ChangeDistPT(pt,mx,my,newdist);
end;

function ps_ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean; extdecl;
begin
  result := ChangeDistTPA(tpa,mx,my,newdist);
end;

function ps_FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray; extdecl;
begin
  result := FindGapsTPA(TPA,minpixels);
end;

function ps_RemoveDistTPointArray(x, y, dist: Integer;const ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray; extdecl;
begin
  Result := RemoveDistTPointArray(x,y,dist,thepoints,removehigher);
end;

function ps_CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;   extdecl;
begin
  result := CombineTPA(ar1,ar2);
end;

function ps_ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;extdecl;
begin
  result := ReArrangeandShortenArrayEx(a,w,h);
end;

function ps_ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray; extdecl;
begin
  result := ReArrangeandShortenArray(a,dist);
end;

function ps_TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;  extdecl;
begin
  result := TPAtoATPAEx(tpa,w,h);
end;

function ps_TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;extdecl;
begin
  Result := TPAtoATPA(tpa,dist);
end;

function ps_CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray; extdecl;
begin
  result := CombineIntArray(ar1,ar2);
end;

function ps_MergeATPA(const ATPA : T2DPointArray)  : TPointArray;  extdecl;
begin
  result := MergeATPA(ATPA);
end;

procedure ps_AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);
begin
  AppendTPA(tpa,ToAppend);
end;

function ps_TPAFromLine(const x1, y1, x2, y2: Integer): TPointArray;  extdecl;
begin
  Result := TPAFromLine(x1, y1, x2, y2);
end;

function ps_EdgeFromBox(const Box: TBox): TPointArray;  extdecl;
begin
  Result := EdgeFromBox(Box);
end;

function ps_TPAFromBox(const Box : TBox) : TPointArray;  extdecl;
begin
  result := TPAFromBox(box);
end;

function ps_TPAFromEllipse(const CX, CY, XRadius, YRadius : Integer) : TPointArray;  extdecl;
begin
  result := TPAFromEllipse(CX, CY, XRadius, YRadius);
end;

function ps_TPAFromCircle(const CX, CY, Radius: Integer) : TPointArray;  extdecl;
begin
  result := TPAFromCircle(CX, CY, Radius);
end;

procedure ps_FillEllipse(var a: TPointArray);
begin
  FillEllipse(a);
end;

function ps_RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray; extdecl;
begin
  Result := RotatePoints(p,a,cx,cy);
end;

function ps_FindTPAEdges(const p: TPointArray): TPointArray;extdecl;
begin
  result := FindTPAEdges(p);
end;

function ps_ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray; extdecl;
begin
  result := ClearTPAFromTPA(arP,ClearPoints);
end;

function ps_ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray; extdecl;
begin
  result := ReturnPointsNotInTPA(totaltpa,box);
end;

function ps_PointInTPA(p: TPoint;const arP: TPointArray): Boolean; extdecl;
begin
  result := PointInTPA(p,arp);
end;

procedure ps_ClearDoubleTPA(var TPA: TPointArray); extdecl;
begin
  ClearDoubleTPA(tpa);
end;

procedure ps_TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean); extdecl;
begin;
  TPACountSort(TPA,max,sortonx);
end;

procedure ps_TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);extdecl;
begin
  TPACountSortBase(TPA,maxx,base,sortonx);
end;

procedure ps_InvertTIA(var tI: TIntegerArray); extdecl;
begin
  InvertTIA(ti);
end;

function ps_SumIntegerArray(const Ints : TIntegerArray): Integer;  extdecl;
begin
  result := SumIntegerArray(ints);
end;

function ps_AverageTIA(const tI: TIntegerArray): Integer; extdecl;
begin
  result := AverageTIA(ti);
end;

function ps_AverageExtended(const tE: TExtendedArray): Extended; extdecl;
begin
  result := AverageExtended(te);
end;

procedure ps_SplitTPAExWrap(const arr: TPointArray; w, h: Integer; var res : T2DPointArray);extdecl;
begin
  res := SplitTPAEx(arr,w,h);
end;

procedure ps_SplitTPAWrap(const arr: TPointArray; Dist: Integer; var res: T2DPointArray);extdecl;
begin
  res := SplitTPA(arr,dist);
end;

procedure ps_FindGapsTPAWrap(const TPA: TPointArray; MinPixels: Integer; var Res : T2DPointArray); extdecl;
begin
  Res := FindGapsTPA(TPA,MinPixels);
end;
procedure ps_RemoveDistTPointArrayWrap(x, y, dist: Integer;const ThePoints: TPointArray; RemoveHigher: Boolean; var Res :  TPointArray);extdecl;
begin
  Res :=  RemoveDistTPointArray(x,y,dist,thepoints,removehigher);
end;
procedure ps_CombineTPAWrap(const Ar1, Ar2: TPointArray; var Res :  TPointArray);extdecl;
begin
  Res := CombineTPA(Ar1,Ar2);
end;
procedure ps_ReArrangeandShortenArrayExWrap(const a: TPointArray; w, h: Integer; var Res :  TPointArray);extdecl;
begin
  Res := ReArrangeandShortenArrayEx(a,w,h);
end;
procedure ps_ReArrangeandShortenArrayWrap(const a: TPointArray; Dist: Integer; var Res :  TPointArray);extdecl;
begin
  Res := ReArrangeandShortenArray(a,dist);
end;
procedure ps_TPAtoATPAExWrap(const TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray);extdecl;
begin
  Res := TPAtoATPAEx(TPA,w,h);
end;
procedure ps_TPAtoATPAWrap(const TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray);extdecl;
begin
  Res := TPAtoATPA(TPA,Dist);
end;
procedure ps_MiddleTPAWrap(const TPA: TPointArray; var Res: TPoint);extdecl;
begin
  Res := MiddleTPA(TPA);
end;
procedure ps_CombineIntArrayWrap(const Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray);extdecl;
begin
  Res := CombineIntArray(Ar1,Ar2);
end;
procedure ps_MergeATPAWrap(const ATPA : T2DPointArray; var Res: TPointArray); extdecl;
begin
  Res := MergeATPA(ATPA);
end;
procedure ps_TPAFromLineWrap(const x1, y1, x2, y2: Integer; var Res: TPointArray); extdecl;
begin
  Res := TPAFromLine(x1, y1, x2, y2);
end;
procedure ps_EdgeFromBoxWrap(const Box: TBox; var Res: TPointArray); extdecl;
begin
  Res := EdgeFromBox(Box);
end;
procedure ps_TPAFromBoxWrap(const Box : TBox; var Res : TPointArray);extdecl;
begin
  Res := TPAFromBox(Box);
end;
procedure ps_TPAFromEllipseWrap(const CX, CY, XRadius, YRadius : Integer; var Res : TPointArray);extdecl;
begin
  Res := TPAFromEllipse(CX, CY, XRadius, YRadius);
end;
procedure ps_TPAFromCircleWrap(const CX, CY, Radius: Integer; var Res : TPointArray);extdecl;
begin
  Res := TPAFromCircle(CX, CY, Radius);
end;
procedure ps_RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray);extdecl;
begin
  Res := RotatePoints(P,a,cx,cy);
end;
procedure ps_FindTPAEdgesWrap(const p: TPointArray; var Res :  TPointArray);extdecl;
begin
  Res := FindTPAEdges(p);
end;
procedure ps_ClearTPAFromTPAWrap(const arP, ClearPoints: TPointArray; var Res :  TPointArray);extdecl;
begin
  Res := ClearTPAFromTPA(arP, clearpoints);
end;
procedure ps_ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray);extdecl;
begin
  Res := ReturnPointsNotInTPA(TotalTPA,box);
end;

function ps_SameTPA(const aTPA, bTPA: TPointArray): Boolean;extdecl;
begin
  result := SameTPA(atpa,btpa);
end;

function ps_TPAInATPA(const TPA: TPointArray;const InATPA: T2DPointArray; var Index: LongInt): Boolean;extdecl;
begin
  result := TPAInATPA(tpa,inatpa,index);
end;
procedure ps_OffsetTPA(var TPA : TPointArray; const Offset : TPoint); extdecl;
begin
  OffsetTPA(TPA,offset);
end;

procedure ps_OffsetATPA(var ATPA : T2DPointArray; const Offset : TPoint);extdecl;
begin
  OffsetATPA(atpa,offset);
end;
function ps_CopyTPA(const TPA : TPointArray) : TPointArray;extdecl;
begin
  result := Copy(TPA,0,Length(TPA));
end;

function ps_CopyATPA(const ATPA : T2DPointArray) : T2DPointArray; extdecl;
var
  i,l : integer;
begin
  l := high(ATPA);
  SetLength(result,l+1);
  for i := 0 to l do
    result[i] := copy(ATPA[i],0,Length(ATPA[i]));
end;

function ps_MiddleBox(b : TBox) : TPoint;
begin
  result := MiddleBox(b);
end;

function  ps_TPAPosNext(const Find: TPoint; const V: TPointArray; const PrevPos: Integer;
          const IsSortedAscending: Boolean): Integer; extdecl;
begin
  result:=TPAPosNext(Find,V,PrevPos,IsSortedAscending);
end;

function ps_GlueTPAs(const V1, V2: TPointArray; const IsSortedAscending,byDifference: Boolean):TPointArray; extdecl;
begin
  result:=GlueTPAs(V1,V2,IsSortedAscending,byDifference);
end;

