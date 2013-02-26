procedure Lape_Quicksort(const Params: PParamArray); lape_extdecl
begin
  ps_Quicksort(PIntegerArray(Params^[0])^);
end;

procedure Lape_tSwap(const Params: PParamArray); lape_extdecl
begin
  ps_tSwap(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_tpaSwap(const Params: PParamArray); lape_extdecl
begin
  ps_tpaSwap(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_SwapE(const Params: PParamArray); lape_extdecl
begin
  ps_SwapE(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_RAaSTPAEx(const Params: PParamArray); lape_extdecl
begin
  ps_RAaSTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_RAaSTPA(const Params: PParamArray); lape_extdecl
begin
  ps_RAaSTPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_NearbyPointInArrayEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_NearbyPointInArrayEx(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_NearbyPointInArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_NearbyPointInArray(PPoint(Params^[0])^, PInteger(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_QuickTPASort(const Params: PParamArray); lape_extdecl
begin
  ps_QuickTPASort(PIntegerArray(Params^[0])^, PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_QuickATPASort(const Params: PParamArray); lape_extdecl
begin
  ps_QuickATPASort(PIntegerArray(Params^[0])^, P2DPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_SortTPAByX(const Params: PParamArray); lape_extdecl
begin
  ps_SortTPAByX(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_SortTPAByY(const Params: PParamArray); lape_extdecl
begin
  ps_SortTPAByY(PPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_FindTPARows(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_FindTPARows(PPointArray(Params^[0])^);
end;

procedure Lape_FindTPAColumns(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_FindTPAColumns(PPointArray(Params^[0])^);
end;

procedure Lape_SortTPAFrom(const Params: PParamArray); lape_extdecl
begin
  ps_SortTPAFrom(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFrom(const Params: PParamArray); lape_extdecl
begin
  ps_SortATPAFrom(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFromFirstPoint(const Params: PParamArray); lape_extdecl
begin
  ps_SortATPAFromFirstPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_SortATPAFromMidPoint(const Params: PParamArray); lape_extdecl
begin
  ps_SortATPAFromMidPoint(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_InvertTPA(const Params: PParamArray); lape_extdecl
begin
  ps_InvertTPA(PPointArray(Params^[0])^);
end;

procedure Lape_InvertATPA(const Params: PParamArray); lape_extdecl
begin
  ps_InvertATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_MiddleTPAEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_MiddleTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_MiddleTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := ps_MiddleTPA(PPointArray(Params^[0])^);
end;

procedure Lape_SortATPASize(const Params: PParamArray); lape_extdecl
begin
  ps_SortATPASize(P2DPointArray(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_SortATPAFromSize(const Params: PParamArray); lape_extdecl
begin
  ps_SortATPAFromSize(P2DPointArray(Params^[0])^, PInteger(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_InIntArrayEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_InIntArrayEx(PIntegerArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_InIntArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_InIntArray(PIntegerArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_ClearSameIntegers(const Params: PParamArray); lape_extdecl
begin
  ps_ClearSameIntegers(PIntegerArray(Params^[0])^);
end;

procedure Lape_ClearSameIntegersAndTPA(const Params: PParamArray); lape_extdecl
begin
  ps_ClearSameIntegersAndTPA(PIntegerArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_SplitTPAEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_SplitTPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_SplitTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_SplitTPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_FloodFillTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_FloodFillTPA(PPointArray(Params^[0])^);
end;

procedure Lape_FilterPointsPie(const Params: PParamArray); lape_extdecl
begin
  ps_FilterPointsPie(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure Lape_FilterPointsDist(const Params: PParamArray); lape_extdecl
begin
  ps_FilterPointsDist(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure Lape_FilterPointsLine(const Params: PParamArray); lape_extdecl
begin
  ps_FilterPointsLine(PPointArray(Params^[0])^, PExtended(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure Lape_FilterTPADist(const Params: PParamArray); lape_extdecl
begin
  ps_FilterTPADist(PPointArray(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_GetATPABounds(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBox(Result)^ := ps_GetATPABounds(P2DPointArray(Params^[0])^);
end;

procedure Lape_GetTPABounds(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBox(Result)^ := ps_GetTPABounds(PPointArray(Params^[0])^);
end;

procedure Lape_FindTPAinTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindTPAinTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_GetSamePointsATPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_GetSamePointsATPA(P2DPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_FindTextTPAinTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindTextTPAinTPA(Pinteger(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_SortCircleWise(const Params: PParamArray); lape_extdecl
begin
  ps_SortCircleWise(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBoolean(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure Lape_LinearSort(const Params: PParamArray); lape_extdecl
begin
  ps_LinearSort(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_RotatePoint(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := ps_RotatePoint(PPoint(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_ChangeDistPT(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := ps_ChangeDistPT(PPoint(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pextended(Params^[3])^);
end;

procedure Lape_ChangeDistTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_ChangeDistTPA(PPointArray(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pextended(Params^[3])^);
end;

procedure Lape_FindGapsTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_FindGapsTPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_RemoveDistTPointArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_RemoveDistTPointArray(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure Lape_CombineTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_CombineTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ReArrangeandShortenArrayEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_ReArrangeandShortenArrayEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_ReArrangeandShortenArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_ReArrangeandShortenArray(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_TPAtoATPAEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_TPAtoATPAEx(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_TPAtoATPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_TPAtoATPA(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_CombineIntArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PIntegerArray(Result)^ := ps_CombineIntArray(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure Lape_MergeATPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_MergeATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_AppendTPA(const Params: PParamArray); lape_extdecl
begin
  ps_AppendTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAFromLine(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_TPAFromLine(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_EdgeFromBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_EdgeFromBox(PBox(Params^[0])^);
end;

procedure Lape_TPAFromBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_TPAFromBox(PBox(Params^[0])^);
end;

procedure Lape_TPAFromEllipse(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_TPAFromEllipse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_TPAFromCircle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_TPAFromCircle(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_FillEllipse(const Params: PParamArray); lape_extdecl
begin
  ps_FillEllipse(PPointArray(Params^[0])^);
end;

procedure Lape_RotatePoints(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_RotatePoints(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_FindTPAEdges(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_FindTPAEdges(PPointArray(Params^[0])^);
end;

procedure Lape_ClearTPAFromTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_ClearTPAFromTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ReturnPointsNotInTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_ReturnPointsNotInTPA(PPointArray(Params^[0])^, PBox(Params^[1])^);
end;

procedure Lape_PointInTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_PointInTPA(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ClearDoubleTPA(const Params: PParamArray); lape_extdecl
begin
  ps_ClearDoubleTPA(PPointArray(Params^[0])^);
end;

procedure Lape_TPACountSort(const Params: PParamArray); lape_extdecl
begin
  ps_TPACountSort(PPointArray(Params^[0])^, PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_TPACountSortBase(const Params: PParamArray); lape_extdecl
begin
  ps_TPACountSortBase(PPointArray(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_InvertTIA(const Params: PParamArray); lape_extdecl
begin
  ps_InvertTIA(PIntegerArray(Params^[0])^);
end;

procedure Lape_SumIntegerArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_SumIntegerArray(PIntegerArray(Params^[0])^);
end;

procedure Lape_AverageTIA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_AverageTIA(PIntegerArray(Params^[0])^);
end;

procedure Lape_AverageExtended(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := ps_AverageExtended(PExtendedArray(Params^[0])^);
end;

procedure Lape_SplitTPAExWrap(const Params: PParamArray); lape_extdecl
begin
  ps_SplitTPAExWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, P2DPointArray(Params^[3])^);
end;

procedure Lape_SplitTPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_SplitTPAWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, P2DPointArray(Params^[2])^);
end;

procedure Lape_FindGapsTPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_FindGapsTPAWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, P2DPointArray(Params^[2])^);
end;

procedure Lape_RemoveDistTPointArrayWrap(const Params: PParamArray); lape_extdecl
begin
  ps_RemoveDistTPointArrayWrap(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^, PBoolean(Params^[4])^, PPointArray(Params^[5])^);
end;

procedure Lape_CombineTPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_CombineTPAWrap(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_ReArrangeandShortenArrayExWrap(const Params: PParamArray); lape_extdecl
begin
  ps_ReArrangeandShortenArrayExWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_ReArrangeandShortenArrayWrap(const Params: PParamArray); lape_extdecl
begin
  ps_ReArrangeandShortenArrayWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_TPAtoATPAExWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAtoATPAExWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, P2DPointArray(Params^[3])^);
end;

procedure Lape_TPAtoATPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAtoATPAWrap(PPointArray(Params^[0])^, PInteger(Params^[1])^, P2DPointArray(Params^[2])^);
end;

procedure Lape_CombineIntArrayWrap(const Params: PParamArray); lape_extdecl
begin
  ps_CombineIntArrayWrap(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure Lape_MergeATPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_MergeATPAWrap(P2DPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAFromLineWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAFromLineWrap(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PPointArray(Params^[4])^);
end;

procedure Lape_EdgeFromBoxWrap(const Params: PParamArray); lape_extdecl
begin
  ps_EdgeFromBoxWrap(PBox(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAFromBoxWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAFromBoxWrap(PBox(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAFromEllipseWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAFromEllipseWrap(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PPointArray(Params^[4])^);
end;

procedure Lape_TPAFromCircleWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAFromCircleWrap(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^);
end;

procedure Lape_RotatePointsWrap(const Params: PParamArray); lape_extdecl
begin
  ps_RotatePointsWrap(PPointArray(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PPointArray(Params^[4])^);
end;

procedure Lape_FindTPAEdgesWrap(const Params: PParamArray); lape_extdecl
begin
  ps_FindTPAEdgesWrap(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_ClearTPAFromTPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_ClearTPAFromTPAWrap(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_ReturnPointsNotInTPAWrap(const Params: PParamArray); lape_extdecl
begin
  ps_ReturnPointsNotInTPAWrap(PPointArray(Params^[0])^, PBox(Params^[1])^, PPointArray(Params^[2])^);
end;

procedure Lape_SameTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_SameTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_TPAInATPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_TPAInATPA(PPointArray(Params^[0])^, P2DPointArray(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure Lape_OffsetTPA(const Params: PParamArray); lape_extdecl
begin
  ps_OffsetTPA(PPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_OffsetATPA(const Params: PParamArray); lape_extdecl
begin
  ps_OffsetATPA(P2DPointArray(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_CopyTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_CopyTPA(PPointArray(Params^[0])^);
end;

procedure Lape_CopyATPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DPointArray(Result)^ := ps_CopyATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_MiddleBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := ps_MiddleBox(PBox(Params^[0])^);
end;

procedure Lape_TPAPosNext(const Params: PParamArray;const Result: pointer); lape_extdecl
begin
 PInteger(Result)^:= ps_TPAPosNext(PPoint(Params^[0])^, PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure Lape_GlueTPAs(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
 PPointArray(Result)^:= ps_GlueTPAs(PPointArray(Params^[0])^, PPointArray(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;
