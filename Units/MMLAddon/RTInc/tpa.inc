procedure RUTIS_ps_Quicksort(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_Quicksort(PIntegerArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_tSwap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_tSwap(PPoint(Params^[0].Data)^, PPoint(Params^[1].Data)^);
end;

procedure RUTIS_ps_tpaSwap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_tpaSwap(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_SwapE(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SwapE(PExtended(Params^[0].Data)^, PExtended(Params^[1].Data)^);
end;

procedure RUTIS_ps_RAaSTPAEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_RAaSTPAEx(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_RAaSTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_RAaSTPA(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_NearbyPointInArrayEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_NearbyPointInArrayEx(PPoint(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PPointArray(Params^[3].Data)^);
end;

procedure RUTIS_ps_NearbyPointInArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_NearbyPointInArray(PPoint(Params^[0].Data)^, PInteger(Params^[1].Data)^, PPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_QuickTPASort(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_QuickTPASort(PIntegerArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PBoolean(Params^[4].Data)^);
end;

procedure RUTIS_ps_QuickATPASort(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_QuickATPASort(PIntegerArray(Params^[0].Data)^, P2DPointArray(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PBoolean(Params^[4].Data)^);
end;

procedure RUTIS_ps_SortTPAFrom(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SortTPAFrom(PPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^);
end;

procedure RUTIS_ps_SortATPAFrom(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SortATPAFrom(P2DPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^);
end;

procedure RUTIS_ps_SortATPAFromFirstPoint(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SortATPAFromFirstPoint(P2DPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^);
end;

procedure RUTIS_ps_InvertTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_InvertTPA(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_InvertATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_InvertATPA(P2DPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_MiddleTPAEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_MiddleTPAEx(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_MiddleTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPoint(Result^.Data)^ := ps_MiddleTPA(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_SortATPASize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SortATPASize(P2DPointArray(Params^[0].Data)^, PBoolean(Params^[1].Data)^);
end;

procedure RUTIS_ps_SortATPAFromSize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SortATPAFromSize(P2DPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PBoolean(Params^[2].Data)^);
end;

procedure RUTIS_ps_InIntArrayEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_InIntArrayEx(PIntegerArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_InIntArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_InIntArray(PIntegerArray(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_ClearSameIntegers(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ClearSameIntegers(PIntegerArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_ClearSameIntegersAndTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ClearSameIntegersAndTPA(PIntegerArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_SplitTPAEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_SplitTPAEx(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_SplitTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_SplitTPA(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_FloodFillTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_FloodFillTPA(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_FilterPointsPie(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FilterPointsPie(PPointArray(Params^[0].Data)^, PExtended(Params^[1].Data)^, PExtended(Params^[2].Data)^, PExtended(Params^[3].Data)^, PExtended(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FilterPointsDist(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FilterPointsDist(PPointArray(Params^[0].Data)^, PExtended(Params^[1].Data)^, PExtended(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^);
end;

procedure RUTIS_ps_FilterPointsLine(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FilterPointsLine(PPointArray(Params^[0].Data)^, PExtended(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^);
end;

procedure RUTIS_ps_GetATPABounds(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBox(Result^.Data)^ := ps_GetATPABounds(P2DPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetTPABounds(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBox(Result^.Data)^ := ps_GetTPABounds(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_FindTPAinTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindTPAinTPA(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_GetSamePointsATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_GetSamePointsATPA(P2DPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_FindTextTPAinTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindTextTPAinTPA(Pinteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PPointArray(Params^[2].Data)^, PPointArray(Params^[3].Data)^);
end;

procedure RUTIS_ps_SortCircleWise(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SortCircleWise(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PBoolean(Params^[4].Data)^, PBoolean(Params^[5].Data)^);
end;

procedure RUTIS_ps_LinearSort(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_LinearSort(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PBoolean(Params^[4].Data)^);
end;

procedure RUTIS_ps_RotatePoint(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPoint(Result^.Data)^ := ps_RotatePoint(PPoint(Params^[0].Data)^, PExtended(Params^[1].Data)^, PExtended(Params^[2].Data)^, PExtended(Params^[3].Data)^);
end;

procedure RUTIS_ps_ChangeDistPT(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPoint(Result^.Data)^ := ps_ChangeDistPT(PPoint(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pextended(Params^[3].Data)^);
end;

procedure RUTIS_ps_ChangeDistTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_ChangeDistTPA(PPointArray(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pextended(Params^[3].Data)^);
end;

procedure RUTIS_ps_FindGapsTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_FindGapsTPA(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_RemoveDistTPointArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_RemoveDistTPointArray(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PPointArray(Params^[3].Data)^, PBoolean(Params^[4].Data)^);
end;

procedure RUTIS_ps_CombineTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_CombineTPA(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_ReArrangeandShortenArrayEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_ReArrangeandShortenArrayEx(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_ReArrangeandShortenArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_ReArrangeandShortenArray(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_TPAtoATPAEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_TPAtoATPAEx(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_TPAtoATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_TPAtoATPA(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_CombineIntArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PIntegerArray(Result^.Data)^ := ps_CombineIntArray(PIntegerArray(Params^[0].Data)^, PIntegerArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_MergeATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_MergeATPA(P2DPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_AppendTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_AppendTPA(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_TPAFromBox(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_TPAFromBox(PBox(Params^[0].Data)^);
end;

procedure RUTIS_ps_RotatePoints(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_RotatePoints(PPointArray(Params^[0].Data)^, PExtended(Params^[1].Data)^, PExtended(Params^[2].Data)^, PExtended(Params^[3].Data)^);
end;

procedure RUTIS_ps_FindTPAEdges(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_FindTPAEdges(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_ClearTPAFromTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_ClearTPAFromTPA(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_ReturnPointsNotInTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_ReturnPointsNotInTPA(PPointArray(Params^[0].Data)^, PBox(Params^[1].Data)^);
end;

procedure RUTIS_ps_PointInTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_PointInTPA(PPoint(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_ClearDoubleTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ClearDoubleTPA(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_TPACountSort(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_TPACountSort(PPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^, PBoolean(Params^[2].Data)^);
end;

procedure RUTIS_ps_TPACountSortBase(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_TPACountSortBase(PPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^, PPoint(Params^[2].Data)^, PBoolean(Params^[3].Data)^);
end;

procedure RUTIS_ps_InvertTIA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_InvertTIA(PIntegerArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_SumIntegerArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_SumIntegerArray(PIntegerArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_AverageTIA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_AverageTIA(PIntegerArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_AverageExtended(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PExtended(Result^.Data)^ := ps_AverageExtended(PExtendedArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_SplitTPAExWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SplitTPAExWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, P2DPointArray(Params^[3].Data)^);
end;

procedure RUTIS_ps_SplitTPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SplitTPAWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, P2DPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_FindGapsTPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FindGapsTPAWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, P2DPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_RemoveDistTPointArrayWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_RemoveDistTPointArrayWrap(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PPointArray(Params^[3].Data)^, PBoolean(Params^[4].Data)^, PPointArray(Params^[5].Data)^);
end;

procedure RUTIS_ps_CombineTPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_CombineTPAWrap(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_ReArrangeandShortenArrayExWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ReArrangeandShortenArrayExWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PPointArray(Params^[3].Data)^);
end;

procedure RUTIS_ps_ReArrangeandShortenArrayWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ReArrangeandShortenArrayWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_TPAtoATPAExWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_TPAtoATPAExWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, P2DPointArray(Params^[3].Data)^);
end;

procedure RUTIS_ps_TPAtoATPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_TPAtoATPAWrap(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, P2DPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_CombineIntArrayWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_CombineIntArrayWrap(PIntegerArray(Params^[0].Data)^, PIntegerArray(Params^[1].Data)^, PIntegerArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_MergeATPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_MergeATPAWrap(P2DPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_TPAFromBoxWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_TPAFromBoxWrap(PBox(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_RotatePointsWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_RotatePointsWrap(PPointArray(Params^[0].Data)^, PExtended(Params^[1].Data)^, PExtended(Params^[2].Data)^, PExtended(Params^[3].Data)^, PPointArray(Params^[4].Data)^);
end;

procedure RUTIS_ps_FindTPAEdgesWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FindTPAEdgesWrap(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_ClearTPAFromTPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ClearTPAFromTPAWrap(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_ReturnPointsNotInTPAWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ReturnPointsNotInTPAWrap(PPointArray(Params^[0].Data)^, PBox(Params^[1].Data)^, PPointArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_SameTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_SameTPA(PPointArray(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_TPAInATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_TPAInATPA(PPointArray(Params^[0].Data)^, P2DPointArray(Params^[1].Data)^, PLongInt(Params^[2].Data)^);
end;

procedure RUTIS_ps_OffsetTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_OffsetTPA(PPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^);
end;

procedure RUTIS_ps_OffsetATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_OffsetATPA(P2DPointArray(Params^[0].Data)^, PPoint(Params^[1].Data)^);
end;

procedure RUTIS_ps_CopyTPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPointArray(Result^.Data)^ := ps_CopyTPA(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_CopyATPA(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DPointArray(Result^.Data)^ := ps_CopyATPA(P2DPointArray(Params^[0].Data)^);
end;
