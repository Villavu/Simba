procedure _LapeWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DbgOut(PString(Params^[0])^);
end;

procedure _LapeWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DebugLn('');
end;

procedure _LapeGetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariable(PString(Params^[0])^);
end;

procedure _LapeGetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(GetCurrentThreadID());
end;

procedure _LapeGetMainThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(MainThreadID);
end;

procedure _LapeGetProcessorCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := TThread.ProcessorCount;
end;

procedure _LapeWait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

type
  TSync = object
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSync.Execute;
type
  TSyncProcedure = procedure of object; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(Params^[0]^)();
end;

procedure _LapeSync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := Default(TSync);
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);
end;

procedure _LapePauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bUnknown;
end;

procedure _LapeTerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bFalse;
end;

procedure _LapeIsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated;
end;

procedure _LapeIsUserTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated and SimbaScript.UserTerminated;
end;

procedure _LapePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Point(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIntToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := IntToBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIntInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IntInBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapePointToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PointToBox(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapePointInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapeMiddleBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleBox(PBox(Params^[0])^);
end;

procedure _LapeSortTIA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sort(PIntegerArray(Params^[0])^);
end;

procedure _LapeUniqueTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := Unique(PPointArray(Params^[0])^);
end;

procedure _LapeUniqueTIA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := Unique(PIntegerArray(Params^[0])^);
end;

procedure _LapeIndicesOfTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf<TPoint>(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndicesOfTIA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf<Integer>(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndicesOfTSA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := specialize IndicesOf<String>(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeIndexOfTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf<TPoint>(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapeIndexOfTIA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf<Integer>(PInteger(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure _LapeIndexOfTSA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize IndexOf<String>(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRectangle_ToTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PRectangle(Params^[0])^.ToTPA();
end;

procedure _LapeRectangle_Bounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PRectangle(Params^[0])^.Bounds();
end;

procedure _LapeRectangle_Middle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PRectangle(Params^[0])^.Middle();
end;

procedure _LapeRectangle_Offset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Result)^ := PRectangle(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeRectangle_OffsetPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Result)^ := PRectangle(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure _LapeRectangle_Expand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Result)^ := PRectangle(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

procedure _LapeRectangle_Contains(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PRectangle(Params^[0])^.Contains(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeRectangle_ContainsPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PRectangle(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

procedure _LapeRectangle_ContainsBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PRectangle(Params^[0])^.Contains(PBox(Params^[1])^);
end;

procedure _LapeRectangle_LongSideLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PRectangle(Params^[0])^.LongSideLen();
end;

procedure _LapeRectangle_ShortSideLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PRectangle(Params^[0])^.ShortSideLen();
end;

procedure _LapeRectangle_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PRectangle(Params^[0])^.Area();
end;

procedure _LapeRectangle_Rotate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Result)^ := PRectangle(Params^[0])^.Rotate(PDouble(Params^[1])^);
end;

procedure _LapeRectangle_RotateFast(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Result)^ := PRectangle(Params^[0])^.RotateFast(PInteger(Params^[1])^);
end;

procedure _LapeRectangle_Normalize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Params^[0])^.Normalize();
end;

procedure _LapeBox_ToRectangle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRectangle(Result)^ := PBox(Params^[0])^.ToRectangle;
end;

procedure _LapeBox_Middle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.Middle;
end;

procedure _LapeBox_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Width;
end;

procedure _LapeBox_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Height;
end;

procedure _LapeBox_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Area;
end;

procedure _LapeBox_Offset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_OffsetPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure _LapeBox_Contains(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_ContainsPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

procedure _LapeBox_ContainsBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PBox(Params^[1])^);
end;

procedure _LapeBox_Combine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Combine(PBox(Params^[1])^);
end;

procedure _LapeBox_Expand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

procedure _LapeBox_ExpandEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeBox_ExpandWidthHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_ExpandWidthHeightEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeBox_Clip(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Params^[0])^.Clip(PBox(Params^[1])^);
end;

procedure _LapeBox_Normalize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Params^[0])^.Normalize();
end;

procedure _LapePoint_InBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InBox(PBox(Params^[1])^);
end;

procedure _LapePoint_InRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InRect(PRectangle(Params^[1])^);
end;

procedure _LapePoint_InCircle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePoint_DistanceTo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PPoint(Params^[0])^.DistanceTo(PPoint(Params^[1])^);
end;

procedure _LapePoint_AngleBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PPoint(Params^[0])^.AngleBetween(PPoint(Params^[1])^);
end;

procedure _LapePoint_Offset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePoint_OffsetPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure _LapePoint_Rotate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Rotate(PDouble(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapePoint_RotateFast(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.RotateFast(PInteger(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapePoint_Scale(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Scale(PDouble(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_Before(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Before(PString(Params^[1])^);
end;

procedure _LapeString_After(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.After(PString(Params^[1])^);
end;

procedure _LapeString_StartsWith(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PString(Params^[0])^.StartsWith(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_EndsWith(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PString(Params^[0])^.EndsWith(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_Upper(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Upper();
end;

procedure _LapeString_Lower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Lower();
end;

procedure _LapeString_Split(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := PString(Params^[0])^.Split(PString(Params^[1])^);
end;

procedure _LapeString_PadLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.PadLeft(PInteger(Params^[1])^, PChar(Params^[2])^);
end;

procedure _LapeString_PadRight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.PadRight(PInteger(Params^[1])^, PChar(Params^[2])^);
end;

procedure _LapeString_Replace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Replace(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeString_ReplaceEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PReplaceFlags = ^TReplaceFlags;
begin
  PString(Result)^ := PString(Params^[0])^.Replace(PString(Params^[1])^, PString(Params^[2])^, PReplaceFlags(Params^[3])^);
end;

procedure _LapeString_Trim(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Trim();
end;

procedure _LapeString_TrimLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.TrimLeft();
end;

procedure _LapeString_TrimRight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.TrimRight();
end;

procedure _LapeString_TrimEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.Trim(PCharArray(Params^[1])^);
end;

procedure _LapeString_TrimLeftEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.TrimLeft(PCharArray(Params^[1])^);
end;

procedure _LapeString_TrimRightEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PCharArray = ^TCharArray;
begin
  PString(Result)^ := PString(Params^[0])^.TrimRight(PCharArray(Params^[1])^);
end;

procedure _LapeString_RegExprSplit(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := PString(Params^[0])^.RegExprSplit(PString(Params^[1])^);
end;

procedure _LapeString_RegExprFind(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PRegExprMatch = ^TRegExprMatch;
begin
  PRegExprMatch(Result)^ := PString(Params^[0])^.RegExprFind(PString(Params^[1])^);
end;

procedure _LapeString_RegExprFindAll(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PRegExprMatchArray = ^TRegExprMatchArray;
begin
  PRegExprMatchArray(Result)^ := PString(Params^[0])^.RegExprFindAll(PString(Params^[1])^);
end;

procedure _LapeString_RegExprExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PString(Params^[0])^.RegExprExists(PString(Params^[1])^);
end;

procedure _LapeString_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Copy();
end;

procedure _LapeString_CopyEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_CopyExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Copy(PInteger(Params^[1])^);
end;

procedure _LapeString_CopyRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.CopyRange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Params^[0])^.Delete(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_DeleteRange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Params^[0])^.DeleteRange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Params^[0])^.Insert(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PString(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeString_IndexOfOffset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PString(Params^[0])^.IndexOf(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_LastIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PString(Params^[0])^.LastIndexOf(PString(Params^[1])^);
end;

procedure _LapeString_LastIndexOfOffset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PString(Params^[0])^.LastIndexOf(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_IndicesOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PString(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeString_IndicesOfOffset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PString(Params^[0])^.IndicesOf(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeString_Between(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Between(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeString_BetweenAll(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := PString(Params^[0])^.BetweenAll(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeString_NumberChars(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := String.NumberChars();
end;

procedure _LapeString_AlphaChars(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := String.AlphaChars();
end;

procedure _LapeString_Extract(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PString(Params^[0])^.Extract(PString(Params^[1])^);
end;

procedure _LapeString_ExtractNumber(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PString(Params^[0])^.ExtractNumber(PInt64(Params^[1])^);
end;

procedure _LapeString_ExtractNumberEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PString(Params^[0])^.ExtractNumber(PString(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeString_Contains(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PString(Params^[0])^.Contains(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_ContainsAny(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PString(Params^[0])^.ContainsAny(PStringArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_IndexOfAny(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PString(Params^[0])^.IndexOfAny(PStringArray(Params^[1])^);
end;

procedure _LapeString_IndexOfAnyOffset(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PString(Params^[0])^.IndexOfAny(PStringArray(Params^[1])^, PInt64(Params^[2])^);
end;


