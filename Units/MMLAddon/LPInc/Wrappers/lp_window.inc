procedure Lape_SetDesktopAsClient(const Params: PParamArray); lape_extdecl
begin
  ps_SetDesktopAsClient();
end;

procedure Lape_SetTargetArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_SetTargetArray(PInteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_SetTargetBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_SetTargetBitmap(PInteger(Params^[0])^);
end;

procedure Lape_SetEIOSTarget(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^:= CurrThread.Client.IOManager.SetTarget(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_MouseSetClientArea(PInteger(Params^[0])^,
      PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_MouseResetClientArea(const Params: PParamArray); lape_extdecl
begin
  ps_MouseResetClientArea;
end;

procedure Lape_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_ImageSetClientArea(PInteger(Params^[0])^,
      PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_ImageResetClientArea(const Params: PParamArray); lape_extdecl
begin
  ps_ImageResetClientArea;
end;

procedure Lape_SetImageTarget(const Params: PParamArray); lape_extdecl
begin
  ps_SetImageTarget(Pinteger(Params^[0])^);
end;

procedure Lape_SetKeyMouseTarget(const Params: PParamArray); lape_extdecl
begin
  ps_SetKeyMouseTarget(Pinteger(Params^[0])^);
end;

procedure Lape_GetImageTarget(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_GetImageTarget();
end;

procedure Lape_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_GetKeyMouseTarget();
end;

procedure Lape_ExportImageTarget(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTarget_Exported(Result)^ := ps_ExportImageTarget();
end;

procedure Lape_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTarget_Exported(Result)^ := ps_ExportKeyMouseTarget();
end;

procedure Lape_FreeTarget(const Params: PParamArray); lape_extdecl
begin
  ps_FreeTarget(Pinteger(Params^[0])^);
end;

procedure Lape_GetClientDimensions(const Params: PParamArray); lape_extdecl
begin
  ps_GetClientDimensions(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_GetClientPosition(const Params: PParamArray); lape_extdecl
begin
  ps_GetClientPosition(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Freeze(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_Freeze();
end;

procedure Lape_Unfreeze(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_Unfreeze();
end;

procedure Lape_ActivateClient(const Params: PParamArray); lape_extdecl
begin
  ps_ActivateClient();
end;

procedure Lape_IsTargetValid(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_IsTargetValid();
end;
