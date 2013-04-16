procedure Lape_TMufasaBitmap_LoadFromFile(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.LoadFromFile(PString(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PBoolean(Result)^ := Self.SaveToFile(PString(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_Rectangle(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.Rectangle(PBox(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_FloodFill(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.FloodFill(PPoint(Params^[1])^, Pinteger(Params^[2])^,Pinteger(Params^[3])^);
end;

procedure Lape_TMufasaBitmap_FastSetPixel(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.FastSetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^,Pinteger(Params^[3])^);
end;

procedure Lape_TMufasaBitmap_FastSetPixels(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.FastSetPixels(PPointArray(Params^[1])^, PintegerArray(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_DrawTPA(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.DrawTPA(PPointArray(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_LineTo(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^,PInteger(Params^[3])^);
end;

procedure Lape_TMufasaBitmap_DrawATPA(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.DrawATPA(P2DPointArray(Params^[1])^);
end;

{procedure Lape_TMufasaBitmap_DrawToCanvas(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.DrawToCanvas(Pinteger(Params^[1])^,Pinteger(Params^[2])^,PCanvas(Params^[3])^);
end; }

procedure Lape_TMufasaBitmap_CreateTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PPointArray(Result)^ := Self.CreateTPA(PInteger(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_FastGetPixel(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Pinteger(Result)^ := Self.FastGetPixel(PInteger(Params^[1])^,PInteger(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_FastGetPixels(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PIntegerArray(Result)^ := Self.FastGetPixels(PPointArray(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_GetAreaColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  P2dintegerArray(Result)^ := Self.GetAreaColors(PInteger(Params^[1])^,PInteger(Params^[2])^,PInteger(Params^[3])^,PInteger(Params^[4])^);
end;

procedure Lape_TMufasaBitmap_FastDrawClear(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.FastDrawClear(Pinteger(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_FastDrawTransparent(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.FastDrawTransparent(Pinteger(Params^[1])^,Pinteger(Params^[2])^,PMufasaBitmap(Params^[3])^);
end;

procedure Lape_TMufasaBitmap_FastReplaceColor(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.FastReplaceColor(Pinteger(Params^[1])^,Pinteger(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_RotateBitmap(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.RotateBitmap(PExtended(Params^[1])^,PMufasaBitmap(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_Desaturate(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.Desaturate(PMufasaBitmap(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_GreyScale(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.GreyScale(PMufasaBitmap(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_Brightness(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.Brightness(PMufasaBitmap(Params^[1])^,PInteger(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_Contrast(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.Contrast(PMufasaBitmap(Params^[1])^,PExtended(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_Invert(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.Invert(PMufasaBitmap(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_Posterize(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.Posterize(PMufasaBitmap(Params^[1])^,PInteger(Params^[2])^);
end;

procedure Lape_TMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PMufasaBitmap(Result)^ := Self.Copy(PInteger(Params^[1])^,PInteger(Params^[2])^,PInteger(Params^[3])^,PInteger(Params^[4])^);
end;

procedure Lape_TMufasaBitmap_ToString(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PString(Result)^ := Self.ToString;
end;

procedure Lape_TMufasaBitmap_SetTransparentColor(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.SetTransparentColor(PInteger(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_GetTransparentColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Pinteger(Result)^ := Self.GetTransparentColor;
end;

procedure Lape_TMufasaBitmap_Init(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^ := TMufasaBitmap.Create();
end;

procedure Lape_TMufasaBitmap_Free(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  FreeAndNil(Self);
end;

procedure Lape_TMufasaBitmap_SetAlphaValue(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.SetAlphaValue(PByte(Params^[1])^);
end;



















