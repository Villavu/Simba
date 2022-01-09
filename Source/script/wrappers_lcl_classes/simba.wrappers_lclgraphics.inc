type
  PAntialiasingMode = ^TAntialiasingMode;
  PBitmap = ^TBitmap;
  PBrush = ^TBrush;
  PBrushStyle = ^TBrushStyle;
  PCanvas = ^TCanvas;
  PCopyMode = ^TCopyMode;
  PFillStyle = ^TFillStyle;
  PFont = ^TFont;
  PFontPitch = ^TFontPitch;
  PFontQuality = ^TFontQuality;
  PFontStyles = ^TFontStyles;
  PGraphic = ^TGraphic;
  PGraphicsObject = ^TGraphicsObject;
  PPen = ^TPen;
  PPenMode = ^TPenMode;
  PPenStyle = ^TPenStyle;
  PPicture = ^TPicture;
  PRect = ^TRect;
  PTransparentMode = ^TTransparentMode;

procedure _LapeGraphicsObject_OnChanging_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PGraphicsObject(Params^[0])^.OnChanging;
end;

procedure _LapeGraphicsObject_OnChanging_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicsObject(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure _LapeGraphicsObject_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PGraphicsObject(Params^[0])^.OnChange;
end;

procedure _LapeGraphicsObject_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicsObject(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeGraphicsObject_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicsObject(Params^[0])^ := TGraphicsObject.Create();
end;

procedure _LapeGraphicsObject_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicsObject(Params^[0])^.Free();
end;

procedure _LapeFont_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^ := TFont.Create();
end;

procedure _LapeFont_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeFont_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.BeginUpdate();
end;

procedure _LapeFont_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.EndUpdate();
end;

procedure _LapeFont_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PFont(Params^[0])^.HandleAllocated();
end;

procedure _LapeFont_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PFont(Params^[0])^.Handle;
end;

procedure _LapeFont_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapeFont_IsDefault(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PFont(Params^[0])^.IsDefault();
end;

procedure _LapeFont_IsEqual(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PFont(Params^[0])^.IsEqual(PFont(Params^[1])^);
end;

procedure _LapeFont_IsMonoSpace_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PFont(Params^[0])^.IsMonoSpace;
end;

procedure _LapeFont_SetDefault(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.SetDefault();
end;

procedure _LapeFont_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFont(Params^[0])^.PixelsPerInch;
end;

procedure _LapeFont_PixelsPerInch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.PixelsPerInch := PInteger(Params^[1])^;
end;

procedure _LapeFont_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColor(Result)^ := PFont(Params^[0])^.Color;
end;

procedure _LapeFont_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Color := PColor(Params^[1])^;
end;

procedure _LapeFont_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFont(Params^[0])^.Height;
end;

procedure _LapeFont_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeFont_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFont(Params^[0])^.Name;
end;

procedure _LapeFont_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Name := PString(Params^[1])^;
end;

procedure _LapeFont_Orientation_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFont(Params^[0])^.Orientation;
end;

procedure _LapeFont_Orientation_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Orientation := PInteger(Params^[1])^;
end;

procedure _LapeFont_Pitch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFontPitch(Result)^ := PFont(Params^[0])^.Pitch;
end;

procedure _LapeFont_Pitch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Pitch := PFontPitch(Params^[1])^;
end;

procedure _LapeFont_Size_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFont(Params^[0])^.Size;
end;

procedure _LapeFont_Size_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Size := PInteger(Params^[1])^;
end;

procedure _LapeFont_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFontStyles(Result)^ := PFont(Params^[0])^.Style;
end;

procedure _LapeFont_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Style := PFontStyles(Params^[1])^;
end;

procedure _LapeFont_Quality_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFontQuality(Result)^ := PFont(Params^[0])^.Quality;
end;

procedure _LapeFont_Quality_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Quality := PFontQuality(Params^[1])^;
end;

procedure _LapeFont_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Params^[0])^.Free();
end;

procedure _LapePen_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^ := TPen.Create();
end;

procedure _LapePen_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapePen_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PPen(Params^[0])^.Handle;
end;

procedure _LapePen_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapePen_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PPen(Params^[0])^.Color;
end;

procedure _LapePen_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapePen_Cosmetic_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPen(Params^[0])^.Cosmetic;
end;

procedure _LapePen_Cosmetic_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Cosmetic := PBoolean(Params^[1])^;
end;

procedure _LapePen_Mode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPenMode(Result)^ := PPen(Params^[0])^.Mode;
end;

procedure _LapePen_Mode_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Mode := PPenMode(Params^[1])^;
end;

procedure _LapePen_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPenStyle(Result)^ := PPen(Params^[0])^.Style;
end;

procedure _LapePen_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Style := PPenStyle(Params^[1])^;
end;

procedure _LapePen_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PPen(Params^[0])^.Width;
end;

procedure _LapePen_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

procedure _LapePen_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Params^[0])^.Free();
end;

procedure _LapeBrush_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeBrush_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Params^[0])^ := TBrush.Create();
end;

procedure _LapeBrush_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBrush(Params^[0])^.Color;
end;

procedure _LapeBrush_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapeBrush_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrushStyle(Result)^ := PBrush(Params^[0])^.Style;
end;

procedure _LapeBrush_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Params^[0])^.Style := PBrushStyle(Params^[1])^;
end;

procedure _LapeBrush_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Params^[0])^.Free();
end;

procedure _LapeCanvas_Lock(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Lock();
end;

procedure _LapeCanvas_TryLock(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.TryLock();
end;

procedure _LapeCanvas_Unlock(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Unlock();
end;

procedure _LapeCanvas_Refresh(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Refresh();
end;

procedure _LapeCanvas_Changing(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Changing();
end;

procedure _LapeCanvas_Changed(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Changed();
end;

procedure _LapeCanvas_SaveHandleState(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.SaveHandleState();
end;

procedure _LapeCanvas_RestoreHandleState(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.RestoreHandleState();
end;

procedure _LapeCanvas_Arc(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Arc(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeCanvas_Chord(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Chord(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeCanvas_CopyRect(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.CopyRect(PRect(Params^[1])^, PCanvas(Params^[2])^, PRect(Params^[3])^);
end;

procedure _LapeCanvas_DrawFocusRect(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.DrawFocusRect(PRect(Params^[1])^);
end;

procedure _LapeCanvas_Ellipse(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Ellipse(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_FillRect(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.FillRect(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_FloodFill(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.FloodFill(PInteger(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PFillStyle(Params^[4])^);
end;

procedure _LapeCanvas_RadialPie(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.RadialPie(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeCanvas_Pie(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Pie(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeCanvas_PolyBezierEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.PolyBezier(PPointArray(Params^[1])^, Pboolean(Params^[2])^, Pboolean(Params^[3])^);
end;

procedure _LapeCanvas_Polygon(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Polygon(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_PolygonExEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Polygon(PPointArray(Params^[1])^);
end;

procedure _LapeCanvas_Polyline(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Polyline(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCanvas_PolylineExEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Polyline(PPointArray(Params^[1])^);
end;

procedure _LapeCanvas_Rectangle(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Rectangle(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_RectangleEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Rectangle(PRect(Params^[1])^);
end;

procedure _LapeCanvas_RoundRect(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.RoundRect(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeCanvas_RoundRectEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.RoundRect(PRect(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCanvas_TextOut(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.TextOut(PInteger(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeCanvas_TextRect(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.TextRect(PRect(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, PString(Params^[4])^);
end;

procedure _LapeCanvas_TextHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCanvas(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeCanvas_TextWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCanvas(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure _LapeCanvas_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCanvas(Params^[0])^.HandleAllocated();
end;

procedure _LapeCanvas_AutoRedraw_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.AutoRedraw;
end;

procedure _LapeCanvas_AutoRedraw_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.AutoRedraw := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_Brush_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Result)^ := PCanvas(Params^[0])^.Brush;
end;

procedure _LapeCanvas_Brush_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Brush := PBrush(Params^[1])^;
end;

procedure _LapeCanvas_CopyMode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCopyMode(Result)^ := PCanvas(Params^[0])^.CopyMode;
end;

procedure _LapeCanvas_CopyMode_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.CopyMode := PCopyMode(Params^[1])^;
end;

procedure _LapeCanvas_Font_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Result)^ := PCanvas(Params^[0])^.Font;
end;

procedure _LapeCanvas_Font_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Font := PFont(Params^[1])^;
end;

procedure _LapeCanvas_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCanvas(Params^[0])^.Height;
end;

procedure _LapeCanvas_Pen_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Result)^ := PCanvas(Params^[0])^.Pen;
end;

procedure _LapeCanvas_Pen_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Pen := PPen(Params^[1])^;
end;

procedure _LapeCanvas_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCanvas(Params^[0])^.Width;
end;

procedure _LapeCanvas_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCanvas(Params^[0])^.OnChange;
end;

procedure _LapeCanvas_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCanvas_OnChanging_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCanvas(Params^[0])^.OnChanging;
end;

procedure _LapeCanvas_OnChanging_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCanvas_GetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
var
  TPA: TPointArray;
  i, h: integer;
begin
  TPA := PPointArray(Params^[1])^;
  h := High(TPA);
  SetLength(PIntegerArray(Result)^, h + 1);
  for i := 0 to h do
    PIntegerArray(Result)^[i] := PCanvas(Params^[0])^.Pixels[TPA[i].x, TPA[i].y];
end;

procedure _LapeCanvas_Get_Pixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColor(Result)^ := PCanvas(Params^[0])^.Pixels[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeCanvas_Set_Pixel(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Pixels[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

procedure _LapeCanvas_Set_Pixels(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
var
  TPA: TPointArray;
  i, h: integer;
  Col: TColor;
begin
  TPA := PPointArray(Params^[1])^;
  Col := PColor(Params^[2])^;
  h := High(TPA);

  for i := 0 to h do
    PCanvas(Params^[0])^.Pixels[TPA[i].x, TPA[i].y] := Col;
end;

procedure _LapeCanvas_SetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
var
  TPA: TPointArray;
  Cols: TIntegerArray;
  i, h: integer;
begin
  TPA := PPointArray(Params^[1])^;
  Cols := PIntegerArray(Params^[2])^;
  h := High(TPA);

  for i := 0 to h do
    PCanvas(Params^[0])^.Pixels[TPA[i].x, TPA[i].y] := Cols[i];
end;

procedure _LapeCanvas_Draw(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Draw(PInteger(Params^[1])^, PInteger(Params^[2])^, PGraphic(Params^[3])^);
end;

procedure _LapeCanvas_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^ := TCanvas.Create();
end;

procedure _LapeCanvas_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Free();
end;

procedure _LapeCanvas_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Clear();
end;

procedure _LapeCanvas_Frame(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Frame(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_Frame3D(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.Frame3D(PRect(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_LineTo(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.LineTo(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_MoveTo(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.MoveTo(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_AntialiasingMode_Set(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Params^[0])^.AntialiasingMode := PAntialiasingMode(Params^[1])^;
end;

procedure _LapeCanvas_AntialiasingMode_Get(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PAntialiasingMode(Result)^ := PCanvas(Params^[0])^.AntialiasingMode;
end;

procedure _LapeGraphic_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeGraphic_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^ := TGraphic.Create();
end;

procedure _LapeGraphic_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Clear();
end;

procedure _LapeGraphic_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeGraphic_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeGraphic_Empty_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Empty;
end;

procedure _LapeGraphic_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Height;
end;

procedure _LapeGraphic_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeGraphic_Modified_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Modified;
end;

procedure _LapeGraphic_Modified_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

procedure _LapeGraphic_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PGraphic(Params^[0])^.OnChange;
end;

procedure _LapeGraphic_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeGraphic_Palette_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Palette;
end;

procedure _LapeGraphic_Palette_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Palette := PInteger(Params^[1])^;
end;

procedure _LapeGraphic_PaletteModified_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.PaletteModified;
end;

procedure _LapeGraphic_PaletteModified_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.PaletteModified := PBoolean(Params^[1])^;
end;

procedure _LapeGraphic_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Transparent;
end;

procedure _LapeGraphic_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeGraphic_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Width;
end;

procedure _LapeGraphic_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeGraphic_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Params^[0])^.Free();
end;

procedure _LapeGraphic_LoadFromClipboardFormat(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  if (not Clipboard.HasPictureFormat) then
    raise Exception.Create('Clipboard has no image')
  else
    PGraphic(Params^[0])^.LoadFromClipboardFormat(CF_Bitmap);
end;

procedure _LapeBitmap_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^ := TBitmap.Create();
end;

procedure _LapeBitmap_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.BeginUpdate(PBoolean(Params^[1])^);
end;

procedure _LapeBitmap_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.EndUpdate(PBoolean(Params^[1])^);
end;

procedure _LapeBitmap_FreeImage(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.FreeImage();
end;

procedure _LapeBitmap_BitmapHandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.BitmapHandleAllocated();
end;

procedure _LapeBitmap_MaskHandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.MaskHandleAllocated();
end;

procedure _LapeBitmap_PaletteAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.PaletteAllocated();
end;

procedure _LapeBitmap_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeBitmap_LoadFromStreamEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.LoadFromStream(PStream(Params^[1])^, PCardinal(Params^[2])^);
end;

procedure _LapeBitmap_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeBitmap_GetSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBitmap_Mask(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.Mask(PColor(Params^[1])^);
end;

procedure _LapeBitmap_ReleaseBitmapHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PBitmap(Params^[0])^.ReleaseBitmapHandle();
end;

procedure _LapeBitmap_ReleaseMaskHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PBitmap(Params^[0])^.ReleaseMaskHandle();
end;

procedure _LapeBitmap_ReleasePalette(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PBitmap(Params^[0])^.ReleasePalette();
end;

procedure _LapeBitmap_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PBitmap(Params^[0])^.Canvas;
end;

procedure _LapeBitmap_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBitmap(Params^[0])^.HandleAllocated();
end;

procedure _LapeBitmap_BitmapHandle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PBitmap(Params^[0])^.BitmapHandle;
end;

procedure _LapeBitmap_BitmapHandle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.BitmapHandle := PHandle(Params^[1])^;
end;

procedure _LapeBitmap_Masked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.Masked;
end;

procedure _LapeBitmap_Masked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.Masked := PBoolean(Params^[1])^;
end;

procedure _LapeBitmap_MaskHandle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PBitmap(Params^[0])^.MaskHandle;
end;

procedure _LapeBitmap_MaskHandle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.MaskHandle := PHandle(Params^[1])^;
end;

procedure _LapeBitmap_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColor(Result)^ := PBitmap(Params^[0])^.TransparentColor;
end;

procedure _LapeBitmap_TransparentColor_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.TransparentColor := PColor(Params^[1])^;
end;

procedure _LapeBitmap_TransparentMode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTransparentMode(Result)^ := PBitmap(Params^[0])^.TransparentMode;
end;

procedure _LapeBitmap_TransparentMode_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.TransparentMode := PTransparentMode(Params^[1])^;
end;

procedure _LapeBitmap_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.Free();
end;

procedure _LapeBitmap_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeBitmap_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.Transparent;
end;

procedure _LapeBitmap_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
var
  x, y, w, h: integer;
  Addition, Data: string;
begin
  PBitmap(Params^[0])^.GetSize(w, h);
  Data := '';

  for x := 0 to (w - 1) do
    for y := 0 to (h - 1) do
    begin
      Addition := IntToHex(PBitmap(Params^[0])^.Canvas.Pixels[x, y], 1);

      while (length(Addition) < 6) do
        Addition := '0' + Addition;

      Data := (Data + Addition);
    end;

  PString(Result)^ := Format('%d, %d, ''', [w, h]) + Data + '''';
end;

procedure _LapeBitmap_LoadFromString(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
var
  x, y: integer;
begin
  PBitmap(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);

  for x := (PInteger(Params^[1])^ - 1) downto 0 do
    for y := (PInteger(Params^[2])^ - 1) downto 0 do
      PBitmap(Params^[0])^.Canvas.Pixels[x, y] := StrToInt('$' + Copy(PString(Params^[4])^, y * 6 + x * PInteger(Params^[2])^ * 6 + 1, 6));

  PBitmap(Params^[0])^.Mask(PInteger(Params^[3])^);
end;

procedure _LapePicture_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^ := TPicture.Create();
end;

procedure _LapePicture_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.Clear();
end;

procedure _LapePicture_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapePicture_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapePicture_LoadFromStreamWithFileExt(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.LoadFromStreamWithFileExt(PStream(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePicture_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.SaveToFile(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePicture_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapePicture_SaveToStreamWithFileExt(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.SaveToStreamWithFileExt(PStream(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePicture_Bitmap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PPicture(Params^[0])^.Bitmap;
end;

procedure _LapePicture_Bitmap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

procedure _LapePicture_Graphic_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphic(Result)^ := PPicture(Params^[0])^.Graphic;
end;

procedure _LapePicture_Graphic_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.Graphic := PGraphic(Params^[1])^;
end;

procedure _LapePicture_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PPicture(Params^[0])^.Height;
end;

procedure _LapePicture_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PPicture(Params^[0])^.Width;
end;

procedure _LapePicture_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PPicture(Params^[0])^.OnChange;
end;

procedure _LapePicture_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapePicture_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Params^[0])^.Free();
end;

