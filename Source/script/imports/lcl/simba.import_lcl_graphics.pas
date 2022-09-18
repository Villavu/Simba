unit simba.import_lcl_graphics;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, clipbrd, lptypes, ffi,
  simba.mufasatypes, simba.script_compiler;

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
  PNotifyEvent = ^TNotifyEvent;
  PPersistent = ^TPersistent;
  PHandle = ^THandle;
  PStream = ^TStream;

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
  PBoolean(Result)^ := PFont(Params^[0])^.HandleAllocated();
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
  PBoolean(Result)^ := PFont(Params^[0])^.IsDefault();
end;

procedure _LapeFont_IsEqual(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PFont(Params^[0])^.IsEqual(PFont(Params^[1])^);
end;

procedure _LapeFont_IsMonoSpace_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PFont(Params^[0])^.IsMonoSpace;
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
  PCanvas(Params^[0])^.PolyBezier(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
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
  PBoolean(Result)^ := PCanvas(Params^[0])^.HandleAllocated();
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
  i, h: Integer;
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
  i, h: Integer;
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
  i, h: Integer;
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
  PBoolean(Result)^ := PBitmap(Params^[0])^.BitmapHandleAllocated();
end;

procedure _LapeBitmap_MaskHandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.MaskHandleAllocated();
end;

procedure _LapeBitmap_PaletteAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.PaletteAllocated();
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
  PBoolean(Result)^ := PBitmap(Params^[0])^.HandleAllocated();
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
  x, y, w, h: Integer;
  Addition, Data: String;
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
  x, y: Integer;
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

procedure ImportLCLGraphics(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('record Left, Top ,Right, Bottom: Integer; end', 'TRect');
    addGlobalType('(fsBold, fsItalic, fsStrikeOut, fsUnderline)', 'TFontStyle');
    addGlobalType('(fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqCleartype, fqCleartypeNatural)', 'TFontQuality');
    addGlobalType('set of TFontStyle', 'TFontStyles');
    addGlobalType('(fpDefault, fpVariable, fpFixed)', 'TFontPitch');
    addGlobalType('Integer', 'TCopyMode');
    addGlobalType('(psSolid, psDash, psDot, psDashDot, psDashDotDot, psInsideFrame, psPattern, psClear)', 'TPenStyle');
    addGlobalType('(pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor)', 'TPenMode');
    addGlobalType('(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern)', 'TBrushStyle');
    addGlobalType('(fsSurface, fsBorder)', 'TFillStyle');
    addGlobalType('(tmAuto, tmFixed)', 'TTransparentMode');
    addGlobalType('(amDontCare, amOn, amOff)', 'TAntialiasingMode');
    addGlobalType('(tlTop, tlCenter, tlBottom)', 'TTextLayout');

    addClass('TGraphicsObject');
    addClassVar('TGraphicsObject', 'OnChanging', 'TNotifyEvent', @_LapeGraphicsObject_OnChanging_Read, @_LapeGraphicsObject_OnChanging_Write);
    addClassVar('TGraphicsObject', 'OnChange', 'TNotifyEvent', @_LapeGraphicsObject_OnChange_Read, @_LapeGraphicsObject_OnChange_Write);
    addGlobalFunc('procedure TGraphicsObject.Init', @_LapeGraphicsObject_Init);

    addClass('TFont', 'TGraphicsObject');
    addGlobalFunc('procedure TFont.Init; override', @_LapeFont_Init);
    addGlobalFunc('procedure TFont.BeginUpdate;', @_LapeFont_BeginUpdate);
    addGlobalFunc('procedure TFont.EndUpdate;', @_LapeFont_EndUpdate);
    addGlobalFunc('function TFont.HandleAllocated: Boolean;', @_LapeFont_HandleAllocated);
    addClassVar('TFont', 'Handle', 'THandle', @_LapeFont_Handle_Read, @_LapeFont_Handle_Write);
    addGlobalFunc('function TFont.IsDefault: Boolean;', @_LapeFont_IsDefault);
    addGlobalFunc('function TFont.IsEqual(AFont: TFont): Boolean;', @_LapeFont_IsEqual);
    addClassVar('TFont', 'IsMonoSpace', 'Boolean', @_LapeFont_IsMonoSpace_Read);
    addGlobalFunc('procedure TFont.SetDefault;', @_LapeFont_SetDefault);
    addClassVar('TFont', 'PixelsPerInch', 'Integer', @_LapeFont_PixelsPerInch_Read, @_LapeFont_PixelsPerInch_Write);
    addClassVar('TFont', 'Color', 'TColor', @_LapeFont_Color_Read, @_LapeFont_Color_Write);
    addClassVar('TFont', 'Height', 'Integer', @_LapeFont_Height_Read, @_LapeFont_Height_Write);
    addClassVar('TFont', 'Name', 'String', @_LapeFont_Name_Read, @_LapeFont_Name_Write);
    addClassVar('TFont', 'Orientation', 'Integer', @_LapeFont_Orientation_Read, @_LapeFont_Orientation_Write);
    addClassVar('TFont', 'Pitch', 'TFontPitch', @_LapeFont_Pitch_Read, @_LapeFont_Pitch_Write);
    addClassVar('TFont', 'Size', 'Integer', @_LapeFont_Size_Read, @_LapeFont_Size_Write);
    addClassVar('TFont', 'Style', 'TFontStyles', @_LapeFont_Style_Read, @_LapeFont_Style_Write);
    addClassVar('TFont', 'Quality', 'TFontQuality', @_LapeFont_Quality_Read, @_LapeFont_Quality_Write);

    addClass('TPen', 'TGraphicsObject');
    addGlobalFunc('procedure TPen.Init; override', @_LapePen_Init);
    addClassVar('TPen', 'Color', 'Integer', @_LapePen_Color_Read, @_LapePen_Color_Write);
    addClassVar('TPen', 'Cosmetic', 'Boolean', @_LapePen_Cosmetic_Read, @_LapePen_Cosmetic_Write);
    addClassVar('TPen', 'Mode', 'TPenMode', @_LapePen_Mode_Read, @_LapePen_Mode_Write);
    addClassVar('TPen', 'Style', 'TPenStyle', @_LapePen_Style_Read, @_LapePen_Style_Write);
    addClassVar('TPen', 'Width', 'Integer', @_LapePen_Width_Read, @_LapePen_Width_Write);

    addClass('TBrush', 'TGraphicsObject');
    addGlobalFunc('procedure TBrush.Init; override', @_LapeBrush_Init);
    addClassVar('TBrush', 'Color', 'Integer', @_LapeBrush_Color_Read, @_LapeBrush_Color_Write);
    addClassVar('TBrush', 'Style', 'TBrushStyle', @_LapeBrush_Style_Read, @_LapeBrush_Style_Write);

    addClass('TGraphic');
    addGlobalFunc('procedure TGraphic.Clear;', @_LapeGraphic_Clear);
    addGlobalFunc('procedure TGraphic.LoadFromFile(const Filename: String);', @_LapeGraphic_LoadFromFile);
    addGlobalFunc('procedure TGraphic.SaveToFile(const Filename: String);', @_LapeGraphic_SaveToFile);
    addGlobalFunc('procedure TGraphic.LoadFromClipboardFormat;', @_LapeGraphic_LoadFromClipboardFormat);
    addClassVar('TGraphic', 'Empty', 'Boolean', @_LapeGraphic_Empty_Read);
    addClassVar('TGraphic', 'Height', 'Integer', @_LapeGraphic_Height_Read, @_LapeGraphic_Height_Write);
    addClassVar('TGraphic', 'Modified', 'Boolean', @_LapeGraphic_Modified_Read, @_LapeGraphic_Modified_Write);
    addClassVar('TGraphic', 'OnChange', 'TNotifyEvent', @_LapeGraphic_OnChange_Read, @_LapeGraphic_OnChange_Write);
    addClassVar('TGraphic', 'Palette', 'Integer', @_LapeGraphic_Palette_Read, @_LapeGraphic_Palette_Write);
    addClassVar('TGraphic', 'PaletteModified', 'Boolean', @_LapeGraphic_PaletteModified_Read, @_LapeGraphic_PaletteModified_Write);
    addClassVar('TGraphic', 'Transparent', 'Boolean', @_LapeGraphic_Transparent_Read, @_LapeGraphic_Transparent_Write);
    addClassVar('TGraphic', 'Width', 'Integer', @_LapeGraphic_Width_Read, @_LapeGraphic_Width_Write);

    addClass('TCanvas');
    addGlobalFunc('procedure TCanvas.Lock;', @_LapeCanvas_Lock);
    addGlobalFunc('function TCanvas.TryLock: Boolean;', @_LapeCanvas_TryLock);
    addGlobalFunc('procedure TCanvas.Unlock;', @_LapeCanvas_Unlock);
    addGlobalFunc('procedure TCanvas.Refresh;', @_LapeCanvas_Refresh);
    addGlobalFunc('procedure TCanvas.Changing;', @_LapeCanvas_Changing);
    addGlobalFunc('procedure TCanvas.Changed;', @_LapeCanvas_Changed);
    addGlobalFunc('procedure TCanvas.SaveHandleState;', @_LapeCanvas_SaveHandleState);
    addGlobalFunc('procedure TCanvas.RestoreHandleState;', @_LapeCanvas_RestoreHandleState);
    addGlobalFunc('procedure TCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);', @_LapeCanvas_Arc);
    addGlobalFunc('procedure TCanvas.Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer);', @_LapeCanvas_Chord);
    addGlobalFunc('procedure TCanvas.CopyRect(Dest: TRect; SrcCanvas: TCanvas; Source: TRect);', @_LapeCanvas_CopyRect);
    addGlobalFunc('procedure TCanvas.Draw(X,Y: Integer; SrcGraphic: TGraphic);', @_LapeCanvas_Draw);
    addGlobalFunc('procedure TCanvas.DrawFocusRect(ARect: TRect);', @_LapeCanvas_DrawFocusRect);
    addGlobalFunc('procedure TCanvas.Ellipse(x1, y1, x2, y2: Integer);', @_LapeCanvas_Ellipse);
    addGlobalFunc('procedure TCanvas.FillRect(X1,Y1,X2,Y2: Integer);', @_LapeCanvas_FillRect);
    addGlobalFunc('procedure TCanvas.FloodFill(X, Y: Integer; FillColor: TColor;FillStyle: TFillStyle);', @_LapeCanvas_FloodFill);
    addGlobalFunc('procedure TCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);', @_LapeCanvas_RadialPie);
    addGlobalFunc('procedure TCanvas.Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2, StartX,StartY,EndX,EndY: Integer);', @_LapeCanvas_Pie);
    addGlobalFunc('procedure TCanvas.PolyBezier(Points: TPointArray;Filled: Boolean; Continuous: Boolean); overload', @_LapeCanvas_PolyBezierEx);
    addGlobalFunc('procedure TCanvas.Polygon(Points: TPointArray;Winding: Boolean;StartIndex: Integer; NumPts: Integer);', @_LapeCanvas_Polygon);
    addGlobalFunc('procedure TCanvas.Polygon(Points: TPointArray); overload', @_LapeCanvas_PolygonExEx);
    addGlobalFunc('procedure TCanvas.Polyline(Points: TPointArray;StartIndex: Integer;NumPts: Integer);', @_LapeCanvas_Polyline);
    addGlobalFunc('procedure TCanvas.Polyline(Points: TPointArray); overload', @_LapeCanvas_PolylineExEx);
    addGlobalFunc('procedure TCanvas.Rectangle(X1,Y1,X2,Y2: Integer);', @_LapeCanvas_Rectangle);
    addGlobalFunc('procedure TCanvas.Rectangle(ARect: TRect); overload', @_LapeCanvas_RectangleEx);
    addGlobalFunc('procedure TCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer);', @_LapeCanvas_RoundRect);
    addGlobalFunc('procedure TCanvas.RoundRect(Rect: TRect; RX,RY: Integer); overload', @_LapeCanvas_RoundRectEx);
    addGlobalFunc('procedure TCanvas.TextOut(X,Y: Integer;  Text: String);', @_LapeCanvas_TextOut);
    addGlobalFunc('procedure TCanvas.TextRect(ARect: TRect; X, Y: Integer;  Text: String);', @_LapeCanvas_TextRect);
    addGlobalFunc('function TCanvas.TextHeight(Text: String): Integer;', @_LapeCanvas_TextHeight);
    addGlobalFunc('function TCanvas.TextWidth(Text: String): Integer;', @_LapeCanvas_TextWidth);
    addGlobalFunc('function TCanvas.HandleAllocated: Boolean;', @_LapeCanvas_HandleAllocated);
    addGlobalFunc('function TCanvas.GetPixel(x, y: Integer): TColor;', @_LapeCanvas_Get_Pixel);
    addGlobalFunc('procedure TCanvas.SetPixel(x, y: Integer; Colour: TColor);', @_LapeCanvas_Set_Pixel);
    addGlobalFunc('procedure TCanvas.SetPixels(TPA: TPointArray; Colour: TColor);', @_LapeCanvas_Set_Pixels);
    addGlobalFunc('procedure TCanvas.SetPixels(TPA: TPointArray; Cols: TIntegerArray); overload', @_LapeCanvas_SetPixels);
    addGlobalFunc('function TCanvas.GetPixels(const TPA: TPointArray): TIntegerArray;', @_LapeCanvas_GetPixels);
    addGlobalFunc('procedure TCanvas.Clear;', @_LapeCanvas_Clear);
    addGlobalFunc('procedure TCanvas.MoveTo(x, y: Integer);', @_LapeCanvas_MoveTo);
    addGlobalFunc('procedure TCanvas.LineTo(x, y: Integer);', @_LapeCanvas_LineTo);
    addGlobalFunc('procedure TCanvas.Frame3D(var ARect: TRect; TopColor, BottomColor: TColor; const FrameWidth: Integer);', @_LapeCanvas_Frame3D);
    addGlobalFunc('procedure TCanvas.Frame(X1, Y1, X2, Y2: Integer);', @_LapeCanvas_Frame);
    addClassVar('TCanvas', 'AutoRedraw', 'Boolean', @_LapeCanvas_AutoRedraw_Read, @_LapeCanvas_AutoRedraw_Write);
    addClassVar('TCanvas', 'Brush', 'TBrush', @_LapeCanvas_Brush_Read, @_LapeCanvas_Brush_Write);
    addClassVar('TCanvas', 'CopyMode', 'TCopyMode', @_LapeCanvas_CopyMode_Read, @_LapeCanvas_CopyMode_Write);
    addClassVar('TCanvas', 'Font', 'TFont', @_LapeCanvas_Font_Read, @_LapeCanvas_Font_Write);
    addClassVar('TCanvas', 'Height', 'Integer', @_LapeCanvas_Height_Read);
    addClassVar('TCanvas', 'Pen', 'TPen', @_LapeCanvas_Pen_Read, @_LapeCanvas_Pen_Write);
    addClassVar('TCanvas', 'Width', 'Integer', @_LapeCanvas_Width_Read);
    addClassVar('TCanvas', 'OnChange', 'TNotifyEvent', @_LapeCanvas_OnChange_Read, @_LapeCanvas_OnChange_Write);
    addClassVar('TCanvas', 'OnChanging', 'TNotifyEvent', @_LapeCanvas_OnChanging_Read, @_LapeCanvas_OnChanging_Write);
    addClassVar('TCanvas', 'AntialiasingMode', 'TAntialiasingMode', @_LapeCanvas_AntialiasingMode_Get, @_LapeCanvas_AntialiasingMode_Set);
    addGlobalFunc('procedure TCanvas.Init', @_LapeCanvas_Init);

    addClass('TBitmap', 'TGraphic');
    addGlobalFunc('procedure TBitmap.Init', @_LapeBitmap_Init);
    addGlobalFunc('procedure TBitmap.BeginUpdate(ACanvasOnly: Boolean);', @_LapeBitmap_BeginUpdate);
    addGlobalFunc('procedure TBitmap.EndUpdate(AStreamIsValid: Boolean);', @_LapeBitmap_EndUpdate);
    addGlobalFunc('procedure TBitmap.FreeImage;', @_LapeBitmap_FreeImage);
    addGlobalFunc('function TBitmap.BitmapHandleAllocated: Boolean;', @_LapeBitmap_BitmapHandleAllocated);
    addGlobalFunc('function TBitmap.MaskHandleAllocated: Boolean;', @_LapeBitmap_MaskHandleAllocated);
    addGlobalFunc('function TBitmap.PaletteAllocated: Boolean;', @_LapeBitmap_PaletteAllocated);
    addGlobalFunc('procedure TBitmap.LoadFromStream(AStream: TStream);', @_LapeBitmap_LoadFromStream);
    addGlobalFunc('procedure TBitmap.LoadFromStream(AStream: TStream; ASize: UInt32); overload', @_LapeBitmap_LoadFromStreamEx);
    addGlobalFunc('procedure TBitmap.SaveToStream(AStream: TStream);', @_LapeBitmap_SaveToStream);
    addGlobalFunc('procedure TBitmap.GetSize(var AWidth, AHeight: Integer);', @_LapeBitmap_GetSize);
    addGlobalFunc('procedure TBitmap.Mask(ATransparentColor: TColor);', @_LapeBitmap_Mask);
    addGlobalFunc('function TBitmap.ReleaseBitmapHandle: THandle;', @_LapeBitmap_ReleaseBitmapHandle);
    addGlobalFunc('function TBitmap.ReleaseMaskHandle: THandle;', @_LapeBitmap_ReleaseMaskHandle);
    addGlobalFunc('function TBitmap.ReleasePalette: THandle;', @_LapeBitmap_ReleasePalette);
    addGlobalFunc('function TBitmap.ToString: String;', @_LapeBitmap_ToString);
    addGlobalFunc('procedure TBitmap.LoadFromString(w, h, TransparentColor: Integer; data: String);', @_LapeBitmap_LoadFromString);
    addGlobalFunc('function TBitmap.HandleAllocated: Boolean;', @_LapeBitmap_HandleAllocated);
    addClassVar('TBitmap', 'Canvas', 'TCanvas', @_LapeBitmap_Canvas_Read);
    addClassVar('TBitmap', 'BitmapHandle', 'THandle', @_LapeBitmap_BitmapHandle_Read, @_LapeBitmap_BitmapHandle_Write);
    addClassVar('TBitmap', 'Masked', 'Boolean', @_LapeBitmap_Masked_Read, @_LapeBitmap_Masked_Write);
    addClassVar('TBitmap', 'MaskHandle', 'THandle', @_LapeBitmap_MaskHandle_Read, @_LapeBitmap_MaskHandle_Write);
    addClassVar('TBitmap', 'TransparentColor', 'TColor', @_LapeBitmap_TransparentColor_Read, @_LapeBitmap_TransparentColor_Write);
    addClassVar('TBitmap', 'TransparentMode', 'TTransparentMode', @_LapeBitmap_TransparentMode_Read, @_LapeBitmap_TransparentMode_Write);

    addClass('TPicture');
    addGlobalFunc('procedure TPicture.Init;', @_LapePicture_Init);
    addGlobalFunc('procedure TPicture.Clear;', @_LapePicture_Clear);
    addGlobalFunc('procedure TPicture.LoadFromFile(const Filename: String);', @_LapePicture_LoadFromFile);
    addGlobalFunc('procedure TPicture.LoadFromStream(Stream: TStream);', @_LapePicture_LoadFromStream);
    addGlobalFunc('procedure TPicture.LoadFromStreamWithFileExt(Stream: TStream; const FileExt: String);', @_LapePicture_LoadFromStreamWithFileExt);
    addGlobalFunc('procedure TPicture.SaveToFile(const Filename: String; const FileExt: String);', @_LapePicture_SaveToFile);
    addGlobalFunc('procedure TPicture.SaveToStream(Stream: TStream);', @_LapePicture_SaveToStream);
    addGlobalFunc('procedure TPicture.SaveToStreamWithFileExt(Stream: TStream; const FileExt: String);', @_LapePicture_SaveToStreamWithFileExt);
    addClassVar('TPicture', 'Bitmap', 'TBitmap', @_LapePicture_Bitmap_Read, @_LapePicture_Bitmap_Write);
    addClassVar('TPicture', 'Graphic', 'TGraphic', @_LapePicture_Graphic_Read, @_LapePicture_Graphic_Write);
    addClassVar('TPicture', 'Height', 'Integer', @_LapePicture_Height_Read);
    addClassVar('TPicture', 'Width', 'Integer', @_LapePicture_Width_Read);
    addClassVar('TPicture', 'OnChange', 'TNotifyEvent', @_LapePicture_OnChange_Read, @_LapePicture_OnChange_Write);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportLCLGraphics);

end.

