unit simba.import_lcl_graphics;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportLCLGraphics(Compiler: TSimbaScript_Compiler);

implementation

uses
  graphics, clipbrd, lptypes, ffi;

type
  PAntialiasingMode = ^TAntialiasingMode;
  PBitmap = ^TBitmap;
  PBrush = ^TBrush;
  PBrushStyle = ^TBrushStyle;
  PCanvas = ^TCanvas;
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

procedure _LapeGraphicsObject_OnChanging_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PGraphicsObject(Params^[0])^.OnChanging;
end;

procedure _LapeGraphicsObject_OnChanging_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphicsObject(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure _LapeGraphicsObject_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PGraphicsObject(Params^[0])^.OnChange;
end;

procedure _LapeGraphicsObject_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphicsObject(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeGraphicsObject_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphicsObject(Params^[0])^ := TGraphicsObject.Create();
end;

procedure _LapeGraphicsObject_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphicsObject(Params^[0])^.Free();
end;

procedure _LapeFont_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^ := TFont.Create();
end;

procedure _LapeFont_Assign(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeFont_BeginUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.BeginUpdate();
end;

procedure _LapeFont_EndUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.EndUpdate();
end;

procedure _LapeFont_HandleAllocated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PFont(Params^[0])^.HandleAllocated();
end;

procedure _LapeFont_Handle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PFont(Params^[0])^.Handle;
end;

procedure _LapeFont_Handle_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapeFont_IsDefault(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PFont(Params^[0])^.IsDefault();
end;

procedure _LapeFont_IsEqual(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PFont(Params^[0])^.IsEqual(PFont(Params^[1])^);
end;

procedure _LapeFont_IsMonoSpace_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PFont(Params^[0])^.IsMonoSpace;
end;

procedure _LapeFont_SetDefault(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.SetDefault();
end;

procedure _LapeFont_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PFont(Params^[0])^.PixelsPerInch;
end;

procedure _LapeFont_PixelsPerInch_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.PixelsPerInch := PInteger(Params^[1])^;
end;

procedure _LapeFont_Color_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PFont(Params^[0])^.Color;
end;

procedure _LapeFont_Color_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Color := PColor(Params^[1])^;
end;

procedure _LapeFont_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PFont(Params^[0])^.Height;
end;

procedure _LapeFont_Height_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeFont_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PFont(Params^[0])^.Name;
end;

procedure _LapeFont_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Name := PString(Params^[1])^;
end;

procedure _LapeFont_Orientation_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PFont(Params^[0])^.Orientation;
end;

procedure _LapeFont_Orientation_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Orientation := PInteger(Params^[1])^;
end;

procedure _LapeFont_Pitch_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFontPitch(Result)^ := PFont(Params^[0])^.Pitch;
end;

procedure _LapeFont_Pitch_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Pitch := PFontPitch(Params^[1])^;
end;

procedure _LapeFont_Size_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PFont(Params^[0])^.Size;
end;

procedure _LapeFont_Size_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Size := PInteger(Params^[1])^;
end;

procedure _LapeFont_Style_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFontStyles(Result)^ := PFont(Params^[0])^.Style;
end;

procedure _LapeFont_Style_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Style := PFontStyles(Params^[1])^;
end;

procedure _LapeFont_Quality_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFontQuality(Result)^ := PFont(Params^[0])^.Quality;
end;

procedure _LapeFont_Quality_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Quality := PFontQuality(Params^[1])^;
end;

procedure _LapeFont_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Params^[0])^.Free();
end;

procedure _LapePen_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^ := TPen.Create();
end;

procedure _LapePen_Color_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPen(Params^[0])^.Color;
end;

procedure _LapePen_Color_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapePen_Cosmetic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPen(Params^[0])^.Cosmetic;
end;

procedure _LapePen_Cosmetic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^.Cosmetic := PBoolean(Params^[1])^;
end;

procedure _LapePen_Mode_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPenMode(Result)^ := PPen(Params^[0])^.Mode;
end;

procedure _LapePen_Mode_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^.Mode := PPenMode(Params^[1])^;
end;

procedure _LapePen_Style_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPenStyle(Result)^ := PPen(Params^[0])^.Style;
end;

procedure _LapePen_Style_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^.Style := PPenStyle(Params^[1])^;
end;

procedure _LapePen_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PPen(Params^[0])^.Width;
end;

procedure _LapePen_Width_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

procedure _LapePen_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Params^[0])^.Free();
end;

procedure _LapeBrush_Assign(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeBrush_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Params^[0])^ := TBrush.Create();
end;

procedure _LapeBrush_Color_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PBrush(Params^[0])^.Color;
end;

procedure _LapeBrush_Color_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapeBrush_Style_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBrushStyle(Result)^ := PBrush(Params^[0])^.Style;
end;

procedure _LapeBrush_Style_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Params^[0])^.Style := PBrushStyle(Params^[1])^;
end;

procedure _LapeBrush_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Params^[0])^.Free();
end;

procedure _LapeCanvas_Lock(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Lock();
end;

procedure _LapeCanvas_TryLock(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.TryLock();
end;

procedure _LapeCanvas_Unlock(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Unlock();
end;

procedure _LapeCanvas_Refresh(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Refresh();
end;

procedure _LapeCanvas_Changing(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Changing();
end;

procedure _LapeCanvas_Changed(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Changed();
end;

procedure _LapeCanvas_CopyRect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.CopyRect(PRect(Params^[1])^, PCanvas(Params^[2])^, PRect(Params^[3])^);
end;

procedure _LapeCanvas_DrawFocusRect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.DrawFocusRect(PRect(Params^[1])^);
end;

procedure _LapeCanvas_Ellipse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Ellipse(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_FillRect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.FillRect(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_Pie(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Pie(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeCanvas_Polygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Polygon(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_PolygonExEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Polygon(PPointArray(Params^[1])^);
end;

procedure _LapeCanvas_Polyline(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Polyline(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCanvas_PolylineExEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Polyline(PPointArray(Params^[1])^);
end;

procedure _LapeCanvas_Rectangle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Rectangle(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_RectangleEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Rectangle(PRect(Params^[1])^);
end;

procedure _LapeCanvas_RoundRect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.RoundRect(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeCanvas_RoundRectEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.RoundRect(PRect(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCanvas_TextOut(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.TextOut(PInteger(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeCanvas_TextRect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.TextRect(PRect(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, PString(Params^[4])^);
end;

procedure _LapeCanvas_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCanvas(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeCanvas_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCanvas(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure _LapeCanvas_HandleAllocated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.HandleAllocated();
end;

procedure _LapeCanvas_AutoRedraw_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCanvas(Params^[0])^.AutoRedraw;
end;

procedure _LapeCanvas_AutoRedraw_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.AutoRedraw := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_Brush_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Result)^ := PCanvas(Params^[0])^.Brush;
end;

procedure _LapeCanvas_Brush_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Brush := PBrush(Params^[1])^;
end;

procedure _LapeCanvas_Font_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Result)^ := PCanvas(Params^[0])^.Font;
end;

procedure _LapeCanvas_Font_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Font := PFont(Params^[1])^;
end;

procedure _LapeCanvas_Pen_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Result)^ := PCanvas(Params^[0])^.Pen;
end;

procedure _LapeCanvas_Pen_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Pen := PPen(Params^[1])^;
end;

procedure _LapeCanvas_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCanvas(Params^[0])^.OnChange;
end;

procedure _LapeCanvas_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCanvas_OnChanging_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCanvas(Params^[0])^.OnChanging;
end;

procedure _LapeCanvas_OnChanging_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCanvas_Get_Pixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PCanvas(Params^[0])^.Pixels[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeCanvas_Set_Pixel(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Pixels[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

procedure _LapeCanvas_Draw(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Draw(PInteger(Params^[1])^, PInteger(Params^[2])^, PGraphic(Params^[3])^);
end;

procedure _LapeCanvas_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^ := TCanvas.Create();
end;

procedure _LapeCanvas_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Free();
end;

procedure _LapeCanvas_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Clear();
end;

procedure _LapeCanvas_Frame(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.Frame(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCanvas_AntialiasingMode_Set(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Params^[0])^.AntialiasingMode := PAntialiasingMode(Params^[1])^;
end;

procedure _LapeCanvas_AntialiasingMode_Get(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAntialiasingMode(Result)^ := PCanvas(Params^[0])^.AntialiasingMode;
end;

procedure _LapeGraphic_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.Clear();
end;

procedure _LapeGraphic_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeGraphic_SaveToFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeGraphic_Empty_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Empty;
end;

procedure _LapeGraphic_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Height;
end;

procedure _LapeGraphic_Height_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeGraphic_Modified_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Modified;
end;

procedure _LapeGraphic_Modified_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

procedure _LapeGraphic_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PGraphic(Params^[0])^.OnChange;
end;

procedure _LapeGraphic_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeGraphic_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PGraphic(Params^[0])^.Transparent;
end;

procedure _LapeGraphic_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeGraphic_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PGraphic(Params^[0])^.Width;
end;

procedure _LapeGraphic_Width_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeGraphic_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.Free();
end;

procedure _LapeGraphic_LoadFromClipboardFormat(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.LoadFromClipboardFormat(CF_Bitmap);
end;

procedure _LapeBitmap_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^ := TBitmap.Create();
end;

procedure _LapeBitmap_BeginUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.BeginUpdate(PBoolean(Params^[1])^);
end;

procedure _LapeBitmap_EndUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.EndUpdate(PBoolean(Params^[1])^);
end;

procedure _LapeBitmap_FreeImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.FreeImage();
end;

procedure _LapeBitmap_LoadFromStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeBitmap_LoadFromStreamEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.LoadFromStream(PStream(Params^[1])^, PCardinal(Params^[2])^);
end;

procedure _LapeBitmap_SaveToStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeBitmap_GetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.GetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBitmap_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PBitmap(Params^[0])^.Canvas;
end;

procedure _LapeBitmap_HandleAllocated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.HandleAllocated();
end;

procedure _LapeBitmap_BitmapHandle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PBitmap(Params^[0])^.BitmapHandle;
end;

procedure _LapeBitmap_BitmapHandle_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.BitmapHandle := PHandle(Params^[1])^;
end;

procedure _LapeBitmap_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PBitmap(Params^[0])^.TransparentColor;
end;

procedure _LapeBitmap_TransparentColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.TransparentColor := PColor(Params^[1])^;
end;

procedure _LapeBitmap_TransparentMode_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTransparentMode(Result)^ := PBitmap(Params^[0])^.TransparentMode;
end;

procedure _LapeBitmap_TransparentMode_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.TransparentMode := PTransparentMode(Params^[1])^;
end;

procedure _LapeBitmap_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.Free();
end;

procedure _LapeBitmap_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeBitmap_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.Transparent;
end;

procedure _LapePicture_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^ := TPicture.Create();
end;

procedure _LapePicture_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.Clear();
end;

procedure _LapePicture_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapePicture_LoadFromStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapePicture_LoadFromStreamWithFileExt(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.LoadFromStreamWithFileExt(PStream(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePicture_SaveToFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.SaveToFile(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePicture_SaveToStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapePicture_SaveToStreamWithFileExt(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.SaveToStreamWithFileExt(PStream(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePicture_Bitmap_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PPicture(Params^[0])^.Bitmap;
end;

procedure _LapePicture_Bitmap_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

procedure _LapePicture_Graphic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Result)^ := PPicture(Params^[0])^.Graphic;
end;

procedure _LapePicture_Graphic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.Graphic := PGraphic(Params^[1])^;
end;

procedure _LapePicture_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPicture(Params^[0])^.Height;
end;

procedure _LapePicture_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPicture(Params^[0])^.Width;
end;

procedure _LapePicture_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPicture(Params^[0])^.OnChange;
end;

procedure _LapePicture_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapePicture_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
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
    addGlobalType('(psSolid, psDash, psDot, psDashDot, psDashDotDot, psInsideFrame, psPattern, psClear)', 'TPenStyle');
    addGlobalType('(pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor)', 'TPenMode');
    addGlobalType('(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern)', 'TBrushStyle');
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
    addClassVar('TGraphic', 'Transparent', 'Boolean', @_LapeGraphic_Transparent_Read, @_LapeGraphic_Transparent_Write);
    addClassVar('TGraphic', 'Width', 'Integer', @_LapeGraphic_Width_Read, @_LapeGraphic_Width_Write);

    addClass('TCanvas');
    addGlobalFunc('procedure TCanvas.Lock;', @_LapeCanvas_Lock);
    addGlobalFunc('function TCanvas.TryLock: Boolean;', @_LapeCanvas_TryLock);
    addGlobalFunc('procedure TCanvas.Unlock;', @_LapeCanvas_Unlock);
    addGlobalFunc('procedure TCanvas.Refresh;', @_LapeCanvas_Refresh);
    addGlobalFunc('procedure TCanvas.Changing;', @_LapeCanvas_Changing);
    addGlobalFunc('procedure TCanvas.Changed;', @_LapeCanvas_Changed);
    addGlobalFunc('procedure TCanvas.CopyRect(Dest: TRect; SrcCanvas: TCanvas; Source: TRect);', @_LapeCanvas_CopyRect);
    addGlobalFunc('procedure TCanvas.Draw(X,Y: Integer; SrcGraphic: TGraphic);', @_LapeCanvas_Draw);
    addGlobalFunc('procedure TCanvas.DrawFocusRect(ARect: TRect);', @_LapeCanvas_DrawFocusRect);
    addGlobalFunc('procedure TCanvas.Ellipse(x1, y1, x2, y2: Integer);', @_LapeCanvas_Ellipse);
    addGlobalFunc('procedure TCanvas.FillRect(X1,Y1,X2,Y2: Integer);', @_LapeCanvas_FillRect);
    addGlobalFunc('procedure TCanvas.Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2, StartX,StartY,EndX,EndY: Integer);', @_LapeCanvas_Pie);
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
    addGlobalFunc('procedure TCanvas.SetPixel(x, y: Integer; Color: TColor);', @_LapeCanvas_Set_Pixel);
    addGlobalFunc('procedure TCanvas.Clear;', @_LapeCanvas_Clear);
    addGlobalFunc('procedure TCanvas.Frame(X1, Y1, X2, Y2: Integer);', @_LapeCanvas_Frame);
    addClassVar('TCanvas', 'AutoRedraw', 'Boolean', @_LapeCanvas_AutoRedraw_Read, @_LapeCanvas_AutoRedraw_Write);
    addClassVar('TCanvas', 'Brush', 'TBrush', @_LapeCanvas_Brush_Read, @_LapeCanvas_Brush_Write);
    addClassVar('TCanvas', 'Font', 'TFont', @_LapeCanvas_Font_Read, @_LapeCanvas_Font_Write);
    addClassVar('TCanvas', 'Pen', 'TPen', @_LapeCanvas_Pen_Read, @_LapeCanvas_Pen_Write);
    addClassVar('TCanvas', 'OnChange', 'TNotifyEvent', @_LapeCanvas_OnChange_Read, @_LapeCanvas_OnChange_Write);
    addClassVar('TCanvas', 'OnChanging', 'TNotifyEvent', @_LapeCanvas_OnChanging_Read, @_LapeCanvas_OnChanging_Write);
    addClassVar('TCanvas', 'AntialiasingMode', 'TAntialiasingMode', @_LapeCanvas_AntialiasingMode_Get, @_LapeCanvas_AntialiasingMode_Set);
    addGlobalFunc('procedure TCanvas.Init', @_LapeCanvas_Init);

    addClass('TBitmap', 'TGraphic');
    addGlobalFunc('procedure TBitmap.Init', @_LapeBitmap_Init);
    addGlobalFunc('procedure TBitmap.BeginUpdate(ACanvasOnly: Boolean);', @_LapeBitmap_BeginUpdate);
    addGlobalFunc('procedure TBitmap.EndUpdate(AStreamIsValid: Boolean);', @_LapeBitmap_EndUpdate);
    addGlobalFunc('procedure TBitmap.FreeImage;', @_LapeBitmap_FreeImage);
    addGlobalFunc('procedure TBitmap.LoadFromStream(AStream: TStream);', @_LapeBitmap_LoadFromStream);
    addGlobalFunc('procedure TBitmap.LoadFromStream(AStream: TStream; ASize: UInt32); overload', @_LapeBitmap_LoadFromStreamEx);
    addGlobalFunc('procedure TBitmap.SaveToStream(AStream: TStream);', @_LapeBitmap_SaveToStream);
    addGlobalFunc('procedure TBitmap.GetSize(var AWidth, AHeight: Integer);', @_LapeBitmap_GetSize);
    addGlobalFunc('function TBitmap.HandleAllocated: Boolean;', @_LapeBitmap_HandleAllocated);
    addClassVar('TBitmap', 'Canvas', 'TCanvas', @_LapeBitmap_Canvas_Read);
    addClassVar('TBitmap', 'BitmapHandle', 'THandle', @_LapeBitmap_BitmapHandle_Read, @_LapeBitmap_BitmapHandle_Write);
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

end.

