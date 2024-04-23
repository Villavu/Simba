unit simba.import_lcl_graphics;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

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

procedure _LapeGraphicsObject_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphicsObject(Result)^ := TGraphicsObject.Create();
end;

procedure _LapeFont_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Result)^ := TFont.Create();
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

procedure _LapePen_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPen(Result)^ := TPen.Create();
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

procedure _LapeBrush_Assign(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeBrush_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Result)^ := TBrush.Create();
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

procedure _LapeCanvas_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := TCanvas.Create();
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

procedure _LapeGraphic_LoadFromClipboardFormat(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphic(Params^[0])^.LoadFromClipboardFormat(CF_Bitmap);
end;

procedure _LapeBitmap_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := TBitmap.Create();
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

procedure _LapeBitmap_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeBitmap_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PBitmap(Params^[0])^.Transparent;
end;

procedure _LapePicture_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Result)^ := TPicture.Create();
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

procedure ImportLCLGraphics(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('record Left, Top ,Right, Bottom: Integer; end', 'TLazRect');
    addGlobalType('enum(fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqCleartype, fqCleartypeNatural)', 'ELazFontQuality');
    addGlobalType('set of enum(fsBold, fsItalic, fsStrikeOut, fsUnderline)', 'ELazFontStyles');
    addGlobalType('enum(fpDefault, fpVariable, fpFixed)', 'ELazFontPitch');
    addGlobalType('enum(psSolid, psDash, psDot, psDashDot, psDashDotDot, psInsideFrame, psPattern, psClear)', 'ELazPenStyle');
    addGlobalType('enum(pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor)', 'ELazPenMode');
    addGlobalType('enum(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern)', 'ELazBrushStyle');
    addGlobalType('enum(tmAuto, tmFixed)', 'ELazTransparentMode');
    addGlobalType('enum(amDontCare, amOn, amOff)', 'ELazAntialiasingMode');
    addGlobalType('enum(tlTop, tlCenter, tlBottom)', 'ELazTextLayout');

    addClass('TLazGraphicsObject');
    addProperty('TLazGraphicsObject', 'OnChanging', 'TLazNotifyEvent', @_LapeGraphicsObject_OnChanging_Read, @_LapeGraphicsObject_OnChanging_Write);
    addProperty('TLazGraphicsObject', 'OnChange', 'TLazNotifyEvent', @_LapeGraphicsObject_OnChange_Read, @_LapeGraphicsObject_OnChange_Write);
    addClassConstructor('TLazGraphicsObject', '', @_LapeGraphicsObject_Create);

    addClass('TLazFont', 'TLazGraphicsObject');
    addClassConstructor('TLazFont', '', @_LapeFont_Create);
    addGlobalFunc('procedure TLazFont.BeginUpdate;', @_LapeFont_BeginUpdate);
    addGlobalFunc('procedure TLazFont.EndUpdate;', @_LapeFont_EndUpdate);
    addGlobalFunc('function TLazFont.HandleAllocated: Boolean;', @_LapeFont_HandleAllocated);
    addProperty('TLazFont', 'Handle', 'TLazHandle', @_LapeFont_Handle_Read, @_LapeFont_Handle_Write);
    addGlobalFunc('function TLazFont.IsDefault: Boolean;', @_LapeFont_IsDefault);
    addGlobalFunc('function TLazFont.IsEqual(AFont: TLazFont): Boolean;', @_LapeFont_IsEqual);
    addProperty('TLazFont', 'IsMonoSpace', 'Boolean', @_LapeFont_IsMonoSpace_Read);
    addGlobalFunc('procedure TLazFont.SetDefault;', @_LapeFont_SetDefault);
    addProperty('TLazFont', 'PixelsPerInch', 'Integer', @_LapeFont_PixelsPerInch_Read, @_LapeFont_PixelsPerInch_Write);
    addProperty('TLazFont', 'Color', 'TColor', @_LapeFont_Color_Read, @_LapeFont_Color_Write);
    addProperty('TLazFont', 'Height', 'Integer', @_LapeFont_Height_Read, @_LapeFont_Height_Write);
    addProperty('TLazFont', 'Name', 'String', @_LapeFont_Name_Read, @_LapeFont_Name_Write);
    addProperty('TLazFont', 'Orientation', 'Integer', @_LapeFont_Orientation_Read, @_LapeFont_Orientation_Write);
    addProperty('TLazFont', 'Pitch', 'ELazFontPitch', @_LapeFont_Pitch_Read, @_LapeFont_Pitch_Write);
    addProperty('TLazFont', 'Size', 'Integer', @_LapeFont_Size_Read, @_LapeFont_Size_Write);
    addProperty('TLazFont', 'Style', 'ELazFontStyles', @_LapeFont_Style_Read, @_LapeFont_Style_Write);
    addProperty('TLazFont', 'Quality', 'ELazFontQuality', @_LapeFont_Quality_Read, @_LapeFont_Quality_Write);

    addClass('TLazPen', 'TLazGraphicsObject');
    addClassConstructor('TLazPen', '', @_LapePen_Create);
    addProperty('TLazPen', 'Color', 'Integer', @_LapePen_Color_Read, @_LapePen_Color_Write);
    addProperty('TLazPen', 'Cosmetic', 'Boolean', @_LapePen_Cosmetic_Read, @_LapePen_Cosmetic_Write);
    addProperty('TLazPen', 'Mode', 'ELazPenMode', @_LapePen_Mode_Read, @_LapePen_Mode_Write);
    addProperty('TLazPen', 'Style', 'ELazPenStyle', @_LapePen_Style_Read, @_LapePen_Style_Write);
    addProperty('TLazPen', 'Width', 'Integer', @_LapePen_Width_Read, @_LapePen_Width_Write);

    addClass('TLazBrush', 'TLazGraphicsObject');
    addClassConstructor('TLazBrush', '', @_LapeBrush_Create);
    addProperty('TLazBrush', 'Color', 'Integer', @_LapeBrush_Color_Read, @_LapeBrush_Color_Write);
    addProperty('TLazBrush', 'Style', 'ELazBrushStyle', @_LapeBrush_Style_Read, @_LapeBrush_Style_Write);

    addClass('TLazGraphic');
    addGlobalFunc('procedure TLazGraphic.Clear;', @_LapeGraphic_Clear);
    addGlobalFunc('procedure TLazGraphic.LoadFromFile(const Filename: String);', @_LapeGraphic_LoadFromFile);
    addGlobalFunc('procedure TLazGraphic.SaveToFile(const Filename: String);', @_LapeGraphic_SaveToFile);
    addGlobalFunc('procedure TLazGraphic.LoadFromClipboardFormat;', @_LapeGraphic_LoadFromClipboardFormat);
    addProperty('TLazGraphic', 'Empty', 'Boolean', @_LapeGraphic_Empty_Read);
    addProperty('TLazGraphic', 'Height', 'Integer', @_LapeGraphic_Height_Read, @_LapeGraphic_Height_Write);
    addProperty('TLazGraphic', 'Modified', 'Boolean', @_LapeGraphic_Modified_Read, @_LapeGraphic_Modified_Write);
    addProperty('TLazGraphic', 'OnChange', 'TLazNotifyEvent', @_LapeGraphic_OnChange_Read, @_LapeGraphic_OnChange_Write);
    addProperty('TLazGraphic', 'Transparent', 'Boolean', @_LapeGraphic_Transparent_Read, @_LapeGraphic_Transparent_Write);
    addProperty('TLazGraphic', 'Width', 'Integer', @_LapeGraphic_Width_Read, @_LapeGraphic_Width_Write);

    addClass('TLazCanvas');
    addGlobalFunc('procedure TLazCanvas.Lock;', @_LapeCanvas_Lock);
    addGlobalFunc('function TLazCanvas.TryLock: Boolean;', @_LapeCanvas_TryLock);
    addGlobalFunc('procedure TLazCanvas.Unlock;', @_LapeCanvas_Unlock);
    addGlobalFunc('procedure TLazCanvas.Refresh;', @_LapeCanvas_Refresh);
    addGlobalFunc('procedure TLazCanvas.Changing;', @_LapeCanvas_Changing);
    addGlobalFunc('procedure TLazCanvas.Changed;', @_LapeCanvas_Changed);
    addGlobalFunc('procedure TLazCanvas.CopyRect(Dest: TLazRect; SrcCanvas: TLazCanvas; Source: TLazRect);', @_LapeCanvas_CopyRect);
    addGlobalFunc('procedure TLazCanvas.Draw(X,Y: Integer; SrcGraphic: TLazGraphic);', @_LapeCanvas_Draw);
    addGlobalFunc('procedure TLazCanvas.DrawFocusRect(ARect: TLazRect);', @_LapeCanvas_DrawFocusRect);
    addGlobalFunc('procedure TLazCanvas.Ellipse(x1, y1, x2, y2: Integer);', @_LapeCanvas_Ellipse);
    addGlobalFunc('procedure TLazCanvas.FillRect(X1,Y1,X2,Y2: Integer);', @_LapeCanvas_FillRect);
    addGlobalFunc('procedure TLazCanvas.Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2, StartX,StartY,EndX,EndY: Integer);', @_LapeCanvas_Pie);
    addGlobalFunc('procedure TLazCanvas.Polygon(Points: TPointArray;Winding: Boolean;StartIndex: Integer; NumPts: Integer);', @_LapeCanvas_Polygon);
    addGlobalFunc('procedure TLazCanvas.Polygon(Points: TPointArray); overload', @_LapeCanvas_PolygonExEx);
    addGlobalFunc('procedure TLazCanvas.Polyline(Points: TPointArray;StartIndex: Integer;NumPts: Integer);', @_LapeCanvas_Polyline);
    addGlobalFunc('procedure TLazCanvas.Polyline(Points: TPointArray); overload', @_LapeCanvas_PolylineExEx);
    addGlobalFunc('procedure TLazCanvas.Rectangle(X1,Y1,X2,Y2: Integer);', @_LapeCanvas_Rectangle);
    addGlobalFunc('procedure TLazCanvas.Rectangle(ARect: TLazRect); overload', @_LapeCanvas_RectangleEx);
    addGlobalFunc('procedure TLazCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer);', @_LapeCanvas_RoundRect);
    addGlobalFunc('procedure TLazCanvas.RoundRect(Rect: TLazRect; RX,RY: Integer); overload', @_LapeCanvas_RoundRectEx);
    addGlobalFunc('procedure TLazCanvas.TextOut(X,Y: Integer;  Text: String);', @_LapeCanvas_TextOut);
    addGlobalFunc('procedure TLazCanvas.TextRect(ARect: TLazRect; X, Y: Integer;  Text: String);', @_LapeCanvas_TextRect);
    addGlobalFunc('function TLazCanvas.TextHeight(Text: String): Integer;', @_LapeCanvas_TextHeight);
    addGlobalFunc('function TLazCanvas.TextWidth(Text: String): Integer;', @_LapeCanvas_TextWidth);
    addGlobalFunc('function TLazCanvas.HandleAllocated: Boolean;', @_LapeCanvas_HandleAllocated);
    addGlobalFunc('function TLazCanvas.GetPixel(x, y: Integer): TColor;', @_LapeCanvas_Get_Pixel);
    addGlobalFunc('procedure TLazCanvas.SetPixel(x, y: Integer; Color: TColor);', @_LapeCanvas_Set_Pixel);
    addGlobalFunc('procedure TLazCanvas.Clear;', @_LapeCanvas_Clear);
    addGlobalFunc('procedure TLazCanvas.Frame(X1, Y1, X2, Y2: Integer);', @_LapeCanvas_Frame);
    addProperty('TLazCanvas', 'AutoRedraw', 'Boolean', @_LapeCanvas_AutoRedraw_Read, @_LapeCanvas_AutoRedraw_Write);
    addProperty('TLazCanvas', 'Brush', 'TLazBrush', @_LapeCanvas_Brush_Read, @_LapeCanvas_Brush_Write);
    addProperty('TLazCanvas', 'Font', 'TLazFont', @_LapeCanvas_Font_Read, @_LapeCanvas_Font_Write);
    addProperty('TLazCanvas', 'Pen', 'TLazPen', @_LapeCanvas_Pen_Read, @_LapeCanvas_Pen_Write);
    addProperty('TLazCanvas', 'OnChange', 'TLazNotifyEvent', @_LapeCanvas_OnChange_Read, @_LapeCanvas_OnChange_Write);
    addProperty('TLazCanvas', 'OnChanging', 'TLazNotifyEvent', @_LapeCanvas_OnChanging_Read, @_LapeCanvas_OnChanging_Write);
    addProperty('TLazCanvas', 'AntialiasingMode', 'ELazAntialiasingMode', @_LapeCanvas_AntialiasingMode_Get, @_LapeCanvas_AntialiasingMode_Set);
    addClassConstructor('TLazCanvas', '', @_LapeCanvas_Create);

    addClass('TLazBitmap', 'TLazGraphic');
    addClassConstructor('TLazBitmap', '', @_LapeBitmap_Create);
    addGlobalFunc('procedure TLazBitmap.BeginUpdate(ACanvasOnly: Boolean);', @_LapeBitmap_BeginUpdate);
    addGlobalFunc('procedure TLazBitmap.EndUpdate(AStreamIsValid: Boolean);', @_LapeBitmap_EndUpdate);
    addGlobalFunc('procedure TLazBitmap.FreeImage;', @_LapeBitmap_FreeImage);
    addGlobalFunc('procedure TLazBitmap.LoadFromStream(AStream: TLazStream);', @_LapeBitmap_LoadFromStream);
    addGlobalFunc('procedure TLazBitmap.LoadFromStream(AStream: TLazStream; ASize: UInt32); overload', @_LapeBitmap_LoadFromStreamEx);
    addGlobalFunc('procedure TLazBitmap.SaveToStream(AStream: TLazStream);', @_LapeBitmap_SaveToStream);
    addGlobalFunc('procedure TLazBitmap.GetSize(var AWidth, AHeight: Integer);', @_LapeBitmap_GetSize);
    addGlobalFunc('function TLazBitmap.HandleAllocated: Boolean;', @_LapeBitmap_HandleAllocated);
    addProperty('TLazBitmap', 'Canvas', 'TLazCanvas', @_LapeBitmap_Canvas_Read);
    addProperty('TLazBitmap', 'BitmapHandle', 'TLazHandle', @_LapeBitmap_BitmapHandle_Read, @_LapeBitmap_BitmapHandle_Write);
    addProperty('TLazBitmap', 'TransparentColor', 'TColor', @_LapeBitmap_TransparentColor_Read, @_LapeBitmap_TransparentColor_Write);
    addProperty('TLazBitmap', 'TransparentMode', 'ELazTransparentMode', @_LapeBitmap_TransparentMode_Read, @_LapeBitmap_TransparentMode_Write);

    addClass('TLazPicture');
    addClassConstructor('TLazPicture', '', @_LapePicture_Create);
    addGlobalFunc('procedure TLazPicture.Clear;', @_LapePicture_Clear);
    addGlobalFunc('procedure TLazPicture.LoadFromFile(const Filename: String);', @_LapePicture_LoadFromFile);
    addGlobalFunc('procedure TLazPicture.LoadFromStream(Stream: TLazStream);', @_LapePicture_LoadFromStream);
    addGlobalFunc('procedure TLazPicture.LoadFromStreamWithFileExt(Stream: TLazStream; const FileExt: String);', @_LapePicture_LoadFromStreamWithFileExt);
    addGlobalFunc('procedure TLazPicture.SaveToFile(const Filename: String; const FileExt: String);', @_LapePicture_SaveToFile);
    addGlobalFunc('procedure TLazPicture.SaveToStream(Stream: TLazStream);', @_LapePicture_SaveToStream);
    addGlobalFunc('procedure TLazPicture.SaveToStreamWithFileExt(Stream: TLazStream; const FileExt: String);', @_LapePicture_SaveToStreamWithFileExt);
    addProperty('TLazPicture', 'Bitmap', 'TLazBitmap', @_LapePicture_Bitmap_Read, @_LapePicture_Bitmap_Write);
    addProperty('TLazPicture', 'Graphic', 'TLazGraphic', @_LapePicture_Graphic_Read, @_LapePicture_Graphic_Write);
    addProperty('TLazPicture', 'Height', 'Integer', @_LapePicture_Height_Read);
    addProperty('TLazPicture', 'Width', 'Integer', @_LapePicture_Width_Read);
    addProperty('TLazPicture', 'OnChange', 'TLazNotifyEvent', @_LapePicture_OnChange_Read, @_LapePicture_OnChange_Write);
  end;
end;

end.

