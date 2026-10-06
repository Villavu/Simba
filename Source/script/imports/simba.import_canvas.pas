{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_canvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportSimbaCanvas(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.canvas, simba.image_drawtext,
  simba.colormath,
  simba.vartype_quad, simba.vartype_circle, simba.vartype_polygon;

type
  PSimbaCanvas = ^TSimbaCanvas;
  PSimbaCanvasState = ^TSimbaCanvasState;
  PQuad = ^TQuad;
  PQuadArray = ^TQuadArray;

(*
TCanvas.DefaultPixel
--------------------
```
property TCanvas.DefaultPixel: TColorBGRA;
property TCanvas.DefaultPixel(Value: TColorBGRA);
```

What Clear writes, and where a colour given without one takes its alpha. Opaque black by default.
*)
procedure _LapeCanvas_DefaultPixel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PSimbaCanvas(Params^[0])^.DefaultPixel;
end;

procedure _LapeCanvas_DefaultPixel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DefaultPixel := PColorBGRA(Params^[1])^;
end;

procedure _LapeCanvas_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaCanvas(Params^[0])^.Width;
end;

procedure _LapeCanvas_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaCanvas(Params^[0])^.Height;
end;

procedure _LapeCanvas_Pixel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaCanvas(Params^[0])^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeCanvas_Pixel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

procedure _LapeCanvas_Alpha_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PSimbaCanvas(Params^[0])^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeCanvas_Alpha_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^] := PByte(Params^[3])^;
end;

procedure _LapeCanvas_SetPixels1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeCanvas_SetPixels2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PColorArray(Params^[2])^);
end;

procedure _LapeCanvas_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.Fill(PColor(Params^[1])^);
end;

procedure _LapeCanvas_FillWithAlpha(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FillWithAlpha(PByte(Params^[1])^);
end;

procedure _LapeCanvas_Clear1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.Clear();
end;

procedure _LapeCanvas_Clear2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.Clear(PBox(Params^[1])^);
end;

procedure _LapeCanvas_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

procedure _LapeCanvas_DrawColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaCanvas(Params^[0])^.DrawColor;
end;

procedure _LapeCanvas_DrawColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawColor := PColor(Params^[1])^;
end;

procedure _LapeCanvas_DrawAlpha_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PSimbaCanvas(Params^[0])^.DrawAlpha;
end;

procedure _LapeCanvas_DrawAlpha_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawAlpha := PByte(Params^[1])^;
end;

procedure _LapeCanvas_DrawThickness_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaCanvas(Params^[0])^.DrawThickness;
end;

procedure _LapeCanvas_DrawThickness_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawThickness := PSingle(Params^[1])^;
end;

procedure _LapeCanvas_DrawAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaCanvas(Params^[0])^.DrawAntialiasing;
end;

procedure _LapeCanvas_DrawAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawAntialiasing := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_DrawFilled_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaCanvas(Params^[0])^.DrawFilled;
end;

procedure _LapeCanvas_DrawFilled_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawFilled := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_DrawFeather_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaCanvas(Params^[0])^.DrawFeather;
end;

procedure _LapeCanvas_DrawFeather_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawFeather := PSingle(Params^[1])^;
end;

procedure _LapeCanvas_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaCanvas(Params^[0])^.FontName;
end;

procedure _LapeCanvas_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FontName := PString(Params^[1])^;
end;

procedure _LapeCanvas_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaCanvas(Params^[0])^.FontSize;
end;

procedure _LapeCanvas_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

procedure _LapeCanvas_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaCanvas(Params^[0])^.FontAntialiasing;
end;

procedure _LapeCanvas_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaCanvas(Params^[0])^.FontBold;
end;

procedure _LapeCanvas_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaCanvas(Params^[0])^.FontItalic;
end;

procedure _LapeCanvas_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_FontUnderline_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaCanvas(Params^[0])^.FontUnderline;
end;

procedure _LapeCanvas_FontUnderline_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.FontUnderline := PBoolean(Params^[1])^;
end;

procedure _LapeCanvas_Fonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaCanvas.FontNames();
end;

(*
TCanvas.LoadFonts
-----------------
```
function TCanvas.LoadFonts(Dir: String): Boolean; static;
```

Loads all ".ttf" fonts in the given directory.
*)
procedure _LapeCanvas_LoadFonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaCanvas.LoadFontsInDir(PString(Params^[0])^);
end;

procedure _LapeCanvas_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaCanvas(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure _LapeCanvas_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaCanvas(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeCanvas_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaCanvas(Params^[0])^.TextSize(PString(Params^[1])^);
end;

procedure _LapeCanvas_DrawText1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeCanvas_DrawText2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, ECanvasTextAligns(Params^[3]^));
end;

procedure _LapeCanvas_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeCanvas_DrawHeatmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawHeatmap(PSingleMatrix(Params^[1])^);
end;

procedure _LapeCanvas_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure _LapeCanvas_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawTPA(PPointArray(Params^[1])^);
end;

procedure _LapeCanvas_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeCanvas_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawBox(PBox(Params^[1])^);
end;

procedure _LapeCanvas_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^);
end;

procedure _LapeCanvas_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawPolygon(PPolygon(Params^[1])^);
end;

procedure _LapeCanvas_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawPolygonInverted(PPolygon(Params^[1])^);
end;

procedure _LapeCanvas_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawQuad(PQuad(Params^[1])^);
end;

procedure _LapeCanvas_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^);
end;

procedure _LapeCanvas_DrawCircle1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_DrawCircleInverted1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_DrawCircle2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  with PCircle(Params^[1])^ do
    PSimbaCanvas(Params^[0])^.DrawCircle(TPoint.Create(X, Y), Radius);
end;

procedure _LapeCanvas_DrawCircleInverted2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  with PCircle(Params^[1])^ do
    PSimbaCanvas(Params^[0])^.DrawCircleInverted(TPoint.Create(X, Y), Radius);
end;

procedure _LapeCanvas_DrawEllipse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawEllipse(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCanvas_DrawEllipseInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawEllipseInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCanvas_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^);
end;

procedure _LapeCanvas_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^);
end;

procedure _LapeCanvas_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawPolygonArray(PPolygonArray(Params^[1])^);
end;

procedure _LapeCanvas_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCanvas_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;
(*
TCanvas.State
-------------
```
property TCanvas.State: TCanvasState;
property TCanvas.State(Value: TCanvasState);
```

Every drawing parameter of the canvas in one record, so it can be saved and put back.

```
var
  saved: TCanvasState;
begin
  saved := img.Canvas.State;
  img.Canvas.DrawColor := $0000FF;
  img.Canvas.State := saved;
end;
```
*)
procedure _LapeCanvas_State_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvasState(Result)^ := PSimbaCanvas(Params^[0])^.State;
end;

procedure _LapeCanvas_State_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Params^[0])^.State := PSimbaCanvasState(Params^[1])^;
end;

procedure ImportSimbaCanvas(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addGlobalType('set of enum(LEFT, CENTER, RIGHT, TOP, BOTTOM)', 'ECanvasTextAlign');

    addGlobalType('set of enum(ANTIALIASED, BOLD, ITALIC, UNDERLINE)', 'ECanvasFontStyle');
    addGlobalType('packed record DrawColor: TColor; DrawAlpha: Byte; DrawThickness: Single; DrawAntialiasing: Boolean; DrawFeather: Single; DrawFilled: Boolean; FontName: String; FontSize: Single; FontStyles: ECanvasFontStyle; end;', 'TCanvasState');

    addGlobalType('type TBaseClass', 'TCanvas');
    addProperty('TCanvas', 'Width', 'Integer', @_LapeCanvas_Width_Read);
    addProperty('TCanvas', 'Height', 'Integer', @_LapeCanvas_Height_Read);
    addPropertyIndexed('TCanvas', 'Pixel', 'X, Y: Integer', 'TColor', @_LapeCanvas_Pixel_Read, @_LapeCanvas_Pixel_Write);
    addPropertyIndexed('TCanvas', 'Alpha', 'X, Y: Integer', 'Byte', @_LapeCanvas_Alpha_Read, @_LapeCanvas_Alpha_Write);
    addGlobalFunc('procedure TCanvas.SetPixels(Points: TPointArray; Color: TColor); overload', @_LapeCanvas_SetPixels1);
    addGlobalFunc('procedure TCanvas.SetPixels(Points: TPointArray; Colors: TColorArray); overload', @_LapeCanvas_SetPixels2);
    addGlobalFunc('procedure TCanvas.Fill(Color: TColor);', @_LapeCanvas_Fill);
    addGlobalFunc('procedure TCanvas.FillWithAlpha(Value: Byte);', @_LapeCanvas_FillWithAlpha);
    addGlobalFunc('procedure TCanvas.Clear; overload', @_LapeCanvas_Clear1);
    addGlobalFunc('procedure TCanvas.Clear(Box: TBox); overload', @_LapeCanvas_Clear2);
    addGlobalFunc('procedure TCanvas.ClearInverted(Box: TBox);', @_LapeCanvas_ClearInverted);
    addProperty('TCanvas', 'State', 'TCanvasState', @_LapeCanvas_State_Read, @_LapeCanvas_State_Write);
    addProperty('TCanvas', 'DefaultPixel', 'TColorBGRA', @_LapeCanvas_DefaultPixel_Read, @_LapeCanvas_DefaultPixel_Write);
    addProperty('TCanvas', 'DrawColor', 'TColor', @_LapeCanvas_DrawColor_Read, @_LapeCanvas_DrawColor_Write);
    addProperty('TCanvas', 'DrawAlpha', 'Byte', @_LapeCanvas_DrawAlpha_Read, @_LapeCanvas_DrawAlpha_Write);
    addProperty('TCanvas', 'DrawThickness', 'Single', @_LapeCanvas_DrawThickness_Read, @_LapeCanvas_DrawThickness_Write);
    addProperty('TCanvas', 'DrawAntialiasing', 'Boolean', @_LapeCanvas_DrawAntialiasing_Read, @_LapeCanvas_DrawAntialiasing_Write);
    addProperty('TCanvas', 'DrawFilled', 'Boolean', @_LapeCanvas_DrawFilled_Read, @_LapeCanvas_DrawFilled_Write);
    addProperty('TCanvas', 'DrawFeather', 'Single', @_LapeCanvas_DrawFeather_Read, @_LapeCanvas_DrawFeather_Write);
    addProperty('TCanvas', 'FontName', 'String', @_LapeCanvas_FontName_Read, @_LapeCanvas_FontName_Write);
    addProperty('TCanvas', 'FontSize', 'Single', @_LapeCanvas_FontSize_Read, @_LapeCanvas_FontSize_Write);
    addProperty('TCanvas', 'FontAntialiasing', 'Boolean', @_LapeCanvas_FontAntialiasing_Read, @_LapeCanvas_FontAntialiasing_Write);
    addProperty('TCanvas', 'FontBold', 'Boolean', @_LapeCanvas_FontBold_Read, @_LapeCanvas_FontBold_Write);
    addProperty('TCanvas', 'FontItalic', 'Boolean', @_LapeCanvas_FontItalic_Read, @_LapeCanvas_FontItalic_Write);
    addProperty('TCanvas', 'FontUnderline', 'Boolean', @_LapeCanvas_FontUnderline_Read, @_LapeCanvas_FontUnderline_Write);
    addGlobalFunc('function TCanvas.Fonts: TStringArray; static;', @_LapeCanvas_Fonts);
    addGlobalFunc('function TCanvas.LoadFonts(Dir: String): Boolean; static;', @_LapeCanvas_LoadFonts);
    addGlobalFunc('function TCanvas.TextWidth(Text: String): Integer;', @_LapeCanvas_TextWidth);
    addGlobalFunc('function TCanvas.TextHeight(Text: String): Integer;', @_LapeCanvas_TextHeight);
    addGlobalFunc('function TCanvas.TextSize(Text: String): TPoint;', @_LapeCanvas_TextSize);
    addGlobalFunc('procedure TCanvas.DrawText(Text: String; Position: TPoint); overload', @_LapeCanvas_DrawText1);
    addGlobalFunc('procedure TCanvas.DrawText(Text: String; Box: TBox; Alignments: ECanvasTextAlign); overload', @_LapeCanvas_DrawText2);
    addGlobalFunc('procedure TCanvas.DrawTextLines(Text: TStringArray; Position: TPoint);', @_LapeCanvas_DrawTextLines);
    addGlobalFunc('procedure TCanvas.DrawHeatmap(Mat: TSingleMatrix);', @_LapeCanvas_DrawHeatmap);
    addGlobalFunc('procedure TCanvas.DrawATPA(ATPA: T2DPointArray);', @_LapeCanvas_DrawATPA);
    addGlobalFunc('procedure TCanvas.DrawTPA(TPA: TPointArray);', @_LapeCanvas_DrawTPA);
    addGlobalFunc('procedure TCanvas.DrawLine(Start, Stop: TPoint);', @_LapeCanvas_DrawLine);
    addGlobalFunc('procedure TCanvas.DrawCrosshairs(ACenter: TPoint; Size: Integer);', @_LapeCanvas_DrawCrosshairs);
    addGlobalFunc('procedure TCanvas.DrawCross(ACenter: TPoint; Radius: Integer);', @_LapeCanvas_DrawCross);
    addGlobalFunc('procedure TCanvas.DrawBox(B: TBox);', @_LapeCanvas_DrawBox);
    addGlobalFunc('procedure TCanvas.DrawBoxInverted(B: TBox);', @_LapeCanvas_DrawBoxInverted);
    addGlobalFunc('procedure TCanvas.DrawPolygon(Points: TPolygon);', @_LapeCanvas_DrawPolygon);
    addGlobalFunc('procedure TCanvas.DrawPolygonInverted(Points: TPolygon);', @_LapeCanvas_DrawPolygonInverted);
    addGlobalFunc('procedure TCanvas.DrawQuad(Quad: TQuad);', @_LapeCanvas_DrawQuad);
    addGlobalFunc('procedure TCanvas.DrawQuadInverted(Quad: TQuad);', @_LapeCanvas_DrawQuadInverted);
    addGlobalFunc('procedure TCanvas.DrawCircle(Center: TPoint; Radius: Integer); overload', @_LapeCanvas_DrawCircle1);
    addGlobalFunc('procedure TCanvas.DrawCircleInverted(Center: TPoint; Radius: Integer); overload', @_LapeCanvas_DrawCircleInverted1);
    addGlobalFunc('procedure TCanvas.DrawCircle(Circle: TCircle); overload', @_LapeCanvas_DrawCircle2);
    addGlobalFunc('procedure TCanvas.DrawCircleInverted(Circle: TCircle); overload', @_LapeCanvas_DrawCircleInverted2);
    addGlobalFunc('procedure TCanvas.DrawEllipse(ACenter: TPoint; XRadius, YRadius: Integer);', @_LapeCanvas_DrawEllipse);
    addGlobalFunc('procedure TCanvas.DrawEllipseInverted(ACenter: TPoint; XRadius, YRadius: Integer);', @_LapeCanvas_DrawEllipseInverted);
    addGlobalFunc('procedure TCanvas.DrawQuadArray(Quads: TQuadArray);', @_LapeCanvas_DrawQuadArray);
    addGlobalFunc('procedure TCanvas.DrawBoxArray(Boxes: TBoxArray);', @_LapeCanvas_DrawBoxArray);
    addGlobalFunc('procedure TCanvas.DrawPolygonArray(Polygons: TPolygonArray);', @_LapeCanvas_DrawPolygonArray);
    addGlobalFunc('procedure TCanvas.DrawCircleArray(Centers: TPointArray; Radius: Integer);', @_LapeCanvas_DrawCircleArray);
    addGlobalFunc('procedure TCanvas.DrawCrossArray(Points: TPointArray; Radius: Integer);', @_LapeCanvas_DrawCrossArray);
  end;
end;

end.
