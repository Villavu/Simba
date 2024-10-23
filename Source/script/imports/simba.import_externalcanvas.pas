unit simba.import_externalcanvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.script_compiler;

procedure ImportExternalCanvas(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.image, simba.image_textdrawer, simba.externalcanvas;

type
  PSimbaExternalCanvas = ^TSimbaExternalCanvas;

procedure _LapeExternalCanvas_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := TSimbaExternalCanvas.Create();
end;

procedure _LapeExternalCanvas_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.Free();
end;

(*
TExternalCanvas.Width
--------------------
> property TExternalCanvas.Width: Integer;
*)
procedure _LapeExternalCanvas_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalCanvas(Params^[0])^.Width;
end;

(*
TExternalCanvas.Height
---------------------
> property TExternalCanvas.Height: Integer;
*)
procedure _LapeExternalCanvas_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalCanvas(Params^[0])^.Height;
end;

(*
TExternalCanvas.GetDefaultPixel
------------------------------
> function TExternalCanvas.GetDefaultPixel: TColorBGRA;
*)
procedure _LapeExternalCanvas_DefaultPixel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PSimbaExternalCanvas(Params^[0])^.DefaultPixel;
end;

(*
TExternalCanvas.SetDefaultPixel
------------------------------
> procedure TExternalCanvas.SetDefaultPixel(Value: TColorBGRA);
*)
procedure _LapeExternalCanvas_DefaultPixel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DefaultPixel := PColorBGRA(Params^[1])^;
end;

(*
TExternalCanvas.GetFontName
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.GetFontName: String;
*)
procedure _LapeExternalCanvas_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaExternalCanvas(Params^[0])^.FontName;
end;

(*
TExternalCanvas.SetFontName
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.SetFontName(Value: String);
*)
procedure _LapeExternalCanvas_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.FontName := PString(Params^[1])^;
end;

(*
TExternalCanvas.GetFontSize
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.GetFontSize: Single;
*)
procedure _LapeExternalCanvas_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaExternalCanvas(Params^[0])^.FontSize;
end;

(*
TExternalCanvas.SetFontSize
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.SetFontSize(Value: Single);
*)
procedure _LapeExternalCanvas_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

(*
TExternalCanvas.GetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.GetFontAntialiasing: Boolean;
*)
procedure _LapeExternalCanvas_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalCanvas(Params^[0])^.FontAntialiasing;
end;

(*
TExternalCanvas.SetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.SetFontAntialiasing(Value: Boolean);
*)
procedure _LapeExternalCanvas_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TExternalCanvas.GetFontBold
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.GetFontBold: Boolean;
*)
procedure _LapeExternalCanvas_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalCanvas(Params^[0])^.FontBold;
end;

(*
TExternalCanvas.SetFontBold
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.SetFontBold(Value: Boolean);
*)
procedure _LapeExternalCanvas_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

(*
TExternalCanvas.GetFontItalic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.GetFontItalic: Boolean;
*)
procedure _LapeExternalCanvas_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalCanvas(Params^[0])^.FontItalic;
end;

(*
TExternalCanvas.SetFontItalic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.SetFontItalic(Value: Boolean);
*)
procedure _LapeExternalCanvas_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

(*
TExternalCanvas.TextWidth
~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.TextWidth(Text: String): Integer;
*)
procedure _LapeExternalCanvas_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalCanvas(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

(*
TExternalCanvas.TextHeight
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.TextHeight(Text: String): Integer;
*)
procedure _LapeExternalCanvas_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalCanvas(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeExternalCanvas_Clear1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.Clear();
end;

procedure _LapeExternalCanvas_Clear2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.Clear(PBox(Params^[1])^);
end;

procedure _LapeExternalCanvas_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

(*
TExternalCanvas.TextSize
~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalCanvas.TextSize(Text: String): TPoint;
*)
procedure _LapeExternalCanvas_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaExternalCanvas(Params^[0])^.TextSize(PString(Params^[1])^);
end;

(*
TExternalCanvas.GetAlpha
-----------------------
> function TExternalCanvas.GetAlpha(X, Y: Integer): Byte;
*)
procedure _LapeExternalCanvas_Alpha_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PSimbaExternalCanvas(Params^[0])^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

(*
TExternalCanvas.SetAlpha
-----------------------
> procedure TExternalCanvas.SetAlpha(X, Y: Integer; Alpha: Byte);
*)
procedure _LapeExternalCanvas_Alpha_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^] := PByte(Params^[3])^;
end;

(*
TExternalCanvas.GetPixel
-----------------------
> function TExternalCanvas.GetPixel(X, Y: Integer): TColor;
*)
procedure _LapeExternalCanvas_Pixel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaExternalCanvas(Params^[0])^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

(*
TExternalCanvas.SetPixel
-----------------------
> procedure TExternalCanvas.SetPixel(X, Y: Integer; Color: TColor);
*)
procedure _LapeExternalCanvas_Pixel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

(*
TExternalCanvas.SetPixels
------------------------
> procedure TExternalCanvas.SetPixels(Points: TPointArray; Color: TColor);
*)
procedure _LapeExternalCanvas_SetPixels1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalCanvas.SetPixels
------------------------
> procedure TExternalCanvas.SetPixels(Points: TPointArray; Colors: TColorArray);
*)
procedure _LapeExternalCanvas_SetPixels2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TExternalCanvas.GetDrawColor
---------------------------
> function TExternalCanvas.GetDrawColor: TColor;

Returns the current drawing color.

```{note}
Red is the default value.
```
*)
procedure _LapeExternalCanvas_DrawColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaExternalCanvas(Params^[0])^.DrawColor;
end;

(*
TExternalCanvas.SetDrawColor
---------------------------
> procedure TExternalCanvas.SetDrawColor(Color: TColor);

Sets the current draw color.

```{note}
Red is the default value.
```
*)
procedure _LapeExternalCanvas_DrawColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawColor := PColor(Params^[1])^;
end;

(*
TExternalCanvas.GetDrawAlpha
---------------------------
> function TExternalCanvas.GetDrawAlpha: Byte;

Returns the current draw alpha.
0 is completely transparent and 255 is completely opauge.

```{note}
255 is the default value.
```
*)
procedure _LapeExternalCanvas_DrawAlpha_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PSimbaExternalCanvas(Params^[0])^.DrawAlpha;
end;

(*
TExternalCanvas.SetDrawAlpha
---------------------------
> procedure TExternalCanvas.SetDrawAlpha(Value: Byte);

Sets the current draw color. This determines how transparent something is drawn.

```{note}
255 is the default value.
```
*)
procedure _LapeExternalCanvas_DrawAlpha_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawAlpha := PByte(Params^[1])^;
end;

(*
TExternalCanvas.DrawText
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.DrawText(Text: String; Position: TPoint);
*)
procedure _LapeExternalCanvas_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TExternalCanvas.DrawText
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign);
*)
procedure _LapeExternalCanvas_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, EImageTextAlign(Params^[3]^));
end;

(*
TExternalCanvas.DrawTextLines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalCanvas.DrawTextLines(Text: TStringArray; Position: TPoint);
*)
procedure _LapeExternalCanvas_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeExternalCanvas_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.Fill(PColor(Params^[1])^);
end;

procedure _LapeExternalCanvas_SetMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.SetMemory(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeExternalCanvas_UserData_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaExternalCanvas(Params^[0])^.UserData
end;

procedure _LapeExternalCanvas_UserData_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.UserData := PPointer(Params^[1])^;
end;

procedure _LapeExternalCanvas_DoubleBuffered_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalCanvas(Params^[0])^.DoubleBuffered;
end;

procedure _LapeExternalCanvas_DoubleBuffered_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DoubleBuffered := PBoolean(Params^[1])^;
end;

procedure _LapeExternalCanvas_DrawCircleAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCircleAA(PPoint(Params^[1])^, PInteger(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeExternalCanvas_DrawLineAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawLineAA(PPoint(Params^[1])^, PPoint(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeExternalCanvas_DrawEllipseAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawEllipseAA(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PSingle(Params^[4])^);
end;

procedure _LapeExternalCanvas_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawImage(PSimbaImage(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawBox(PBox(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawQuad(PQuad(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawLineGap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawLineGap(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeExternalCanvas_FillWithAlpha(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.FillWithAlpha(PByte(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawTPA(PPointArray(Params^[1])^);
end;

procedure _LapeExternalCanvas_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeExternalCanvas_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeExternalCanvas_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeExternalCanvas_BeginUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.BeginUpdate();
end;

procedure _LapeExternalCanvas_EndUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.EndUpdate();
end;

procedure ImportExternalCanvas(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TExternalCanvas', 'TBaseClass');

    addGlobalFunc('function TExternalCanvas.Create: TExternalCanvas; static;', @_LapeExternalCanvas_Create);
    addGlobalFunc('procedure TExternalCanvas.Free;', @_LapeExternalCanvas_Free);
    addGlobalFunc('procedure TExternalCanvas.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);', @_LapeExternalCanvas_SetMemory);

    addProperty('TExternalCanvas', 'UserData', 'Pointer', @_LapeExternalCanvas_UserData_Read, @_LapeExternalCanvas_UserData_Write);
    addProperty('TExternalCanvas', 'DoubleBuffered', 'Boolean', @_LapeExternalCanvas_DoubleBuffered_Read, @_LapeExternalCanvas_DoubleBuffered_Write);

    addProperty('TExternalCanvas', 'Width', 'Integer', @_LapeExternalCanvas_Width_Read);
    addProperty('TExternalCanvas', 'Height', 'Integer', @_LapeExternalCanvas_Height_Read);
    addProperty('TExternalCanvas', 'DefaultPixel', 'TColorBGRA', @_LapeExternalCanvas_DefaultPixel_Read, @_LapeExternalCanvas_DefaultPixel_Write);

    addProperty('TExternalCanvas', 'FontName', 'String', @_LapeExternalCanvas_FontName_Read, @_LapeExternalCanvas_FontName_Write);
    addProperty('TExternalCanvas', 'FontSize', 'Single', @_LapeExternalCanvas_FontSize_Read, @_LapeExternalCanvas_FontSize_Write);
    addProperty('TExternalCanvas', 'FontAntialiasing', 'Boolean', @_LapeExternalCanvas_FontAntialiasing_Read, @_LapeExternalCanvas_FontAntialiasing_Write);
    addProperty('TExternalCanvas', 'FontBold', 'Boolean', @_LapeExternalCanvas_FontBold_Read, @_LapeExternalCanvas_FontBold_Write);
    addProperty('TExternalCanvas', 'FontItalic', 'Boolean', @_LapeExternalCanvas_FontItalic_Read, @_LapeExternalCanvas_FontItalic_Write);

    addProperty('TExternalCanvas', 'DrawColor', 'TColor', @_LapeExternalCanvas_DrawColor_Read, @_LapeExternalCanvas_DrawColor_Write);
    addProperty('TExternalCanvas', 'DrawAlpha', 'Byte', @_LapeExternalCanvas_DrawAlpha_Read, @_LapeExternalCanvas_DrawAlpha_Write);

    addPropertyIndexed('TExternalCanvas', 'Alpha', 'X, Y: Integer', 'Byte', @_LapeExternalCanvas_Alpha_Read, @_LapeExternalCanvas_Alpha_Write);
    addPropertyIndexed('TExternalCanvas', 'Pixel', 'X, Y: Integer', 'TColor', @_LapeExternalCanvas_Pixel_Read, @_LapeExternalCanvas_Pixel_Write);

    addGlobalFunc('function TExternalCanvas.TextWidth(Text: String): Integer;', @_LapeExternalCanvas_TextWidth);
    addGlobalFunc('function TExternalCanvas.TextHeight(Text: String): Integer;', @_LapeExternalCanvas_TextHeight);
    addGlobalFunc('function TExternalCanvas.TextSize(Text: String): TPoint;', @_LapeExternalCanvas_TextSize);

    addGlobalFunc('procedure TExternalCanvas.SetPixels(Points: TPointArray; Color: TColor); overload', @_LapeExternalCanvas_SetPixels1);
    addGlobalFunc('procedure TExternalCanvas.SetPixels(Points: TPointArray; Colors: TColorArray); overload', @_LapeExternalCanvas_SetPixels2);

    addGlobalFunc('procedure TExternalCanvas.DrawText(Text: String; Position: TPoint); overload', @_LapeExternalCanvas_DrawText);
    addGlobalFunc('procedure TExternalCanvas.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign); overload', @_LapeExternalCanvas_DrawTextEx);
    addGlobalFunc('procedure TExternalCanvas.DrawTextLines(Text: TStringArray; Position: TPoint);', @_LapeExternalCanvas_DrawTextLines);

    addGlobalFunc('procedure TExternalCanvas.Fill(Color: TColor)', @_LapeExternalCanvas_Fill);
    addGlobalFunc('procedure TExternalCanvas.FillWithAlpha(Value: Byte);', @_LapeExternalCanvas_FillWithAlpha);

    addGlobalFunc('procedure TExternalCanvas.Clear; overload', @_LapeExternalCanvas_Clear1);
    addGlobalFunc('procedure TExternalCanvas.Clear(Box: TBox); overload;', @_LapeExternalCanvas_Clear2);
    addGlobalFunc('procedure TExternalCanvas.ClearInverted(Box: TBox);', @_LapeExternalCanvas_ClearInverted);

    addGlobalFunc('procedure TExternalCanvas.DrawATPA(ATPA: T2DPointArray)', @_LapeExternalCanvas_DrawATPA);
    addGlobalFunc('procedure TExternalCanvas.DrawTPA(TPA: TPointArray)', @_LapeExternalCanvas_DrawTPA);

    addGlobalFunc('procedure TExternalCanvas.DrawImage(Image: TImage; Position: TPoint);', @_LapeExternalCanvas_DrawImage);

    addGlobalFunc('procedure TExternalCanvas.DrawBox(B: TBox);', @_LapeExternalCanvas_DrawBox);
    addGlobalFunc('procedure TExternalCanvas.DrawBoxFilled(B: TBox);', @_LapeExternalCanvas_DrawBoxFilled);
    addGlobalFunc('procedure TExternalCanvas.DrawBoxInverted(B: TBox);', @_LapeExternalCanvas_DrawBoxInverted);

    addGlobalFunc('procedure TExternalCanvas.DrawPolygon(Points: TPointArray);', @_LapeExternalCanvas_DrawPolygon);
    addGlobalFunc('procedure TExternalCanvas.DrawPolygonFilled(Points: TPointArray);', @_LapeExternalCanvas_DrawPolygonFilled);
    addGlobalFunc('procedure TExternalCanvas.DrawPolygonInverted(Points: TPointArray);', @_LapeExternalCanvas_DrawPolygonInverted);

    addGlobalFunc('procedure TExternalCanvas.DrawQuad(Quad: TQuad);', @_LapeExternalCanvas_DrawQuad);
    addGlobalFunc('procedure TExternalCanvas.DrawQuadFilled(Quad: TQuad);', @_LapeExternalCanvas_DrawQuadFilled);
    addGlobalFunc('procedure TExternalCanvas.DrawQuadInverted(Quad: TQuad);', @_LapeExternalCanvas_DrawQuadInverted);

    addGlobalFunc('procedure TExternalCanvas.DrawCircle(Center: TPoint; Radius: Integer)', @_LapeExternalCanvas_DrawCircle);
    addGlobalFunc('procedure TExternalCanvas.DrawCircleFilled(Center: TPoint; Radius: Integer)', @_LapeExternalCanvas_DrawCircleFilled);
    addGlobalFunc('procedure TExternalCanvas.DrawCircleInverted(Center: TPoint; Radius: Integer)', @_LapeExternalCanvas_DrawCircleInverted);

    addGlobalFunc('procedure TExternalCanvas.DrawCrosshairs(ACenter: TPoint; Size: Integer);', @_LapeExternalCanvas_DrawCrosshairs);
    addGlobalFunc('procedure TExternalCanvas.DrawCross(ACenter: TPoint; Radius: Integer);', @_LapeExternalCanvas_DrawCross);

    addGlobalFunc('procedure TExternalCanvas.DrawLine(Start, Stop: TPoint);', @_LapeExternalCanvas_DrawLine);
    addGlobalFunc('procedure TExternalCanvas.DrawLineGap(Start, Stop: TPoint; GapSize: Integer);', @_LapeExternalCanvas_DrawLineGap);

    addGlobalFunc('procedure TExternalCanvas.DrawCircleAA(ACenter: TPoint; Radius: Integer; Thickness: Single = 1.5)', @_LapeExternalCanvas_DrawCircleAA);
    addGlobalFunc('procedure TExternalCanvas.DrawLineAA(Start, Stop: TPoint; Thickness: Single = 1.5)', @_LapeExternalCanvas_DrawLineAA);
    addGlobalFunc('procedure TExternalCanvas.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single = 1.5)', @_LapeExternalCanvas_DrawEllipseAA);

    addGlobalFunc('procedure TExternalCanvas.DrawQuadArray(Quads: TQuadArray; Filled: Boolean);', @_LapeExternalCanvas_DrawQuadArray);
    addGlobalFunc('procedure TExternalCanvas.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);', @_LapeExternalCanvas_DrawBoxArray);
    addGlobalFunc('procedure TExternalCanvas.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean);', @_LapeExternalCanvas_DrawPolygonArray);
    addGlobalFunc('procedure TExternalCanvas.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);', @_LapeExternalCanvas_DrawCircleArray);
    addGlobalFunc('procedure TExternalCanvas.DrawCrossArray(Points: TPointArray; Radius: Integer);', @_LapeExternalCanvas_DrawCrossArray);

    addGlobalFunc('procedure TExternalCanvas.BeginUpdate;', @_LapeExternalCanvas_BeginUpdate);
    addGlobalFunc('procedure TExternalCanvas.EndUpdate;', @_LapeExternalCanvas_EndUpdate);
  end;
end;

end.

