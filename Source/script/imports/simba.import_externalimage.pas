unit simba.import_externalimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.script_compiler;

procedure ImportSimbaExternalImage(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.image, simba.image_textdrawer, simba.externalimage;

procedure _LapeExternalImage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := TSimbaExternalImage.Create();
end;

procedure _LapeExternalImage_FreeOnTerminate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

procedure _LapeExternalImage_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalImage(Params^[0])^.Width;
end;

procedure _LapeExternalImage_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalImage(Params^[0])^.Height;
end;

(*
TExternalImage.GetFontName
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.GetFontName: String;
*)
procedure _LapeExternalImage_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaExternalImage(Params^[0])^.FontName;
end;

(*
TExternalImage.SetFontName
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.SetFontName(Value: String);
*)
procedure _LapeExternalImage_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.FontName := PString(Params^[1])^;
end;

(*
TExternalImage.GetFontSize
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.GetFontSize: Single;
*)
procedure _LapeExternalImage_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaExternalImage(Params^[0])^.FontSize;
end;

(*
TExternalImage.SetFontSize
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.SetFontSize(Value: Single);
*)
procedure _LapeExternalImage_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

(*
TExternalImage.GetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.GetFontAntialiasing: Boolean;
*)
procedure _LapeExternalImage_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalImage(Params^[0])^.FontAntialiasing;
end;

(*
TExternalImage.SetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.SetFontAntialiasing(Value: Boolean);
*)
procedure _LapeExternalImage_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TExternalImage.GetFontBold
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.GetFontBold: Boolean;
*)
procedure _LapeExternalImage_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalImage(Params^[0])^.FontBold;
end;

(*
TExternalImage.SetFontBold
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.SetFontBold(Value: Boolean);
*)
procedure _LapeExternalImage_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

(*
TExternalImage.GetFontItalic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.GetFontItalic: Boolean;
*)
procedure _LapeExternalImage_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalImage(Params^[0])^.FontItalic;
end;

(*
TExternalImage.SetFontItalic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.SetFontItalic(Value: Boolean);
*)
procedure _LapeExternalImage_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

(*
TExternalImage.TextWidth
~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.TextWidth(Text: String): Integer;
*)
procedure _LapeExternalImage_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalImage(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

(*
TExternalImage.TextHeight
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.TextHeight(Text: String): Integer;
*)
procedure _LapeExternalImage_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalImage(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeExternalImage_Clear1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Clear();
end;

procedure _LapeExternalImage_Clear2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Clear(PBox(Params^[1])^);
end;

procedure _LapeExternalImage_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

(*
TExternalImage.TextSize
~~~~~~~~~~~~~~~~~~~~~~~
> function TExternalImage.TextSize(Text: String): TPoint;
*)
procedure _LapeExternalImage_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaExternalImage(Params^[0])^.TextSize(PString(Params^[1])^);
end;

(*
TExternalImage.DrawText
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawText(Text: String; Position: TPoint; Color: TColor);
*)
procedure _LapeExternalImage_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawText
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawText(Text: String; Box: TBox; Alignments: ETextDrawAlignmentSet; Color: TColor);
*)
procedure _LapeExternalImage_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, ETextDrawAlignSet(Params^[3]^), PColor(Params^[4])^);
end;

(*
TExternalImage.DrawTextLines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
*)
procedure _LapeExternalImage_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeExternalImage_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Fill(PColor(Params^[1])^, PByte(Params^[2])^);
end;

procedure _LapeExternalImage_SetMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.SetMemory(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeExternalImage_GetUserData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaExternalImage(Params^[0])^.UserData
end;

procedure _LapeExternalImage_SetUserData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.UserData := PPointer(Params^[1])^;
end;

procedure _LapeExternalImage_DrawCircleAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleAA(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PSingle(Params^[4])^);
end;

procedure _LapeExternalImage_DrawLineAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawLineAA(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^, PSingle(Params^[4])^);
end;

procedure _LapeExternalImage_DrawEllipseAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawEllipseAA(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PColor(Params^[4])^, PSingle(Params^[5])^);
end;

procedure _LapeExternalImage_BeginUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.BeginUpdate();
end;

procedure _LapeExternalImage_EndUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.EndUpdate();
end;

procedure _LapeExternalImage_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawImage(PSimbaImage(Params^[1])^, PPoint(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PByte(Params^[4])^);
end;

procedure _LapeExternalImage_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PByte(Params^[4])^);
end;

procedure _LapeExternalImage_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PByte(Params^[4])^);
end;

procedure _LapeExternalImage_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PByte(Params^[4])^);
end;

procedure _LapeExternalImage_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^, PByte(Params^[4])^);
end;

procedure _LapeExternalImage_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^, PByte(Params^[4])^);
end;

procedure _LapeExternalImage_DrawLineGap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawLineGap(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PByte(Params^[5])^);
end;

procedure _LapeExternalImageSetAlpha1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.SetAlpha(PByte(Params^[1])^);
end;

procedure _LapeExternalImageSetAlpha2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.SetAlpha(PPointArray(Params^[1])^, PByte(Params^[2])^);
end;

procedure _LapeExternalImageSetAlpha3(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.SetAlpha(PColor(Params^[1])^, PByte(Params^[2])^);
end;

procedure _LapeExternalImage_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^, PByte(Params^[3])^);
end;

procedure _LapeExternalImage_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeExternalImage_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeExternalImage_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeExternalImage_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

procedure _LapeExternalImage_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure ImportSimbaExternalImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TExternalImage');

    addGlobalFunc('function TExternalImage.Create: TExternalImage; static;', @_LapeExternalImage_Create);
    addGlobalFunc('procedure TExternalImage.FreeOnTerminate(Value: Boolean);', @_LapeExternalImage_FreeOnTerminate);

    addGlobalFunc('procedure TExternalImage.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);', @_LapeExternalImage_SetMemory);
    addGlobalFunc('procedure TExternalImage.BeginUpdate', @_LapeExternalImage_BeginUpdate);
    addGlobalFunc('procedure TExternalImage.EndUpdate', @_LapeExternalImage_EndUpdate);

    addGlobalFunc('function TExternalImage.Width: Integer;', @_LapeExternalImage_Width);
    addGlobalFunc('function TExternalImage.Height: Integer;', @_LapeExternalImage_Height);

    addClassVar('TExternalImage', 'UserData', 'Pointer', @_LapeExternalImage_GetUserData, @_LapeExternalImage_SetUserData);

    addClassVar('TExternalImage', 'FontName', 'String', @_LapeExternalImage_FontName_Read, @_LapeExternalImage_FontName_Write);
    addClassVar('TExternalImage', 'FontSize', 'Single', @_LapeExternalImage_FontSize_Read, @_LapeExternalImage_FontSize_Write);
    addClassVar('TExternalImage', 'FontAntialiasing', 'Boolean', @_LapeExternalImage_FontAntialiasing_Read, @_LapeExternalImage_FontAntialiasing_Write);
    addClassVar('TExternalImage', 'FontBold', 'Boolean', @_LapeExternalImage_FontBold_Read, @_LapeExternalImage_FontBold_Write);
    addClassVar('TExternalImage', 'FontItalic', 'Boolean', @_LapeExternalImage_FontItalic_Read, @_LapeExternalImage_FontItalic_Write);

    addGlobalFunc('function TExternalImage.TextWidth(Text: String): Integer;', @_LapeExternalImage_TextWidth);
    addGlobalFunc('function TExternalImage.TextHeight(Text: String): Integer;', @_LapeExternalImage_TextHeight);
    addGlobalFunc('function TExternalImage.TextSize(Text: String): TPoint;', @_LapeExternalImage_TextSize);

    addGlobalFunc('procedure TExternalImage.DrawText(Text: String; Position: TPoint; Color: TColor); overload', @_LapeExternalImage_DrawText);
    addGlobalFunc('procedure TExternalImage.DrawText(Text: String; Box: TBox; Alignments: ETextDrawAlignSet; Color: TColor); overload', @_LapeExternalImage_DrawTextEx);
    addGlobalFunc('procedure TExternalImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);', @_LapeExternalImage_DrawTextLines);

    addGlobalFunc('procedure TExternalImage.Fill(Color: TColor; Alpha: Byte)', @_LapeExternalImage_Fill);

    addGlobalFunc('procedure TExternalImage.SetAlpha(Value: Byte); overload;', @_LapeExternalImageSetAlpha1);
    addGlobalFunc('procedure TExternalImage.SetAlpha(Points: TPointArray; Value: Byte); overload;', @_LapeExternalImageSetAlpha2);
    addGlobalFunc('procedure TExternalImage.SetAlpha(Color: TColor; Value: Byte); overload;', @_LapeExternalImageSetAlpha3);

    addGlobalFunc('procedure TExternalImage.Clear; overload', @_LapeExternalImage_Clear1);
    addGlobalFunc('procedure TExternalImage.Clear(Box: TBox); overload;', @_LapeExternalImage_Clear2);
    addGlobalFunc('procedure TExternalImage.ClearInverted(Box: TBox);', @_LapeExternalImage_ClearInverted);

    addGlobalFunc('procedure TExternalImage.DrawATPA(ATPA: T2DPointArray; Color: TColor = -1; Alpha: Byte = 0)', @_LapeExternalImage_DrawATPA);
    addGlobalFunc('procedure TExternalImage.DrawTPA(TPA: TPointArray; Color: TColor; Alpha: Byte = 0)', @_LapeExternalImage_DrawTPA);

    addGlobalFunc('procedure TExternalImage.DrawImage(Image: TImage; Position: TPoint; Alpha: Byte = 0);', @_LapeExternalImage_DrawImage);

    addGlobalFunc('procedure TExternalImage.DrawBox(B: TBox; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawBox);
    addGlobalFunc('procedure TExternalImage.DrawBoxFilled(B: TBox; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawBoxFilled);
    addGlobalFunc('procedure TExternalImage.DrawBoxInverted(B: TBox; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawBoxInverted);

    addGlobalFunc('procedure TExternalImage.DrawPolygon(Points: TPointArray; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawPolygon);
    addGlobalFunc('procedure TExternalImage.DrawPolygonFilled(Points: TPointArray; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawPolygonFilled);
    addGlobalFunc('procedure TExternalImage.DrawPolygonInverted(Points: TPointArray; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawPolygonInverted);

    addGlobalFunc('procedure TExternalImage.DrawQuad(Quad: TQuad; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawQuad);
    addGlobalFunc('procedure TExternalImage.DrawQuadFilled(Quad: TQuad; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawQuadFilled);
    addGlobalFunc('procedure TExternalImage.DrawQuadInverted(Quad: TQuad; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawQuadInverted);

    addGlobalFunc('procedure TExternalImage.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0)', @_LapeExternalImage_DrawCircle);
    addGlobalFunc('procedure TExternalImage.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0)', @_LapeExternalImage_DrawCircleFilled);
    addGlobalFunc('procedure TExternalImage.DrawCircleInverted(Center: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0)', @_LapeExternalImage_DrawCircleInverted);

    addGlobalFunc('procedure TExternalImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawCrosshairs);
    addGlobalFunc('procedure TExternalImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawCross);

    addGlobalFunc('procedure TExternalImage.DrawLine(Start, Stop: TPoint; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawLine);
    addGlobalFunc('procedure TExternalImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor; Alpha: Byte = 0);', @_LapeExternalImage_DrawLineGap);

    addGlobalFunc('procedure TExternalImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Color: TColor; Thickness: Single = 1.5)', @_LapeExternalImage_DrawCircleAA);
    addGlobalFunc('procedure TExternalImage.DrawLineAA(Start, Stop: TPoint; Color: TColor; Thickness: Single = 1.5)', @_LapeExternalImage_DrawLineAA);
    addGlobalFunc('procedure TExternalImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Color: TColor; Thickness: Single = 1.5)', @_LapeExternalImage_DrawEllipseAA);

    addGlobalFunc('procedure TExternalImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawQuadArray);
    addGlobalFunc('procedure TExternalImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawBoxArray);
    addGlobalFunc('procedure TExternalImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawPolygonArray);
    addGlobalFunc('procedure TExternalImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawCircleArray);
    addGlobalFunc('procedure TExternalImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);', @_LapeExternalImage_DrawCrossArray);
  end;
end;

end.

