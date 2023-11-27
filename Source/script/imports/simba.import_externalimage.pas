unit simba.import_externalimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes, simba.script_compiler;

procedure ImportSimbaExternalImage(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.image, simba.externalimage;

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
  PInteger(Result)^ := PSimbaExternalImage(Params^[0])^.Width();
end;

procedure _LapeExternalImage_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaExternalImage(Params^[0])^.Height();
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
TExternalImage.Draw
~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.Draw(Image: TImage; Position: TPoint);
*)
procedure _LapeExternalImage_Draw(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TExternalImage.DrawText
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor);
*)
procedure _LapeExternalImage_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
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

(*
TExternalImage.DrawATPA
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawATPA(ATPA: T2DPointArray; Color: TColor = -1);
*)
procedure _LapeExternalImage_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawTPA
~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawTPA(TPA: TPointArray; Color: TColor);
*)
procedure _LapeExternalImage_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawCross
~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeExternalImage_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawCrosshairs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor);
*)
procedure _LapeExternalImage_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawLine
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeExternalImage_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawLine
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeExternalImage_DrawLineEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawPolygon
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawPolygon(Points: TPointArray; Color: TColor);
*)
procedure _LapeExternalImage_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawPolygonFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);
*)
procedure _LapeExternalImage_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawPolygonInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);
*)
procedure _LapeExternalImage_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawCircle
~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeExternalImage_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircle(PCircle(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawCircleFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeExternalImage_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleFilled(PCircle(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawCircleInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeExternalImage_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleInverted(PCircle(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawBox
~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawBox(B: TBox; Color: TColor);
*)
procedure _LapeExternalImage_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawBoxFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawBoxFilled(B: TBox; Color: TColor);
*)
procedure _LapeExternalImage_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawBoxInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawBoxInverted(B: TBox; Color: TColor);
*)
procedure _LapeExternalImage_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawQuad
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawQuad(B: TBox; Color: TColor);
*)
procedure _LapeExternalImage_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawQuadFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawQuadFilled(B: TBox; Color: TColor);
*)
procedure _LapeExternalImage_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TExternalImage.DrawQuadInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawQuadInverted(B: TBox; Color: TColor);
*)
procedure _LapeExternalImage_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TExternalImage.DrawQuadArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeExternalImage_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawBoxArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeExternalImage_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawPolygonArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeExternalImage_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawCircleArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeExternalImage_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCircleArray(PCircleArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.DrawCrossArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);
*)
procedure _LapeExternalImage_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TExternalImage.Fill
~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.Fill(Color: TColor);
*)
procedure _LapeExternalImage_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Fill(PColor(Params^[1])^);
end;

(*
TExternalImage.Clear
~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.Clear;
*)
procedure _LapeExternalImage_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Clear();
end;

(*
TExternalImage.Clear
~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.Clear(Area: TBox);
*)
procedure _LapeExternalImage_ClearEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Clear(PBox(Params^[1])^);
end;

(*
TExternalImage.ClearInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TExternalImage.ClearInverted(Area: TBox);
*)
procedure _LapeExternalImage_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

procedure _LapeExternalImage_InternalImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaExternalImage(Params^[0])^.InternalImage();
end;

procedure _LapeExternalImage_TryLock(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaExternalImage(Params^[0])^.TryLock();
end;

procedure _LapeExternalImage_Lock(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Lock();
end;

procedure _LapeExternalImage_Unlock(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.Unlock();
end;

procedure _LapeExternalImage_SetMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.SetMemory(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeExternalImage_GetUserData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaExternalImage(Params^[0])^.GetUserData();
end;

procedure _LapeExternalImage_SetUserData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalImage(Params^[0])^.SetUserData(PPointer(Params^[1])^);
end;

procedure ImportSimbaExternalImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TExternalImage');

    addGlobalFunc('function TExternalImage.Create: TExternalImage; static;', @_LapeExternalImage_Create);
    addGlobalFunc('procedure TExternalImage.FreeOnTerminate(Value: Boolean);', @_LapeExternalImage_FreeOnTerminate);

    addClassVar('TExternalImage', 'FontName', 'String', @_LapeExternalImage_FontName_Read, @_LapeExternalImage_FontName_Write);
    addClassVar('TExternalImage', 'FontSize', 'Single', @_LapeExternalImage_FontSize_Read, @_LapeExternalImage_FontSize_Write);
    addClassVar('TExternalImage', 'FontAntialiasing', 'Boolean', @_LapeExternalImage_FontAntialiasing_Read, @_LapeExternalImage_FontAntialiasing_Write);
    addClassVar('TExternalImage', 'FontBold', 'Boolean', @_LapeExternalImage_FontBold_Read, @_LapeExternalImage_FontBold_Write);
    addClassVar('TExternalImage', 'FontItalic', 'Boolean', @_LapeExternalImage_FontItalic_Read, @_LapeExternalImage_FontItalic_Write);
    addClassVar('TExternalImage', 'UserData', 'Pointer', @_LapeExternalImage_GetUserData, @_LapeExternalImage_SetUserData);

    addGlobalFunc('function TExternalImage.TextWidth(Text: String): Integer;', @_LapeExternalImage_TextWidth);
    addGlobalFunc('function TExternalImage.TextHeight(Text: String): Integer;', @_LapeExternalImage_TextHeight);
    addGlobalFunc('function TExternalImage.TextSize(Text: String): TPoint;', @_LapeExternalImage_TextSize);

    addGlobalFunc('procedure TExternalImage.DrawText(Text: String; Position: TPoint; Color: TColor); overload', @_LapeExternalImage_DrawText);
    addGlobalFunc('procedure TExternalImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor); overload', @_LapeExternalImage_DrawTextEx);
    addGlobalFunc('procedure TExternalImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);', @_LapeExternalImage_DrawTextLines);

    addGlobalFunc('function TExternalImage.Width: Integer;', @_LapeExternalImage_Width);
    addGlobalFunc('function TExternalImage.Height: Integer;', @_LapeExternalImage_Height);

    addGlobalFunc('function TExternalImage.InternalImage: TImage;', @_LapeExternalImage_InternalImage);

    addGlobalFunc('function TExternalImage.TryLock: Boolean;', @_LapeExternalImage_TryLock);
    addGlobalFunc('procedure TExternalImage.Lock;', @_LapeExternalImage_Lock);
    addGlobalFunc('procedure TExternalImage.Unlock;', @_LapeExternalImage_Unlock);
    addGlobalFunc('procedure TExternalImage.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);', @_LapeExternalImage_SetMemory);

    addGlobalFunc('procedure TExternalImage.Draw(Image: TImage; Position: TPoint);', @_LapeExternalImage_Draw);

    addGlobalFunc('procedure TExternalImage.DrawATPA(ATPA: T2DPointArray; Color: TColor)', @_LapeExternalImage_DrawATPA);
    addGlobalFunc('procedure TExternalImage.DrawTPA(TPA: TPointArray; Color: TColor);', @_LapeExternalImage_DrawTPA);

    addGlobalFunc('procedure TExternalImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Thickness: Integer; Color: TColor);', @_LapeExternalImage_DrawCrosshairs);
    addGlobalFunc('procedure TExternalImage.DrawCross(ACenter: TPoint; Radius: Integer; Thickness: Integer; Color: TColor);', @_LapeExternalImage_DrawCross);

    addGlobalFunc('procedure TExternalImage.DrawLine(Start, Stop: TPoint; Color: TColor); overload', @_LapeExternalImage_DrawLine);
    addGlobalFunc('procedure TExternalImage.DrawLine(Start, Stop: TPoint; Thickness: Integer; Color: TColor); overload', @_LapeExternalImage_DrawLineEx);

    addGlobalFunc('procedure TExternalImage.DrawPolygon(Points: TPointArray; Color: TColor);', @_LapeExternalImage_DrawPolygon);
    addGlobalFunc('procedure TExternalImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);', @_LapeExternalImage_DrawPolygonFilled);
    addGlobalFunc('procedure TExternalImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);', @_LapeExternalImage_DrawPolygonInverted);

    addGlobalFunc('procedure TExternalImage.DrawCircle(Circle: TCircle; Color: TColor);', @_LapeExternalImage_DrawCircle);
    addGlobalFunc('procedure TExternalImage.DrawCircleFilled(Circle: TCircle; Radius: Integer; Color: TColor);', @_LapeExternalImage_DrawCircleFilled);
    addGlobalFunc('procedure TExternalImage.DrawCircleInverted(Circle: TCircle; Radius: Integer; Color: TColor);', @_LapeExternalImage_DrawCircleInverted);

    addGlobalFunc('procedure TExternalImage.DrawBox(B: TBox; Color: TColor);', @_LapeExternalImage_DrawBox);
    addGlobalFunc('procedure TExternalImage.DrawBoxFilled(B: TBox; Color: TColor);', @_LapeExternalImage_DrawBoxFilled);
    addGlobalFunc('procedure TExternalImage.DrawBoxInverted(B: TBox; Color: TColor);', @_LapeExternalImage_DrawBoxInverted);

    addGlobalFunc('procedure TExternalImage.DrawQuad(Quad: TQuad; Color: TColor);', @_LapeExternalImage_DrawQuad);
    addGlobalFunc('procedure TExternalImage.DrawQuadFilled(Quad: TQuad; Color: TColor);', @_LapeExternalImage_DrawQuadFilled);
    addGlobalFunc('procedure TExternalImage.DrawQuadInverted(Quad: TQuad; Color: TColor);', @_LapeExternalImage_DrawQuadInverted);

    addGlobalFunc('procedure TExternalImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawQuadArray);
    addGlobalFunc('procedure TExternalImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawBoxArray);
    addGlobalFunc('procedure TExternalImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawPolygonArray);
    addGlobalFunc('procedure TExternalImage.DrawCircleArray(Circles: TCircleArray; Filled: Boolean; Color: TColor = -1);', @_LapeExternalImage_DrawCircleArray);
    addGlobalFunc('procedure TExternalImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Thickness: Integer; Color: TColor = -1);', @_LapeExternalImage_DrawCrossArray);

    addGlobalFunc('procedure TExternalImage.Clear; overload', @_LapeExternalImage_Clear);
    addGlobalFunc('procedure TExternalImage.Clear(Area: TBox); overload', @_LapeExternalImage_ClearEx);
    addGlobalFunc('procedure TExternalImage.ClearInverted(Area: TBox);', @_LapeExternalImage_ClearInverted);

    addGlobalFunc('procedure TExternalImage.Fill(Color: TColor);', @_LapeExternalImage_Fill);
  end;
end;

end.

