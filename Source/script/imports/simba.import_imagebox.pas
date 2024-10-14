unit simba.import_imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  simba.base, simba.script_compiler;

procedure ImportSimbaImageBox(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi,
  simba.component_imagebox, simba.component_imageboxcanvas,
  simba.image, simba.image_textdrawer,
  simba.dtm, simba.colormath,
  simba.target;

type
  PComponent = ^TComponent;
  PBitmap = ^TBitmap;

procedure _LapeSimbaImageBox_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImageBox(Params^[0])^.FindDTM(PDTM(Params^[1])^);
end;

procedure _LapeSimbaImageBox_FindColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImageBox(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^);
end;

procedure _LapeSimbaImageBox_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaImageBox(Params^[0])^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^);
end;

procedure _LapeSimbaImageBox_MoveTo(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.MoveTo(PPoint(Params^[1])^);
end;

procedure _LapeSimbaImageBox_IsPointVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.IsPointVisible(PPoint(Params^[1])^);
end;

procedure _LapeSimbaImageBox_MouseX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.MouseX;
end;

procedure _LapeSimbaImageBox_MouseY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.MouseY;
end;

procedure _LapeSimbaImageBox_MousePoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImageBox(Params^[0])^.MousePoint;
end;

procedure _LapeSimbaImageBox_SetBackground(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.SetBackground(PSimbaImage(Params^[1])^);
end;

procedure _LapeSimbaImageBox_SetBackgroundFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.SetBackgroundFromFile(PString(Params^[1])^);
end;

procedure _LapeSimbaImageBox_SetBackgroundFromWindow(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.SetBackgroundFromWindow(PWindowHandle(Params^[1])^);
end;

procedure _LapeSimbaImageBox_SetBackgroundFromTarget1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.SetBackgroundFromTarget(PSimbaTarget(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaImageBox_SetBackgroundFromTarget2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.SetBackgroundFromTarget(PSimbaTarget(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Result)^ := TSimbaImageBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeSimbaImageBox_Status_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImageBox(Params^[0])^.Status;
end;

procedure _LapeSimbaImageBox_Status_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Status := PString(Params^[1])^;
end;

procedure _LapeSimbaImageBox_Background_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSimbaImageBox(Params^[0])^.Background;
end;

procedure _LapeSimbaImageBox_OnImgPaint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxPaintEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgPaint;
end;

procedure _LapeSimbaImageBox_OnImgPaint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgPaint := TImageBoxPaintEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseEnter;
end;

procedure _LapeSimbaImageBox_OnImgMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseEnter := TImageBoxEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseLeave;
end;

procedure _LapeSimbaImageBox_OnImgMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseLeave := TImageBoxEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxMouseEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseDown;
end;

procedure _LapeSimbaImageBox_OnImgMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseDown := TImageBoxMouseEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxMouseEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseUp;
end;

procedure _LapeSimbaImageBox_OnImgMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseUp := TImageBoxMouseEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxMouseMoveEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseMove;
end;

procedure _LapeSimbaImageBox_OnImgMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseMove := TImageBoxMouseMoveEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxClickEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgClick;
end;

procedure _LapeSimbaImageBox_OnImgClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgClick := TImageBoxClickEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgDoubleClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxClickEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgDoubleClick;
end;

procedure _LapeSimbaImageBox_OnImgDoubleClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgDoubleClick := TImageBoxClickEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_OnImgKeyDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxKeyEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgKeyDown;
end;

procedure _LapeSimbaImageBox_OnImgKeyDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgKeyDown := TImageBoxKeyEvent(Params^[1]^);
end;

procedure _LapeSimbaImageBox_ShowScrollBars_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.ShowScrollbars;
end;

procedure _LapeSimbaImageBox_ShowScrollBars_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.ShowScrollbars := PBoolean(Params^[1])^;
end;

procedure _LapeSimbaImageBox_ShowStatusBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.ShowStatusBar;
end;

procedure _LapeSimbaImageBox_ShowStatusBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.ShowStatusBar := PBoolean(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnImgKeyUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxKeyEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgKeyUp;
end;

procedure _LapeSimbaImageBox_OnImgKeyUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgKeyUp := TImageBoxKeyEvent(Params^[1]^);
end;

procedure _LapeImageBoxCanvas_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.FontName;
end;

procedure _LapeImageBoxCanvas_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.FontName := PString(Params^[1])^;
end;

procedure _LapeImageBoxCanvas_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.FontSize;
end;

procedure _LapeImageBoxCanvas_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

procedure _LapeImageBoxCanvas_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.FontAntialiasing;
end;

procedure _LapeImageBoxCanvas_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

procedure _LapeImageBoxCanvas_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.FontBold;
end;

procedure _LapeImageBoxCanvas_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

procedure _LapeImageBoxCanvas_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.FontItalic;
end;

procedure _LapeImageBoxCanvas_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

procedure _LapeImageBoxCanvas_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure _LapeImageBoxCanvas_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeImageBoxCanvas_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImageBoxCanvas(Params^[0])^.TextSize(PString(Params^[1])^);
end;

procedure _LapeImageBoxCanvas_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, EImageTextAlign(Params^[3]^), PColor(Params^[4])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawLineGap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawLineGap(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^, PColor(Params^[4])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawBoxFilled1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawBoxFilled2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawPoly(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPoly(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawPolyFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPolyFilled(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawPoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPoint(PPoint(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawPoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPoints(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxCanvas_DrawHeatmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawHeatmap(PSingleMatrix(Params^[1])^);
end;

procedure ImportSimbaImageBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TImageBoxCanvas');

    addProperty('TImageBoxCanvas', 'FontName', 'String', @_LapeImageBoxCanvas_FontName_Read, @_LapeImageBoxCanvas_FontName_Write);
    addProperty('TImageBoxCanvas', 'FontSize', 'Single', @_LapeImageBoxCanvas_FontSize_Read, @_LapeImageBoxCanvas_FontSize_Write);
    addProperty('TImageBoxCanvas', 'FontAntialiasing', 'Boolean', @_LapeImageBoxCanvas_FontAntialiasing_Read, @_LapeImageBoxCanvas_FontAntialiasing_Write);
    addProperty('TImageBoxCanvas', 'FontBold', 'Boolean', @_LapeImageBoxCanvas_FontBold_Read, @_LapeImageBoxCanvas_FontBold_Write);
    addProperty('TImageBoxCanvas', 'FontItalic', 'Boolean', @_LapeImageBoxCanvas_FontItalic_Read, @_LapeImageBoxCanvas_FontItalic_Write);

    addGlobalFunc('function TImageBoxCanvas.TextWidth(Text: String): Integer;', @_LapeImageBoxCanvas_TextWidth);
    addGlobalFunc('function TImageBoxCanvas.TextHeight(Text: String): Integer;', @_LapeImageBoxCanvas_TextHeight);
    addGlobalFunc('function TImageBoxCanvas.TextSize(Text: String): TPoint;', @_LapeImageBoxCanvas_TextSize);
    addGlobalFunc('procedure TImageBoxCanvas.DrawText(Text: String; Position: TPoint; Color: TColor); overload', @_LapeImageBoxCanvas_DrawText);
    addGlobalFunc('procedure TImageBoxCanvas.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign; Color: TColor); overload', @_LapeImageBoxCanvas_DrawTextEx);

    addGlobalFunc('procedure TImageBoxCanvas.DrawLine(Start, Stop: TPoint; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawLine);
    addGlobalFunc('procedure TImageBoxCanvas.DrawLineGap(Start, Stop: TPoint; GapSize: Integer; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawLineGap);

    addGlobalFunc('procedure TImageBoxCanvas.DrawCross(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawCross);
    addGlobalFunc('procedure TImageBoxCanvas.DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawCrossArray);

    addGlobalFunc('procedure TImageBoxCanvas.DrawBox(Box: TBox; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawBox);
    addGlobalFunc('procedure TImageBoxCanvas.DrawBoxFilled(Box: TBox; Color: TColor); overload;', @_LapeSimbaImageBoxCanvas_DrawBoxFilled1);
    addGlobalFunc('procedure TImageBoxCanvas.DrawBoxFilled(Box: TBox; Color: TColor; Transparency: Single); overload;', @_LapeSimbaImageBoxCanvas_DrawBoxFilled2);

    addGlobalFunc('procedure TImageBoxCanvas.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawCircle);
    addGlobalFunc('procedure TImageBoxCanvas.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawCircleFilled);

    addGlobalFunc('procedure TImageBoxCanvas.DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawPoly);
    addGlobalFunc('procedure TImageBoxCanvas.DrawPolyFilled(Poly: TPointArray; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawPolyFilled);

    addGlobalFunc('procedure TImageBoxCanvas.DrawQuad(Quad: TQuad; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawQuad);
    addGlobalFunc('procedure TImageBoxCanvas.DrawQuadFilled(Quad: TQuad; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawQuadFilled);

    addGlobalFunc('procedure TImageBoxCanvas.DrawPoint(Point: TPoint; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawPoint);
    addGlobalFunc('procedure TImageBoxCanvas.DrawPoints(TPA: TPointArray; Color: TColor);', @_LapeSimbaImageBoxCanvas_DrawPoints);

    addGlobalFunc('procedure TImageBoxCanvas.DrawHeatmap(Mat: TSingleMatrix);', @_LapeSimbaImageBoxCanvas_DrawHeatmap);

    addClass('TImageBox', 'TLazCustomControl');

    addGlobalType('procedure(Sender: TImageBox; Canvas: TImageBoxCanvas; R: TLazRect) of object', 'TImageBoxPaintEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox) of object', 'TImageBoxEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; X, Y: Integer) of object', 'TImageBoxClickEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; var Key: UInt16; Shift: ELazShiftStates) of object', 'TImageBoxKeyEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; Button: ELazMouseButton; Shift: ELazShiftStates; X, Y: Integer) of object', 'TImageBoxMouseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; Shift: ELazShiftStates; X, Y: Integer) of object', 'TImageBoxMouseMoveEvent', FFI_DEFAULT_ABI);

    addProperty('TImageBox', 'OnImgPaint', 'TImageBoxPaintEvent', @_LapeSimbaImageBox_OnImgPaint_Read, @_LapeSimbaImageBox_OnImgPaint_Write);
    addProperty('TImageBox', 'OnImgMouseEnter', 'TImageBoxEvent', @_LapeSimbaImageBox_OnImgMouseEnter_Read, @_LapeSimbaImageBox_OnImgMouseEnter_Write);
    addProperty('TImageBox', 'OnImgMouseLeave', 'TImageBoxEvent', @_LapeSimbaImageBox_OnImgMouseLeave_Read, @_LapeSimbaImageBox_OnImgMouseLeave_Write);
    addProperty('TImageBox', 'OnImgMouseDown', 'TImageBoxMouseEvent', @_LapeSimbaImageBox_OnImgMouseDown_Read, @_LapeSimbaImageBox_OnImgMouseDown_Write);
    addProperty('TImageBox', 'OnImgMouseUp', 'TImageBoxMouseEvent', @_LapeSimbaImageBox_OnImgMouseUp_Read, @_LapeSimbaImageBox_OnImgMouseUp_Write);
    addProperty('TImageBox', 'OnImgMouseMove', 'TImageBoxMouseMoveEvent', @_LapeSimbaImageBox_OnImgMouseMove_Read, @_LapeSimbaImageBox_OnImgMouseMove_Write);
    addProperty('TImageBox', 'OnImgClick', 'TImageBoxClickEvent', @_LapeSimbaImageBox_OnImgClick_Read, @_LapeSimbaImageBox_OnImgClick_Write);
    addProperty('TImageBox', 'OnImgDoubleClick', 'TImageBoxClickEvent', @_LapeSimbaImageBox_OnImgDoubleClick_Read, @_LapeSimbaImageBox_OnImgDoubleClick_Write);
    addProperty('TImageBox', 'OnImgKeyDown', 'TImageBoxKeyEvent', @_LapeSimbaImageBox_OnImgKeyDown_Read, @_LapeSimbaImageBox_OnImgKeyDown_Write);
    addProperty('TImageBox', 'OnImgKeyUp', 'TImageBoxKeyEvent', @_LapeSimbaImageBox_OnImgKeyUp_Read, @_LapeSimbaImageBox_OnImgKeyUp_Write);

    addProperty('TImageBox', 'ShowScrollBars', 'Boolean', @_LapeSimbaImageBox_ShowScrollBars_Read, @_LapeSimbaImageBox_ShowScrollBars_Write);
    addProperty('TImageBox', 'ShowStatusBar', 'Boolean', @_LapeSimbaImageBox_ShowStatusBar_Read, @_LapeSimbaImageBox_ShowStatusBar_Write);

    addProperty('TImageBox', 'Status', 'String', @_LapeSimbaImageBox_Status_Read, @_LapeSimbaImageBox_Status_Write);
    addProperty('TImageBox', 'Background', 'TLazBitmap', @_LapeSimbaImageBox_Background_Read);

    addGlobalFunc('function TImageBox.FindDTM(DTM: TDTM): TPointArray', @_LapeSimbaImageBox_FindDTM);
    addGlobalFunc('function TImageBox.FindColor(Col: TColor; Tol: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray', @_LapeSimbaImageBox_FindColor);
    addGlobalFunc('function TImageBox.MatchColor(Col: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix', @_LapeSimbaImageBox_MatchColor);

    addGlobalFunc('procedure TImageBox.MoveTo(ImageXY: TPoint);', @_LapeSimbaImageBox_MoveTo);
    addGlobalFunc('function TImageBox.IsPointVisible(ImageXY: TPoint): Boolean;', @_LapeSimbaImageBox_IsPointVisible);
    addGlobalFunc('function TImageBox.MouseX: Integer;', @_LapeSimbaImageBox_MouseX);
    addGlobalFunc('function TImageBox.MouseY: Integer;', @_LapeSimbaImageBox_MouseY);
    addGlobalFunc('function TImageBox.MousePoint: TPoint;', @_LapeSimbaImageBox_MousePoint);

    addGlobalFunc('procedure TImageBox.SetBackground(Image: TImage)', @_LapeSimbaImageBox_SetBackground);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromFile(FileName: String)', @_LapeSimbaImageBox_SetBackgroundFromFile);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromWindow(Window: TWindowHandle)', @_LapeSimbaImageBox_SetBackgroundFromWindow);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromTarget(Target: TTarget; Bounds: TBox); overload', @_LapeSimbaImageBox_SetBackgroundFromTarget1);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromTarget(Target: TTarget); overload', @_LapeSimbaImageBox_SetBackgroundFromTarget2);

    addClassConstructor('TImageBox', '(Owner: TLazComponent)', @_LapeSimbaImageBox_Create);
  end;
end;

end.

