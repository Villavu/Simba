unit simba.import_imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics,
  simba.base, simba.script, simba.script_objectutil;

procedure ImportSimbaImageBox(Script: TSimbaScript);

implementation

uses
  lptypes, ffi,
  simba.component_imagebox,
  simba.component_imageboxcanvas,
  simba.image_textdrawer,
  simba.dtm,
  simba.colormath,
  simba.target,
  simba.vartype_quad;

type
  PComponent = ^TComponent;
  PPanel = ^TPanel;
  PBitmap = ^TBitmap;
  PSimbaImageBox = ^TSimbaImageBox;
  PSimbaImageBoxCanvas = ^TSimbaImageBoxCanvas;
  PQuad = ^TQuad;

procedure _LapeImageBox_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImageBox(Params^[0])^.FindDTM(PDTM(Params^[1])^);
end;

procedure _LapeImageBox_FindColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImageBox(Params^[0])^.FindColor(PColorTolerance(Params^[1])^);
end;

procedure _LapeImageBox_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaImageBox(Params^[0])^.MatchColor(PColorTolerance(Params^[1])^);
end;

procedure _LapeImageBox_MoveTo(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.MoveTo(PPoint(Params^[1])^);
end;

procedure _LapeImageBox_IsPointVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.IsPointVisible(PPoint(Params^[1])^);
end;

procedure _LapeImageBox_MouseX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.MouseX;
end;

procedure _LapeImageBox_MouseY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.MouseY;
end;

procedure _LapeImageBox_MouseXY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImageBox(Params^[0])^.MouseXY;
end;

procedure _LapeImageBox_SetImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.SetImage(PLapeObjectImage(Params^[1])^^, False);
end;

procedure _LapeImageBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Result)^ := TSimbaImageBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeImageBox_Status_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImageBox(Params^[0])^.Status;
end;

procedure _LapeImageBox_Status_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Status := PString(Params^[1])^;
end;

procedure _LapeImageBox_Background_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSimbaImageBox(Params^[0])^.Background;
end;

procedure _LapeImageBox_Zoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.Zoom;
end;

procedure _LapeImageBox_Zoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Zoom := PInteger(Params^[1])^;
end;

procedure _LapeImageBox_AllowZoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.AllowZoom;
end;

procedure _LapeImageBox_AllowZoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.AllowZoom := PBoolean(Params^[1])^;
end;

procedure _LapeImageBox_OnImgPaint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxPaintEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgPaint;
end;

procedure _LapeImageBox_OnImgPaint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgPaint := TImageBoxPaintEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseEnter;
end;

procedure _LapeImageBox_OnImgMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseEnter := TImageBoxEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseLeave;
end;

procedure _LapeImageBox_OnImgMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseLeave := TImageBoxEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxMouseEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseDown;
end;

procedure _LapeImageBox_OnImgMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseDown := TImageBoxMouseEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxMouseEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseUp;
end;

procedure _LapeImageBox_OnImgMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseUp := TImageBoxMouseEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxMouseMoveEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgMouseMove;
end;

procedure _LapeImageBox_OnImgMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgMouseMove := TImageBoxMouseMoveEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxClickEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgClick;
end;

procedure _LapeImageBox_OnImgClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgClick := TImageBoxClickEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgDoubleClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxClickEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgDoubleClick;
end;

procedure _LapeImageBox_OnImgDoubleClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgDoubleClick := TImageBoxClickEvent(Params^[1]^);
end;

procedure _LapeImageBox_OnImgKeyDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxKeyEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgKeyDown;
end;

procedure _LapeImageBox_OnImgKeyDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnImgKeyDown := TImageBoxKeyEvent(Params^[1]^);
end;

procedure _LapeImageBox_ShowScrollBars_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.ShowScrollbars;
end;

procedure _LapeImageBox_ShowScrollBars_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.ShowScrollbars := PBoolean(Params^[1])^;
end;

procedure _LapeImageBox_ShowStatusBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.ShowStatusBar;
end;

procedure _LapeImageBox_ShowStatusBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.ShowStatusBar := PBoolean(Params^[1])^;
end;

procedure _LapeImageBox_AllowMoving_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.AllowMoving;
end;

procedure _LapeImageBox_AllowMoving_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.AllowMoving := PBoolean(Params^[1])^;
end;

procedure _LapeImageBox_OnImgKeyUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageBoxKeyEvent(Result^) := PSimbaImageBox(Params^[0])^.OnImgKeyUp;
end;

procedure _LapeImageBox_UserPanel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Result)^ := PSimbaImageBox(Params^[0])^.UserPanel;
end;

procedure _LapeImageBox_OnImgKeyUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
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

procedure _LapeImageBoxCanvas_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawBoxFilled1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawBoxFilled2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawPoly(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPoly(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeImageBoxCanvas_DrawPolyFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPolyFilled(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawPoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPoint(PPoint(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawPoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawPoints(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeImageBoxCanvas_DrawHeatmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawHeatmap(PSingleMatrix(Params^[1])^);
end;

procedure _LapeImageBoxCanvas_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxCanvas(Params^[0])^.DrawImage(PLapeObjectImage(Params^[1])^^, PPoint(Params^[2])^);
end;

procedure ImportSimbaImageBox(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addGlobalType('type TBaseClass', 'TImageBoxCanvas');

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
    addGlobalFunc('procedure TImageBoxCanvas.DrawLine(Start, Stop: TPoint; Color: TColor);', @_LapeImageBoxCanvas_DrawLine);
    addGlobalFunc('procedure TImageBoxCanvas.DrawCross(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeImageBoxCanvas_DrawCross);
    addGlobalFunc('procedure TImageBoxCanvas.DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);', @_LapeImageBoxCanvas_DrawCrossArray);
    addGlobalFunc('procedure TImageBoxCanvas.DrawBox(Box: TBox; Color: TColor);', @_LapeImageBoxCanvas_DrawBox);
    addGlobalFunc('procedure TImageBoxCanvas.DrawBoxFilled(Box: TBox; Color: TColor); overload;', @_LapeImageBoxCanvas_DrawBoxFilled1);
    addGlobalFunc('procedure TImageBoxCanvas.DrawBoxFilled(Box: TBox; Color: TColor; Transparency: Single); overload;', @_LapeImageBoxCanvas_DrawBoxFilled2);
    addGlobalFunc('procedure TImageBoxCanvas.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeImageBoxCanvas_DrawCircle);
    addGlobalFunc('procedure TImageBoxCanvas.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeImageBoxCanvas_DrawCircleFilled);
    addGlobalFunc('procedure TImageBoxCanvas.DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);', @_LapeImageBoxCanvas_DrawPoly);
    addGlobalFunc('procedure TImageBoxCanvas.DrawPolyFilled(Poly: TPointArray; Color: TColor);', @_LapeImageBoxCanvas_DrawPolyFilled);
    addGlobalFunc('procedure TImageBoxCanvas.DrawQuad(Quad: TQuad; Color: TColor);', @_LapeImageBoxCanvas_DrawQuad);
    addGlobalFunc('procedure TImageBoxCanvas.DrawQuadFilled(Quad: TQuad; Color: TColor);', @_LapeImageBoxCanvas_DrawQuadFilled);
    addGlobalFunc('procedure TImageBoxCanvas.DrawPoint(Point: TPoint; Color: TColor);', @_LapeImageBoxCanvas_DrawPoint);
    addGlobalFunc('procedure TImageBoxCanvas.DrawPoints(TPA: TPointArray; Color: TColor);', @_LapeImageBoxCanvas_DrawPoints);
    addGlobalFunc('procedure TImageBoxCanvas.DrawHeatmap(Mat: TSingleMatrix);', @_LapeImageBoxCanvas_DrawHeatmap);
    addGlobalFunc('procedure TImageBoxCanvas.DrawImage(Image: TImage; Point: TPoint);', @_LapeImageBoxCanvas_DrawImage);

    addClass('TImageBox', 'TLazCustomControl', TCustomControl);

    addGlobalType('procedure(Sender: TImageBox; Canvas: TImageBoxCanvas; R: TLazRect) of object', 'TImageBoxPaintEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox) of object', 'TImageBoxEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; X, Y: Integer) of object', 'TImageBoxClickEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; var Key: UInt16; Shift: ELazShiftStates) of object', 'TImageBoxKeyEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; Button: ELazMouseButton; Shift: ELazShiftStates; X, Y: Integer) of object', 'TImageBoxMouseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TImageBox; Shift: ELazShiftStates; X, Y: Integer) of object', 'TImageBoxMouseMoveEvent', FFI_DEFAULT_ABI);

    addProperty('TImageBox', 'OnImgPaint', 'TImageBoxPaintEvent', @_LapeImageBox_OnImgPaint_Read, @_LapeImageBox_OnImgPaint_Write);
    addProperty('TImageBox', 'OnImgMouseEnter', 'TImageBoxEvent', @_LapeImageBox_OnImgMouseEnter_Read, @_LapeImageBox_OnImgMouseEnter_Write);
    addProperty('TImageBox', 'OnImgMouseLeave', 'TImageBoxEvent', @_LapeImageBox_OnImgMouseLeave_Read, @_LapeImageBox_OnImgMouseLeave_Write);
    addProperty('TImageBox', 'OnImgMouseDown', 'TImageBoxMouseEvent', @_LapeImageBox_OnImgMouseDown_Read, @_LapeImageBox_OnImgMouseDown_Write);
    addProperty('TImageBox', 'OnImgMouseUp', 'TImageBoxMouseEvent', @_LapeImageBox_OnImgMouseUp_Read, @_LapeImageBox_OnImgMouseUp_Write);
    addProperty('TImageBox', 'OnImgMouseMove', 'TImageBoxMouseMoveEvent', @_LapeImageBox_OnImgMouseMove_Read, @_LapeImageBox_OnImgMouseMove_Write);
    addProperty('TImageBox', 'OnImgClick', 'TImageBoxClickEvent', @_LapeImageBox_OnImgClick_Read, @_LapeImageBox_OnImgClick_Write);
    addProperty('TImageBox', 'OnImgDoubleClick', 'TImageBoxClickEvent', @_LapeImageBox_OnImgDoubleClick_Read, @_LapeImageBox_OnImgDoubleClick_Write);
    addProperty('TImageBox', 'OnImgKeyDown', 'TImageBoxKeyEvent', @_LapeImageBox_OnImgKeyDown_Read, @_LapeImageBox_OnImgKeyDown_Write);
    addProperty('TImageBox', 'OnImgKeyUp', 'TImageBoxKeyEvent', @_LapeImageBox_OnImgKeyUp_Read, @_LapeImageBox_OnImgKeyUp_Write);

    addProperty('TImageBox', 'UserPanel', 'TLazPanel', @_LapeImageBox_UserPanel_Read);
    addProperty('TImageBox', 'ShowScrollBars', 'Boolean', @_LapeImageBox_ShowScrollBars_Read, @_LapeImageBox_ShowScrollBars_Write);
    addProperty('TImageBox', 'ShowStatusBar', 'Boolean', @_LapeImageBox_ShowStatusBar_Read, @_LapeImageBox_ShowStatusBar_Write);
    addProperty('TImageBox', 'AllowMoving', 'Boolean', @_LapeImageBox_AllowMoving_Read, @_LapeImageBox_AllowMoving_Write);
    addProperty('TImageBox', 'AllowZoom', 'Boolean', @_LapeImageBox_AllowZoom_Read, @_LapeImageBox_AllowZoom_Write);
    addProperty('TImageBox', 'Status', 'String', @_LapeImageBox_Status_Read, @_LapeImageBox_Status_Write);
    addProperty('TImageBox', 'Background', 'TLazBitmap', @_LapeImageBox_Background_Read);
    addProperty('TImageBox', 'Zoom', 'Integer', @_LapeImageBox_Zoom_Read, @_LapeImageBox_Zoom_Write);
    addProperty('TImageBox', 'MouseX', 'Integer', @_LapeImageBox_MouseX);
    addProperty('TImageBox', 'MouseY', 'Integer', @_LapeImageBox_MouseY);
    addProperty('TImageBox', 'MouseXY', 'TPoint', @_LapeImageBox_MouseXY);

    addGlobalFunc('function TImageBox.FindDTM(DTM: TDTM): TPointArray', @_LapeImageBox_FindDTM);
    addGlobalFunc('function TImageBox.FindColor(ColorTolerance: TColorTolerance): TPointArray', @_LapeImageBox_FindColor);
    addGlobalFunc('function TImageBox.MatchColor(ColorTolerance: TColorTolerance): TSingleMatrix', @_LapeImageBox_MatchColor);
    addGlobalFunc('procedure TImageBox.SetImage(Image: TImage)', @_LapeImageBox_SetImage);
    addGlobalFunc('procedure TImageBox.MoveTo(ImageXY: TPoint);', @_LapeImageBox_MoveTo);
    addGlobalFunc('function TImageBox.IsPointVisible(ImageXY: TPoint): Boolean;', @_LapeImageBox_IsPointVisible);

    addClassConstructor('TImageBox', '(Owner: TLazComponent)', @_LapeImageBox_Create);
  end;
end;

end.

