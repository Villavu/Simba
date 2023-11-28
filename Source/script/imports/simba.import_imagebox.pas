unit simba.import_imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics,
  simba.mufasatypes, simba.script_compiler;

procedure ImportSimbaImageBox(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi,
  simba.imagebox, simba.imagebox_image, simba.image, simba.dtm, simba.colormath,
  simba.target;

type
  PComponent = ^TComponent;
  PCursor = ^TCursor;
  PNotifyEvent = ^TNotifyEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PMouseEvent = ^TMouseEvent;
  PStatusBar = ^TStatusBar;
  PStatusPanel = ^TStatusPanel;
  PBitmap = ^TBitmap;

procedure _LapeSimbaImageBoxBitmap_DrawLineGap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawLineGap(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^, PColor(Params^[4])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawCrossHair(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawCrosshair(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawPoly(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawPoly(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawPoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawPoint(PPoint(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawPoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawPoints(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawBoxTransparent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawBoxTransparent(PBox(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawEllipse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawEllipse(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PColor(Params^[4])^);
end;

procedure _LapeSimbaImageBoxBitmap_DrawHeatmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxBitmap(Params^[0])^.DrawHeatmap(PSingleMatrix(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Zoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImageBox(Params^[0])^.Zoom;
end;

procedure _LapeSimbaImageBox_Zoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Zoom := PSingle(Params^[1])^;
end;

procedure _LapeSimbaImageBox_StatusBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Result)^ := PSimbaImageBox(Params^[0])^.StatusBar;
end;

procedure _LapeSimbaImageBox_StatusPanel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Result)^ := PSimbaImageBox(Params^[0])^.StatusPanel;
end;

procedure _LapeSimbaImageBox_OnPaintArea_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxPaintAreaEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnPaintArea;
end;

procedure _LapeSimbaImageBox_OnPaintArea_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnPaintArea := PSimbaImageBoxPaintAreaEvent(Params^[1])^;
end;

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
  PSimbaImageBox(Params^[0])^.MoveTo(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaImageBox_IsVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.IsVisible(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaImageBox_Paint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Paint();
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

procedure _LapeSimbaImageBox_Cursor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCursor(Result)^ := PSimbaImageBox(Params^[0])^.Cursor;
end;

procedure _LapeSimbaImageBox_Cursor_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseMoveEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseMove;
end;

procedure _LapeSimbaImageBox_OnMouseMove_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseDown;
end;

procedure _LapeSimbaImageBox_OnMouseDown_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseUp;
end;

procedure _LapeSimbaImageBox_OnMouseUp_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseEnter;
end;

procedure _LapeSimbaImageBox_OnMouseEnter_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseLeave;
end;

procedure _LapeSimbaImageBox_OnMouseLeave_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnDblClick;
end;

procedure _LapeSimbaImageBox_OnDblClick_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_MousePoint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImageBox(Params^[0])^.MousePoint;
end;

procedure _LapeSimbaImageBox_Background_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSimbaImageBox(Params^[0])^.Background;
end;

procedure ImportSimbaImageBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TImageBoxImage');

    addGlobalFunc('procedure TImageBoxImage.DrawLineGap(Start, Stop: TPoint; Gap: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawLineGap);
    addGlobalFunc('procedure TImageBoxImage.DrawLine(Start, Stop: TPoint; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawLine);
    addGlobalFunc('procedure TImageBoxImage.DrawCross(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawCross);
    addGlobalFunc('procedure TImageBoxImage.DrawCrossArray(Centers: TPointArray; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawCrossArray);
    addGlobalFunc('procedure TImageBoxImage.DrawCrossHair(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawCrossHair);
    addGlobalFunc('procedure TImageBoxImage.DrawBox(Box: TBox; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawBox);
    addGlobalFunc('procedure TImageBoxImage.DrawBoxFilled(Box: TBox; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawBoxFilled);
    addGlobalFunc('procedure TImageBoxImage.DrawPoly(Poly: TPointArray; Connect: Boolean; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawPoly);
    addGlobalFunc('procedure TImageBoxImage.DrawPoint(P: TPoint; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawPoint);
    addGlobalFunc('procedure TImageBoxImage.DrawPoints(TPA: TPointArray; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawPoints);
    addGlobalFunc('procedure TImageBoxImage.DrawCircle(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawCircle);
    addGlobalFunc('procedure TImageBoxImage.DrawCircleFilled(Center: TPoint; Radius: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawCircleFilled);
    addGlobalFunc('procedure TImageBoxImage.DrawBoxTransparent(Box: TBox; Color: TColor; Transparency: Single);', @_LapeSimbaImageBoxBitmap_DrawBoxTransparent);
    addGlobalFunc('procedure TImageBoxImage.DrawEllipse(Center: TPoint; RadiusX, RadiusY: Integer; Color: TColor);', @_LapeSimbaImageBoxBitmap_DrawEllipse);
    addGlobalFunc('procedure TImageBoxImage.DrawHeatmap(const Mat: TSingleMatrix);', @_LapeSimbaImageBoxBitmap_DrawHeatmap);

    addGlobalType('procedure(Sender: TObject; Bitmap: TImageBoxImage; Rect: TLazRect) of object', 'TImageBoxPaintAreaEvent', FFI_DEFAULT_ABI);

    addClass('TImageBox', 'TLazWinControl');

    addClassVar('TImageBox', 'MousePoint', 'TPoint', @_LapeSimbaImageBox_MousePoint_Read);
    addClassVar('TImageBox', 'Background', 'TLazBitmap', @_LapeSimbaImageBox_Background_Read);
    addClassVar('TImageBox', 'Zoom', 'Single', @_LapeSimbaImageBox_Zoom_Read, @_LapeSimbaImageBox_Zoom_Write);
    addClassVar('TImageBox', 'StatusBar', 'TLazStatusBar', @_LapeSimbaImageBox_StatusBar_Read);
    addClassVar('TImageBox', 'StatusPanel', 'TLazStatusPanel', @_LapeSimbaImageBox_StatusPanel_Read);
    addClassVar('TImageBox', 'OnMouseMove', 'TLazMouseMoveEvent', @_LapeSimbaImageBox_OnMouseMove_Read, @_LapeSimbaImageBox_OnMouseMove_Write);
    addClassVar('TImageBox', 'OnMouseDown', 'TLazMouseEvent', @_LapeSimbaImageBox_OnMouseDown_Read, @_LapeSimbaImageBox_OnMouseDown_Write);
    addClassVar('TImageBox', 'OnMouseUp', 'TLazMouseEvent', @_LapeSimbaImageBox_OnMouseUp_Read, @_LapeSimbaImageBox_OnMouseUp_Write);
    addClassVar('TImageBox', 'OnMouseLeave', 'TLazNotifyEvent', @_LapeSimbaImageBox_OnMouseLeave_Read, @_LapeSimbaImageBox_OnMouseLeave_Write);
    addClassVar('TImageBox', 'OnMouseEnter', 'TLazNotifyEvent', @_LapeSimbaImageBox_OnMouseEnter_Read, @_LapeSimbaImageBox_OnMouseEnter_Write);
    addClassVar('TImageBox', 'OnDblClick', 'TLazNotifyEvent', @_LapeSimbaImageBox_OnDblClick_Read, @_LapeSimbaImageBox_OnDblClick_Write);
    addClassVar('TImageBox', 'OnPaintArea', 'TImageBoxPaintAreaEvent', @_LapeSimbaImageBox_OnPaintArea_Read, @_LapeSimbaImageBox_OnPaintArea_Write);

    addGlobalFunc('function TImageBox.FindDTM(DTM: TDTM): TPointArray', @_LapeSimbaImageBox_FindDTM);
    addGlobalFunc('function TImageBox.FindColor(Col: TColor; Tol: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray', @_LapeSimbaImageBox_FindColor);
    addGlobalFunc('function TImageBox.MatchColor(Col: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TSingleMatrix', @_LapeSimbaImageBox_MatchColor);

    addGlobalFunc('procedure TImageBox.MoveTo(X, Y: Integer);', @_LapeSimbaImageBox_MoveTo);
    addGlobalFunc('function TImageBox.IsVisible(X, Y: Integer): Boolean;', @_LapeSimbaImageBox_IsVisible);
    addGlobalFunc('procedure TImageBox.Paint;', @_LapeSimbaImageBox_Paint);

    addGlobalFunc('procedure TImageBox.SetBackground(Image: TImage)', @_LapeSimbaImageBox_SetBackground);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromFile(FileName: String)', @_LapeSimbaImageBox_SetBackgroundFromFile);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromWindow(Window: TWindowHandle)', @_LapeSimbaImageBox_SetBackgroundFromWindow);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromTarget(Target: TTarget; Bounds: TBox); overload', @_LapeSimbaImageBox_SetBackgroundFromTarget1);
    addGlobalFunc('procedure TImageBox.SetBackgroundFromTarget(Target: TTarget); overload', @_LapeSimbaImageBox_SetBackgroundFromTarget2);

    addClassConstructor('TImageBox', '(Owner: TLazComponent)', @_LapeSimbaImageBox_Create);
  end;
end;

end.

