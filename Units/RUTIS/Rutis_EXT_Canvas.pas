Unit Rutis_EXT_Canvas;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls,
  Rutis_Stack, Rutis_Engine, Rutis_Defs;

Type
  TUniversalCanvas = Class(TControlCanvas)
  Protected
    fDCHandle  : HWND;
  Public
    Destructor Destroy; Override;
    Procedure ReleaseTarget;
    Procedure SetTargetDC(DC : HDC);
    Procedure SetTargetHandle(AHandle : HWND);
    Procedure SetTargetControl(AControl : TControl);
  End;

Var
  RC_Buffer          : TBitmap;
  RC_DrawCanvas      : TCanvas;
  RC_TargetCanvas    : TCanvas;
  RC_DoubleBuffered  : Boolean;
  RC_Width,
  RC_Height          : Integer;
  RC_BGColor         : TColor;


Procedure RC_SetTargetDC(DC : HDC);
Procedure RC_SetTargetHandle(Handle : HWND);
Procedure RC_SetTargetCanvas(Canvas : TCanvas);
Procedure RC_SetTargetControl(Control : TControl);
Procedure RC_Repaint;
Procedure RegisterEXTMethods(Engine : TRutisEngine);

Implementation

Var
  RC_DCCanvas  : TUniversalCanvas;

Type
  PControl = ^TControl;

 //==============================================================================
 //==============================================================================
 { TUniversalCanvas }

Destructor TUniversalCanvas.Destroy;
Begin
  ReleaseTarget;
  Inherited;
End;

Procedure TUniversalCanvas.ReleaseTarget;
Begin
  If HandleAllocated Then
  Begin
    If (fDCHandle <> 0) Then
      ReleaseDC(Handle, fDCHandle);
    Handle    := 0;
    fDCHandle := 0;
  End;
End;

Procedure TUniversalCanvas.SetTargetDC(DC : HDC);
Begin
  ReleaseTarget;
  If DC = 0 Then exit;
  Handle := DC;
End;

Procedure TUniversalCanvas.SetTargetHandle(AHandle : HWND);
Begin
  ReleaseTarget;
  //If AHandle = 0 then exit;
  fDCHandle := AHandle;
  Try
    Handle := GetDC(fDCHandle);
  Except
    Handle := 0;
    fDCHandle := 0;
  End;
End;

Procedure TUniversalCanvas.SetTargetControl(AControl : TControl);
Begin
  ReleaseTarget;
  If AControl <> nil Then
    Control := AControl;
End;

//==============================================================================
//==============================================================================

Procedure RC_SetTargetDC(DC : HDC);
Begin
  RC_DCCanvas.SetTargetDC(DC);
  RC_TargetCanvas := RC_DCCanvas;

  If RC_DoubleBuffered Then
    RC_DrawCanvas := RC_Buffer.Canvas
  Else
    RC_DrawCanvas := RC_TargetCanvas;
End;

Procedure RC_SetTargetHandle(Handle : HWND);
Begin
  RC_DCCanvas.SetTargetHandle(Handle);
  RC_TargetCanvas := RC_DCCanvas;

  If RC_DoubleBuffered Then
    RC_DrawCanvas := RC_Buffer.Canvas
  Else
    RC_DrawCanvas := RC_TargetCanvas;
End;

Procedure RC_SetTargetControl(Control : TControl);
Begin
  If (Control is TImage) Then
  Begin
    RC_TargetCanvas := TImage(Control).Canvas;
    RC_DCCanvas.ReleaseTarget;
  End
  Else
  Begin
    RC_DCCanvas.SetTargetControl(Control);
    RC_TargetCanvas := RC_DCCanvas;
  End;

  If RC_DoubleBuffered Then
    RC_DrawCanvas := RC_Buffer.Canvas
  Else
    RC_DrawCanvas := RC_TargetCanvas;
End;

Procedure RC_SetTargetCanvas(Canvas : TCanvas);
Begin
  RC_DCCanvas.ReleaseTarget;

  RC_TargetCanvas := Canvas;

  If RC_DoubleBuffered Then
    RC_DrawCanvas := RC_Buffer.Canvas
  Else
    RC_DrawCanvas := RC_TargetCanvas;
End;

Procedure RC_Repaint;
Begin
  If RC_DoubleBuffered Then
    RC_TargetCanvas.Draw(0, 0, RC_Buffer);
End;

//==============================================================================
//==============================================================================

Procedure _CanvasDC(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_SetTargetDC(PCardinal(Params^[0].Data)^);
End;

Procedure _CanvasHandle(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_SetTargetHandle(PCardinal(Params^[0].Data)^);
End;

Procedure _CanvasControl(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_SetTargetControl(PControl(Params^[0].Data)^);
End;

Procedure _CanvasDoubleBuffered(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DoubleBuffered := PBoolean(Params^[0].Data)^;
  If RC_DoubleBuffered Then
    RC_DrawCanvas := RC_Buffer.Canvas
  Else
    RC_DrawCanvas := RC_TargetCanvas;
End;

Procedure _CanvasClear(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  rect      : TRect;
  TmpColor  : TColor;
Begin
//  RC_BGColor := RC_DrawCanvas.Brush.Color;
//  If RC_DoubleBuffered Then
//  Begin
//    //RC_Buffer.Width := 0;
//    //RC_Buffer.Width := RC_Width;
//    rect := RC_Buffer.Canvas.ClipRect;
//    inc(rect.Right);
//    inc(rect.Bottom);

//    TmpColor := RC_Buffer.Canvas.Pen.Color;
//    RC_Buffer.Canvas.Pen.Color := RC_BGColor;

//    RC_Buffer.Canvas.FillRect(rect);

//    RC_Buffer.Canvas.Pen.Color := TmpColor;
//  End
//  Else
//  Begin
//    rect := RC_TargetCanvas.ClipRect;
//    inc(rect.Right);
//    inc(rect.Bottom);

//    RC_TargetCanvas.Brush.Color := RC_BGColor;
//    RC_TargetCanvas.Pen.Color   := RC_BGColor;
//    RC_TargetCanvas.FillRect(rect);
//  End;

  RC_BGColor := RC_DrawCanvas.Brush.Color;
  TmpColor   := RC_DrawCanvas.Pen.Color;

  rect := RC_DrawCanvas.ClipRect;
  Inc(rect.Right);
  Inc(rect.Bottom);

  RC_DrawCanvas.Pen.Color := RC_BGColor;

  RC_DrawCanvas.FillRect(rect);

  RC_DrawCanvas.Pen.Color := TmpColor;
End;

Procedure _CanvasSize(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  AColor  : TColor;
Begin
  RC_Width  := PInteger(Params^[0].Data)^;
  RC_Height := PInteger(Params^[1].Data)^;
  //CorrectBufferSize(RC_Width, RC_Height);
  If RC_DoubleBuffered Then
    With RC_Buffer Do
    Begin
      AColor := Canvas.Brush.Color;
      Canvas.Brush.Color := RC_BGColor;
      If (RC_Width <> Width) Then Width := RC_Width;
      If (RC_Height <> Height) Then Height := RC_Height;
      Canvas.Brush.Color := AColor;
    End;
End;

Procedure _CanvasDisplay(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  If RC_DoubleBuffered Then
    RC_TargetCanvas.Draw(0, 0, RC_Buffer);
End;

//==============================================================================

Procedure _BrushSolid(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Brush.Style := bsSolid;
End;

Procedure _BrushClear(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Brush.Style := bsClear;
End;

Procedure _PenColor(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Pen.Color := PColor(Params^[0].Data)^;
End;

Procedure _BrushColor(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Brush.Color := PColor(Params^[0].Data)^;
End;

Procedure _PenColorRGB(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Pen.Color := rgb(PByte(Params^[0].Data)^, PByte(Params^[1].Data)^, PByte(Params^[2].Data)^);
End;

Procedure _BrushColorRGB(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Brush.Color := rgb(PByte(Params^[0].Data)^, PByte(Params^[1].Data)^, PByte(Params^[2].Data)^);
End;

Procedure _PenWidth(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Pen.Width := PInteger(Params^[0].Data)^;
End;

Procedure _Rectangle(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var x1, y1, x2, y2  : Integer;
Begin
  x1 := PInteger(Params^[0].Data)^;
  y1 := PInteger(Params^[1].Data)^;
  x2 := PInteger(Params^[2].Data)^;
  y2 := PInteger(Params^[3].Data)^;
  //CorrectBufferSize(Max(x1, x2), Max(y1, y2));
  RC_DrawCanvas.Rectangle(x1, y1, x2, y2);
End;

Procedure _Ellipse(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var x1, y1, x2, y2  : Integer;
Begin
  x1 := PInteger(Params^[0].Data)^;
  y1 := PInteger(Params^[1].Data)^;
  x2 := PInteger(Params^[2].Data)^;
  y2 := PInteger(Params^[3].Data)^;
  //CorrectBufferSize(Max(x1, x2), Max(y1, y2));
  RC_DrawCanvas.Ellipse(x1, y1, x2, y2);
End;

Procedure _MoveTo(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var x1, y1  : Integer;
Begin
  x1 := PInteger(Params^[0].Data)^;
  y1 := PInteger(Params^[1].Data)^;
  //CorrectBufferSize(x1,y1);
  RC_DrawCanvas.MoveTo(x1, y1);
End;

Procedure _LineTo(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var x1, y1  : Integer;
Begin
  x1 := PInteger(Params^[0].Data)^;
  y1 := PInteger(Params^[1].Data)^;
  //CorrectBufferSize(x1,y1);
  RC_DrawCanvas.LineTo(x1, y1);
End;

Procedure _TextOut(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var x1, y1  : Integer;
Begin
  x1 := PInteger(Params^[0].Data)^;
  y1 := PInteger(Params^[1].Data)^;
  With RC_DrawCanvas Do
  Begin
  //CorrectBufferSize(x1 + TextHeight(Params^[2]), y1 + TextWidth(Params^[2]));
    RC_DrawCanvas.Font.Color := RC_DrawCanvas.Pen.Color;
    TextOut(x1, y1, PAnsiString(Params^[2].Data)^);
  End;
End;

Procedure _TextSize(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Font.Size := GetPInteger(Params^[0].Data);
End;

Procedure _GetPixel(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PCardinal(Result^.Data)^ := Cardinal(RC_DrawCanvas.Pixels[PInteger(Params^[0].Data)^,
    PInteger(Params^[1].Data)^]);
End;

Procedure _SetPixel(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  RC_DrawCanvas.Pixels[PInteger(Params^[0].Data)^,
    PInteger(Params^[1].Data)^] := TColor(PCardinal(Params^[2].Data)^);
End;

//==============================================================================
//==============================================================================

Procedure RegisterEXTMethods(Engine : TRutisEngine);
Begin
  OutputDebugString(PChar('Rutis_EXT_Canvas.RegisterEXTMethods  -  Registering RUTIS Canvas Support'));

  //==============================================================================
  //====================== CANVAS ================================================
  //==============================================================================

  Engine.RegExtMethod('CanvasControl',{$IfDef FPC}@{$EndIf}_CanvasControl, ['TControl'], '',
    'Set the Control for the Canvas');
  Engine.RegExtMethod('CanvasHandle',{$IfDef FPC}@{$EndIf}_CanvasHandle, ['HWND'], '',
    'Set the Handle for the Canvas' + sLineBreak +
    'The needed Device Context will be retrived automatically');
  Engine.RegExtMethod('CanvasDC',{$IfDef FPC}@{$EndIf}_CanvasDC, ['HDC'], '',
    'Set the DeviceContext for the Canvas' + sLineBreak +
    'You need to get the DC with GetDC first' + sLineBreak +
    'CanvasDC should normally not be used - use CanvasHandle instead');

  Engine.RegExtMethod('CanvasSize',{$IfDef FPC}@{$EndIf}_CanvasSize, ['Integer', 'Integer'], '',
    'Set up the Buffer-Size' + sLineBreak +
    'Only needed when using DoubleBuffered-ON');
  Engine.RegExtMethod('CanvasDisplay',{$IfDef FPC}@{$EndIf}_CanvasDisplay, [], '',
    'Displays the Buffer' + sLineBreak +
    'Only needed when using DoubleBuffered-ON');
  Engine.RegExtMethod('CanvasClear',{$IfDef FPC}@{$EndIf}_CanvasClear, [], '',
    'Clears the painted content');
  Engine.RegExtMethod('CanvasDoubleBuffered',{$IfDef FPC}@{$EndIf}_CanvasDoubleBuffered, ['Boolean'], '',
    'If set, all content will be painted into a separate Buffer' + sLineBreak +
    'You will need to set up the Buffer with CanvasSize and CanvasClear first and' + sLineBreak +
    'you have to call CanvasDisplay to show the painted content');

  Engine.RegExtMethod('MoveTo',{$IfDef FPC}@{$EndIf}_Moveto, ['Integer', 'Integer'], '');
  Engine.RegExtMethod('LineTo',{$IfDef FPC}@{$EndIf}_Lineto, ['Integer', 'Integer'], '');
  Engine.RegExtMethod('Rectangle',{$IfDef FPC}@{$EndIf}_Rectangle, ['Integer', 'Integer', 'Integer', 'Integer'], '');
  Engine.RegExtMethod('Ellipse',{$IfDef FPC}@{$EndIf}_Ellipse, ['Integer', 'Integer', 'Integer', 'Integer'], '');
  Engine.RegExtMethod('TextOut',{$IfDef FPC}@{$EndIf}_TextOut, ['Integer', 'Integer', 'String'], '');
  Engine.RegExtMethod('TextSize',{$IfDef FPC}@{$EndIf}_TextSize, ['Integer'], '');
  Engine.RegExtMethod('PenColorRGB',{$IfDef FPC}@{$EndIf}_PenColorRGB, ['Byte', 'Byte', 'Byte'], '');
  Engine.RegExtMethod('BrushColorRGB',{$IfDef FPC}@{$EndIf}_BrushColorRGB, ['Byte', 'Byte', 'Byte'], '');
  Engine.RegExtMethod('PenColor',{$IfDef FPC}@{$EndIf}_PenColor, ['Integer'], '');
  Engine.RegExtMethod('BrushColor',{$IfDef FPC}@{$EndIf}_BrushColor, ['Integer'], '');
  Engine.RegExtMethod('BrushSolid',{$IfDef FPC}@{$EndIf}_BrushSolid, [], '');
  Engine.RegExtMethod('BrushClear',{$IfDef FPC}@{$EndIf}_BrushClear, [], '');
  Engine.RegExtMethod('PenWidth',{$IfDef FPC}@{$EndIf}_PenWidth, ['Integer'], '');

  Engine.RegExtMethod('GetPixel',{$IfDef FPC}@{$EndIf}_GetPixel, ['Integer', 'Integer'], 'Integer');
  Engine.RegExtMethod('SetPixel',{$IfDef FPC}@{$EndIf}_SetPixel, ['Integer', 'Integer', 'Integer'], '');


  OutputDebugString(PChar('Rutis_EXT_Canvas.RegisterEXTMethods  -  Successfully registered RUTIS Canvas Support'));
End;

//==============================================================================
//==============================================================================

Initialization
  RC_Buffer   := TBitmap.Create;
  RC_DCCanvas := TUniversalCanvas.Create;

  RC_DoubleBuffered := True;
  RC_TargetCanvas   := RC_Buffer.Canvas;
Finalization
  RC_DCCanvas.Free;
  RC_Buffer.Free;
End.

