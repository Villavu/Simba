unit simba.import_imagebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportSimbaImageBox(Script: TSimbaScript);

implementation

uses
  Controls, ExtCtrls, lptypes, ffi,
  simba.script_objectutil,
  simba.component_imagebox,
  simba.image;

type
  PComponent = ^TComponent;
  PPanel = ^TPanel;
  PSimbaImageBox = ^TSimbaImageBox;
  PSimbaImageBoxLayer = ^TSimbaImageBoxLayer;

procedure _LapeImageBox_MoveTo(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.MoveTo(PPoint(Params^[1])^);
end;

procedure _LapeImageBox_IsPointVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.IsPointVisible(PPoint(Params^[1])^);
end;

procedure _LapeImageBox_MouseXY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImageBox(Params^[0])^.MouseXY;
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
  // the box's own image, not the script's to free
  PLapeObject(Result)^ := LapeObjectAlloc(PSimbaImageBox(Params^[0])^.Background, False);
end;

procedure _LapeImageBox_Background_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
var
  Box: TSimbaImageBox;
  Img: TSimbaImage;
begin
  if (PLapeObjectImage(Params^[1])^ = nil) then
    SimbaException('TImageBox.Background cannot be nil');

  Box := PSimbaImageBox(Params^[0])^;
  Img := PLapeObjectImage(Params^[1])^^;

  // copied into the box's own image, so a Background a script is holding stays live
  Box.Background.FromData(Img.Data, Img.Width, Img.Width, Img.Height);
  Box.BackgroundChanged();
end;

procedure _LapeImageBox_Zoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.Zoom;
end;

procedure _LapeImageBox_Zoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.Zoom := PInteger(Params^[1])^;
end;

procedure _LapeImageBox_MinZoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.MinZoom;
end;

procedure _LapeImageBox_MinZoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.MinZoom := PInteger(Params^[1])^;
end;

procedure _LapeImageBox_MaxZoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.MaxZoom;
end;

procedure _LapeImageBox_MaxZoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.MaxZoom := PInteger(Params^[1])^;
end;


procedure _LapeImageBox_AllowUserZoom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.AllowUserZoom;
end;

procedure _LapeImageBox_AllowUserZoom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Params^[0])^.AllowUserZoom := PBoolean(Params^[1])^;
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


procedure _LapeImageBoxLayer_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxLayer(Result)^ := TSimbaImageBoxLayer.Create(PSimbaImageBox(Params^[0])^);
end;

procedure _LapeImageBoxLayer_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxLayer(Params^[0])^.Free();
end;

procedure _LapeImageBoxLayer_Visible_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageBoxLayer(Params^[0])^.Visible;
end;

procedure _LapeImageBoxLayer_Visible_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxLayer(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeImageBoxLayer_Opacity_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PSimbaImageBoxLayer(Params^[0])^.Opacity;
end;

procedure _LapeImageBoxLayer_Opacity_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxLayer(Params^[0])^.Opacity := PByte(Params^[1])^;
end;

procedure _LapeImageBoxLayer_Priority_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBoxLayer(Params^[0])^.Priority;
end;

procedure _LapeImageBoxLayer_Priority_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxLayer(Params^[0])^.Priority := PInteger(Params^[1])^;
end;

procedure _LapeImageBox_LayerCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageBox(Params^[0])^.LayerCount;
end;

procedure _LapeImageBox_Layers_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBoxLayer(Result)^ := PSimbaImageBox(Params^[0])^.Layers[PInteger(Params^[1])^];
end;

procedure ImportSimbaImageBox(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin




    addClass('TImageBox', 'TLazCustomControl', TCustomControl);

    addGlobalType('procedure(Sender: TImageBox; Canvas: TCanvas; R: TLazRect) of object', 'TImageBoxPaintEvent', FFI_DEFAULT_ABI);
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
    addProperty('TImageBox', 'AllowUserZoom', 'Boolean', @_LapeImageBox_AllowUserZoom_Read, @_LapeImageBox_AllowUserZoom_Write);
    addProperty('TImageBox', 'Status', 'String', @_LapeImageBox_Status_Read, @_LapeImageBox_Status_Write);
    addProperty('TImageBox', 'Background', 'TImage', @_LapeImageBox_Background_Read, @_LapeImageBox_Background_Write);

    addGlobalType('type TCanvas', 'TImageBoxLayer');
    addGlobalFunc('function TImageBoxLayer.Create(ImageBox: TImageBox): TImageBoxLayer; static;', @_LapeImageBoxLayer_Create);
    addGlobalFunc('procedure TImageBoxLayer.Free;', @_LapeImageBoxLayer_Free);
    addProperty('TImageBoxLayer', 'Visible', 'Boolean', @_LapeImageBoxLayer_Visible_Read, @_LapeImageBoxLayer_Visible_Write);
    addProperty('TImageBoxLayer', 'Priority', 'Integer', @_LapeImageBoxLayer_Priority_Read, @_LapeImageBoxLayer_Priority_Write);
    addProperty('TImageBoxLayer', 'Opacity', 'Byte', @_LapeImageBoxLayer_Opacity_Read, @_LapeImageBoxLayer_Opacity_Write);

    addProperty('TImageBox', 'LayerCount', 'Integer', @_LapeImageBox_LayerCount_Read);
    addPropertyIndexed('TImageBox', 'Layers', 'Index: Integer', 'TImageBoxLayer', @_LapeImageBox_Layers_Read);
    addProperty('TImageBox', 'Zoom', 'Integer', @_LapeImageBox_Zoom_Read, @_LapeImageBox_Zoom_Write);
    addProperty('TImageBox', 'MinZoom', 'Integer', @_LapeImageBox_MinZoom_Read, @_LapeImageBox_MinZoom_Write);
    addProperty('TImageBox', 'MaxZoom', 'Integer', @_LapeImageBox_MaxZoom_Read, @_LapeImageBox_MaxZoom_Write);
    addProperty('TImageBox', 'MouseXY', 'TPoint', @_LapeImageBox_MouseXY);

    addGlobalFunc('procedure TImageBox.MoveTo(ImageXY: TPoint);', @_LapeImageBox_MoveTo);
    addGlobalFunc('function TImageBox.IsPointVisible(ImageXY: TPoint): Boolean;', @_LapeImageBox_IsPointVisible);

    addClassConstructor('TImageBox', '(Owner: TLazComponent)', @_LapeImageBox_Create);
  end;
end;

end.

