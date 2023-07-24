unit simba.import_lcl_extctrls;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, controls, extctrls, graphics, customtimer, lptypes, ffi,
  simba.script_compiler;

type
  PBrush = ^TBrush;
  PPen = ^TPen;
  PAlignment = ^TAlignment;
  PNotifyEvent = ^TNotifyEvent;
  PBevelWidth = ^TBevelWidth;
  PCustomImage = ^TCustomImage;
  PCustomPanel = ^TCustomPanel;
  PCustomTimer = ^TCustomTimer;
  PImage = ^TImage;
  PPanel = ^TPanel;
  PPanelBevel = ^TPanelBevel;
  PTimer = ^TTimer;
  PComponent = ^TComponent;
  PCanvas = ^TCanvas;
  PRect = ^TRect;
  PPicture = ^TPicture;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;

procedure _LapeCustomTimer_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^ := TCustomTimer.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomTimer(Params^[0])^.Enabled;
end;

procedure _LapeCustomTimer_Enabled_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCardinal(Result)^ := PCustomTimer(Params^[0])^.Interval;
end;

procedure _LapeCustomTimer_Interval_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^.Interval := PCardinal(Params^[1])^;
end;

procedure _LapeCustomTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnTimer;
end;

procedure _LapeCustomTimer_OnTimer_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTimer_OnStartTimer_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStartTimer;
end;

procedure _LapeCustomTimer_OnStartTimer_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^.OnStartTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTimer_OnStopTimer_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStopTimer;
end;

procedure _LapeCustomTimer_OnStopTimer_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^.OnStopTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTimer_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Params^[0])^.Free();
end;

procedure _LapeTimer_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^ := TTimer.Create(PComponent(Params^[1])^);
end;

procedure _LapeTimer_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^.Free();
end;

procedure _LapeCustomImage_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^ := TCustomImage.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomImage_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PCustomImage(Params^[0])^.Canvas;
end;

procedure _LapeCustomImage_DestRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PCustomImage(Params^[0])^.DestRect();
end;

procedure _LapeCustomImage_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Center;
end;

procedure _LapeCustomImage_Center_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.Center := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_Picture_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Result)^ := PCustomImage(Params^[0])^.Picture;
end;

procedure _LapeCustomImage_Picture_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.Picture := PPicture(Params^[1])^;
end;

procedure _LapeCustomImage_Stretch_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Stretch;
end;

procedure _LapeCustomImage_Stretch_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.Stretch := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Transparent;
end;

procedure _LapeCustomImage_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_Proportional_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Proportional;
end;

procedure _LapeCustomImage_Proportional_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.Proportional := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_OnPictureChanged_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomImage(Params^[0])^.OnPictureChanged;
end;

procedure _LapeCustomImage_OnPictureChanged_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.OnPictureChanged := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomImage_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.Free;
end;

procedure _LapeCustomImage_OnMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeCustomImage_OnMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeImage_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^ := TImage.Create(PComponent(Params^[1])^);
end;

procedure _LapeImage_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.Free();
end;

procedure _LapeImage_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseEnter;
end;

procedure _LapeImage_OnMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseLeave;
end;

procedure _LapeImage_OnMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseMoveEvent(Result)^ := PImage(Params^[0])^.OnMouseMove;
end;

procedure _LapeImage_OnMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeCustomPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAlignment(Result)^ := PCustomPanel(Params^[0])^.Alignment;
end;

procedure _LapeCustomPanel_Alignment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelInner_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelInner;
end;

procedure _LapeCustomPanel_BevelInner_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.BevelInner := PPanelBevel(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelOuter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelOuter;
end;

procedure _LapeCustomPanel_BevelOuter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.BevelOuter := PPanelBevel(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBevelWidth(Result)^ := PCustomPanel(Params^[0])^.BevelWidth;
end;

procedure _LapeCustomPanel_BevelWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.BevelWidth := PBevelWidth(Params^[1])^;
end;

procedure _LapeCustomPanel_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^ := TCustomPanel.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomPanel_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.Free();
end;

procedure _LapePanel_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^ := TPanel.Create(PComponent(Params^[1])^);
end;

procedure _LapePanel_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.Free();
end;

procedure ImportLCLExtCtrls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TCustomTimer', 'TComponent');
    addGlobalFunc('procedure TCustomTimer.Init(AOwner: TComponent); override', @_LapeCustomTimer_Init);
    addClassVar('TCustomTimer', 'Enabled', 'Boolean', @_LapeCustomTimer_Enabled_Read, @_LapeCustomTimer_Enabled_Write);
    addClassVar('TCustomTimer', 'Interval', 'UInt32', @_LapeCustomTimer_Interval_Read, @_LapeCustomTimer_Interval_Write);
    addClassVar('TCustomTimer', 'OnTimer', 'TNotifyEvent', @_LapeCustomTimer_OnTimer_Read, @_LapeCustomTimer_OnTimer_Write);
    addClassVar('TCustomTimer', 'OnStartTimer', 'TNotifyEvent', @_LapeCustomTimer_OnStartTimer_Read, @_LapeCustomTimer_OnStartTimer_Write);
    addClassVar('TCustomTimer', 'OnStopTimer', 'TNotifyEvent', @_LapeCustomTimer_OnStopTimer_Read, @_LapeCustomTimer_OnStopTimer_Write);
    //addGlobalFunc('procedure TCustomTimer.Free;', @_LapeCustomTimer_Free);

    addClass('TTimer', 'TCustomTimer');
    addGlobalFunc('procedure TTimer.Init(AOwner: TComponent); override', @_LapeTimer_Init);
    //addGlobalFunc('procedure TTimer.Free;', @_LapeTimer_Free);

    addClass('TCustomImage', 'TGraphicControl');
    addGlobalFunc('procedure TCustomImage.Init(AOwner: TComponent); override', @_LapeCustomImage_Init);
    //addClassVar('TCustomImage', 'Canvas', 'TCanvas', @_LapeCustomImage_Canvas_Read);
    addGlobalFunc('function TCustomImage.DestRect: TRect;', @_LapeCustomImage_DestRect);
    addClassVar('TCustomImage', 'Center', 'Boolean', @_LapeCustomImage_Center_Read, @_LapeCustomImage_Center_Write);
    addClassVar('TCustomImage', 'Picture', 'TPicture', @_LapeCustomImage_Picture_Read, @_LapeCustomImage_Picture_Write);
    addClassVar('TCustomImage', 'Stretch', 'Boolean', @_LapeCustomImage_Stretch_Read, @_LapeCustomImage_Stretch_Write);
    addClassVar('TCustomImage', 'Transparent', 'Boolean', @_LapeCustomImage_Transparent_Read, @_LapeCustomImage_Transparent_Write);
    addClassVar('TCustomImage', 'Proportional', 'Boolean', @_LapeCustomImage_Proportional_Read, @_LapeCustomImage_Proportional_Write);
    addClassVar('TCustomImage', 'OnPictureChanged', 'TNotifyEvent', @_LapeCustomImage_OnPictureChanged_Read, @_LapeCustomImage_OnPictureChanged_Write);
    addClassVar('TCustomImage', 'OnMouseDown', 'TMouseEvent', nil, @_LapeCustomImage_OnMouseDown_Write);
    addClassVar('TCustomImage', 'OnMouseUp', 'TMouseEvent', nil, @_LapeCustomImage_OnMouseUp_Write);
    //addGlobalFunc('procedure TCustomImage.Free;', @_LapeCustomImage_Free);

    addClass('TImage', 'TCustomImage');
    addGlobalFunc('procedure TImage.Init(AOwner: TComponent); override', @_LapeImage_Init);
    //addGlobalFunc('procedure TImage.Free;', @_LapeImage_Free);
    addClassVar('TImage', 'OnMouseEnter', 'TNotifyEvent', @_LapeImage_OnMouseEnter_Read, @_LapeImage_OnMouseEnter_Write);
    addClassVar('TImage', 'OnMouseLeave', 'TNotifyEvent', @_LapeImage_OnMouseLeave_Read, @_LapeImage_OnMouseLeave_Write);
    addClassVar('TImage', 'OnMouseMove', 'TMouseMoveEvent', @_LapeImage_OnMouseMove_Read, @_LapeImage_OnMouseMove_Write);

    addClass('TCustomPanel', 'TCustomControl');
    addGlobalType('(bvNone, bvLowered, bvRaised, bvSpace)', 'TPanelBevel');
    addClassVar('TCustomPanel', 'Alignment', 'TAlignment', @_LapeCustomPanel_Alignment_Read, @_LapeCustomPanel_Alignment_Write);
    addClassVar('TCustomPanel', 'BevelInner', 'TPanelBevel', @_LapeCustomPanel_BevelInner_Read, @_LapeCustomPanel_BevelInner_Write);
    addClassVar('TCustomPanel', 'BevelOuter', 'TPanelBevel', @_LapeCustomPanel_BevelOuter_Read, @_LapeCustomPanel_BevelOuter_Write);
    addClassVar('TCustomPanel', 'BevelWidth', 'Integer', @_LapeCustomPanel_BevelWidth_Read, @_LapeCustomPanel_BevelWidth_Write);
    addGlobalFunc('procedure TCustomPanel.Init(TheOwner: TComponent); override', @_LapeCustomPanel_Init);
    //addGlobalFunc('procedure TCustomPanel.Free;', @_LapeCustomPanel_Free);

    addClass('TPanel', 'TCustomPanel');
    addGlobalFunc('procedure TPanel.Init(TheOwner: TComponent); override', @_LapePanel_Init);
    //addGlobalFunc('procedure TPanel.Free;', @_LapePanel_Free);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportLCLExtCtrls);

end.

