unit simba.import_lcl_extctrls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportLCLExtCtrls(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, extctrls, graphics, customtimer, lptypes, ffi;

type
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
  PRect = ^TRect;
  PPicture = ^TPicture;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;

procedure _LapeCustomTimer_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTimer(Result)^ := TCustomTimer.Create(PComponent(Params^[0])^);
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

procedure _LapeTimer_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Result)^ := TTimer.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomImage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Result)^ := TCustomImage.Create(PComponent(Params^[0])^);
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

procedure _LapeCustomImage_OnMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeCustomImage_OnMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomImage(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeImage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Result)^ := TImage.Create(PComponent(Params^[0])^);
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

procedure _LapeCustomPanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Result)^ := TCustomPanel.Create(PComponent(Params^[0])^);
end;

procedure _LapePanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Result)^ := TPanel.Create(PComponent(Params^[0])^);
end;

procedure ImportLCLExtCtrls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TLazCustomTimer', 'TLazComponent');
    addClassConstructor('TLazCustomTimer', '(AOwner: TLazComponent)', @_LapeCustomTimer_Create);
    addClassVar('TLazCustomTimer', 'Enabled', 'Boolean', @_LapeCustomTimer_Enabled_Read, @_LapeCustomTimer_Enabled_Write);
    addClassVar('TLazCustomTimer', 'Interval', 'UInt32', @_LapeCustomTimer_Interval_Read, @_LapeCustomTimer_Interval_Write);
    addClassVar('TLazCustomTimer', 'OnTimer', 'TLazNotifyEvent', @_LapeCustomTimer_OnTimer_Read, @_LapeCustomTimer_OnTimer_Write);
    addClassVar('TLazCustomTimer', 'OnStartTimer', 'TLazNotifyEvent', @_LapeCustomTimer_OnStartTimer_Read, @_LapeCustomTimer_OnStartTimer_Write);
    addClassVar('TLazCustomTimer', 'OnStopTimer', 'TLazNotifyEvent', @_LapeCustomTimer_OnStopTimer_Read, @_LapeCustomTimer_OnStopTimer_Write);

    addClass('TLazTimer', 'TLazCustomTimer');
    addClassConstructor('TLazTimer', '(AOwner: TLazComponent)', @_LapeTimer_Create);

    addClass('TLazCustomImage', 'TLazGraphicControl');
    addClassConstructor('TLazCustomImage', '(AOwner: TLazComponent)', @_LapeCustomImage_Create);
    addGlobalFunc('function TLazCustomImage.DestRect: TLazRect;', @_LapeCustomImage_DestRect);
    addClassVar('TLazCustomImage', 'Center', 'Boolean', @_LapeCustomImage_Center_Read, @_LapeCustomImage_Center_Write);
    addClassVar('TLazCustomImage', 'Picture', 'TLazPicture', @_LapeCustomImage_Picture_Read, @_LapeCustomImage_Picture_Write);
    addClassVar('TLazCustomImage', 'Stretch', 'Boolean', @_LapeCustomImage_Stretch_Read, @_LapeCustomImage_Stretch_Write);
    addClassVar('TLazCustomImage', 'Transparent', 'Boolean', @_LapeCustomImage_Transparent_Read, @_LapeCustomImage_Transparent_Write);
    addClassVar('TLazCustomImage', 'Proportional', 'Boolean', @_LapeCustomImage_Proportional_Read, @_LapeCustomImage_Proportional_Write);
    addClassVar('TLazCustomImage', 'OnPictureChanged', 'TLazNotifyEvent', @_LapeCustomImage_OnPictureChanged_Read, @_LapeCustomImage_OnPictureChanged_Write);
    addClassVar('TLazCustomImage', 'OnMouseDown', 'TLazMouseEvent', nil, @_LapeCustomImage_OnMouseDown_Write);
    addClassVar('TLazCustomImage', 'OnMouseUp', 'TLazMouseEvent', nil, @_LapeCustomImage_OnMouseUp_Write);

    addClass('TLazImage', 'TLazCustomImage');
    addClassConstructor('TLazImage', '(AOwner: TLazComponent)', @_LapeImage_Create);
    addClassVar('TLazImage', 'OnMouseEnter', 'TLazNotifyEvent', @_LapeImage_OnMouseEnter_Read, @_LapeImage_OnMouseEnter_Write);
    addClassVar('TLazImage', 'OnMouseLeave', 'TLazNotifyEvent', @_LapeImage_OnMouseLeave_Read, @_LapeImage_OnMouseLeave_Write);
    addClassVar('TLazImage', 'OnMouseMove', 'TLazMouseMoveEvent', @_LapeImage_OnMouseMove_Read, @_LapeImage_OnMouseMove_Write);

    addClass('TLazCustomPanel', 'TLazCustomControl');
    addGlobalType('(bvNone, bvLowered, bvRaised, bvSpace)', 'TLazPanelBevel');
    addClassVar('TLazCustomPanel', 'Alignment', 'TLazAlignment', @_LapeCustomPanel_Alignment_Read, @_LapeCustomPanel_Alignment_Write);
    addClassVar('TLazCustomPanel', 'BevelInner', 'TLazPanelBevel', @_LapeCustomPanel_BevelInner_Read, @_LapeCustomPanel_BevelInner_Write);
    addClassVar('TLazCustomPanel', 'BevelOuter', 'TLazPanelBevel', @_LapeCustomPanel_BevelOuter_Read, @_LapeCustomPanel_BevelOuter_Write);
    addClassVar('TLazCustomPanel', 'BevelWidth', 'Integer', @_LapeCustomPanel_BevelWidth_Read, @_LapeCustomPanel_BevelWidth_Write);
    addClassConstructor('TLazCustomPanel', '(TheOwner: TLazComponent)', @_LapeCustomPanel_Create);

    addClass('TLazPanel', 'TLazCustomPanel');
    addClassConstructor('TLazPanel', '(TheOwner: TLazComponent)', @_LapePanel_Create);
  end;
end;

end.

