type
  PBevelWidth = ^TBevelWidth;
  PCustomImage = ^TCustomImage;
  PCustomPanel = ^TCustomPanel;
  PCustomTimer = ^TCustomTimer;
  PImage = ^TImage;
  PPanel = ^TPanel;
  PPanelBevel = ^TPanelBevel;
  PShape = ^TShape;
  PShapeType = ^TShapeType;
  PTimer = ^TTimer;

procedure _LapeCustomTimer_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^ := TCustomTimer.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTimer(Params^[0])^.Enabled;
end;

procedure _LapeCustomTimer_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PCustomTimer(Params^[0])^.Interval;
end;

procedure _LapeCustomTimer_Interval_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.Interval := PCardinal(Params^[1])^;
end;

procedure _LapeCustomTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnTimer;
end;

procedure _LapeCustomTimer_OnTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTimer_OnStartTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStartTimer;
end;

procedure _LapeCustomTimer_OnStartTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.OnStartTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTimer_OnStopTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStopTimer;
end;

procedure _LapeCustomTimer_OnStopTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.OnStopTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTimer_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.Free();
end;

procedure _LapeTimer_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTimer(Params^[0])^ := TTimer.Create(PComponent(Params^[1])^);
end;

procedure _LapeTimer_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTimer(Params^[0])^.Free();
end;

procedure _LapeCustomImage_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^ := TCustomImage.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomImage_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomImage(Params^[0])^.Canvas;
end;

procedure _LapeCustomImage_DestRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomImage(Params^[0])^.DestRect();
end;

procedure _LapeCustomImage_Center_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Center;
end;

procedure _LapeCustomImage_Center_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Center := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_Picture_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPicture(Result)^ := PCustomImage(Params^[0])^.Picture;
end;

procedure _LapeCustomImage_Picture_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Picture := PPicture(Params^[1])^;
end;

procedure _LapeCustomImage_Stretch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Stretch;
end;

procedure _LapeCustomImage_Stretch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Stretch := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Transparent;
end;

procedure _LapeCustomImage_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_Proportional_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Proportional;
end;

procedure _LapeCustomImage_Proportional_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Proportional := PBoolean(Params^[1])^;
end;

procedure _LapeCustomImage_OnPictureChanged_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomImage(Params^[0])^.OnPictureChanged;
end;

procedure _LapeCustomImage_OnPictureChanged_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.OnPictureChanged := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomImage_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Free;
end;

procedure _LapeCustomImage_OnMouseDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
  ;
end;

procedure _LapeCustomImage_OnMouseUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeImage_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^ := TImage.Create(PComponent(Params^[1])^);
end;

procedure _LapeImage_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.Free();
end;

procedure _LapeImage_ShowHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure _LapeImage_ShowHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PImage(Params^[0])^.ShowHint;
end;

procedure _LapeImage_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.Hint := PLPString(Params^[1])^;
end;

procedure _LapeImage_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PImage(Params^[0])^.Hint;
end;

procedure _LapeImage_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseEnter;
end;

procedure _LapeImage_OnMouseEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseLeave;
end;

procedure _LapeImage_OnMouseLeave_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseMoveEvent(Result)^ := PImage(Params^[0])^.OnMouseMove;
end;

procedure _LapeImage_OnMouseMove_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeCustomPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PAlignment(Result)^ := PCustomPanel(Params^[0])^.Alignment;
end;

procedure _LapeCustomPanel_Alignment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelInner_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelInner;
end;

procedure _LapeCustomPanel_BevelInner_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.BevelInner := PPanelBevel(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelOuter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelOuter;
end;

procedure _LapeCustomPanel_BevelOuter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.BevelOuter := PPanelBevel(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBevelWidth(Result)^ := PCustomPanel(Params^[0])^.BevelWidth;
end;

procedure _LapeCustomPanel_BevelWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.BevelWidth := PBevelWidth(Params^[1])^;
end;

procedure _LapeCustomPanel_FullRepaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomPanel(Params^[0])^.FullRepaint;
end;

procedure _LapeCustomPanel_FullRepaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.FullRepaint := PBoolean(Params^[1])^;
end;

procedure _LapeCustomPanel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^ := TCustomPanel.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomPanel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.Free();
end;

procedure _LapePanel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPanel(Params^[0])^ := TPanel.Create(PComponent(Params^[1])^);
end;

procedure _LapePanel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPanel(Params^[0])^.Free();
end;

procedure _LapeShape_StyleChanged(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.StyleChanged(PObject(Params^[1])^);
end;

procedure _LapeShape_Brush_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Result)^ := PShape(Params^[0])^.Brush;
end;

procedure _LapeShape_Brush_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Brush := PBrush(Params^[1])^;
end;

procedure _LapeShape_Pen_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPen(Result)^ := PShape(Params^[0])^.Pen;
end;

procedure _LapeShape_Pen_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Pen := PPen(Params^[1])^;
end;

procedure _LapeShape_Shape_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShapeType(Result)^ := PShape(Params^[0])^.Shape;
end;

procedure _LapeShape_Shape_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Shape := PShapeType(Params^[1])^;
end;

procedure _LapeShape_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShape(Params^[0])^ := TShape.Create(PComponent(Params^[1])^);
end;

procedure _LapeShape_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Free();
end;

