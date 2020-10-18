unit simba.script_import_lclextctrls;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLExtCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  customtimer, extctrls, forms, lcltype, controls, graphics;

type
  PCustomTimer = ^TCustomTimer;
  PTimer = ^TTimer;
  PCustomImage = ^TCustomImage;
  PImage = ^TImage;
  PCustomPanel = ^TCustomPanel;
  PBevelWidth = ^TBevelWidth;
  PPanelBevel = ^TPanelBevel;
  PAlignment = ^TAlignMent;
  PPanel = ^TPanel;
  PShape = ^TShape;
  PShapeType = ^TShapeType;
  PRect = ^TRect;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PBrush = ^TBrush;
  PPicture = ^TPicture;
  PMouseEvent = ^TMouseEvent;
  PComponent = ^TComponent;
  PNotifyEvent = ^TNotifyEvent;
  PObject = ^TObject;
  PPen = ^TPen;

//constructor Create(AOwner: TComponent);
procedure Lape_TCustomTimer_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^ := TCustomTimer.Create(PComponent(Params^[1])^);
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure Lape_TCustomTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTimer(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure Lape_TCustomTimer_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Interval: Cardinal read Interval write Interval;
procedure Lape_TCustomTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCardinal(Result)^ := PCustomTimer(Params^[0])^.Interval;
end;

//Write: property Interval: Cardinal read Interval write Interval;
procedure Lape_TCustomTimer_Interval_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.Interval := PCardinal(Params^[1])^;
end;

//Read: property OnTimer: TNotifyEvent read OnTimer write OnTimer;
procedure Lape_TCustomTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnTimer;
end;

//Write: property OnTimer: TNotifyEvent read OnTimer write OnTimer;
procedure Lape_TCustomTimer_OnTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

//Read: property OnStartTimer: TNotifyEvent read OnStartTimer write OnStartTimer;
procedure Lape_TCustomTimer_OnStartTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStartTimer;
end;

//Write: property OnStartTimer: TNotifyEvent read OnStartTimer write OnStartTimer;
procedure Lape_TCustomTimer_OnStartTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.OnStartTimer := PNotifyEvent(Params^[1])^;
end;

//Read: property OnStopTimer: TNotifyEvent read OnStopTimer write OnStopTimer;
procedure Lape_TCustomTimer_OnStopTimer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStopTimer;
end;

//Write: property OnStopTimer: TNotifyEvent read OnStopTimer write OnStopTimer;
procedure Lape_TCustomTimer_OnStopTimer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.OnStopTimer := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomTimer_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTimer(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomTimer(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomTimer', 'TComponent');

    addGlobalFunc('procedure TCustomTimer.Init(AOwner: TComponent); override;', @Lape_TCustomTimer_Init);
    addClassVar('TCustomTimer', 'Enabled', 'Boolean', @Lape_TCustomTimer_Enabled_Read, @Lape_TCustomTimer_Enabled_Write);
    addClassVar('TCustomTimer', 'Interval', 'Cardinal', @Lape_TCustomTimer_Interval_Read, @Lape_TCustomTimer_Interval_Write);
    addClassVar('TCustomTimer', 'OnTimer', 'TNotifyEvent', @Lape_TCustomTimer_OnTimer_Read, @Lape_TCustomTimer_OnTimer_Write);
    addClassVar('TCustomTimer', 'OnStartTimer', 'TNotifyEvent', @Lape_TCustomTimer_OnStartTimer_Read, @Lape_TCustomTimer_OnStartTimer_Write);
    addClassVar('TCustomTimer', 'OnStopTimer', 'TNotifyEvent', @Lape_TCustomTimer_OnStopTimer_Read, @Lape_TCustomTimer_OnStopTimer_Write);
    //addGlobalFunc('procedure TCustomTimer.Free(); constref;', @Lape_TCustomTimer_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure Lape_TTimer_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTimer(Params^[0])^ := TTimer.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TTimer_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTimer(Params^[0])^.Free();
end;

procedure Lape_Import_TTimer(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TTimer', 'TCustomTimer');

    addGlobalFunc('procedure TTimer.Init(AOwner: TComponent); override;', @Lape_TTimer_Init);
    //addGlobalFunc('procedure TTimer.Free(); constref;', @Lape_TTimer_Free);
  end;
end;

{TImage}
//constructor Create(AOwner: TComponent);
procedure Lape_TCustomImage_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^ := TCustomImage.Create(PComponent(Params^[1])^);
end;

//Read: property Canvas: TCanvas read Canvas;
procedure Lape_TCustomImage_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomImage(Params^[0])^.Canvas;
end;

//function DestRect: TRect;
procedure Lape_TCustomImage_DestRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomImage(Params^[0])^.DestRect();
end;

//Read: property Center: Boolean read Center write Center;
procedure Lape_TCustomImage_Center_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Center;
end;

//Write: property Center: Boolean read Center write Center;
procedure Lape_TCustomImage_Center_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Center := PBoolean(Params^[1])^;
end;

//Read: property Picture: TPicture read Picture write Picture;
procedure Lape_TCustomImage_Picture_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPicture(Result)^ := PCustomImage(Params^[0])^.Picture;
end;

//Write: property Picture: TPicture read Picture write Picture;
procedure Lape_TCustomImage_Picture_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Picture := PPicture(Params^[1])^;
end;

//Read: property Stretch: Boolean read Stretch write Stretch;
procedure Lape_TCustomImage_Stretch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Stretch;
end;

//Write: property Stretch: Boolean read Stretch write Stretch;
procedure Lape_TCustomImage_Stretch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Stretch := PBoolean(Params^[1])^;
end;

//Read: property Transparent: Boolean read Transparent write Transparent;
procedure Lape_TCustomImage_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read Transparent write Transparent;
procedure Lape_TCustomImage_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//Read: property Proportional: Boolean read Proportional write Proportional;
procedure Lape_TCustomImage_Proportional_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Proportional;
end;

//Write: property Proportional: Boolean read Proportional write Proportional;
procedure Lape_TCustomImage_Proportional_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Proportional := PBoolean(Params^[1])^;
end;

//Read: property OnPictureChanged: TNotifyEvent read OnPictureChanged write OnPictureChanged;
procedure Lape_TCustomImage_OnPictureChanged_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomImage(Params^[0])^.OnPictureChanged;
end;

//Write: property OnPictureChanged: TNotifyEvent read OnPictureChanged write OnPictureChanged;
procedure Lape_TCustomImage_OnPictureChanged_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.OnPictureChanged := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomImage_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.Free;
end;

procedure Lape_TCustomImage_OnMouseDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;;
end;

procedure Lape_TCustomImage_OnMouseUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomImage(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure Lape_Import_TCustomImage(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomImage', 'TGraphicControl');
    addGlobalFunc('procedure TCustomImage.Init(AOwner: TComponent); override;', @Lape_TCustomImage_Init);
    //addClassVar('TCustomImage', 'Canvas', 'TCanvas', @Lape_TCustomImage_Canvas_Read);
    addGlobalFunc('function TCustomImage.DestRect(): TRect; constref;', @Lape_TCustomImage_DestRect);
    addClassVar('TCustomImage', 'Center', 'Boolean', @Lape_TCustomImage_Center_Read, @Lape_TCustomImage_Center_Write);
    addClassVar('TCustomImage', 'Picture', 'TPicture', @Lape_TCustomImage_Picture_Read, @Lape_TCustomImage_Picture_Write);
    addClassVar('TCustomImage', 'Stretch', 'Boolean', @Lape_TCustomImage_Stretch_Read, @Lape_TCustomImage_Stretch_Write);
    addClassVar('TCustomImage', 'Transparent', 'Boolean', @Lape_TCustomImage_Transparent_Read, @Lape_TCustomImage_Transparent_Write);
    addClassVar('TCustomImage', 'Proportional', 'Boolean', @Lape_TCustomImage_Proportional_Read, @Lape_TCustomImage_Proportional_Write);
    addClassVar('TCustomImage', 'OnPictureChanged', 'TNotifyEvent', @Lape_TCustomImage_OnPictureChanged_Read, @Lape_TCustomImage_OnPictureChanged_Write);
    addClassVar('TCustomImage', 'OnMouseDown', 'TMouseEvent', nil, @Lape_TCustomImage_OnMouseDown_Write);
    addClassVar('TCustomImage', 'OnMouseUp', 'TMouseEvent', nil, @Lape_TCustomImage_OnMouseUp_Write);
    //addGlobalFunc('procedure TCustomImage.Free(); constref;', @Lape_TCustomImage_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure Lape_TImage_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^ := TImage.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TImage_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.Free();
end;

procedure Lape_TImage_ShowHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure Lape_TImage_ShowHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PBoolean(Result)^ := PImage(Params^[0])^.ShowHint;
end;

procedure Lape_TImage_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.Hint := PLPString(Params^[1])^;
end;

procedure Lape_TImage_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PImage(Params^[0])^.Hint;
end;

procedure Lape_TImage_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseEnter;
end;

procedure Lape_TImage_OnMouseEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure Lape_TImage_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseLeave;
end;

procedure Lape_TImage_OnMouseLeave_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure Lape_TImage_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseMoveEvent(Result)^ := PImage(Params^[0])^.OnMouseMove;
end;

procedure Lape_TImage_OnMouseMove_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PImage(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure Lape_Import_TImage(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TImage', 'TCustomImage');

    addGlobalFunc('procedure TImage.Init(AOwner: TComponent); override;', @Lape_TImage_Init);
    //addGlobalFunc('procedure TImage.Free(); constref;', @Lape_TImage_Free);
    //addClassVar('TImage', 'Hint', 'String', @Lape_TImage_Hint_Read, @Lape_TImage_Hint_Write);
    //addClassVar('TImage', 'ShowHint', 'Boolean', @Lape_TImage_ShowHint_Read, @Lape_TImage_ShowHint_Write);
    addClassVar('TImage', 'OnMouseEnter', 'TNotifyEvent', @Lape_TImage_OnMouseEnter_Read, @Lape_TImage_OnMouseEnter_Write);
    addClassVar('TImage', 'OnMouseLeave', 'TNotifyEvent', @Lape_TImage_OnMouseLeave_Read, @Lape_TImage_OnMouseLeave_Write);
    addClassVar('TImage', 'OnMouseMove', 'TMouseMoveEvent', @Lape_TImage_OnMouseMove_Read, @Lape_TImage_OnMouseMove_Write);
  end;
end;

//Read: property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
procedure Lape_TCustomPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAlignment(Result)^ := PCustomPanel(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
procedure Lape_TCustomPanel_Alignment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
procedure Lape_TCustomPanel_BevelInner_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelInner;
end;

//Write: property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
procedure Lape_TCustomPanel_BevelInner_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.BevelInner := PPanelBevel(Params^[1])^;
end;

//Read: property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
procedure Lape_TCustomPanel_BevelOuter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelOuter;
end;

//Write: property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
procedure Lape_TCustomPanel_BevelOuter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.BevelOuter := PPanelBevel(Params^[1])^;
end;

//Read: property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
procedure Lape_TCustomPanel_BevelWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBevelWidth(Result)^ := PCustomPanel(Params^[0])^.BevelWidth;
end;

//Write: property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
procedure Lape_TCustomPanel_BevelWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.BevelWidth := PBevelWidth(Params^[1])^;
end;

//Read: property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
procedure Lape_TCustomPanel_FullRepaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomPanel(Params^[0])^.FullRepaint;
end;

//Write: property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
procedure Lape_TCustomPanel_FullRepaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.FullRepaint := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCustomPanel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^ := TCustomPanel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomPanel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPanel(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomPanel(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomPanel', 'TCustomControl');
    addClassVar('TCustomPanel', 'Alignment', 'TAlignment', @Lape_TCustomPanel_Alignment_Read, @Lape_TCustomPanel_Alignment_Write);
    addClassVar('TCustomPanel', 'BevelInner', 'TPanelBevel', @Lape_TCustomPanel_BevelInner_Read, @Lape_TCustomPanel_BevelInner_Write);
    addClassVar('TCustomPanel', 'BevelOuter', 'TPanelBevel', @Lape_TCustomPanel_BevelOuter_Read, @Lape_TCustomPanel_BevelOuter_Write);
    addClassVar('TCustomPanel', 'BevelWidth', 'TBevelWidth', @Lape_TCustomPanel_BevelWidth_Read, @Lape_TCustomPanel_BevelWidth_Write);
    addClassVar('TCustomPanel', 'FullRepaint', 'Boolean', @Lape_TCustomPanel_FullRepaint_Read, @Lape_TCustomPanel_FullRepaint_Write);
    addGlobalFunc('procedure TCustomPanel.Init(TheOwner: TComponent); override;', @Lape_TCustomPanel_Init);
    //addGlobalFunc('procedure TCustomPanel.Free(); constref;', @Lape_TCustomPanel_Free);
  end;
end;

//constructor Create();
procedure Lape_TPanel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPanel(Params^[0])^ := TPanel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TPanel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPanel(Params^[0])^.Free();
end;

procedure Lape_Import_TPanel(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TPanel', 'TCustomPanel');

    addGlobalFunc('procedure TPanel.Init(TheOwner: TComponent); override;', @Lape_TPanel_Init);
    //addGlobalFunc('procedure TPanel.Free(); constref;', @Lape_TPanel_Free);
  end;
end;

//procedure StyleChanged(Sender: TObject);
procedure Lape_TShape_StyleChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.StyleChanged(PObject(Params^[1])^);
end;

//Read: property Brush: TBrush read FBrush write SetBrush;
procedure Lape_TShape_Brush_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBrush(Result)^ := PShape(Params^[0])^.Brush;
end;

//Write: property Brush: TBrush read FBrush write SetBrush;
procedure Lape_TShape_Brush_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Brush := PBrush(Params^[1])^;
end;

//Read: property Pen: TPen read FPen write SetPen;
procedure Lape_TShape_Pen_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPen(Result)^ := PShape(Params^[0])^.Pen;
end;

//Write: property Pen: TPen read FPen write SetPen;
procedure Lape_TShape_Pen_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Pen := PPen(Params^[1])^;
end;

//Read: property Shape: TShapeType read FShape write SetShape default stRectangle;
procedure Lape_TShape_Shape_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShapeType(Result)^ := PShape(Params^[0])^.Shape;
end;

//Write: property Shape: TShapeType read FShape write SetShape default stRectangle;
procedure Lape_TShape_Shape_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Shape := PShapeType(Params^[1])^;
end;

//constructor Create();
procedure Lape_TShape_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShape(Params^[0])^ := TShape.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TShape_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShape(Params^[0])^.Free();
end;

procedure Lape_Import_TShape(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TShape', 'TGraphicControl');

    addGlobalFunc('procedure TShape.StyleChanged(Sender: TObject); constref;', @Lape_TShape_StyleChanged);
    addClassVar('TShape', 'Brush', 'TBrush', @Lape_TShape_Brush_Read, @Lape_TShape_Brush_Write);
    addClassVar('TShape', 'Pen', 'TPen', @Lape_TShape_Pen_Read, @Lape_TShape_Pen_Write);
    addClassVar('TShape', 'Shape', 'TShapeType', @Lape_TShape_Shape_Read, @Lape_TShape_Shape_Write);
    addGlobalFunc('procedure TShape.Init(TheOwner: TComponent); override;', @Lape_TShape_Init);
   // addGlobalFunc('procedure TShape.Free(); constref;', @Lape_TShape_Free);
  end;
end;

procedure Lape_Import_LCLExtCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
   with Compiler do
   begin
     addGlobalType('(bvNone, bvLowered, bvRaised, bvSpace)', 'TGraphicsBevelCut');
     addGlobalType('TGraphicsBevelCut', 'TBevelCut');
     addGlobalType('TBevelCut', 'TPanelBevel');
     addGlobalType('Integer', 'TBevelWidth');
     addGlobalType('(stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle, stSquaredDiamond, stDiamond, stTriangle)', 'TShapeType');
   end;

   Lape_Import_TCustomTimer(Compiler);
   Lape_Import_TTimer(Compiler);
   Lape_Import_TCustomImage(Compiler);
   Lape_Import_TImage(Compiler);
   Lape_Import_TCustomPanel(Compiler);
   Lape_Import_TPanel(Compiler);
   Lape_Import_TShape(Compiler);
end;

end.

