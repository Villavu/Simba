unit lplclextctrls;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils,lpcompiler, lptypes, lpClassHelper;

procedure RegisterLCLExtCtrls(Compiler: TLapeCompiler);

implementation
  uses MufasaTypes,CustomTimer,extctrls,forms,lplclsystem,lplclgraphics,lplclcontrols,lclType;

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

   {TTimer}

   //constructor Create(AOwner: TComponent);
procedure TCustomTimer_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^ := TCustomTimer.Create(PComponent(Params^[1])^);
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure TCustomTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomTimer(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure TCustomTimer_Enabled_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Interval: Cardinal read Interval write Interval;
procedure TCustomTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PCustomTimer(Params^[0])^.Interval;
end;

//Write: property Interval: Cardinal read Interval write Interval;
procedure TCustomTimer_Interval_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^.Interval := PCardinal(Params^[1])^;
end;

//Read: property OnTimer: TNotifyEvent read OnTimer write OnTimer;
procedure TCustomTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnTimer;
end;

//Write: property OnTimer: TNotifyEvent read OnTimer write OnTimer;
procedure TCustomTimer_OnTimer_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

//Read: property OnStartTimer: TNotifyEvent read OnStartTimer write OnStartTimer;
procedure TCustomTimer_OnStartTimer_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStartTimer;
end;

//Write: property OnStartTimer: TNotifyEvent read OnStartTimer write OnStartTimer;
procedure TCustomTimer_OnStartTimer_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^.OnStartTimer := PNotifyEvent(Params^[1])^;
end;

//Read: property OnStopTimer: TNotifyEvent read OnStopTimer write OnStopTimer;
procedure TCustomTimer_OnStopTimer_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomTimer(Params^[0])^.OnStopTimer;
end;

//Write: property OnStopTimer: TNotifyEvent read OnStopTimer write OnStopTimer;
procedure TCustomTimer_OnStopTimer_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^.OnStopTimer := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure TCustomTimer_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomTimer(Params^[0])^.Free();
end;

procedure Register_TCustomTimer(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomTimer', 'TComponent');

    addGlobalFunc('procedure TCustomTimer.Init(AOwner: TComponent);', @TCustomTimer_Init);
    addClassVar('TCustomTimer', 'Enabled', 'Boolean', @TCustomTimer_Enabled_Read, @TCustomTimer_Enabled_Write);
    addClassVar('TCustomTimer', 'Interval', 'Cardinal', @TCustomTimer_Interval_Read, @TCustomTimer_Interval_Write);
    addClassVar('TCustomTimer', 'OnTimer', 'TNotifyEvent', @TCustomTimer_OnTimer_Read, @TCustomTimer_OnTimer_Write);
    addClassVar('TCustomTimer', 'OnStartTimer', 'TNotifyEvent', @TCustomTimer_OnStartTimer_Read, @TCustomTimer_OnStartTimer_Write);
    addClassVar('TCustomTimer', 'OnStopTimer', 'TNotifyEvent', @TCustomTimer_OnStopTimer_Read, @TCustomTimer_OnStopTimer_Write);
    addGlobalFunc('procedure TCustomTimer.Free();', @TCustomTimer_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TTimer_Init(const Params: PParamArray); lape_extdecl
begin
  PTimer(Params^[0])^ := TTimer.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TTimer_Free(const Params: PParamArray); lape_extdecl
begin
  PTimer(Params^[0])^.Free();
end;

procedure Register_TTimer(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TTimer', 'TCustomTimer');

    addGlobalFunc('procedure TTimer.Init(AOwner: TComponent);', @TTimer_Init);
    addGlobalFunc('procedure TTimer.Free();', @TTimer_Free);
  end;
end;

{TImage}
//constructor Create(AOwner: TComponent);
procedure TCustomImage_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^ := TCustomImage.Create(PComponent(Params^[1])^);
end;

//Read: property Canvas: TCanvas read Canvas;
procedure TCustomImage_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PCustomImage(Params^[0])^.Canvas;
end;

//function DestRect: TRect;
procedure TCustomImage_DestRect(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PCustomImage(Params^[0])^.DestRect();
end;

//Read: property Center: Boolean read Center write Center;
procedure TCustomImage_Center_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Center;
end;

//Write: property Center: Boolean read Center write Center;
procedure TCustomImage_Center_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.Center := PBoolean(Params^[1])^;
end;

//Read: property Picture: TPicture read Picture write Picture;
procedure TCustomImage_Picture_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPicture(Result)^ := PCustomImage(Params^[0])^.Picture;
end;

//Write: property Picture: TPicture read Picture write Picture;
procedure TCustomImage_Picture_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.Picture := PPicture(Params^[1])^;
end;

//Read: property Stretch: Boolean read Stretch write Stretch;
procedure TCustomImage_Stretch_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Stretch;
end;

//Write: property Stretch: Boolean read Stretch write Stretch;
procedure TCustomImage_Stretch_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.Stretch := PBoolean(Params^[1])^;
end;

//Read: property Transparent: Boolean read Transparent write Transparent;
procedure TCustomImage_Transparent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read Transparent write Transparent;
procedure TCustomImage_Transparent_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//Read: property Proportional: Boolean read Proportional write Proportional;
procedure TCustomImage_Proportional_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomImage(Params^[0])^.Proportional;
end;

//Write: property Proportional: Boolean read Proportional write Proportional;
procedure TCustomImage_Proportional_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.Proportional := PBoolean(Params^[1])^;
end;

//Read: property OnPictureChanged: TNotifyEvent read OnPictureChanged write OnPictureChanged;
procedure TCustomImage_OnPictureChanged_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomImage(Params^[0])^.OnPictureChanged;
end;

//Write: property OnPictureChanged: TNotifyEvent read OnPictureChanged write OnPictureChanged;
procedure TCustomImage_OnPictureChanged_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.OnPictureChanged := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure TCustomImage_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomImage(Params^[0])^.Free();
end;

procedure Register_TCustomImage(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomImage', 'TGraphicControl');

    addGlobalFunc('procedure TCustomImage.Init(AOwner: TComponent);', @TCustomImage_Init);
    addClassVar('TCustomImage', 'Canvas', 'TCanvas', @TCustomImage_Canvas_Read);
    addGlobalFunc('function TCustomImage.DestRect(): TRect;', @TCustomImage_DestRect);
    addClassVar('TCustomImage', 'Center', 'Boolean', @TCustomImage_Center_Read, @TCustomImage_Center_Write);
    addClassVar('TCustomImage', 'Picture', 'TPicture', @TCustomImage_Picture_Read, @TCustomImage_Picture_Write);
    addClassVar('TCustomImage', 'Stretch', 'Boolean', @TCustomImage_Stretch_Read, @TCustomImage_Stretch_Write);
    addClassVar('TCustomImage', 'Transparent', 'Boolean', @TCustomImage_Transparent_Read, @TCustomImage_Transparent_Write);
    addClassVar('TCustomImage', 'Proportional', 'Boolean', @TCustomImage_Proportional_Read, @TCustomImage_Proportional_Write);
    addClassVar('TCustomImage', 'OnPictureChanged', 'TNotifyEvent', @TCustomImage_OnPictureChanged_Read, @TCustomImage_OnPictureChanged_Write);
    addGlobalFunc('procedure TCustomImage.Free();', @TCustomImage_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TImage_Init(const Params: PParamArray); lape_extdecl
begin
  PImage(Params^[0])^ := TImage.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TImage_Free(const Params: PParamArray); lape_extdecl
begin
  PImage(Params^[0])^.Free();
end;

procedure TImage_ShowHint_Write(const Params: PParamArray); lape_extdecl
begin
  PImage(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure TImage_ShowHint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
 PBoolean(Result)^ := PImage(Params^[0])^.ShowHint;
end;

procedure TImage_Hint_Write(const Params: PParamArray); lape_extdecl
begin
  PImage(Params^[0])^.Hint := PLPString(Params^[1])^;
end;

procedure TImage_Hint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PImage(Params^[0])^.Hint;
end;

procedure TImage_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseEnter;
end;

procedure TImage_OnMouseEnter_Write(const Params: PParamArray); lape_extdecl
begin
  PImage(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure TImage_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseLeave;
end;

procedure TImage_OnMouseLeave_Write(const Params: PParamArray); lape_extdecl
begin
  PImage(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure TImage_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Component: TComponent;
begin
  Component := PImage(Params^[0])^.FindComponent('MouseMove');

  if (Assigned(Component)) then
    PMouseMoveEventWrapper(Result)^ := TOnMouseMoveWrapper(Component).InternalMethod
  else
    PMouseMoveEvent(Result)^ := nil;
end;

procedure TImage_OnMouseMove_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PImage(Params^[0])^.FindComponent('MouseMove');
  if (not Assigned(Component)) then
  begin
    Component := TOnMouseMoveWrapper.Create(PImage(Params^[0])^);
    Component.Name := 'MouseMove';
  end;

  with TOnMouseMoveWrapper(Component) do
  begin
    InternalMethod := PMouseMoveEventWrapper(Params^[1])^;
    PImage(Params^[0])^.OnMouseMove := @MouseMove;
  end;
end;

procedure Register_TImage(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TImage', 'TCustomImage');

    addGlobalFunc('procedure TImage.Init(AOwner: TComponent);', @TImage_Init);
    addGlobalFunc('procedure TImage.Free();', @TImage_Free);
    addClassVar('TImage', 'Hint', 'String', @TImage_Hint_Read, @TImage_Hint_Write);
    addClassVar('TImage', 'ShowHint', 'Boolean', @TImage_ShowHint_Read, @TImage_ShowHint_Write);
    addClassVar('TImage', 'OnMouseEnter', 'TNotifyEvent', @TImage_OnMouseEnter_Read, @TImage_OnMouseEnter_Write);
    addClassVar('TImage', 'OnMouseLeave', 'TNotifyEvent', @TImage_OnMouseLeave_Read, @TImage_OnMouseLeave_Write);
    addClassVar('TImage', 'OnMouseMove', 'TMouseMoveEvent', @TImage_OnMouseMove_Read, @TImage_OnMouseMove_Write);
  end;
end;

//Read: property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
procedure TCustomPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlignment(Result)^ := PCustomPanel(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
procedure TCustomPanel_Alignment_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
procedure TCustomPanel_BevelInner_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelInner;
end;

//Write: property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
procedure TCustomPanel_BevelInner_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^.BevelInner := PPanelBevel(Params^[1])^;
end;

//Read: property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
procedure TCustomPanel_BevelOuter_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelOuter;
end;

//Write: property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
procedure TCustomPanel_BevelOuter_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^.BevelOuter := PPanelBevel(Params^[1])^;
end;

//Read: property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
procedure TCustomPanel_BevelWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBevelWidth(Result)^ := PCustomPanel(Params^[0])^.BevelWidth;
end;

//Write: property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
procedure TCustomPanel_BevelWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^.BevelWidth := PBevelWidth(Params^[1])^;
end;

//Read: property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
procedure TCustomPanel_FullRepaint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomPanel(Params^[0])^.FullRepaint;
end;

//Write: property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
procedure TCustomPanel_FullRepaint_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^.FullRepaint := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure TCustomPanel_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^ := TCustomPanel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomPanel_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomPanel(Params^[0])^.Free();
end;

procedure Register_TCustomPanel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomPanel', 'TCustomControl');
    addClassVar('TCustomPanel', 'Alignment', 'TAlignment', @TCustomPanel_Alignment_Read, @TCustomPanel_Alignment_Write);
    addClassVar('TCustomPanel', 'BevelInner', 'TPanelBevel', @TCustomPanel_BevelInner_Read, @TCustomPanel_BevelInner_Write);
    addClassVar('TCustomPanel', 'BevelOuter', 'TPanelBevel', @TCustomPanel_BevelOuter_Read, @TCustomPanel_BevelOuter_Write);
    addClassVar('TCustomPanel', 'BevelWidth', 'TBevelWidth', @TCustomPanel_BevelWidth_Read, @TCustomPanel_BevelWidth_Write);
    addClassVar('TCustomPanel', 'FullRepaint', 'Boolean', @TCustomPanel_FullRepaint_Read, @TCustomPanel_FullRepaint_Write);
    addGlobalFunc('procedure TCustomPanel.Init(TheOwner: TComponent);', @TCustomPanel_Init);
    addGlobalFunc('procedure TCustomPanel.Free();', @TCustomPanel_Free);
  end;
end;

//constructor Create();
procedure TPanel_Init(const Params: PParamArray); lape_extdecl
begin
  PPanel(Params^[0])^ := TPanel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TPanel_Free(const Params: PParamArray); lape_extdecl
begin
  PPanel(Params^[0])^.Free();
end;

procedure Register_TPanel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TPanel', 'TCustomPanel');

    addGlobalFunc('procedure TPanel.Init(TheOwner: TComponent);', @TPanel_Init);
    addGlobalFunc('procedure TPanel.Free();', @TPanel_Free);
  end;
end;

//procedure StyleChanged(Sender: TObject);
procedure TShape_StyleChanged(const Params: PParamArray); lape_extdecl
begin
  PShape(Params^[0])^.StyleChanged(PObject(Params^[1])^);
end;

//Read: property Brush: TBrush read FBrush write SetBrush;
procedure TShape_Brush_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBrush(Result)^ := PShape(Params^[0])^.Brush;
end;

//Write: property Brush: TBrush read FBrush write SetBrush;
procedure TShape_Brush_Write(const Params: PParamArray); lape_extdecl
begin
  PShape(Params^[0])^.Brush := PBrush(Params^[1])^;
end;

//Read: property Pen: TPen read FPen write SetPen;
procedure TShape_Pen_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPen(Result)^ := PShape(Params^[0])^.Pen;
end;

//Write: property Pen: TPen read FPen write SetPen;
procedure TShape_Pen_Write(const Params: PParamArray); lape_extdecl
begin
  PShape(Params^[0])^.Pen := PPen(Params^[1])^;
end;

//Read: property Shape: TShapeType read FShape write SetShape default stRectangle;
procedure TShape_Shape_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PShapeType(Result)^ := PShape(Params^[0])^.Shape;
end;

//Write: property Shape: TShapeType read FShape write SetShape default stRectangle;
procedure TShape_Shape_Write(const Params: PParamArray); lape_extdecl
begin
  PShape(Params^[0])^.Shape := PShapeType(Params^[1])^;
end;

//constructor Create();
procedure TShape_Init(const Params: PParamArray); lape_extdecl
begin
  PShape(Params^[0])^ := TShape.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TShape_Free(const Params: PParamArray); lape_extdecl
begin
  PShape(Params^[0])^.Free();
end;

procedure Register_TShape(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TShape', 'TGraphicControl');

    addGlobalFunc('procedure TShape.StyleChanged(Sender: TObject);', @TShape_StyleChanged);
    addClassVar('TShape', 'Brush', 'TBrush', @TShape_Brush_Read, @TShape_Brush_Write);
    addClassVar('TShape', 'Pen', 'TPen', @TShape_Pen_Read, @TShape_Pen_Write);
    addClassVar('TShape', 'Shape', 'TShapeType', @TShape_Shape_Read, @TShape_Shape_Write);
    addGlobalFunc('procedure TShape.Init(TheOwner: TComponent);', @TShape_Init);
    addGlobalFunc('procedure TShape.Free();', @TShape_Free);
  end;
end;

procedure RegisterLCLExtCtrls(Compiler: TLapeCompiler);
begin
   with Compiler do
   begin
     addGlobalType('(bvNone, bvLowered, bvRaised, bvSpace)', 'TGraphicsBevelCut');
     addGlobalType('TGraphicsBevelCut', 'TBevelCut');
     addGlobalType('TBevelCut', 'TPanelBevel');
     addGlobalType('Integer', 'TBevelWidth');
     addGlobalType('(stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle, stSquaredDiamond, stDiamond, stTriangle)', 'TShapeType');
   end;

   Register_TCustomTimer(Compiler);
   Register_TTimer(Compiler);
   Register_TCustomImage(Compiler);
   Register_TImage(Compiler);
   Register_TCustomPanel(Compiler);
   Register_TPanel(Compiler);
   Register_TShape(Compiler);
end;

end.

