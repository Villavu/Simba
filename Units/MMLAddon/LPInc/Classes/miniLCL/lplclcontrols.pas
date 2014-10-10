unit lplclcontrols;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils,Controls, lpcompiler, lptypes, lpClassHelper;

type
  PControl = ^TControl;
  //
  PWinControl = ^TWinControl;
  PKeyEvent = ^TkeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  //
  PCustomControl = ^TCustomControl;
  PGraphicControl = ^TGraphicControl;

procedure RegisterLCLControls(Compiler: TLapeCompiler);

implementation

uses lplclsystem, lplclgraphics, forms, lplclforms;

type
  PScrollBarKind = ^TScrollBarKind;
  PControlScrollBar = ^TcontrolScrollBar;
  PScrollingWinControl = ^TScrollingWinControl;
  PAlign = ^TAlign;
  PCursor = ^TCursor;

{TControl}
//procedure AdjustSize;
procedure TControl_AdjustSize(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.AdjustSize();
end;

//function AutoSizeDelayed: boolean;
procedure TControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.AutoSizeDelayed();
end;

//function AutoSizeDelayedReport: string;
procedure TControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PControl(Params^[0])^.AutoSizeDelayedReport();
end;

//function AutoSizeDelayedHandle: Boolean;
procedure TControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSizeDelayedHandle();
end;

//procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure TControl_SetBounds(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure TControl_SetInitialBounds(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.SetInitialBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer);
procedure TControl_SetBoundsKeepBase(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.SetBoundsKeepBase(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw: boolean;WithThemeSpace: boolean);
procedure TControl_GetPreferredSize(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//function GetDefaultWidth: integer;
procedure TControl_GetDefaultWidth(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultWidth();
end;

//function GetDefaultHeight: integer;
procedure TControl_GetDefaultHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultHeight();
end;

//function GetColorResolvingParent: Integer;
procedure TControl_GetColorResolvingParent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetColorResolvingParent();
end;

//function GetRGBColorResolvingParent: Integer;
procedure TControl_GetRGBColorResolvingParent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetRGBColorResolvingParent();
end;

//procedure InvalidatePreferredSize;
procedure TControl_InvalidatePreferredSize(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.InvalidatePreferredSize();
end;

//procedure UpdateBaseBounds(StoreBounds, StoreParentClientSize,UseLoadedValues: boolean);
procedure TControl_UpdateBaseBounds(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.UpdateBaseBounds(Pboolean(Params^[1])^, Pboolean(Params^[2])^, Pboolean(Params^[3])^);
end;

//Read: property BaseBounds: TRect read BaseBounds;
procedure TControl_BaseBounds_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PControl(Params^[0])^.BaseBounds;
end;

//Read: property ReadBounds: TRect read ReadBounds;
procedure TControl_ReadBounds_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PControl(Params^[0])^.ReadBounds;
end;

//procedure WriteLayoutDebugReport(const Prefix: string);
procedure TControl_WriteLayoutDebugReport(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.WriteLayoutDebugReport(PlpString(Params^[1])^);
end;

//function ShouldAutoAdjustLeftAndTop: Boolean;
procedure TControl_ShouldAutoAdjustLeftAndTop(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShouldAutoAdjustLeftAndTop();
end;

//function ShouldAutoAdjustWidthAndHeight: Boolean;
procedure TControl_ShouldAutoAdjustWidthAndHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShouldAutoAdjustWidthAndHeight();
end;

//constructor Create(TheOwner: TComponent);
procedure TControl_Init(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^ := TControl.Create(PComponent(Params^[1])^);
end;

//procedure BeforeDestruction;
procedure TControl_BeforeDestruction(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.BeforeDestruction();
end;

//procedure EditingDone;
procedure TControl_EditingDone(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.EditingDone();
end;

//procedure ExecuteDefaultAction;
procedure TControl_ExecuteDefaultAction(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ExecuteDefaultAction();
end;

//procedure ExecuteCancelAction;
procedure TControl_ExecuteCancelAction(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ExecuteCancelAction();
end;

//procedure BringToFront;
procedure TControl_BringToFront(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.BringToFront();
end;

//function HasParent: Boolean;
procedure TControl_HasParent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.HasParent();
end;

//function GetParentComponent: TComponent;
procedure TControl_GetParentComponent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponent(Result)^ := PControl(Params^[0])^.GetParentComponent();
end;

//function IsParentOf(AControl: TControl): boolean;
procedure TControl_IsParentOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsParentOf(PControl(Params^[1])^);
end;

//function GetTopParent: TControl;
procedure TControl_GetTopParent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PControl(Params^[0])^.GetTopParent();
end;

//function IsVisible: Boolean;
procedure TControl_IsVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsVisible();
end;

//function IsControlVisible: Boolean;
procedure TControl_IsControlVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControlVisible();
end;

//function IsEnabled: Boolean;
procedure TControl_IsEnabled(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsEnabled();
end;

//function IsParentColor: Boolean;
procedure TControl_IsParentColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentColor();
end;

//function IsParentFont: Boolean;
procedure TControl_IsParentFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentFont();
end;

//function FormIsUpdating: boolean;
procedure TControl_FormIsUpdating(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.FormIsUpdating();
end;

//function IsProcessingPaintMsg: boolean;
procedure TControl_IsProcessingPaintMsg(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsProcessingPaintMsg();
end;

//procedure Hide;
procedure TControl_Hide(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Hide();
end;

//procedure Refresh;
procedure TControl_Refresh(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Refresh();
end;

//procedure Repaint;
procedure TControl_Repaint(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Repaint();
end;

//procedure Invalidate;
procedure TControl_Invalidate(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Invalidate();
end;

//procedure SendToBack;
procedure TControl_SendToBack(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.SendToBack();
end;

//procedure UpdateRolesForForm;
procedure TControl_UpdateRolesForForm(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.UpdateRolesForForm();
end;

//procedure ActiveDefaultControlChanged(NewControl: TControl);
procedure TControl_ActiveDefaultControlChanged(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

//function  GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
procedure TControl_GetTextBuf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetTextBuf(PPChar(Params^[1])^, PInteger(Params^[2])^);
end;

//function  GetTextLen: Integer;
procedure TControl_GetTextLen(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetTextLen();
end;

//procedure SetTextBuf(Buffer: PChar);
procedure TControl_SetTextBuf(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.SetTextBuf(PPChar(Params^[1])^);
end;

//function  ScreenToClient(const APoint: TPoint): TPoint;
procedure TControl_ScreenToClient(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToClient(PPoint(Params^[1])^);
end;

//function  ClientToScreen(const APoint: TPoint): TPoint;
procedure TControl_ClientToScreen(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientToScreen(PPoint(Params^[1])^);
end;

//function  ScreenToControl(const APoint: TPoint): TPoint;
procedure TControl_ScreenToControl(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToControl(PPoint(Params^[1])^);
end;

//function  ControlToScreen(const APoint: TPoint): TPoint;
procedure TControl_ControlToScreen(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlToScreen(PPoint(Params^[1])^);
end;

//function GetChildsRect(Scrolled: boolean): TRect;
procedure TControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PControl(Params^[0])^.GetChildsRect(Pboolean(Params^[1])^);
end;

//procedure Show;
procedure TControl_Show(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Show();
end;

//procedure Update;
procedure TControl_Update(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Update();
end;

//function HandleObjectShouldBeVisible: boolean;
procedure TControl_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.HandleObjectShouldBeVisible();
end;

//function ParentDestroyingHandle: boolean;
procedure TControl_ParentDestroyingHandle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.ParentDestroyingHandle();
end;

//function ParentHandlesAllocated: boolean;
procedure TControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PControl(Params^[0])^.ParentHandlesAllocated();
end;

//procedure InitiateAction;
procedure TControl_InitiateAction(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.InitiateAction();
end;

//Read: property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
procedure TControl_AutoSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSize;
end;

//Write: property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
procedure TControl_AutoSize_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.AutoSize := PBoolean(Params^[1])^;
end;

//Read: property BoundsRect: TRect read BoundsRect write BoundsRect;
procedure TControl_BoundsRect_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRect;
end;

//Write: property BoundsRect: TRect read BoundsRect write BoundsRect;
procedure TControl_BoundsRect_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.BoundsRect := PRect(Params^[1])^;
end;

//Read: property BoundsRectForNewParent: TRect read BoundsRectForNewParent write BoundsRectForNewParent;
procedure TControl_BoundsRectForNewParent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRectForNewParent;
end;

//Write: property BoundsRectForNewParent: TRect read BoundsRectForNewParent write BoundsRectForNewParent;
procedure TControl_BoundsRectForNewParent_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.BoundsRectForNewParent := PRect(Params^[1])^;
end;

//Read: property Caption: String read Caption write Caption;
procedure TControl_Caption_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PControl(Params^[0])^.Caption;
end;

//Write: property Caption: String read Caption write Caption;
procedure TControl_Caption_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

//Read: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure TControl_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientHeight;
end;

//Write: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure TControl_ClientHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

//Read: property ClientOrigin: TPoint read ClientOrigin;
procedure TControl_ClientOrigin_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientOrigin;
end;

//Read: property ClientRect: TRect read ClientRect;
procedure TControl_ClientRect_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PControl(Params^[0])^.ClientRect;
end;

//Read: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure TControl_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientWidth;
end;

//Write: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure TControl_ClientWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

//Read: property Color: Integer read Color write Color;
procedure TControl_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.Color;
end;

//Write: property Color: Integer read Color write Color;
procedure TControl_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Color := PInteger(Params^[1])^;
end;

//Read: property ControlOrigin: TPoint read ControlOrigin;
procedure TControl_ControlOrigin_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlOrigin;
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure TControl_Enabled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure TControl_Enabled_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Font: TFont read Font write Font;
procedure TControl_Font_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFont(Result)^ := PControl(Params^[0])^.Font;
end;

//Write: property Font: TFont read Font write Font;
procedure TControl_Font_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property IsControl: Boolean read IsControl write IsControl;
procedure TControl_IsControl_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControl;
end;

//Write: property IsControl: Boolean read IsControl write IsControl;
procedure TControl_IsControl_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.IsControl := PBoolean(Params^[1])^;
end;

//Read: property MouseEntered: Boolean read MouseEntered;
procedure TControl_MouseEntered_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.MouseEntered;
end;

//Read: property OnChangeBounds: TNotifyEvent read OnChangeBounds write OnChangeBounds;
procedure TControl_OnChangeBounds_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnChangeBounds;
end;

//Write: property OnChangeBounds: TNotifyEvent read OnChangeBounds write OnChangeBounds;
procedure TControl_OnChangeBounds_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.OnChangeBounds := PNotifyEvent(Params^[1])^;
end;

procedure TControl_OnClick_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Component: TComponent;
begin
  Component := PControl(Params^[0])^.FindComponent('OnClickEvent');

  if (Assigned(Component)) then
    PClickWrapper(Result)^ := TOnClickWrapper(Component).InternalMethod
  else
    PClickWrapper(Result)^ := nil;
end;

procedure TControl_OnClick_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PControl(Params^[0])^.FindComponent('OnClickEvent');
  if (not Assigned(Component)) then
  begin
    Component := TOnClickWrapper.Create(PControl(Params^[0])^);
    Component.Name := 'OnClickEvent';
  end;

  with TOnClickWrapper(Component) do
  begin
    InternalMethod := PClickWrapper(Params^[1])^;
    PControl(Params^[0])^.OnClick := @OnClick;
  end;
end;

//Read: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure TControl_OnResize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnResize;
end;

//Write: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure TControl_OnResize_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure TControl_Align_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlign(Result)^ := PControl(Params^[0])^.Align;
end;

procedure TControl_Align_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure TControl_Visible_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure TControl_Visible_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//function UseRightToLeftAlignment: Boolean;
procedure TControl_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftAlignment();
end;

//function UseRightToLeftReading: Boolean;
procedure TControl_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftReading();
end;

//function UseRightToLeftScrollBar: Boolean;
procedure TControl_UseRightToLeftScrollBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftScrollBar();
end;

//function IsRightToLeft: Boolean;
procedure TControl_IsRightToLeft(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsRightToLeft();
end;

//Read: property Left: Integer read Left write Left;
procedure TControl_Left_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.Left;
end;

//Write: property Left: Integer read Left write Left;
procedure TControl_Left_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Left := PInteger(Params^[1])^;
end;

//Read: property Height: Integer read Height write Height;
procedure TControl_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure TControl_Height_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Top: Integer read Top write Top;
procedure TControl_Top_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.Top;
end;

//Write: property Top: Integer read Top write Top;
procedure TControl_Top_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Top := PInteger(Params^[1])^;
end;

//Read: property Width: Integer read Width write Width;
procedure TControl_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControl(Params^[0])^.Width;
end;

//Write: property Width: Integer read Width write Width;
procedure TControl_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Width := PInteger(Params^[1])^;
end;

// Procedure ShowHint();
procedure TControl_ShowHint(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ShowHint := true;
end;

//Read: property Parent: TWinControl;
procedure TControl_Parent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWinControl(Result)^ := PControl(Params^[0])^.Parent;
end;

//Write: property Parent: TWinControl;
procedure TControl_Parent_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Parent := PWinControl(Params^[1])^;
end;

procedure TControl_Hint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PControl(Params^[0])^.Hint;
end;

procedure TControl_Hint_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Hint := PLPString(Params^[1])^;
end;

procedure TControl_ShowHint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShowHint;
end;

procedure TControl_ShowHint_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure TControl_Cursor_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCursor(Result)^ := PControl(Params^[0])^.Cursor;
end;

procedure TControl_Cursor_Write(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

//procedure Free();
procedure TControl_Free(const Params: PParamArray); lape_extdecl
begin
  PControl(Params^[0])^.Free();
end;

procedure Register_TControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TControl', 'TComponent');
	
    addGlobalFunc('procedure TControl.AdjustSize();', @TControl_AdjustSize);
    addGlobalFunc('function TControl.AutoSizeDelayed(): boolean;', @TControl_AutoSizeDelayed);
    addGlobalFunc('function TControl.AutoSizeDelayedReport(): string;', @TControl_AutoSizeDelayedReport);
    addGlobalFunc('function TControl.AutoSizeDelayedHandle(): Boolean;', @TControl_AutoSizeDelayedHandle);
    addGlobalFunc('procedure TControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer);', @TControl_SetBounds);
    addGlobalFunc('procedure TControl.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);', @TControl_SetInitialBounds);
    addGlobalFunc('procedure TControl.SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer);', @TControl_SetBoundsKeepBase);
    addGlobalFunc('procedure TControl.GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw: boolean;WithThemeSpace: boolean);', @TControl_GetPreferredSize);
    addGlobalFunc('function TControl.GetDefaultWidth(): integer;', @TControl_GetDefaultWidth);
    addGlobalFunc('function TControl.GetDefaultHeight(): integer;', @TControl_GetDefaultHeight);
    addGlobalFunc('function TControl.GetColorResolvingParent(): Integer;', @TControl_GetColorResolvingParent);
    addGlobalFunc('function TControl.GetRGBColorResolvingParent(): Integer;', @TControl_GetRGBColorResolvingParent);
    addGlobalFunc('procedure TControl.InvalidatePreferredSize();', @TControl_InvalidatePreferredSize);
    addGlobalFunc('procedure TControl.UpdateBaseBounds(StoreBounds, StoreParentClientSize,UseLoadedValues: boolean);', @TControl_UpdateBaseBounds);
    addClassVar('TControl', 'BaseBounds', 'TRect', @TControl_BaseBounds_Read);
    addClassVar('TControl', 'ReadBounds', 'TRect', @TControl_ReadBounds_Read);
    addGlobalFunc('procedure TControl.WriteLayoutDebugReport(const Prefix: string);', @TControl_WriteLayoutDebugReport);
    addGlobalFunc('function TControl.ShouldAutoAdjustLeftAndTop(): Boolean;', @TControl_ShouldAutoAdjustLeftAndTop);
    addGlobalFunc('function TControl.ShouldAutoAdjustWidthAndHeight(): Boolean;', @TControl_ShouldAutoAdjustWidthAndHeight);
    addGlobalFunc('procedure TControl.Init(TheOwner: TComponent);', @TControl_Init);
    addGlobalFunc('procedure TControl.BeforeDestruction();', @TControl_BeforeDestruction);
    addGlobalFunc('procedure TControl.EditingDone();', @TControl_EditingDone);
    addGlobalFunc('procedure TControl.ExecuteDefaultAction();', @TControl_ExecuteDefaultAction);
    addGlobalFunc('procedure TControl.ExecuteCancelAction();', @TControl_ExecuteCancelAction);
    addGlobalFunc('procedure TControl.BringToFront();', @TControl_BringToFront);
    addGlobalFunc('function TControl.HasParent(): Boolean;', @TControl_HasParent);
    addGlobalFunc('function TControl.GetParentComponent(): TComponent;', @TControl_GetParentComponent);
    addGlobalFunc('function TControl.IsParentOf(AControl: TControl): boolean;', @TControl_IsParentOf);
    addGlobalFunc('function TControl.GetTopParent(): TControl;', @TControl_GetTopParent);
    addGlobalFunc('function TControl.IsVisible(): Boolean;', @TControl_IsVisible);
    addGlobalFunc('function TControl.IsControlVisible(): Boolean;', @TControl_IsControlVisible);
    addGlobalFunc('function TControl.IsEnabled(): Boolean;', @TControl_IsEnabled);
    addGlobalFunc('function TControl.IsParentColor(): Boolean;', @TControl_IsParentColor);
    addGlobalFunc('function TControl.IsParentFont(): Boolean;', @TControl_IsParentFont);
    addGlobalFunc('function TControl.FormIsUpdating(): boolean;', @TControl_FormIsUpdating);
    addGlobalFunc('function TControl.IsProcessingPaintMsg(): boolean;', @TControl_IsProcessingPaintMsg);
    addGlobalFunc('procedure TControl.Hide();', @TControl_Hide);
    addGlobalFunc('procedure TControl.Refresh();', @TControl_Refresh);
    addGlobalFunc('procedure TControl.Repaint();', @TControl_Repaint);
    addGlobalFunc('procedure TControl.Invalidate();', @TControl_Invalidate);
    addGlobalFunc('procedure TControl.SendToBack();', @TControl_SendToBack);
    addGlobalFunc('procedure TControl.UpdateRolesForForm();', @TControl_UpdateRolesForForm);
    addGlobalFunc('procedure TControl.ActiveDefaultControlChanged(NewControl: TControl);', @TControl_ActiveDefaultControlChanged);
    addGlobalFunc('function TControl.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;', @TControl_GetTextBuf);
    addGlobalFunc('function TControl.GetTextLen(): Integer;', @TControl_GetTextLen);
    addGlobalFunc('procedure TControl.SetTextBuf(Buffer: PChar);', @TControl_SetTextBuf);
    addGlobalFunc('function TControl.ScreenToClient(const APoint: TPoint): TPoint;', @TControl_ScreenToClient);
    addGlobalFunc('function TControl.ClientToScreen(const APoint: TPoint): TPoint;', @TControl_ClientToScreen);
    addGlobalFunc('function TControl.ScreenToControl(const APoint: TPoint): TPoint;', @TControl_ScreenToControl);
    addGlobalFunc('function TControl.ControlToScreen(const APoint: TPoint): TPoint;', @TControl_ControlToScreen);
    addGlobalFunc('function TControl.GetChildsRect(Scrolled: boolean): TRect;', @TControl_GetChildsRect);
    addGlobalFunc('procedure TControl.Show();', @TControl_Show);
    addGlobalFunc('procedure TControl.Update();', @TControl_Update);
    addGlobalFunc('function TControl.HandleObjectShouldBeVisible(): boolean;', @TControl_HandleObjectShouldBeVisible);
    addGlobalFunc('function TControl.ParentDestroyingHandle(): boolean;', @TControl_ParentDestroyingHandle);
    addGlobalFunc('function TControl.ParentHandlesAllocated(): boolean;', @TControl_ParentHandlesAllocated);
    addGlobalFunc('procedure TControl.InitiateAction();', @TControl_InitiateAction);
    addClassVar('TControl', 'Cursor', 'TCursor', @TControl_Cursor_Read, @TControl_Cursor_Write);      addClassVar('TControl', 'Align', 'TAlign', @TControl_Align_Read, @TControl_Align_Write);
    addClassVar('TControl', 'AutoSize', 'Boolean', @TControl_AutoSize_Read, @TControl_AutoSize_Write);
    addClassVar('TControl', 'BoundsRect', 'TRect', @TControl_BoundsRect_Read, @TControl_BoundsRect_Write);
    addClassVar('TControl', 'BoundsRectForNewParent', 'TRect', @TControl_BoundsRectForNewParent_Read, @TControl_BoundsRectForNewParent_Write);
    addClassVar('TControl', 'Caption', 'String', @TControl_Caption_Read, @TControl_Caption_Write);
    addClassVar('TControl', 'ClientHeight', 'Integer', @TControl_ClientHeight_Read, @TControl_ClientHeight_Write);
    addClassVar('TControl', 'ClientOrigin', 'TPoint', @TControl_ClientOrigin_Read);
    addClassVar('TControl', 'ClientRect', 'TRect', @TControl_ClientRect_Read);
    addClassVar('TControl', 'ClientWidth', 'Integer', @TControl_ClientWidth_Read, @TControl_ClientWidth_Write);
    addClassVar('TControl', 'Color', 'Integer', @TControl_Color_Read, @TControl_Color_Write);
    addClassVar('TControl', 'ControlOrigin', 'TPoint', @TControl_ControlOrigin_Read);
    addClassVar('TControl', 'Enabled', 'Boolean', @TControl_Enabled_Read, @TControl_Enabled_Write);
    addClassVar('TControl', 'Font', 'TFont', @TControl_Font_Read, @TControl_Font_Write);
    addClassVar('TControl', 'IsControl', 'Boolean', @TControl_IsControl_Read, @TControl_IsControl_Write);
    addClassVar('TControl', 'MouseEntered', 'Boolean', @TControl_MouseEntered_Read);
    addClassVar('TControl', 'OnChangeBounds', 'TNotifyEvent', @TControl_OnChangeBounds_Read, @TControl_OnChangeBounds_Write);
    addClassVar('TControl', 'OnClick', 'TNotifyEvent', @TControl_OnClick_Read, @TControl_OnClick_Write);
    addClassVar('TControl', 'OnResize', 'TNotifyEvent', @TControl_OnResize_Read, @TControl_OnResize_Write);
    addClassVar('TControl', 'Visible', 'Boolean', @TControl_Visible_Read, @TControl_Visible_Write);
    addClassVar('TControl', 'ShowHint', 'Boolean', @TControl_ShowHint_Read, @TControl_ShowHint_Write);
    addGlobalFunc('function TControl.UseRightToLeftAlignment(): Boolean;', @TControl_UseRightToLeftAlignment);
    addGlobalFunc('function TControl.UseRightToLeftReading(): Boolean;', @TControl_UseRightToLeftReading);
    addGlobalFunc('function TControl.UseRightToLeftScrollBar(): Boolean;', @TControl_UseRightToLeftScrollBar);
    addGlobalFunc('function TControl.IsRightToLeft(): Boolean;', @TControl_IsRightToLeft);
    addClassVar('TControl', 'Left', 'Integer', @TControl_Left_Read, @TControl_Left_Write);
    addClassVar('TControl', 'Height', 'Integer', @TControl_Height_Read, @TControl_Height_Write);
    addClassVar('TControl', 'Top', 'Integer', @TControl_Top_Read, @TControl_Top_Write);
    addClassVar('TControl', 'Width', 'Integer', @TControl_Width_Read, @TControl_Width_Write);
    addGlobalFunc('procedure TControl.ShowHint();', @TControl_ShowHint);
    addClassVar('TControl', 'Hint', 'String', @TControl_Hint_Read, @TControl_Hint_Write);
    addClassVar('TControl', 'Parent', 'TControl', @TControl_Parent_Read, @TControl_Parent_Write); //FIXME: Should be OS-Depend TControl
    addGlobalFunc('procedure TControl.Free();', @TControl_Free);
  end;
end;

{TWinControl}
//Read: property BorderWidth: integer read BorderWidth write BorderWidth;
procedure TWinControl_BorderWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BorderWidth;
end;

//Write: property BorderWidth: integer read BorderWidth write BorderWidth;
procedure TWinControl_BorderWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.BorderWidth := Pinteger(Params^[1])^;
end;

//Read: property BoundsLockCount: integer read BoundsLockCount;
procedure TWinControl_BoundsLockCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BoundsLockCount;
end;

//Read: property Brush: TBrush read Brush;
procedure TWinControl_Brush_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBrush(Result)^ := PWinControl(Params^[0])^.Brush;
end;

//Read: property CachedClientHeight: integer read ClientHeight;
procedure TWinControl_CachedClientHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientHeight;
end;

//Read: property CachedClientWidth: integer read ClientWidth;
procedure TWinControl_CachedClientWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientWidth;
end;

//Read: property ControlCount: Integer read ControlCount;
procedure TWinControl_ControlCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.ControlCount;
end;

//Read: property DoubleBuffered: Boolean read DoubleBuffered write DoubleBuffered;
procedure TWinControl_DoubleBuffered_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.DoubleBuffered;
end;

//Write: property DoubleBuffered: Boolean read DoubleBuffered write DoubleBuffered;
procedure TWinControl_DoubleBuffered_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.DoubleBuffered := PBoolean(Params^[1])^;
end;

//Read: property Handle: THandle read Handle write Handle;
procedure TWinControl_Handle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read Handle write Handle;
procedure TWinControl_Handle_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//Read: property IsResizing: Boolean read IsResizing;
procedure TWinControl_IsResizing_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.IsResizing;
end;

//Read: property TabOrder: Integer read TabOrder write Taborder ;
procedure TWinControl_TabOrder_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.TabOrder;
end;

//Write: property TabOrder: Integer read TabOrder write Taborder ;
procedure TWinControl_TabOrder_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.TabOrder := PInteger(Params^[1])^;
end;

//Read: property TabStop: Boolean read TabStop write TabStop;
procedure TWinControl_TabStop_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.TabStop;
end;

//Write: property TabStop: Boolean read TabStop write TabStop;
procedure TWinControl_TabStop_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.TabStop := PBoolean(Params^[1])^;
end;

//Read: property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
procedure TWinControl_OnEnter_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnEnter;
end;

//Write: property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
procedure TWinControl_OnEnter_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

//Read: property OnExit: TNotifyEvent read FOnExit write FOnExit;
procedure TWinControl_OnExit_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnExit;
end;

//Write: property OnExit: TNotifyEvent read FOnExit write FOnExit;
procedure TWinControl_OnExit_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure TWinControl_OnKeyDown_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PWinControl(Params^[0])^.FindComponent('OnKeyDownEvent');
  if (not Assigned(Component)) then
  begin
    Component := TOnKeyEventWrapper.Create(PWinControl(Params^[0])^);
    Component.Name := 'OnKeyDownEvent';
  end;

  with TOnKeyEventWrapper(Component) do
  begin
    InternalMethod := PKeyEventWrapper(Params^[1])^;
    PWinControl(Params^[0])^.OnKeyDown := @KeyEvent;
  end;
end;

procedure TWinControl_OnKeyPress_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PWinControl(Params^[0])^.FindComponent('OnKeyPressEvent');
  if (not Assigned(Component)) then
  begin
    Component := TOnKeyPressWrapper.Create(PWinControl(Params^[0])^);
    Component.Name := 'OnKeyPressEvent';
  end;

  with TOnKeyPressWrapper(Component) do
  begin
    InternalMethod := PKeyPressEventWrapper(Params^[1])^;
    PWinControl(Params^[0])^.OnKeyPress := @KeyPress;
  end;
end;

procedure TWinControl_OnKeyUp_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PWinControl(Params^[0])^.FindComponent('OnKeyUpEvent');
  if (not Assigned(Component)) then
  begin
    Component := TOnKeyEventWrapper.Create(PWinControl(Params^[0])^);
    Component.Name := 'OnKeyUpEvent';
  end;

  with TOnKeyEventWrapper(Component) do
  begin
    InternalMethod := PKeyEventWrapper(Params^[1])^;
    PWinControl(Params^[0])^.OnKeyUp := @KeyEvent;
  end;
end;

//Read: property ParentWindow: THandle read FParentWindow write SetParentWindow;
procedure TWinControl_ParentWindow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.ParentWindow;
end;

//Write: property ParentWindow: THandle read FParentWindow write SetParentWindow;
procedure TWinControl_ParentWindow_Write(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.ParentWindow := PHandle(Params^[1])^;
end;

//Read: property Showing: Boolean read Showing;
procedure TWinControl_Showing_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Showing;
end;

//Read: property VisibleDockClientCount: Integer read VisibleDockClientCount;
procedure TWinControl_VisibleDockClientCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.VisibleDockClientCount;
end;

//function AutoSizeDelayed: boolean;
procedure TWinControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayed();
end;

//function AutoSizeDelayedReport: string;
procedure TWinControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedReport();
end;

//function AutoSizeDelayedHandle: Boolean;
procedure TWinControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedHandle();
end;

//procedure BeginUpdateBounds;
procedure TWinControl_BeginUpdateBounds(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.BeginUpdateBounds();
end;

//procedure EndUpdateBounds;
procedure TWinControl_EndUpdateBounds(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.EndUpdateBounds();
end;

//procedure LockRealizeBounds;
procedure TWinControl_LockRealizeBounds(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.LockRealizeBounds();
end;

//procedure UnlockRealizeBounds;
procedure TWinControl_UnlockRealizeBounds(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.UnlockRealizeBounds();
end;

//function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;
procedure TWinControl_ControlAtPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

//function ControlAtPos(const Pos: TPoint;AllowDisabled, AllowWinControls: Boolean): TControl;
procedure TWinControl_ControlAtPosEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

//function  ContainsControl(Control: TControl): Boolean;
procedure TWinControl_ContainsControl(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.ContainsControl(PControl(Params^[1])^);
end;

//procedure DoAdjustClientRectChange(const InvalidateRect: Boolean);
procedure TWinControl_DoAdjustClientRectChange(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.DoAdjustClientRectChange(PBoolean(Params^[1])^);
end;

//procedure InvalidateClientRectCache(WithChildControls: boolean);
procedure TWinControl_InvalidateClientRectCache(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.InvalidateClientRectCache(Pboolean(Params^[1])^);
end;

//function ClientRectNeedsInterfaceUpdate: boolean;
procedure TWinControl_ClientRectNeedsInterfaceUpdate(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ClientRectNeedsInterfaceUpdate();
end;

//procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
procedure TWinControl_SetBounds(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function  GetChildsRect(Scrolled: boolean): TRect;
procedure TWinControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PWinControl(Params^[0])^.GetChildsRect(Pboolean(Params^[1])^);
end;

//procedure DisableAlign;
procedure TWinControl_DisableAlign(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.DisableAlign();
end;

//procedure EnableAlign;
procedure TWinControl_EnableAlign(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.EnableAlign();
end;

//procedure ReAlign;
procedure TWinControl_ReAlign(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.ReAlign();
end;

//procedure ScrollBy(DeltaX, DeltaY: Integer);
procedure TWinControl_ScrollBy(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure WriteLayoutDebugReport(const Prefix: string);
procedure TWinControl_WriteLayoutDebugReport(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.WriteLayoutDebugReport(PlpString(Params^[1])^);
end;

//constructor Create(TheOwner: TComponent);
procedure TWinControl_Init(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^ := TWinControl.Create(PComponent(Params^[1])^);
end;

//constructor CreateParented(AParentWindow: Thandle);
procedure TWinControl_CreateParented(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^ := TWinControl.CreateParented(Phandle(Params^[1])^);
end;

//function CanFocus: Boolean;
procedure TWinControl_CanFocus(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.CanFocus();
end;

//function GetControlIndex(AControl: TControl): integer;
procedure TWinControl_GetControlIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.GetControlIndex(PControl(Params^[1])^);
end;

//procedure SetControlIndex(AControl: TControl; NewIndex: integer);
procedure TWinControl_SetControlIndex(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.SetControlIndex(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

//function Focused: Boolean;
procedure TWinControl_Focused(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Focused();
end;

//function PerformTab(ForwardTab: boolean): boolean;
procedure TWinControl_PerformTab(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.PerformTab(Pboolean(Params^[1])^);
end;

//function FindChildControl(const ControlName: String): TControl;
procedure TWinControl_FindChildControl(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PWinControl(Params^[0])^.FindChildControl(PlpString(Params^[1])^);
end;

//procedure SelectNext(CurControl: TWinControl;GoForward, CheckTabStop: Boolean);
procedure TWinControl_SelectNext(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.SelectNext(PWinControl(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

//function  GetTextLen: Integer;
procedure TWinControl_GetTextLen(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.GetTextLen();
end;

//procedure Invalidate;
procedure TWinControl_Invalidate(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.Invalidate();
end;

//procedure AddControl;
procedure TWinControl_AddControl(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.AddControl();
end;

//procedure InsertControl(AControl: TControl);
procedure TWinControl_InsertControl(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^);
end;

//procedure InsertControl(AControl: TControl; Index: integer);
procedure TWinControl_InsertControlEx(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure RemoveControl(AControl: TControl);
procedure TWinControl_RemoveControl(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.RemoveControl(PControl(Params^[1])^);
end;

//procedure Repaint;
procedure TWinControl_Repaint(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.Repaint();
end;

//procedure Update;
procedure TWinControl_Update(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.Update();
end;

//procedure SetFocus;
procedure TWinControl_SetFocus(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.SetFocus();
end;

//procedure FlipChildren(AllLevels: Boolean);
procedure TWinControl_FlipChildren(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.FlipChildren(PBoolean(Params^[1])^);
end;

//procedure ScaleBy(Multiplier, Divider: Integer);
procedure TWinControl_ScaleBy(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.ScaleBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function GetDockCaption(AControl: TControl): String;
procedure TWinControl_GetDockCaption(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PWinControl(Params^[0])^.GetDockCaption(PControl(Params^[1])^);
end;

//procedure UpdateDockCaption(Exclude: TControl);
procedure TWinControl_UpdateDockCaption(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.UpdateDockCaption(PControl(Params^[1])^);
end;

//function HandleAllocated: Boolean;
procedure TWinControl_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.HandleAllocated();
end;

//function ParentHandlesAllocated: boolean;
procedure TWinControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ParentHandlesAllocated();
end;

//procedure HandleNeeded;
procedure TWinControl_HandleNeeded(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.HandleNeeded();
end;

//function BrushCreated: Boolean;
procedure TWinControl_BrushCreated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.BrushCreated();
end;

//procedure PaintTo(ACanvas: TCanvas; X, Y: Integer);
procedure TWinControl_PaintTo(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.PaintTo(PCanvas(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure SetShape(AShape: TBitmap);
procedure TWinControl_SetShape(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.SetShape(PBitmap(Params^[1])^);
end;

//procedure Free();
procedure TWinControl_Free(const Params: PParamArray); lape_extdecl
begin
  PWinControl(Params^[0])^.Free();
end;

procedure Register_TWinControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TWinControl', 'TControl');

    addClassVar('TWinControl', 'BorderWidth', 'integer', @TWinControl_BorderWidth_Read, @TWinControl_BorderWidth_Write);
    addClassVar('TWinControl', 'BoundsLockCount', 'integer', @TWinControl_BoundsLockCount_Read);
    addClassVar('TWinControl', 'Brush', 'TBrush', @TWinControl_Brush_Read);
    addClassVar('TWinControl', 'CachedClientHeight', 'integer', @TWinControl_CachedClientHeight_Read);
    addClassVar('TWinControl', 'CachedClientWidth', 'integer', @TWinControl_CachedClientWidth_Read);
    addClassVar('TWinControl', 'ControlCount', 'Integer', @TWinControl_ControlCount_Read);
    addClassVar('TWinControl', 'DoubleBuffered', 'Boolean', @TWinControl_DoubleBuffered_Read, @TWinControl_DoubleBuffered_Write);
    addClassVar('TWinControl', 'Handle', 'THandle', @TWinControl_Handle_Read, @TWinControl_Handle_Write);
    addClassVar('TWinControl', 'IsResizing', 'Boolean', @TWinControl_IsResizing_Read);
    addClassVar('TWinControl', 'TabOrder', 'Integer', @TWinControl_TabOrder_Read, @TWinControl_TabOrder_Write);
    addClassVar('TWinControl', 'TabStop', 'Boolean', @TWinControl_TabStop_Read, @TWinControl_TabStop_Write);
    addClassVar('TWinControl', 'OnEnter', 'TNotifyEvent', @TWinControl_OnEnter_Read, @TWinControl_OnEnter_Write);
    addClassVar('TWinControl', 'OnExit', 'TNotifyEvent', @TWinControl_OnExit_Read, @TWinControl_OnExit_Write);
    addClassVar('TWinControl', 'OnKeyDown', 'TKeyEvent', nil, @TWinControl_OnKeyDown_Write);
    addClassVar('TWinControl', 'OnKeyPress', 'TKeyPressEvent', nil, @TWinControl_OnKeyPress_Write);
    addClassVar('TWinControl', 'OnKeyUp', 'TKeyEvent', nil, @TWinControl_OnKeyUp_Write);
    addClassVar('TWinControl', 'ParentWindow', 'THandle', @TWinControl_ParentWindow_Read, @TWinControl_ParentWindow_Write);
    addClassVar('TWinControl', 'Showing', 'Boolean', @TWinControl_Showing_Read);
    addClassVar('TWinControl', 'VisibleDockClientCount', 'Integer', @TWinControl_VisibleDockClientCount_Read, nil);
    addGlobalFunc('function TWinControl.AutoSizeDelayed(): boolean;', @TWinControl_AutoSizeDelayed);
    addGlobalFunc('function TWinControl.AutoSizeDelayedReport(): string;', @TWinControl_AutoSizeDelayedReport);
    addGlobalFunc('function TWinControl.AutoSizeDelayedHandle(): Boolean;', @TWinControl_AutoSizeDelayedHandle);
    addGlobalFunc('procedure TWinControl.BeginUpdateBounds();', @TWinControl_BeginUpdateBounds);
    addGlobalFunc('procedure TWinControl.EndUpdateBounds();', @TWinControl_EndUpdateBounds);
    addGlobalFunc('procedure TWinControl.LockRealizeBounds();', @TWinControl_LockRealizeBounds);
    addGlobalFunc('procedure TWinControl.UnlockRealizeBounds();', @TWinControl_UnlockRealizeBounds);
    addGlobalFunc('function TWinControl.ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;', @TWinControl_ControlAtPos);
    addGlobalFunc('function TWinControl.ControlAtPos(const Pos: TPoint;AllowDisabled, AllowWinControls: Boolean): TControl; overload;', @TWinControl_ControlAtPosEx);
    addGlobalFunc('function TWinControl.ContainsControl(Control: TControl): Boolean;', @TWinControl_ContainsControl);
    addGlobalFunc('procedure TWinControl.DoAdjustClientRectChange(const InvalidateRect: Boolean);', @TWinControl_DoAdjustClientRectChange);
    addGlobalFunc('procedure TWinControl.InvalidateClientRectCache(WithChildControls: boolean);', @TWinControl_InvalidateClientRectCache);
    addGlobalFunc('function TWinControl.ClientRectNeedsInterfaceUpdate(): boolean;', @TWinControl_ClientRectNeedsInterfaceUpdate);
    addGlobalFunc('procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);', @TWinControl_SetBounds);
    addGlobalFunc('function TWinControl.GetChildsRect(Scrolled: boolean): TRect;', @TWinControl_GetChildsRect);
    addGlobalFunc('procedure TWinControl.DisableAlign();', @TWinControl_DisableAlign);
    addGlobalFunc('procedure TWinControl.EnableAlign();', @TWinControl_EnableAlign);
    addGlobalFunc('procedure TWinControl.ReAlign();', @TWinControl_ReAlign);
    addGlobalFunc('procedure TWinControl.ScrollBy(DeltaX, DeltaY: Integer);', @TWinControl_ScrollBy);
    addGlobalFunc('procedure TWinControl.WriteLayoutDebugReport(const Prefix: string);', @TWinControl_WriteLayoutDebugReport);
    addGlobalFunc('procedure TWinControl.Init(TheOwner: TComponent);', @TWinControl_Init);
    addGlobalFunc('procedure TWinControl.CreateParented(AParentWindow: Thandle);', @TWinControl_CreateParented);
    addGlobalFunc('function TWinControl.CanFocus(): Boolean;', @TWinControl_CanFocus);
    addGlobalFunc('function TWinControl.GetControlIndex(AControl: TControl): integer;', @TWinControl_GetControlIndex);
    addGlobalFunc('procedure TWinControl.SetControlIndex(AControl: TControl; NewIndex: integer);', @TWinControl_SetControlIndex);
    addGlobalFunc('function TWinControl.Focused(): Boolean;', @TWinControl_Focused);
    addGlobalFunc('function TWinControl.PerformTab(ForwardTab: boolean): boolean;', @TWinControl_PerformTab);
    addGlobalFunc('function TWinControl.FindChildControl(const ControlName: String): TControl;', @TWinControl_FindChildControl);
    addGlobalFunc('procedure TWinControl.SelectNext(CurControl: TWinControl;GoForward, CheckTabStop: Boolean);', @TWinControl_SelectNext);
    addGlobalFunc('function TWinControl.GetTextLen(): Integer;', @TWinControl_GetTextLen);
    addGlobalFunc('procedure TWinControl.Invalidate();', @TWinControl_Invalidate);
    addGlobalFunc('procedure TWinControl.AddControl();', @TWinControl_AddControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl);', @TWinControl_InsertControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl; Index: integer); overload;', @TWinControl_InsertControlEx);
    addGlobalFunc('procedure TWinControl.RemoveControl(AControl: TControl);', @TWinControl_RemoveControl);
    addGlobalFunc('procedure TWinControl.Repaint();', @TWinControl_Repaint);
    addGlobalFunc('procedure TWinControl.Update();', @TWinControl_Update);
    addGlobalFunc('procedure TWinControl.SetFocus();', @TWinControl_SetFocus);
    addGlobalFunc('procedure TWinControl.FlipChildren(AllLevels: Boolean);', @TWinControl_FlipChildren);
    addGlobalFunc('procedure TWinControl.ScaleBy(Multiplier, Divider: Integer);', @TWinControl_ScaleBy);
    addGlobalFunc('function TWinControl.GetDockCaption(AControl: TControl): String;', @TWinControl_GetDockCaption);
    addGlobalFunc('procedure TWinControl.UpdateDockCaption(Exclude: TControl);', @TWinControl_UpdateDockCaption);
    addGlobalFunc('function TWinControl.HandleAllocated(): Boolean;', @TWinControl_HandleAllocated);
    addGlobalFunc('function TWinControl.ParentHandlesAllocated(): boolean;', @TWinControl_ParentHandlesAllocated);
    addGlobalFunc('procedure TWinControl.HandleNeeded();', @TWinControl_HandleNeeded);
    addGlobalFunc('function TWinControl.BrushCreated(): Boolean;', @TWinControl_BrushCreated);
    addGlobalFunc('procedure TWinControl.PaintTo(ACanvas: TCanvas; X, Y: Integer);', @TWinControl_PaintTo);
    addGlobalFunc('procedure TWinControl.SetShape(AShape: TBitmap);', @TWinControl_SetShape);
    addGlobalFunc('procedure TWinControl.Free();', @TWinControl_Free);
  end;
end;

{TCustomControl}

//constructor Create(AOwner: TComponent);
procedure TCustomControl_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomControl(Params^[0])^ := TCustomControl.Create(PComponent(Params^[1])^);
end;

//Read: property Canvas: TCanvas read Canvas write Canvas;
procedure TCustomControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PCustomControl(Params^[0])^.Canvas;
end;

//Write: property Canvas: TCanvas read Canvas write Canvas;
procedure TCustomControl_Canvas_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomControl(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

//Read: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure TCustomControl_OnPaint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomControl(Params^[0])^.OnPaint;
end;

//Write: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure TCustomControl_OnPaint_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomControl(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure TCustomControl_BorderStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomControl(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

//procedure Free();
procedure TCustomControl_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomControl(Params^[0])^.Free();
end;

procedure Register_TCustomControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomControl', 'TWinControl');

    addGlobalFunc('procedure TCustomControl.Init(AOwner: TComponent);', @TCustomControl_Init);
    addClassVar('TCustomControl', 'Canvas', 'TCanvas', @TCustomControl_Canvas_Read, @TCustomControl_Canvas_Write);
    addClassVar('TCustomControl', 'OnPaint', 'TNotifyEvent', @TCustomControl_OnPaint_Read, @TCustomControl_OnPaint_Write);
    addClassVar('TCustomControl', 'BorderStyle', 'TFormBorderStyle', nil, @TCustomControl_BorderStyle_Write);
    addGlobalFunc('procedure TCustomControl.Free();', @TCustomControl_Free);
  end;
end;

{TControlScrollBar}
//constructor Create(AControl: TWinControl; AKind: TScrollBarKind);
procedure TControlScrollBar_Init(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^ := TControlScrollBar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

//procedure Assign(Source: TPersistent);
procedure TControlScrollBar_Assign(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//function IsScrollBarVisible: Boolean;
procedure TControlScrollBar_IsScrollBarVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.IsScrollBarVisible();
end;

//function ScrollPos: Integer;
procedure TControlScrollBar_ScrollPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.ScrollPos();
end;

//function GetOtherScrollBar: TControlScrollBar;
procedure TControlScrollBar_GetOtherScrollBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControlScrollBar(Result)^ := PControlScrollBar(Params^[0])^.GetOtherScrollBar();
end;

//Read: property Size: integer read Size;
procedure TControlScrollBar_Size_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.Size;
end;

//function ClientSize: integer;
procedure TControlScrollBar_ClientSize(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSize();
end;

//function ClientSizeWithBar: integer;
procedure TControlScrollBar_ClientSizeWithBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithBar();
end;

//function ClientSizeWithoutBar: integer;
procedure TControlScrollBar_ClientSizeWithoutBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithoutBar();
end;

//Read: property Increment: Integer read Increment write Increment;
procedure TControlScrollBar_Increment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Increment;
end;

//Write: property Increment: Integer read Increment write Increment;
procedure TControlScrollBar_Increment_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Increment := PInteger(Params^[1])^;
end;

//Read: property Page: Integer read Page write Page;
procedure TControlScrollBar_Page_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Page;
end;

//Write: property Page: Integer read Page write Page;
procedure TControlScrollBar_Page_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Page := PInteger(Params^[1])^;
end;

//Read: property Smooth: Boolean read Smooth write Smooth;
procedure TControlScrollBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Smooth;
end;

//Write: property Smooth: Boolean read Smooth write Smooth;
procedure TControlScrollBar_Smooth_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Smooth := PBoolean(Params^[1])^;
end;

//Read: property Position: Integer read Position write Position;
procedure TControlScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read Position write Position;
procedure TControlScrollBar_Position_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property Range: Integer read Range write Range;
procedure TControlScrollBar_Range_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Range;
end;

//Write: property Range: Integer read Range write Range;
procedure TControlScrollBar_Range_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Range := PInteger(Params^[1])^;
end;

//Read: property Tracking: Boolean read Tracking write Tracking;
procedure TControlScrollBar_Tracking_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Tracking;
end;

//Write: property Tracking: Boolean read Tracking write Tracking;
procedure TControlScrollBar_Tracking_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Tracking := PBoolean(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure TControlScrollBar_Visible_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure TControlScrollBar_Visible_Write(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TControlScrollBar_Free(const Params: PParamArray); lape_extdecl
begin
  PControlScrollBar(Params^[0])^.Free();
end;

procedure Register_TControlScrollBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TControlScrollBar', 'TPersistent');

    addGlobalFunc('procedure TControlScrollBar.Init(AControl: TWinControl; AKind: TScrollBarKind);', @TControlScrollBar_Init);
    addGlobalFunc('procedure TControlScrollBar.Assign(Source: TPersistent);', @TControlScrollBar_Assign);
    addGlobalFunc('function TControlScrollBar.IsScrollBarVisible(): Boolean;', @TControlScrollBar_IsScrollBarVisible);
    addGlobalFunc('function TControlScrollBar.ScrollPos(): Integer;', @TControlScrollBar_ScrollPos);
    addGlobalFunc('function TControlScrollBar.GetOtherScrollBar(): TControlScrollBar;', @TControlScrollBar_GetOtherScrollBar);
    addClassVar('TControlScrollBar', 'Size', 'integer', @TControlScrollBar_Size_Read);
    addGlobalFunc('function TControlScrollBar.ClientSize(): integer;', @TControlScrollBar_ClientSize);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithBar(): integer;', @TControlScrollBar_ClientSizeWithBar);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithoutBar(): integer;', @TControlScrollBar_ClientSizeWithoutBar);
    addClassVar('TControlScrollBar', 'Increment', 'Integer', @TControlScrollBar_Increment_Read, @TControlScrollBar_Increment_Write);
    addClassVar('TControlScrollBar', 'Page', 'Integer', @TControlScrollBar_Page_Read, @TControlScrollBar_Page_Write);
    addClassVar('TControlScrollBar', 'Smooth', 'Boolean', @TControlScrollBar_Smooth_Read, @TControlScrollBar_Smooth_Write);
    addClassVar('TControlScrollBar', 'Position', 'Integer', @TControlScrollBar_Position_Read, @TControlScrollBar_Position_Write);
    addClassVar('TControlScrollBar', 'Range', 'Integer', @TControlScrollBar_Range_Read, @TControlScrollBar_Range_Write);
    addClassVar('TControlScrollBar', 'Tracking', 'Boolean', @TControlScrollBar_Tracking_Read, @TControlScrollBar_Tracking_Write);
    addClassVar('TControlScrollBar', 'Visible', 'Boolean', @TControlScrollBar_Visible_Read, @TControlScrollBar_Visible_Write);
    addGlobalFunc('procedure TControlScrollBar.Free();', @TControlScrollBar_Free);
  end;
end;
{TScrollingWinControl}
//constructor Create(TheOwner : TComponent);
procedure TScrollingWinControl_Init(const Params: PParamArray); lape_extdecl
begin
  PScrollingWinControl(Params^[0])^ := TScrollingWinControl.Create(PComponent(Params^[1])^);
end;

//procedure UpdateScrollbars;
procedure TScrollingWinControl_UpdateScrollbars(const Params: PParamArray); lape_extdecl
begin
  PScrollingWinControl(Params^[0])^.UpdateScrollbars();
end;

//procedure ScrollBy(DeltaX, DeltaY: Integer);
procedure TScrollingWinControl_ScrollBy(const Params: PParamArray); lape_extdecl
begin
  PScrollingWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//Read: property HorzScrollBar: TControlScrollBar read HorzScrollBar write HorzScrollBar;
procedure TScrollingWinControl_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.HorzScrollBar;
end;

//Write: property HorzScrollBar: TControlScrollBar read HorzScrollBar write HorzScrollBar;
procedure TScrollingWinControl_HorzScrollBar_Write(const Params: PParamArray); lape_extdecl
begin
  PScrollingWinControl(Params^[0])^.HorzScrollBar := PControlScrollBar(Params^[1])^;
end;

//Read: property VertScrollBar: TControlScrollBar read VertScrollBar write VertScrollBar;
procedure TScrollingWinControl_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.VertScrollBar;
end;

//Write: property VertScrollBar: TControlScrollBar read VertScrollBar write VertScrollBar;
procedure TScrollingWinControl_VertScrollBar_Write(const Params: PParamArray); lape_extdecl
begin
  PScrollingWinControl(Params^[0])^.VertScrollBar := PControlScrollBar(Params^[1])^;
end;

//procedure Free();
procedure TScrollingWinControl_Free(const Params: PParamArray); lape_extdecl
begin
  PScrollingWinControl(Params^[0])^.Free();
end;

procedure Register_TScrollingWinControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TScrollingWinControl', 'TCustomControl');

    addGlobalFunc('procedure TScrollingWinControl.Init(TheOwner : TComponent);', @TScrollingWinControl_Init);
    addGlobalFunc('procedure TScrollingWinControl.UpdateScrollbars();', @TScrollingWinControl_UpdateScrollbars);
    addGlobalFunc('procedure TScrollingWinControl.ScrollBy(DeltaX, DeltaY: Integer);', @TScrollingWinControl_ScrollBy);
    addClassVar('TScrollingWinControl', 'HorzScrollBar', 'TControlScrollBar', @TScrollingWinControl_HorzScrollBar_Read, @TScrollingWinControl_HorzScrollBar_Write);
    addClassVar('TScrollingWinControl', 'VertScrollBar', 'TControlScrollBar', @TScrollingWinControl_VertScrollBar_Read, @TScrollingWinControl_VertScrollBar_Write);
    addGlobalFunc('procedure TScrollingWinControl.Free();', @TScrollingWinControl_Free);
  end;
end;

//Read: property Canvas: TCanvas read Canvas
procedure TGraphicControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PGraphicControl(Params^[0])^.Canvas;
end;

//procedure Update();
procedure TGraphicControl_Update(const Params: PParamArray); lape_extdecl
begin
  PGraphicControl(Params^[0])^.Update();
end;

procedure TGraphicControl_Align_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlign(Result)^ := PGraphicControl(Params^[0])^.Align;
end;

procedure TGraphicControl_Align_Write(const Params: PParamArray); lape_extdecl
begin
  PGraphicControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

procedure Register_TGraphicControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGraphicControl', 'TControl');

    addGlobalFunc('procedure TGraphicControl.Update();', @TGraphicControl_Update);
    addClassVar('TGraphicControl', 'Canvas', 'TCanvas', @TGraphicControl_Canvas_Read);
    addClassVar('TGraphicControl', 'Alignment', 'TAlign', @TGraphicControl_Align_Read, @TGraphicControl_Align_Write);
  end;
end;

procedure RegisterLCLControls(Compiler: TLapeCompiler);
begin
  with Compiler do
   begin
     addGlobalType('(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2)', 'TShiftStateEnum');
     addGlobalType('set of TShiftStateEnum', 'TShiftState');
     addGlobalType('procedure(Sender: TObject; var Key: Word; Shift: TShiftState)','TKeyEvent');
     addGlobalType('procedure(Sender: TObject; var Key: char)','TKeyPressEvent');
     addGlobalType('(mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2)','TMouseButton');
     addGlobalType('procedure(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer)','TMouseEvent');
     addGlobalType('procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer)', 'TMouseMoveEvent');
     addGlobalType('(sbHorizontal, sbVertical)','TScrollBarKind');
     addGlobalType('(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom)', 'TAlign');
     addGlobalType('(bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin)','TFormBorderStyle');
     addGlobalType('Integer', 'TCursor');
     addGlobalVar(crNone, 'crNone').isConstant := True;
     addGlobalVar(crCross, 'crCross').isConstant := True;
     addGlobalVar(crHandPoint, 'crHandPoint').isConstant := True;
     addGlobalVar(crIBeam, 'crIBeam').isConstant := True;
   end;

  Register_TControl(Compiler);
  Register_TWinControl(Compiler);
  Register_TCustomControl(Compiler);
  Register_TControlScrollBar(Compiler);
  Register_TScrollingWinControl(Compiler);
  Register_TGraphicControl(Compiler);
end;

end.

