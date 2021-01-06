unit simba.script_import_lclcontrols;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLControls(Compiler: TSimbaScript_Compiler);

implementation

uses controls, forms, graphics;

type
  PScrollBarKind = ^TScrollBarKind;
  PControlScrollBar = ^TcontrolScrollBar;
  PScrollingWinControl = ^TScrollingWinControl;
  PAlign = ^TAlign;
  PCursor = ^TCursor;
  PControl = ^TControl;
  PWinControl = ^TWinControl;
  PKeyEvent = ^TkeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PCustomControl = ^TCustomControl;
  PGraphicControl = ^TGraphicControl;
  PRect = ^TRect;
  PComponent = ^TComponent;
  PNotifyEvent = ^TNotifyEvent;
  PHandle = ^THandle;
  PBitmap = ^TBitmap;
  PPersistent = ^TPersistent;
  PBrush = ^TBrush;
  PFormBorderStyle = ^TFormBorderStyle;

{TControl}
//procedure AdjustSize;
procedure Lape_TControl_AdjustSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.AdjustSize();
end;

//function AutoSizeDelayed: boolean;
procedure Lape_TControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.AutoSizeDelayed();
end;

//function AutoSizeDelayedReport: string;
procedure Lape_TControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PControl(Params^[0])^.AutoSizeDelayedReport();
end;

//function AutoSizeDelayedHandle: Boolean;
procedure Lape_TControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSizeDelayedHandle();
end;

//procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure Lape_TControl_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure Lape_TControl_SetInitialBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetInitialBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer);
procedure Lape_TControl_SetBoundsKeepBase(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetBoundsKeepBase(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw: boolean;WithThemeSpace: boolean);
procedure Lape_TControl_GetPreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//function GetDefaultWidth: integer;
procedure Lape_TControl_GetDefaultWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultWidth();
end;

//function GetDefaultHeight: integer;
procedure Lape_TControl_GetDefaultHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultHeight();
end;

//function GetColorResolvingParent: Integer;
procedure Lape_TControl_GetColorResolvingParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetColorResolvingParent();
end;

//function GetRGBColorResolvingParent: Integer;
procedure Lape_TControl_GetRGBColorResolvingParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetRGBColorResolvingParent();
end;

//procedure InvalidatePreferredSize;
procedure Lape_TControl_InvalidatePreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.InvalidatePreferredSize();
end;

//procedure UpdateBaseBounds(StoreBounds, StoreParentClientSize,UseLoadedValues: boolean);
procedure Lape_TControl_UpdateBaseBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.UpdateBaseBounds(Pboolean(Params^[1])^, Pboolean(Params^[2])^, Pboolean(Params^[3])^);
end;

//Read: property BaseBounds: TRect read BaseBounds;
procedure Lape_TControl_BaseBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BaseBounds;
end;

//Read: property ReadBounds: TRect read ReadBounds;
procedure Lape_TControl_ReadBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.ReadBounds;
end;

//procedure WriteLayoutDebugReport(const Prefix: string);
procedure Lape_TControl_WriteLayoutDebugReport(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.WriteLayoutDebugReport(PlpString(Params^[1])^);
end;

//function ShouldAutoAdjustLeftAndTop: Boolean;
procedure Lape_TControl_ShouldAutoAdjustLeftAndTop(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  // Breaking changes made in the LCL; this has been replaced with
  // ShouldAutoAdjust.
  //
  //PBoolean(Result)^ := PControl(Params^[0])^.ShouldAutoAdjustLeftAndTop();
end;

//function ShouldAutoAdjustWidthAndHeight: Boolean;
procedure Lape_TControl_ShouldAutoAdjustWidthAndHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  //PBoolean(Result)^ := PControl(Params^[0])^.ShouldAutoAdjustWidthAndHeight();
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^ := TControl.Create(PComponent(Params^[1])^);
end;

//procedure BeforeDestruction;
procedure Lape_TControl_BeforeDestruction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BeforeDestruction();
end;

//procedure EditingDone;
procedure Lape_TControl_EditingDone(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.EditingDone();
end;

//procedure ExecuteDefaultAction;
procedure Lape_TControl_ExecuteDefaultAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ExecuteDefaultAction();
end;

//procedure ExecuteCancelAction;
procedure Lape_TControl_ExecuteCancelAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ExecuteCancelAction();
end;

//procedure BringToFront;
procedure Lape_TControl_BringToFront(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BringToFront();
end;

//function HasParent: Boolean;
procedure Lape_TControl_HasParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.HasParent();
end;

//function GetParentComponent: TComponent;
procedure Lape_TControl_GetParentComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PControl(Params^[0])^.GetParentComponent();
end;

//function IsParentOf(AControl: TControl): boolean;
procedure Lape_TControl_IsParentOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsParentOf(PControl(Params^[1])^);
end;

//function GetTopParent: TControl;
procedure Lape_TControl_GetTopParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PControl(Params^[0])^.GetTopParent();
end;

//function IsVisible: Boolean;
procedure Lape_TControl_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsVisible();
end;

//function IsControlVisible: Boolean;
procedure Lape_TControl_IsControlVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControlVisible();
end;

//function IsEnabled: Boolean;
procedure Lape_TControl_IsEnabled(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsEnabled();
end;

//function IsParentColor: Boolean;
procedure Lape_TControl_IsParentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentColor();
end;

//function IsParentFont: Boolean;
procedure Lape_TControl_IsParentFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentFont();
end;

//function FormIsUpdating: boolean;
procedure Lape_TControl_FormIsUpdating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.FormIsUpdating();
end;

//function IsProcessingPaintMsg: boolean;
procedure Lape_TControl_IsProcessingPaintMsg(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsProcessingPaintMsg();
end;

//procedure Hide;
procedure Lape_TControl_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Hide();
end;

//procedure Refresh;
procedure Lape_TControl_Refresh(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Refresh();
end;

//procedure Repaint;
procedure Lape_TControl_Repaint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Repaint();
end;

//procedure Invalidate;
procedure Lape_TControl_Invalidate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Invalidate();
end;

//procedure SendToBack;
procedure Lape_TControl_SendToBack(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SendToBack();
end;

//procedure UpdateRolesForForm;
procedure Lape_TControl_UpdateRolesForForm(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.UpdateRolesForForm();
end;

//procedure ActiveDefaultControlChanged(NewControl: TControl);
procedure Lape_TControl_ActiveDefaultControlChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

//function  GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
procedure Lape_TControl_GetTextBuf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetTextBuf(PPChar(Params^[1])^, PInteger(Params^[2])^);
end;

//function  GetTextLen: Integer;
procedure Lape_TControl_GetTextLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetTextLen();
end;

//procedure SetTextBuf(Buffer: PChar);
procedure Lape_TControl_SetTextBuf(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetTextBuf(PPChar(Params^[1])^);
end;

//function  ScreenToClient(const APoint: TPoint): TPoint;
procedure Lape_TControl_ScreenToClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToClient(PPoint(Params^[1])^);
end;

//function  ClientToScreen(const APoint: TPoint): TPoint;
procedure Lape_TControl_ClientToScreen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientToScreen(PPoint(Params^[1])^);
end;

//function  ScreenToControl(const APoint: TPoint): TPoint;
procedure Lape_TControl_ScreenToControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToControl(PPoint(Params^[1])^);
end;

//function  ControlToScreen(const APoint: TPoint): TPoint;
procedure Lape_TControl_ControlToScreen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlToScreen(PPoint(Params^[1])^);
end;

//function GetChildsRect(Scrolled: boolean): TRect;
procedure Lape_TControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.GetChildrenRect(Pboolean(Params^[1])^);
end;

//procedure Show;
procedure Lape_TControl_Show(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Show();
end;

//procedure Update;
procedure Lape_TControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Update();
end;

//function HandleObjectShouldBeVisible: boolean;
procedure Lape_TControl_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.HandleObjectShouldBeVisible();
end;

//function ParentDestroyingHandle: boolean;
procedure Lape_TControl_ParentDestroyingHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.ParentDestroyingHandle();
end;

//function ParentHandlesAllocated: boolean;
procedure Lape_TControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.ParentHandlesAllocated();
end;

//procedure InitiateAction;
procedure Lape_TControl_InitiateAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.InitiateAction();
end;

//Read: property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
procedure Lape_TControl_AutoSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSize;
end;

//Write: property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
procedure Lape_TControl_AutoSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.AutoSize := PBoolean(Params^[1])^;
end;

//Read: property BoundsRect: TRect read BoundsRect write BoundsRect;
procedure Lape_TControl_BoundsRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRect;
end;

//Write: property BoundsRect: TRect read BoundsRect write BoundsRect;
procedure Lape_TControl_BoundsRect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BoundsRect := PRect(Params^[1])^;
end;

//Read: property BoundsRectForNewParent: TRect read BoundsRectForNewParent write BoundsRectForNewParent;
procedure Lape_TControl_BoundsRectForNewParent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRectForNewParent;
end;

//Write: property BoundsRectForNewParent: TRect read BoundsRectForNewParent write BoundsRectForNewParent;
procedure Lape_TControl_BoundsRectForNewParent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BoundsRectForNewParent := PRect(Params^[1])^;
end;

//Read: property Caption: String read Caption write Caption;
procedure Lape_TControl_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PControl(Params^[0])^.Caption;
end;

//Write: property Caption: String read Caption write Caption;
procedure Lape_TControl_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

//Read: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure Lape_TControl_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientHeight;
end;

//Write: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure Lape_TControl_ClientHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

//Read: property ClientOrigin: TPoint read ClientOrigin;
procedure Lape_TControl_ClientOrigin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientOrigin;
end;

//Read: property ClientRect: TRect read ClientRect;
procedure Lape_TControl_ClientRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.ClientRect;
end;

//Read: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure Lape_TControl_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientWidth;
end;

//Write: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure Lape_TControl_ClientWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

//Read: property Color: Integer read Color write Color;
procedure Lape_TControl_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Color;
end;

//Write: property Color: Integer read Color write Color;
procedure Lape_TControl_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Color := PInteger(Params^[1])^;
end;

//Read: property ControlOrigin: TPoint read ControlOrigin;
procedure Lape_TControl_ControlOrigin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlOrigin;
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure Lape_TControl_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure Lape_TControl_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Font: TFont read Font write Font;
procedure Lape_TControl_Font_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFont(Result)^ := PControl(Params^[0])^.Font;
end;

//Write: property Font: TFont read Font write Font;
procedure Lape_TControl_Font_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property IsControl: Boolean read IsControl write IsControl;
procedure Lape_TControl_IsControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControl;
end;

//Write: property IsControl: Boolean read IsControl write IsControl;
procedure Lape_TControl_IsControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.IsControl := PBoolean(Params^[1])^;
end;

//Read: property MouseEntered: Boolean read MouseEntered;
procedure Lape_TControl_MouseEntered_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.MouseEntered;
end;

//Read: property OnChangeBounds: TNotifyEvent read OnChangeBounds write OnChangeBounds;
procedure Lape_TControl_OnChangeBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnChangeBounds;
end;

//Write: property OnChangeBounds: TNotifyEvent read OnChangeBounds write OnChangeBounds;
procedure Lape_TControl_OnChangeBounds_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnChangeBounds := PNotifyEvent(Params^[1])^;
end;

procedure Lape_TControl_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnClick;
end;

procedure Lape_TControl_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

//Read: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure Lape_TControl_OnResize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnResize;
end;

//Write: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure Lape_TControl_OnResize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure Lape_TControl_Align_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAlign(Result)^ := PControl(Params^[0])^.Align;
end;

procedure Lape_TControl_Align_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure Lape_TControl_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure Lape_TControl_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//function UseRightToLeftAlignment: Boolean;
procedure Lape_TControl_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftAlignment();
end;

//function UseRightToLeftReading: Boolean;
procedure Lape_TControl_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftReading();
end;

//function UseRightToLeftScrollBar: Boolean;
procedure Lape_TControl_UseRightToLeftScrollBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftScrollBar();
end;

//function IsRightToLeft: Boolean;
procedure Lape_TControl_IsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsRightToLeft();
end;

//Read: property Left: Integer read Left write Left;
procedure Lape_TControl_Left_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Left;
end;

//Write: property Left: Integer read Left write Left;
procedure Lape_TControl_Left_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Left := PInteger(Params^[1])^;
end;

//Read: property Height: Integer read Height write Height;
procedure Lape_TControl_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure Lape_TControl_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Top: Integer read Top write Top;
procedure Lape_TControl_Top_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Top;
end;

//Write: property Top: Integer read Top write Top;
procedure Lape_TControl_Top_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Top := PInteger(Params^[1])^;
end;

//Read: property Width: Integer read Width write Width;
procedure Lape_TControl_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Width;
end;

//Write: property Width: Integer read Width write Width;
procedure Lape_TControl_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Width := PInteger(Params^[1])^;
end;

// Procedure ShowHint();
procedure Lape_TControl_ShowHint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ShowHint := true;
end;

//Read: property Parent: TWinControl;
procedure Lape_TControl_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PControl(Params^[0])^.Parent;
end;

//Write: property Parent: TWinControl;
procedure Lape_TControl_Parent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Parent := PWinControl(Params^[1])^;
end;

procedure Lape_TControl_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PControl(Params^[0])^.Hint;
end;

procedure Lape_TControl_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Hint := PLPString(Params^[1])^;
end;

procedure Lape_TControl_ShowHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShowHint;
end;

procedure Lape_TControl_ShowHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure Lape_TControl_Cursor_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCursor(Result)^ := PControl(Params^[0])^.Cursor;
end;

procedure Lape_TControl_Cursor_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

//procedure Free();
procedure Lape_TControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Free();
end;

procedure Lape_Import_TControl(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TControl', 'TComponent');
	
    addGlobalFunc('procedure TControl.AdjustSize(); constref;', @Lape_TControl_AdjustSize);
    addGlobalFunc('function TControl.AutoSizeDelayed(): boolean; constref;', @Lape_TControl_AutoSizeDelayed);
    addGlobalFunc('function TControl.AutoSizeDelayedReport(): string; constref;', @Lape_TControl_AutoSizeDelayedReport);
    addGlobalFunc('function TControl.AutoSizeDelayedHandle(): Boolean; constref;', @Lape_TControl_AutoSizeDelayedHandle);
    addGlobalFunc('procedure TControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer); constref;', @Lape_TControl_SetBounds);
    addGlobalFunc('procedure TControl.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); constref;', @Lape_TControl_SetInitialBounds);
    addGlobalFunc('procedure TControl.SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer); constref;', @Lape_TControl_SetBoundsKeepBase);
    addGlobalFunc('procedure TControl.GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw: boolean;WithThemeSpace: boolean); constref;', @Lape_TControl_GetPreferredSize);
    addGlobalFunc('function TControl.GetDefaultWidth(): integer; constref;', @Lape_TControl_GetDefaultWidth);
    addGlobalFunc('function TControl.GetDefaultHeight(): integer; constref;', @Lape_TControl_GetDefaultHeight);
    addGlobalFunc('function TControl.GetColorResolvingParent(): Integer; constref;', @Lape_TControl_GetColorResolvingParent);
    addGlobalFunc('function TControl.GetRGBColorResolvingParent(): Integer; constref;', @Lape_TControl_GetRGBColorResolvingParent);
    addGlobalFunc('procedure TControl.InvalidatePreferredSize(); constref;', @Lape_TControl_InvalidatePreferredSize);
    addGlobalFunc('procedure TControl.UpdateBaseBounds(StoreBounds, StoreParentClientSize,UseLoadedValues: boolean); constref;', @Lape_TControl_UpdateBaseBounds);
    addClassVar('TControl', 'BaseBounds', 'TRect', @Lape_TControl_BaseBounds_Read);
    addClassVar('TControl', 'ReadBounds', 'TRect', @Lape_TControl_ReadBounds_Read);
    addGlobalFunc('procedure TControl.WriteLayoutDebugReport(const Prefix: string); constref;', @Lape_TControl_WriteLayoutDebugReport);
    addGlobalFunc('function TControl.ShouldAutoAdjustLeftAndTop(): Boolean; constref;', @Lape_TControl_ShouldAutoAdjustLeftAndTop);
    addGlobalFunc('function TControl.ShouldAutoAdjustWidthAndHeight(): Boolean; constref;', @Lape_TControl_ShouldAutoAdjustWidthAndHeight);
    addGlobalFunc('procedure TControl.Init(TheOwner: TComponent); override;', @Lape_TControl_Init);
    addGlobalFunc('procedure TControl.BeforeDestruction(); constref;', @Lape_TControl_BeforeDestruction);
    addGlobalFunc('procedure TControl.EditingDone(); constref;', @Lape_TControl_EditingDone);
    addGlobalFunc('procedure TControl.ExecuteDefaultAction(); constref;', @Lape_TControl_ExecuteDefaultAction);
    addGlobalFunc('procedure TControl.ExecuteCancelAction(); constref;', @Lape_TControl_ExecuteCancelAction);
    addGlobalFunc('procedure TControl.BringToFront(); constref;', @Lape_TControl_BringToFront);
    addGlobalFunc('function TControl.HasParent(): Boolean; constref;', @Lape_TControl_HasParent);
    addGlobalFunc('function TControl.GetParentComponent(): TComponent; constref;', @Lape_TControl_GetParentComponent);
    addGlobalFunc('function TControl.IsParentOf(AControl: TControl): boolean; constref;', @Lape_TControl_IsParentOf);
    addGlobalFunc('function TControl.GetTopParent(): TControl; constref;', @Lape_TControl_GetTopParent);
    addGlobalFunc('function TControl.IsVisible(): Boolean; constref;', @Lape_TControl_IsVisible);
    addGlobalFunc('function TControl.IsControlVisible(): Boolean; constref;', @Lape_TControl_IsControlVisible);
    addGlobalFunc('function TControl.IsEnabled(): Boolean; constref;', @Lape_TControl_IsEnabled);
    addGlobalFunc('function TControl.IsParentColor(): Boolean; constref;', @Lape_TControl_IsParentColor);
    addGlobalFunc('function TControl.IsParentFont(): Boolean; constref;', @Lape_TControl_IsParentFont);
    addGlobalFunc('function TControl.FormIsUpdating(): boolean; constref;', @Lape_TControl_FormIsUpdating);
    addGlobalFunc('function TControl.IsProcessingPaintMsg(): boolean; constref;', @Lape_TControl_IsProcessingPaintMsg);
    addGlobalFunc('procedure TControl.Hide(); constref;', @Lape_TControl_Hide);
    addGlobalFunc('procedure TControl.Refresh(); constref;', @Lape_TControl_Refresh);
    addGlobalFunc('procedure TControl.Repaint(); constref;', @Lape_TControl_Repaint);
    addGlobalFunc('procedure TControl.Invalidate(); constref;', @Lape_TControl_Invalidate);
    addGlobalFunc('procedure TControl.SendToBack(); constref;', @Lape_TControl_SendToBack);
    addGlobalFunc('procedure TControl.UpdateRolesForForm(); constref;', @Lape_TControl_UpdateRolesForForm);
    addGlobalFunc('procedure TControl.ActiveDefaultControlChanged(NewControl: TControl); constref;', @Lape_TControl_ActiveDefaultControlChanged);
    addGlobalFunc('function TControl.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer; constref;', @Lape_TControl_GetTextBuf);
    addGlobalFunc('function TControl.GetTextLen(): Integer; constref;', @Lape_TControl_GetTextLen);
    addGlobalFunc('procedure TControl.SetTextBuf(Buffer: PChar); constref;', @Lape_TControl_SetTextBuf);
    addGlobalFunc('function TControl.ScreenToClient(const APoint: TPoint): TPoint; constref;', @Lape_TControl_ScreenToClient);
    addGlobalFunc('function TControl.ClientToScreen(const APoint: TPoint): TPoint; constref;', @Lape_TControl_ClientToScreen);
    addGlobalFunc('function TControl.ScreenToControl(const APoint: TPoint): TPoint; constref;', @Lape_TControl_ScreenToControl);
    addGlobalFunc('function TControl.ControlToScreen(const APoint: TPoint): TPoint; constref;', @Lape_TControl_ControlToScreen);
    addGlobalFunc('function TControl.GetChildsRect(Scrolled: boolean): TRect; constref;', @Lape_TControl_GetChildsRect);
    addGlobalFunc('procedure TControl.Show(); constref;', @Lape_TControl_Show);
    addGlobalFunc('procedure TControl.Update(); constref;', @Lape_TControl_Update);
    addGlobalFunc('function TControl.HandleObjectShouldBeVisible(): boolean; constref;', @Lape_TControl_HandleObjectShouldBeVisible);
    addGlobalFunc('function TControl.ParentDestroyingHandle(): boolean; constref;', @Lape_TControl_ParentDestroyingHandle);
    addGlobalFunc('function TControl.ParentHandlesAllocated(): boolean; constref;', @Lape_TControl_ParentHandlesAllocated);
    addGlobalFunc('procedure TControl.InitiateAction(); constref;', @Lape_TControl_InitiateAction);
    addClassVar('TControl', 'Cursor', 'TCursor', @Lape_TControl_Cursor_Read, @Lape_TControl_Cursor_Write);      addClassVar('TControl', 'Align', 'TAlign', @Lape_TControl_Align_Read, @Lape_TControl_Align_Write);
    addClassVar('TControl', 'AutoSize', 'Boolean', @Lape_TControl_AutoSize_Read, @Lape_TControl_AutoSize_Write);
    addClassVar('TControl', 'BoundsRect', 'TRect', @Lape_TControl_BoundsRect_Read, @Lape_TControl_BoundsRect_Write);
    addClassVar('TControl', 'BoundsRectForNewParent', 'TRect', @Lape_TControl_BoundsRectForNewParent_Read, @Lape_TControl_BoundsRectForNewParent_Write);
    addClassVar('TControl', 'Caption', 'String', @Lape_TControl_Caption_Read, @Lape_TControl_Caption_Write);
    addClassVar('TControl', 'ClientHeight', 'Integer', @Lape_TControl_ClientHeight_Read, @Lape_TControl_ClientHeight_Write);
    addClassVar('TControl', 'ClientOrigin', 'TPoint', @Lape_TControl_ClientOrigin_Read);
    addClassVar('TControl', 'ClientRect', 'TRect', @Lape_TControl_ClientRect_Read);
    addClassVar('TControl', 'ClientWidth', 'Integer', @Lape_TControl_ClientWidth_Read, @Lape_TControl_ClientWidth_Write);
    addClassVar('TControl', 'Color', 'Integer', @Lape_TControl_Color_Read, @Lape_TControl_Color_Write);
    addClassVar('TControl', 'ControlOrigin', 'TPoint', @Lape_TControl_ControlOrigin_Read);
    addClassVar('TControl', 'Enabled', 'Boolean', @Lape_TControl_Enabled_Read, @Lape_TControl_Enabled_Write);
    addClassVar('TControl', 'Font', 'TFont', @Lape_TControl_Font_Read, @Lape_TControl_Font_Write);
    addClassVar('TControl', 'IsControl', 'Boolean', @Lape_TControl_IsControl_Read, @Lape_TControl_IsControl_Write);
    addClassVar('TControl', 'MouseEntered', 'Boolean', @Lape_TControl_MouseEntered_Read);
    addClassVar('TControl', 'OnChangeBounds', 'TNotifyEvent', @Lape_TControl_OnChangeBounds_Read, @Lape_TControl_OnChangeBounds_Write);
    addClassVar('TControl', 'OnClick', 'TNotifyEvent', @Lape_TControl_OnClick_Read, @Lape_TControl_OnClick_Write);
    addClassVar('TControl', 'OnResize', 'TNotifyEvent', @Lape_TControl_OnResize_Read, @Lape_TControl_OnResize_Write);
    addClassVar('TControl', 'Visible', 'Boolean', @Lape_TControl_Visible_Read, @Lape_TControl_Visible_Write);
    addClassVar('TControl', 'ShowHint', 'Boolean', @Lape_TControl_ShowHint_Read, @Lape_TControl_ShowHint_Write);
    addGlobalFunc('function TControl.UseRightToLeftAlignment(): Boolean; constref;', @Lape_TControl_UseRightToLeftAlignment);
    addGlobalFunc('function TControl.UseRightToLeftReading(): Boolean; constref;', @Lape_TControl_UseRightToLeftReading);
    addGlobalFunc('function TControl.UseRightToLeftScrollBar(): Boolean; constref;', @Lape_TControl_UseRightToLeftScrollBar);
    addGlobalFunc('function TControl.IsRightToLeft(): Boolean; constref;', @Lape_TControl_IsRightToLeft);
    addClassVar('TControl', 'Left', 'Integer', @Lape_TControl_Left_Read, @Lape_TControl_Left_Write);
    addClassVar('TControl', 'Height', 'Integer', @Lape_TControl_Height_Read, @Lape_TControl_Height_Write);
    addClassVar('TControl', 'Top', 'Integer', @Lape_TControl_Top_Read, @Lape_TControl_Top_Write);
    addClassVar('TControl', 'Width', 'Integer', @Lape_TControl_Width_Read, @Lape_TControl_Width_Write);
    addGlobalFunc('procedure TControl.ShowHint(); constref;', @Lape_TControl_ShowHint);
    addClassVar('TControl', 'Hint', 'String', @Lape_TControl_Hint_Read, @Lape_TControl_Hint_Write);
    addClassVar('TControl', 'Parent', 'TControl', @Lape_TControl_Parent_Read, @Lape_TControl_Parent_Write); //FIXME: Should be OS-Depend TControl
   // addGlobalFunc('procedure TControl.Free(); constref;', @Lape_TControl_Free);
  end;
end;

{TWinControl}
//Read: property BorderWidth: integer read BorderWidth write BorderWidth;
procedure Lape_TWinControl_BorderWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BorderWidth;
end;

//Write: property BorderWidth: integer read BorderWidth write BorderWidth;
procedure Lape_TWinControl_BorderWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.BorderWidth := Pinteger(Params^[1])^;
end;

//Read: property BoundsLockCount: integer read BoundsLockCount;
procedure Lape_TWinControl_BoundsLockCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BoundsLockCount;
end;

//Read: property Brush: TBrush read Brush;
procedure Lape_TWinControl_Brush_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBrush(Result)^ := PWinControl(Params^[0])^.Brush;
end;

//Read: property CachedClientHeight: integer read ClientHeight;
procedure Lape_TWinControl_CachedClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientHeight;
end;

//Read: property CachedClientWidth: integer read ClientWidth;
procedure Lape_TWinControl_CachedClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientWidth;
end;

//Read: property ControlCount: Integer read ControlCount;
procedure Lape_TWinControl_ControlCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.ControlCount;
end;

//Read: property DoubleBuffered: Boolean read DoubleBuffered write DoubleBuffered;
procedure Lape_TWinControl_DoubleBuffered_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.DoubleBuffered;
end;

//Write: property DoubleBuffered: Boolean read DoubleBuffered write DoubleBuffered;
procedure Lape_TWinControl_DoubleBuffered_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DoubleBuffered := PBoolean(Params^[1])^;
end;

//Read: property Handle: THandle read Handle write Handle;
procedure Lape_TWinControl_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read Handle write Handle;
procedure Lape_TWinControl_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//Read: property IsResizing: Boolean read IsResizing;
procedure Lape_TWinControl_IsResizing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.IsResizing;
end;

//Read: property TabOrder: Integer read TabOrder write Taborder ;
procedure Lape_TWinControl_TabOrder_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.TabOrder;
end;

//Write: property TabOrder: Integer read TabOrder write Taborder ;
procedure Lape_TWinControl_TabOrder_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.TabOrder := PInteger(Params^[1])^;
end;

//Read: property TabStop: Boolean read TabStop write TabStop;
procedure Lape_TWinControl_TabStop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.TabStop;
end;

//Write: property TabStop: Boolean read TabStop write TabStop;
procedure Lape_TWinControl_TabStop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.TabStop := PBoolean(Params^[1])^;
end;

//Read: property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
procedure Lape_TWinControl_OnEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnEnter;
end;

//Write: property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
procedure Lape_TWinControl_OnEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

//Read: property OnExit: TNotifyEvent read FOnExit write FOnExit;
procedure Lape_TWinControl_OnExit_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnExit;
end;

//Write: property OnExit: TNotifyEvent read FOnExit write FOnExit;
procedure Lape_TWinControl_OnExit_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure Lape_TWinControl_OnKeyDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure Lape_TWinControl_OnKeyPress_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure Lape_TWinControl_OnKeyUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

//Read: property ParentWindow: THandle read FParentWindow write SetParentWindow;
procedure Lape_TWinControl_ParentWindow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.ParentWindow;
end;

//Write: property ParentWindow: THandle read FParentWindow write SetParentWindow;
procedure Lape_TWinControl_ParentWindow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ParentWindow := PHandle(Params^[1])^;
end;

//Read: property Showing: Boolean read Showing;
procedure Lape_TWinControl_Showing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Showing;
end;

//Read: property VisibleDockClientCount: Integer read VisibleDockClientCount;
procedure Lape_TWinControl_VisibleDockClientCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.VisibleDockClientCount;
end;

//function AutoSizeDelayed: boolean;
procedure Lape_TWinControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayed();
end;

//function AutoSizeDelayedReport: string;
procedure Lape_TWinControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedReport();
end;

//function AutoSizeDelayedHandle: Boolean;
procedure Lape_TWinControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedHandle();
end;

//procedure BeginUpdateBounds;
procedure Lape_TWinControl_BeginUpdateBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.BeginUpdateBounds();
end;

//procedure EndUpdateBounds;
procedure Lape_TWinControl_EndUpdateBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.EndUpdateBounds();
end;

//procedure LockRealizeBounds;
procedure Lape_TWinControl_LockRealizeBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.LockRealizeBounds();
end;

//procedure UnlockRealizeBounds;
procedure Lape_TWinControl_UnlockRealizeBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.UnlockRealizeBounds();
end;

//function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;
procedure Lape_TWinControl_ControlAtPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

//function ControlAtPos(const Pos: TPoint;AllowDisabled, AllowWinControls: Boolean): TControl;
procedure Lape_TWinControl_ControlAtPosEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

//function  ContainsControl(Control: TControl): Boolean;
procedure Lape_TWinControl_ContainsControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.ContainsControl(PControl(Params^[1])^);
end;

//procedure DoAdjustClientRectChange(const InvalidateRect: Boolean);
procedure Lape_TWinControl_DoAdjustClientRectChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DoAdjustClientRectChange(PBoolean(Params^[1])^);
end;

//procedure InvalidateClientRectCache(WithChildControls: boolean);
procedure Lape_TWinControl_InvalidateClientRectCache(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InvalidateClientRectCache(Pboolean(Params^[1])^);
end;

//function ClientRectNeedsInterfaceUpdate: boolean;
procedure Lape_TWinControl_ClientRectNeedsInterfaceUpdate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ClientRectNeedsInterfaceUpdate();
end;

//procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
procedure Lape_TWinControl_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function  GetChildsRect(Scrolled: boolean): TRect;
procedure Lape_TWinControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PWinControl(Params^[0])^.GetChildrenRect(Pboolean(Params^[1])^);
end;

//procedure DisableAlign;
procedure Lape_TWinControl_DisableAlign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DisableAlign();
end;

//procedure EnableAlign;
procedure Lape_TWinControl_EnableAlign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.EnableAlign();
end;

//procedure ReAlign;
procedure Lape_TWinControl_ReAlign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ReAlign();
end;

//procedure ScrollBy(DeltaX, DeltaY: Integer);
procedure Lape_TWinControl_ScrollBy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure WriteLayoutDebugReport(const Prefix: string);
procedure Lape_TWinControl_WriteLayoutDebugReport(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.WriteLayoutDebugReport(PlpString(Params^[1])^);
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TWinControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^ := TWinControl.Create(PComponent(Params^[1])^);
end;

//constructor CreateParented(AParentWindow: Thandle);
procedure Lape_TWinControl_CreateParented(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^ := TWinControl.CreateParented(Phandle(Params^[1])^);
end;

//function CanFocus: Boolean;
procedure Lape_TWinControl_CanFocus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.CanFocus();
end;

//function GetControlIndex(AControl: TControl): integer;
procedure Lape_TWinControl_GetControlIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.GetControlIndex(PControl(Params^[1])^);
end;

//procedure SetControlIndex(AControl: TControl; NewIndex: integer);
procedure Lape_TWinControl_SetControlIndex(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetControlIndex(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

//function Focused: Boolean;
procedure Lape_TWinControl_Focused(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Focused();
end;

//function PerformTab(ForwardTab: boolean): boolean;
procedure Lape_TWinControl_PerformTab(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.PerformTab(Pboolean(Params^[1])^);
end;

//function FindChildControl(const ControlName: String): TControl;
procedure Lape_TWinControl_FindChildControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.FindChildControl(PlpString(Params^[1])^);
end;

//procedure SelectNext(CurControl: TWinControl;GoForward, CheckTabStop: Boolean);
procedure Lape_TWinControl_SelectNext(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SelectNext(PWinControl(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

//function  GetTextLen: Integer;
procedure Lape_TWinControl_GetTextLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.GetTextLen();
end;

//procedure Invalidate;
procedure Lape_TWinControl_Invalidate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Invalidate();
end;

//procedure AddControl;
procedure Lape_TWinControl_AddControl(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.AddControl();
end;

//procedure InsertControl(AControl: TControl);
procedure Lape_TWinControl_InsertControl(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^);
end;

//procedure InsertControl(AControl: TControl; Index: integer);
procedure Lape_TWinControl_InsertControlEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure RemoveControl(AControl: TControl);
procedure Lape_TWinControl_RemoveControl(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.RemoveControl(PControl(Params^[1])^);
end;

//procedure Repaint;
procedure Lape_TWinControl_Repaint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Repaint();
end;

//procedure Update;
procedure Lape_TWinControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Update();
end;

//procedure SetFocus;
procedure Lape_TWinControl_SetFocus(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetFocus();
end;

//procedure FlipChildren(AllLevels: Boolean);
procedure Lape_TWinControl_FlipChildren(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.FlipChildren(PBoolean(Params^[1])^);
end;

//procedure ScaleBy(Multiplier, Divider: Integer);
procedure Lape_TWinControl_ScaleBy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ScaleBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function GetDockCaption(AControl: TControl): String;
procedure Lape_TWinControl_GetDockCaption(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PWinControl(Params^[0])^.GetDockCaption(PControl(Params^[1])^);
end;

//procedure UpdateDockCaption(Exclude: TControl);
procedure Lape_TWinControl_UpdateDockCaption(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.UpdateDockCaption(PControl(Params^[1])^);
end;

//function HandleAllocated: Boolean;
procedure Lape_TWinControl_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.HandleAllocated();
end;

//function ParentHandlesAllocated: boolean;
procedure Lape_TWinControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ParentHandlesAllocated();
end;

//procedure HandleNeeded;
procedure Lape_TWinControl_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.HandleNeeded();
end;

//function BrushCreated: Boolean;
procedure Lape_TWinControl_BrushCreated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.BrushCreated();
end;

//procedure PaintTo(ACanvas: TCanvas; X, Y: Integer);
procedure Lape_TWinControl_PaintTo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.PaintTo(PCanvas(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure SetShape(AShape: TBitmap);
procedure Lape_TWinControl_SetShape(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetShape(PBitmap(Params^[1])^);
end;

//procedure Free();
procedure Lape_TWinControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Free();
end;

procedure Lape_Import_TWinControl(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TWinControl', 'TControl');

    addClassVar('TWinControl', 'BorderWidth', 'integer', @Lape_TWinControl_BorderWidth_Read, @Lape_TWinControl_BorderWidth_Write);
    addClassVar('TWinControl', 'BoundsLockCount', 'integer', @Lape_TWinControl_BoundsLockCount_Read);
    addClassVar('TWinControl', 'Brush', 'TBrush', @Lape_TWinControl_Brush_Read);
    addClassVar('TWinControl', 'CachedClientHeight', 'integer', @Lape_TWinControl_CachedClientHeight_Read);
    addClassVar('TWinControl', 'CachedClientWidth', 'integer', @Lape_TWinControl_CachedClientWidth_Read);
    addClassVar('TWinControl', 'ControlCount', 'Integer', @Lape_TWinControl_ControlCount_Read);
    addClassVar('TWinControl', 'DoubleBuffered', 'Boolean', @Lape_TWinControl_DoubleBuffered_Read, @Lape_TWinControl_DoubleBuffered_Write);
    addClassVar('TWinControl', 'Handle', 'THandle', @Lape_TWinControl_Handle_Read, @Lape_TWinControl_Handle_Write);
    addClassVar('TWinControl', 'IsResizing', 'Boolean', @Lape_TWinControl_IsResizing_Read);
    addClassVar('TWinControl', 'TabOrder', 'Integer', @Lape_TWinControl_TabOrder_Read, @Lape_TWinControl_TabOrder_Write);
    addClassVar('TWinControl', 'TabStop', 'Boolean', @Lape_TWinControl_TabStop_Read, @Lape_TWinControl_TabStop_Write);
    addClassVar('TWinControl', 'OnEnter', 'TNotifyEvent', @Lape_TWinControl_OnEnter_Read, @Lape_TWinControl_OnEnter_Write);
    addClassVar('TWinControl', 'OnExit', 'TNotifyEvent', @Lape_TWinControl_OnExit_Read, @Lape_TWinControl_OnExit_Write);
    addClassVar('TWinControl', 'OnKeyDown', 'TKeyEvent', nil, @Lape_TWinControl_OnKeyDown_Write);
    addClassVar('TWinControl', 'OnKeyPress', 'TKeyPressEvent', nil, @Lape_TWinControl_OnKeyPress_Write);
    addClassVar('TWinControl', 'OnKeyUp', 'TKeyEvent', nil, @Lape_TWinControl_OnKeyUp_Write);
    addClassVar('TWinControl', 'ParentWindow', 'THandle', @Lape_TWinControl_ParentWindow_Read, @Lape_TWinControl_ParentWindow_Write);
    addClassVar('TWinControl', 'Showing', 'Boolean', @Lape_TWinControl_Showing_Read);
    addClassVar('TWinControl', 'VisibleDockClientCount', 'Integer', @Lape_TWinControl_VisibleDockClientCount_Read, nil);
    //addGlobalFunc('function TWinControl.AutoSizeDelayed(): boolean; constref;', @Lape_TWinControl_AutoSizeDelayed);
    //addGlobalFunc('function TWinControl.AutoSizeDelayedReport(): string; constref;', @Lape_TWinControl_AutoSizeDelayedReport);
    //addGlobalFunc('function TWinControl.AutoSizeDelayedHandle(): Boolean; constref;', @Lape_TWinControl_AutoSizeDelayedHandle);
    addGlobalFunc('procedure TWinControl.BeginUpdateBounds(); constref;', @Lape_TWinControl_BeginUpdateBounds);
    addGlobalFunc('procedure TWinControl.EndUpdateBounds(); constref;', @Lape_TWinControl_EndUpdateBounds);
    addGlobalFunc('procedure TWinControl.LockRealizeBounds(); constref;', @Lape_TWinControl_LockRealizeBounds);
    addGlobalFunc('procedure TWinControl.UnlockRealizeBounds(); constref;', @Lape_TWinControl_UnlockRealizeBounds);
    addGlobalFunc('function TWinControl.ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl; constref;', @Lape_TWinControl_ControlAtPos);
    addGlobalFunc('function TWinControl.ControlAtPos(const Pos: TPoint;AllowDisabled, AllowWinControls: Boolean): TControl; constref; overload;', @Lape_TWinControl_ControlAtPosEx);
    addGlobalFunc('function TWinControl.ContainsControl(Control: TControl): Boolean; constref;', @Lape_TWinControl_ContainsControl);
    addGlobalFunc('procedure TWinControl.DoAdjustClientRectChange(const InvalidateRect: Boolean); constref;', @Lape_TWinControl_DoAdjustClientRectChange);
    addGlobalFunc('procedure TWinControl.InvalidateClientRectCache(WithChildControls: boolean); constref;', @Lape_TWinControl_InvalidateClientRectCache);
    addGlobalFunc('function TWinControl.ClientRectNeedsInterfaceUpdate(): boolean; constref;', @Lape_TWinControl_ClientRectNeedsInterfaceUpdate);
    //addGlobalFunc('procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer); constref;', @Lape_TWinControl_SetBounds);
    //addGlobalFunc('function TWinControl.GetChildsRect(Scrolled: boolean): TRect; constref;', @Lape_TWinControl_GetChildsRect);
    addGlobalFunc('procedure TWinControl.DisableAlign(); constref;', @Lape_TWinControl_DisableAlign);
    addGlobalFunc('procedure TWinControl.EnableAlign(); constref;', @Lape_TWinControl_EnableAlign);
    addGlobalFunc('procedure TWinControl.ReAlign(); constref;', @Lape_TWinControl_ReAlign);
    addGlobalFunc('procedure TWinControl.ScrollBy(DeltaX, DeltaY: Integer); constref;', @Lape_TWinControl_ScrollBy);
    //addGlobalFunc('procedure TWinControl.WriteLayoutDebugReport(const Prefix: string); constref;', @Lape_TWinControl_WriteLayoutDebugReport);
    addGlobalFunc('procedure TWinControl.Init(TheOwner: TComponent); override;', @Lape_TWinControl_Init);
    addGlobalFunc('procedure TWinControl.CreateParented(AParentWindow: Thandle); constref;', @Lape_TWinControl_CreateParented);
    addGlobalFunc('function TWinControl.CanFocus(): Boolean; constref;', @Lape_TWinControl_CanFocus);
    addGlobalFunc('function TWinControl.GetControlIndex(AControl: TControl): integer; constref;', @Lape_TWinControl_GetControlIndex);
    addGlobalFunc('procedure TWinControl.SetControlIndex(AControl: TControl; NewIndex: integer); constref;', @Lape_TWinControl_SetControlIndex);
    addGlobalFunc('function TWinControl.Focused(): Boolean; constref;', @Lape_TWinControl_Focused);
    addGlobalFunc('function TWinControl.PerformTab(ForwardTab: boolean): boolean; constref;', @Lape_TWinControl_PerformTab);
    addGlobalFunc('function TWinControl.FindChildControl(const ControlName: String): TControl; constref;', @Lape_TWinControl_FindChildControl);
    addGlobalFunc('procedure TWinControl.SelectNext(CurControl: TWinControl;GoForward, CheckTabStop: Boolean); constref;', @Lape_TWinControl_SelectNext);
    //addGlobalFunc('function TWinControl.GetTextLen(): Integer; constref;', @Lape_TWinControl_GetTextLen);
    //addGlobalFunc('procedure TWinControl.Invalidate(); constref;', @Lape_TWinControl_Invalidate);
    addGlobalFunc('procedure TWinControl.AddControl(); constref;', @Lape_TWinControl_AddControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl); constref;', @Lape_TWinControl_InsertControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl; Index: integer); constref; overload;', @Lape_TWinControl_InsertControlEx);
    addGlobalFunc('procedure TWinControl.RemoveControl(AControl: TControl); constref;', @Lape_TWinControl_RemoveControl);
    //addGlobalFunc('procedure TWinControl.Repaint(); constref;', @Lape_TWinControl_Repaint);
    //addGlobalFunc('procedure TWinControl.Update(); constref;', @Lape_TWinControl_Update);
    addGlobalFunc('procedure TWinControl.SetFocus(); constref;', @Lape_TWinControl_SetFocus);
    addGlobalFunc('procedure TWinControl.FlipChildren(AllLevels: Boolean); constref;', @Lape_TWinControl_FlipChildren);
    addGlobalFunc('procedure TWinControl.ScaleBy(Multiplier, Divider: Integer); constref;', @Lape_TWinControl_ScaleBy);
    addGlobalFunc('function TWinControl.GetDockCaption(AControl: TControl): String; constref;', @Lape_TWinControl_GetDockCaption);
    addGlobalFunc('procedure TWinControl.UpdateDockCaption(Exclude: TControl); constref;', @Lape_TWinControl_UpdateDockCaption);
    addGlobalFunc('function TWinControl.HandleAllocated(): Boolean; constref;', @Lape_TWinControl_HandleAllocated);
    //addGlobalFunc('function TWinControl.ParentHandlesAllocated(): boolean; constref;', @Lape_TWinControl_ParentHandlesAllocated);
    addGlobalFunc('procedure TWinControl.HandleNeeded(); constref;', @Lape_TWinControl_HandleNeeded);
    addGlobalFunc('function TWinControl.BrushCreated(): Boolean; constref;', @Lape_TWinControl_BrushCreated);
    addGlobalFunc('procedure TWinControl.PaintTo(ACanvas: TCanvas; X, Y: Integer); constref;', @Lape_TWinControl_PaintTo);
    addGlobalFunc('procedure TWinControl.SetShape(AShape: TBitmap); constref;', @Lape_TWinControl_SetShape);
    //addGlobalFunc('procedure TWinControl.Free(); constref;', @Lape_TWinControl_Free);
  end;
end;

{TCustomControl}

//constructor Create(AOwner: TComponent);
procedure Lape_TCustomControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^ := TCustomControl.Create(PComponent(Params^[1])^);
end;

//Read: property Canvas: TCanvas read Canvas write Canvas;
procedure Lape_TCustomControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomControl(Params^[0])^.Canvas;
end;

//Write: property Canvas: TCanvas read Canvas write Canvas;
procedure Lape_TCustomControl_Canvas_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

//Read: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure Lape_TCustomControl_OnPaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomControl(Params^[0])^.OnPaint;
end;

//Write: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure Lape_TCustomControl_OnPaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure Lape_TCustomControl_BorderStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomControl(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TCustomControl', 'TWinControl');

    addGlobalFunc('procedure TCustomControl.Init(AOwner: TComponent); override;', @Lape_TCustomControl_Init);
    addClassVar('TCustomControl', 'Canvas', 'TCanvas', @Lape_TCustomControl_Canvas_Read, @Lape_TCustomControl_Canvas_Write);
    addClassVar('TCustomControl', 'OnPaint', 'TNotifyEvent', @Lape_TCustomControl_OnPaint_Read, @Lape_TCustomControl_OnPaint_Write);
    addClassVar('TCustomControl', 'BorderStyle', 'TFormBorderStyle', nil, @Lape_TCustomControl_BorderStyle_Write);
    //addGlobalFunc('procedure TCustomControl.Free(); constref;', @Lape_TCustomControl_Free);
  end;
end;

{TControlScrollBar}
//constructor Create(AControl: TWinControl; AKind: TScrollBarKind);
procedure Lape_TControlScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^ := TControlScrollBar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

//procedure Assign(Source: TPersistent);
procedure Lape_TControlScrollBar_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//function IsScrollBarVisible: Boolean;
procedure Lape_TControlScrollBar_IsScrollBarVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.IsScrollBarVisible();
end;

//function ScrollPos: Integer;
procedure Lape_TControlScrollBar_ScrollPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.ScrollPos();
end;

//function GetOtherScrollBar: TControlScrollBar;
procedure Lape_TControlScrollBar_GetOtherScrollBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PControlScrollBar(Params^[0])^.GetOtherScrollBar();
end;

//Read: property Size: integer read Size;
procedure Lape_TControlScrollBar_Size_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.Size;
end;

//function ClientSize: integer;
procedure Lape_TControlScrollBar_ClientSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSize();
end;

//function ClientSizeWithBar: integer;
procedure Lape_TControlScrollBar_ClientSizeWithBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithBar();
end;

//function ClientSizeWithoutBar: integer;
procedure Lape_TControlScrollBar_ClientSizeWithoutBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithoutBar();
end;

//Read: property Increment: Integer read Increment write Increment;
procedure Lape_TControlScrollBar_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Increment;
end;

//Write: property Increment: Integer read Increment write Increment;
procedure Lape_TControlScrollBar_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Increment := PInteger(Params^[1])^;
end;

//Read: property Page: Integer read Page write Page;
procedure Lape_TControlScrollBar_Page_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Page;
end;

//Write: property Page: Integer read Page write Page;
procedure Lape_TControlScrollBar_Page_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Page := PInteger(Params^[1])^;
end;

//Read: property Smooth: Boolean read Smooth write Smooth;
procedure Lape_TControlScrollBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Smooth;
end;

//Write: property Smooth: Boolean read Smooth write Smooth;
procedure Lape_TControlScrollBar_Smooth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Smooth := PBoolean(Params^[1])^;
end;

//Read: property Position: Integer read Position write Position;
procedure Lape_TControlScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read Position write Position;
procedure Lape_TControlScrollBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property Range: Integer read Range write Range;
procedure Lape_TControlScrollBar_Range_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Range;
end;

//Write: property Range: Integer read Range write Range;
procedure Lape_TControlScrollBar_Range_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Range := PInteger(Params^[1])^;
end;

//Read: property Tracking: Boolean read Tracking write Tracking;
procedure Lape_TControlScrollBar_Tracking_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Tracking;
end;

//Write: property Tracking: Boolean read Tracking write Tracking;
procedure Lape_TControlScrollBar_Tracking_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Tracking := PBoolean(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure Lape_TControlScrollBar_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure Lape_TControlScrollBar_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure Lape_TControlScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Free();
end;

procedure Lape_Import_TControlScrollBar(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TControlScrollBar', 'TPersistent');

    addGlobalFunc('procedure TControlScrollBar.Init(AControl: TWinControl; AKind: TScrollBarKind);', @Lape_TControlScrollBar_Init);
    //addGlobalFunc('procedure TControlScrollBar.Assign(Source: TPersistent); constref;', @Lape_TControlScrollBar_Assign);
    addGlobalFunc('function TControlScrollBar.IsScrollBarVisible(): Boolean; constref;', @Lape_TControlScrollBar_IsScrollBarVisible);
    addGlobalFunc('function TControlScrollBar.ScrollPos(): Integer; constref;', @Lape_TControlScrollBar_ScrollPos);
    addGlobalFunc('function TControlScrollBar.GetOtherScrollBar(): TControlScrollBar; constref;', @Lape_TControlScrollBar_GetOtherScrollBar);
    addClassVar('TControlScrollBar', 'Size', 'integer', @Lape_TControlScrollBar_Size_Read);
    addGlobalFunc('function TControlScrollBar.ClientSize(): integer; constref;', @Lape_TControlScrollBar_ClientSize);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithBar(): integer; constref;', @Lape_TControlScrollBar_ClientSizeWithBar);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithoutBar(): integer; constref;', @Lape_TControlScrollBar_ClientSizeWithoutBar);
    addClassVar('TControlScrollBar', 'Increment', 'Integer', @Lape_TControlScrollBar_Increment_Read, @Lape_TControlScrollBar_Increment_Write);
    addClassVar('TControlScrollBar', 'Page', 'Integer', @Lape_TControlScrollBar_Page_Read, @Lape_TControlScrollBar_Page_Write);
    addClassVar('TControlScrollBar', 'Smooth', 'Boolean', @Lape_TControlScrollBar_Smooth_Read, @Lape_TControlScrollBar_Smooth_Write);
    addClassVar('TControlScrollBar', 'Position', 'Integer', @Lape_TControlScrollBar_Position_Read, @Lape_TControlScrollBar_Position_Write);
    addClassVar('TControlScrollBar', 'Range', 'Integer', @Lape_TControlScrollBar_Range_Read, @Lape_TControlScrollBar_Range_Write);
    addClassVar('TControlScrollBar', 'Tracking', 'Boolean', @Lape_TControlScrollBar_Tracking_Read, @Lape_TControlScrollBar_Tracking_Write);
    addClassVar('TControlScrollBar', 'Visible', 'Boolean', @Lape_TControlScrollBar_Visible_Read, @Lape_TControlScrollBar_Visible_Write);
    //addGlobalFunc('procedure TControlScrollBar.Free(); constref;', @Lape_TControlScrollBar_Free);
  end;
end;
{TScrollingWinControl}
//constructor Create(TheOwner : TComponent);
procedure Lape_TScrollingWinControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^ := TScrollingWinControl.Create(PComponent(Params^[1])^);
end;

//procedure UpdateScrollbars;
procedure Lape_TScrollingWinControl_UpdateScrollbars(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.UpdateScrollbars();
end;

//procedure ScrollBy(DeltaX, DeltaY: Integer);
procedure Lape_TScrollingWinControl_ScrollBy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//Read: property HorzScrollBar: TControlScrollBar read HorzScrollBar write HorzScrollBar;
procedure Lape_TScrollingWinControl_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.HorzScrollBar;
end;

//Write: property HorzScrollBar: TControlScrollBar read HorzScrollBar write HorzScrollBar;
procedure Lape_TScrollingWinControl_HorzScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.HorzScrollBar := PControlScrollBar(Params^[1])^;
end;

//Read: property VertScrollBar: TControlScrollBar read VertScrollBar write VertScrollBar;
procedure Lape_TScrollingWinControl_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.VertScrollBar;
end;

//Write: property VertScrollBar: TControlScrollBar read VertScrollBar write VertScrollBar;
procedure Lape_TScrollingWinControl_VertScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.VertScrollBar := PControlScrollBar(Params^[1])^;
end;

//procedure Free();
procedure Lape_TScrollingWinControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.Free();
end;

procedure Lape_Import_TScrollingWinControl(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TScrollingWinControl', 'TCustomControl');

    addGlobalFunc('procedure TScrollingWinControl.Init(TheOwner : TComponent); override;', @Lape_TScrollingWinControl_Init);
    addGlobalFunc('procedure TScrollingWinControl.UpdateScrollbars(); constref;', @Lape_TScrollingWinControl_UpdateScrollbars);
    //addGlobalFunc('procedure TScrollingWinControl.ScrollBy(DeltaX, DeltaY: Integer); constref;', @Lape_TScrollingWinControl_ScrollBy);
    addClassVar('TScrollingWinControl', 'HorzScrollBar', 'TControlScrollBar', @Lape_TScrollingWinControl_HorzScrollBar_Read, @Lape_TScrollingWinControl_HorzScrollBar_Write);
    addClassVar('TScrollingWinControl', 'VertScrollBar', 'TControlScrollBar', @Lape_TScrollingWinControl_VertScrollBar_Read, @Lape_TScrollingWinControl_VertScrollBar_Write);
    //addGlobalFunc('procedure TScrollingWinControl.Free(); constref;', @Lape_TScrollingWinControl_Free);
  end;
end;

//Read: property Canvas: TCanvas read Canvas
procedure Lape_TGraphicControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PGraphicControl(Params^[0])^.Canvas;
end;

//procedure Update();
procedure Lape_TGraphicControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PGraphicControl(Params^[0])^.Update();
end;

procedure Lape_TGraphicControl_Align_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAlign(Result)^ := PGraphicControl(Params^[0])^.Align;
end;

procedure Lape_TGraphicControl_Align_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PGraphicControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

procedure Lape_Import_TGraphicControl(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TGraphicControl', 'TControl');

    //addGlobalFunc('procedure TGraphicControl.Update(); constref;', @Lape_TGraphicControl_Update);
    addClassVar('TGraphicControl', 'Canvas', 'TCanvas', @Lape_TGraphicControl_Canvas_Read);
    addClassVar('TGraphicControl', 'Alignment', 'TAlign', @Lape_TGraphicControl_Align_Read, @Lape_TGraphicControl_Align_Write);
  end;
end;

procedure Lape_Import_LCLControls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
   begin
     addGlobalType('(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2)', 'TShiftStateEnum');
     addGlobalType('set of TShiftStateEnum', 'TShiftState');
     addGlobalType('procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object','TKeyEvent', FFI_DEFAULT_ABI);
     addGlobalType('procedure(Sender: TObject; var Key: Char) of object','TKeyPressEvent', FFI_DEFAULT_ABI);
     addGlobalType('(mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2)','TMouseButton');
     addGlobalType('procedure(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer) of object','TMouseEvent', FFI_DEFAULT_ABI);
     addGlobalType('procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object', 'TMouseMoveEvent', FFI_DEFAULT_ABI);
     addGlobalType('(sbHorizontal, sbVertical)','TScrollBarKind');
     addGlobalType('(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom)', 'TAlign');
     addGlobalType('(bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin)','TFormBorderStyle');
     addGlobalType('Integer', 'TCursor');
     addGlobalVar(crNone, 'crNone').isConstant := True;
     addGlobalVar(crCross, 'crCross').isConstant := True;
     addGlobalVar(crHandPoint, 'crHandPoint').isConstant := True;
     addGlobalVar(crIBeam, 'crIBeam').isConstant := True;
   end;

  Lape_Import_TControl(Compiler);
  Lape_Import_TWinControl(Compiler);
  Lape_Import_TCustomControl(Compiler);
  Lape_Import_TControlScrollBar(Compiler);
  Lape_Import_TScrollingWinControl(Compiler);
  Lape_Import_TGraphicControl(Compiler);
end;

end.

