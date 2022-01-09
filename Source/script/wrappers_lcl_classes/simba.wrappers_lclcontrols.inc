type
  PAlign = ^TAlign;
  PControl = ^TControl;
  PControlScrollBar = ^TControlScrollBar;
  PCursor = ^TCursor;
  PCustomControl = ^TCustomControl;
  PFormBorderStyle = ^TFormBorderStyle;
  PGraphicControl = ^TGraphicControl;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;
  PScrollBarKind = ^TScrollBarKind;
  PScrollingWinControl = ^TScrollingWinControl;
  PWinControl = ^TWinControl;

procedure _LapeControl_AdjustSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.AdjustSize();
end;

procedure _LapeControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.AutoSizeDelayed();
end;

procedure _LapeControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PControl(Params^[0])^.AutoSizeDelayedReport();
end;

procedure _LapeControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSizeDelayedHandle();
end;

procedure _LapeControl_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_SetInitialBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetInitialBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_SetBoundsKeepBase(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetBoundsKeepBase(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_GetPreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

procedure _LapeControl_GetDefaultWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultWidth();
end;

procedure _LapeControl_GetDefaultHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultHeight();
end;

procedure _LapeControl_GetColorResolvingParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetColorResolvingParent();
end;

procedure _LapeControl_GetRGBColorResolvingParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetRGBColorResolvingParent();
end;

procedure _LapeControl_InvalidatePreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.InvalidatePreferredSize();
end;

procedure _LapeControl_UpdateBaseBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.UpdateBaseBounds(Pboolean(Params^[1])^, Pboolean(Params^[2])^, Pboolean(Params^[3])^);
end;

procedure _LapeControl_BaseBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BaseBounds;
end;

procedure _LapeControl_ReadBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.ReadBounds;
end;

procedure _LapeControl_WriteLayoutDebugReport(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.WriteLayoutDebugReport(PString(Params^[1])^);
end;

procedure _LapeControl_ShouldAutoAdjustLeftAndTop(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  // Breaking changes made in the LCL; this has been replaced with
  // ShouldAutoAdjust.

  //PBoolean(Result)^ := PControl(Params^[0])^.ShouldAutoAdjustLeftAndTop();
end;

procedure _LapeControl_ShouldAutoAdjustWidthAndHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  //PBoolean(Result)^ := PControl(Params^[0])^.ShouldAutoAdjustWidthAndHeight();
end;

procedure _LapeControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^ := TControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeControl_BeforeDestruction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BeforeDestruction();
end;

procedure _LapeControl_EditingDone(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.EditingDone();
end;

procedure _LapeControl_ExecuteDefaultAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ExecuteDefaultAction();
end;

procedure _LapeControl_ExecuteCancelAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ExecuteCancelAction();
end;

procedure _LapeControl_BringToFront(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BringToFront();
end;

procedure _LapeControl_HasParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.HasParent();
end;

procedure _LapeControl_GetParentComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PControl(Params^[0])^.GetParentComponent();
end;

procedure _LapeControl_IsParentOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsParentOf(PControl(Params^[1])^);
end;

procedure _LapeControl_GetTopParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PControl(Params^[0])^.GetTopParent();
end;

procedure _LapeControl_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsVisible();
end;

procedure _LapeControl_IsControlVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControlVisible();
end;

procedure _LapeControl_IsEnabled(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsEnabled();
end;

procedure _LapeControl_IsParentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentColor();
end;

procedure _LapeControl_IsParentFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentFont();
end;

procedure _LapeControl_FormIsUpdating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.FormIsUpdating();
end;

procedure _LapeControl_IsProcessingPaintMsg(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsProcessingPaintMsg();
end;

procedure _LapeControl_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Hide();
end;

procedure _LapeControl_Refresh(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Refresh();
end;

procedure _LapeControl_Repaint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Repaint();
end;

procedure _LapeControl_Invalidate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Invalidate();
end;

procedure _LapeControl_SendToBack(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SendToBack();
end;

procedure _LapeControl_UpdateRolesForForm(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.UpdateRolesForForm();
end;

procedure _LapeControl_ActiveDefaultControlChanged(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

procedure _LapeControl_GetTextBuf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetTextBuf(PPChar(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeControl_GetTextLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.GetTextLen();
end;

procedure _LapeControl_SetTextBuf(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetTextBuf(PPChar(Params^[1])^);
end;

procedure _LapeControl_ScreenToClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToClient(PPoint(Params^[1])^);
end;

procedure _LapeControl_ClientToScreen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientToScreen(PPoint(Params^[1])^);
end;

procedure _LapeControl_ScreenToControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToControl(PPoint(Params^[1])^);
end;

procedure _LapeControl_ControlToScreen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlToScreen(PPoint(Params^[1])^);
end;

procedure _LapeControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.GetChildrenRect(Pboolean(Params^[1])^);
end;

procedure _LapeControl_Show(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Show();
end;

procedure _LapeControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Update();
end;

procedure _LapeControl_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.HandleObjectShouldBeVisible();
end;

procedure _LapeControl_ParentDestroyingHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.ParentDestroyingHandle();
end;

procedure _LapeControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.ParentHandlesAllocated();
end;

procedure _LapeControl_InitiateAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.InitiateAction();
end;

procedure _LapeControl_AutoSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSize;
end;

procedure _LapeControl_AutoSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.AutoSize := PBoolean(Params^[1])^;
end;

procedure _LapeControl_BoundsRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRect;
end;

procedure _LapeControl_BoundsRect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BoundsRect := PRect(Params^[1])^;
end;

procedure _LapeControl_BoundsRectForNewParent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRectForNewParent;
end;

procedure _LapeControl_BoundsRectForNewParent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BoundsRectForNewParent := PRect(Params^[1])^;
end;

procedure _LapeControl_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PControl(Params^[0])^.Caption;
end;

procedure _LapeControl_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Caption := PString(Params^[1])^;
end;

procedure _LapeControl_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientHeight;
end;

procedure _LapeControl_ClientHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

procedure _LapeControl_ClientOrigin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientOrigin;
end;

procedure _LapeControl_ClientRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.ClientRect;
end;

procedure _LapeControl_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientWidth;
end;

procedure _LapeControl_ClientWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

procedure _LapeControl_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Color;
end;

procedure _LapeControl_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapeControl_ControlOrigin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlOrigin;
end;

procedure _LapeControl_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Enabled;
end;

procedure _LapeControl_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Font_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Result)^ := PControl(Params^[0])^.Font;
end;

procedure _LapeControl_Font_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Font := PFont(Params^[1])^;
end;

procedure _LapeControl_IsControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControl;
end;

procedure _LapeControl_IsControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.IsControl := PBoolean(Params^[1])^;
end;

procedure _LapeControl_MouseEntered_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.MouseInClient;
end;

procedure _LapeControl_OnChangeBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnChangeBounds;
end;

procedure _LapeControl_OnChangeBounds_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnChangeBounds := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnClick;
end;

procedure _LapeControl_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_OnResize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnResize;
end;

procedure _LapeControl_OnResize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_Align_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PAlign(Result)^ := PControl(Params^[0])^.Align;
end;

procedure _LapeControl_Align_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

procedure _LapeControl_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Visible;
end;

procedure _LapeControl_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeControl_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftAlignment();
end;

procedure _LapeControl_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftReading();
end;

procedure _LapeControl_UseRightToLeftScrollBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.UseRightToLeftScrollBar();
end;

procedure _LapeControl_IsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsRightToLeft();
end;

procedure _LapeControl_Left_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Left;
end;

procedure _LapeControl_Left_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Left := PInteger(Params^[1])^;
end;

procedure _LapeControl_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Height;
end;

procedure _LapeControl_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeControl_Top_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Top;
end;

procedure _LapeControl_Top_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Top := PInteger(Params^[1])^;
end;

procedure _LapeControl_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Width;
end;

procedure _LapeControl_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeControl_ShowHint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ShowHint := True;
end;

procedure _LapeControl_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PControl(Params^[0])^.Parent;
end;

procedure _LapeControl_Parent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Parent := PWinControl(Params^[1])^;
end;

procedure _LapeControl_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PControl(Params^[0])^.Hint;
end;

procedure _LapeControl_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Hint := PString(Params^[1])^;
end;

procedure _LapeControl_ShowHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShowHint;
end;

procedure _LapeControl_ShowHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Cursor_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCursor(Result)^ := PControl(Params^[0])^.Cursor;
end;

procedure _LapeControl_Cursor_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

procedure _LapeControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Free();
end;

procedure _LapeWinControl_BorderWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BorderWidth;
end;

procedure _LapeWinControl_BorderWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.BorderWidth := Pinteger(Params^[1])^;
end;

procedure _LapeWinControl_BoundsLockCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BoundsLockCount;
end;

procedure _LapeWinControl_Brush_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Result)^ := PWinControl(Params^[0])^.Brush;
end;

procedure _LapeWinControl_CachedClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientHeight;
end;

procedure _LapeWinControl_CachedClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientWidth;
end;

procedure _LapeWinControl_ControlCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.ControlCount;
end;

procedure _LapeWinControl_DoubleBuffered_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.DoubleBuffered;
end;

procedure _LapeWinControl_DoubleBuffered_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DoubleBuffered := PBoolean(Params^[1])^;
end;

procedure _LapeWinControl_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.Handle;
end;

procedure _LapeWinControl_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapeWinControl_IsResizing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.IsResizing;
end;

procedure _LapeWinControl_TabOrder_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.TabOrder;
end;

procedure _LapeWinControl_TabOrder_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.TabOrder := PInteger(Params^[1])^;
end;

procedure _LapeWinControl_TabStop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.TabStop;
end;

procedure _LapeWinControl_TabStop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.TabStop := PBoolean(Params^[1])^;
end;

procedure _LapeWinControl_OnEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnEnter;
end;

procedure _LapeWinControl_OnEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnExit_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnExit;
end;

procedure _LapeWinControl_OnExit_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyPress_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

procedure _LapeWinControl_ParentWindow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.ParentWindow;
end;

procedure _LapeWinControl_ParentWindow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ParentWindow := PHandle(Params^[1])^;
end;

procedure _LapeWinControl_Showing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Showing;
end;

procedure _LapeWinControl_VisibleDockClientCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.VisibleDockClientCount;
end;

procedure _LapeWinControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayed();
end;

procedure _LapeWinControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedReport();
end;

procedure _LapeWinControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedHandle();
end;

procedure _LapeWinControl_BeginUpdateBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.BeginUpdateBounds();
end;

procedure _LapeWinControl_EndUpdateBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.EndUpdateBounds();
end;

procedure _LapeWinControl_LockRealizeBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.LockRealizeBounds();
end;

procedure _LapeWinControl_UnlockRealizeBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.UnlockRealizeBounds();
end;

procedure _LapeWinControl_ControlAtPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeWinControl_ControlAtPosEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeWinControl_ContainsControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.ContainsControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_DoAdjustClientRectChange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DoAdjustClientRectChange(PBoolean(Params^[1])^);
end;

procedure _LapeWinControl_InvalidateClientRectCache(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InvalidateClientRectCache(Pboolean(Params^[1])^);
end;

procedure _LapeWinControl_ClientRectNeedsInterfaceUpdate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ClientRectNeedsInterfaceUpdate();
end;

procedure _LapeWinControl_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeWinControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PWinControl(Params^[0])^.GetChildrenRect(Pboolean(Params^[1])^);
end;

procedure _LapeWinControl_DisableAlign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DisableAlign();
end;

procedure _LapeWinControl_EnableAlign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.EnableAlign();
end;

procedure _LapeWinControl_ReAlign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ReAlign();
end;

procedure _LapeWinControl_ScrollBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeWinControl_WriteLayoutDebugReport(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.WriteLayoutDebugReport(PString(Params^[1])^);
end;

procedure _LapeWinControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^ := TWinControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeWinControl_CreateParented(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^ := TWinControl.CreateParented(Phandle(Params^[1])^);
end;

procedure _LapeWinControl_CanFocus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.CanFocus();
end;

procedure _LapeWinControl_GetControlIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.GetControlIndex(PControl(Params^[1])^);
end;

procedure _LapeWinControl_SetControlIndex(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetControlIndex(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeWinControl_Focused(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Focused();
end;

procedure _LapeWinControl_PerformTab(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.PerformTab(Pboolean(Params^[1])^);
end;

procedure _LapeWinControl_FindChildControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.FindChildControl(PString(Params^[1])^);
end;

procedure _LapeWinControl_SelectNext(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SelectNext(PWinControl(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeWinControl_GetTextLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.GetTextLen();
end;

procedure _LapeWinControl_Invalidate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Invalidate();
end;

procedure _LapeWinControl_AddControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.AddControl();
end;

procedure _LapeWinControl_InsertControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_InsertControlEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeWinControl_RemoveControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.RemoveControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_Repaint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Repaint();
end;

procedure _LapeWinControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Update();
end;

procedure _LapeWinControl_SetFocus(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetFocus();
end;

procedure _LapeWinControl_FlipChildren(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.FlipChildren(PBoolean(Params^[1])^);
end;

procedure _LapeWinControl_ScaleBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ScaleBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeWinControl_GetDockCaption(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PWinControl(Params^[0])^.GetDockCaption(PControl(Params^[1])^);
end;

procedure _LapeWinControl_UpdateDockCaption(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.UpdateDockCaption(PControl(Params^[1])^);
end;

procedure _LapeWinControl_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.HandleAllocated();
end;

procedure _LapeWinControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ParentHandlesAllocated();
end;

procedure _LapeWinControl_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.HandleNeeded();
end;

procedure _LapeWinControl_BrushCreated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.BrushCreated();
end;

procedure _LapeWinControl_PaintTo(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.PaintTo(PCanvas(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeWinControl_SetShape(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetShape(PBitmap(Params^[1])^);
end;

procedure _LapeWinControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Free();
end;

procedure _LapeCustomControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^ := TCustomControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomControl(Params^[0])^.Canvas;
end;

procedure _LapeCustomControl_Canvas_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

procedure _LapeCustomControl_OnPaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomControl(Params^[0])^.OnPaint;
end;

procedure _LapeCustomControl_OnPaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomControl_BorderStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

procedure _LapeCustomControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.Free();
end;

procedure _LapeControlScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^ := TControlScrollBar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

procedure _LapeControlScrollBar_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeControlScrollBar_IsScrollBarVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.IsScrollBarVisible();
end;

procedure _LapeControlScrollBar_ScrollPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.ScrollPos();
end;

procedure _LapeControlScrollBar_GetOtherScrollBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PControlScrollBar(Params^[0])^.GetOtherScrollBar();
end;

procedure _LapeControlScrollBar_Size_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.Size;
end;

procedure _LapeControlScrollBar_ClientSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSize();
end;

procedure _LapeControlScrollBar_ClientSizeWithBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithBar();
end;

procedure _LapeControlScrollBar_ClientSizeWithoutBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithoutBar();
end;

procedure _LapeControlScrollBar_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Increment;
end;

procedure _LapeControlScrollBar_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Increment := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Page_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Page;
end;

procedure _LapeControlScrollBar_Page_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Page := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Smooth;
end;

procedure _LapeControlScrollBar_Smooth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Smooth := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Position;
end;

procedure _LapeControlScrollBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Range_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Range;
end;

procedure _LapeControlScrollBar_Range_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Range := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Tracking_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Tracking;
end;

procedure _LapeControlScrollBar_Tracking_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Tracking := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Visible;
end;

procedure _LapeControlScrollBar_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Free();
end;

procedure _LapeScrollingWinControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^ := TScrollingWinControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeScrollingWinControl_UpdateScrollbars(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.UpdateScrollbars();
end;

procedure _LapeScrollingWinControl_ScrollBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeScrollingWinControl_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.HorzScrollBar;
end;

procedure _LapeScrollingWinControl_HorzScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.HorzScrollBar := PControlScrollBar(Params^[1])^;
end;

procedure _LapeScrollingWinControl_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.VertScrollBar;
end;

procedure _LapeScrollingWinControl_VertScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.VertScrollBar := PControlScrollBar(Params^[1])^;
end;

procedure _LapeScrollingWinControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.Free();
end;

procedure _LapeGraphicControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PGraphicControl(Params^[0])^.Canvas;
end;

procedure _LapeGraphicControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicControl(Params^[0])^.Update();
end;

procedure _LapeGraphicControl_Align_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PAlign(Result)^ := PGraphicControl(Params^[0])^.Align;
end;

procedure _LapeGraphicControl_Align_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

