type
  PBorderIcons = ^TBorderIcons;
  PCloseEvent = ^TCloseEvent;
  PCloseQueryEvent = ^TCloseQueryEvent;
  PConstraintSize = ^TConstraintSize;
  PCustomForm = ^TCustomForm;
  PForm = ^TForm;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PPosition = ^TPosition;
  PScrollBox = ^TScrollBox;
  PShowInTaskBar = ^TShowInTaskBar;
  PSizeConstraints = ^TSizeConstraints;

procedure _LapeSizeConstraints_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^ := TSizeConstraints.Create(PControl(Params^[1])^);
end;

procedure _LapeSizeConstraints_UpdateInterfaceConstraints(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.UpdateInterfaceConstraints();
end;

procedure _LapeSizeConstraints_SetInterfaceConstraints(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.SetInterfaceConstraints(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeSizeConstraints_EffectiveMinWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMinWidth();
end;

procedure _LapeSizeConstraints_EffectiveMinHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMinHeight();
end;

procedure _LapeSizeConstraints_EffectiveMaxWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMaxWidth();
end;

procedure _LapeSizeConstraints_EffectiveMaxHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMaxHeight();
end;

procedure _LapeSizeConstraints_MinMaxWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinMaxWidth(Pinteger(Params^[1])^);
end;

procedure _LapeSizeConstraints_MinMaxHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinMaxHeight(Pinteger(Params^[1])^);
end;

procedure _LapeSizeConstraints_MaxInterfaceHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MaxInterfaceHeight;
end;

procedure _LapeSizeConstraints_MaxInterfaceWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MaxInterfaceWidth;
end;

procedure _LapeSizeConstraints_MinInterfaceHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinInterfaceHeight;
end;

procedure _LapeSizeConstraints_MinInterfaceWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinInterfaceWidth;
end;

procedure _LapeSizeConstraints_Control_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PSizeConstraints(Params^[0])^.Control;
end;

procedure _LapeSizeConstraints_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  //PSizeConstraintsOptions(Result)^ := PSizeConstraints(Params^[0])^.Options;
end;

procedure _LapeSizeConstraints_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  // PSizeConstraints(Params^[0])^.Options := PSizeConstraintsOptions(Params^[1])^;
end;

procedure _LapeSizeConstraints_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSizeConstraints(Params^[0])^.OnChange;
end;

procedure _LapeSizeConstraints_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSizeConstraints_MaxHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PConstraintSize(Result)^ := PSizeConstraints(Params^[0])^.MaxHeight;
end;

procedure _LapeSizeConstraints_MaxHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MaxHeight := PConstraintSize(Params^[1])^;
end;

procedure _LapeSizeConstraints_MaxWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MaxWidth;
end;

procedure _LapeSizeConstraints_MaxWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MaxWidth := PInteger(Params^[1])^;
end;

procedure _LapeSizeConstraints_MinHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinHeight;
end;

procedure _LapeSizeConstraints_MinHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MinHeight := PInteger(Params^[1])^;
end;

procedure _LapeSizeConstraints_MinWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinWidth;
end;

procedure _LapeSizeConstraints_MinWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MinWidth := PConstraintSize(Params^[1])^;
end;

procedure _LapeSizeConstraints_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.Free();
end;

procedure _LapeCustomForm_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^ := TCustomForm.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomForm_CreateNew(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^ := TCustomForm.CreateNew(PComponent(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomForm_AfterConstruction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AfterConstruction();
end;

procedure _LapeCustomForm_BeforeDestruction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.BeforeDestruction();
end;

procedure _LapeCustomForm_Close(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Close();
end;

procedure _LapeCustomForm_CloseQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.CloseQuery();
end;

procedure _LapeCustomForm_DefocusControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.DefocusControl(PWinControl(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeCustomForm_DestroyWnd(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.DestroyWnd();
end;

procedure _LapeCustomForm_EnsureVisible(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.EnsureVisible(PBoolean(Params^[1])^);
end;

procedure _LapeCustomForm_FocusControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.FocusControl(PWinControl(Params^[1])^);
end;

procedure _LapeCustomForm_FormIsUpdating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.FormIsUpdating();
end;

procedure _LapeCustomForm_GetFormImage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PCustomForm(Params^[0])^.GetFormImage();
end;

procedure _LapeCustomForm_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Hide();
end;

procedure _LapeCustomForm_IntfDropFiles(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.IntfDropFiles(PStringArray(Params^[1])^);
end;

procedure _LapeCustomForm_IntfHelp(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.IntfHelp(PComponent(Params^[1])^);
end;

procedure _LapeCustomForm_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AutoSizeDelayedHandle();
end;

procedure _LapeCustomForm_GetPreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

procedure _LapeCustomForm_Release(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Release();
end;

procedure _LapeCustomForm_CanFocus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.CanFocus();
end;

procedure _LapeCustomForm_SetFocus(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.SetFocus();
end;

procedure _LapeCustomForm_SetFocusedControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.SetFocusedControl(PWinControl(Params^[1])^);
end;

procedure _LapeCustomForm_SetRestoredBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.SetRestoredBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeCustomForm_Show(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Show();
end;

procedure _LapeCustomForm_ShowModal(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomForm(Params^[0])^.ShowModal();
end;

procedure _LapeCustomForm_ShowOnTop(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ShowOnTop();
end;

procedure _LapeCustomForm_RemoveAllHandlersOfObject(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveAllHandlersOfObject(PObject(Params^[1])^);
end;

procedure _LapeCustomForm_AddHandlerFirstShow(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AddHandlerFirstShow(PNotifyEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeCustomForm_RemoveHandlerFirstShow(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveHandlerFirstShow(PNotifyEvent(Params^[1])^);
end;

procedure _LapeCustomForm_AddHandlerClose(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AddHandlerClose(PCloseEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeCustomForm_RemoveHandlerClose(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveHandlerClose(PCloseEvent(Params^[1])^);
end;

procedure _LapeCustomForm_AddHandlerCreate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AddHandlerCreate(PNotifyEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeCustomForm_RemoveHandlerCreate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveHandlerCreate(PNotifyEvent(Params^[1])^);
end;

procedure _LapeCustomForm_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.Active;
end;

procedure _LapeCustomForm_ActiveControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PCustomForm(Params^[0])^.ActiveControl;
end;

procedure _LapeCustomForm_ActiveControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ActiveControl := PWinControl(Params^[1])^;
end;

procedure _LapeCustomForm_ActiveDefaultControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.ActiveDefaultControl;
end;

procedure _LapeCustomForm_ActiveDefaultControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ActiveDefaultControl := PControl(Params^[1])^;
end;

procedure _LapeCustomForm_AllowDropFiles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AllowDropFiles;
end;

procedure _LapeCustomForm_AllowDropFiles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AllowDropFiles := PBoolean(Params^[1])^;
end;

procedure _LapeCustomForm_AlphaBlend_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AlphaBlend;
end;

procedure _LapeCustomForm_AlphaBlend_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AlphaBlend := PBoolean(Params^[1])^;
end;

procedure _LapeCustomForm_AlphaBlendValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  pbyte(Result)^ := PCustomForm(Params^[0])^.AlphaBlendValue;
end;

procedure _LapeCustomForm_AlphaBlendValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AlphaBlendValue := pbyte(Params^[1])^;
end;

procedure _LapeCustomForm_CancelControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.CancelControl;
end;

procedure _LapeCustomForm_CancelControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.CancelControl := PControl(Params^[1])^;
end;

procedure _LapeCustomForm_DefaultControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.DefaultControl;
end;

procedure _LapeCustomForm_DefaultControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.DefaultControl := PControl(Params^[1])^;
end;

procedure _LapeCustomForm_KeyPreview_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.KeyPreview;
end;

procedure _LapeCustomForm_KeyPreview_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.KeyPreview := PBoolean(Params^[1])^;
end;

procedure _LapeCustomForm_PopupParent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Result)^ := PCustomForm(Params^[0])^.PopupParent;
end;

procedure _LapeCustomForm_PopupParent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.PopupParent := PCustomForm(Params^[1])^;
end;

procedure _LapeCustomForm_OnActivate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnActivate;
end;

procedure _LapeCustomForm_OnActivate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnActivate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseEvent(Result)^ := PCustomForm(Params^[0])^.OnClose;
end;

procedure _LapeCustomForm_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnCloseQuery_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseQueryEvent(Result)^ := PCustomForm(Params^[0])^.OnCloseQuery;
end;

procedure _LapeCustomForm_OnCloseQuery_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnCloseQuery := PCloseQueryEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnCreate;
end;

procedure _LapeCustomForm_OnCreate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnDeactivate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnDeactivate;
end;

procedure _LapeCustomForm_OnDeactivate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnDeactivate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnDestroy;
end;

procedure _LapeCustomForm_OnDestroy_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnHide;
end;

procedure _LapeCustomForm_OnHide_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnShow;
end;

procedure _LapeCustomForm_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_OnWindowStateChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnWindowStateChange;
end;

procedure _LapeCustomForm_OnWindowStateChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnWindowStateChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PCustomForm(Params^[0])^.PixelsPerInch;
end;

procedure _LapeCustomForm_PixelsPerInch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.PixelsPerInch := PLongint(Params^[1])^;
end;

procedure _LapeCustomForm_RestoredLeft_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredLeft;
end;

procedure _LapeCustomForm_RestoredTop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredTop;
end;

procedure _LapeCustomForm_RestoredWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredWidth;
end;

procedure _LapeCustomForm_RestoredHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredHeight;
end;

procedure _LapeCustomForm_Write_BorderStyle(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

procedure _LapeCustomForm_Read_BorderStyle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFormBorderStyle(Result)^ := PCustomForm(Params^[0])^.BorderStyle;
end;

procedure _LapeCustomForm_Write_BorderIcons(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.BorderIcons := PBorderIcons(Params^[1])^;
end;

procedure _LapeCustomForm_Read_BorderIcons(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBorderIcons(Result)^ := PCustomForm(Params^[0])^.BorderIcons;
end;

procedure _LapeCustomForm_Constraints_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSizeConstraints(Result)^ := PCustomForm(Params^[0])^.Constraints;
end;

procedure _LapeCustomForm_Constraints_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Constraints := PSizeConstraints(Params^[1])^;
end;

procedure _LapeCustomForm_ShowInTaskBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PShowInTaskBar(Result)^ := PCustomForm(Params^[0])^.ShowInTaskBar;
end;

procedure _LapeCustomForm_ShowInTaskBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ShowInTaskBar := PShowInTaskBar(Params^[1])^;
end;

procedure _LapeCustomForm_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Free;
end;

procedure _LapeForm_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^ := TForm.Create(PComponent(Params^[1])^);
  PForm(Params^[0])^.ShowInTaskBar := stAlways;
end;

procedure _LapeForm_Cascade(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Cascade();
end;

procedure _LapeForm_Next(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Next();
end;

procedure _LapeForm_Previous(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Previous();
end;

procedure _LapeForm_Tile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Tile();
end;

procedure _LapeForm_Show(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Show();
end;

procedure _LapeForm_Close(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Close();
end;

procedure _LapeForm_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Hide();
end;

procedure _LapeForm_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientWidth;
end;

procedure _LapeForm_ClientWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

procedure _LapeForm_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientHeight;
end;

procedure _LapeForm_ClientHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

procedure _LapeForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseEvent(Result)^ := PForm(Params^[0])^.OnClose;
end;

procedure _LapeForm_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

procedure _LapeForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnCreate;
end;

procedure _LapeForm_OnCreate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDestroy;
end;

procedure _LapeForm_OnDestroy_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnHide;
end;

procedure _LapeForm_OnHide_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnPaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnPaint;
end;

procedure _LapeForm_OnPaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnShow;
end;

procedure _LapeForm_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDblClick;
end;

procedure _LapeForm_OnDblClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnEnter;
end;

procedure _LapeForm_OnEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnExit_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnExit;
end;

procedure _LapeForm_OnExit_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnClick;
end;

procedure _LapeForm_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnResize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnResize;
end;

procedure _LapeForm_OnResize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Enabled;
end;

procedure _LapeForm_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeForm_Font_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Result)^ := PForm(Params^[0])^.Font;
end;

procedure _LapeForm_Font_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Font := PFont(Params^[1])^;
end;

procedure _LapeForm_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Visible;
end;

procedure _LapeForm_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeForm_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PForm(Params^[0])^.Canvas;
end;

procedure _LapeForm_Canvas_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

procedure _LapeForm_Left_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Left;
end;

procedure _LapeForm_Left_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Left := PInteger(Params^[1])^;
end;

procedure _LapeForm_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Height;
end;

procedure _LapeForm_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeForm_Top_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Top;
end;

procedure _LapeForm_Top_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Top := PInteger(Params^[1])^;
end;

procedure _LapeForm_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Width;
end;

procedure _LapeForm_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeForm_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PForm(Params^[0])^.Caption;
end;

procedure _LapeForm_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

procedure _LapeForm_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPosition(Result)^ := PForm(Params^[0])^.Position;
end;

procedure _LapeForm_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Position := PPosition(Params^[1])^;
end;

procedure _LapeForm_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Free();
end;

procedure _LapeForm_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseMoveEvent(Result)^ := PForm(Params^[0])^.OnMouseMove;
end;

procedure _LapeForm_OnMouseMove_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PForm(Params^[0])^.OnMouseDown;
end;

procedure _LapeForm_OnMouseUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PForm(Params^[0])^.OnMouseUp;
end;

procedure _LapeForm_OnKeyUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

procedure _LapeForm_OnKeyUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PKeyEvent(Result)^ := PForm(Params^[0])^.OnKeyUp;
end;

procedure _LapeForm_OnKeyDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure _LapeForm_OnKeyDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PKeyEvent(Result)^ := PForm(Params^[0])^.OnKeyDown;
end;

procedure _LapeForm_OnKeyPress_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure _LapeForm_OnKeyPress_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PKeyPressEvent(Result)^ := PForm(Params^[0])^.OnKeyPress;
end;

procedure _LapeScrollBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollBox(Params^[0])^ := TScrollBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeScrollBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollBox(Params^[0])^.Free();
end;

