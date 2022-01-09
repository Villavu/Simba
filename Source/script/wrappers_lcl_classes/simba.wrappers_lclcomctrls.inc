type
  PCaption = ^TCaption;
  PCheckListBox = ^TCheckListBox;
  PCheckListClicked = ^TCheckListClicked;
  PCustomCheckListBox = ^TCustomCheckListBox;
  PCustomPage = ^TCustomPage;
  PCustomProgressBar = ^TCustomProgressBar;
  PCustomTabControl = ^TCustomTabControl;
  PCustomTrackBar = ^TCustomTrackBar;
  PPageControl = ^TPageControl;
  PPanelParts = ^TPanelParts;
  PProgressBar = ^TProgressBar;
  PProgressBarOrientation = ^TProgressBarOrientation;
  PProgressBarStyle = ^TProgressBarStyle;
  PStatusBar = ^TStatusBar;
  PStatusPanel = ^TStatusPanel;
  PStatusPanelBevel = ^TStatusPanelBevel;
  PStatusPanels = ^TStatusPanels;
  PStatusPanelStyle = ^TStatusPanelStyle;
  PTabChangingEvent = ^TTabChangingEvent;
  PTabControlOptions = ^TCTabControlOptions;
  PTabPosition = ^TTabPosition;
  PTabSheet = ^TTabSheet;
  PTickMark = ^TTickMark;
  PTickStyle = ^TTickStyle;
  PTrackBar = ^TTrackBar;
  PTrackBarOrientation = ^TTrackBarOrientation;
  PTrackBarScalePos = ^TTrackBarScalePos;

procedure _LapeCustomProgressBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^ := TCustomProgressBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomProgressBar_StepIt(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.StepIt();
end;

procedure _LapeCustomProgressBar_StepBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.StepBy(PInteger(Params^[1])^);
end;

procedure _LapeCustomProgressBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Max;
end;

procedure _LapeCustomProgressBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Min;
end;

procedure _LapeCustomProgressBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProgressBarOrientation(Result)^ := PCustomProgressBar(Params^[0])^.Orientation;
end;

procedure _LapeCustomProgressBar_Orientation_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Orientation := PProgressBarOrientation(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Position;
end;

procedure _LapeCustomProgressBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.Smooth;
end;

procedure _LapeCustomProgressBar_Smooth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Smooth := Pboolean(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Step_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Step;
end;

procedure _LapeCustomProgressBar_Step_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Step := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProgressBarStyle(Result)^ := PCustomProgressBar(Params^[0])^.Style;
end;

procedure _LapeCustomProgressBar_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Style := PProgressBarStyle(Params^[1])^;
end;

procedure _LapeCustomProgressBar_BarShowText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.BarShowText;
end;

procedure _LapeCustomProgressBar_BarShowText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.BarShowText := Pboolean(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Free();
end;

procedure _LapeProgressBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProgressBar(Params^[0])^ := TProgressBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeProgressBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PProgressBar(Params^[0])^.Free();
end;

procedure _LapeCustomTrackBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^ := TCustomTrackBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomTrackBar_SetTick(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.SetTick(PInteger(Params^[1])^);
end;

procedure _LapeCustomTrackBar_Frequency_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Frequency;
end;

procedure _LapeCustomTrackBar_Frequency_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Frequency := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_LineSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.LineSize;
end;

procedure _LapeCustomTrackBar_LineSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.LineSize := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Max;
end;

procedure _LapeCustomTrackBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Min;
end;

procedure _LapeCustomTrackBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTrackBar(Params^[0])^.OnChange;
end;

procedure _LapeCustomTrackBar_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTrackBarOrientation(Result)^ := PCustomTrackBar(Params^[0])^.Orientation;
end;

procedure _LapeCustomTrackBar_Orientation_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Orientation := PTrackBarOrientation(Params^[1])^;
end;

procedure _LapeCustomTrackBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.PageSize;
end;

procedure _LapeCustomTrackBar_PageSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Position;
end;

procedure _LapeCustomTrackBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Reversed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.Reversed;
end;

procedure _LapeCustomTrackBar_Reversed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Reversed := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTrackBar_ScalePos_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTrackBarScalePos(Result)^ := PCustomTrackBar(Params^[0])^.ScalePos;
end;

procedure _LapeCustomTrackBar_ScalePos_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.ScalePos := PTrackBarScalePos(Params^[1])^;
end;

procedure _LapeCustomTrackBar_SelEnd_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelEnd;
end;

procedure _LapeCustomTrackBar_SelEnd_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.SelEnd := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelStart;
end;

procedure _LapeCustomTrackBar_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.SelStart := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_ShowSelRange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.ShowSelRange;
end;

procedure _LapeCustomTrackBar_ShowSelRange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.ShowSelRange := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTrackBar_TickMarks_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTickMark(Result)^ := PCustomTrackBar(Params^[0])^.TickMarks;
end;

procedure _LapeCustomTrackBar_TickMarks_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.TickMarks := PTickMark(Params^[1])^;
end;

procedure _LapeCustomTrackBar_TickStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTickStyle(Result)^ := PCustomTrackBar(Params^[0])^.TickStyle;
end;

procedure _LapeCustomTrackBar_TickStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.TickStyle := PTickStyle(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Free();
end;

procedure _LapeTrackBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTrackBar(Params^[0])^ := TTrackBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeTrackBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTrackBar(Params^[0])^.Free();
end;

procedure _LapeCustomCheckListBox_MeasureItem(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomCheckListBox_Toggle(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.Toggle(PInteger(Params^[1])^);
end;

procedure _LapeCustomCheckListBox_CheckAll(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.CheckAll(PCheckBoxState(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeCustomCheckListBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.AllowGrayed;
end;

procedure _LapeCustomCheckListBox_AllowGrayed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

procedure _LapeCustomCheckListBox_Checked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^];
end;

procedure _LapeCustomCheckListBox_Checked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

procedure _LapeCustomCheckListBox_ItemEnabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^];
end;

procedure _LapeCustomCheckListBox_ItemEnabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

procedure _LapeCustomCheckListBox_State_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCheckBoxState(Result)^ := PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^];
end;

procedure _LapeCustomCheckListBox_State_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^] := PCheckBoxState(Params^[1])^;
end;

procedure _LapeCustomCheckListBox_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomCheckListBox(Params^[0])^.Count;
end;

procedure _LapeCustomCheckListBox_OnClickCheck_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomCheckListBox(Params^[0])^.OnClickCheck;
end;

procedure _LapeCustomCheckListBox_OnClickCheck_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.OnClickCheck := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomCheckListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^ := TCustomCheckListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomCheckListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.Free();
end;

procedure _LapeCustomCheckListBox_OnCheckListClicked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.OnItemClick := PCheckListClicked(Params^[1])^;
end;

procedure _LapeCheckListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCheckListBox(Params^[0])^ := TCheckListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCheckListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCheckListBox(Params^[0])^.Free();
end;

procedure _LapeCustomPage_CanTab(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.CanTab();
end;

procedure _LapeCustomPage_IsControlVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.IsControlVisible();
end;

procedure _LapeCustomPage_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.HandleObjectShouldBeVisible();
end;

procedure _LapeCustomPage_VisibleIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomPage(Params^[0])^.VisibleIndex();
end;

procedure _LapeCustomPage_PageIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomPage(Params^[0])^.PageIndex;
end;

procedure _LapeCustomPage_PageIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomPage_TabVisible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.TabVisible;
end;

procedure _LapeCustomPage_TabVisible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.TabVisible := PBoolean(Params^[1])^;
end;

procedure _LapeCustomPage_OnHide_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnHide;
end;

procedure _LapeCustomPage_OnHide_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomPage_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnShow;
end;

procedure _LapeCustomPage_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomPage_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^ := TCustomPage.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomPage_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.Free();
end;

procedure _LapeCustomTabControl_TabRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomTabControl(Params^[0])^.TabRect(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_GetImageIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.GetImageIndex(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.IndexOf(PPersistent(Params^[1])^);
end;

procedure _LapeCustomTabControl_CustomPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.CustomPage(Pinteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_CanChangePageIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomTabControl(Params^[0])^.CanChangePageIndex();
end;

procedure _LapeCustomTabControl_GetMinimumTabWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.GetMinimumTabWidth();
end;

procedure _LapeCustomTabControl_GetMinimumTabHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.GetMinimumTabHeight();
end;

procedure _LapeCustomTabControl_TabToPageIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.TabToPageIndex(Pinteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_PageToTabIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.PageToTabIndex(Pinteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_IndexOfTabAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  //PInteger(Result)^ := PCustomTabControl(Params^[0])^.IndexOfTabAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomTabControl_DoCloseTabClicked(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.DoCloseTabClicked(PCustomPage(Params^[1])^);
end;

procedure _LapeCustomTabControl_MultiLine_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.MultiLine;
end;

procedure _LapeCustomTabControl_MultiLine_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.MultiLine := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTabControl_OnChanging_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabChangingEvent(Result)^ := PCustomTabControl(Params^[0])^.OnChanging;
end;

procedure _LapeCustomTabControl_OnChanging_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.OnChanging := PTabChangingEvent(Params^[1])^;
end;

procedure _LapeCustomTabControl_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabControlOptions(Result)^ := PCustomTabControl(Params^[0])^.Options;
end;

procedure _LapeCustomTabControl_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.Options := PTabControlOptions(Params^[1])^;
end;

procedure _LapeCustomTabControl_Page_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.Page[PInteger(Params^[1])^];
end;

procedure _LapeCustomTabControl_PageCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.PageCount;
end;

procedure _LapeCustomTabControl_PageIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.PageIndex;
end;

procedure _LapeCustomTabControl_PageIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomTabControl_Pages_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomTabControl(Params^[0])^.Pages;
end;

procedure _LapeCustomTabControl_Pages_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.Pages := PStrings(Params^[1])^;
end;

procedure _LapeCustomTabControl_ShowTabs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.ShowTabs;
end;

procedure _LapeCustomTabControl_ShowTabs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.ShowTabs := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTabControl_TabPosition_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabPosition(Result)^ := PCustomTabControl(Params^[0])^.TabPosition;
end;

procedure _LapeCustomTabControl_TabPosition_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.TabPosition := PTabPosition(Params^[1])^;
end;

procedure _LapeCustomTabControl_IsUnpaged_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  //Pboolean(Result)^ := PCustomTabControl(Params^[0])^.IsUnpaged;
end;

procedure _LapeCustomTabControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^ := TCustomTabControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomTabControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.Free();
end;

procedure _LapeTabSheet_TabIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PTabSheet(Params^[0])^.TabIndex;
end;

procedure _LapeTabSheet_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabSheet(Params^[0])^ := TTabSheet.Create(PComponent(Params^[1])^);
end;

procedure _LapeTabSheet_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabSheet(Params^[0])^.Free();
end;

procedure _LapePageControl_SelectNextPage(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^);
end;

procedure _LapePageControl_SelectNextPageEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapePageControl_AddTabSheet(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.AddTabSheet();
end;

procedure _LapePageControl_Pages_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.Pages[PInteger(Params^[1])^];
end;

procedure _LapePageControl_ActivePage_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.ActivePage;
end;

procedure _LapePageControl_ActivePage_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.ActivePage := PTabSheet(Params^[1])^;
end;

procedure _LapePageControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^ := TPageControl.Create(PComponent(Params^[1])^);
end;

procedure _LapePageControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.Free();
end;

procedure _LapeStatusPanel_StatusBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Result)^ := PStatusPanel(Params^[0])^.StatusBar();
end;

procedure _LapeStatusPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PAlignment(Result)^ := PStatusPanel(Params^[0])^.Alignment;
end;

procedure _LapeStatusPanel_Alignment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

procedure _LapeStatusPanel_Bevel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanelBevel(Result)^ := PStatusPanel(Params^[0])^.Bevel;
end;

procedure _LapeStatusPanel_Bevel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Bevel := PStatusPanelBevel(Params^[1])^;
end;

procedure _LapeStatusPanel_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanelStyle(Result)^ := PStatusPanel(Params^[0])^.Style;
end;

procedure _LapeStatusPanel_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Style := PStatusPanelStyle(Params^[1])^;
end;

procedure _LapeStatusPanel_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStatusPanel(Params^[0])^.Text;
end;

procedure _LapeStatusPanel_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Text := PCaption(Params^[1])^;
end;

procedure _LapeStatusPanel_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStatusPanel(Params^[0])^.Width;
end;

procedure _LapeStatusPanel_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeStatusPanel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^ := TStatusPanel.Create(PCollection(Params^[1])^);
end;

procedure _LapeStatusPanel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Free();
end;

procedure _LapeStatusPanels_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanels(Params^[0])^ := TStatusPanels.Create(PStatusBar(Params^[1])^);
end;

procedure _LapeStatusPanels_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Add();
end;

procedure _LapeStatusPanels_Items_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeStatusPanels_Items_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^] := PStatusPanel(Params^[2])^;
end;

procedure _LapeStatusPanels_StatusBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Result)^ := PStatusPanels(Params^[0])^.StatusBar;
end;

procedure _LapeStatusPanels_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanels(Params^[0])^.Free();
end;

procedure _LapeStatusBar_InvalidatePanel(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.InvalidatePanel(Pinteger(Params^[1])^, PPanelParts(Params^[2])^);
end;

procedure _LapeStatusBar_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.BeginUpdate();
end;

procedure _LapeStatusBar_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.EndUpdate();
end;

procedure _LapeStatusBar_GetPanelIndexAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStatusBar(Params^[0])^.GetPanelIndexAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStatusBar_SizeGripEnabled(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGripEnabled();
end;

procedure _LapeStatusBar_UpdatingStatusBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PStatusBar(Params^[0])^.UpdatingStatusBar();
end;

procedure _LapeStatusBar_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PStatusBar(Params^[0])^.Canvas;
end;

procedure _LapeStatusBar_AutoHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.AutoHint;
end;

procedure _LapeStatusBar_AutoHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.AutoHint := PBoolean(Params^[1])^;
end;

procedure _LapeStatusBar_Panels_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusPanels(Result)^ := PStatusBar(Params^[0])^.Panels;
end;

procedure _LapeStatusBar_Panels_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.Panels := PStatusPanels(Params^[1])^;
end;

procedure _LapeStatusBar_SimpleText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCaption(Result)^ := PStatusBar(Params^[0])^.SimpleText;
end;

procedure _LapeStatusBar_SimpleText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.SimpleText := PCaption(Params^[1])^;
end;

procedure _LapeStatusBar_SimplePanel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SimplePanel;
end;

procedure _LapeStatusBar_SimplePanel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.SimplePanel := PBoolean(Params^[1])^;
end;

procedure _LapeStatusBar_SizeGrip_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGrip;
end;

procedure _LapeStatusBar_SizeGrip_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.SizeGrip := PBoolean(Params^[1])^;
end;

procedure _LapeStatusBar_OnHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PStatusBar(Params^[0])^.OnHint;
end;

procedure _LapeStatusBar_OnHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.OnHint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeStatusBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^ := TStatusBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeStatusBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.Free();
end;

