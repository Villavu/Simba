unit simba.import_lcl_comctrls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportLCLComCtrls(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, extctrls, comctrls, graphics, checklst, stdctrls, lptypes, ffi;

type
  PRect = ^TRect;
  PCollection = ^TCollection;
  PStrings = ^TStrings;
  PAlignment = ^TAlignment;
  PCanvas = ^TCanvas;
  PNotifyEvent = ^TNotifyEvent;
  PComponent = ^TComponent;
  PCaption = ^TCaption;
  PCheckBoxState = ^TCheckBoxState;
  PCheckListBox = ^TCheckListBox;
  PCheckListClicked = ^TCheckListClicked;
  PCustomCheckListBox = ^TCustomCheckListBox;
  PCustomPage = ^TCustomPage;
  PCustomProgressBar = ^TCustomProgressBar;
  PCustomTabControl = ^TCustomTabControl;
  PCustomTrackBar = ^TCustomTrackBar;
  PPageControl = ^TPageControl;
  PProgressBar = ^TProgressBar;
  PStatusBar = ^TStatusBar;
  PStatusPanel = ^TStatusPanel;
  PStatusPanels = ^TStatusPanels;
  PTabChangingEvent = ^TTabChangingEvent;
  PTabPosition = ^TTabPosition;
  PTabSheet = ^TTabSheet;
  PTickMark = ^TTickMark;
  PTickStyle = ^TTickStyle;
  PTrackBar = ^TTrackBar;
  PTrackBarOrientation = ^TTrackBarOrientation;

procedure _LapeCustomProgressBar_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^ := TCustomProgressBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomProgressBar_StepIt(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.StepIt();
end;

procedure _LapeCustomProgressBar_StepBy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.StepBy(PInteger(Params^[1])^);
end;

procedure _LapeCustomProgressBar_Max_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Max;
end;

procedure _LapeCustomProgressBar_Max_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Min_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Min;
end;

procedure _LapeCustomProgressBar_Min_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Position;
end;

procedure _LapeCustomProgressBar_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.Smooth;
end;

procedure _LapeCustomProgressBar_Smooth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.Smooth := Pboolean(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Step_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Step;
end;

procedure _LapeCustomProgressBar_Step_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.Step := PInteger(Params^[1])^;
end;

procedure _LapeCustomProgressBar_BarShowText_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.BarShowText;
end;

procedure _LapeCustomProgressBar_BarShowText_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.BarShowText := Pboolean(Params^[1])^;
end;

procedure _LapeCustomProgressBar_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Params^[0])^.Free();
end;

procedure _LapeProgressBar_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^ := TProgressBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeProgressBar_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.Free();
end;

procedure _LapeCustomTrackBar_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^ := TCustomTrackBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomTrackBar_SetTick(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.SetTick(PInteger(Params^[1])^);
end;

procedure _LapeCustomTrackBar_Frequency_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Frequency;
end;

procedure _LapeCustomTrackBar_Frequency_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Frequency := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_LineSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.LineSize;
end;

procedure _LapeCustomTrackBar_LineSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.LineSize := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Max_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Max;
end;

procedure _LapeCustomTrackBar_Max_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Min_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Min;
end;

procedure _LapeCustomTrackBar_Min_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomTrackBar(Params^[0])^.OnChange;
end;

procedure _LapeCustomTrackBar_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBarOrientation(Result)^ := PCustomTrackBar(Params^[0])^.Orientation;
end;

procedure _LapeCustomTrackBar_Orientation_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Orientation := PTrackBarOrientation(Params^[1])^;
end;

procedure _LapeCustomTrackBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.PageSize;
end;

procedure _LapeCustomTrackBar_PageSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Position;
end;

procedure _LapeCustomTrackBar_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Reversed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.Reversed;
end;

procedure _LapeCustomTrackBar_Reversed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Reversed := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTrackBar_SelEnd_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelEnd;
end;

procedure _LapeCustomTrackBar_SelEnd_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.SelEnd := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_SelStart_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelStart;
end;

procedure _LapeCustomTrackBar_SelStart_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.SelStart := PInteger(Params^[1])^;
end;

procedure _LapeCustomTrackBar_ShowSelRange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.ShowSelRange;
end;

procedure _LapeCustomTrackBar_ShowSelRange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.ShowSelRange := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTrackBar_TickMarks_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTickMark(Result)^ := PCustomTrackBar(Params^[0])^.TickMarks;
end;

procedure _LapeCustomTrackBar_TickMarks_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.TickMarks := PTickMark(Params^[1])^;
end;

procedure _LapeCustomTrackBar_TickStyle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTickStyle(Result)^ := PCustomTrackBar(Params^[0])^.TickStyle;
end;

procedure _LapeCustomTrackBar_TickStyle_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.TickStyle := PTickStyle(Params^[1])^;
end;

procedure _LapeCustomTrackBar_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Params^[0])^.Free();
end;

procedure _LapeTrackBar_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^ := TTrackBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeTrackBar_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Free();
end;

procedure _LapeCustomCheckListBox_MeasureItem(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomCheckListBox_Toggle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.Toggle(PInteger(Params^[1])^);
end;

procedure _LapeCustomCheckListBox_CheckAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.CheckAll(PCheckBoxState(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeCustomCheckListBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.AllowGrayed;
end;

procedure _LapeCustomCheckListBox_AllowGrayed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

procedure _LapeCustomCheckListBox_Checked_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^];
end;

procedure _LapeCustomCheckListBox_Checked_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

procedure _LapeCustomCheckListBox_ItemEnabled_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^];
end;

procedure _LapeCustomCheckListBox_ItemEnabled_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

procedure _LapeCustomCheckListBox_State_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckBoxState(Result)^ := PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^];
end;

procedure _LapeCustomCheckListBox_State_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^] := PCheckBoxState(Params^[1])^;
end;

procedure _LapeCustomCheckListBox_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomCheckListBox(Params^[0])^.Count;
end;

procedure _LapeCustomCheckListBox_OnClickCheck_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomCheckListBox(Params^[0])^.OnClickCheck;
end;

procedure _LapeCustomCheckListBox_OnClickCheck_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.OnClickCheck := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomCheckListBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^ := TCustomCheckListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomCheckListBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.Free();
end;

procedure _LapeCustomCheckListBox_OnCheckListClicked_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.OnItemClick := PCheckListClicked(Params^[1])^;
end;

procedure _LapeCheckListBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^ := TCheckListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCheckListBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.Free();
end;

procedure _LapeCustomPage_CanTab(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.CanTab();
end;

procedure _LapeCustomPage_IsControlVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.IsControlVisible();
end;

procedure _LapeCustomPage_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.HandleObjectShouldBeVisible();
end;

procedure _LapeCustomPage_VisibleIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomPage(Params^[0])^.VisibleIndex();
end;

procedure _LapeCustomPage_PageIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomPage(Params^[0])^.PageIndex;
end;

procedure _LapeCustomPage_PageIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomPage_TabVisible_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.TabVisible;
end;

procedure _LapeCustomPage_TabVisible_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Params^[0])^.TabVisible := PBoolean(Params^[1])^;
end;

procedure _LapeCustomPage_OnHide_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnHide;
end;

procedure _LapeCustomPage_OnHide_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomPage_OnShow_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnShow;
end;

procedure _LapeCustomPage_OnShow_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomPage_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Params^[0])^ := TCustomPage.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomPage_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Params^[0])^.Free();
end;

procedure _LapeCustomTabControl_TabRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PCustomTabControl(Params^[0])^.TabRect(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_GetImageIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.GetImageIndex(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_CustomPage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.CustomPage(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_CanChangePageIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCustomTabControl(Params^[0])^.CanChangePageIndex();
end;

procedure _LapeCustomTabControl_TabToPageIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.TabToPageIndex(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_PageToTabIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.PageToTabIndex(PInteger(Params^[1])^);
end;

procedure _LapeCustomTabControl_OnChanging_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabChangingEvent(Result)^ := PCustomTabControl(Params^[0])^.OnChanging;
end;

procedure _LapeCustomTabControl_OnChanging_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^.OnChanging := PTabChangingEvent(Params^[1])^;
end;

procedure _LapeCustomTabControl_Page_Index_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.Page[PInteger(Params^[1])^];
end;

procedure _LapeCustomTabControl_PageCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.PageCount;
end;

procedure _LapeCustomTabControl_PageIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.PageIndex;
end;

procedure _LapeCustomTabControl_PageIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomTabControl_Pages_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PCustomTabControl(Params^[0])^.Pages;
end;

procedure _LapeCustomTabControl_Pages_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^.Pages := PStrings(Params^[1])^;
end;

procedure _LapeCustomTabControl_ShowTabs_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.ShowTabs;
end;

procedure _LapeCustomTabControl_ShowTabs_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^.ShowTabs := PBoolean(Params^[1])^;
end;

procedure _LapeCustomTabControl_TabPosition_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabPosition(Result)^ := PCustomTabControl(Params^[0])^.TabPosition;
end;

procedure _LapeCustomTabControl_TabPosition_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^.TabPosition := PTabPosition(Params^[1])^;
end;

procedure _LapeCustomTabControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^ := TCustomTabControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomTabControl_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Params^[0])^.Free();
end;

procedure _LapeTabSheet_TabIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTabSheet(Params^[0])^.TabIndex;
end;

procedure _LapeTabSheet_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Params^[0])^ := TTabSheet.Create(PComponent(Params^[1])^);
end;

procedure _LapeTabSheet_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Params^[0])^.Free();
end;

procedure _LapePageControl_SelectNextPage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^);
end;

procedure _LapePageControl_SelectNextPageEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapePageControl_AddTabSheet(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.AddTabSheet();
end;

procedure _LapePageControl_IndexOfPageAt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.IndexOfPageAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePageControl_IndexOfTabAt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.IndexOfTabAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePageControl_Pages_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.Pages[PInteger(Params^[1])^];
end;

procedure _LapePageControl_ActivePage_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.ActivePage;
end;

procedure _LapePageControl_ActivePage_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.ActivePage := PTabSheet(Params^[1])^;
end;

procedure _LapePageControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^ := TPageControl.Create(PComponent(Params^[1])^);
end;

procedure _LapePageControl_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.Free();
end;

procedure _LapeStatusPanel_StatusBar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Result)^ := PStatusPanel(Params^[0])^.StatusBar();
end;

procedure _LapeStatusPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAlignment(Result)^ := PStatusPanel(Params^[0])^.Alignment;
end;

procedure _LapeStatusPanel_Alignment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

procedure _LapeStatusPanel_Text_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStatusPanel(Params^[0])^.Text;
end;

procedure _LapeStatusPanel_Text_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Params^[0])^.Text := PCaption(Params^[1])^;
end;

procedure _LapeStatusPanel_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStatusPanel(Params^[0])^.Width;
end;

procedure _LapeStatusPanel_Width_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeStatusPanel_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Params^[0])^ := TStatusPanel.Create(PCollection(Params^[1])^);
end;

procedure _LapeStatusPanel_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Params^[0])^.Free();
end;

procedure _LapeStatusPanels_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanels(Params^[0])^ := TStatusPanels.Create(PStatusBar(Params^[1])^);
end;

procedure _LapeStatusPanels_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Add();
end;

procedure _LapeStatusPanels_Items_Index_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeStatusPanels_Items_Index_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^] := PStatusPanel(Params^[2])^;
end;

procedure _LapeStatusPanels_StatusBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Result)^ := PStatusPanels(Params^[0])^.StatusBar;
end;

procedure _LapeStatusPanels_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanels(Params^[0])^.Free();
end;

procedure _LapeStatusBar_GetPanelIndexAt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStatusBar(Params^[0])^.GetPanelIndexAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStatusBar_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PStatusBar(Params^[0])^.Canvas;
end;

procedure _LapeStatusBar_AutoHint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.AutoHint;
end;

procedure _LapeStatusBar_AutoHint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.AutoHint := PBoolean(Params^[1])^;
end;

procedure _LapeStatusBar_Panels_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanels(Result)^ := PStatusBar(Params^[0])^.Panels;
end;

procedure _LapeStatusBar_Panels_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.Panels := PStatusPanels(Params^[1])^;
end;

procedure _LapeStatusBar_SimpleText_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCaption(Result)^ := PStatusBar(Params^[0])^.SimpleText;
end;

procedure _LapeStatusBar_SimpleText_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.SimpleText := PCaption(Params^[1])^;
end;

procedure _LapeStatusBar_SimplePanel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SimplePanel;
end;

procedure _LapeStatusBar_SimplePanel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.SimplePanel := PBoolean(Params^[1])^;
end;

procedure _LapeStatusBar_SizeGrip_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGrip;
end;

procedure _LapeStatusBar_SizeGrip_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.SizeGrip := PBoolean(Params^[1])^;
end;

procedure _LapeStatusBar_OnHint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PStatusBar(Params^[0])^.OnHint;
end;

procedure _LapeStatusBar_OnHint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.OnHint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeStatusBar_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^ := TStatusBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeStatusBar_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Params^[0])^.Free();
end;

procedure ImportLCLComCtrls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(tmBottomRight, tmTopLeft, tmBoth)', 'TTickMark');
    addGlobalType('(tsNone, tsAuto, tsManual)', 'TTickStyle');
    addGlobalType('procedure(Sender: TObject; Index: Integer) of object', 'TCheckListClicked', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var AllowChange: Boolean) of object', 'TTabChangingEvent', FFI_DEFAULT_ABI);
    addGlobalType('(tpTop, tpBottom, tpLeft, tpRight)', 'TTabPosition');
    addGlobalType('(trHorizontal, trVertical)', 'TTrackBarOrientation');

    addClass('TCustomProgressBar', 'TWinControl');
    addGlobalFunc('procedure TCustomProgressBar.Init(AOwner: TComponent); override', @_LapeCustomProgressBar_Init);
    addGlobalFunc('procedure TCustomProgressBar.StepIt;', @_LapeCustomProgressBar_StepIt);
    addGlobalFunc('procedure TCustomProgressBar.StepBy(Delta: Integer);', @_LapeCustomProgressBar_StepBy);
    addClassVar('TCustomProgressBar', 'Max', 'Integer', @_LapeCustomProgressBar_Max_Read, @_LapeCustomProgressBar_Max_Write);
    addClassVar('TCustomProgressBar', 'Min', 'Integer', @_LapeCustomProgressBar_Min_Read, @_LapeCustomProgressBar_Min_Write);
    addClassVar('TCustomProgressBar', 'Position', 'Integer', @_LapeCustomProgressBar_Position_Read, @_LapeCustomProgressBar_Position_Write);
    addClassVar('TCustomProgressBar', 'Smooth', 'boolean', @_LapeCustomProgressBar_Smooth_Read, @_LapeCustomProgressBar_Smooth_Write);
    addClassVar('TCustomProgressBar', 'Step', 'Integer', @_LapeCustomProgressBar_Step_Read, @_LapeCustomProgressBar_Step_Write);
    addClassVar('TCustomProgressBar', 'BarShowText', 'boolean', @_LapeCustomProgressBar_BarShowText_Read, @_LapeCustomProgressBar_BarShowText_Write);

    addClass('TProgressBar', 'TCustomProgressBar');
    addGlobalFunc('procedure TProgressBar.Init(AOwner: TComponent); override', @_LapeProgressBar_Init);

    addClass('TCustomTrackBar', 'TWinControl');
    addGlobalFunc('procedure TCustomTrackBar.Init(AOwner: TComponent); override', @_LapeCustomTrackBar_Init);
    addGlobalFunc('procedure TCustomTrackBar.SetTick(Value: Integer);', @_LapeCustomTrackBar_SetTick);
    addClassVar('TCustomTrackBar', 'Frequency', 'Integer', @_LapeCustomTrackBar_Frequency_Read, @_LapeCustomTrackBar_Frequency_Write);
    addClassVar('TCustomTrackBar', 'LineSize', 'Integer', @_LapeCustomTrackBar_LineSize_Read, @_LapeCustomTrackBar_LineSize_Write);
    addClassVar('TCustomTrackBar', 'Max', 'Integer', @_LapeCustomTrackBar_Max_Read, @_LapeCustomTrackBar_Max_Write);
    addClassVar('TCustomTrackBar', 'Min', 'Integer', @_LapeCustomTrackBar_Min_Read, @_LapeCustomTrackBar_Min_Write);
    addClassVar('TCustomTrackBar', 'OnChange', 'TNotifyEvent', @_LapeCustomTrackBar_OnChange_Read, @_LapeCustomTrackBar_OnChange_Write);
    addClassVar('TCustomTrackBar', 'Orientation', 'TTrackBarOrientation', @_LapeCustomTrackBar_Orientation_Read, @_LapeCustomTrackBar_Orientation_Write);
    addClassVar('TCustomTrackBar', 'PageSize', 'Integer', @_LapeCustomTrackBar_PageSize_Read, @_LapeCustomTrackBar_PageSize_Write);
    addClassVar('TCustomTrackBar', 'Position', 'Integer', @_LapeCustomTrackBar_Position_Read, @_LapeCustomTrackBar_Position_Write);
    addClassVar('TCustomTrackBar', 'Reversed', 'Boolean', @_LapeCustomTrackBar_Reversed_Read, @_LapeCustomTrackBar_Reversed_Write);
    addClassVar('TCustomTrackBar', 'SelEnd', 'Integer', @_LapeCustomTrackBar_SelEnd_Read, @_LapeCustomTrackBar_SelEnd_Write);
    addClassVar('TCustomTrackBar', 'SelStart', 'Integer', @_LapeCustomTrackBar_SelStart_Read, @_LapeCustomTrackBar_SelStart_Write);
    addClassVar('TCustomTrackBar', 'ShowSelRange', 'Boolean', @_LapeCustomTrackBar_ShowSelRange_Read, @_LapeCustomTrackBar_ShowSelRange_Write);
    addClassVar('TCustomTrackBar', 'TickMarks', 'TTickMark', @_LapeCustomTrackBar_TickMarks_Read, @_LapeCustomTrackBar_TickMarks_Write);
    addClassVar('TCustomTrackBar', 'TickStyle', 'TTickStyle', @_LapeCustomTrackBar_TickStyle_Read, @_LapeCustomTrackBar_TickStyle_Write);

    addClass('TTrackBar', 'TCustomTrackBar');
    addGlobalFunc('procedure TTrackBar.Init(AOwner: TComponent); override', @_LapeTrackBar_Init);

    addClass('TCustomCheckListBox', 'TCustomListBox');
    addGlobalFunc('procedure TCustomCheckListBox.Toggle(AIndex: Integer);', @_LapeCustomCheckListBox_Toggle);
    addGlobalFunc('procedure TCustomCheckListBox.CheckAll(AState: TCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);', @_LapeCustomCheckListBox_CheckAll);
    addClassVar('TCustomCheckListBox', 'AllowGrayed', 'Boolean', @_LapeCustomCheckListBox_AllowGrayed_Read, @_LapeCustomCheckListBox_AllowGrayed_Write);
    addClassVar('TCustomCheckListBox', 'Checked', 'Boolean', @_LapeCustomCheckListBox_Checked_Read, @_LapeCustomCheckListBox_Checked_Write, True);
    addClassVar('TCustomCheckListBox', 'ItemEnabled', 'Boolean', @_LapeCustomCheckListBox_ItemEnabled_Read, @_LapeCustomCheckListBox_ItemEnabled_Write, True);
    addClassVar('TCustomCheckListBox', 'State', 'TCheckBoxState', @_LapeCustomCheckListBox_State_Read, @_LapeCustomCheckListBox_State_Write, True);
    addClassVar('TCustomCheckListBox', 'OnClickCheck', 'TNotifyEvent', @_LapeCustomCheckListBox_OnClickCheck_Read, @_LapeCustomCheckListBox_OnClickCheck_Write);
    addClassVar('TCustomCheckListBox', 'OnItemClick', 'TCheckListClicked', nil, @_LapeCustomCheckListBox_OnCheckListClicked_Write);
    addGlobalFunc('procedure TCustomCheckListBox.Init(AOwner: TComponent)', @_LapeCustomCheckListBox_Init);

    addClass('TCheckListBox', 'TCustomCheckListBox');
    addGlobalFunc('procedure TCheckListBox.Init(AOwner: TComponent); override', @_LapeCheckListBox_Init);

    addClass('TCustomPage', 'TWinControl');
    addGlobalFunc('procedure TCustomPage.Init(TheOwner: TComponent); override', @_LapeCustomPage_Init);
    addGlobalFunc('function TCustomPage.CanTab: boolean;', @_LapeCustomPage_CanTab);
    addGlobalFunc('function TCustomPage.VisibleIndex: integer;', @_LapeCustomPage_VisibleIndex);
    addClassVar('TCustomPage', 'PageIndex', 'Integer', @_LapeCustomPage_PageIndex_Read, @_LapeCustomPage_PageIndex_Write);
    addClassVar('TCustomPage', 'TabVisible', 'Boolean', @_LapeCustomPage_TabVisible_Read, @_LapeCustomPage_TabVisible_Write);
    addClassVar('TCustomPage', 'OnHide', 'TNotifyEvent', @_LapeCustomPage_OnHide_Read, @_LapeCustomPage_OnHide_Write);
    addClassVar('TCustomPage', 'OnShow', 'TNotifyEvent', @_LapeCustomPage_OnShow_Read, @_LapeCustomPage_OnShow_Write);

    addClass('TCustomTabControl', 'TWinControl');
    addGlobalFunc('function TCustomTabControl.TabRect(AIndex: Integer): TRect;', @_LapeCustomTabControl_TabRect);
    addGlobalFunc('function TCustomTabControl.GetImageIndex(ThePageIndex: Integer): Integer;', @_LapeCustomTabControl_GetImageIndex);
    addGlobalFunc('function TCustomTabControl.CustomPage(Index: integer): TCustomPage;', @_LapeCustomTabControl_CustomPage);
    addGlobalFunc('function TCustomTabControl.TabToPageIndex(AIndex: integer): integer;', @_LapeCustomTabControl_TabToPageIndex);
    addGlobalFunc('function TCustomTabControl.PageToTabIndex(AIndex: integer): integer;', @_LapeCustomTabControl_PageToTabIndex);
    addClassVar('TCustomTabControl', 'OnChanging', 'TTabChangingEvent', @_LapeCustomTabControl_OnChanging_Read, @_LapeCustomTabControl_OnChanging_Write);
    addClassVar('TCustomTabControl', 'Page', 'TCustomPage', @_LapeCustomTabControl_Page_Index_Read);
    addClassVar('TCustomTabControl', 'PageCount', 'integer', @_LapeCustomTabControl_PageCount_Read);
    addClassVar('TCustomTabControl', 'PageIndex', 'Integer', @_LapeCustomTabControl_PageIndex_Read, @_LapeCustomTabControl_PageIndex_Write);
    addClassVar('TCustomTabControl', 'Pages', 'TStrings', @_LapeCustomTabControl_Pages_Read, @_LapeCustomTabControl_Pages_Write);
    addClassVar('TCustomTabControl', 'ShowTabs', 'Boolean', @_LapeCustomTabControl_ShowTabs_Read, @_LapeCustomTabControl_ShowTabs_Write);
    addClassVar('TCustomTabControl', 'TabPosition', 'TTabPosition', @_LapeCustomTabControl_TabPosition_Read, @_LapeCustomTabControl_TabPosition_Write);
    addGlobalFunc('procedure TCustomTabControl.Init(TheOwner: TComponent); override', @_LapeCustomTabControl_Init);

    addClass('TTabSheet', 'TCustomPage');
    addClassVar('TTabSheet', 'TabIndex', 'Integer', @_LapeTabSheet_TabIndex_Read);
    addGlobalFunc('procedure TTabSheet.Init(TheOwner: TComponent); override', @_LapeTabSheet_Init);

    addClass('TPageControl', 'TCustomTabControl');
    addGlobalFunc('procedure TPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);', @_LapePageControl_SelectNextPageEx);
    addGlobalFunc('function TPageControl.AddTabSheet: TTabSheet;', @_LapePageControl_AddTabSheet);
    addGlobalFunc('function TPageControl.IndexOfPageAt(X, Y: Integer): Integer;', @_LapePageControl_IndexOfPageAt);
    addGlobalFunc('function TPageControl.IndexOfTabAt(X, Y: Integer): Integer;', @_LapePageControl_IndexOfTabAt);
    addClassVar('TPageControl', 'Pages', 'Integer', @_LapePageControl_Pages_Read);
    addClassVar('TPageControl', 'ActivePage', 'TTabSheet', @_LapePageControl_ActivePage_Read, @_LapePageControl_ActivePage_Write);
    addGlobalFunc('procedure TPageControl.Init(TheOwner: TComponent); override', @_LapePageControl_Init);

    addClass('TStatusBar', 'TWinControl');
    addClass('TStatusPanel');
    addGlobalFunc('function TStatusPanel.StatusBar: TStatusBar;', @_LapeStatusPanel_StatusBar);
    addClassVar('TStatusPanel', 'Alignment', 'TAlignment', @_LapeStatusPanel_Alignment_Read, @_LapeStatusPanel_Alignment_Write);
    addClassVar('TStatusPanel', 'Text', 'String', @_LapeStatusPanel_Text_Read, @_LapeStatusPanel_Text_Write);
    addClassVar('TStatusPanel', 'Width', 'Integer', @_LapeStatusPanel_Width_Read, @_LapeStatusPanel_Width_Write);

    addClass('TStatusPanels');
    addGlobalFunc('procedure TStatusPanels.Init(AStatusBar: TStatusBar)', @_LapeStatusPanels_Init);
    addGlobalFunc('function TStatusPanels.Add: TStatusPanel;', @_LapeStatusPanels_Add);
    addClassVar('TStatusPanels', 'Items', 'TStatusPanel', @_LapeStatusPanels_Items_Index_Read, @_LapeStatusPanels_Items_Index_Write, True);
    addClassVar('TStatusPanels', 'StatusBar', 'TStatusBar', @_LapeStatusPanels_StatusBar_Read);

    addGlobalFunc('function TStatusBar.GetPanelIndexAt(X, Y: Integer): Integer;', @_LapeStatusBar_GetPanelIndexAt);
    addClassVar('TStatusBar', 'Canvas', 'TCanvas', @_LapeStatusBar_Canvas_Read);
    addClassVar('TStatusBar', 'AutoHint', 'Boolean', @_LapeStatusBar_AutoHint_Read, @_LapeStatusBar_AutoHint_Write);
    addClassVar('TStatusBar', 'Panels', 'TStatusPanels', @_LapeStatusBar_Panels_Read, @_LapeStatusBar_Panels_Write);
    addClassVar('TStatusBar', 'SimpleText', 'String', @_LapeStatusBar_SimpleText_Read, @_LapeStatusBar_SimpleText_Write);
    addClassVar('TStatusBar', 'SimplePanel', 'Boolean', @_LapeStatusBar_SimplePanel_Read, @_LapeStatusBar_SimplePanel_Write);
    addClassVar('TStatusBar', 'SizeGrip', 'Boolean', @_LapeStatusBar_SizeGrip_Read, @_LapeStatusBar_SizeGrip_Write);
    addClassVar('TStatusBar', 'OnHint', 'TNotifyEvent', @_LapeStatusBar_OnHint_Read, @_LapeStatusBar_OnHint_Write);
    addGlobalFunc('procedure TStatusBar.Init(TheOwner: TComponent); override', @_LapeStatusBar_Init);
  end;
end;

end.

