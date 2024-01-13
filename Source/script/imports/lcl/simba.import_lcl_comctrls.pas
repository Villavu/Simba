unit simba.import_lcl_comctrls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

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

procedure _LapeCustomProgressBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomProgressBar(Result)^ := TCustomProgressBar.Create(PComponent(Params^[0])^);
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

procedure _LapeProgressBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Result)^ := TProgressBar.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomTrackBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTrackBar(Result)^ := TCustomTrackBar.Create(PComponent(Params^[0])^);
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

procedure _LapeTrackBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Result)^ := TTrackBar.Create(PComponent(Params^[0])^);
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

procedure _LapeCustomCheckListBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Result)^ := TCustomCheckListBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomCheckListBox_OnCheckListClicked_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckListBox(Params^[0])^.OnItemClick := PCheckListClicked(Params^[1])^;
end;

procedure _LapeCheckListBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Result)^ := TCheckListBox.Create(PComponent(Params^[0])^);
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

procedure _LapeCustomPage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPage(Result)^ := TCustomPage.Create(PComponent(Params^[0])^);
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

procedure _LapeCustomTabControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomTabControl(Result)^ := TCustomTabControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeTabSheet_TabIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTabSheet(Params^[0])^.TabIndex;
end;

procedure _LapeTabSheet_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := TTabSheet.Create(PComponent(Params^[0])^);
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

procedure _LapePageControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Result)^ := TPageControl.Create(PComponent(Params^[0])^);
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

procedure _LapeStatusPanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanel(Result)^ := TStatusPanel.Create(PCollection(Params^[0])^);
end;

procedure _LapeStatusPanels_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusPanels(Result)^ := TStatusPanels.Create(PStatusBar(Params^[0])^);
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

procedure _LapeStatusBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStatusBar(Result)^ := TStatusBar.Create(PComponent(Params^[0])^);
end;

procedure ImportLCLComCtrls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(tmBottomRight, tmTopLeft, tmBoth)', 'TLazTickMark');
    addGlobalType('(tsNone, tsAuto, tsManual)', 'TLazStickStyle');
    addGlobalType('procedure(Sender: TObject; Index: Integer) of object', 'TLazCheckListClickedEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var AllowChange: Boolean) of object', 'TLazTabChangingEvent', FFI_DEFAULT_ABI);
    addGlobalType('(tpTop, tpBottom, tpLeft, tpRight)', 'TLazTabPosition');
    addGlobalType('(trHorizontal, trVertical)', 'TLazTrackBarOrientation');

    addClass('TLazCustomProgressBar', 'TLazWinControl');
    addClassConstructor('TLazCustomProgressBar', '(AOwner: TLazComponent)', @_LapeCustomProgressBar_Create);
    addGlobalFunc('procedure TLazCustomProgressBar.StepIt;', @_LapeCustomProgressBar_StepIt);
    addGlobalFunc('procedure TLazCustomProgressBar.StepBy(Delta: Integer);', @_LapeCustomProgressBar_StepBy);
    addClassVar('TLazCustomProgressBar', 'Max', 'Integer', @_LapeCustomProgressBar_Max_Read, @_LapeCustomProgressBar_Max_Write);
    addClassVar('TLazCustomProgressBar', 'Min', 'Integer', @_LapeCustomProgressBar_Min_Read, @_LapeCustomProgressBar_Min_Write);
    addClassVar('TLazCustomProgressBar', 'Position', 'Integer', @_LapeCustomProgressBar_Position_Read, @_LapeCustomProgressBar_Position_Write);
    addClassVar('TLazCustomProgressBar', 'Smooth', 'boolean', @_LapeCustomProgressBar_Smooth_Read, @_LapeCustomProgressBar_Smooth_Write);
    addClassVar('TLazCustomProgressBar', 'Step', 'Integer', @_LapeCustomProgressBar_Step_Read, @_LapeCustomProgressBar_Step_Write);
    addClassVar('TLazCustomProgressBar', 'BarShowText', 'boolean', @_LapeCustomProgressBar_BarShowText_Read, @_LapeCustomProgressBar_BarShowText_Write);

    addClass('TLazProgressBar', 'TLazCustomProgressBar');
    addClassConstructor('TLazProgressBar', '(AOwner: TLazComponent)', @_LapeProgressBar_Create);

    addClass('TLazCustomTrackBar', 'TLazWinControl');
    addClassConstructor('TLazCustomTrackBar', '(AOwner: TLazComponent)', @_LapeCustomTrackBar_Create);
    addGlobalFunc('procedure TLazCustomTrackBar.SetTick(Value: Integer);', @_LapeCustomTrackBar_SetTick);
    addClassVar('TLazCustomTrackBar', 'Frequency', 'Integer', @_LapeCustomTrackBar_Frequency_Read, @_LapeCustomTrackBar_Frequency_Write);
    addClassVar('TLazCustomTrackBar', 'LineSize', 'Integer', @_LapeCustomTrackBar_LineSize_Read, @_LapeCustomTrackBar_LineSize_Write);
    addClassVar('TLazCustomTrackBar', 'Max', 'Integer', @_LapeCustomTrackBar_Max_Read, @_LapeCustomTrackBar_Max_Write);
    addClassVar('TLazCustomTrackBar', 'Min', 'Integer', @_LapeCustomTrackBar_Min_Read, @_LapeCustomTrackBar_Min_Write);
    addClassVar('TLazCustomTrackBar', 'OnChange', 'TLazNotifyEvent', @_LapeCustomTrackBar_OnChange_Read, @_LapeCustomTrackBar_OnChange_Write);
    addClassVar('TLazCustomTrackBar', 'Orientation', 'TLazTrackBarOrientation', @_LapeCustomTrackBar_Orientation_Read, @_LapeCustomTrackBar_Orientation_Write);
    addClassVar('TLazCustomTrackBar', 'PageSize', 'Integer', @_LapeCustomTrackBar_PageSize_Read, @_LapeCustomTrackBar_PageSize_Write);
    addClassVar('TLazCustomTrackBar', 'Position', 'Integer', @_LapeCustomTrackBar_Position_Read, @_LapeCustomTrackBar_Position_Write);
    addClassVar('TLazCustomTrackBar', 'Reversed', 'Boolean', @_LapeCustomTrackBar_Reversed_Read, @_LapeCustomTrackBar_Reversed_Write);
    addClassVar('TLazCustomTrackBar', 'SelEnd', 'Integer', @_LapeCustomTrackBar_SelEnd_Read, @_LapeCustomTrackBar_SelEnd_Write);
    addClassVar('TLazCustomTrackBar', 'SelStart', 'Integer', @_LapeCustomTrackBar_SelStart_Read, @_LapeCustomTrackBar_SelStart_Write);
    addClassVar('TLazCustomTrackBar', 'ShowSelRange', 'Boolean', @_LapeCustomTrackBar_ShowSelRange_Read, @_LapeCustomTrackBar_ShowSelRange_Write);
    addClassVar('TLazCustomTrackBar', 'TickMarks', 'TLazTickMark', @_LapeCustomTrackBar_TickMarks_Read, @_LapeCustomTrackBar_TickMarks_Write);
    addClassVar('TLazCustomTrackBar', 'TickStyle', 'TLazStickStyle', @_LapeCustomTrackBar_TickStyle_Read, @_LapeCustomTrackBar_TickStyle_Write);

    addClass('TLazTrackBar', 'TLazCustomTrackBar');
    addClassConstructor('TLazTrackBar', '(AOwner: TLazComponent)', @_LapeTrackBar_Create);

    addClass('TLazCustomCheckListBox', 'TLazCustomListBox');
    addGlobalFunc('procedure TLazCustomCheckListBox.Toggle(AIndex: Integer);', @_LapeCustomCheckListBox_Toggle);
    addGlobalFunc('procedure TLazCustomCheckListBox.CheckAll(AState: TLazCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);', @_LapeCustomCheckListBox_CheckAll);
    addClassVar('TLazCustomCheckListBox', 'AllowGrayed', 'Boolean', @_LapeCustomCheckListBox_AllowGrayed_Read, @_LapeCustomCheckListBox_AllowGrayed_Write);
    addClassVar('TLazCustomCheckListBox', 'Checked', 'Boolean', @_LapeCustomCheckListBox_Checked_Read, @_LapeCustomCheckListBox_Checked_Write, True);
    addClassVar('TLazCustomCheckListBox', 'ItemEnabled', 'Boolean', @_LapeCustomCheckListBox_ItemEnabled_Read, @_LapeCustomCheckListBox_ItemEnabled_Write, True);
    addClassVar('TLazCustomCheckListBox', 'State', 'TLazCheckBoxState', @_LapeCustomCheckListBox_State_Read, @_LapeCustomCheckListBox_State_Write, True);
    addClassVar('TLazCustomCheckListBox', 'OnClickCheck', 'TLazNotifyEvent', @_LapeCustomCheckListBox_OnClickCheck_Read, @_LapeCustomCheckListBox_OnClickCheck_Write);
    addClassVar('TLazCustomCheckListBox', 'OnItemClick', 'TLazCheckListClickedEvent', nil, @_LapeCustomCheckListBox_OnCheckListClicked_Write);
    addClassConstructor('TLazCustomCheckListBox', '(AOwner: TLazComponent)', @_LapeCustomCheckListBox_Create);

    addClass('TLazCheckListBox', 'TLazCustomCheckListBox');
    addClassConstructor('TLazCheckListBox', '(AOwner: TLazComponent)', @_LapeCheckListBox_Create);

    addClass('TLazCustomPage', 'TLazWinControl');
    addClassConstructor('TLazCustomPage', '(TheOwner: TLazComponent)', @_LapeCustomPage_Create);
    addGlobalFunc('function TLazCustomPage.CanTab: boolean;', @_LapeCustomPage_CanTab);
    addGlobalFunc('function TLazCustomPage.VisibleIndex: Integer;', @_LapeCustomPage_VisibleIndex);
    addClassVar('TLazCustomPage', 'PageIndex', 'Integer', @_LapeCustomPage_PageIndex_Read, @_LapeCustomPage_PageIndex_Write);
    addClassVar('TLazCustomPage', 'TabVisible', 'Boolean', @_LapeCustomPage_TabVisible_Read, @_LapeCustomPage_TabVisible_Write);
    addClassVar('TLazCustomPage', 'OnHide', 'TLazNotifyEvent', @_LapeCustomPage_OnHide_Read, @_LapeCustomPage_OnHide_Write);
    addClassVar('TLazCustomPage', 'OnShow', 'TLazNotifyEvent', @_LapeCustomPage_OnShow_Read, @_LapeCustomPage_OnShow_Write);

    addClass('TLazCustomTabControl', 'TLazWinControl');
    addGlobalFunc('function TLazCustomTabControl.TabRect(AIndex: Integer): TLazRect;', @_LapeCustomTabControl_TabRect);
    addGlobalFunc('function TLazCustomTabControl.GetImageIndex(ThePageIndex: Integer): Integer;', @_LapeCustomTabControl_GetImageIndex);
    addGlobalFunc('function TLazCustomTabControl.CustomPage(Index: Integer): TLazCustomPage;', @_LapeCustomTabControl_CustomPage);
    addGlobalFunc('function TLazCustomTabControl.TabToPageIndex(AIndex: Integer): Integer;', @_LapeCustomTabControl_TabToPageIndex);
    addGlobalFunc('function TLazCustomTabControl.PageToTabIndex(AIndex: Integer): Integer;', @_LapeCustomTabControl_PageToTabIndex);
    addClassVar('TLazCustomTabControl', 'OnChanging', 'TLazTabChangingEvent', @_LapeCustomTabControl_OnChanging_Read, @_LapeCustomTabControl_OnChanging_Write);
    addClassVar('TLazCustomTabControl', 'Page', 'TLazCustomPage', @_LapeCustomTabControl_Page_Index_Read);
    addClassVar('TLazCustomTabControl', 'PageCount', 'Integer', @_LapeCustomTabControl_PageCount_Read);
    addClassVar('TLazCustomTabControl', 'PageIndex', 'Integer', @_LapeCustomTabControl_PageIndex_Read, @_LapeCustomTabControl_PageIndex_Write);
    addClassVar('TLazCustomTabControl', 'Pages', 'TLazStrings', @_LapeCustomTabControl_Pages_Read, @_LapeCustomTabControl_Pages_Write);
    addClassVar('TLazCustomTabControl', 'ShowTabs', 'Boolean', @_LapeCustomTabControl_ShowTabs_Read, @_LapeCustomTabControl_ShowTabs_Write);
    addClassVar('TLazCustomTabControl', 'TabPosition', 'TLazTabPosition', @_LapeCustomTabControl_TabPosition_Read, @_LapeCustomTabControl_TabPosition_Write);
    addClassConstructor('TLazCustomTabControl', '(TheOwner: TLazComponent)', @_LapeCustomTabControl_Create);

    addClass('TLazTabSheet', 'TLazCustomPage');
    addClassVar('TLazTabSheet', 'TabIndex', 'Integer', @_LapeTabSheet_TabIndex_Read);
    addClassConstructor('TLazTabSheet', '(TheOwner: TLazComponent)', @_LapeTabSheet_Create);

    addClass('TLazPageControl', 'TLazCustomTabControl');
    addGlobalFunc('procedure TLazPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);', @_LapePageControl_SelectNextPageEx);
    addGlobalFunc('function TLazPageControl.AddTabSheet: TLazTabSheet;', @_LapePageControl_AddTabSheet);
    addGlobalFunc('function TLazPageControl.IndexOfPageAt(X, Y: Integer): Integer;', @_LapePageControl_IndexOfPageAt);
    addGlobalFunc('function TLazPageControl.IndexOfTabAt(X, Y: Integer): Integer;', @_LapePageControl_IndexOfTabAt);
    addClassVar('TLazPageControl', 'Pages', 'Integer', @_LapePageControl_Pages_Read);
    addClassVar('TLazPageControl', 'ActivePage', 'TLazTabSheet', @_LapePageControl_ActivePage_Read, @_LapePageControl_ActivePage_Write);
    addClassConstructor('TLazPageControl', '(TheOwner: TLazComponent)', @_LapePageControl_Create);

    addClass('TLazStatusBar', 'TLazWinControl');
    addClass('TLazStatusPanel');
    addGlobalFunc('function TLazStatusPanel.StatusBar: TLazStatusBar;', @_LapeStatusPanel_StatusBar);
    addClassVar('TLazStatusPanel', 'Alignment', 'TLazAlignment', @_LapeStatusPanel_Alignment_Read, @_LapeStatusPanel_Alignment_Write);
    addClassVar('TLazStatusPanel', 'Text', 'String', @_LapeStatusPanel_Text_Read, @_LapeStatusPanel_Text_Write);
    addClassVar('TLazStatusPanel', 'Width', 'Integer', @_LapeStatusPanel_Width_Read, @_LapeStatusPanel_Width_Write);

    addClass('TLazStatusPanels');
    addClassConstructor('TLazStatusPanels', '(AStatusBar: TLazStatusBar)', @_LapeStatusPanels_Create);
    addGlobalFunc('function TLazStatusPanels.Add: TLazStatusPanel;', @_LapeStatusPanels_Add);
    addClassVar('TLazStatusPanels', 'Items', 'TLazStatusPanel', @_LapeStatusPanels_Items_Index_Read, @_LapeStatusPanels_Items_Index_Write, True);
    addClassVar('TLazStatusPanels', 'StatusBar', 'TLazStatusBar', @_LapeStatusPanels_StatusBar_Read);

    addGlobalFunc('function TLazStatusBar.GetPanelIndexAt(X, Y: Integer): Integer;', @_LapeStatusBar_GetPanelIndexAt);
    addClassVar('TLazStatusBar', 'Canvas', 'TLazCanvas', @_LapeStatusBar_Canvas_Read);
    addClassVar('TLazStatusBar', 'AutoHint', 'Boolean', @_LapeStatusBar_AutoHint_Read, @_LapeStatusBar_AutoHint_Write);
    addClassVar('TLazStatusBar', 'Panels', 'TLazStatusPanels', @_LapeStatusBar_Panels_Read, @_LapeStatusBar_Panels_Write);
    addClassVar('TLazStatusBar', 'SimpleText', 'String', @_LapeStatusBar_SimpleText_Read, @_LapeStatusBar_SimpleText_Write);
    addClassVar('TLazStatusBar', 'SimplePanel', 'Boolean', @_LapeStatusBar_SimplePanel_Read, @_LapeStatusBar_SimplePanel_Write);
    addClassVar('TLazStatusBar', 'SizeGrip', 'Boolean', @_LapeStatusBar_SizeGrip_Read, @_LapeStatusBar_SizeGrip_Write);
    addClassVar('TLazStatusBar', 'OnHint', 'TLazNotifyEvent', @_LapeStatusBar_OnHint_Read, @_LapeStatusBar_OnHint_Write);
    addClassConstructor('TLazStatusBar', '(TheOwner: TLazComponent)', @_LapeStatusBar_Create);
  end;
end;

end.

