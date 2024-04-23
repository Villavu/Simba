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
  PAlignment = ^TAlignment;
  PCanvas = ^TCanvas;
  PNotifyEvent = ^TNotifyEvent;
  PComponent = ^TComponent;
  PCaption = ^TCaption;
  PCheckBoxState = ^TCheckBoxState;
  PCheckListBox = ^TCheckListBox;
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

procedure _LapeProgressBar_StepIt(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.StepIt();
end;

procedure _LapeProgressBar_StepBy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.StepBy(PInteger(Params^[1])^);
end;

procedure _LapeProgressBar_Max_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProgressBar(Params^[0])^.Max;
end;

procedure _LapeProgressBar_Max_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeProgressBar_Min_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProgressBar(Params^[0])^.Min;
end;

procedure _LapeProgressBar_Min_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeProgressBar_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProgressBar(Params^[0])^.Position;
end;

procedure _LapeProgressBar_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeProgressBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProgressBar(Params^[0])^.Smooth;
end;

procedure _LapeProgressBar_Smooth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.Smooth := PBoolean(Params^[1])^;
end;

procedure _LapeProgressBar_Step_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProgressBar(Params^[0])^.Step;
end;

procedure _LapeProgressBar_Step_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.Step := PInteger(Params^[1])^;
end;

procedure _LapeProgressBar_BarShowText_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProgressBar(Params^[0])^.BarShowText;
end;

procedure _LapeProgressBar_BarShowText_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Params^[0])^.BarShowText := PBoolean(Params^[1])^;
end;

procedure _LapeProgressBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProgressBar(Result)^ := TProgressBar.Create(PComponent(Params^[0])^);
end;

procedure _LapeTrackBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Result)^ := TTrackBar.Create(PComponent(Params^[0])^);
end;

procedure _LapeTrackBar_SetTick(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.SetTick(PInteger(Params^[1])^);
end;

procedure _LapeTrackBar_Frequency_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.Frequency;
end;

procedure _LapeTrackBar_Frequency_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Frequency := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_LineSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.LineSize;
end;

procedure _LapeTrackBar_LineSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.LineSize := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_Max_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.Max;
end;

procedure _LapeTrackBar_Max_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_Min_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.Min;
end;

procedure _LapeTrackBar_Min_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTrackBar(Params^[0])^.OnChange;
end;

procedure _LapeTrackBar_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeTrackBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBarOrientation(Result)^ := PTrackBar(Params^[0])^.Orientation;
end;

procedure _LapeTrackBar_Orientation_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Orientation := PTrackBarOrientation(Params^[1])^;
end;

procedure _LapeTrackBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.PageSize;
end;

procedure _LapeTrackBar_PageSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.Position;
end;

procedure _LapeTrackBar_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_Reversed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTrackBar(Params^[0])^.Reversed;
end;

procedure _LapeTrackBar_Reversed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.Reversed := PBoolean(Params^[1])^;
end;

procedure _LapeTrackBar_SelEnd_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.SelEnd;
end;

procedure _LapeTrackBar_SelEnd_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.SelEnd := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_SelStart_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTrackBar(Params^[0])^.SelStart;
end;

procedure _LapeTrackBar_SelStart_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.SelStart := PInteger(Params^[1])^;
end;

procedure _LapeTrackBar_ShowSelRange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTrackBar(Params^[0])^.ShowSelRange;
end;

procedure _LapeTrackBar_ShowSelRange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.ShowSelRange := PBoolean(Params^[1])^;
end;

procedure _LapeTrackBar_TickMarks_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTickMark(Result)^ := PTrackBar(Params^[0])^.TickMarks;
end;

procedure _LapeTrackBar_TickMarks_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.TickMarks := PTickMark(Params^[1])^;
end;

procedure _LapeTrackBar_TickStyle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTickStyle(Result)^ := PTrackBar(Params^[0])^.TickStyle;
end;

procedure _LapeTrackBar_TickStyle_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTrackBar(Params^[0])^.TickStyle := PTickStyle(Params^[1])^;
end;

procedure _LapeCheckListBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Result)^ := TCheckListBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCheckListBox_MeasureItem(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCheckListBox_Toggle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.Toggle(PInteger(Params^[1])^);
end;

procedure _LapeCheckListBox_CheckAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.CheckAll(PCheckBoxState(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeCheckListBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCheckListBox(Params^[0])^.AllowGrayed;
end;

procedure _LapeCheckListBox_AllowGrayed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

procedure _LapeCheckListBox_Checked_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^];
end;

procedure _LapeCheckListBox_Checked_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

procedure _LapeCheckListBox_ItemEnabled_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^];
end;

procedure _LapeCheckListBox_ItemEnabled_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

procedure _LapeCheckListBox_State_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckBoxState(Result)^ := PCheckListBox(Params^[0])^.State[PInteger(Params^[1])^];
end;

procedure _LapeCheckListBox_State_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.State[PInteger(Params^[1])^] := PCheckBoxState(Params^[1])^;
end;

procedure _LapeCheckListBox_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCheckListBox(Params^[0])^.Count;
end;

procedure _LapeCheckListBox_OnClickCheck_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCheckListBox(Params^[0])^.OnClickCheck;
end;

procedure _LapeCheckListBox_OnClickCheck_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckListBox(Params^[0])^.OnClickCheck := PNotifyEvent(Params^[1])^;
end;

procedure _LapePageControl_TabRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PPageControl(Params^[0])^.TabRect(PInteger(Params^[1])^);
end;

procedure _LapePageControl_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPageControl(Params^[0])^.OnChange;
end;

procedure _LapePageControl_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapePageControl_OnChanging_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabChangingEvent(Result)^ := PPageControl(Params^[0])^.OnChanging;
end;

procedure _LapePageControl_OnChanging_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.OnChanging := PTabChangingEvent(Params^[1])^;
end;

procedure _LapePageControl_Tab_Index_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.Pages[PInteger(Params^[1])^];
end;

procedure _LapePageControl_PageCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.PageCount;
end;

procedure _LapePageControl_ActiveTabIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.PageIndex;
end;

procedure _LapePageControl_ActiveTabIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

procedure _LapePageControl_ShowTabs_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPageControl(Params^[0])^.ShowTabs;
end;

procedure _LapePageControl_ShowTabs_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.ShowTabs := PBoolean(Params^[1])^;
end;

procedure _LapePageControl_TabPosition_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabPosition(Result)^ := PPageControl(Params^[0])^.TabPosition;
end;

procedure _LapePageControl_TabPosition_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.TabPosition := PTabPosition(Params^[1])^;
end;

procedure _LapePageControl_MultiLine_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPageControl(Params^[0])^.MultiLine;
end;

procedure _LapePageControl_MultiLine_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.MultiLine := PBoolean(Params^[1])^;
end;

procedure _LapePageControl_TabHeight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.TabHeight;
end;

procedure _LapePageControl_TabHeight_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.TabHeight := PInteger(Params^[1])^;
end;

procedure _LapePageControl_TabWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.TabWidth;
end;

procedure _LapePageControl_TabWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.TabWidth := PInteger(Params^[1])^;
end;

procedure _LapePageControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Result)^ := TPageControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeTabSheet_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := TTabSheet.Create(PComponent(Params^[0])^);
end;

procedure _LapeTabSheet_TabIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTabSheet(Params^[0])^.TabIndex;
end;

procedure _LapeTabSheet_OnHide_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTabSheet(Params^[0])^.OnHide;
end;

procedure _LapeTabSheet_OnHide_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

procedure _LapeTabSheet_OnShow_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTabSheet(Params^[0])^.OnShow;
end;

procedure _LapeTabSheet_OnShow_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapePageControl_AddTabSheet(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.AddTabSheet();
end;

procedure _LapePageControl_IndexOfTabAt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PPageControl(Params^[0])^.IndexOfTabAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePageControl_Pages_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.Pages[PInteger(Params^[1])^];
end;

procedure _LapePageControl_ActiveTab_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.ActivePage;
end;

procedure _LapePageControl_ActiveTab_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPageControl(Params^[0])^.ActivePage := PTabSheet(Params^[1])^;
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
    addGlobalType('enum(BottomRight, TopLeft, Both)', 'ELazTickMark');
    addGlobalType('enum(None, Auto, Manual)', 'ELazStickStyle');
    addGlobalType('enum(Top, Bottom, Left, Right)', 'ELazTabPosition');
    addGlobalType('enum(Horizontal, Vertical)', 'ELazTrackBarOrientation');

    addGlobalType('procedure(Sender: TObject; var AllowChange: Boolean) of object', 'TLazTabChangingEvent', FFI_DEFAULT_ABI);

    addClass('TLazProgressBar', 'TLazWinControl');
    addClassConstructor('TLazProgressBar', '(AOwner: TLazComponent)', @_LapeProgressBar_Create);
    addGlobalFunc('procedure TLazProgressBar.StepIt;', @_LapeProgressBar_StepIt);
    addGlobalFunc('procedure TLazProgressBar.StepBy(Delta: Integer);', @_LapeProgressBar_StepBy);
    addProperty('TLazProgressBar', 'Max', 'Integer', @_LapeProgressBar_Max_Read, @_LapeProgressBar_Max_Write);
    addProperty('TLazProgressBar', 'Min', 'Integer', @_LapeProgressBar_Min_Read, @_LapeProgressBar_Min_Write);
    addProperty('TLazProgressBar', 'Position', 'Integer', @_LapeProgressBar_Position_Read, @_LapeProgressBar_Position_Write);
    addProperty('TLazProgressBar', 'Smooth', 'Boolean', @_LapeProgressBar_Smooth_Read, @_LapeProgressBar_Smooth_Write);
    addProperty('TLazProgressBar', 'Step', 'Integer', @_LapeProgressBar_Step_Read, @_LapeProgressBar_Step_Write);
    addProperty('TLazProgressBar', 'BarShowText', 'Boolean', @_LapeProgressBar_BarShowText_Read, @_LapeProgressBar_BarShowText_Write);

    addClass('TLazTrackBar', 'TLazWinControl');
    addClassConstructor('TLazTrackBar', '(AOwner: TLazComponent)', @_LapeTrackBar_Create);
    addGlobalFunc('procedure TLazTrackBar.SetTick(Value: Integer);', @_LapeTrackBar_SetTick);
    addProperty('TLazTrackBar', 'Frequency', 'Integer', @_LapeTrackBar_Frequency_Read, @_LapeTrackBar_Frequency_Write);
    addProperty('TLazTrackBar', 'LineSize', 'Integer', @_LapeTrackBar_LineSize_Read, @_LapeTrackBar_LineSize_Write);
    addProperty('TLazTrackBar', 'Max', 'Integer', @_LapeTrackBar_Max_Read, @_LapeTrackBar_Max_Write);
    addProperty('TLazTrackBar', 'Min', 'Integer', @_LapeTrackBar_Min_Read, @_LapeTrackBar_Min_Write);
    addProperty('TLazTrackBar', 'OnChange', 'TLazNotifyEvent', @_LapeTrackBar_OnChange_Read, @_LapeTrackBar_OnChange_Write);
    addProperty('TLazTrackBar', 'Orientation', 'ELazTrackBarOrientation', @_LapeTrackBar_Orientation_Read, @_LapeTrackBar_Orientation_Write);
    addProperty('TLazTrackBar', 'PageSize', 'Integer', @_LapeTrackBar_PageSize_Read, @_LapeTrackBar_PageSize_Write);
    addProperty('TLazTrackBar', 'Position', 'Integer', @_LapeTrackBar_Position_Read, @_LapeTrackBar_Position_Write);
    addProperty('TLazTrackBar', 'Reversed', 'Boolean', @_LapeTrackBar_Reversed_Read, @_LapeTrackBar_Reversed_Write);
    addProperty('TLazTrackBar', 'SelEnd', 'Integer', @_LapeTrackBar_SelEnd_Read, @_LapeTrackBar_SelEnd_Write);
    addProperty('TLazTrackBar', 'SelStart', 'Integer', @_LapeTrackBar_SelStart_Read, @_LapeTrackBar_SelStart_Write);
    addProperty('TLazTrackBar', 'ShowSelRange', 'Boolean', @_LapeTrackBar_ShowSelRange_Read, @_LapeTrackBar_ShowSelRange_Write);
    addProperty('TLazTrackBar', 'TickMarks', 'ELazTickMark', @_LapeTrackBar_TickMarks_Read, @_LapeTrackBar_TickMarks_Write);
    addProperty('TLazTrackBar', 'TickStyle', 'ELazStickStyle', @_LapeTrackBar_TickStyle_Read, @_LapeTrackBar_TickStyle_Write);

    addClass('TLazCheckListBox', 'TLazCustomListBox');
    addClassConstructor('TLazCheckListBox', '(AOwner: TLazComponent)', @_LapeCheckListBox_Create);
    addGlobalFunc('procedure TLazCheckListBox.Toggle(AIndex: Integer);', @_LapeCheckListBox_Toggle);
    addGlobalFunc('procedure TLazCheckListBox.CheckAll(AState: ELazCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);', @_LapeCheckListBox_CheckAll);
    addProperty('TLazCheckListBox', 'AllowGrayed', 'Boolean', @_LapeCheckListBox_AllowGrayed_Read, @_LapeCheckListBox_AllowGrayed_Write);
    addPropertyIndexed('TLazCheckListBox', 'Checked', 'Index: Integer', 'Boolean', @_LapeCheckListBox_Checked_Read, @_LapeCheckListBox_Checked_Write);
    addPropertyIndexed('TLazCheckListBox', 'ItemEnabled', 'Index: Integer', 'Boolean', @_LapeCheckListBox_ItemEnabled_Read, @_LapeCheckListBox_ItemEnabled_Write);
    addPropertyIndexed('TLazCheckListBox', 'State', 'Index: Integer', 'ELazCheckBoxState', @_LapeCheckListBox_State_Read, @_LapeCheckListBox_State_Write);
    addProperty('TLazCheckListBox', 'OnClickCheck', 'TLazNotifyEvent', @_LapeCheckListBox_OnClickCheck_Read, @_LapeCheckListBox_OnClickCheck_Write);

    addClass('TLazTabSheet', 'TLazWinControl');
    addClassConstructor('TLazTabSheet', '(TheOwner: TLazComponent)', @_LapeTabSheet_Create);
    addProperty('TLazTabSheet', 'TabIndex', 'Integer', @_LapeTabSheet_TabIndex_Read);
    addProperty('TLazTabSheet', 'OnHide', 'TLazNotifyEvent', @_LapeTabSheet_OnHide_Read, @_LapeTabSheet_OnHide_Write);
    addProperty('TLazTabSheet', 'OnShow', 'TLazNotifyEvent', @_LapeTabSheet_OnShow_Read, @_LapeTabSheet_OnShow_Write);

    addClass('TLazPageControl', 'TLazWinControl');
    addClassConstructor('TLazPageControl', '(TheOwner: TLazComponent)', @_LapePageControl_Create);
    addGlobalFunc('function TLazPageControl.TabRect(AIndex: Integer): TLazRect;', @_LapePageControl_TabRect);
    addGlobalFunc('function TLazPageControl.AddTab: TLazTabSheet;', @_LapePageControl_AddTabSheet);
    addGlobalFunc('function TLazPageControl.IndexOfTabAt(X, Y: Integer): Integer;', @_LapePageControl_IndexOfTabAt);
    addProperty('TLazPageControl', 'OnChange', 'TLazNotifyEvent', @_LapePageControl_OnChange_Read, @_LapePageControl_OnChange_Write);
    addProperty('TLazPageControl', 'OnChanging', 'TLazTabChangingEvent', @_LapePageControl_OnChanging_Read, @_LapePageControl_OnChanging_Write);
    addProperty('TLazPageControl', 'ActiveTab', 'TLazTabSheet', @_LapePageControl_ActiveTab_Read, @_LapePageControl_ActiveTab_Write);
    addProperty('TLazPageControl', 'ActiveTabIndex', 'Integer', @_LapePageControl_ActiveTabIndex_Read, @_LapePageControl_ActiveTabIndex_Write);
    addPropertyIndexed('TLazPageControl', 'Tab', 'Index: Integer', 'TLazTabSheet', @_LapePageControl_Tab_Index_Read);
    addProperty('TLazPageControl', 'TabCount', 'Integer', @_LapePageControl_PageCount_Read);
    addProperty('TLazPageControl', 'ShowTabs', 'Boolean', @_LapePageControl_ShowTabs_Read, @_LapePageControl_ShowTabs_Write);
    addProperty('TLazPageControl', 'TabPosition', 'ELazTabPosition', @_LapePageControl_TabPosition_Read, @_LapePageControl_TabPosition_Write);
    addProperty('TLazPageControl', 'MultiLine', 'Boolean', @_LapePageControl_MultiLine_Read, @_LapePageControl_MultiLine_Write);
    addProperty('TLazPageControl', 'TabWidth', 'Integer', @_LapePageControl_TabWidth_Read, @_LapePageControl_TabWidth_Write);
    addProperty('TLazPageControl', 'TabHeight', 'Integer', @_LapePageControl_TabHeight_Read, @_LapePageControl_TabHeight_Write);

    addClass('TLazStatusBar', 'TLazWinControl');
    addClass('TLazStatusPanel');
    addGlobalFunc('function TLazStatusPanel.StatusBar: TLazStatusBar;', @_LapeStatusPanel_StatusBar);
    addProperty('TLazStatusPanel', 'Alignment', 'ELazAlignment', @_LapeStatusPanel_Alignment_Read, @_LapeStatusPanel_Alignment_Write);
    addProperty('TLazStatusPanel', 'Text', 'String', @_LapeStatusPanel_Text_Read, @_LapeStatusPanel_Text_Write);
    addProperty('TLazStatusPanel', 'Width', 'Integer', @_LapeStatusPanel_Width_Read, @_LapeStatusPanel_Width_Write);

    addClass('TLazStatusPanels');
    addClassConstructor('TLazStatusPanels', '(AStatusBar: TLazStatusBar)', @_LapeStatusPanels_Create);
    addGlobalFunc('function TLazStatusPanels.Add: TLazStatusPanel;', @_LapeStatusPanels_Add);
    addPropertyIndexed('TLazStatusPanels', 'Items', 'Index: Integer', 'TLazStatusPanel', @_LapeStatusPanels_Items_Index_Read, @_LapeStatusPanels_Items_Index_Write);
    addProperty('TLazStatusPanels', 'StatusBar', 'TLazStatusBar', @_LapeStatusPanels_StatusBar_Read);

    addGlobalFunc('function TLazStatusBar.GetPanelIndexAt(X, Y: Integer): Integer;', @_LapeStatusBar_GetPanelIndexAt);
    addProperty('TLazStatusBar', 'Canvas', 'TLazCanvas', @_LapeStatusBar_Canvas_Read);
    addProperty('TLazStatusBar', 'AutoHint', 'Boolean', @_LapeStatusBar_AutoHint_Read, @_LapeStatusBar_AutoHint_Write);
    addProperty('TLazStatusBar', 'Panels', 'TLazStatusPanels', @_LapeStatusBar_Panels_Read, @_LapeStatusBar_Panels_Write);
    addProperty('TLazStatusBar', 'SimpleText', 'String', @_LapeStatusBar_SimpleText_Read, @_LapeStatusBar_SimpleText_Write);
    addProperty('TLazStatusBar', 'SimplePanel', 'Boolean', @_LapeStatusBar_SimplePanel_Read, @_LapeStatusBar_SimplePanel_Write);
    addProperty('TLazStatusBar', 'SizeGrip', 'Boolean', @_LapeStatusBar_SizeGrip_Read, @_LapeStatusBar_SizeGrip_Write);
    addProperty('TLazStatusBar', 'OnHint', 'TLazNotifyEvent', @_LapeStatusBar_OnHint_Read, @_LapeStatusBar_OnHint_Write);
    addClassConstructor('TLazStatusBar', '(TheOwner: TLazComponent)', @_LapeStatusBar_Create);
  end;
end;

end.

