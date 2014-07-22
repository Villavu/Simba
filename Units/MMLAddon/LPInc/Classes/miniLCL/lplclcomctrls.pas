unit lplclcomctrls;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper, lplclstdctrls;

procedure RegisterLCLComCtrls(Compiler: TLapeCompiler);

implementation
  uses MufasaTypes,stdctrls,forms,lplclsystem,lplclgraphics,lplclcontrols, ComCtrls, CheckLst, Controls;

type
  PCustomProgressBar = ^TCustomProgressBar;
  PProgressBarStyle = ^TProgressBarStyle;
  PProgressBarOrientation = ^TProgressBarOrientation;

  PProgressBar = ^TProgressBar;

  PCustomTrackBar = ^TCustomTrackBar;
  PTrackBarOrientation = ^TTrackBarOrientation;
  PTickMark = ^TTickMark;
  PTickStyle = ^TTickStyle;
  PTrackBarScalePos = ^TTrackBarScalePos;

  PTrackBar = ^TTrackBar;

  PCustomCheckListBox = ^TCustomCheckListBox;
  PCheckBoxState = ^TCheckBoxState;
  PCheckListClicked  = ^TCheckListClicked;
  PCheckListBox = ^TCheckListBox;

  PCustomPage = ^TCustomPage;

  PCustomTabControl = ^TCustomTabControl;
  PTabChangingEvent = ^TTabChangingEvent;
  PCTabControlOption = ^TCTabControlOption;
  PTabPosition = ^TTabPosition;
  PTabStyle = ^TTabStyle;
  PCTabControlOptions = ^TCTabControlOptions;

  PPageControl = ^TPageControl;
  PTabSheet = ^TTabSheet;

  PStatusPanel = ^TStatusPanel;
  PStatusBar = ^TStatusBar;
  PAlignment = ^TAlignment;
  PStatusPanelBevel = ^TStatusPanelBevel;
  PStatusPanelStyle = ^TStatusPanelStyle;
  PStatusPanels = ^TStatusPanels;
  PPanelPart = ^TPanelPart;
  PPanelParts = ^TPanelParts;
  PCaption = ^TCaption;

procedure TCustomProgressBar_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^ := TCustomProgressBar.Create(PComponent(Params^[1])^);
end;

//procedure StepIt;
procedure TCustomProgressBar_StepIt(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.StepIt();
end;

//procedure StepBy(Delta: Integer);
procedure TCustomProgressBar_StepBy(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.StepBy(PInteger(Params^[1])^);
end;

//Read: property Max: Integer read GetMax write SetMax default 100;
procedure TCustomProgressBar_Max_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read GetMax write SetMax default 100;
procedure TCustomProgressBar_Max_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read GetMin write SetMin default 0;
procedure TCustomProgressBar_Min_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read GetMin write SetMin default 0;
procedure TCustomProgressBar_Min_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
procedure TCustomProgressBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PProgressBarOrientation(Result)^ := PCustomProgressBar(Params^[0])^.Orientation;
end;

//Write: property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
procedure TCustomProgressBar_Orientation_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Orientation := PProgressBarOrientation(Params^[1])^;
end;

//Read: property Position: Integer read GetPosition write SetPosition default 0;
procedure TCustomProgressBar_Position_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read GetPosition write SetPosition default 0;
procedure TCustomProgressBar_Position_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property Smooth : boolean read FSmooth write SetSmooth default False;
procedure TCustomProgressBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.Smooth;
end;

//Write: property Smooth : boolean read FSmooth write SetSmooth default False;
procedure TCustomProgressBar_Smooth_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Smooth := Pboolean(Params^[1])^;
end;

//Read: property Step: Integer read FStep write SetStep default 10;
procedure TCustomProgressBar_Step_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Step;
end;

//Write: property Step: Integer read FStep write SetStep default 10;
procedure TCustomProgressBar_Step_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Step := PInteger(Params^[1])^;
end;

//Read: property Style: TProgressBarStyle read FStyle write SetStyle default pbstNormal;
procedure TCustomProgressBar_Style_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PProgressBarStyle(Result)^ := PCustomProgressBar(Params^[0])^.Style;
end;

//Write: property Style: TProgressBarStyle read FStyle write SetStyle default pbstNormal;
procedure TCustomProgressBar_Style_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Style := PProgressBarStyle(Params^[1])^;
end;

//Read: property BarShowText : boolean read FBarShowText write SetBarShowText default False;
procedure TCustomProgressBar_BarShowText_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.BarShowText;
end;

//Write: property BarShowText : boolean read FBarShowText write SetBarShowText default False;
procedure TCustomProgressBar_BarShowText_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.BarShowText := Pboolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomProgressBar_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomProgressBar(Params^[0])^.Free();
end;

procedure Register_TCustomProgressBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomProgressBar', 'TWinControl');
    addGlobalFunc('procedure TCustomProgressBar.Init(AOwner: TComponent);', @TCustomProgressBar_Init);
    addGlobalFunc('procedure TCustomProgressBar.StepIt();', @TCustomProgressBar_StepIt);
    addGlobalFunc('procedure TCustomProgressBar.StepBy(Delta: Integer);', @TCustomProgressBar_StepBy);
    addClassVar('TCustomProgressBar', 'Max', 'Integer', @TCustomProgressBar_Max_Read, @TCustomProgressBar_Max_Write);
    addClassVar('TCustomProgressBar', 'Min', 'Integer', @TCustomProgressBar_Min_Read, @TCustomProgressBar_Min_Write);
    addClassVar('TCustomProgressBar', 'Orientation', 'TProgressBarOrientation', @TCustomProgressBar_Orientation_Read, @TCustomProgressBar_Orientation_Write);
    addClassVar('TCustomProgressBar', 'Position', 'Integer', @TCustomProgressBar_Position_Read, @TCustomProgressBar_Position_Write);
    addClassVar('TCustomProgressBar', 'Smooth', 'boolean', @TCustomProgressBar_Smooth_Read, @TCustomProgressBar_Smooth_Write);
    addClassVar('TCustomProgressBar', 'Step', 'Integer', @TCustomProgressBar_Step_Read, @TCustomProgressBar_Step_Write);
    addClassVar('TCustomProgressBar', 'Style', 'TProgressBarStyle', @TCustomProgressBar_Style_Read, @TCustomProgressBar_Style_Write);
    addClassVar('TCustomProgressBar', 'BarShowText', 'boolean', @TCustomProgressBar_BarShowText_Read, @TCustomProgressBar_BarShowText_Write);
    addGlobalFunc('procedure TCustomProgressBar.Free();', @TCustomProgressBar_Free);
  end;
end;

//constructor Create();
procedure TProgressBar_Init(const Params: PParamArray); lape_extdecl
begin
  PProgressBar(Params^[0])^ := TProgressBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TProgressBar_Free(const Params: PParamArray); lape_extdecl
begin
  PProgressBar(Params^[0])^.Free();
end;

procedure Register_TProgressBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TProgressBar', 'TCustomProgressBar');

    addGlobalFunc('procedure TProgressBar.Init(AOwner: TComponent);', @TProgressBar_Init);
    addGlobalFunc('procedure TProgressBar.Free();', @TProgressBar_Free);
  end;
end;

//constructor Create();
procedure TCustomTrackBar_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^ := TCustomTrackBar.Create(PComponent(Params^[1])^);
end;

//procedure SetTick(Value: Integer);
procedure TCustomTrackBar_SetTick(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.SetTick(PInteger(Params^[1])^);
end;

//Read: property Frequency: Integer read FFrequency write SetFrequency default 1;
procedure TCustomTrackBar_Frequency_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Frequency;
end;

//Write: property Frequency: Integer read FFrequency write SetFrequency default 1;
procedure TCustomTrackBar_Frequency_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Frequency := PInteger(Params^[1])^;
end;

//Read: property LineSize: Integer read FLineSize write SetLineSize default 1;
procedure TCustomTrackBar_LineSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.LineSize;
end;

//Write: property LineSize: Integer read FLineSize write SetLineSize default 1;
procedure TCustomTrackBar_LineSize_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.LineSize := PInteger(Params^[1])^;
end;

//Read: property Max: Integer read FMax write SetMax default 10;
procedure TCustomTrackBar_Max_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read FMax write SetMax default 10;
procedure TCustomTrackBar_Max_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read FMin write SetMin default 0;
procedure TCustomTrackBar_Min_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read FMin write SetMin default 0;
procedure TCustomTrackBar_Min_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCustomTrackBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomTrackBar(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCustomTrackBar_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
procedure TCustomTrackBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTrackBarOrientation(Result)^ := PCustomTrackBar(Params^[0])^.Orientation;
end;

//Write: property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
procedure TCustomTrackBar_Orientation_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Orientation := PTrackBarOrientation(Params^[1])^;
end;

//Read: property PageSize: Integer read FPageSize write SetPageSize default 2;
procedure TCustomTrackBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.PageSize;
end;

//Write: property PageSize: Integer read FPageSize write SetPageSize default 2;
procedure TCustomTrackBar_PageSize_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

//Read: property Position: Integer read FPosition write SetPosition;
procedure TCustomTrackBar_Position_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read FPosition write SetPosition;
procedure TCustomTrackBar_Position_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property Reversed: Boolean read FReversed write SetReversed default False;
procedure TCustomTrackBar_Reversed_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.Reversed;
end;

//Write: property Reversed: Boolean read FReversed write SetReversed default False;
procedure TCustomTrackBar_Reversed_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Reversed := PBoolean(Params^[1])^;
end;

//Read: property ScalePos: TTrackBarScalePos read FScalePos write SetScalePos default trTop;
procedure TCustomTrackBar_ScalePos_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTrackBarScalePos(Result)^ := PCustomTrackBar(Params^[0])^.ScalePos;
end;

//Write: property ScalePos: TTrackBarScalePos read FScalePos write SetScalePos default trTop;
procedure TCustomTrackBar_ScalePos_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.ScalePos := PTrackBarScalePos(Params^[1])^;
end;

//Read: property SelEnd: Integer read FSelEnd write SetSelEnd default 0;
procedure TCustomTrackBar_SelEnd_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelEnd;
end;

//Write: property SelEnd: Integer read FSelEnd write SetSelEnd default 0;
procedure TCustomTrackBar_SelEnd_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.SelEnd := PInteger(Params^[1])^;
end;

//Read: property SelStart: Integer read FSelStart write SetSelStart default 0;
procedure TCustomTrackBar_SelStart_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelStart;
end;

//Write: property SelStart: Integer read FSelStart write SetSelStart default 0;
procedure TCustomTrackBar_SelStart_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.SelStart := PInteger(Params^[1])^;
end;

//Read: property ShowSelRange: Boolean read FShowSelRange write SetShowSelRange default True;
procedure TCustomTrackBar_ShowSelRange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.ShowSelRange;
end;

//Write: property ShowSelRange: Boolean read FShowSelRange write SetShowSelRange default True;
procedure TCustomTrackBar_ShowSelRange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.ShowSelRange := PBoolean(Params^[1])^;
end;

//Read: property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
procedure TCustomTrackBar_TickMarks_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTickMark(Result)^ := PCustomTrackBar(Params^[0])^.TickMarks;
end;

//Write: property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
procedure TCustomTrackBar_TickMarks_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.TickMarks := PTickMark(Params^[1])^;
end;

//Read: property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
procedure TCustomTrackBar_TickStyle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTickStyle(Result)^ := PCustomTrackBar(Params^[0])^.TickStyle;
end;

//Write: property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
procedure TCustomTrackBar_TickStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.TickStyle := PTickStyle(Params^[1])^;
end;

//procedure Free();
procedure TCustomTrackBar_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomTrackBar(Params^[0])^.Free();
end;

procedure Register_TCustomTrackBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomTrackBar', 'TWinControl');

    addGlobalFunc('procedure TCustomTrackBar.Init(AOwner: TComponent);', @TCustomTrackBar_Init);
    addGlobalFunc('procedure TCustomTrackBar.SetTick(Value: Integer);', @TCustomTrackBar_SetTick);
    addClassVar('TCustomTrackBar', 'Frequency', 'Integer', @TCustomTrackBar_Frequency_Read, @TCustomTrackBar_Frequency_Write);
    addClassVar('TCustomTrackBar', 'LineSize', 'Integer', @TCustomTrackBar_LineSize_Read, @TCustomTrackBar_LineSize_Write);
    addClassVar('TCustomTrackBar', 'Max', 'Integer', @TCustomTrackBar_Max_Read, @TCustomTrackBar_Max_Write);
    addClassVar('TCustomTrackBar', 'Min', 'Integer', @TCustomTrackBar_Min_Read, @TCustomTrackBar_Min_Write);
    addClassVar('TCustomTrackBar', 'OnChange', 'TNotifyEvent', @TCustomTrackBar_OnChange_Read, @TCustomTrackBar_OnChange_Write);
    addClassVar('TCustomTrackBar', 'Orientation', 'TTrackBarOrientation', @TCustomTrackBar_Orientation_Read, @TCustomTrackBar_Orientation_Write);
    addClassVar('TCustomTrackBar', 'PageSize', 'Integer', @TCustomTrackBar_PageSize_Read, @TCustomTrackBar_PageSize_Write);
    addClassVar('TCustomTrackBar', 'Position', 'Integer', @TCustomTrackBar_Position_Read, @TCustomTrackBar_Position_Write);
    addClassVar('TCustomTrackBar', 'Reversed', 'Boolean', @TCustomTrackBar_Reversed_Read, @TCustomTrackBar_Reversed_Write);
    addClassVar('TCustomTrackBar', 'ScalePos', 'TTrackBarScalePos', @TCustomTrackBar_ScalePos_Read, @TCustomTrackBar_ScalePos_Write);
    addClassVar('TCustomTrackBar', 'SelEnd', 'Integer', @TCustomTrackBar_SelEnd_Read, @TCustomTrackBar_SelEnd_Write);
    addClassVar('TCustomTrackBar', 'SelStart', 'Integer', @TCustomTrackBar_SelStart_Read, @TCustomTrackBar_SelStart_Write);
    addClassVar('TCustomTrackBar', 'ShowSelRange', 'Boolean', @TCustomTrackBar_ShowSelRange_Read, @TCustomTrackBar_ShowSelRange_Write);
    addClassVar('TCustomTrackBar', 'TickMarks', 'TTickMark', @TCustomTrackBar_TickMarks_Read, @TCustomTrackBar_TickMarks_Write);
    addClassVar('TCustomTrackBar', 'TickStyle', 'TTickStyle', @TCustomTrackBar_TickStyle_Read, @TCustomTrackBar_TickStyle_Write);
    addGlobalFunc('procedure TCustomTrackBar.Free();', @TCustomTrackBar_Free);
  end;
end;

//constructor Create();
procedure TTrackBar_Init(const Params: PParamArray); lape_extdecl
begin
  PTrackBar(Params^[0])^ := TTrackBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TTrackBar_Free(const Params: PParamArray); lape_extdecl
begin
  PTrackBar(Params^[0])^.Free();
end;

procedure Register_TTrackBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TTrackBar', 'TCustomTrackBar');

    addGlobalFunc('procedure TTrackBar.Init(AOwner: TComponent);', @TTrackBar_Init);
    addGlobalFunc('procedure TTrackBar.Free();', @TTrackBar_Free);
  end;
end;

//procedure MeasureItem(Index: Integer; var TheHeight: Integer); override;
procedure TCustomCheckListBox_MeasureItem(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure Toggle(AIndex: Integer);
procedure TCustomCheckListBox_Toggle(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.Toggle(PInteger(Params^[1])^);
end;

//procedure CheckAll(AState: TCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);
procedure TCustomCheckListBox_CheckAll(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.CheckAll(PCheckBoxState(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

//Read: property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
procedure TCustomCheckListBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.AllowGrayed;
end;

//Write: property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
procedure TCustomCheckListBox_AllowGrayed_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

//Read: property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
procedure TCustomCheckListBox_Checked_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^];
end;

//Write: property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
procedure TCustomCheckListBox_Checked_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

//Read: property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
procedure TCustomCheckListBox_ItemEnabled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^];
end;

//Write: property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
procedure TCustomCheckListBox_ItemEnabled_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

//Read: property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
procedure TCustomCheckListBox_State_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCheckBoxState(Result)^ := PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^];
end;

//Write: property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
procedure TCustomCheckListBox_State_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^] := PCheckBoxState(Params^[1])^;
end;

//Read: property Count: integer read GetCount;
procedure TCustomCheckListBox_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomCheckListBox(Params^[0])^.Count;
end;

//Read: property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
procedure TCustomCheckListBox_OnClickCheck_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomCheckListBox(Params^[0])^.OnClickCheck;
end;

//Write: property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
procedure TCustomCheckListBox_OnClickCheck_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.OnClickCheck := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure TCustomCheckListBox_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^ := TCustomCheckListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomCheckListBox_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckListBox(Params^[0])^.Free();
end;

procedure TCustomCheckListBox_OnCheckListClicked_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PCustomCheckListBox(Params^[0])^.FindComponent('CheckListClicked');
  if (not Assigned(Component)) then
  begin
    Component := TOnCheckListClicked.Create(PCustomCheckListBox(Params^[0])^);
    Component.Name := 'CheckListClicked';
  end;

  with TOnCheckListClicked(Component) do
  begin
    InternalMethod := PCheckListClickedWrapper(Params^[1])^;
    PCustomCheckListBox(Params^[0])^.OnItemClick := @CheckListClicked;
  end;
end;

procedure Register_TCustomCheckListBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomCheckListBox', 'TCustomListBox');

    addGlobalFunc('procedure TCustomCheckListBox.Toggle(AIndex: Integer);', @TCustomCheckListBox_Toggle);
    addGlobalFunc('procedure TCustomCheckListBox.CheckAll(AState: TCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);', @TCustomCheckListBox_CheckAll);
    addClassVar('TCustomCheckListBox', 'AllowGrayed', 'Boolean', @TCustomCheckListBox_AllowGrayed_Read, @TCustomCheckListBox_AllowGrayed_Write);
    addClassVar('TCustomCheckListBox', 'Checked', 'Boolean', @TCustomCheckListBox_Checked_Read, @TCustomCheckListBox_Checked_Write, true);
    addClassVar('TCustomCheckListBox', 'ItemEnabled', 'Boolean', @TCustomCheckListBox_ItemEnabled_Read, @TCustomCheckListBox_ItemEnabled_Write, true);
    addClassVar('TCustomCheckListBox', 'State', 'TCheckBoxState', @TCustomCheckListBox_State_Read, @TCustomCheckListBox_State_Write, true);
    addClassVar('TCustomCheckListBox', 'Count', 'integer', @TCustomCheckListBox_Count_Read, nil);
    addClassVar('TCustomCheckListBox', 'OnClickCheck', 'TNotifyEvent', @TCustomCheckListBox_OnClickCheck_Read, @TCustomCheckListBox_OnClickCheck_Write);
    addClassVar('TCustomCheckListBox', 'OnItemClick', 'TCheckListClicked', nil, @TCustomCheckListBox_OnCheckListClicked_Write);
    addGlobalFunc('procedure TCustomCheckListBox.Init(AOwner: TComponent);', @TCustomCheckListBox_Init);
    addGlobalFunc('procedure TCustomCheckListBox.Free();', @TCustomCheckListBox_Free);
  end;
end;

//constructor Create();
procedure TCheckListBox_Init(const Params: PParamArray); lape_extdecl
begin
  PCheckListBox(Params^[0])^ := TCheckListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCheckListBox_Free(const Params: PParamArray); lape_extdecl
begin
  PCheckListBox(Params^[0])^.Free();
end;

procedure Register_TCheckListBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCheckListBox', 'TCustomCheckListBox');

    addGlobalFunc('procedure TCheckListBox.Init(AOwner: TComponent);', @TCheckListBox_Init);
    addGlobalFunc('procedure TCheckListBox.Free();', @TCheckListBox_Free);
  end;
end;

{TCustomPage}

//function CanTab: boolean; override;
procedure TCustomPage_CanTab(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.CanTab();
end;

//function IsControlVisible: Boolean; override;
procedure TCustomPage_IsControlVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.IsControlVisible();
end;

//function HandleObjectShouldBeVisible: boolean; override;
procedure TCustomPage_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.HandleObjectShouldBeVisible();
end;

//function VisibleIndex: integer; virtual;
procedure TCustomPage_VisibleIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomPage(Params^[0])^.VisibleIndex();
end;

//Read: property PageIndex: Integer read GetPageIndex write SetPageIndex;
procedure TCustomPage_PageIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomPage(Params^[0])^.PageIndex;
end;

//Write: property PageIndex: Integer read GetPageIndex write SetPageIndex;
procedure TCustomPage_PageIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPage(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

//Read: property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
procedure TCustomPage_TabVisible_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.TabVisible;
end;

//Write: property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
procedure TCustomPage_TabVisible_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPage(Params^[0])^.TabVisible := PBoolean(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read FOnHide write FOnHide;
procedure TCustomPage_OnHide_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read FOnHide write FOnHide;
procedure TCustomPage_OnHide_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPage(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure TCustomPage_OnShow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure TCustomPage_OnShow_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomPage(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure TCustomPage_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomPage(Params^[0])^ := TCustomPage.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomPage_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomPage(Params^[0])^.Free();
end;

procedure Register_TCustomPage(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomPage', 'TWinControl');

    addGlobalFunc('procedure TCustomPage.Init(TheOwner: TComponent);', @TCustomPage_Init);
    addGlobalFunc('function TCustomPage.CanTab(): boolean;', @TCustomPage_CanTab);
    addGlobalFunc('function TCustomPage.IsControlVisible(): Boolean;', @TCustomPage_IsControlVisible);
    addGlobalFunc('function TCustomPage.HandleObjectShouldBeVisible(): boolean;', @TCustomPage_HandleObjectShouldBeVisible);
    addGlobalFunc('function TCustomPage.VisibleIndex(): integer;', @TCustomPage_VisibleIndex);
    addClassVar('TCustomPage', 'PageIndex', 'Integer', @TCustomPage_PageIndex_Read, @TCustomPage_PageIndex_Write);
    addClassVar('TCustomPage', 'TabVisible', 'Boolean', @TCustomPage_TabVisible_Read, @TCustomPage_TabVisible_Write);
    addClassVar('TCustomPage', 'OnHide', 'TNotifyEvent', @TCustomPage_OnHide_Read, @TCustomPage_OnHide_Write);
    addClassVar('TCustomPage', 'OnShow', 'TNotifyEvent', @TCustomPage_OnShow_Read, @TCustomPage_OnShow_Write);
    addGlobalFunc('procedure TCustomPage.Free();', @TCustomPage_Free);
  end;
end;

//function TabIndexAtClientPos(ClientPos: TPoint): integer;
procedure TCustomTabControl_TabIndexAtClientPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.TabIndexAtClientPos(PPoint(Params^[1])^);
end;

//function TabRect(AIndex: Integer): TRect;
procedure TCustomTabControl_TabRect(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PCustomTabControl(Params^[0])^.TabRect(PInteger(Params^[1])^);
end;

//function GetImageIndex(ThePageIndex: Integer): Integer; virtual;
procedure TCustomTabControl_GetImageIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.GetImageIndex(PInteger(Params^[1])^);
end;

//function IndexOf(APage: TPersistent): integer; virtual;
procedure TCustomTabControl_IndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.IndexOf(PPersistent(Params^[1])^);
end;

//function CustomPage(Index: integer): TCustomPage;
procedure TCustomTabControl_CustomPage(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.CustomPage(Pinteger(Params^[1])^);
end;

//function CanChangePageIndex: boolean; virtual;
procedure TCustomTabControl_CanChangePageIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomTabControl(Params^[0])^.CanChangePageIndex();
end;

//function GetMinimumTabWidth: integer; virtual;
procedure TCustomTabControl_GetMinimumTabWidth(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.GetMinimumTabWidth();
end;

//function GetMinimumTabHeight: integer; virtual;
procedure TCustomTabControl_GetMinimumTabHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.GetMinimumTabHeight();
end;

//function TabToPageIndex(AIndex: integer): integer;
procedure TCustomTabControl_TabToPageIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.TabToPageIndex(Pinteger(Params^[1])^);
end;

//function PageToTabIndex(AIndex: integer): integer;
procedure TCustomTabControl_PageToTabIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.PageToTabIndex(Pinteger(Params^[1])^);
end;

//function IndexOfTabAt(X, Y: Integer): Integer;
procedure TCustomTabControl_IndexOfTabAt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.IndexOfTabAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure DoCloseTabClicked(APage: TCustomPage); virtual;
procedure TCustomTabControl_DoCloseTabClicked(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.DoCloseTabClicked(PCustomPage(Params^[1])^);
end;

//Read: property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
procedure TCustomTabControl_MultiLine_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.MultiLine;
end;

//Write: property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
procedure TCustomTabControl_MultiLine_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.MultiLine := PBoolean(Params^[1])^;
end;

//Read: property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
procedure TCustomTabControl_OnChanging_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTabChangingEvent(Result)^ := PCustomTabControl(Params^[0])^.OnChanging;
end;

//Write: property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
procedure TCustomTabControl_OnChanging_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.OnChanging := PTabChangingEvent(Params^[1])^;
end;

//Read: property Options: TCTabControlOptions read FOptions write SetOptions default [];
procedure TCustomTabControl_Options_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCTabControlOptions(Result)^ := PCustomTabControl(Params^[0])^.Options;
end;

//Write: property Options: TCTabControlOptions read FOptions write SetOptions default [];
procedure TCustomTabControl_Options_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.Options := PCTabControlOptions(Params^[1])^;
end;

//Read: property Page[Index: Integer]: TCustomPage read GetPage;
procedure TCustomTabControl_Page_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.Page[PInteger(Params^[1])^];
end;

//Read: property PageCount: integer read GetPageCount;
procedure TCustomTabControl_PageCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.PageCount;
end;

//Read: property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
procedure TCustomTabControl_PageIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.PageIndex;
end;

//Write: property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
procedure TCustomTabControl_PageIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

//Read: property Pages: TStrings read FAccess write SetPages;
procedure TCustomTabControl_Pages_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PCustomTabControl(Params^[0])^.Pages;
end;

//Write: property Pages: TStrings read FAccess write SetPages;
procedure TCustomTabControl_Pages_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.Pages := PStrings(Params^[1])^;
end;

//Read: property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
procedure TCustomTabControl_ShowTabs_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.ShowTabs;
end;

//Write: property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
procedure TCustomTabControl_ShowTabs_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.ShowTabs := PBoolean(Params^[1])^;
end;

//Read: property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
procedure TCustomTabControl_TabPosition_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTabPosition(Result)^ := PCustomTabControl(Params^[0])^.TabPosition;
end;

//Write: property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
procedure TCustomTabControl_TabPosition_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.TabPosition := PTabPosition(Params^[1])^;
end;

//Read: property IsUnpaged: boolean read FUnPaged;
procedure TCustomTabControl_IsUnpaged_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomTabControl(Params^[0])^.IsUnpaged;
end;

//constructor Create();
procedure TCustomTabControl_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^ := TCustomTabControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomTabControl_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomTabControl(Params^[0])^.Free();
end;

procedure Register_TCustomTabControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomTabControl', 'TWinControl');

    addGlobalFunc('function TCustomTabControl.TabIndexAtClientPos(ClientPos: TPoint): integer;', @TCustomTabControl_TabIndexAtClientPos);
    addGlobalFunc('function TCustomTabControl.TabRect(AIndex: Integer): TRect;', @TCustomTabControl_TabRect);
    addGlobalFunc('function TCustomTabControl.GetImageIndex(ThePageIndex: Integer): Integer;', @TCustomTabControl_GetImageIndex);
    addGlobalFunc('function TCustomTabControl.IndexOf(APage: TPersistent): integer;', @TCustomTabControl_IndexOf);
    addGlobalFunc('function TCustomTabControl.CustomPage(Index: integer): TCustomPage;', @TCustomTabControl_CustomPage);
    addGlobalFunc('function TCustomTabControl.CanChangePageIndex(): boolean;', @TCustomTabControl_CanChangePageIndex);
    addGlobalFunc('function TCustomTabControl.GetMinimumTabWidth(): integer;', @TCustomTabControl_GetMinimumTabWidth);
    addGlobalFunc('function TCustomTabControl.GetMinimumTabHeight(): integer;', @TCustomTabControl_GetMinimumTabHeight);
    addGlobalFunc('function TCustomTabControl.TabToPageIndex(AIndex: integer): integer;', @TCustomTabControl_TabToPageIndex);
    addGlobalFunc('function TCustomTabControl.PageToTabIndex(AIndex: integer): integer;', @TCustomTabControl_PageToTabIndex);
    addGlobalFunc('function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;', @TCustomTabControl_IndexOfTabAt);
    addGlobalFunc('procedure TCustomTabControl.DoCloseTabClicked(APage: TCustomPage);', @TCustomTabControl_DoCloseTabClicked);
    addClassVar('TCustomTabControl', 'MultiLine', 'Boolean', @TCustomTabControl_MultiLine_Read, @TCustomTabControl_MultiLine_Write);
    addClassVar('TCustomTabControl', 'OnChanging', 'TTabChangingEvent', @TCustomTabControl_OnChanging_Read, @TCustomTabControl_OnChanging_Write);
    addClassVar('TCustomTabControl', 'Options', 'TCTabControlOptions', @TCustomTabControl_Options_Read, @TCustomTabControl_Options_Write);
    addClassVar('TCustomTabControl', 'Page', 'TCustomPage', @TCustomTabControl_Page_Index_Read, nil);
    addClassVar('TCustomTabControl', 'PageCount', 'integer', @TCustomTabControl_PageCount_Read, nil);
    addClassVar('TCustomTabControl', 'PageIndex', 'Integer', @TCustomTabControl_PageIndex_Read, @TCustomTabControl_PageIndex_Write);
    addClassVar('TCustomTabControl', 'Pages', 'TStrings', @TCustomTabControl_Pages_Read, @TCustomTabControl_Pages_Write);
    addClassVar('TCustomTabControl', 'ShowTabs', 'Boolean', @TCustomTabControl_ShowTabs_Read, @TCustomTabControl_ShowTabs_Write);
    addClassVar('TCustomTabControl', 'TabPosition', 'TTabPosition', @TCustomTabControl_TabPosition_Read, @TCustomTabControl_TabPosition_Write);
    addClassVar('TCustomTabControl', 'IsUnpaged', 'boolean', @TCustomTabControl_IsUnpaged_Read, nil);
    addGlobalFunc('procedure TCustomTabControl.Init(TheOwner: TComponent);', @TCustomTabControl_Init);
    addGlobalFunc('procedure TCustomTabControl.Free();', @TCustomTabControl_Free);
  end;
end;

//Read: property TabIndex: Integer read GetTabIndex;
procedure TTabSheet_TabIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PTabSheet(Params^[0])^.TabIndex;
end;

//constructor Create();
procedure TTabSheet_Init(const Params: PParamArray); lape_extdecl
begin
  PTabSheet(Params^[0])^ := TTabSheet.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TTabSheet_Free(const Params: PParamArray); lape_extdecl
begin
  PTabSheet(Params^[0])^.Free();
end;

procedure Register_TTabSheet(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TTabSheet', 'TCustomPage');
    addClassVar('TTabSheet', 'TabIndex', 'Integer', @TTabSheet_TabIndex_Read, nil);
    addGlobalFunc('procedure TTabSheet.Init(TheOwner: TComponent);', @TTabSheet_Init);
    addGlobalFunc('procedure TTabSheet.Free();', @TTabSheet_Free);
  end;
end;

//procedure SelectNextPage(GoForward: Boolean);
procedure TPageControl_SelectNextPage(const Params: PParamArray); lape_extdecl
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^);
end;

//procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);
procedure TPageControl_SelectNextPageEx(const Params: PParamArray); lape_extdecl
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^, PBoolean(Params^[2])^);
end;

//function AddTabSheet: TTabSheet;
procedure TPageControl_AddTabSheet(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.AddTabSheet();
end;

//Read: property Pages[Index: Integer]: TTabSheet read GetTabSheet;
procedure TPageControl_Pages_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.Pages[PInteger(Params^[1])^];
end;

//Read: property ActivePage: TTabSheet read GetActiveTabSheet write SetActiveTabSheet;
procedure TPageControl_ActivePage_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.ActivePage;
end;

//Write: property ActivePage: TTabSheet read GetActiveTabSheet write SetActiveTabSheet;
procedure TPageControl_ActivePage_Write(const Params: PParamArray); lape_extdecl
begin
  PPageControl(Params^[0])^.ActivePage := PTabSheet(Params^[1])^;
end;

//constructor Create();
procedure TPageControl_Init(const Params: PParamArray); lape_extdecl
begin
  PPageControl(Params^[0])^ := TPageControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TPageControl_Free(const Params: PParamArray); lape_extdecl
begin
  PPageControl(Params^[0])^.Free();
end;

procedure Register_TPageControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TPageControl', 'TCustomTabControl');
    addGlobalFunc('procedure TPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);', @TPageControl_SelectNextPageEx);
    addGlobalFunc('function TPageControl.AddTabSheet(): TTabSheet;', @TPageControl_AddTabSheet);
    addClassVar('TPageControl', 'Pages', 'Integer', @TPageControl_Pages_Read, nil);
    addClassVar('TPageControl', 'ActivePage', 'TTabSheet', @TPageControl_ActivePage_Read, @TPageControl_ActivePage_Write);
    addGlobalFunc('procedure TPageControl.Init(TheOwner: TComponent);', @TPageControl_Init);
    addGlobalFunc('procedure TPageControl.Free();', @TPageControl_Free);
  end;
end;

procedure Register_TStatusBar_Forward(Compiler: TLapeCompiler);
begin
  with Compiler do
    addClass('TStatusBar', 'TWinControl');
end;

//function StatusBar: TStatusBar;
procedure TStatusPanel_StatusBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusBar(Result)^ := PStatusPanel(Params^[0])^.StatusBar();
end;

//Read: property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
procedure TStatusPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlignment(Result)^ := PStatusPanel(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
procedure TStatusPanel_Alignment_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
procedure TStatusPanel_Bevel_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusPanelBevel(Result)^ := PStatusPanel(Params^[0])^.Bevel;
end;

//Write: property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
procedure TStatusPanel_Bevel_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^.Bevel := PStatusPanelBevel(Params^[1])^;
end;

//Read: property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
procedure TStatusPanel_Style_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusPanelStyle(Result)^ := PStatusPanel(Params^[0])^.Style;
end;

//Write: property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
procedure TStatusPanel_Style_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^.Style := PStatusPanelStyle(Params^[1])^;
end;

//Read: property Text: TCaption read FText write SetText;
procedure TStatusPanel_Text_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
 PLPString(Result)^ := PStatusPanel(Params^[0])^.Text;
end;

//Write: property Text: TCaption read FText write SetText;
procedure TStatusPanel_Text_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^.Text := PCaption(Params^[1])^;
end;

//Read: property Width: Integer read FWidth write SetWidth;
procedure TStatusPanel_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStatusPanel(Params^[0])^.Width;
end;

//Write: property Width: Integer read FWidth write SetWidth;
procedure TStatusPanel_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//constructor Create();
procedure TStatusPanel_Init(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^ := TStatusPanel.Create(PCollection(Params^[1])^);
end;

//procedure Free();
procedure TStatusPanel_Free(const Params: PParamArray); lape_extdecl
begin
  PStatusPanel(Params^[0])^.Free();
end;

procedure Register_TStatusPanel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStatusPanel', 'TCollectionItem');

    addGlobalFunc('function TStatusPanel.StatusBar(): TStatusBar;', @TStatusPanel_StatusBar);
    addClassVar('TStatusPanel', 'Alignment', 'TAlignment', @TStatusPanel_Alignment_Read, @TStatusPanel_Alignment_Write);
    addClassVar('TStatusPanel', 'Bevel', 'TStatusPanelBevel', @TStatusPanel_Bevel_Read, @TStatusPanel_Bevel_Write);
    addClassVar('TStatusPanel', 'Style', 'TStatusPanelStyle', @TStatusPanel_Style_Read, @TStatusPanel_Style_Write);
    addClassVar('TStatusPanel', 'Text', 'TCaption', @TStatusPanel_Text_Read, @TStatusPanel_Text_Write);
    addClassVar('TStatusPanel', 'Width', 'Integer', @TStatusPanel_Width_Read, @TStatusPanel_Width_Write);
    addGlobalFunc('procedure TStatusPanel.Init(ACollection: TCollection);', @TStatusPanel_Init);
    addGlobalFunc('procedure TStatusPanel.Free();', @TStatusPanel_Free);
  end;
end;

//constructor Create(AStatusBar: TStatusBar);
procedure TStatusPanels_Init(const Params: PParamArray); lape_extdecl
begin
  PStatusPanels(Params^[0])^ := TStatusPanels.Create(PStatusBar(Params^[1])^);
end;

//function Add: TStatusPanel;
procedure TStatusPanels_Add(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Add();
end;

//Read: property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
procedure TStatusPanels_Items_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Write: property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
procedure TStatusPanels_Items_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^] := PStatusPanel(Params^[2])^;
end;

//Read: property StatusBar: TStatusBar read FStatusBar;
procedure TStatusPanels_StatusBar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusBar(Result)^ := PStatusPanels(Params^[0])^.StatusBar;
end;

//procedure Free();
procedure TStatusPanels_Free(const Params: PParamArray); lape_extdecl
begin
  PStatusPanels(Params^[0])^.Free();
end;

procedure Register_TStatusPanels(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStatusPanels', 'TCollection');

    addGlobalFunc('procedure TStatusPanels.Init(AStatusBar: TStatusBar);', @TStatusPanels_Init);
    addGlobalFunc('function TStatusPanels.Add(): TStatusPanel;', @TStatusPanels_Add);
    addClassVar('TStatusPanels', 'Items', 'TStatusPanel', @TStatusPanels_Items_Index_Read, @TStatusPanels_Items_Index_Write, True); // array
    addClassVar('TStatusPanels', 'StatusBar', 'TStatusBar', @TStatusPanels_StatusBar_Read, nil);
    addGlobalFunc('procedure TStatusPanels.Free();', @TStatusPanels_Free);
  end;
end;

//procedure InvalidatePanel(PanelIndex: integer; PanelParts: TPanelParts); virtual;
procedure TStatusBar_InvalidatePanel(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.InvalidatePanel(Pinteger(Params^[1])^, PPanelParts(Params^[2])^);
end;

//procedure BeginUpdate;
procedure TStatusBar_BeginUpdate(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.BeginUpdate();
end;

//procedure EndUpdate;
procedure TStatusBar_EndUpdate(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.EndUpdate();
end;

//function GetPanelIndexAt(X, Y: Integer): Integer;
procedure TStatusBar_GetPanelIndexAt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStatusBar(Params^[0])^.GetPanelIndexAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function SizeGripEnabled: Boolean;
procedure TStatusBar_SizeGripEnabled(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGripEnabled();
end;

//function UpdatingStatusBar: boolean;
procedure TStatusBar_UpdatingStatusBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PStatusBar(Params^[0])^.UpdatingStatusBar();
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure TStatusBar_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PStatusBar(Params^[0])^.Canvas;
end;

//Read: property AutoHint: Boolean read FAutoHint write FAutoHint default false;
procedure TStatusBar_AutoHint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.AutoHint;
end;

//Write: property AutoHint: Boolean read FAutoHint write FAutoHint default false;
procedure TStatusBar_AutoHint_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.AutoHint := PBoolean(Params^[1])^;
end;

//Read: property Panels: TStatusPanels read FPanels write SetPanels;
procedure TStatusBar_Panels_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStatusPanels(Result)^ := PStatusBar(Params^[0])^.Panels;
end;

//Write: property Panels: TStatusPanels read FPanels write SetPanels;
procedure TStatusBar_Panels_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.Panels := PStatusPanels(Params^[1])^;
end;

//Read: property SimpleText: TCaption read FSimpleText write SetSimpleText;
procedure TStatusBar_SimpleText_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCaption(Result)^ := PStatusBar(Params^[0])^.SimpleText;
end;

//Write: property SimpleText: TCaption read FSimpleText write SetSimpleText;
procedure TStatusBar_SimpleText_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.SimpleText := PCaption(Params^[1])^;
end;

//Read: property SimplePanel: Boolean read FSimplePanel write SetSimplePanel default True;
procedure TStatusBar_SimplePanel_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SimplePanel;
end;

//Write: property SimplePanel: Boolean read FSimplePanel write SetSimplePanel default True;
procedure TStatusBar_SimplePanel_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.SimplePanel := PBoolean(Params^[1])^;
end;

//Read: property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
procedure TStatusBar_SizeGrip_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGrip;
end;

//Write: property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
procedure TStatusBar_SizeGrip_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.SizeGrip := PBoolean(Params^[1])^;
end;

//Read: property OnHint: TNotifyEvent read FOnHint write FOnHint;
procedure TStatusBar_OnHint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PStatusBar(Params^[0])^.OnHint;
end;

//Write: property OnHint: TNotifyEvent read FOnHint write FOnHint;
procedure TStatusBar_OnHint_Write(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.OnHint := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure TStatusBar_Init(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^ := TStatusBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TStatusBar_Free(const Params: PParamArray); lape_extdecl
begin
  PStatusBar(Params^[0])^.Free();
end;

procedure Register_TStatusBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addGlobalFunc('procedure TStatusBar.InvalidatePanel(PanelIndex: integer; PanelParts: TPanelParts);', @TStatusBar_InvalidatePanel);
    addGlobalFunc('procedure TStatusBar.BeginUpdate();', @TStatusBar_BeginUpdate);
    addGlobalFunc('procedure TStatusBar.EndUpdate();', @TStatusBar_EndUpdate);
    addGlobalFunc('function TStatusBar.GetPanelIndexAt(X, Y: Integer): Integer;', @TStatusBar_GetPanelIndexAt);
    addGlobalFunc('function TStatusBar.SizeGripEnabled(): Boolean;', @TStatusBar_SizeGripEnabled);
    addGlobalFunc('function TStatusBar.UpdatingStatusBar(): boolean;', @TStatusBar_UpdatingStatusBar);
    addClassVar('TStatusBar', 'Canvas', 'TCanvas', @TStatusBar_Canvas_Read, nil);
    addClassVar('TStatusBar', 'AutoHint', 'Boolean', @TStatusBar_AutoHint_Read, @TStatusBar_AutoHint_Write);
    addClassVar('TStatusBar', 'Panels', 'TStatusPanels', @TStatusBar_Panels_Read, @TStatusBar_Panels_Write);
    addClassVar('TStatusBar', 'SimpleText', 'TCaption', @TStatusBar_SimpleText_Read, @TStatusBar_SimpleText_Write);
    addClassVar('TStatusBar', 'SimplePanel', 'Boolean', @TStatusBar_SimplePanel_Read, @TStatusBar_SimplePanel_Write);
    addClassVar('TStatusBar', 'SizeGrip', 'Boolean', @TStatusBar_SizeGrip_Read, @TStatusBar_SizeGrip_Write);
    addClassVar('TStatusBar', 'OnHint', 'TNotifyEvent', @TStatusBar_OnHint_Read, @TStatusBar_OnHint_Write);
    addGlobalFunc('procedure TStatusBar.Init(TheOwner: TComponent);', @TStatusBar_Init);
    addGlobalFunc('procedure TStatusBar.Free();', @TStatusBar_Free);
  end;
end;

procedure RegisterLCLComCtrls(Compiler: TLapeCompiler);
begin
  with Compiler do
   begin
     addGlobalType('(pbHorizontal, pbVertical, pbRightToLeft, pbTopDown)','TProgressBarOrientation');
     addGlobalType('(pbstNormal, pbstMarquee)','TProgressBarStyle');
     addGlobalType('(trHorizontal, trVertical)', 'TTrackBarOrientation');
     addGlobalType('(tmBottomRight, tmTopLeft, tmBoth)', 'TTickMark');
     addGlobalType('(tsNone, tsAuto, tsManual)', 'TTickStyle');
     addGlobalType('(trLeft, trRight, trTop, trBottom)', 'TTrackBarScalePos');
     addGlobalType('procedure(Sender: TObject; Index: integer)', 'TCheckListClicked');
     addGlobalType('procedure(Sender: TObject; var AllowChange: Boolean)', 'TTabChangingEvent');
     addGlobalType('(tsTabs, tsButtons, tsFlatButtons)', 'TTabStyle');
     addGlobalType('(tpTop, tpBottom, tpLeft, tpRight)', 'TTabPosition');
     addGlobalType('(nboShowCloseButtons, nboMultiLine, nboHidePageListPopup, nboKeyboardTabSwitch, nboShowAddTabButton)', 'TCTabControlOption');
     addGlobalType('set of TCTabControlOption', 'TCTabControlOptions');
     addGlobalType('(ppText, ppBorder, ppWidth)', 'TPanelPart');
     addGlobalType('set of TPanelPart', 'TPanelParts');
     addGlobalType('(psText, psOwnerDraw)', 'TStatusPanelStyle');
     addGlobalType('(pbNone, pbLowered, pbRaised)', 'TStatusPanelBevel');
   end;

  Register_TCustomProgressBar(Compiler);
  Register_TProgressBar(Compiler);
  Register_TCustomTrackBar(Compiler);
  Register_TTrackBar(Compiler);
  Register_TCustomCheckListBox(Compiler);
  Register_TCheckListBox(Compiler);
  Register_TCustomPage(Compiler);
  Register_TCustomTabControl(Compiler);
  Register_TTabSheet(Compiler);
  Register_TPageControl(Compiler);
  Register_TStatusBar_Forward(Compiler);
  Register_TStatusPanel(Compiler);
  Register_TStatusPanels(Compiler);
  Register_TStatusBar(Compiler);
end;

end.
