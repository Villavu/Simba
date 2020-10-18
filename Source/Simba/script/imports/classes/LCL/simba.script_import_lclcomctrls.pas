unit simba.script_import_lclcomctrls;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLComCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  stdctrls, forms, comctrls, checklst, controls, graphics;

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
  PComponent = ^TComponent;
  PNotifyEvent = ^TNotifyEvent;
  PPen = ^TPen;
  PPersistent = ^TPersistent;
  PRect = ^TRect;
  PCollection = ^TCollection;

procedure Lape_TCustomProgressBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^ := TCustomProgressBar.Create(PComponent(Params^[1])^);
end;

//procedure StepIt;
procedure Lape_TCustomProgressBar_StepIt(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.StepIt();
end;

//procedure StepBy(Delta: Integer);
procedure Lape_TCustomProgressBar_StepBy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.StepBy(PInteger(Params^[1])^);
end;

//Read: property Max: Integer read GetMax write SetMax default 100;
procedure Lape_TCustomProgressBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read GetMax write SetMax default 100;
procedure Lape_TCustomProgressBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read GetMin write SetMin default 0;
procedure Lape_TCustomProgressBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read GetMin write SetMin default 0;
procedure Lape_TCustomProgressBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
procedure Lape_TCustomProgressBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProgressBarOrientation(Result)^ := PCustomProgressBar(Params^[0])^.Orientation;
end;

//Write: property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
procedure Lape_TCustomProgressBar_Orientation_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Orientation := PProgressBarOrientation(Params^[1])^;
end;

//Read: property Position: Integer read GetPosition write SetPosition default 0;
procedure Lape_TCustomProgressBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read GetPosition write SetPosition default 0;
procedure Lape_TCustomProgressBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property Smooth : boolean read FSmooth write SetSmooth default False;
procedure Lape_TCustomProgressBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.Smooth;
end;

//Write: property Smooth : boolean read FSmooth write SetSmooth default False;
procedure Lape_TCustomProgressBar_Smooth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Smooth := Pboolean(Params^[1])^;
end;

//Read: property Step: Integer read FStep write SetStep default 10;
procedure Lape_TCustomProgressBar_Step_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomProgressBar(Params^[0])^.Step;
end;

//Write: property Step: Integer read FStep write SetStep default 10;
procedure Lape_TCustomProgressBar_Step_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Step := PInteger(Params^[1])^;
end;

//Read: property Style: TProgressBarStyle read FStyle write SetStyle default pbstNormal;
procedure Lape_TCustomProgressBar_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProgressBarStyle(Result)^ := PCustomProgressBar(Params^[0])^.Style;
end;

//Write: property Style: TProgressBarStyle read FStyle write SetStyle default pbstNormal;
procedure Lape_TCustomProgressBar_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Style := PProgressBarStyle(Params^[1])^;
end;

//Read: property BarShowText : boolean read FBarShowText write SetBarShowText default False;
procedure Lape_TCustomProgressBar_BarShowText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomProgressBar(Params^[0])^.BarShowText;
end;

//Write: property BarShowText : boolean read FBarShowText write SetBarShowText default False;
procedure Lape_TCustomProgressBar_BarShowText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.BarShowText := Pboolean(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomProgressBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomProgressBar(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomProgressBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomProgressBar', 'TWinControl');
    addGlobalFunc('procedure TCustomProgressBar.Init(AOwner: TComponent); override;', @Lape_TCustomProgressBar_Init);
    addGlobalFunc('procedure TCustomProgressBar.StepIt(); constref;', @Lape_TCustomProgressBar_StepIt);
    addGlobalFunc('procedure TCustomProgressBar.StepBy(Delta: Integer); constref;', @Lape_TCustomProgressBar_StepBy);
    addClassVar('TCustomProgressBar', 'Max', 'Integer', @Lape_TCustomProgressBar_Max_Read, @Lape_TCustomProgressBar_Max_Write);
    addClassVar('TCustomProgressBar', 'Min', 'Integer', @Lape_TCustomProgressBar_Min_Read, @Lape_TCustomProgressBar_Min_Write);
    addClassVar('TCustomProgressBar', 'Orientation', 'TProgressBarOrientation', @Lape_TCustomProgressBar_Orientation_Read, @Lape_TCustomProgressBar_Orientation_Write);
    addClassVar('TCustomProgressBar', 'Position', 'Integer', @Lape_TCustomProgressBar_Position_Read, @Lape_TCustomProgressBar_Position_Write);
    addClassVar('TCustomProgressBar', 'Smooth', 'boolean', @Lape_TCustomProgressBar_Smooth_Read, @Lape_TCustomProgressBar_Smooth_Write);
    addClassVar('TCustomProgressBar', 'Step', 'Integer', @Lape_TCustomProgressBar_Step_Read, @Lape_TCustomProgressBar_Step_Write);
    addClassVar('TCustomProgressBar', 'Style', 'TProgressBarStyle', @Lape_TCustomProgressBar_Style_Read, @Lape_TCustomProgressBar_Style_Write);
    addClassVar('TCustomProgressBar', 'BarShowText', 'boolean', @Lape_TCustomProgressBar_BarShowText_Read, @Lape_TCustomProgressBar_BarShowText_Write);
    //addGlobalFunc('procedure TCustomProgressBar.Free(); constref;', @Lape_TCustomProgressBar_Free);
  end;
end;

//constructor Create();
procedure Lape_TProgressBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProgressBar(Params^[0])^ := TProgressBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TProgressBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProgressBar(Params^[0])^.Free();
end;

procedure Lape_Import_TProgressBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TProgressBar', 'TCustomProgressBar');

    addGlobalFunc('procedure TProgressBar.Init(AOwner: TComponent); override;', @Lape_TProgressBar_Init);
    //addGlobalFunc('procedure TProgressBar.Free(); constref;', @Lape_TProgressBar_Free);
  end;
end;

//constructor Create();
procedure Lape_TCustomTrackBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^ := TCustomTrackBar.Create(PComponent(Params^[1])^);
end;

//procedure SetTick(Value: Integer);
procedure Lape_TCustomTrackBar_SetTick(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.SetTick(PInteger(Params^[1])^);
end;

//Read: property Frequency: Integer read FFrequency write SetFrequency default 1;
procedure Lape_TCustomTrackBar_Frequency_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Frequency;
end;

//Write: property Frequency: Integer read FFrequency write SetFrequency default 1;
procedure Lape_TCustomTrackBar_Frequency_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Frequency := PInteger(Params^[1])^;
end;

//Read: property LineSize: Integer read FLineSize write SetLineSize default 1;
procedure Lape_TCustomTrackBar_LineSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.LineSize;
end;

//Write: property LineSize: Integer read FLineSize write SetLineSize default 1;
procedure Lape_TCustomTrackBar_LineSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.LineSize := PInteger(Params^[1])^;
end;

//Read: property Max: Integer read FMax write SetMax default 10;
procedure Lape_TCustomTrackBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read FMax write SetMax default 10;
procedure Lape_TCustomTrackBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read FMin write SetMin default 0;
procedure Lape_TCustomTrackBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read FMin write SetMin default 0;
procedure Lape_TCustomTrackBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure Lape_TCustomTrackBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomTrackBar(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure Lape_TCustomTrackBar_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
procedure Lape_TCustomTrackBar_Orientation_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTrackBarOrientation(Result)^ := PCustomTrackBar(Params^[0])^.Orientation;
end;

//Write: property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
procedure Lape_TCustomTrackBar_Orientation_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Orientation := PTrackBarOrientation(Params^[1])^;
end;

//Read: property PageSize: Integer read FPageSize write SetPageSize default 2;
procedure Lape_TCustomTrackBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.PageSize;
end;

//Write: property PageSize: Integer read FPageSize write SetPageSize default 2;
procedure Lape_TCustomTrackBar_PageSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

//Read: property Position: Integer read FPosition write SetPosition;
procedure Lape_TCustomTrackBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read FPosition write SetPosition;
procedure Lape_TCustomTrackBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property Reversed: Boolean read FReversed write SetReversed default False;
procedure Lape_TCustomTrackBar_Reversed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.Reversed;
end;

//Write: property Reversed: Boolean read FReversed write SetReversed default False;
procedure Lape_TCustomTrackBar_Reversed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Reversed := PBoolean(Params^[1])^;
end;

//Read: property ScalePos: TTrackBarScalePos read FScalePos write SetScalePos default trTop;
procedure Lape_TCustomTrackBar_ScalePos_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTrackBarScalePos(Result)^ := PCustomTrackBar(Params^[0])^.ScalePos;
end;

//Write: property ScalePos: TTrackBarScalePos read FScalePos write SetScalePos default trTop;
procedure Lape_TCustomTrackBar_ScalePos_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.ScalePos := PTrackBarScalePos(Params^[1])^;
end;

//Read: property SelEnd: Integer read FSelEnd write SetSelEnd default 0;
procedure Lape_TCustomTrackBar_SelEnd_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelEnd;
end;

//Write: property SelEnd: Integer read FSelEnd write SetSelEnd default 0;
procedure Lape_TCustomTrackBar_SelEnd_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.SelEnd := PInteger(Params^[1])^;
end;

//Read: property SelStart: Integer read FSelStart write SetSelStart default 0;
procedure Lape_TCustomTrackBar_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTrackBar(Params^[0])^.SelStart;
end;

//Write: property SelStart: Integer read FSelStart write SetSelStart default 0;
procedure Lape_TCustomTrackBar_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.SelStart := PInteger(Params^[1])^;
end;

//Read: property ShowSelRange: Boolean read FShowSelRange write SetShowSelRange default True;
procedure Lape_TCustomTrackBar_ShowSelRange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTrackBar(Params^[0])^.ShowSelRange;
end;

//Write: property ShowSelRange: Boolean read FShowSelRange write SetShowSelRange default True;
procedure Lape_TCustomTrackBar_ShowSelRange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.ShowSelRange := PBoolean(Params^[1])^;
end;

//Read: property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
procedure Lape_TCustomTrackBar_TickMarks_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTickMark(Result)^ := PCustomTrackBar(Params^[0])^.TickMarks;
end;

//Write: property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
procedure Lape_TCustomTrackBar_TickMarks_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.TickMarks := PTickMark(Params^[1])^;
end;

//Read: property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
procedure Lape_TCustomTrackBar_TickStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTickStyle(Result)^ := PCustomTrackBar(Params^[0])^.TickStyle;
end;

//Write: property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
procedure Lape_TCustomTrackBar_TickStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.TickStyle := PTickStyle(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomTrackBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTrackBar(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomTrackBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomTrackBar', 'TWinControl');

    addGlobalFunc('procedure TCustomTrackBar.Init(AOwner: TComponent); override;', @Lape_TCustomTrackBar_Init);
    addGlobalFunc('procedure TCustomTrackBar.SetTick(Value: Integer); constref;', @Lape_TCustomTrackBar_SetTick);
    addClassVar('TCustomTrackBar', 'Frequency', 'Integer', @Lape_TCustomTrackBar_Frequency_Read, @Lape_TCustomTrackBar_Frequency_Write);
    addClassVar('TCustomTrackBar', 'LineSize', 'Integer', @Lape_TCustomTrackBar_LineSize_Read, @Lape_TCustomTrackBar_LineSize_Write);
    addClassVar('TCustomTrackBar', 'Max', 'Integer', @Lape_TCustomTrackBar_Max_Read, @Lape_TCustomTrackBar_Max_Write);
    addClassVar('TCustomTrackBar', 'Min', 'Integer', @Lape_TCustomTrackBar_Min_Read, @Lape_TCustomTrackBar_Min_Write);
    addClassVar('TCustomTrackBar', 'OnChange', 'TNotifyEvent', @Lape_TCustomTrackBar_OnChange_Read, @Lape_TCustomTrackBar_OnChange_Write);
    addClassVar('TCustomTrackBar', 'Orientation', 'TTrackBarOrientation', @Lape_TCustomTrackBar_Orientation_Read, @Lape_TCustomTrackBar_Orientation_Write);
    addClassVar('TCustomTrackBar', 'PageSize', 'Integer', @Lape_TCustomTrackBar_PageSize_Read, @Lape_TCustomTrackBar_PageSize_Write);
    addClassVar('TCustomTrackBar', 'Position', 'Integer', @Lape_TCustomTrackBar_Position_Read, @Lape_TCustomTrackBar_Position_Write);
    addClassVar('TCustomTrackBar', 'Reversed', 'Boolean', @Lape_TCustomTrackBar_Reversed_Read, @Lape_TCustomTrackBar_Reversed_Write);
    addClassVar('TCustomTrackBar', 'ScalePos', 'TTrackBarScalePos', @Lape_TCustomTrackBar_ScalePos_Read, @Lape_TCustomTrackBar_ScalePos_Write);
    addClassVar('TCustomTrackBar', 'SelEnd', 'Integer', @Lape_TCustomTrackBar_SelEnd_Read, @Lape_TCustomTrackBar_SelEnd_Write);
    addClassVar('TCustomTrackBar', 'SelStart', 'Integer', @Lape_TCustomTrackBar_SelStart_Read, @Lape_TCustomTrackBar_SelStart_Write);
    addClassVar('TCustomTrackBar', 'ShowSelRange', 'Boolean', @Lape_TCustomTrackBar_ShowSelRange_Read, @Lape_TCustomTrackBar_ShowSelRange_Write);
    addClassVar('TCustomTrackBar', 'TickMarks', 'TTickMark', @Lape_TCustomTrackBar_TickMarks_Read, @Lape_TCustomTrackBar_TickMarks_Write);
    addClassVar('TCustomTrackBar', 'TickStyle', 'TTickStyle', @Lape_TCustomTrackBar_TickStyle_Read, @Lape_TCustomTrackBar_TickStyle_Write);
    //addGlobalFunc('procedure TCustomTrackBar.Free(); constref;', @Lape_TCustomTrackBar_Free);
  end;
end;

//constructor Create();
procedure Lape_TTrackBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTrackBar(Params^[0])^ := TTrackBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TTrackBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTrackBar(Params^[0])^.Free();
end;

procedure Lape_Import_TTrackBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TTrackBar', 'TCustomTrackBar');

    addGlobalFunc('procedure TTrackBar.Init(AOwner: TComponent); override;', @Lape_TTrackBar_Init);
    //addGlobalFunc('procedure TTrackBar.Free(); constref;', @Lape_TTrackBar_Free);
  end;
end;

//procedure MeasureItem(Index: Integer; var TheHeight: Integer); override;
procedure Lape_TCustomCheckListBox_MeasureItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure Toggle(AIndex: Integer);
procedure Lape_TCustomCheckListBox_Toggle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.Toggle(PInteger(Params^[1])^);
end;

//procedure CheckAll(AState: TCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True);
procedure Lape_TCustomCheckListBox_CheckAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.CheckAll(PCheckBoxState(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

//Read: property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
procedure Lape_TCustomCheckListBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.AllowGrayed;
end;

//Write: property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
procedure Lape_TCustomCheckListBox_AllowGrayed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

//Read: property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
procedure Lape_TCustomCheckListBox_Checked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^];
end;

//Write: property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
procedure Lape_TCustomCheckListBox_Checked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.Checked[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

//Read: property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
procedure Lape_TCustomCheckListBox_ItemEnabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^];
end;

//Write: property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
procedure Lape_TCustomCheckListBox_ItemEnabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.ItemEnabled[PInteger(Params^[1])^] := PBoolean(Params^[2])^;
end;

//Read: property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
procedure Lape_TCustomCheckListBox_State_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBoxState(Result)^ := PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^];
end;

//Write: property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
procedure Lape_TCustomCheckListBox_State_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.State[PInteger(Params^[1])^] := PCheckBoxState(Params^[1])^;
end;

//Read: property Count: integer read GetCount;
procedure Lape_TCustomCheckListBox_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomCheckListBox(Params^[0])^.Count;
end;

//Read: property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
procedure Lape_TCustomCheckListBox_OnClickCheck_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomCheckListBox(Params^[0])^.OnClickCheck;
end;

//Write: property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
procedure Lape_TCustomCheckListBox_OnClickCheck_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.OnClickCheck := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCustomCheckListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^ := TCustomCheckListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomCheckListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.Free();
end;

procedure Lape_TCustomCheckListBox_OnCheckListClicked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckListBox(Params^[0])^.OnItemClick := PCheckListClicked(Params^[1])^;
end;

procedure Lape_Import_TCustomCheckListBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomCheckListBox', 'TCustomListBox');

    addGlobalFunc('procedure TCustomCheckListBox.Toggle(AIndex: Integer); constref;', @Lape_TCustomCheckListBox_Toggle);
    addGlobalFunc('procedure TCustomCheckListBox.CheckAll(AState: TCheckBoxState; aAllowGrayed: Boolean = True; aAllowDisabled: Boolean = True); constref;', @Lape_TCustomCheckListBox_CheckAll);
    addClassVar('TCustomCheckListBox', 'AllowGrayed', 'Boolean', @Lape_TCustomCheckListBox_AllowGrayed_Read, @Lape_TCustomCheckListBox_AllowGrayed_Write);
    addClassVar('TCustomCheckListBox', 'Checked', 'Boolean', @Lape_TCustomCheckListBox_Checked_Read, @Lape_TCustomCheckListBox_Checked_Write, true);
    addClassVar('TCustomCheckListBox', 'ItemEnabled', 'Boolean', @Lape_TCustomCheckListBox_ItemEnabled_Read, @Lape_TCustomCheckListBox_ItemEnabled_Write, true);
    addClassVar('TCustomCheckListBox', 'State', 'TCheckBoxState', @Lape_TCustomCheckListBox_State_Read, @Lape_TCustomCheckListBox_State_Write, true);
    //addClassVar('TCustomCheckListBox', 'Count', 'integer', @Lape_TCustomCheckListBox_Count_Read, nil);
    addClassVar('TCustomCheckListBox', 'OnClickCheck', 'TNotifyEvent', @Lape_TCustomCheckListBox_OnClickCheck_Read, @Lape_TCustomCheckListBox_OnClickCheck_Write);
    addClassVar('TCustomCheckListBox', 'OnItemClick', 'TCheckListClicked', nil, @Lape_TCustomCheckListBox_OnCheckListClicked_Write);
    addGlobalFunc('procedure TCustomCheckListBox.Init(AOwner: TComponent);', @Lape_TCustomCheckListBox_Init);
    //addGlobalFunc('procedure TCustomCheckListBox.Free(); constref;', @Lape_TCustomCheckListBox_Free);
  end;
end;

//constructor Create();
procedure Lape_TCheckListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckListBox(Params^[0])^ := TCheckListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCheckListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckListBox(Params^[0])^.Free();
end;

procedure Lape_Import_TCheckListBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCheckListBox', 'TCustomCheckListBox');

    addGlobalFunc('procedure TCheckListBox.Init(AOwner: TComponent); override;', @Lape_TCheckListBox_Init);
    //addGlobalFunc('procedure TCheckListBox.Free(); constref;', @Lape_TCheckListBox_Free);
  end;
end;

{TCustomPage}

//function CanTab: boolean; override;
procedure Lape_TCustomPage_CanTab(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.CanTab();
end;

//function IsControlVisible: Boolean; override;
procedure Lape_TCustomPage_IsControlVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.IsControlVisible();
end;

//function HandleObjectShouldBeVisible: boolean; override;
procedure Lape_TCustomPage_HandleObjectShouldBeVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomPage(Params^[0])^.HandleObjectShouldBeVisible();
end;

//function VisibleIndex: integer; virtual;
procedure Lape_TCustomPage_VisibleIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomPage(Params^[0])^.VisibleIndex();
end;

//Read: property PageIndex: Integer read GetPageIndex write SetPageIndex;
procedure Lape_TCustomPage_PageIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomPage(Params^[0])^.PageIndex;
end;

//Write: property PageIndex: Integer read GetPageIndex write SetPageIndex;
procedure Lape_TCustomPage_PageIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

//Read: property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
procedure Lape_TCustomPage_TabVisible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomPage(Params^[0])^.TabVisible;
end;

//Write: property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
procedure Lape_TCustomPage_TabVisible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.TabVisible := PBoolean(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read FOnHide write FOnHide;
procedure Lape_TCustomPage_OnHide_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read FOnHide write FOnHide;
procedure Lape_TCustomPage_OnHide_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure Lape_TCustomPage_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomPage(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure Lape_TCustomPage_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCustomPage_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^ := TCustomPage.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomPage_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomPage(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomPage', 'TWinControl');

    addGlobalFunc('procedure TCustomPage.Init(TheOwner: TComponent); override;', @Lape_TCustomPage_Init);
    addGlobalFunc('function TCustomPage.CanTab(): boolean; constref;', @Lape_TCustomPage_CanTab);
    //addGlobalFunc('function TCustomPage.IsControlVisible(): Boolean; constref;', @Lape_TCustomPage_IsControlVisible);
    //addGlobalFunc('function TCustomPage.HandleObjectShouldBeVisible(): boolean; constref;', @Lape_TCustomPage_HandleObjectShouldBeVisible);
    addGlobalFunc('function TCustomPage.VisibleIndex(): integer; constref;', @Lape_TCustomPage_VisibleIndex);
    addClassVar('TCustomPage', 'PageIndex', 'Integer', @Lape_TCustomPage_PageIndex_Read, @Lape_TCustomPage_PageIndex_Write);
    addClassVar('TCustomPage', 'TabVisible', 'Boolean', @Lape_TCustomPage_TabVisible_Read, @Lape_TCustomPage_TabVisible_Write);
    addClassVar('TCustomPage', 'OnHide', 'TNotifyEvent', @Lape_TCustomPage_OnHide_Read, @Lape_TCustomPage_OnHide_Write);
    addClassVar('TCustomPage', 'OnShow', 'TNotifyEvent', @Lape_TCustomPage_OnShow_Read, @Lape_TCustomPage_OnShow_Write);
    //addGlobalFunc('procedure TCustomPage.Free(); constref;', @Lape_TCustomPage_Free);
  end;
end;

//function TabRect(AIndex: Integer): TRect;
procedure Lape_TCustomTabControl_TabRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomTabControl(Params^[0])^.TabRect(PInteger(Params^[1])^);
end;

//function GetImageIndex(ThePageIndex: Integer): Integer; virtual;
procedure Lape_TCustomTabControl_GetImageIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.GetImageIndex(PInteger(Params^[1])^);
end;

//function IndexOf(APage: TPersistent): integer; virtual;
procedure Lape_TCustomTabControl_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.IndexOf(PPersistent(Params^[1])^);
end;

//function CustomPage(Index: integer): TCustomPage;
procedure Lape_TCustomTabControl_CustomPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.CustomPage(Pinteger(Params^[1])^);
end;

//function CanChangePageIndex: boolean; virtual;
procedure Lape_TCustomTabControl_CanChangePageIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomTabControl(Params^[0])^.CanChangePageIndex();
end;

//function GetMinimumTabWidth: integer; virtual;
procedure Lape_TCustomTabControl_GetMinimumTabWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.GetMinimumTabWidth();
end;

//function GetMinimumTabHeight: integer; virtual;
procedure Lape_TCustomTabControl_GetMinimumTabHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.GetMinimumTabHeight();
end;

//function TabToPageIndex(AIndex: integer): integer;
procedure Lape_TCustomTabControl_TabToPageIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.TabToPageIndex(Pinteger(Params^[1])^);
end;

//function PageToTabIndex(AIndex: integer): integer;
procedure Lape_TCustomTabControl_PageToTabIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.PageToTabIndex(Pinteger(Params^[1])^);
end;

//function IndexOfTabAt(X, Y: Integer): Integer;
procedure Lape_TCustomTabControl_IndexOfTabAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  //PInteger(Result)^ := PCustomTabControl(Params^[0])^.IndexOfTabAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure DoCloseTabClicked(APage: TCustomPage); virtual;
procedure Lape_TCustomTabControl_DoCloseTabClicked(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.DoCloseTabClicked(PCustomPage(Params^[1])^);
end;

//Read: property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
procedure Lape_TCustomTabControl_MultiLine_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.MultiLine;
end;

//Write: property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
procedure Lape_TCustomTabControl_MultiLine_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.MultiLine := PBoolean(Params^[1])^;
end;

//Read: property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
procedure Lape_TCustomTabControl_OnChanging_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabChangingEvent(Result)^ := PCustomTabControl(Params^[0])^.OnChanging;
end;

//Write: property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
procedure Lape_TCustomTabControl_OnChanging_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.OnChanging := PTabChangingEvent(Params^[1])^;
end;

//Read: property Options: TCTabControlOptions read FOptions write SetOptions default [];
procedure Lape_TCustomTabControl_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCTabControlOptions(Result)^ := PCustomTabControl(Params^[0])^.Options;
end;

//Write: property Options: TCTabControlOptions read FOptions write SetOptions default [];
procedure Lape_TCustomTabControl_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.Options := PCTabControlOptions(Params^[1])^;
end;

//Read: property Page[Index: Integer]: TCustomPage read GetPage;
procedure Lape_TCustomTabControl_Page_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomPage(Result)^ := PCustomTabControl(Params^[0])^.Page[PInteger(Params^[1])^];
end;

//Read: property PageCount: integer read GetPageCount;
procedure Lape_TCustomTabControl_PageCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomTabControl(Params^[0])^.PageCount;
end;

//Read: property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
procedure Lape_TCustomTabControl_PageIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomTabControl(Params^[0])^.PageIndex;
end;

//Write: property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
procedure Lape_TCustomTabControl_PageIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.PageIndex := PInteger(Params^[1])^;
end;

//Read: property Pages: TStrings read FAccess write SetPages;
procedure Lape_TCustomTabControl_Pages_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomTabControl(Params^[0])^.Pages;
end;

//Write: property Pages: TStrings read FAccess write SetPages;
procedure Lape_TCustomTabControl_Pages_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.Pages := PStrings(Params^[1])^;
end;

//Read: property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
procedure Lape_TCustomTabControl_ShowTabs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomTabControl(Params^[0])^.ShowTabs;
end;

//Write: property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
procedure Lape_TCustomTabControl_ShowTabs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.ShowTabs := PBoolean(Params^[1])^;
end;

//Read: property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
procedure Lape_TCustomTabControl_TabPosition_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabPosition(Result)^ := PCustomTabControl(Params^[0])^.TabPosition;
end;

//Write: property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
procedure Lape_TCustomTabControl_TabPosition_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.TabPosition := PTabPosition(Params^[1])^;
end;

//Read: property IsUnpaged: boolean read FUnPaged;
procedure Lape_TCustomTabControl_IsUnpaged_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  // FIXME: R0b0t1 @ 11/16/17 22:05 CST.
  //Pboolean(Result)^ := PCustomTabControl(Params^[0])^.IsUnpaged;
end;

//constructor Create();
procedure Lape_TCustomTabControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^ := TCustomTabControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomTabControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomTabControl(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomTabControl(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomTabControl', 'TWinControl');

    addGlobalFunc('function TCustomTabControl.TabRect(AIndex: Integer): TRect; constref;', @Lape_TCustomTabControl_TabRect);
    addGlobalFunc('function TCustomTabControl.GetImageIndex(ThePageIndex: Integer): Integer; constref;', @Lape_TCustomTabControl_GetImageIndex);
    addGlobalFunc('function TCustomTabControl.IndexOf(APage: TPersistent): integer; constref;', @Lape_TCustomTabControl_IndexOf);
    addGlobalFunc('function TCustomTabControl.CustomPage(Index: integer): TCustomPage; constref;', @Lape_TCustomTabControl_CustomPage);
    addGlobalFunc('function TCustomTabControl.CanChangePageIndex(): boolean; constref;', @Lape_TCustomTabControl_CanChangePageIndex);
    addGlobalFunc('function TCustomTabControl.GetMinimumTabWidth(): integer; constref;', @Lape_TCustomTabControl_GetMinimumTabWidth);
    addGlobalFunc('function TCustomTabControl.GetMinimumTabHeight(): integer; constref;', @Lape_TCustomTabControl_GetMinimumTabHeight);
    addGlobalFunc('function TCustomTabControl.TabToPageIndex(AIndex: integer): integer; constref;', @Lape_TCustomTabControl_TabToPageIndex);
    addGlobalFunc('function TCustomTabControl.PageToTabIndex(AIndex: integer): integer; constref;', @Lape_TCustomTabControl_PageToTabIndex);
    addGlobalFunc('function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer; constref;', @Lape_TCustomTabControl_IndexOfTabAt);
    addGlobalFunc('procedure TCustomTabControl.DoCloseTabClicked(APage: TCustomPage); constref;', @Lape_TCustomTabControl_DoCloseTabClicked);
    addClassVar('TCustomTabControl', 'MultiLine', 'Boolean', @Lape_TCustomTabControl_MultiLine_Read, @Lape_TCustomTabControl_MultiLine_Write);
    addClassVar('TCustomTabControl', 'OnChanging', 'TTabChangingEvent', @Lape_TCustomTabControl_OnChanging_Read, @Lape_TCustomTabControl_OnChanging_Write);
    addClassVar('TCustomTabControl', 'Options', 'TCTabControlOptions', @Lape_TCustomTabControl_Options_Read, @Lape_TCustomTabControl_Options_Write);
    addClassVar('TCustomTabControl', 'Page', 'TCustomPage', @Lape_TCustomTabControl_Page_Index_Read, nil);
    addClassVar('TCustomTabControl', 'PageCount', 'integer', @Lape_TCustomTabControl_PageCount_Read, nil);
    addClassVar('TCustomTabControl', 'PageIndex', 'Integer', @Lape_TCustomTabControl_PageIndex_Read, @Lape_TCustomTabControl_PageIndex_Write);
    addClassVar('TCustomTabControl', 'Pages', 'TStrings', @Lape_TCustomTabControl_Pages_Read, @Lape_TCustomTabControl_Pages_Write);
    addClassVar('TCustomTabControl', 'ShowTabs', 'Boolean', @Lape_TCustomTabControl_ShowTabs_Read, @Lape_TCustomTabControl_ShowTabs_Write);
    addClassVar('TCustomTabControl', 'TabPosition', 'TTabPosition', @Lape_TCustomTabControl_TabPosition_Read, @Lape_TCustomTabControl_TabPosition_Write);
    addClassVar('TCustomTabControl', 'IsUnpaged', 'boolean', @Lape_TCustomTabControl_IsUnpaged_Read, nil);
    addGlobalFunc('procedure TCustomTabControl.Init(TheOwner: TComponent); override;', @Lape_TCustomTabControl_Init);
    //addGlobalFunc('procedure TCustomTabControl.Free(); constref;', @Lape_TCustomTabControl_Free);
  end;
end;

//Read: property TabIndex: Integer read GetTabIndex;
procedure Lape_TTabSheet_TabIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PTabSheet(Params^[0])^.TabIndex;
end;

//constructor Create();
procedure Lape_TTabSheet_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabSheet(Params^[0])^ := TTabSheet.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TTabSheet_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabSheet(Params^[0])^.Free();
end;

procedure Lape_Import_TTabSheet(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TTabSheet', 'TCustomPage');
    addClassVar('TTabSheet', 'TabIndex', 'Integer', @Lape_TTabSheet_TabIndex_Read, nil);
    addGlobalFunc('procedure TTabSheet.Init(TheOwner: TComponent); override;', @Lape_TTabSheet_Init);
    //addGlobalFunc('procedure TTabSheet.Free(); constref;', @Lape_TTabSheet_Free);
  end;
end;

//procedure SelectNextPage(GoForward: Boolean);
procedure Lape_TPageControl_SelectNextPage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^);
end;

//procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);
procedure Lape_TPageControl_SelectNextPageEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.SelectNextPage(PBoolean(Params^[1])^, PBoolean(Params^[2])^);
end;

//function AddTabSheet: TTabSheet;
procedure Lape_TPageControl_AddTabSheet(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.AddTabSheet();
end;

//Read: property Pages[Index: Integer]: TTabSheet read GetTabSheet;
procedure Lape_TPageControl_Pages_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.Pages[PInteger(Params^[1])^];
end;

//Read: property ActivePage: TTabSheet read GetActiveTabSheet write SetActiveTabSheet;
procedure Lape_TPageControl_ActivePage_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTabSheet(Result)^ := PPageControl(Params^[0])^.ActivePage;
end;

//Write: property ActivePage: TTabSheet read GetActiveTabSheet write SetActiveTabSheet;
procedure Lape_TPageControl_ActivePage_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.ActivePage := PTabSheet(Params^[1])^;
end;

//constructor Create();
procedure Lape_TPageControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^ := TPageControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TPageControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPageControl(Params^[0])^.Free();
end;

procedure Lape_Import_TPageControl(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TPageControl', 'TCustomTabControl');
    addGlobalFunc('procedure TPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean); constref;', @Lape_TPageControl_SelectNextPageEx);
    addGlobalFunc('function TPageControl.AddTabSheet(): TTabSheet; constref;', @Lape_TPageControl_AddTabSheet);
    addClassVar('TPageControl', 'Pages', 'Integer', @Lape_TPageControl_Pages_Read, nil);
    addClassVar('TPageControl', 'ActivePage', 'TTabSheet', @Lape_TPageControl_ActivePage_Read, @Lape_TPageControl_ActivePage_Write);
    addGlobalFunc('procedure TPageControl.Init(TheOwner: TComponent); override;', @Lape_TPageControl_Init);
    //addGlobalFunc('procedure TPageControl.Free(); constref;', @Lape_TPageControl_Free);
  end;
end;

procedure Lape_Import_TStatusBar_Forward(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
    addClass('TStatusBar', 'TWinControl');
end;

//function StatusBar: TStatusBar;
procedure Lape_TStatusPanel_StatusBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Result)^ := PStatusPanel(Params^[0])^.StatusBar();
end;

//Read: property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
procedure Lape_TStatusPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAlignment(Result)^ := PStatusPanel(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
procedure Lape_TStatusPanel_Alignment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
procedure Lape_TStatusPanel_Bevel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanelBevel(Result)^ := PStatusPanel(Params^[0])^.Bevel;
end;

//Write: property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
procedure Lape_TStatusPanel_Bevel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Bevel := PStatusPanelBevel(Params^[1])^;
end;

//Read: property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
procedure Lape_TStatusPanel_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanelStyle(Result)^ := PStatusPanel(Params^[0])^.Style;
end;

//Write: property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
procedure Lape_TStatusPanel_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Style := PStatusPanelStyle(Params^[1])^;
end;

//Read: property Text: TCaption read FText write SetText;
procedure Lape_TStatusPanel_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PLPString(Result)^ := PStatusPanel(Params^[0])^.Text;
end;

//Write: property Text: TCaption read FText write SetText;
procedure Lape_TStatusPanel_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Text := PCaption(Params^[1])^;
end;

//Read: property Width: Integer read FWidth write SetWidth;
procedure Lape_TStatusPanel_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStatusPanel(Params^[0])^.Width;
end;

//Write: property Width: Integer read FWidth write SetWidth;
procedure Lape_TStatusPanel_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//constructor Create();
procedure Lape_TStatusPanel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^ := TStatusPanel.Create(PCollection(Params^[1])^);
end;

//procedure Free();
procedure Lape_TStatusPanel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Params^[0])^.Free();
end;

procedure Lape_Import_TStatusPanel(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStatusPanel', 'TCollectionItem');

    addGlobalFunc('function TStatusPanel.StatusBar(): TStatusBar; constref;', @Lape_TStatusPanel_StatusBar);
    addClassVar('TStatusPanel', 'Alignment', 'TAlignment', @Lape_TStatusPanel_Alignment_Read, @Lape_TStatusPanel_Alignment_Write);
    addClassVar('TStatusPanel', 'Bevel', 'TStatusPanelBevel', @Lape_TStatusPanel_Bevel_Read, @Lape_TStatusPanel_Bevel_Write);
    addClassVar('TStatusPanel', 'Style', 'TStatusPanelStyle', @Lape_TStatusPanel_Style_Read, @Lape_TStatusPanel_Style_Write);
    addClassVar('TStatusPanel', 'Text', 'TCaption', @Lape_TStatusPanel_Text_Read, @Lape_TStatusPanel_Text_Write);
    addClassVar('TStatusPanel', 'Width', 'Integer', @Lape_TStatusPanel_Width_Read, @Lape_TStatusPanel_Width_Write);
    addGlobalFunc('procedure TStatusPanel.Init(ACollection: TCollection); override;', @Lape_TStatusPanel_Init);
    //addGlobalFunc('procedure TStatusPanel.Free(); constref;', @Lape_TStatusPanel_Free);
  end;
end;

//constructor Create(AStatusBar: TStatusBar);
procedure Lape_TStatusPanels_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanels(Params^[0])^ := TStatusPanels.Create(PStatusBar(Params^[1])^);
end;

//function Add: TStatusPanel;
procedure Lape_TStatusPanels_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Add();
end;

//Read: property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
procedure Lape_TStatusPanels_Items_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Result)^ := PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Write: property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
procedure Lape_TStatusPanels_Items_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanels(Params^[0])^.Items[PInteger(Params^[1])^] := PStatusPanel(Params^[2])^;
end;

//Read: property StatusBar: TStatusBar read FStatusBar;
procedure Lape_TStatusPanels_StatusBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Result)^ := PStatusPanels(Params^[0])^.StatusBar;
end;

//procedure Free();
procedure Lape_TStatusPanels_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanels(Params^[0])^.Free();
end;

procedure Lape_Import_TStatusPanels(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStatusPanels', 'TCollection');

    addGlobalFunc('procedure TStatusPanels.Init(AStatusBar: TStatusBar);', @Lape_TStatusPanels_Init);
    addGlobalFunc('function TStatusPanels.Add(): TStatusPanel; constref;', @Lape_TStatusPanels_Add);
    addClassVar('TStatusPanels', 'Items', 'TStatusPanel', @Lape_TStatusPanels_Items_Index_Read, @Lape_TStatusPanels_Items_Index_Write, True); // array
    addClassVar('TStatusPanels', 'StatusBar', 'TStatusBar', @Lape_TStatusPanels_StatusBar_Read, nil);
    //addGlobalFunc('procedure TStatusPanels.Free(); constref;', @Lape_TStatusPanels_Free);
  end;
end;

//procedure InvalidatePanel(PanelIndex: integer; PanelParts: TPanelParts); virtual;
procedure Lape_TStatusBar_InvalidatePanel(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.InvalidatePanel(Pinteger(Params^[1])^, PPanelParts(Params^[2])^);
end;

//procedure BeginUpdate;
procedure Lape_TStatusBar_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.BeginUpdate();
end;

//procedure EndUpdate;
procedure Lape_TStatusBar_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.EndUpdate();
end;

//function GetPanelIndexAt(X, Y: Integer): Integer;
procedure Lape_TStatusBar_GetPanelIndexAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStatusBar(Params^[0])^.GetPanelIndexAt(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function SizeGripEnabled: Boolean;
procedure Lape_TStatusBar_SizeGripEnabled(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGripEnabled();
end;

//function UpdatingStatusBar: boolean;
procedure Lape_TStatusBar_UpdatingStatusBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PStatusBar(Params^[0])^.UpdatingStatusBar();
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure Lape_TStatusBar_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PStatusBar(Params^[0])^.Canvas;
end;

//Read: property AutoHint: Boolean read FAutoHint write FAutoHint default false;
procedure Lape_TStatusBar_AutoHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.AutoHint;
end;

//Write: property AutoHint: Boolean read FAutoHint write FAutoHint default false;
procedure Lape_TStatusBar_AutoHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.AutoHint := PBoolean(Params^[1])^;
end;

//Read: property Panels: TStatusPanels read FPanels write SetPanels;
procedure Lape_TStatusBar_Panels_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanels(Result)^ := PStatusBar(Params^[0])^.Panels;
end;

//Write: property Panels: TStatusPanels read FPanels write SetPanels;
procedure Lape_TStatusBar_Panels_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.Panels := PStatusPanels(Params^[1])^;
end;

//Read: property SimpleText: TCaption read FSimpleText write SetSimpleText;
procedure Lape_TStatusBar_SimpleText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCaption(Result)^ := PStatusBar(Params^[0])^.SimpleText;
end;

//Write: property SimpleText: TCaption read FSimpleText write SetSimpleText;
procedure Lape_TStatusBar_SimpleText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.SimpleText := PCaption(Params^[1])^;
end;

//Read: property SimplePanel: Boolean read FSimplePanel write SetSimplePanel default True;
procedure Lape_TStatusBar_SimplePanel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SimplePanel;
end;

//Write: property SimplePanel: Boolean read FSimplePanel write SetSimplePanel default True;
procedure Lape_TStatusBar_SimplePanel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.SimplePanel := PBoolean(Params^[1])^;
end;

//Read: property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
procedure Lape_TStatusBar_SizeGrip_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStatusBar(Params^[0])^.SizeGrip;
end;

//Write: property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
procedure Lape_TStatusBar_SizeGrip_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.SizeGrip := PBoolean(Params^[1])^;
end;

//Read: property OnHint: TNotifyEvent read FOnHint write FOnHint;
procedure Lape_TStatusBar_OnHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PStatusBar(Params^[0])^.OnHint;
end;

//Write: property OnHint: TNotifyEvent read FOnHint write FOnHint;
procedure Lape_TStatusBar_OnHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.OnHint := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure Lape_TStatusBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^ := TStatusBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TStatusBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Params^[0])^.Free();
end;

procedure Lape_Import_TStatusBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addGlobalFunc('procedure TStatusBar.InvalidatePanel(PanelIndex: integer; PanelParts: TPanelParts); constref;', @Lape_TStatusBar_InvalidatePanel);
    addGlobalFunc('procedure TStatusBar.BeginUpdate(); constref;', @Lape_TStatusBar_BeginUpdate);
    addGlobalFunc('procedure TStatusBar.EndUpdate(); constref;', @Lape_TStatusBar_EndUpdate);
    addGlobalFunc('function TStatusBar.GetPanelIndexAt(X, Y: Integer): Integer; constref;', @Lape_TStatusBar_GetPanelIndexAt);
    addGlobalFunc('function TStatusBar.SizeGripEnabled(): Boolean; constref;', @Lape_TStatusBar_SizeGripEnabled);
    addGlobalFunc('function TStatusBar.UpdatingStatusBar(): boolean; constref;', @Lape_TStatusBar_UpdatingStatusBar);
    addClassVar('TStatusBar', 'Canvas', 'TCanvas', @Lape_TStatusBar_Canvas_Read, nil);
    addClassVar('TStatusBar', 'AutoHint', 'Boolean', @Lape_TStatusBar_AutoHint_Read, @Lape_TStatusBar_AutoHint_Write);
    addClassVar('TStatusBar', 'Panels', 'TStatusPanels', @Lape_TStatusBar_Panels_Read, @Lape_TStatusBar_Panels_Write);
    addClassVar('TStatusBar', 'SimpleText', 'TCaption', @Lape_TStatusBar_SimpleText_Read, @Lape_TStatusBar_SimpleText_Write);
    addClassVar('TStatusBar', 'SimplePanel', 'Boolean', @Lape_TStatusBar_SimplePanel_Read, @Lape_TStatusBar_SimplePanel_Write);
    addClassVar('TStatusBar', 'SizeGrip', 'Boolean', @Lape_TStatusBar_SizeGrip_Read, @Lape_TStatusBar_SizeGrip_Write);
    addClassVar('TStatusBar', 'OnHint', 'TNotifyEvent', @Lape_TStatusBar_OnHint_Read, @Lape_TStatusBar_OnHint_Write);
    addGlobalFunc('procedure TStatusBar.Init(TheOwner: TComponent); override;', @Lape_TStatusBar_Init);
    //addGlobalFunc('procedure TStatusBar.Free(); constref;', @Lape_TStatusBar_Free);
  end;
end;

procedure Lape_Import_LCLComCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addGlobalType('(pbHorizontal, pbVertical, pbRightToLeft, pbTopDown)','TProgressBarOrientation');
    addGlobalType('(pbstNormal, pbstMarquee)','TProgressBarStyle');
    addGlobalType('(trHorizontal, trVertical)', 'TTrackBarOrientation');
    addGlobalType('(tmBottomRight, tmTopLeft, tmBoth)', 'TTickMark');
    addGlobalType('(tsNone, tsAuto, tsManual)', 'TTickStyle');
    addGlobalType('(trLeft, trRight, trTop, trBottom)', 'TTrackBarScalePos');
    addGlobalType('procedure(Sender: TObject; Index: Integer) of object', 'TCheckListClicked', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var AllowChange: Boolean) of object', 'TTabChangingEvent', FFI_DEFAULT_ABI);
    addGlobalType('(tsTabs, tsButtons, tsFlatButtons)', 'TTabStyle');
    addGlobalType('(tpTop, tpBottom, tpLeft, tpRight)', 'TTabPosition');
    addGlobalType('(nboShowCloseButtons, nboMultiLine, nboHidePageListPopup, nboKeyboardTabSwitch, nboShowAddTabButton)', 'TCTabControlOption');
    addGlobalType('set of TCTabControlOption', 'TCTabControlOptions');
    addGlobalType('(ppText, ppBorder, ppWidth)', 'TPanelPart');
    addGlobalType('set of TPanelPart', 'TPanelParts');
    addGlobalType('(psText, psOwnerDraw)', 'TStatusPanelStyle');
    addGlobalType('(pbNone, pbLowered, pbRaised)', 'TStatusPanelBevel');
  end;

  Lape_Import_TCustomProgressBar(Compiler);
  Lape_Import_TProgressBar(Compiler);
  Lape_Import_TCustomTrackBar(Compiler);
  Lape_Import_TTrackBar(Compiler);
  Lape_Import_TCustomCheckListBox(Compiler);
  Lape_Import_TCheckListBox(Compiler);
  Lape_Import_TCustomPage(Compiler);
  Lape_Import_TCustomTabControl(Compiler);
  Lape_Import_TTabSheet(Compiler);
  Lape_Import_TPageControl(Compiler);
  Lape_Import_TStatusBar_Forward(Compiler);
  Lape_Import_TStatusPanel(Compiler);
  Lape_Import_TStatusPanels(Compiler);
  Lape_Import_TStatusBar(Compiler);
end;

end.
