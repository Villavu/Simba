unit simba.import_lcl_form;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.threading, simba.script_compiler;

procedure ImportLCLForm(Compiler: TSimbaScript_Compiler);

implementation

uses
  IniPropStorage, controls, extctrls, comctrls, graphics, forms, dialogs, lptypes, ffi;

type
  PStrings = ^TStrings;
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
  PControl = ^TControl;
  PNotifyEvent = ^TNotifyEvent;
  PDropFilesEvent = ^TDropFilesEvent;
  PMouseWheelEvent = ^TMouseWheelEvent;
  PMouseWheelUpDownEvent = ^TMouseWheelUpDownEvent;

  PComponent = ^TComponent;
  PWinControl = ^TWinControl;
  PFormBorderStyle = ^TFormBorderStyle;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;

  PColorDialog = ^TColorDialog;
  PCommonDialog = ^TCommonDialog;
  PFileDialog = ^TFileDialog;
  POpenDialog = ^TOpenDialog;
  POpenOption = ^TOpenOption;
  POpenOptions = ^TOpenOptions;

procedure _LapeSizeConstraints_Control_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := PSizeConstraints(Params^[0])^.Control;
end;

procedure _LapeSizeConstraints_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSizeConstraints(Params^[0])^.OnChange;
end;

procedure _LapeSizeConstraints_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSizeConstraints(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSizeConstraints_MaxHeight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PConstraintSize(Result)^ := PSizeConstraints(Params^[0])^.MaxHeight;
end;

procedure _LapeSizeConstraints_MaxHeight_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSizeConstraints(Params^[0])^.MaxHeight := PConstraintSize(Params^[1])^;
end;

procedure _LapeSizeConstraints_MaxWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MaxWidth;
end;

procedure _LapeSizeConstraints_MaxWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSizeConstraints(Params^[0])^.MaxWidth := PInteger(Params^[1])^;
end;

procedure _LapeSizeConstraints_MinHeight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinHeight;
end;

procedure _LapeSizeConstraints_MinHeight_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSizeConstraints(Params^[0])^.MinHeight := PInteger(Params^[1])^;
end;

procedure _LapeSizeConstraints_MinWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinWidth;
end;

procedure _LapeSizeConstraints_MinWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSizeConstraints(Params^[0])^.MinWidth := PConstraintSize(Params^[1])^;
end;

procedure _LapeCustomForm_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Result)^ := TCustomForm.CreateNew(PComponent(Params^[0])^);
end;

procedure _LapeCustomForm_Close(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.Close();
end;

procedure _LapeCustomForm_CloseQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.CloseQuery();
end;

procedure _LapeCustomForm_EnsureVisible(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PCustomForm(Params^[0])^.EnsureVisible(PBoolean(Params^[1])^);
  end;

begin
  RunInMainThread(@Execute);
end;

procedure _LapeCustomForm_FocusControl(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.FocusControl(PWinControl(Params^[1])^);
end;

procedure _LapeCustomForm_Hide(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.Hide();
end;

procedure _LapeCustomForm_GetPreferredSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

procedure _LapeCustomForm_CanFocus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.CanFocus();
end;

procedure _LapeCustomForm_SetFocus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.SetFocus();
end;

procedure _LapeCustomForm_SetFocusedControl(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.SetFocusedControl(PWinControl(Params^[1])^);
end;

procedure _LapeCustomForm_SetRestoredBounds(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.SetRestoredBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeCustomForm_StayOnTop_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if (PCustomForm(Params^[0])^.FormStyle = fsSystemStayOnTop) then
    PBoolean(Result)^ := True
  else
    PBoolean(Result)^ := False;
end;

procedure _LapeCustomForm_StayOnTop_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if PBoolean(Params^[1])^ then
    PCustomForm(Params^[0])^.FormStyle := fsSystemStayOnTop
  else
    PCustomForm(Params^[0])^.FormStyle := fsNormal;
end;

procedure _LapeCustomForm_Show(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PCustomForm(Params^[0])^.Show();
  end;

begin
  RunInMainThread(@Execute);
end;

procedure _LapeCustomForm_ShowModal(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PInteger(Result)^ := PCustomForm(Params^[0])^.ShowModal();
  end;

begin
  RunInMainThread(@Execute);
end;

procedure _LapeCustomForm_ShowOnTop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PCustomForm(Params^[0])^.ShowOnTop();
  end;

begin
  RunInMainThread(@Execute);
end;

procedure _LapeCustomForm_Active_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.Active;
end;

procedure _LapeCustomForm_ActiveControl_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Result)^ := PCustomForm(Params^[0])^.ActiveControl;
end;

procedure _LapeCustomForm_ActiveControl_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.ActiveControl := PWinControl(Params^[1])^;
end;

procedure _LapeCustomForm_ActiveDefaultControl_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.ActiveDefaultControl;
end;

procedure _LapeCustomForm_ActiveDefaultControl_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.ActiveDefaultControl := PControl(Params^[1])^;
end;

procedure _LapeCustomForm_AllowDropFiles_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AllowDropFiles;
end;

procedure _LapeCustomForm_AllowDropFiles_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.AllowDropFiles := PBoolean(Params^[1])^;
end;

procedure _LapeCustomForm_AlphaBlend_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AlphaBlend;
end;

procedure _LapeCustomForm_AlphaBlend_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.AlphaBlend := PBoolean(Params^[1])^;
end;

procedure _LapeCustomForm_AlphaBlendValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  pbyte(Result)^ := PCustomForm(Params^[0])^.AlphaBlendValue;
end;

procedure _LapeCustomForm_AlphaBlendValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.AlphaBlendValue := pbyte(Params^[1])^;
end;

procedure _LapeCustomForm_DefaultControl_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.DefaultControl;
end;

procedure _LapeCustomForm_DefaultControl_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.DefaultControl := PControl(Params^[1])^;
end;

procedure _LapeCustomForm_KeyPreview_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.KeyPreview;
end;

procedure _LapeCustomForm_KeyPreview_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.KeyPreview := PBoolean(Params^[1])^;
end;

procedure _LapeCustomForm_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomForm(Params^[0])^.PixelsPerInch;
end;

procedure _LapeCustomForm_PixelsPerInch_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.PixelsPerInch := PInteger(Params^[1])^;
end;

procedure _LapeCustomForm_RestoredLeft_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredLeft;
end;

procedure _LapeCustomForm_RestoredTop_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredTop;
end;

procedure _LapeCustomForm_RestoredWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredWidth;
end;

procedure _LapeCustomForm_RestoredHeight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredHeight;
end;

procedure _LapeCustomForm_Write_BorderStyle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

procedure _LapeCustomForm_Read_BorderStyle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFormBorderStyle(Result)^ := PCustomForm(Params^[0])^.BorderStyle;
end;

procedure _LapeCustomForm_Write_BorderIcons(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.BorderIcons := PBorderIcons(Params^[1])^;
end;

procedure _LapeCustomForm_Read_BorderIcons(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBorderIcons(Result)^ := PCustomForm(Params^[0])^.BorderIcons;
end;

procedure _LapeCustomForm_Constraints_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSizeConstraints(Result)^ := PCustomForm(Params^[0])^.Constraints;
end;

procedure _LapeCustomForm_Constraints_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.Constraints := PSizeConstraints(Params^[1])^;
end;

procedure _LapeCustomForm_ShowInTaskBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PShowInTaskBar(Result)^ := PCustomForm(Params^[0])^.ShowInTaskBar;
end;

procedure _LapeCustomForm_ShowInTaskBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.ShowInTaskBar := PShowInTaskBar(Params^[1])^;
end;

procedure _LapeForm_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Result)^ := TForm.CreateNew(PComponent(Params^[0])^);
  PForm(Result)^.ShowInTaskBar := stAlways;
end;

procedure _LapeFormThread_Run(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  RunInMainThread(TThreadMethod(Params^[0]^));
end;

procedure _LapeFormThread_Queue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  QueueOnMainThread(TThreadMethod(Params^[0]^));
end;

procedure _LapeFormThread_IsCurrentThread(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := IsMainThread();
end;

procedure _LapeFormThread_ProcessMessages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Application.ProcessMessages();
end;

procedure _LapeForm_SaveSession(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  function ArrToSessionProperties(const Arr: TStringArray): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to High(Arr) do
      Result := Result + Arr[I] + ';';
  end;

begin
  PForm(Params^[0])^.SessionProperties := ArrToSessionProperties(PStringArray(Params^[2])^);

  with TIniPropStorage.Create(PForm(Params^[0])^) do
  try
    IniFileName := PString(Params^[1])^;
    Save();
  finally
    Free();
  end;
end;

procedure _LapeForm_RestoreSession(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  function ArrToSessionProperties(const Arr: TStringArray): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to High(Arr) do
      Result := Result + Arr[I] + ';';
  end;

begin
  PForm(Params^[0])^.SessionProperties := ArrToSessionProperties(PStringArray(Params^[2])^);

  with TIniPropStorage.Create(PForm(Params^[0])^) do
  try
    IniFileName := PString(Params^[1])^;
    Restore();
  finally
    Free();
  end;
end;

procedure _LapeForm_OnActivate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnActivate;
end;

procedure _LapeForm_OnActivate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnActivate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCloseEvent(Result)^ := PForm(Params^[0])^.OnClose;
end;

procedure _LapeForm_OnClose_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

procedure _LapeForm_OnCloseQuery_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCloseQueryEvent(Result)^ := PForm(Params^[0])^.OnCloseQuery;
end;

procedure _LapeForm_OnCloseQuery_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnCloseQuery := PCloseQueryEvent(Params^[1])^;
end;

procedure _LapeForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnCreate;
end;

procedure _LapeForm_OnCreate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDeactivate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDeactivate;
end;

procedure _LapeForm_OnDeactivate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnDeactivate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDestroy;
end;

procedure _LapeForm_OnDestroy_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDropFiles_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDropFilesEvent(Result)^ := PForm(Params^[0])^.OnDropFiles;
end;

procedure _LapeForm_OnDropFiles_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnDropFiles := PDropFilesEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnMouseLeave;
end;

procedure _LapeForm_OnMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnMouseEnter;
end;

procedure _LapeForm_OnMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseWheel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseWheelEvent(Result)^ := PForm(Params^[0])^.OnMouseWheel;
end;

procedure _LapeForm_OnMouseWheel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseWheel := PMouseWheelEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseWheelUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseWheelUpDownEvent(Result)^ := PForm(Params^[0])^.OnMouseWheelUp;
end;

procedure _LapeForm_OnMouseWheelUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseWheelUp := PMouseWheelUpDownEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseWheelDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseWheelUpDownEvent(Result)^ := PForm(Params^[0])^.OnMouseWheelDown;
end;

procedure _LapeForm_OnMouseWheelDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseWheelDown := PMouseWheelUpDownEvent(Params^[1])^;
end;

procedure _LapeForm_OnWindowStateChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnWindowStateChange;
end;

procedure _LapeForm_OnWindowStateChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnWindowStateChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnHide;
end;

procedure _LapeForm_OnHide_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnPaint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnPaint;
end;

procedure _LapeForm_OnPaint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnShow;
end;

procedure _LapeForm_OnShow_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDblClick;
end;

procedure _LapeForm_OnDblClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnEnter;
end;

procedure _LapeForm_OnEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnExit_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnExit;
end;

procedure _LapeForm_OnExit_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnClick;
end;

procedure _LapeForm_OnClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnResize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnResize;
end;

procedure _LapeForm_OnResize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomForm_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPosition(Result)^ := PCustomForm(Params^[0])^.Position;
end;

procedure _LapeCustomForm_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.Position := PPosition(Params^[1])^;
end;

procedure _LapeCustomForm_AutoScroll_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AutoScroll;
end;

procedure _LapeCustomForm_AutoScroll_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomForm(Params^[0])^.AutoScroll := PBoolean(Params^[1])^;
end;

procedure _LapeForm_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseMoveEvent(Result)^ := PForm(Params^[0])^.OnMouseMove;
end;

procedure _LapeForm_OnMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PForm(Params^[0])^.OnMouseDown;
end;

procedure _LapeForm_OnMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PForm(Params^[0])^.OnMouseUp;
end;

procedure _LapeForm_OnKeyUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

procedure _LapeForm_OnKeyUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKeyEvent(Result)^ := PForm(Params^[0])^.OnKeyUp;
end;

procedure _LapeForm_OnKeyDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure _LapeForm_OnKeyDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKeyEvent(Result)^ := PForm(Params^[0])^.OnKeyDown;
end;

procedure _LapeForm_OnKeyPress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PForm(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure _LapeForm_OnKeyPress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKeyPressEvent(Result)^ := PForm(Params^[0])^.OnKeyPress;
end;

procedure _LapeScrollBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollBox(Result)^ := TScrollBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCommonDialog_Execute(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.Execute();
end;

procedure _LapeCommonDialog_UserChoice_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.UserChoice;
end;

procedure _LapeCommonDialog_UserChoice_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.UserChoice := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Close(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.Close();
end;

procedure _LapeCommonDialog_OnClose_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnClose;
end;

procedure _LapeCommonDialog_OnClose_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.OnClose := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_OnCanClose_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCloseQueryEvent(Result)^ := PCommonDialog(Params^[0])^.OnCanClose;
end;

procedure _LapeCommonDialog_OnCanClose_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.OnCanClose := PCloseQueryEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_OnShow_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnShow;
end;

procedure _LapeCommonDialog_OnShow_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Width;
end;

procedure _LapeCommonDialog_Width_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Height;
end;

procedure _LapeCommonDialog_Height_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCommonDialog(Params^[0])^.Height := Pinteger(Params^[1])^;
end;

procedure _LapeFileDialog_Execute(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PFileDialog(Params^[0])^.Execute();
end;

procedure _LapeFileDialog_Files_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.Files;
end;

procedure _LapeFileDialog_HistoryList_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.HistoryList;
end;

procedure _LapeFileDialog_HistoryList_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.HistoryList := PStrings(Params^[1])^;
end;

procedure _LapeFileDialog_DefaultExt_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PFileDialog(Params^[0])^.DefaultExt;
end;

procedure _LapeFileDialog_DefaultExt_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.DefaultExt := PString(Params^[1])^;
end;

procedure _LapeFileDialog_FileName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PFileDialog(Params^[0])^.FileName;
end;

procedure _LapeFileDialog_FileName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.FileName := PString(Params^[1])^;
end;

procedure _LapeFileDialog_Filter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PFileDialog(Params^[0])^.Filter;
end;

procedure _LapeFileDialog_Filter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.Filter := PString(Params^[1])^;
end;

procedure _LapeFileDialog_FilterIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PFileDialog(Params^[0])^.FilterIndex;
end;

procedure _LapeFileDialog_FilterIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.FilterIndex := PInteger(Params^[1])^;
end;

procedure _LapeFileDialog_InitialDir_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PFileDialog(Params^[0])^.InitialDir;
end;

procedure _LapeFileDialog_InitialDir_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.InitialDir := PString(Params^[1])^;
end;

procedure _LapeFileDialog_OnHelpClicked_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnHelpClicked;
end;

procedure _LapeFileDialog_OnHelpClicked_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.OnHelpClicked := PNotifyEvent(Params^[1])^;
end;

procedure _LapeFileDialog_OnTypeChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnTypeChange;
end;

procedure _LapeFileDialog_OnTypeChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Params^[0])^.OnTypeChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeFileDialog_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFileDialog(Result)^ := TFileDialog.Create(PComponent(Params^[0])^);
end;

procedure _LapeOpenDialog_IntfSetOption(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  POpenDialog(Params^[0])^.IntfSetOption(POpenOption(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeOpenDialog_Options_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  POpenOptions(Result)^ := POpenDialog(Params^[0])^.Options;
end;

procedure _LapeOpenDialog_Options_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  POpenDialog(Params^[0])^.Options := POpenOptions(Params^[1])^;
end;

procedure _LapeOpenDialog_OnFolderChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnFolderChange;
end;

procedure _LapeOpenDialog_OnFolderChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  POpenDialog(Params^[0])^.OnFolderChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeOpenDialog_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnSelectionChange;
end;

procedure _LapeOpenDialog_OnSelectionChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  POpenDialog(Params^[0])^.OnSelectionChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeOpenDialog_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  POpenDialog(Result)^ := TOpenDialog.Create(PComponent(Params^[0])^);
end;

procedure _LapeColorDialog_Color_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PColorDialog(Params^[0])^.Color;
end;

procedure _LapeColorDialog_Color_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PColorDialog(Params^[0])^.Color := PColor(Params^[1])^;
end;

procedure _LapeColorDialog_CustomColors_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PColorDialog(Params^[0])^.CustomColors;
end;

procedure _LapeColorDialog_CustomColors_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PColorDialog(Params^[0])^.CustomColors := PStrings(Params^[1])^;
end;

procedure _LapeColorDialog_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorDialog(Result)^ := TColorDialog.Create(PComponent(Params^[0])^);
end;

procedure ImportLCLForm(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('enum(Default, Always, Never)', 'ELazFormShowInTaskbar');
    addGlobalType('enum(None, Single, Sizeable, Dialog, ToolWindow, SizeToolWin)', 'ELazFormBorderStyle');
    addGlobalType('enum(None, Hide, Free, Minimize)', 'ELazFormCloseAction');
    addGlobalType('enum(Designed, Default, DefaultPosOnly, DefaultSizeOnly, ScreenCenter, DesktopCenter, MainFormCenter, OwnerFormCenter, WorkAreaCenter)', 'ELazFormPosition');
    addGlobalType('set of enum(SystemMenu, Minimize, Maximize, Help)', 'ELazFormBorderIcons');
    addGlobalType('set of enum(ReadOnly, OverwritePrompt, HideReadOnly, NoChangeDir, ShowHelp, NoValidate, AllowMultiSelect, ExtensionDifferent, PathMustExist, FileMustExist, CreatePrompt, ShareAware, NoReadOnlyReturn, NoTestFileCreate, NoNetworkButton, NoLongNames, OldStyleDialog, NoDereferenceLinks, EnableIncludeNotify, EnableSizing, DontAddToRecent, ForceShowHidden, ViewDetail, AutoPreview)', 'ELazOpenFileOptions');

    addGlobalType('procedure(Sender: TObject; const FileNames: TStringArray) of object', 'TLazDropFilesEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var CloseAction: ELazFormCloseAction) of object', 'TLazCloseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var CanClose: Boolean) of object', 'TLazCloseQueryEvent', FFI_DEFAULT_ABI);

    addClass('TLazSizeConstraints');
    addProperty('TLazSizeConstraints', 'Control', 'TLazControl', @_LapeSizeConstraints_Control_Read);
    addProperty('TLazSizeConstraints', 'OnChange', 'TLazNotifyEvent', @_LapeSizeConstraints_OnChange_Read, @_LapeSizeConstraints_OnChange_Write);
    addProperty('TLazSizeConstraints', 'MaxHeight', 'Integer', @_LapeSizeConstraints_MaxHeight_Read, @_LapeSizeConstraints_MaxHeight_Write);
    addProperty('TLazSizeConstraints', 'MaxWidth', 'Integer', @_LapeSizeConstraints_MaxWidth_Read, @_LapeSizeConstraints_MaxWidth_Write);
    addProperty('TLazSizeConstraints', 'MinHeight', 'Integer', @_LapeSizeConstraints_MinHeight_Read, @_LapeSizeConstraints_MinHeight_Write);
    addProperty('TLazSizeConstraints', 'MinWidth', 'Integer', @_LapeSizeConstraints_MinWidth_Read, @_LapeSizeConstraints_MinWidth_Write);

    addClass('TLazCustomForm', 'TLazScrollingWinControl');
    addClassConstructor('TLazCustomForm', '(AOwner: TLazCustomForm)', @_LapeCustomForm_Create);
    addGlobalFunc('procedure TLazCustomForm.Close;', @_LapeCustomForm_Close);
    addGlobalFunc('function TLazCustomForm.CloseQuery: Boolean;', @_LapeCustomForm_CloseQuery);
    addGlobalFunc('procedure TLazCustomForm.EnsureVisible(AMoveToTop: Boolean);', @_LapeCustomForm_EnsureVisible);
    addGlobalFunc('procedure TLazCustomForm.FocusControl(WinControl: TLazWinControl);', @_LapeCustomForm_FocusControl);
    addGlobalFunc('function TLazCustomForm.SetFocusedControl(Control: TLazWinControl): Boolean;', @_LapeCustomForm_SetFocusedControl);
    addGlobalFunc('procedure TLazCustomForm.SetRestoredBounds(ALeft, ATop, AWidth, AHeight: Integer);', @_LapeCustomForm_SetRestoredBounds);
    addGlobalFunc('function TLazCustomForm.ShowModal: Integer;', @_LapeCustomForm_ShowModal);
    addGlobalFunc('procedure TLazCustomForm.ShowOnTop;', @_LapeCustomForm_ShowOnTop);
    addProperty('TLazCustomForm', 'BorderStyle', 'ELazFormBorderStyle', @_LapeCustomForm_Read_BorderStyle, @_LapeCustomForm_Write_BorderStyle);
    addProperty('TLazCustomForm', 'BorderIcons', 'ELazFormBorderIcons', @_LapeCustomForm_Read_BorderIcons, @_LapeCustomForm_Write_BorderIcons);
    addProperty('TLazCustomForm', 'Active', 'Boolean', @_LapeCustomForm_Active_Read);
    addProperty('TLazCustomForm', 'ActiveControl', 'TLazWinControl', @_LapeCustomForm_ActiveControl_Read, @_LapeCustomForm_ActiveControl_Write);
    addProperty('TLazCustomForm', 'AllowDropFiles', 'Boolean', @_LapeCustomForm_AllowDropFiles_Read, @_LapeCustomForm_AllowDropFiles_Write);
    addProperty('TLazCustomForm', 'AlphaBlend', 'Boolean', @_LapeCustomForm_AlphaBlend_Read, @_LapeCustomForm_AlphaBlend_Write);
    addProperty('TLazCustomForm', 'AlphaBlendValue', 'Byte', @_LapeCustomForm_AlphaBlendValue_Read, @_LapeCustomForm_AlphaBlendValue_Write);
    addProperty('TLazCustomForm', 'DefaultControl', 'TLazControl', @_LapeCustomForm_DefaultControl_Read, @_LapeCustomForm_DefaultControl_Write);
    addProperty('TLazCustomForm', 'KeyPreview', 'Boolean', @_LapeCustomForm_KeyPreview_Read, @_LapeCustomForm_KeyPreview_Write);
    addProperty('TLazCustomForm', 'PixelsPerInch', 'Integer', @_LapeCustomForm_PixelsPerInch_Read, @_LapeCustomForm_PixelsPerInch_Write);
    addProperty('TLazCustomForm', 'RestoredLeft', 'Integer', @_LapeCustomForm_RestoredLeft_Read);
    addProperty('TLazCustomForm', 'RestoredTop', 'Integer', @_LapeCustomForm_RestoredTop_Read);
    addProperty('TLazCustomForm', 'RestoredWidth', 'Integer', @_LapeCustomForm_RestoredWidth_Read);
    addProperty('TLazCustomForm', 'RestoredHeight', 'Integer', @_LapeCustomForm_RestoredHeight_Read);
    addProperty('TLazCustomForm', 'Constraints', 'TLazSizeConstraints', @_LapeCustomForm_Constraints_Read, @_LapeCustomForm_Constraints_Write);
    addProperty('TLazCustomForm', 'ShowInTaskBar', 'ELazFormShowInTaskbar', @_LapeCustomForm_ShowInTaskBar_Read, @_LapeCustomForm_ShowInTaskBar_Write);
    addProperty('TLazCustomForm', 'Position', 'ELazFormPosition', @_LapeCustomForm_Position_Read, @_LapeCustomForm_Position_Write);
    addProperty('TLazCustomForm', 'AutoScroll', 'Boolean', @_LapeCustomForm_AutoScroll_Read, @_LapeCustomForm_AutoScroll_Write);
    addProperty('TLazCustomForm', 'StayOnTop', 'Boolean', @_LapeCustomForm_StayOnTop_Read, @_LapeCustomForm_StayOnTop_Write);

    addClass('TLazForm', 'TLazCustomForm');
    addClassConstructor('TLazForm', '(AOwner: TLazComponent = nil)', @_LapeForm_Create);

    addGlobalType('record end;', 'LazFormThread');
    addGlobalType('procedure() of object', 'TLazFormThreadMethod', FFI_DEFAULT_ABI);
    addGlobalFunc('procedure LazFormThread.Run(Method: TLazFormThreadMethod); static', @_LapeFormThread_Run);
    addGlobalFunc('procedure LazFormThread.Queue(Method: TLazFormThreadMethod); static', @_LapeFormThread_Queue);
    addGlobalFunc('function LazFormThread.IsCurrentThread: Boolean; static', @_LapeFormThread_IsCurrentThread);
    addGlobalFunc('procedure LazFormThread.ProcessMessages; static', @_LapeFormThread_ProcessMessages);

    addGlobalFunc('procedure TLazForm.SaveSession(FileName: String; Things: TStringArray);', @_LapeForm_SaveSession);
    addGlobalFunc('procedure TLazForm.RestoreSession(FileName: String; Things: TStringArray);', @_LapeForm_RestoreSession);

    addProperty('TLazForm', 'OnActivate', 'TLazNotifyEvent', @_LapeForm_OnActivate_Read, @_LapeForm_OnActivate_Write);
    addProperty('TLazForm', 'OnClose', 'TLazCloseEvent', @_LapeForm_OnClose_Read, @_LapeForm_OnClose_Write);
    addProperty('TLazForm', 'OnCloseQuery', 'TLazCloseQueryEvent', @_LapeForm_OnCloseQuery_Read, @_LapeForm_OnCloseQuery_Write);
    addProperty('TLazForm', 'OnCreate', 'TLazNotifyEvent', @_LapeForm_OnCreate_Read, @_LapeForm_OnCreate_Write);
    addProperty('TLazForm', 'OnDblClick', 'TLazNotifyEvent', @_LapeForm_OnDblClick_Read, @_LapeForm_OnDblClick_Write);
    addProperty('TLazForm', 'OnDeactivate', 'TLazNotifyEvent', @_LapeForm_OnDeactivate_Read, @_LapeForm_OnDeactivate_Write);
    addProperty('TLazForm', 'OnDestroy', 'TLazNotifyEvent', @_LapeForm_OnDestroy_Read, @_LapeForm_OnDestroy_Write);
    addProperty('TLazForm', 'OnDropFiles', 'TLazDropFilesEvent', @_LapeForm_OnDropFiles_Read, @_LapeForm_OnDropFiles_Write);
    addProperty('TLazForm', 'OnHide', 'TLazNotifyEvent', @_LapeForm_OnHide_Read, @_LapeForm_OnHide_Write);
    addProperty('TLazForm', 'OnMouseDown', 'TLazMouseEvent', @_LapeForm_OnMouseDown_Read, @_LapeForm_OnMouseDown_Write);
    addProperty('TLazForm', 'OnMouseEnter', 'TLazNotifyEvent', @_LapeForm_OnMouseEnter_Read, @_LapeForm_OnMouseEnter_Write);
    addProperty('TLazForm', 'OnMouseLeave', 'TLazNotifyEvent', @_LapeForm_OnMouseLeave_Read, @_LapeForm_OnMouseLeave_Write);
    addProperty('TLazForm', 'OnMouseMove', 'TLazMouseMoveEvent', @_LapeForm_OnMouseMove_Read, @_LapeForm_OnMouseMove_Write);
    addProperty('TLazForm', 'OnMouseUp', 'TLazMouseEvent', @_LapeForm_OnMouseUp_Read, @_LapeForm_OnMouseUp_Write);
    addProperty('TLazForm', 'OnMouseWheel', 'TLazMouseWheelEvent', @_LapeForm_OnMouseWheel_Read, @_LapeForm_OnMouseWheel_Write);
    addProperty('TLazForm', 'OnMouseWheelDown', 'TLazMouseWheelUpDownEvent', @_LapeForm_OnMouseWheelDown_Read, @_LapeForm_OnMouseWheelDown_Write);
    addProperty('TLazForm', 'OnMouseWheelUp', 'TLazMouseWheelUpDownEvent', @_LapeForm_OnMouseWheelUp_Read, @_LapeForm_OnMouseWheelUp_Write);
    addProperty('TLazForm', 'OnShow', 'TLazNotifyEvent', @_LapeForm_OnShow_Read, @_LapeForm_OnShow_Write);
    addProperty('TLazForm', 'OnWindowStateChange', 'TLazNotifyEvent', @_LapeForm_OnWindowStateChange_Read, @_LapeForm_OnWindowStateChange_Write);

    addClass('TLazScrollBox', 'TLazScrollingWinControl');
    addClassConstructor('TLazScrollBox', '(AOwner: TLazComponent)', @_LapeScrollBox_Create);

    addClass('TLazCommonDialog', 'TLazComponent');
    addGlobalFunc('function TLazCommonDialog.Execute: Boolean;', @_LapeCommonDialog_Execute);
    addProperty('TLazCommonDialog', 'UserChoice', 'Integer', @_LapeCommonDialog_UserChoice_Read, @_LapeCommonDialog_UserChoice_Write);
    addGlobalFunc('procedure TLazCommonDialog.Close;', @_LapeCommonDialog_Close);
    addProperty('TLazCommonDialog', 'OnClose', 'TLazNotifyEvent', @_LapeCommonDialog_OnClose_Read, @_LapeCommonDialog_OnClose_Write);
    addProperty('TLazCommonDialog', 'OnCanClose', 'TLazCloseQueryEvent', @_LapeCommonDialog_OnCanClose_Read, @_LapeCommonDialog_OnCanClose_Write);
    addProperty('TLazCommonDialog', 'OnShow', 'TLazNotifyEvent', @_LapeCommonDialog_OnShow_Read, @_LapeCommonDialog_OnShow_Write);
    addProperty('TLazCommonDialog', 'Width', 'Integer', @_LapeCommonDialog_Width_Read, @_LapeCommonDialog_Width_Write);
    addProperty('TLazCommonDialog', 'Height', 'Integer', @_LapeCommonDialog_Height_Read, @_LapeCommonDialog_Height_Write);

    AddClass('TLazFileDialog', 'TLazCommonDialog');
    addProperty('TLazFileDialog', 'Files', 'TLazStrings', @_LapeFileDialog_Files_Read);
    addProperty('TLazFileDialog', 'HistoryList', 'TLazStrings', @_LapeFileDialog_HistoryList_Read, @_LapeFileDialog_HistoryList_Write);
    addProperty('TLazFileDialog', 'DefaultExt', 'String', @_LapeFileDialog_DefaultExt_Read, @_LapeFileDialog_DefaultExt_Write);
    addProperty('TLazFileDialog', 'FileName', 'String', @_LapeFileDialog_FileName_Read, @_LapeFileDialog_FileName_Write);
    addProperty('TLazFileDialog', 'Filter', 'String', @_LapeFileDialog_Filter_Read, @_LapeFileDialog_Filter_Write);
    addProperty('TLazFileDialog', 'FilterIndex', 'Integer', @_LapeFileDialog_FilterIndex_Read, @_LapeFileDialog_FilterIndex_Write);
    addProperty('TLazFileDialog', 'InitialDir', 'String', @_LapeFileDialog_InitialDir_Read, @_LapeFileDialog_InitialDir_Write);
    addProperty('TLazFileDialog', 'OnHelpClicked', 'TLazNotifyEvent', @_LapeFileDialog_OnHelpClicked_Read, @_LapeFileDialog_OnHelpClicked_Write);
    addProperty('TLazFileDialog', 'OnTypeChange', 'TLazNotifyEvent', @_LapeFileDialog_OnTypeChange_Read, @_LapeFileDialog_OnTypeChange_Write);
    addClassConstructor('TLazFileDialog', '(AOwner: TLazComponent)', @_LapeFileDialog_Create);

    addClass('TLazOpenDialog', 'TLazFileDialog');
    addProperty('TLazOpenDialog', 'Options', 'ELazOpenFileOptions', @_LapeOpenDialog_Options_Read, @_LapeOpenDialog_Options_Write);
    addProperty('TLazOpenDialog', 'OnFolderChange', 'TLazNotifyEvent', @_LapeOpenDialog_OnFolderChange_Read, @_LapeOpenDialog_OnFolderChange_Write);
    addProperty('TLazOpenDialog', 'OnSelectionChange', 'TLazNotifyEvent', @_LapeOpenDialog_OnSelectionChange_Read, @_LapeOpenDialog_OnSelectionChange_Write);
    addClassConstructor('TLazOpenDialog', '(AOwner: TLazComponent)', @_LapeOpenDialog_Create);

    addClass('TLazColorDialog', 'TLazCommonDialog');
    addProperty('TLazColorDialog', 'Color', 'TColor', @_LapeColorDialog_Color_Read, @_LapeColorDialog_Color_Write);
    addProperty('TLazColorDialog', 'CustomColors', 'TLazStrings', @_LapeColorDialog_CustomColors_Read, @_LapeColorDialog_CustomColors_Write);
    addClassConstructor('TLazColorDialog', '(AOwner: TLazComponent)', @_LapeColorDialog_Create);
  end;
end;

end.

