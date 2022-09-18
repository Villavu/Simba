unit simba.import_lcl_form;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, controls, extctrls, comctrls, graphics, forms, dialogs, lptypes, ffi,
  simba.script_compiler;

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
  PBitmap = ^TBitmap;
  PCanvas = ^TCanvas;
  PFont = ^TFont;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;

  PColorDialog = ^TColorDialog;
  PCommonDialog = ^TCommonDialog;
  PFileDialog = ^TFileDialog;
  POpenDialog = ^TOpenDialog;
  POpenOption = ^TOpenOption;
  POpenOptions = ^TOpenOptions;

procedure _LapeSizeConstraints_Control_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PSizeConstraints(Params^[0])^.Control;
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

procedure _LapeCustomForm_GetPreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
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

procedure _LapeCustomForm_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomForm(Params^[0])^.PixelsPerInch;
end;

procedure _LapeCustomForm_PixelsPerInch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.PixelsPerInch := PInteger(Params^[1])^;
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

procedure _LapeForm_OnActivate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnActivate;
end;

procedure _LapeForm_OnActivate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnActivate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseEvent(Result)^ := PForm(Params^[0])^.OnClose;
end;

procedure _LapeForm_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

procedure _LapeForm_OnCloseQuery_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseQueryEvent(Result)^ := PForm(Params^[0])^.OnCloseQuery;
end;

procedure _LapeForm_OnCloseQuery_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnCloseQuery := PCloseQueryEvent(Params^[1])^;
end;

procedure _LapeForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnCreate;
end;

procedure _LapeForm_OnCreate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDeactivate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDeactivate;
end;

procedure _LapeForm_OnDeactivate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDeactivate := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDestroy;
end;

procedure _LapeForm_OnDestroy_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnDropFiles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDropFilesEvent(Result)^ := PForm(Params^[0])^.OnDropFiles;
end;

procedure _LapeForm_OnDropFiles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDropFiles := PDropFilesEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnMouseLeave;
end;

procedure _LapeForm_OnMouseLeave_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnMouseEnter;
end;

procedure _LapeForm_OnMouseEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseWheel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseWheelEvent(Result)^ := PForm(Params^[0])^.OnMouseWheel;
end;

procedure _LapeForm_OnMouseWheel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseWheel := PMouseWheelEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseWheelUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseWheelUpDownEvent(Result)^ := PForm(Params^[0])^.OnMouseWheelUp;
end;

procedure _LapeForm_OnMouseWheelUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseWheelUp := PMouseWheelUpDownEvent(Params^[1])^;
end;

procedure _LapeForm_OnMouseWheelDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMouseWheelUpDownEvent(Result)^ := PForm(Params^[0])^.OnMouseWheelDown;
end;

procedure _LapeForm_OnMouseWheelDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseWheelDown := PMouseWheelUpDownEvent(Params^[1])^;
end;

procedure _LapeForm_OnWindowStateChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnWindowStateChange;
end;

procedure _LapeForm_OnWindowStateChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnWindowStateChange := PNotifyEvent(Params^[1])^;
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
  PString(Result)^ := PForm(Params^[0])^.Caption;
end;

procedure _LapeForm_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Caption := PString(Params^[1])^;
end;

procedure _LapeCustomForm_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPosition(Result)^ := PCustomForm(Params^[0])^.Position;
end;

procedure _LapeCustomForm_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Position := PPosition(Params^[1])^;
end;

procedure _LapeCustomForm_AutoScroll_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AutoScroll;
end;

procedure _LapeCustomForm_AutoScroll_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AutoScroll := PBoolean(Params^[1])^;
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

procedure _LapeCommonDialog_Execute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.Execute();
end;

procedure _LapeCommonDialog_UserChoice_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.UserChoice;
end;

procedure _LapeCommonDialog_UserChoice_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.UserChoice := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Close(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Close();
end;

procedure _LapeCommonDialog_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnClose;
end;

procedure _LapeCommonDialog_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnClose := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_OnCanClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseQueryEvent(Result)^ := PCommonDialog(Params^[0])^.OnCanClose;
end;

procedure _LapeCommonDialog_OnCanClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnCanClose := PCloseQueryEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnShow;
end;

procedure _LapeCommonDialog_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Width;
end;

procedure _LapeCommonDialog_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Height;
end;

procedure _LapeCommonDialog_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Height := Pinteger(Params^[1])^;
end;

procedure _LapeFileDialog_Execute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PFileDialog(Params^[0])^.Execute();
end;

procedure _LapeFileDialog_Files_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.Files;
end;

procedure _LapeFileDialog_HistoryList_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.HistoryList;
end;

procedure _LapeFileDialog_HistoryList_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.HistoryList := PStrings(Params^[1])^;
end;

procedure _LapeFileDialog_DefaultExt_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.DefaultExt;
end;

procedure _LapeFileDialog_DefaultExt_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.DefaultExt := PString(Params^[1])^;
end;

procedure _LapeFileDialog_FileName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.FileName;
end;

procedure _LapeFileDialog_FileName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.FileName := PString(Params^[1])^;
end;

procedure _LapeFileDialog_Filter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.Filter;
end;

procedure _LapeFileDialog_Filter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.Filter := PString(Params^[1])^;
end;

procedure _LapeFileDialog_FilterIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFileDialog(Params^[0])^.FilterIndex;
end;

procedure _LapeFileDialog_FilterIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.FilterIndex := PInteger(Params^[1])^;
end;

procedure _LapeFileDialog_InitialDir_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.InitialDir;
end;

procedure _LapeFileDialog_InitialDir_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.InitialDir := PString(Params^[1])^;
end;

procedure _LapeFileDialog_OnHelpClicked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnHelpClicked;
end;

procedure _LapeFileDialog_OnHelpClicked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.OnHelpClicked := PNotifyEvent(Params^[1])^;
end;

procedure _LapeFileDialog_OnTypeChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnTypeChange;
end;

procedure _LapeFileDialog_OnTypeChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.OnTypeChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeFileDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^ := TFileDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeFileDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.Free();
end;

procedure _LapeOpenDialog_IntfSetOption(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.IntfSetOption(POpenOption(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeOpenDialog_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenOptions(Result)^ := POpenDialog(Params^[0])^.Options;
end;

procedure _LapeOpenDialog_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.Options := POpenOptions(Params^[1])^;
end;

procedure _LapeOpenDialog_OnFolderChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnFolderChange;
end;

procedure _LapeOpenDialog_OnFolderChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.OnFolderChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeOpenDialog_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnSelectionChange;
end;

procedure _LapeOpenDialog_OnSelectionChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.OnSelectionChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeOpenDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^ := TOpenDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeOpenDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.Free();
end;

procedure _LapeColorDialog_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColor(Result)^ := PColorDialog(Params^[0])^.Color;
end;

procedure _LapeColorDialog_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.Color := PColor(Params^[1])^;
end;

procedure _LapeColorDialog_CustomColors_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PColorDialog(Params^[0])^.CustomColors;
end;

procedure _LapeColorDialog_CustomColors_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.CustomColors := PStrings(Params^[1])^;
end;

procedure _LapeColorDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^ := TColorDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeColorDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.Free();
end;

procedure ImportLCLForm(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(stDefault, stAlways, stNever)', 'TShowInTaskbar');
    addGlobalType('(bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin)', 'TFormBorderStyle');
    addGlobalType('(caNone, caHide, caFree, caMinimize)', 'TCloseAction');
    addGlobalType('(poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter, poWorkAreaCenter)', 'TPosition');
    addGlobalType('(biSystemMenu, biMinimize, biMaximize, biHelp)', 'TBorderIcon');
    addGlobalType('set of TBorderIcon', 'TBorderIcons');

    addCallbackType('TDropFilesEvent = procedure(Sender: TObject; const FileNames: TStringArray) of object');
    addCallbackType('TCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object');
    addCallbackType('TCloseQueryEvent = procedure(Sender: TObject; var CanClose: Boolean) of object');

    addClass('TSizeConstraints');
    addClassVar('TSizeConstraints', 'Control', 'TControl', @_LapeSizeConstraints_Control_Read);
    addClassVar('TSizeConstraints', 'OnChange', 'TNotifyEvent', @_LapeSizeConstraints_OnChange_Read, @_LapeSizeConstraints_OnChange_Write);
    addClassVar('TSizeConstraints', 'MaxHeight', 'Integer', @_LapeSizeConstraints_MaxHeight_Read, @_LapeSizeConstraints_MaxHeight_Write);
    addClassVar('TSizeConstraints', 'MaxWidth', 'Integer', @_LapeSizeConstraints_MaxWidth_Read, @_LapeSizeConstraints_MaxWidth_Write);
    addClassVar('TSizeConstraints', 'MinHeight', 'Integer', @_LapeSizeConstraints_MinHeight_Read, @_LapeSizeConstraints_MinHeight_Write);
    addClassVar('TSizeConstraints', 'MinWidth', 'Integer', @_LapeSizeConstraints_MinWidth_Read, @_LapeSizeConstraints_MinWidth_Write);

    addClass('TCustomForm', 'TScrollingWinControl');
    addGlobalFunc('procedure TCustomForm.Init(AOwner: TComponent); override', @_LapeCustomForm_Init);
    addGlobalFunc('procedure TCustomForm.InitNew(AOwner: TComponent; Num: Integer)', @_LapeCustomForm_CreateNew);
    addGlobalFunc('procedure TCustomForm.Close;', @_LapeCustomForm_Close);
    addGlobalFunc('function TCustomForm.CloseQuery: Boolean;', @_LapeCustomForm_CloseQuery);
    addGlobalFunc('procedure TCustomForm.DefocusControl(Control: TWinControl; Removing: Boolean);', @_LapeCustomForm_DefocusControl);
    addGlobalFunc('procedure TCustomForm.EnsureVisible(AMoveToTop: Boolean);', @_LapeCustomForm_EnsureVisible);
    addGlobalFunc('procedure TCustomForm.FocusControl(WinControl: TWinControl);', @_LapeCustomForm_FocusControl);
    addGlobalFunc('function TCustomForm.GetFormImage: TBitmap;', @_LapeCustomForm_GetFormImage);
    addGlobalFunc('function TCustomForm.SetFocusedControl(Control: TWinControl): Boolean;', @_LapeCustomForm_SetFocusedControl);
    addGlobalFunc('procedure TCustomForm.SetRestoredBounds(ALeft, ATop, AWidth, AHeight: Integer);', @_LapeCustomForm_SetRestoredBounds);
    addGlobalFunc('function TCustomForm.ShowModal: Integer;', @_LapeCustomForm_ShowModal);
    addGlobalFunc('procedure TCustomForm.ShowOnTop;', @_LapeCustomForm_ShowOnTop);
    addClassVar('TCustomForm', 'BorderStyle', 'TFormBorderStyle', @_LapeCustomForm_Read_BorderStyle, @_LapeCustomForm_Write_BorderStyle);
    addClassVar('TCustomForm', 'BorderIcons', 'TBorderIcons', @_LapeCustomForm_Read_BorderIcons, @_LapeCustomForm_Write_BorderIcons);
    addClassVar('TCustomForm', 'Active', 'Boolean', @_LapeCustomForm_Active_Read);
    addClassVar('TCustomForm', 'ActiveControl', 'TWinControl', @_LapeCustomForm_ActiveControl_Read, @_LapeCustomForm_ActiveControl_Write);
    addClassVar('TCustomForm', 'ActiveDefaultControl', 'TControl', @_LapeCustomForm_ActiveDefaultControl_Read, @_LapeCustomForm_ActiveDefaultControl_Write);
    addClassVar('TCustomForm', 'AllowDropFiles', 'Boolean', @_LapeCustomForm_AllowDropFiles_Read, @_LapeCustomForm_AllowDropFiles_Write);
    addClassVar('TCustomForm', 'AlphaBlend', 'Boolean', @_LapeCustomForm_AlphaBlend_Read, @_LapeCustomForm_AlphaBlend_Write);
    addClassVar('TCustomForm', 'AlphaBlendValue', 'Byte', @_LapeCustomForm_AlphaBlendValue_Read, @_LapeCustomForm_AlphaBlendValue_Write);
    addClassVar('TCustomForm', 'DefaultControl', 'TControl', @_LapeCustomForm_DefaultControl_Read, @_LapeCustomForm_DefaultControl_Write);
    addClassVar('TCustomForm', 'KeyPreview', 'Boolean', @_LapeCustomForm_KeyPreview_Read, @_LapeCustomForm_KeyPreview_Write);
    addClassVar('TCustomForm', 'PopupParent', 'TCustomForm', @_LapeCustomForm_PopupParent_Read, @_LapeCustomForm_PopupParent_Write);
    addClassVar('TCustomForm', 'PixelsPerInch', 'Integer', @_LapeCustomForm_PixelsPerInch_Read, @_LapeCustomForm_PixelsPerInch_Write);
    addClassVar('TCustomForm', 'RestoredLeft', 'Integer', @_LapeCustomForm_RestoredLeft_Read);
    addClassVar('TCustomForm', 'RestoredTop', 'Integer', @_LapeCustomForm_RestoredTop_Read);
    addClassVar('TCustomForm', 'RestoredWidth', 'Integer', @_LapeCustomForm_RestoredWidth_Read);
    addClassVar('TCustomForm', 'RestoredHeight', 'Integer', @_LapeCustomForm_RestoredHeight_Read);
    addClassVar('TCustomForm', 'Constraints', 'TSizeConstraints', @_LapeCustomForm_Constraints_Read, @_LapeCustomForm_Constraints_Write);
    addClassVar('TCustomForm', 'ShowInTaskBar', 'TShowInTaskBar', @_LapeCustomForm_ShowInTaskBar_Read, @_LapeCustomForm_ShowInTaskBar_Write);
    addClassVar('TCustomForm', 'Position', 'TPosition', @_LapeCustomForm_Position_Read, @_LapeCustomForm_Position_Write);
    addClassVar('TCustomForm', 'AutoScroll', 'Boolean', @_LapeCustomForm_AutoScroll_Read, @_LapeCustomForm_AutoScroll_Write);

    addClass('TForm', 'TCustomForm');
    addGlobalFunc('procedure TForm.Init(TheOwner: TComponent); override', @_LapeForm_Init);

    addClassVar('TForm', 'OnActivate', 'TNotifyEvent', @_LapeForm_OnActivate_Read, @_LapeForm_OnActivate_Write);
    addClassVar('TForm', 'OnClose', 'TCloseEvent', @_LapeForm_OnClose_Read, @_LapeForm_OnClose_Write);
    addClassVar('TForm', 'OnCloseQuery', 'TCloseQueryEvent', @_LapeForm_OnCloseQuery_Read, @_LapeForm_OnCloseQuery_Write);
    addClassVar('TForm', 'OnCreate', 'TNotifyEvent', @_LapeForm_OnCreate_Read, @_LapeForm_OnCreate_Write);
    addClassVar('TForm', 'OnDblClick', 'TNotifyEvent', @_LapeForm_OnDblClick_Read, @_LapeForm_OnDblClick_Write);
    addClassVar('TForm', 'OnDeactivate', 'TNotifyEvent', @_LapeForm_OnDeactivate_Read, @_LapeForm_OnDeactivate_Write);
    addClassVar('TForm', 'OnDestroy', 'TNotifyEvent', @_LapeForm_OnDestroy_Read, @_LapeForm_OnDestroy_Write);
    addClassVar('TForm', 'OnDropFiles', 'TDropFilesEvent', @_LapeForm_OnDropFiles_Read, @_LapeForm_OnDropFiles_Write);
    addClassVar('TForm', 'OnHide', 'TNotifyEvent', @_LapeForm_OnHide_Read, @_LapeForm_OnHide_Write);
    addClassVar('TForm', 'OnMouseDown', 'TMouseEvent', @_LapeForm_OnMouseDown_Read, @_LapeForm_OnMouseDown_Write);
    addClassVar('TForm', 'OnMouseEnter', 'TNotifyEvent', @_LapeForm_OnMouseEnter_Read, @_LapeForm_OnMouseEnter_Write);
    addClassVar('TForm', 'OnMouseLeave', 'TNotifyEvent', @_LapeForm_OnMouseLeave_Read, @_LapeForm_OnMouseLeave_Write);
    addClassVar('TForm', 'OnMouseMove', 'TMouseMoveEvent', @_LapeForm_OnMouseMove_Read, @_LapeForm_OnMouseMove_Write);
    addClassVar('TForm', 'OnMouseUp', 'TMouseEvent', @_LapeForm_OnMouseUp_Read, @_LapeForm_OnMouseUp_Write);
    addClassVar('TForm', 'OnMouseWheel', 'TMouseWheelEvent', @_LapeForm_OnMouseWheel_Read, @_LapeForm_OnMouseWheel_Write);
    addClassVar('TForm', 'OnMouseWheelDown', 'TMouseWheelUpDownEvent', @_LapeForm_OnMouseWheelDown_Read, @_LapeForm_OnMouseWheelDown_Write);
    addClassVar('TForm', 'OnMouseWheelUp', 'TMouseWheelUpDownEvent', @_LapeForm_OnMouseWheelUp_Read, @_LapeForm_OnMouseWheelUp_Write);
    addClassVar('TForm', 'OnShow', 'TNotifyEvent', @_LapeForm_OnShow_Read, @_LapeForm_OnShow_Write);
    addClassVar('TForm', 'OnWindowStateChange', 'TNotifyEvent', @_LapeForm_OnWindowStateChange_Read, @_LapeForm_OnWindowStateChange_Write);

    addClass('TScrollBox', 'TScrollingWinControl');
    addGlobalFunc('procedure TScrollBox.Init(AOwner: TComponent); override', @_LapeScrollBox_Init);

    addGlobalType('(ofReadOnly, ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir, ofShowHelp, ofNoValidate, ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofCreatePrompt, ofShareAware, ofNoReadOnlyReturn, ofNoTestFileCreate, ofNoNetworkButton, ofNoLongNames, ofOldStyleDialog, ofNoDereferenceLinks, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent, ofForceShowHidden, ofViewDetail, ofAutoPreview)', 'TOpenOption');
    addGlobalType('set of TOpenOption', 'TOpenOptions');

    addClass('TCommonDialog', 'TComponent');
    addGlobalFunc('function TCommonDialog.Execute: Boolean;', @_LapeCommonDialog_Execute);
    addClassVar('TCommonDialog', 'UserChoice', 'Integer', @_LapeCommonDialog_UserChoice_Read, @_LapeCommonDialog_UserChoice_Write);
    addGlobalFunc('procedure TCommonDialog.Close;', @_LapeCommonDialog_Close);
    addClassVar('TCommonDialog', 'OnClose', 'TNotifyEvent', @_LapeCommonDialog_OnClose_Read, @_LapeCommonDialog_OnClose_Write);
    addClassVar('TCommonDialog', 'OnCanClose', 'TCloseQueryEvent', @_LapeCommonDialog_OnCanClose_Read, @_LapeCommonDialog_OnCanClose_Write);
    addClassVar('TCommonDialog', 'OnShow', 'TNotifyEvent', @_LapeCommonDialog_OnShow_Read, @_LapeCommonDialog_OnShow_Write);
    addClassVar('TCommonDialog', 'Width', 'Integer', @_LapeCommonDialog_Width_Read, @_LapeCommonDialog_Width_Write);
    addClassVar('TCommonDialog', 'Height', 'Integer', @_LapeCommonDialog_Height_Read, @_LapeCommonDialog_Height_Write);

    AddClass('TFileDialog', 'TCommonDialog');
    addClassVar('TFileDialog', 'Files', 'TStrings', @_LapeFileDialog_Files_Read, nil);
    addClassVar('TFileDialog', 'HistoryList', 'TStrings', @_LapeFileDialog_HistoryList_Read, @_LapeFileDialog_HistoryList_Write);
    addClassVar('TFileDialog', 'DefaultExt', 'String', @_LapeFileDialog_DefaultExt_Read, @_LapeFileDialog_DefaultExt_Write);
    addClassVar('TFileDialog', 'FileName', 'String', @_LapeFileDialog_FileName_Read, @_LapeFileDialog_FileName_Write);
    addClassVar('TFileDialog', 'Filter', 'String', @_LapeFileDialog_Filter_Read, @_LapeFileDialog_Filter_Write);
    addClassVar('TFileDialog', 'FilterIndex', 'Integer', @_LapeFileDialog_FilterIndex_Read, @_LapeFileDialog_FilterIndex_Write);
    addClassVar('TFileDialog', 'InitialDir', 'String', @_LapeFileDialog_InitialDir_Read, @_LapeFileDialog_InitialDir_Write);
    addClassVar('TFileDialog', 'OnHelpClicked', 'TNotifyEvent', @_LapeFileDialog_OnHelpClicked_Read, @_LapeFileDialog_OnHelpClicked_Write);
    addClassVar('TFileDialog', 'OnTypeChange', 'TNotifyEvent', @_LapeFileDialog_OnTypeChange_Read, @_LapeFileDialog_OnTypeChange_Write);
    addGlobalFunc('procedure TFileDialog.Init(AOwner: TComponent); override', @_LapeFileDialog_Init);

    addClass('TOpenDialog', 'TFileDialog');
    addClassVar('TOpenDialog', 'Options', 'TOpenOptions', @_LapeOpenDialog_Options_Read, @_LapeOpenDialog_Options_Write);
    addClassVar('TOpenDialog', 'OnFolderChange', 'TNotifyEvent', @_LapeOpenDialog_OnFolderChange_Read, @_LapeOpenDialog_OnFolderChange_Write);
    addClassVar('TOpenDialog', 'OnSelectionChange', 'TNotifyEvent', @_LapeOpenDialog_OnSelectionChange_Read, @_LapeOpenDialog_OnSelectionChange_Write);
    addGlobalFunc('procedure TOpenDialog.Init(AOwner: TComponent); override', @_LapeOpenDialog_Init);

    addClass('TColorDialog', 'TCommonDialog');
    addClassVar('TColorDialog', 'Color', 'TColor', @_LapeColorDialog_Color_Read, @_LapeColorDialog_Color_Write);
    addClassVar('TColorDialog', 'CustomColors', 'TStrings', @_LapeColorDialog_CustomColors_Read, @_LapeColorDialog_CustomColors_Write);
    addGlobalFunc('procedure TColorDialog.Init(AOwner: TComponent); override', @_LapeColorDialog_Init);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportLCLForm);

end.

