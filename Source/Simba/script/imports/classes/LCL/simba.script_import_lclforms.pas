unit simba.script_import_lclforms;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLForms(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  forms, graphics, controls;

type
  PCloseQueryEvent = ^TCloseQueryEvent;
  PFormBorderIcons = ^TBorderIcons;
  PSizeConstraints = ^TSizeConstraints;
  PConstraintSize = ^TConstraintSize;

  PCustomForm = ^TCustomForm;
  PForm = ^TForm;
  PCloseAction = ^TCloseAction;
  PCloseEvent = ^TCloseEvent;
  PPosition = ^TPosition;
  PScrollBox = ^TScrollBox;
  PFormBorderStyle = ^TFormBorderStyle;

  PCanvas = ^TCanvas;
  PControl = ^TControl;
  PNotifyEvent = ^TNotifyEvent;
  PComponent = ^TComponent;
  PBitmap = ^TBitmap;
  PWinControl = ^TWinControl;
  PObject = ^TObject;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;
  PShowInTaskBar = ^TShowInTaskbar;

//constructor Create(AControl: TControl); virtual;
procedure Lape_TSizeConstraints_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^ := TSizeConstraints.Create(PControl(Params^[1])^);
end;

//procedure UpdateInterfaceConstraints; virtual;
procedure Lape_TSizeConstraints_UpdateInterfaceConstraints(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.UpdateInterfaceConstraints();
end;

//procedure SetInterfaceConstraints(MinW, MinH, MaxW, MaxH: integer); virtual;
procedure Lape_TSizeConstraints_SetInterfaceConstraints(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.SetInterfaceConstraints(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function EffectiveMinWidth: integer; virtual;
procedure Lape_TSizeConstraints_EffectiveMinWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMinWidth();
end;

//function EffectiveMinHeight: integer; virtual;
procedure Lape_TSizeConstraints_EffectiveMinHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMinHeight();
end;

//function EffectiveMaxWidth: integer; virtual;
procedure Lape_TSizeConstraints_EffectiveMaxWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMaxWidth();
end;

//function EffectiveMaxHeight: integer; virtual;
procedure Lape_TSizeConstraints_EffectiveMaxHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMaxHeight();
end;

//function MinMaxWidth(Width: integer): integer;
procedure Lape_TSizeConstraints_MinMaxWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinMaxWidth(Pinteger(Params^[1])^);
end;

//function MinMaxHeight(Height: integer): integer;
procedure Lape_TSizeConstraints_MinMaxHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinMaxHeight(Pinteger(Params^[1])^);
end;

//Read: property MaxInterfaceHeight: integer read FMaxInterfaceHeight;
procedure Lape_TSizeConstraints_MaxInterfaceHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MaxInterfaceHeight;
end;

//Read: property MaxInterfaceWidth: integer read FMaxInterfaceWidth;
procedure Lape_TSizeConstraints_MaxInterfaceWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MaxInterfaceWidth;
end;

//Read: property MinInterfaceHeight: integer read FMinInterfaceHeight;
procedure Lape_TSizeConstraints_MinInterfaceHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinInterfaceHeight;
end;

//Read: property MinInterfaceWidth: integer read FMinInterfaceWidth;
procedure Lape_TSizeConstraints_MinInterfaceWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinInterfaceWidth;
end;

//Read: property Control: TControl read FControl;
procedure Lape_TSizeConstraints_Control_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PSizeConstraints(Params^[0])^.Control;
end;

//Read: property Options: TSizeConstraintsOptions read FOptions write SetOptions default [];
procedure Lape_TSizeConstraints_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  //PSizeConstraintsOptions(Result)^ := PSizeConstraints(Params^[0])^.Options;
end;

//Write: property Options: TSizeConstraintsOptions read FOptions write SetOptions default [];
procedure Lape_TSizeConstraints_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 // PSizeConstraints(Params^[0])^.Options := PSizeConstraintsOptions(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure Lape_TSizeConstraints_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSizeConstraints(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure Lape_TSizeConstraints_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property MaxHeight: TConstraintSize read FMaxHeight write SetMaxHeight default 0;
procedure Lape_TSizeConstraints_MaxHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PConstraintSize(Result)^ := PSizeConstraints(Params^[0])^.MaxHeight;
end;

//Write: property MaxHeight: TConstraintSize read FMaxHeight write SetMaxHeight default 0;
procedure Lape_TSizeConstraints_MaxHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MaxHeight := PConstraintSize(Params^[1])^;
end;

//Read: property MaxWidth: TConstraintSize read FMaxWidth write SetMaxWidth default 0;
procedure Lape_TSizeConstraints_MaxWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MaxWidth;
end;

//Write: property MaxWidth: TConstraintSize read FMaxWidth write SetMaxWidth default 0;
procedure Lape_TSizeConstraints_MaxWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MaxWidth := PInteger(Params^[1])^;
end;

//Read: property MinHeight: TConstraintSize read FMinHeight write SetMinHeight default 0;
procedure Lape_TSizeConstraints_MinHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinHeight;
end;

//Write: property MinHeight: TConstraintSize read FMinHeight write SetMinHeight default 0;
procedure Lape_TSizeConstraints_MinHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MinHeight := PInteger(Params^[1])^;
end;

//Read: property MinWidth: TConstraintSize read FMinWidth write SetMinWidth default 0;
procedure Lape_TSizeConstraints_MinWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinWidth;
end;

//Write: property MinWidth: TConstraintSize read FMinWidth write SetMinWidth default 0;
procedure Lape_TSizeConstraints_MinWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.MinWidth := PConstraintSize(Params^[1])^;
end;

//procedure Free();
procedure Lape_TSizeConstraints_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Params^[0])^.Free();
end;

procedure Lape_Import_TSizeConstraints(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
 with Compiler do
 begin
   addClass('TSizeConstraints', 'TPersistent');

   addGlobalFunc('procedure TSizeConstraints.Init(AControl: TControl);', @Lape_TSizeConstraints_Init);
   addGlobalFunc('procedure TSizeConstraints.UpdateInterfaceConstraints(); constref;', @Lape_TSizeConstraints_UpdateInterfaceConstraints);
   addGlobalFunc('procedure TSizeConstraints.SetInterfaceConstraints(MinW, MinH, MaxW, MaxH: integer); constref;', @Lape_TSizeConstraints_SetInterfaceConstraints);
   addGlobalFunc('function TSizeConstraints.EffectiveMinWidth(): integer; constref;', @Lape_TSizeConstraints_EffectiveMinWidth);
   addGlobalFunc('function TSizeConstraints.EffectiveMinHeight(): integer; constref;', @Lape_TSizeConstraints_EffectiveMinHeight);
   addGlobalFunc('function TSizeConstraints.EffectiveMaxWidth(): integer; constref;', @Lape_TSizeConstraints_EffectiveMaxWidth);
   addGlobalFunc('function TSizeConstraints.EffectiveMaxHeight(): integer; constref;', @Lape_TSizeConstraints_EffectiveMaxHeight);
   addGlobalFunc('function TSizeConstraints.MinMaxWidth(Width: integer): integer; constref;', @Lape_TSizeConstraints_MinMaxWidth);
   addGlobalFunc('function TSizeConstraints.MinMaxHeight(Height: integer): integer; constref;', @Lape_TSizeConstraints_MinMaxHeight);
   addClassVar('TSizeConstraints', 'MaxInterfaceHeight', 'integer', @Lape_TSizeConstraints_MaxInterfaceHeight_Read, nil);
   addClassVar('TSizeConstraints', 'MaxInterfaceWidth', 'integer', @Lape_TSizeConstraints_MaxInterfaceWidth_Read, nil);
   addClassVar('TSizeConstraints', 'MinInterfaceHeight', 'integer', @Lape_TSizeConstraints_MinInterfaceHeight_Read, nil);
   addClassVar('TSizeConstraints', 'MinInterfaceWidth', 'integer', @Lape_TSizeConstraints_MinInterfaceWidth_Read, nil);
   addClassVar( 'TSizeConstraints', 'Control', 'TControl', @Lape_TSizeConstraints_Control_Read, nil);
  // addClassVar('TSizeConstraints', 'Options', 'TSizeConstraintsOptions', @Lape_TSizeConstraints_Options_Read, @Lape_TSizeConstraints_Options_Write);
   addClassVar('TSizeConstraints', 'OnChange', 'TNotifyEvent', @Lape_TSizeConstraints_OnChange_Read, @Lape_TSizeConstraints_OnChange_Write);
   addClassVar('TSizeConstraints', 'MaxHeight', 'Integer', @Lape_TSizeConstraints_MaxHeight_Read, @Lape_TSizeConstraints_MaxHeight_Write);
   addClassVar('TSizeConstraints', 'MaxWidth', 'Integer', @Lape_TSizeConstraints_MaxWidth_Read, @Lape_TSizeConstraints_MaxWidth_Write);
   addClassVar('TSizeConstraints', 'MinHeight', 'Integer', @Lape_TSizeConstraints_MinHeight_Read, @Lape_TSizeConstraints_MinHeight_Write);
   addClassVar('TSizeConstraints', 'MinWidth', 'Integer', @Lape_TSizeConstraints_MinWidth_Read, @Lape_TSizeConstraints_MinWidth_Write);
  // addGlobalFunc('procedure TSizeConstraints.Free(); constref;', @Lape_TSizeConstraints_Free);
 end;
end;

 {TCustomForm}
 //constructor Create(AOwner: TComponent);
procedure Lape_TCustomForm_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^ := TCustomForm.Create(PComponent(Params^[1])^);
end;

//constructor CreateNew(AOwner: TComponent; Num: Integer);
procedure Lape_TCustomForm_CreateNew(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^ := TCustomForm.CreateNew(PComponent(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure AfterConstruction;
procedure Lape_TCustomForm_AfterConstruction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AfterConstruction();
end;

//procedure BeforeDestruction;
procedure Lape_TCustomForm_BeforeDestruction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.BeforeDestruction();
end;

//procedure Close;
procedure Lape_TCustomForm_Close(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Close();
end;

//function CloseQuery: boolean;
procedure Lape_TCustomForm_CloseQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.CloseQuery();
end;

//procedure DefocusControl(Control: TWinControl; Removing: Boolean);
procedure Lape_TCustomForm_DefocusControl(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.DefocusControl(PWinControl(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure DestroyWnd;
procedure Lape_TCustomForm_DestroyWnd(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.DestroyWnd();
end;

//procedure EnsureVisible(AMoveToTop: Boolean);
procedure Lape_TCustomForm_EnsureVisible(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.EnsureVisible(PBoolean(Params^[1])^);
end;

//procedure FocusControl(WinControl: TWinControl);
procedure Lape_TCustomForm_FocusControl(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.FocusControl(PWinControl(Params^[1])^);
end;

//function FormIsUpdating: boolean;
procedure Lape_TCustomForm_FormIsUpdating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.FormIsUpdating();
end;

//function GetFormImage: TBitmap;
procedure Lape_TCustomForm_GetFormImage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PCustomForm(Params^[0])^.GetFormImage();
end;

//procedure Hide;
procedure Lape_TCustomForm_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Hide();
end;

//procedure IntfDropFiles(const FileNames: TStringArray);
procedure Lape_TCustomForm_IntfDropFiles(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.IntfDropFiles(PStringArray(Params^[1])^);
end;

//procedure IntfHelp(AComponent: TComponent);
procedure Lape_TCustomForm_IntfHelp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.IntfHelp(PComponent(Params^[1])^);
end;

//function AutoSizeDelayedHandle: Boolean;
procedure Lape_TCustomForm_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AutoSizeDelayedHandle();
end;

//procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw,WithThemeSpace: boolean );
procedure Lape_TCustomForm_GetPreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure Release;
procedure Lape_TCustomForm_Release(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Release();
end;

//function CanFocus: Boolean;
procedure Lape_TCustomForm_CanFocus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.CanFocus();
end;

//procedure SetFocus;
procedure Lape_TCustomForm_SetFocus(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.SetFocus();
end;

//function SetFocusedControl(Control: TWinControl): Boolean ;
procedure Lape_TCustomForm_SetFocusedControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.SetFocusedControl(PWinControl(Params^[1])^);
end;

//procedure SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer);
procedure Lape_TCustomForm_SetRestoredBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.SetRestoredBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure Show;
procedure Lape_TCustomForm_Show(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Show();
end;

//function ShowModal: Integer;
procedure Lape_TCustomForm_ShowModal(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomForm(Params^[0])^.ShowModal();
end;

//procedure ShowOnTop;
procedure Lape_TCustomForm_ShowOnTop(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ShowOnTop();
end;

//procedure RemoveAllHandlersOfObject(AnObject: TObject);
procedure Lape_TCustomForm_RemoveAllHandlersOfObject(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveAllHandlersOfObject(PObject(Params^[1])^);
end;

//procedure AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;AsFirst: Boolean);
procedure Lape_TCustomForm_AddHandlerFirstShow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AddHandlerFirstShow(PNotifyEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent);
procedure Lape_TCustomForm_RemoveHandlerFirstShow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveHandlerFirstShow(PNotifyEvent(Params^[1])^);
end;

//procedure AddHandlerClose(OnCloseHandler: TCloseEvent; AsFirst: Boolean);
procedure Lape_TCustomForm_AddHandlerClose(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AddHandlerClose(PCloseEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure RemoveHandlerClose(OnCloseHandler: TCloseEvent);
procedure Lape_TCustomForm_RemoveHandlerClose(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveHandlerClose(PCloseEvent(Params^[1])^);
end;

//procedure AddHandlerCreate(OnCreateHandler: TNotifyEvent; AsFirst: Boolean);
procedure Lape_TCustomForm_AddHandlerCreate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AddHandlerCreate(PNotifyEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure RemoveHandlerCreate(OnCreateHandler: TNotifyEvent);
procedure Lape_TCustomForm_RemoveHandlerCreate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.RemoveHandlerCreate(PNotifyEvent(Params^[1])^);
end;

//Read: property Active: Boolean read Active;
procedure Lape_TCustomForm_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.Active;
end;

//Read: property ActiveControl: TWinControl read ActiveControl write ActiveControl;
procedure Lape_TCustomForm_ActiveControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PCustomForm(Params^[0])^.ActiveControl;
end;

//Write: property ActiveControl: TWinControl read ActiveControl write ActiveControl;
procedure Lape_TCustomForm_ActiveControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ActiveControl := PWinControl(Params^[1])^;
end;

//Read: property ActiveDefaultControl: TControl read ActiveDefaultControl write ActiveDefaultControl;
procedure Lape_TCustomForm_ActiveDefaultControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.ActiveDefaultControl;
end;

//Write: property ActiveDefaultControl: TControl read ActiveDefaultControl write ActiveDefaultControl;
procedure Lape_TCustomForm_ActiveDefaultControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ActiveDefaultControl := PControl(Params^[1])^;
end;

//Read: property AllowDropFiles: Boolean read AllowDropFiles write AllowDropFiles ;
procedure Lape_TCustomForm_AllowDropFiles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AllowDropFiles;
end;

//Write: property AllowDropFiles: Boolean read AllowDropFiles write AllowDropFiles ;
procedure Lape_TCustomForm_AllowDropFiles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AllowDropFiles := PBoolean(Params^[1])^;
end;

//Read: property AlphaBlend: Boolean read AlphaBlend write AlphaBlend;
procedure Lape_TCustomForm_AlphaBlend_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AlphaBlend;
end;

//Write: property AlphaBlend: Boolean read AlphaBlend write AlphaBlend;
procedure Lape_TCustomForm_AlphaBlend_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AlphaBlend := PBoolean(Params^[1])^;
end;

//Read: property AlphaBlendValue: Byte read AlphaBlendValue write AlphaBlendValue;
procedure Lape_TCustomForm_AlphaBlendValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByte(Result)^ := PCustomForm(Params^[0])^.AlphaBlendValue;
end;

//Write: property AlphaBlendValue: Byte read AlphaBlendValue write AlphaBlendValue;
procedure Lape_TCustomForm_AlphaBlendValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.AlphaBlendValue := PByte(Params^[1])^;
end;

//Read: property CancelControl: TControl read CancelControl write CancelControl;
procedure Lape_TCustomForm_CancelControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.CancelControl;
end;

//Write: property CancelControl: TControl read CancelControl write CancelControl;
procedure Lape_TCustomForm_CancelControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.CancelControl := PControl(Params^[1])^;
end;

//Read: property DefaultControl: TControl read DefaultControl write DefaultControl;
procedure Lape_TCustomForm_DefaultControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.DefaultControl;
end;

//Write: property DefaultControl: TControl read DefaultControl write DefaultControl;
procedure Lape_TCustomForm_DefaultControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.DefaultControl := PControl(Params^[1])^;
end;

//Read: property KeyPreview: Boolean read KeyPreview write KeyPreview;
procedure Lape_TCustomForm_KeyPreview_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.KeyPreview;
end;

//Write: property KeyPreview: Boolean read KeyPreview write KeyPreview;
procedure Lape_TCustomForm_KeyPreview_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.KeyPreview := PBoolean(Params^[1])^;
end;

//Read: property PopupParent: TCustomForm read PopupParent write PopupParent;
procedure Lape_TCustomForm_PopupParent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Result)^ := PCustomForm(Params^[0])^.PopupParent;
end;

//Write: property PopupParent: TCustomForm read PopupParent write PopupParent;
procedure Lape_TCustomForm_PopupParent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.PopupParent := PCustomForm(Params^[1])^;
end;

//Read: property OnActivate: TNotifyEvent read OnActivate write OnActivate;
procedure Lape_TCustomForm_OnActivate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnActivate;
end;

//Write: property OnActivate: TNotifyEvent read OnActivate write OnActivate;
procedure Lape_TCustomForm_OnActivate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnActivate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure Lape_TCustomForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCloseEvent(Result)^ := PCustomForm(Params^[0])^.OnClose;
end;

//Write: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure Lape_TCustomForm_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

//Read: property OnCloseQuery : TCloseQueryEvent read OnCloseQuery write OnCloseQuery;
procedure Lape_TCustomForm_OnCloseQuery_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCloseQueryEvent(Result)^ := PCustomForm(Params^[0])^.OnCloseQuery;
end;

//Write: property OnCloseQuery : TCloseQueryEvent read OnCloseQuery write OnCloseQuery;
procedure Lape_TCustomForm_OnCloseQuery_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnCloseQuery := PCloseQueryEvent(Params^[1])^;
end;

//Read: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure Lape_TCustomForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnCreate;
end;

//Write: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure Lape_TCustomForm_OnCreate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDeactivate: TNotifyEvent read OnDeactivate write OnDeactivate;
procedure Lape_TCustomForm_OnDeactivate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnDeactivate;
end;

//Write: property OnDeactivate: TNotifyEvent read OnDeactivate write OnDeactivate;
procedure Lape_TCustomForm_OnDeactivate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnDeactivate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure Lape_TCustomForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnDestroy;
end;

//Write: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure Lape_TCustomForm_OnDestroy_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure Lape_TCustomForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure Lape_TCustomForm_OnHide_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;



//Read: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure Lape_TCustomForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure Lape_TCustomForm_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property OnWindowStateChange: TNotifyEvent read OnWindowStateChange write OnWindowStateChange;
procedure Lape_TCustomForm_OnWindowStateChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnWindowStateChange;
end;

//Write: property OnWindowStateChange: TNotifyEvent read OnWindowStateChange write OnWindowStateChange;
procedure Lape_TCustomForm_OnWindowStateChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.OnWindowStateChange := PNotifyEvent(Params^[1])^;
end;

//Read: property PixelsPerInch: Longint read PixelsPerInch write PixelsPerInch;
procedure Lape_TCustomForm_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PCustomForm(Params^[0])^.PixelsPerInch;
end;

//Write: property PixelsPerInch: Longint read PixelsPerInch write PixelsPerInch;
procedure Lape_TCustomForm_PixelsPerInch_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.PixelsPerInch := PLongint(Params^[1])^;
end;

//Read: property RestoredLeft: integer read RestoredLeft;
procedure Lape_TCustomForm_RestoredLeft_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredLeft;
end;

//Read: property RestoredTop: integer read RestoredTop;
procedure Lape_TCustomForm_RestoredTop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredTop;
end;

//Read: property RestoredWidth: integer read RestoredWidth;
procedure Lape_TCustomForm_RestoredWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredWidth;
end;

//Read: property RestoredHeight: integer read RestoredHeight;
procedure Lape_TCustomForm_RestoredHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredHeight;
end;

procedure Lape_TCustomForm_Write_BorderStyle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

procedure Lape_TCustomForm_Read_BorderStyle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFormBorderStyle(Result)^ := PCustomForm(Params^[0])^.BorderStyle;
end;

procedure Lape_TCustomForm_Write_BorderIcons(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.BorderIcons := PFormBorderIcons(Params^[1])^;
end;

procedure Lape_TCustomForm_Read_BorderIcons(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFormBorderIcons(Result)^ := PCustomForm(Params^[0])^.BorderIcons;
end;

procedure Lape_TCustomForm_Constraints_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeConstraints(Result)^ := PCustomForm(Params^[0])^.Constraints;
end;

procedure Lape_TCustomForm_Constraints_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Constraints := PSizeConstraints(Params^[1])^;
end;

procedure Lape_TCustomForm_ShowInTaskBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PShowInTaskBar(Result)^ := PCustomForm(Params^[0])^.ShowInTaskBar;
end;

procedure Lape_TCustomForm_ShowInTaskBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.ShowInTaskBar := PShowInTaskBar(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomForm_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomForm(Params^[0])^.Free;
end;

procedure Lape_Import_TCustomForm(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomForm', 'TScrollingWinControl');

    addGlobalType('(stDefault, stAlways, stNever)', 'TShowInTaskbar');

    addGlobalFunc('procedure TCustomForm.Init(AOwner: TComponent); override;', @Lape_TCustomForm_Init);
    addGlobalFunc('procedure TCustomForm.InitNew(AOwner: TComponent; Num: Integer);', @Lape_TCustomForm_CreateNew);
    addGlobalFunc('procedure TCustomForm.AfterConstruction(); constref;', @Lape_TCustomForm_AfterConstruction);
    //addGlobalFunc('procedure TCustomForm.BeforeDestruction(); constref;', @Lape_TCustomForm_BeforeDestruction);
    addGlobalFunc('procedure TCustomForm.Close(); constref;', @Lape_TCustomForm_Close);
    addGlobalFunc('function TCustomForm.CloseQuery(): boolean; constref;', @Lape_TCustomForm_CloseQuery);
    addGlobalFunc('procedure TCustomForm.DefocusControl(Control: TWinControl; Removing: Boolean); constref;', @Lape_TCustomForm_DefocusControl);
    addGlobalFunc('procedure TCustomForm.DestroyWnd(); constref;', @Lape_TCustomForm_DestroyWnd);
    addGlobalFunc('procedure TCustomForm.EnsureVisible(AMoveToTop: Boolean); constref;', @Lape_TCustomForm_EnsureVisible);
    addGlobalFunc('procedure TCustomForm.FocusControl(WinControl: TWinControl); constref;', @Lape_TCustomForm_FocusControl);
    //addGlobalFunc('function TCustomForm.FormIsUpdating(): boolean; constref;', @Lape_TCustomForm_FormIsUpdating);
    addGlobalFunc('function TCustomForm.GetFormImage(): TBitmap; constref;', @Lape_TCustomForm_GetFormImage);
    //addGlobalFunc('procedure TCustomForm.Hide(); constref;', @Lape_TCustomForm_Hide);
    addGlobalFunc('procedure TCustomForm.IntfDropFiles(const FileNames: TStringArray); constref;', @Lape_TCustomForm_IntfDropFiles);
    addGlobalFunc('procedure TCustomForm.IntfHelp(AComponent: TComponent); constref;', @Lape_TCustomForm_IntfHelp);
    //addGlobalFunc('function TCustomForm.AutoSizeDelayedHandle(): Boolean; constref;', @Lape_TCustomForm_AutoSizeDelayedHandle);
    //addGlobalFunc('procedure TCustomForm.GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw,WithThemeSpace: boolean ); constref;', @Lape_TCustomForm_GetPreferredSize);
    addGlobalFunc('procedure TCustomForm.Release(); constref;', @Lape_TCustomForm_Release);
    //addGlobalFunc('function TCustomForm.CanFocus(): Boolean; constref;', @Lape_TCustomForm_CanFocus);
    //addGlobalFunc('procedure TCustomForm.SetFocus(); constref;', @Lape_TCustomForm_SetFocus);
    addGlobalFunc('function TCustomForm.SetFocusedControl(Control: TWinControl): Boolean; constref;', @Lape_TCustomForm_SetFocusedControl);
    addGlobalFunc('procedure TCustomForm.SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer); constref;', @Lape_TCustomForm_SetRestoredBounds);
    //addGlobalFunc('procedure TCustomForm.Show(); constref;', @Lape_TCustomForm_Show);
    addGlobalFunc('function TCustomForm.ShowModal(): Integer; constref;', @Lape_TCustomForm_ShowModal);
    addGlobalFunc('procedure TCustomForm.ShowOnTop(); constref;', @Lape_TCustomForm_ShowOnTop);
    addGlobalFunc('procedure TCustomForm.RemoveAllHandlersOfObject(AnObject: TObject); constref;', @Lape_TCustomForm_RemoveAllHandlersOfObject);
    addGlobalFunc('procedure TCustomForm.AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;AsFirst: Boolean); constref;', @Lape_TCustomForm_AddHandlerFirstShow);
    addGlobalFunc('procedure TCustomForm.RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent); constref;', @Lape_TCustomForm_RemoveHandlerFirstShow);
    addGlobalFunc('procedure TCustomForm.AddHandlerClose(OnCloseHandler: TCloseEvent; AsFirst: Boolean); constref;', @Lape_TCustomForm_AddHandlerClose);
    addGlobalFunc('procedure TCustomForm.RemoveHandlerClose(OnCloseHandler: TCloseEvent); constref;', @Lape_TCustomForm_RemoveHandlerClose);
    addGlobalFunc('procedure TCustomForm.AddHandlerCreate(OnCreateHandler: TNotifyEvent; AsFirst: Boolean); constref;', @Lape_TCustomForm_AddHandlerCreate);
    addGlobalFunc('procedure TCustomForm.RemoveHandlerCreate(OnCreateHandler: TNotifyEvent); constref;', @Lape_TCustomForm_RemoveHandlerCreate);
    addClassVar('TCustomForm', 'BorderStyle', 'TFormBorderStyle', @Lape_TCustomForm_Read_BorderStyle);
    addClassVar('TCustomForm', 'BorderIcons', 'TBorderIcons', @Lape_TCustomForm_Read_BorderIcons, @Lape_TCustomForm_Write_BorderIcons);
    addClassVar('TCustomForm', 'Active', 'Boolean', @Lape_TCustomForm_Active_Read);
    addClassVar('TCustomForm', 'ActiveControl', 'TWinControl', @Lape_TCustomForm_ActiveControl_Read, @Lape_TCustomForm_ActiveControl_Write);
    addClassVar('TCustomForm', 'ActiveDefaultControl', 'TControl', @Lape_TCustomForm_ActiveDefaultControl_Read, @Lape_TCustomForm_ActiveDefaultControl_Write);
    addClassVar('TCustomForm', 'AllowDropFiles', 'Boolean', @Lape_TCustomForm_AllowDropFiles_Read, @Lape_TCustomForm_AllowDropFiles_Write);
    addClassVar('TCustomForm', 'AlphaBlend', 'Boolean', @Lape_TCustomForm_AlphaBlend_Read, @Lape_TCustomForm_AlphaBlend_Write);
    addClassVar('TCustomForm', 'AlphaBlendValue', 'Byte', @Lape_TCustomForm_AlphaBlendValue_Read, @Lape_TCustomForm_AlphaBlendValue_Write);
    addClassVar('TCustomForm', 'CancelControl', 'TControl', @Lape_TCustomForm_CancelControl_Read, @Lape_TCustomForm_CancelControl_Write);
    addClassVar('TCustomForm', 'DefaultControl', 'TControl', @Lape_TCustomForm_DefaultControl_Read, @Lape_TCustomForm_DefaultControl_Write);
    addClassVar('TCustomForm', 'KeyPreview', 'Boolean', @Lape_TCustomForm_KeyPreview_Read, @Lape_TCustomForm_KeyPreview_Write);
    addClassVar('TCustomForm', 'PopupParent', 'TCustomForm', @Lape_TCustomForm_PopupParent_Read, @Lape_TCustomForm_PopupParent_Write);
    addClassVar('TCustomForm', 'OnActivate', 'TNotifyEvent', @Lape_TCustomForm_OnActivate_Read, @Lape_TCustomForm_OnActivate_Write);
    addClassVar('TCustomForm', 'OnClose', 'TCloseEvent', @Lape_TCustomForm_OnClose_Read, @Lape_TCustomForm_OnClose_Write);
    addClassVar('TCustomForm', 'OnCloseQuery', 'TCloseQueryEvent', @Lape_TCustomForm_OnCloseQuery_Read, @Lape_TCustomForm_OnCloseQuery_Write);
    addClassVar('TCustomForm', 'OnCreate', 'TNotifyEvent', @Lape_TCustomForm_OnCreate_Read, @Lape_TCustomForm_OnCreate_Write);
    addClassVar('TCustomForm', 'OnDeactivate', 'TNotifyEvent', @Lape_TCustomForm_OnDeactivate_Read, @Lape_TCustomForm_OnDeactivate_Write);
    addClassVar('TCustomForm', 'OnDestroy', 'TNotifyEvent', @Lape_TCustomForm_OnDestroy_Read, @Lape_TCustomForm_OnDestroy_Write);
    addClassVar('TCustomForm', 'OnHide', 'TNotifyEvent', @Lape_TCustomForm_OnHide_Read, @Lape_TCustomForm_OnHide_Write);
    addClassVar('TCustomForm', 'OnShow', 'TNotifyEvent', @Lape_TCustomForm_OnShow_Read, @Lape_TCustomForm_OnShow_Write);
    addClassVar('TCustomForm', 'OnWindowStateChange', 'TNotifyEvent', @Lape_TCustomForm_OnWindowStateChange_Read, @Lape_TCustomForm_OnWindowStateChange_Write);
    addClassVar('TCustomForm', 'PixelsPerInch', 'Longint', @Lape_TCustomForm_PixelsPerInch_Read, @Lape_TCustomForm_PixelsPerInch_Write);
    addClassVar('TCustomForm', 'RestoredLeft', 'integer', @Lape_TCustomForm_RestoredLeft_Read);
    addClassVar('TCustomForm', 'RestoredTop', 'integer', @Lape_TCustomForm_RestoredTop_Read);
    addClassVar('TCustomForm', 'RestoredWidth', 'integer', @Lape_TCustomForm_RestoredWidth_Read);
    addClassVar('TCustomForm', 'RestoredHeight', 'integer', @Lape_TCustomForm_RestoredHeight_Read);
    addClassVar('TCustomForm', 'Constraints', 'TSizeConstraints', @Lape_TCustomForm_Constraints_Read, @Lape_TCustomForm_Constraints_Write);
    addClassVar('TCustomForm', 'ShowInTaskBar', 'TShowInTaskBar', @Lape_TCustomForm_ShowInTaskBar_Read, @Lape_TCustomForm_ShowInTaskBar_Write);
    //addGlobalFunc('procedure TCustomForm.Free(); constref;', @Lape_TCustomForm_Free);
  end;
end;
 {TForm}
//constructor Create(TheOwner: TComponent);
procedure Lape_TForm_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^ := TForm.Create(PComponent(Params^[1])^);
  PForm(Params^[0])^.ShowInTaskBar := stAlways;
end;

//procedure Cascade;
procedure Lape_TForm_Cascade(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Cascade();
end;

//procedure Next;
procedure Lape_TForm_Next(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Next();
end;

//procedure Previous;
procedure Lape_TForm_Previous(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Previous();
end;

//procedure Tile;
procedure Lape_TForm_Tile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Tile();
end;

//procedure Show;
procedure Lape_TForm_Show(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Show();
end;

//procedure Close;
procedure Lape_TForm_Close(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Close();
end;

//procedure Hide;
procedure Lape_TForm_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Hide();
end;

//Read: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure Lape_TForm_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientWidth;
end;

//Write: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure Lape_TForm_ClientWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

//Read: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure Lape_TForm_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientHeight;
end;

//Write: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure Lape_TForm_ClientHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

//Read: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure Lape_TForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCloseEvent(Result)^ := PForm(Params^[0])^.OnClose;
end;

//Write: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure Lape_TForm_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

//Read: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure Lape_TForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnCreate;
end;

//Write: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure Lape_TForm_OnCreate_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure Lape_TForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDestroy;
end;

//Write: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure Lape_TForm_OnDestroy_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure Lape_TForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure Lape_TForm_OnHide_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

//Read: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure Lape_TForm_OnPaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnPaint;
end;

//Write: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure Lape_TForm_OnPaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read OnShow write OnShow;
procedure Lape_TForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read OnShow write OnShow;
procedure Lape_TForm_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDblClick: TNotifyEvent read OnDblClick write OnDblClick;
procedure Lape_TForm_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDblClick;
end;

//Write: property OnDblClick: TNotifyEvent read OnDblClick write OnDblClick;
procedure Lape_TForm_OnDblClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

//Read: property OnEnter: TNotifyEvent read OnEnter write OnEnter;
procedure Lape_TForm_OnEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnEnter;
end;

//Write: property OnEnter: TNotifyEvent read OnEnter write OnEnter;
procedure Lape_TForm_OnEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

//Read: property OnExit: TNotifyEvent read FOnExit write OnExit;
procedure Lape_TForm_OnExit_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnExit;
end;

//Write: property OnExit: TNotifyEvent read FOnExit write OnExit;
procedure Lape_TForm_OnExit_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

//Read: property OnClick: TNotifyEvent read OnClick write OnClick;
procedure Lape_TForm_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnClick;
end;

//Write: property OnClick: TNotifyEvent read OnClick write OnClick;
procedure Lape_TForm_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

//Read: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure Lape_TForm_OnResize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnResize;
end;

//Write: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure Lape_TForm_OnResize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure Lape_TForm_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure Lape_TForm_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Font: TFont read Font write Font;
procedure Lape_TForm_Font_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFont(Result)^ := PForm(Params^[0])^.Font;
end;

//Write: property Font: TFont read Font write Font;
procedure Lape_TForm_Font_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure Lape_TForm_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure Lape_TForm_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//Read: property Canvas: TCanvas read Canvas write Canvas;
procedure Lape_TForm_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PForm(Params^[0])^.Canvas;
end;

//Write: property Canvas: TCanvas read Canvas write Canvas;
procedure Lape_TForm_Canvas_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

//Read: property Left: Integer read Left write Left;
procedure Lape_TForm_Left_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Left;
end;

//Write: property Left: Integer read Left write Left;
procedure Lape_TForm_Left_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Left := PInteger(Params^[1])^;
end;

//Read: property Height: Integer read Height write Height;
procedure Lape_TForm_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure Lape_TForm_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Top: Integer read Top write Top;
procedure Lape_TForm_Top_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Top;
end;

//Write: property Top: Integer read Top write Top;
procedure Lape_TForm_Top_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Top := PInteger(Params^[1])^;
end;

//Read: property Width: Integer read Width write Width;
procedure Lape_TForm_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PForm(Params^[0])^.Width;
end;

//Write: property Width: Integer read Width write Width;
procedure Lape_TForm_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//Read: property Caption: string read Caption write Caption;
procedure Lape_TForm_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PForm(Params^[0])^.Caption;
end;

//Write: property Caption: string read Caption write Caption;
procedure Lape_TForm_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

//Read: property Position: TPosition;
procedure Lape_TForm_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPosition(Result)^ := PForm(Params^[0])^.Position;
end;

//Write: property Position: TPosition
procedure Lape_TForm_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Position := PPosition(Params^[1])^;
end;

//procedure Free();
procedure Lape_TForm_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.Free();
end;

procedure Lape_TForm_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseMoveEvent(Result)^ := PForm(Params^[0])^.OnMouseMove;
end;

procedure Lape_TForm_OnMouseMove_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure Lape_TForm_OnMouseDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure Lape_TForm_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PForm(Params^[0])^.OnMouseDown;
end;

procedure Lape_TForm_OnMouseUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure Lape_TForm_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PForm(Params^[0])^.OnMouseUp;
end;

procedure Lape_TForm_OnKeyUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

procedure Lape_TForm_OnKeyUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PKeyEvent(Result)^ := PForm(Params^[0])^.OnKeyUp;
end;

procedure Lape_TForm_OnKeyDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure Lape_TForm_OnKeyDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PKeyEvent(Result)^ := PForm(Params^[0])^.OnKeyDown;
end;

procedure Lape_TForm_OnKeyPress_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PForm(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure Lape_TForm_OnKeyPress_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PKeyPressEvent(Result)^ := PForm(Params^[0])^.OnKeyPress;
end;

procedure Lape_Import_TForm(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TForm', 'TCustomForm');

    addGlobalFunc('procedure TForm.Init(TheOwner: TComponent); override;', @Lape_TForm_Init);
    addGlobalFunc('procedure TForm.Cascade(); constref;', @Lape_TForm_Cascade);
    addGlobalFunc('procedure TForm.Next(); constref;', @Lape_TForm_Next);
    addGlobalFunc('procedure TForm.Previous(); constref;', @Lape_TForm_Previous);
    addGlobalFunc('procedure TForm.Tile(); constref;', @Lape_TForm_Tile);
    //addGlobalFunc('procedure TForm.Show(); constref;', @Lape_TForm_Show);
    //addGlobalFunc('procedure TForm.Close(); constref;', @Lape_TForm_Close);
    //addGlobalFunc('procedure TForm.Hide(); constref;', @Lape_TForm_Hide);
    //addClassVar('TForm', 'ClientWidth', 'Integer', @Lape_TForm_ClientWidth_Read, @Lape_TForm_ClientWidth_Write);
    //addClassVar('TForm', 'ClientHeight', 'Integer', @Lape_TForm_ClientHeight_Read, @Lape_TForm_ClientHeight_Write);
    //addClassVar('TForm', 'OnClose', 'TCloseEvent', @Lape_TForm_OnClose_Read, @Lape_TForm_OnClose_Write);
    //addClassVar('TForm', 'OnCreate', 'TNotifyEvent', @Lape_TForm_OnCreate_Read, @Lape_TForm_OnCreate_Write);
    //addClassVar('TForm', 'OnDestroy', 'TNotifyEvent', @Lape_TForm_OnDestroy_Read, @Lape_TForm_OnDestroy_Write);
    //addClassVar('TForm', 'OnHide', 'TNotifyEvent', @Lape_TForm_OnHide_Read, @Lape_TForm_OnHide_Write);
    //addClassVar('TForm', 'OnPaint', 'TNotifyEvent', @Lape_TForm_OnPaint_Read, @Lape_TForm_OnPaint_Write);
    //addClassVar('TForm', 'OnShow', 'TNotifyEvent', @Lape_TForm_OnShow_Read, @Lape_TForm_OnShow_Write);
    addClassVar('TForm', 'OnDblClick', 'TNotifyEvent', @Lape_TForm_OnDblClick_Read, @Lape_TForm_OnDblClick_Write);
    //addClassVar('TForm', 'OnEnter', 'TNotifyEvent', @Lape_TForm_OnEnter_Read, @Lape_TForm_OnEnter_Write);
    //addClassVar('TForm', 'OnExit', 'TNotifyEvent', @Lape_TForm_OnExit_Read, @Lape_TForm_OnExit_Write);
    //addClassVar('TForm', 'OnClick', 'TNotifyEvent', @Lape_TForm_OnClick_Read, @Lape_TForm_OnClick_Write);
    //addClassVar('TForm', 'OnResize', 'TNotifyEvent', @Lape_TForm_OnResize_Read, @Lape_TForm_OnResize_Write);
    //addClassVar('TForm', 'Enabled', 'Boolean', @Lape_TForm_Enabled_Read, @Lape_TForm_Enabled_Write);
    //addClassVar('TForm', 'Font', 'TFont', @Lape_TForm_Font_Read, @Lape_TForm_Font_Write);
    //addClassVar('TForm', 'Visible', 'Boolean', @Lape_TForm_Visible_Read, @Lape_TForm_Visible_Write);
    //addClassVar('TForm', 'Canvas', 'TCanvas', @Lape_TForm_Canvas_Read, @Lape_TForm_Canvas_Write);
    //addClassVar('TForm', 'Left', 'Integer', @Lape_TForm_Left_Read, @Lape_TForm_Left_Write);
    //addClassVar('TForm', 'Height', 'Integer', @Lape_TForm_Height_Read, @Lape_TForm_Height_Write);
    //addClassVar('TForm', 'Top', 'Integer', @Lape_TForm_Top_Read, @Lape_TForm_Top_Write);
    //addClassVar('TForm', 'Width', 'Integer', @Lape_TForm_Width_Read, @Lape_TForm_Width_Write);
    //addClassVar('TForm', 'Caption', 'string', @Lape_TForm_Caption_Read, @Lape_TForm_Caption_Write);
    addClassVar('TForm', 'OnMouseMove', 'TMouseMoveEvent', @Lape_TForm_OnMouseMove_Read, @Lape_TForm_OnMouseMove_Write);
    addClassVar('TForm', 'OnMouseDown', 'TMouseEvent', @Lape_TForm_OnMouseDown_Read, @Lape_TForm_OnMouseDown_Write);
    addClassVar('TForm', 'OnMouseUp', 'TMouseEvent', @Lape_TForm_OnMouseUp_Read, @Lape_TForm_OnMouseUp_Write);
    addClassVar('TForm', 'OnKeyDown', 'TKeyEvent', @Lape_TForm_OnKeyDown_Read, @Lape_TForm_OnKeyDown_Write);
    addClassVar('TForm', 'OnKeyUp', 'TKeyEvent', @Lape_TForm_OnKeyUp_Read, @Lape_TForm_OnKeyUp_Write);
    addClassVar('TForm', 'OnKeyPress', 'TKeyPressEvent', @Lape_TForm_OnKeyPress_Read, @Lape_TForm_OnKeyPress_Write);
    addClassVar('TForm', 'Position', 'TPosition', @Lape_TForm_Position_Read, @Lape_TForm_Position_Write);

    //addGlobalFunc('procedure TForm.Free(); constref;', @Lape_TForm_Free);
  end;
end;

//constructor Create();
procedure Lape_TScrollBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBox(Params^[0])^ := TScrollBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TScrollBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBox(Params^[0])^.Free();
end;

procedure Lape_Import_TScrollBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TScrollBox', 'TScrollingWinControl');

    addGlobalFunc('procedure TScrollBox.Init(AOwner: TComponent); override', @Lape_TScrollBox_Init);
    //addGlobalFunc('procedure TScrollBox.Free(); constref;', @Lape_TScrollBox_Free);
  end;
end;

procedure Lape_Import_LCLForms(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addGlobalType('(caNone, caHide, caFree, caMinimize)', 'TCloseAction');
    addGlobalType('procedure(Sender: TObject; var CloseAction: TCloseAction) of object', 'TCloseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender : TObject; var CanClose: Boolean) of object', 'TCloseQueryEvent', FFI_DEFAULT_ABI);
    addGlobalType('(poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poMainFormCenter, poOwnerFormCenter)', 'TPosition');
    addGlobalType('(biSystemMenu, biMinimize, biMaximize, biHelp)', 'TBorderIcon');
    addGlobalType('set of TBorderIcon', 'TBorderIcons');
  end;

  Lape_Import_TSizeConstraints(Compiler);
  Lape_Import_TCustomForm(Compiler);
  Lape_Import_TForm(Compiler);
  Lape_Import_TScrollBox(Compiler);
end;

end.

