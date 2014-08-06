unit lplclforms;

{$mode objfpc}{$H+}
{$I Simba.inc}


interface

uses
  Classes, SysUtils, Forms, controls, lpcompiler, lptypes, lpClassHelper;

type
  PCustomForm = ^TCustomForm;
  PForm = ^TForm;
  PCloseAction = ^TCloseAction;
  PCloseEvent = ^TCloseEvent;
  PPosition = ^TPosition;
  PScrollBox = ^TScrollBox;
  PFormBorderStyle = ^TFormBorderStyle;

procedure RegisterLCLForms(Compiler: TLapeCompiler);

implementation
 uses lplclsystem,lplclgraphics,lplclcontrols;

 type
   PCloseQueryEvent = ^TCloseQueryEvent;
   PFormBorderIcons = ^TBorderIcons;
   PSizeConstraints = ^TSizeConstraints;
   PConstraintSize = ^TConstraintSize;

//constructor Create(AControl: TControl); virtual;
procedure TSizeConstraints_Init(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^ := TSizeConstraints.Create(PControl(Params^[1])^);
end;

//procedure UpdateInterfaceConstraints; virtual;
procedure TSizeConstraints_UpdateInterfaceConstraints(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.UpdateInterfaceConstraints();
end;

//procedure SetInterfaceConstraints(MinW, MinH, MaxW, MaxH: integer); virtual;
procedure TSizeConstraints_SetInterfaceConstraints(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.SetInterfaceConstraints(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function EffectiveMinWidth: integer; virtual;
procedure TSizeConstraints_EffectiveMinWidth(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMinWidth();
end;

//function EffectiveMinHeight: integer; virtual;
procedure TSizeConstraints_EffectiveMinHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMinHeight();
end;

//function EffectiveMaxWidth: integer; virtual;
procedure TSizeConstraints_EffectiveMaxWidth(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMaxWidth();
end;

//function EffectiveMaxHeight: integer; virtual;
procedure TSizeConstraints_EffectiveMaxHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.EffectiveMaxHeight();
end;

//function MinMaxWidth(Width: integer): integer;
procedure TSizeConstraints_MinMaxWidth(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinMaxWidth(Pinteger(Params^[1])^);
end;

//function MinMaxHeight(Height: integer): integer;
procedure TSizeConstraints_MinMaxHeight(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinMaxHeight(Pinteger(Params^[1])^);
end;

//Read: property MaxInterfaceHeight: integer read FMaxInterfaceHeight;
procedure TSizeConstraints_MaxInterfaceHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MaxInterfaceHeight;
end;

//Read: property MaxInterfaceWidth: integer read FMaxInterfaceWidth;
procedure TSizeConstraints_MaxInterfaceWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MaxInterfaceWidth;
end;

//Read: property MinInterfaceHeight: integer read FMinInterfaceHeight;
procedure TSizeConstraints_MinInterfaceHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinInterfaceHeight;
end;

//Read: property MinInterfaceWidth: integer read FMinInterfaceWidth;
procedure TSizeConstraints_MinInterfaceWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PSizeConstraints(Params^[0])^.MinInterfaceWidth;
end;

//Read: property Control: TControl read FControl;
procedure TSizeConstraints_Control_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PSizeConstraints(Params^[0])^.Control;
end;

//Read: property Options: TSizeConstraintsOptions read FOptions write SetOptions default [];
procedure TSizeConstraints_Options_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  //PSizeConstraintsOptions(Result)^ := PSizeConstraints(Params^[0])^.Options;
end;

//Write: property Options: TSizeConstraintsOptions read FOptions write SetOptions default [];
procedure TSizeConstraints_Options_Write(const Params: PParamArray); lape_extdecl
begin
 // PSizeConstraints(Params^[0])^.Options := PSizeConstraintsOptions(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TSizeConstraints_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PSizeConstraints(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TSizeConstraints_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property MaxHeight: TConstraintSize read FMaxHeight write SetMaxHeight default 0;
procedure TSizeConstraints_MaxHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PConstraintSize(Result)^ := PSizeConstraints(Params^[0])^.MaxHeight;
end;

//Write: property MaxHeight: TConstraintSize read FMaxHeight write SetMaxHeight default 0;
procedure TSizeConstraints_MaxHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.MaxHeight := PConstraintSize(Params^[1])^;
end;

//Read: property MaxWidth: TConstraintSize read FMaxWidth write SetMaxWidth default 0;
procedure TSizeConstraints_MaxWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MaxWidth;
end;

//Write: property MaxWidth: TConstraintSize read FMaxWidth write SetMaxWidth default 0;
procedure TSizeConstraints_MaxWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.MaxWidth := PInteger(Params^[1])^;
end;

//Read: property MinHeight: TConstraintSize read FMinHeight write SetMinHeight default 0;
procedure TSizeConstraints_MinHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinHeight;
end;

//Write: property MinHeight: TConstraintSize read FMinHeight write SetMinHeight default 0;
procedure TSizeConstraints_MinHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.MinHeight := PInteger(Params^[1])^;
end;

//Read: property MinWidth: TConstraintSize read FMinWidth write SetMinWidth default 0;
procedure TSizeConstraints_MinWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PSizeConstraints(Params^[0])^.MinWidth;
end;

//Write: property MinWidth: TConstraintSize read FMinWidth write SetMinWidth default 0;
procedure TSizeConstraints_MinWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.MinWidth := PConstraintSize(Params^[1])^;
end;

//procedure Free();
procedure TSizeConstraints_Free(const Params: PParamArray); lape_extdecl
begin
  PSizeConstraints(Params^[0])^.Free();
end;

procedure Register_TSizeConstraints(Compiler: TLapeCompiler);
begin
 with Compiler do
 begin
   addClass('TSizeConstraints', 'TPersistent');

   addGlobalFunc('procedure TSizeConstraints.Init(AControl: TControl);', @TSizeConstraints_Init);
   addGlobalFunc('procedure TSizeConstraints.UpdateInterfaceConstraints();', @TSizeConstraints_UpdateInterfaceConstraints);
   addGlobalFunc('procedure TSizeConstraints.SetInterfaceConstraints(MinW, MinH, MaxW, MaxH: integer);', @TSizeConstraints_SetInterfaceConstraints);
   addGlobalFunc('function TSizeConstraints.EffectiveMinWidth(): integer;', @TSizeConstraints_EffectiveMinWidth);
   addGlobalFunc('function TSizeConstraints.EffectiveMinHeight(): integer;', @TSizeConstraints_EffectiveMinHeight);
   addGlobalFunc('function TSizeConstraints.EffectiveMaxWidth(): integer;', @TSizeConstraints_EffectiveMaxWidth);
   addGlobalFunc('function TSizeConstraints.EffectiveMaxHeight(): integer;', @TSizeConstraints_EffectiveMaxHeight);
   addGlobalFunc('function TSizeConstraints.MinMaxWidth(Width: integer): integer;', @TSizeConstraints_MinMaxWidth);
   addGlobalFunc('function TSizeConstraints.MinMaxHeight(Height: integer): integer;', @TSizeConstraints_MinMaxHeight);
   addClassVar('TSizeConstraints', 'MaxInterfaceHeight', 'integer', @TSizeConstraints_MaxInterfaceHeight_Read, nil);
   addClassVar('TSizeConstraints', 'MaxInterfaceWidth', 'integer', @TSizeConstraints_MaxInterfaceWidth_Read, nil);
   addClassVar('TSizeConstraints', 'MinInterfaceHeight', 'integer', @TSizeConstraints_MinInterfaceHeight_Read, nil);
   addClassVar('TSizeConstraints', 'MinInterfaceWidth', 'integer', @TSizeConstraints_MinInterfaceWidth_Read, nil);
   addClassVar( 'TSizeConstraints', 'Control', 'TControl', @TSizeConstraints_Control_Read, nil);
  // addClassVar('TSizeConstraints', 'Options', 'TSizeConstraintsOptions', @TSizeConstraints_Options_Read, @TSizeConstraints_Options_Write);
   addClassVar('TSizeConstraints', 'OnChange', 'TNotifyEvent', @TSizeConstraints_OnChange_Read, @TSizeConstraints_OnChange_Write);
   addClassVar('TSizeConstraints', 'MaxHeight', 'Integer', @TSizeConstraints_MaxHeight_Read, @TSizeConstraints_MaxHeight_Write);
   addClassVar('TSizeConstraints', 'MaxWidth', 'Integer', @TSizeConstraints_MaxWidth_Read, @TSizeConstraints_MaxWidth_Write);
   addClassVar('TSizeConstraints', 'MinHeight', 'Integer', @TSizeConstraints_MinHeight_Read, @TSizeConstraints_MinHeight_Write);
   addClassVar('TSizeConstraints', 'MinWidth', 'Integer', @TSizeConstraints_MinWidth_Read, @TSizeConstraints_MinWidth_Write);
   addGlobalFunc('procedure TSizeConstraints.Free();', @TSizeConstraints_Free);
 end;
end;

 {TCustomForm}
 //constructor Create(AOwner: TComponent);
procedure TCustomForm_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^ := TCustomForm.Create(PComponent(Params^[1])^);
end;

//constructor CreateNew(AOwner: TComponent; Num: Integer);
procedure TCustomForm_CreateNew(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^ := TCustomForm.CreateNew(PComponent(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure AfterConstruction;
procedure TCustomForm_AfterConstruction(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AfterConstruction();
end;

//procedure BeforeDestruction;
procedure TCustomForm_BeforeDestruction(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.BeforeDestruction();
end;

//procedure Close;
procedure TCustomForm_Close(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.Close();
end;

//function CloseQuery: boolean;
procedure TCustomForm_CloseQuery(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.CloseQuery();
end;

//procedure DefocusControl(Control: TWinControl; Removing: Boolean);
procedure TCustomForm_DefocusControl(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.DefocusControl(PWinControl(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure DestroyWnd;
procedure TCustomForm_DestroyWnd(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.DestroyWnd();
end;

//procedure EnsureVisible(AMoveToTop: Boolean);
procedure TCustomForm_EnsureVisible(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.EnsureVisible(PBoolean(Params^[1])^);
end;

//procedure FocusControl(WinControl: TWinControl);
procedure TCustomForm_FocusControl(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.FocusControl(PWinControl(Params^[1])^);
end;

//function FormIsUpdating: boolean;
procedure TCustomForm_FormIsUpdating(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomForm(Params^[0])^.FormIsUpdating();
end;

//function GetFormImage: TBitmap;
procedure TCustomForm_GetFormImage(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBitmap(Result)^ := PCustomForm(Params^[0])^.GetFormImage();
end;

//procedure Hide;
procedure TCustomForm_Hide(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.Hide();
end;

//procedure IntfDropFiles(const FileNames: TStringArray);
procedure TCustomForm_IntfDropFiles(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.IntfDropFiles(PStringArray(Params^[1])^);
end;

//procedure IntfHelp(AComponent: TComponent);
procedure TCustomForm_IntfHelp(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.IntfHelp(PComponent(Params^[1])^);
end;

//function AutoSizeDelayedHandle: Boolean;
procedure TCustomForm_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AutoSizeDelayedHandle();
end;

//procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw,WithThemeSpace: boolean );
procedure TCustomForm_GetPreferredSize(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure Release;
procedure TCustomForm_Release(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.Release();
end;

//function CanFocus: Boolean;
procedure TCustomForm_CanFocus(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.CanFocus();
end;

//procedure SetFocus;
procedure TCustomForm_SetFocus(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.SetFocus();
end;

//function SetFocusedControl(Control: TWinControl): Boolean ;
procedure TCustomForm_SetFocusedControl(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.SetFocusedControl(PWinControl(Params^[1])^);
end;

//procedure SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer);
procedure TCustomForm_SetRestoredBounds(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.SetRestoredBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure Show;
procedure TCustomForm_Show(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.Show();
end;

//function ShowModal: Integer;
procedure TCustomForm_ShowModal(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomForm(Params^[0])^.ShowModal();
end;

//procedure ShowOnTop;
procedure TCustomForm_ShowOnTop(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.ShowOnTop();
end;

//procedure RemoveAllHandlersOfObject(AnObject: TObject);
procedure TCustomForm_RemoveAllHandlersOfObject(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.RemoveAllHandlersOfObject(PObject(Params^[1])^);
end;

//procedure AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;AsFirst: Boolean);
procedure TCustomForm_AddHandlerFirstShow(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AddHandlerFirstShow(PNotifyEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent);
procedure TCustomForm_RemoveHandlerFirstShow(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.RemoveHandlerFirstShow(PNotifyEvent(Params^[1])^);
end;

//procedure AddHandlerClose(OnCloseHandler: TCloseEvent; AsFirst: Boolean);
procedure TCustomForm_AddHandlerClose(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AddHandlerClose(PCloseEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure RemoveHandlerClose(OnCloseHandler: TCloseEvent);
procedure TCustomForm_RemoveHandlerClose(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.RemoveHandlerClose(PCloseEvent(Params^[1])^);
end;

//procedure AddHandlerCreate(OnCreateHandler: TNotifyEvent; AsFirst: Boolean);
procedure TCustomForm_AddHandlerCreate(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AddHandlerCreate(PNotifyEvent(Params^[1])^, PBoolean(Params^[2])^);
end;

//procedure RemoveHandlerCreate(OnCreateHandler: TNotifyEvent);
procedure TCustomForm_RemoveHandlerCreate(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.RemoveHandlerCreate(PNotifyEvent(Params^[1])^);
end;

//Read: property Active: Boolean read Active;
procedure TCustomForm_Active_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.Active;
end;

//Read: property ActiveControl: TWinControl read ActiveControl write ActiveControl;
procedure TCustomForm_ActiveControl_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWinControl(Result)^ := PCustomForm(Params^[0])^.ActiveControl;
end;

//Write: property ActiveControl: TWinControl read ActiveControl write ActiveControl;
procedure TCustomForm_ActiveControl_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.ActiveControl := PWinControl(Params^[1])^;
end;

//Read: property ActiveDefaultControl: TControl read ActiveDefaultControl write ActiveDefaultControl;
procedure TCustomForm_ActiveDefaultControl_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.ActiveDefaultControl;
end;

//Write: property ActiveDefaultControl: TControl read ActiveDefaultControl write ActiveDefaultControl;
procedure TCustomForm_ActiveDefaultControl_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.ActiveDefaultControl := PControl(Params^[1])^;
end;

//Read: property AllowDropFiles: Boolean read AllowDropFiles write AllowDropFiles ;
procedure TCustomForm_AllowDropFiles_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AllowDropFiles;
end;

//Write: property AllowDropFiles: Boolean read AllowDropFiles write AllowDropFiles ;
procedure TCustomForm_AllowDropFiles_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AllowDropFiles := PBoolean(Params^[1])^;
end;

//Read: property AlphaBlend: Boolean read AlphaBlend write AlphaBlend;
procedure TCustomForm_AlphaBlend_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.AlphaBlend;
end;

//Write: property AlphaBlend: Boolean read AlphaBlend write AlphaBlend;
procedure TCustomForm_AlphaBlend_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AlphaBlend := PBoolean(Params^[1])^;
end;

//Read: property AlphaBlendValue: Byte read AlphaBlendValue write AlphaBlendValue;
procedure TCustomForm_AlphaBlendValue_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PByte(Result)^ := PCustomForm(Params^[0])^.AlphaBlendValue;
end;

//Write: property AlphaBlendValue: Byte read AlphaBlendValue write AlphaBlendValue;
procedure TCustomForm_AlphaBlendValue_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.AlphaBlendValue := PByte(Params^[1])^;
end;

//Read: property CancelControl: TControl read CancelControl write CancelControl;
procedure TCustomForm_CancelControl_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.CancelControl;
end;

//Write: property CancelControl: TControl read CancelControl write CancelControl;
procedure TCustomForm_CancelControl_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.CancelControl := PControl(Params^[1])^;
end;

//Read: property DefaultControl: TControl read DefaultControl write DefaultControl;
procedure TCustomForm_DefaultControl_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PControl(Result)^ := PCustomForm(Params^[0])^.DefaultControl;
end;

//Write: property DefaultControl: TControl read DefaultControl write DefaultControl;
procedure TCustomForm_DefaultControl_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.DefaultControl := PControl(Params^[1])^;
end;

//Read: property KeyPreview: Boolean read KeyPreview write KeyPreview;
procedure TCustomForm_KeyPreview_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomForm(Params^[0])^.KeyPreview;
end;

//Write: property KeyPreview: Boolean read KeyPreview write KeyPreview;
procedure TCustomForm_KeyPreview_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.KeyPreview := PBoolean(Params^[1])^;
end;

//Read: property PopupParent: TCustomForm read PopupParent write PopupParent;
procedure TCustomForm_PopupParent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCustomForm(Result)^ := PCustomForm(Params^[0])^.PopupParent;
end;

//Write: property PopupParent: TCustomForm read PopupParent write PopupParent;
procedure TCustomForm_PopupParent_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.PopupParent := PCustomForm(Params^[1])^;
end;

//Read: property OnActivate: TNotifyEvent read OnActivate write OnActivate;
procedure TCustomForm_OnActivate_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnActivate;
end;

//Write: property OnActivate: TNotifyEvent read OnActivate write OnActivate;
procedure TCustomForm_OnActivate_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnActivate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure TCustomForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCloseEvent(Result)^ := PCustomForm(Params^[0])^.OnClose;
end;

//Write: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure TCustomForm_OnClose_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

//Read: property OnCloseQuery : TCloseQueryEvent read OnCloseQuery write OnCloseQuery;
procedure TCustomForm_OnCloseQuery_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCloseQueryEvent(Result)^ := PCustomForm(Params^[0])^.OnCloseQuery;
end;

//Write: property OnCloseQuery : TCloseQueryEvent read OnCloseQuery write OnCloseQuery;
procedure TCustomForm_OnCloseQuery_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnCloseQuery := PCloseQueryEvent(Params^[1])^;
end;

//Read: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure TCustomForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnCreate;
end;

//Write: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure TCustomForm_OnCreate_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDeactivate: TNotifyEvent read OnDeactivate write OnDeactivate;
procedure TCustomForm_OnDeactivate_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnDeactivate;
end;

//Write: property OnDeactivate: TNotifyEvent read OnDeactivate write OnDeactivate;
procedure TCustomForm_OnDeactivate_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnDeactivate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure TCustomForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnDestroy;
end;

//Write: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure TCustomForm_OnDestroy_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure TCustomForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure TCustomForm_OnHide_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;



//Read: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure TCustomForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure TCustomForm_OnShow_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property OnWindowStateChange: TNotifyEvent read OnWindowStateChange write OnWindowStateChange;
procedure TCustomForm_OnWindowStateChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomForm(Params^[0])^.OnWindowStateChange;
end;

//Write: property OnWindowStateChange: TNotifyEvent read OnWindowStateChange write OnWindowStateChange;
procedure TCustomForm_OnWindowStateChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.OnWindowStateChange := PNotifyEvent(Params^[1])^;
end;

//Read: property PixelsPerInch: Longint read PixelsPerInch write PixelsPerInch;
procedure TCustomForm_PixelsPerInch_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PCustomForm(Params^[0])^.PixelsPerInch;
end;

//Write: property PixelsPerInch: Longint read PixelsPerInch write PixelsPerInch;
procedure TCustomForm_PixelsPerInch_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.PixelsPerInch := PLongint(Params^[1])^;
end;

//Read: property RestoredLeft: integer read RestoredLeft;
procedure TCustomForm_RestoredLeft_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredLeft;
end;

//Read: property RestoredTop: integer read RestoredTop;
procedure TCustomForm_RestoredTop_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredTop;
end;

//Read: property RestoredWidth: integer read RestoredWidth;
procedure TCustomForm_RestoredWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredWidth;
end;

//Read: property RestoredHeight: integer read RestoredHeight;
procedure TCustomForm_RestoredHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomForm(Params^[0])^.RestoredHeight;
end;

procedure TCustomForm_Write_BorderStyle(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.BorderStyle := PFormBorderStyle(Params^[1])^;
end;

procedure TCustomForm_Read_BorderStyle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFormBorderStyle(Result)^ := PCustomForm(Params^[0])^.BorderStyle;
end;

procedure TCustomForm_Write_BorderIcons(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.BorderIcons := PFormBorderIcons(Params^[1])^;
end;

procedure TCustomForm_Read_BorderIcons(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFormBorderIcons(Result)^ := PCustomForm(Params^[0])^.BorderIcons;
end;

procedure TCustomForm_Constraints_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PSizeConstraints(Result)^ := PCustomForm(Params^[0])^.Constraints;
end;

procedure TCustomForm_Constraints_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.Constraints := PSizeConstraints(Params^[1])^;
end;

//procedure Free();
procedure TCustomForm_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomForm(Params^[0])^.Free;
end;

procedure Register_TCustomForm(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomForm', 'TScrollingWinControl');

    addGlobalFunc('procedure TCustomForm.Init(AOwner: TComponent);', @TCustomForm_Init);
    addGlobalFunc('procedure TCustomForm.InitNew(AOwner: TComponent; Num: Integer);', @TCustomForm_CreateNew);
    addGlobalFunc('procedure TCustomForm.AfterConstruction();', @TCustomForm_AfterConstruction);
    addGlobalFunc('procedure TCustomForm.BeforeDestruction();', @TCustomForm_BeforeDestruction);
    addGlobalFunc('procedure TCustomForm.Close();', @TCustomForm_Close);
    addGlobalFunc('function TCustomForm.CloseQuery(): boolean;', @TCustomForm_CloseQuery);
    addGlobalFunc('procedure TCustomForm.DefocusControl(Control: TWinControl; Removing: Boolean);', @TCustomForm_DefocusControl);
    addGlobalFunc('procedure TCustomForm.DestroyWnd();', @TCustomForm_DestroyWnd);
    addGlobalFunc('procedure TCustomForm.EnsureVisible(AMoveToTop: Boolean);', @TCustomForm_EnsureVisible);
    addGlobalFunc('procedure TCustomForm.FocusControl(WinControl: TWinControl);', @TCustomForm_FocusControl);
    addGlobalFunc('function TCustomForm.FormIsUpdating(): boolean;', @TCustomForm_FormIsUpdating);
    addGlobalFunc('function TCustomForm.GetFormImage(): TBitmap;', @TCustomForm_GetFormImage);
    addGlobalFunc('procedure TCustomForm.Hide();', @TCustomForm_Hide);
    addGlobalFunc('procedure TCustomForm.IntfDropFiles(const FileNames: TStringArray);', @TCustomForm_IntfDropFiles);
    addGlobalFunc('procedure TCustomForm.IntfHelp(AComponent: TComponent);', @TCustomForm_IntfHelp);
    addGlobalFunc('function TCustomForm.AutoSizeDelayedHandle(): Boolean;', @TCustomForm_AutoSizeDelayedHandle);
    addGlobalFunc('procedure TCustomForm.GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw,WithThemeSpace: boolean );', @TCustomForm_GetPreferredSize);
    addGlobalFunc('procedure TCustomForm.Release();', @TCustomForm_Release);
    addGlobalFunc('function TCustomForm.CanFocus(): Boolean;', @TCustomForm_CanFocus);
    addGlobalFunc('procedure TCustomForm.SetFocus();', @TCustomForm_SetFocus);
    addGlobalFunc('function TCustomForm.SetFocusedControl(Control: TWinControl): Boolean;', @TCustomForm_SetFocusedControl);
    addGlobalFunc('procedure TCustomForm.SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer);', @TCustomForm_SetRestoredBounds);
    addGlobalFunc('procedure TCustomForm.Show();', @TCustomForm_Show);
    addGlobalFunc('function TCustomForm.ShowModal(): Integer;', @TCustomForm_ShowModal);
    addGlobalFunc('procedure TCustomForm.ShowOnTop();', @TCustomForm_ShowOnTop);
    addGlobalFunc('procedure TCustomForm.RemoveAllHandlersOfObject(AnObject: TObject);', @TCustomForm_RemoveAllHandlersOfObject);
    addGlobalFunc('procedure TCustomForm.AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;AsFirst: Boolean);', @TCustomForm_AddHandlerFirstShow);
    addGlobalFunc('procedure TCustomForm.RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent);', @TCustomForm_RemoveHandlerFirstShow);
    addGlobalFunc('procedure TCustomForm.AddHandlerClose(OnCloseHandler: TCloseEvent; AsFirst: Boolean);', @TCustomForm_AddHandlerClose);
    addGlobalFunc('procedure TCustomForm.RemoveHandlerClose(OnCloseHandler: TCloseEvent);', @TCustomForm_RemoveHandlerClose);
    addGlobalFunc('procedure TCustomForm.AddHandlerCreate(OnCreateHandler: TNotifyEvent; AsFirst: Boolean);', @TCustomForm_AddHandlerCreate);
    addGlobalFunc('procedure TCustomForm.RemoveHandlerCreate(OnCreateHandler: TNotifyEvent);', @TCustomForm_RemoveHandlerCreate);
    addClassVar('TCustomForm', 'BorderStyle', 'TFormBorderStyle', @TCustomForm_Read_BorderStyle, @TCustomForm_Write_BorderStyle);
    addClassVar('TCustomForm', 'BorderIcons', 'TBorderIcons', @TCustomForm_Read_BorderIcons, @TCustomForm_Write_BorderIcons);
    addClassVar('TCustomForm', 'Active', 'Boolean', @TCustomForm_Active_Read);
    addClassVar('TCustomForm', 'ActiveControl', 'TWinControl', @TCustomForm_ActiveControl_Read, @TCustomForm_ActiveControl_Write);
    addClassVar('TCustomForm', 'ActiveDefaultControl', 'TControl', @TCustomForm_ActiveDefaultControl_Read, @TCustomForm_ActiveDefaultControl_Write);
    addClassVar('TCustomForm', 'AllowDropFiles', 'Boolean', @TCustomForm_AllowDropFiles_Read, @TCustomForm_AllowDropFiles_Write);
    addClassVar('TCustomForm', 'AlphaBlend', 'Boolean', @TCustomForm_AlphaBlend_Read, @TCustomForm_AlphaBlend_Write);
    addClassVar('TCustomForm', 'AlphaBlendValue', 'Byte', @TCustomForm_AlphaBlendValue_Read, @TCustomForm_AlphaBlendValue_Write);
    addClassVar('TCustomForm', 'CancelControl', 'TControl', @TCustomForm_CancelControl_Read, @TCustomForm_CancelControl_Write);
    addClassVar('TCustomForm', 'DefaultControl', 'TControl', @TCustomForm_DefaultControl_Read, @TCustomForm_DefaultControl_Write);
    addClassVar('TCustomForm', 'KeyPreview', 'Boolean', @TCustomForm_KeyPreview_Read, @TCustomForm_KeyPreview_Write);
    addClassVar('TCustomForm', 'PopupParent', 'TCustomForm', @TCustomForm_PopupParent_Read, @TCustomForm_PopupParent_Write);
    addClassVar('TCustomForm', 'OnActivate', 'TNotifyEvent', @TCustomForm_OnActivate_Read, @TCustomForm_OnActivate_Write);
    addClassVar('TCustomForm', 'OnClose', 'TCloseEvent', @TCustomForm_OnClose_Read, @TCustomForm_OnClose_Write);
    addClassVar('TCustomForm', 'OnCloseQuery', 'TCloseQueryEvent', @TCustomForm_OnCloseQuery_Read, @TCustomForm_OnCloseQuery_Write);
    addClassVar('TCustomForm', 'OnCreate', 'TNotifyEvent', @TCustomForm_OnCreate_Read, @TCustomForm_OnCreate_Write);
    addClassVar('TCustomForm', 'OnDeactivate', 'TNotifyEvent', @TCustomForm_OnDeactivate_Read, @TCustomForm_OnDeactivate_Write);
    addClassVar('TCustomForm', 'OnDestroy', 'TNotifyEvent', @TCustomForm_OnDestroy_Read, @TCustomForm_OnDestroy_Write);
    addClassVar('TCustomForm', 'OnHide', 'TNotifyEvent', @TCustomForm_OnHide_Read, @TCustomForm_OnHide_Write);
    addClassVar('TCustomForm', 'OnShow', 'TNotifyEvent', @TCustomForm_OnShow_Read, @TCustomForm_OnShow_Write);
    addClassVar('TCustomForm', 'OnWindowStateChange', 'TNotifyEvent', @TCustomForm_OnWindowStateChange_Read, @TCustomForm_OnWindowStateChange_Write);
    addClassVar('TCustomForm', 'PixelsPerInch', 'Longint', @TCustomForm_PixelsPerInch_Read, @TCustomForm_PixelsPerInch_Write);
    addClassVar('TCustomForm', 'RestoredLeft', 'integer', @TCustomForm_RestoredLeft_Read);
    addClassVar('TCustomForm', 'RestoredTop', 'integer', @TCustomForm_RestoredTop_Read);
    addClassVar('TCustomForm', 'RestoredWidth', 'integer', @TCustomForm_RestoredWidth_Read);
    addClassVar('TCustomForm', 'RestoredHeight', 'integer', @TCustomForm_RestoredHeight_Read);
    addClassVar('TCustomForm', 'Constraints', 'TSizeConstraints', @TCustomForm_Constraints_Read, @TCustomForm_Constraints_Write);
    addGlobalFunc('procedure TCustomForm.Free();', @TCustomForm_Free);
  end;
end;
 {TForm}
//constructor Create(TheOwner: TComponent);
procedure TForm_Init(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^ := TForm.Create(PComponent(Params^[1])^);
end;

//procedure Cascade;
procedure TForm_Cascade(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Cascade();
end;

//procedure Next;
procedure TForm_Next(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Next();
end;

//procedure Previous;
procedure TForm_Previous(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Previous();
end;

//procedure Tile;
procedure TForm_Tile(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Tile();
end;

//procedure Show;
procedure TForm_Show(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Show();
end;

//procedure Close;
procedure TForm_Close(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Close();
end;

//procedure Hide;
procedure TForm_Hide(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Hide();
end;

//Read: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure TForm_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientWidth;
end;

//Write: property ClientWidth: Integer read ClientWidth write ClientWidth;
procedure TForm_ClientWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

//Read: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure TForm_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.ClientHeight;
end;

//Write: property ClientHeight: Integer read ClientHeight write ClientHeight;
procedure TForm_ClientHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

//Read: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure TForm_OnClose_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCloseEvent(Result)^ := PForm(Params^[0])^.OnClose;
end;

//Write: property OnClose: TCloseEvent read FOnClose write FOnClose;
procedure TForm_OnClose_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnClose := PCloseEvent(Params^[1])^;
end;

//Read: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure TForm_OnCreate_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnCreate;
end;

//Write: property OnCreate: TNotifyEvent read OnCreate write OnCreate;
procedure TForm_OnCreate_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnCreate := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure TForm_OnDestroy_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDestroy;
end;

//Write: property OnDestroy: TNotifyEvent read OnDestroy write OnDestroy;
procedure TForm_OnDestroy_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnDestroy := PNotifyEvent(Params^[1])^;
end;

//Read: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure TForm_OnHide_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnHide;
end;

//Write: property OnHide: TNotifyEvent read OnHide write OnHide;
procedure TForm_OnHide_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnHide := PNotifyEvent(Params^[1])^;
end;

//Read: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure TForm_OnPaint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnPaint;
end;

//Write: property OnPaint: TNotifyEvent read OnPaint write OnPaint;
procedure TForm_OnPaint_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read OnShow write OnShow;
procedure TForm_OnShow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read OnShow write OnShow;
procedure TForm_OnShow_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property OnDblClick: TNotifyEvent read OnDblClick write OnDblClick;
procedure TForm_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnDblClick;
end;

//Write: property OnDblClick: TNotifyEvent read OnDblClick write OnDblClick;
procedure TForm_OnDblClick_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

//Read: property OnEnter: TNotifyEvent read OnEnter write OnEnter;
procedure TForm_OnEnter_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnEnter;
end;

//Write: property OnEnter: TNotifyEvent read OnEnter write OnEnter;
procedure TForm_OnEnter_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

//Read: property OnExit: TNotifyEvent read FOnExit write OnExit;
procedure TForm_OnExit_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnExit;
end;

//Write: property OnExit: TNotifyEvent read FOnExit write OnExit;
procedure TForm_OnExit_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

//Read: property OnClick: TNotifyEvent read OnClick write OnClick;
procedure TForm_OnClick_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnClick;
end;

//Write: property OnClick: TNotifyEvent read OnClick write OnClick;
procedure TForm_OnClick_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

//Read: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure TForm_OnResize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PForm(Params^[0])^.OnResize;
end;

//Write: property OnResize: TNotifyEvent read OnResize write OnResize;
procedure TForm_OnResize_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

//Read: property Enabled: Boolean read Enabled write Enabled;
procedure TForm_Enabled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Enabled;
end;

//Write: property Enabled: Boolean read Enabled write Enabled;
procedure TForm_Enabled_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

//Read: property Font: TFont read Font write Font;
procedure TForm_Font_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFont(Result)^ := PForm(Params^[0])^.Font;
end;

//Write: property Font: TFont read Font write Font;
procedure TForm_Font_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property Visible: Boolean read Visible write Visible;
procedure TForm_Visible_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PForm(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read Visible write Visible;
procedure TForm_Visible_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//Read: property Canvas: TCanvas read Canvas write Canvas;
procedure TForm_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PForm(Params^[0])^.Canvas;
end;

//Write: property Canvas: TCanvas read Canvas write Canvas;
procedure TForm_Canvas_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

//Read: property Left: Integer read Left write Left;
procedure TForm_Left_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Left;
end;

//Write: property Left: Integer read Left write Left;
procedure TForm_Left_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Left := PInteger(Params^[1])^;
end;

//Read: property Height: Integer read Height write Height;
procedure TForm_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Height;
end;

//Write: property Height: Integer read Height write Height;
procedure TForm_Height_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Height := PInteger(Params^[1])^;
end;

//Read: property Top: Integer read Top write Top;
procedure TForm_Top_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Top;
end;

//Write: property Top: Integer read Top write Top;
procedure TForm_Top_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Top := PInteger(Params^[1])^;
end;

//Read: property Width: Integer read Width write Width;
procedure TForm_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PForm(Params^[0])^.Width;
end;

//Write: property Width: Integer read Width write Width;
procedure TForm_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//Read: property Caption: string read Caption write Caption;
procedure TForm_Caption_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PForm(Params^[0])^.Caption;
end;

//Write: property Caption: string read Caption write Caption;
procedure TForm_Caption_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

//Read: property Position: TPosition;
procedure TForm_Position_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPosition(Result)^ := PForm(Params^[0])^.Position;
end;

//Write: property Position: TPosition
procedure TForm_Position_Write(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Position := PPosition(Params^[1])^;
end;

//procedure Free();
procedure TForm_Free(const Params: PParamArray); lape_extdecl
begin
  PForm(Params^[0])^.Free();
end;

procedure TForm_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Component: TComponent;
begin
  Component := PForm(Params^[0])^.FindComponent('MouseMove');

  if (Assigned(Component)) then
    PMouseMoveEventWrapper(Result)^ := TOnMouseMoveWrapper(Component).InternalMethod
  else
    PMouseMoveEvent(Result)^ := nil;
end;

procedure TForm_OnMouseMove_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PForm(Params^[0])^.FindComponent('MouseMove');
  if (not Assigned(Component)) then
  begin
    Component := TOnMouseMoveWrapper.Create(PForm(Params^[0])^);
    Component.Name := 'MouseMove';
  end;

  with TOnMouseMoveWrapper(Component) do
  begin
    InternalMethod := PMouseMoveEventWrapper(Params^[1])^;
    PForm(Params^[0])^.OnMouseMove := @MouseMove;
  end;
end;

procedure Register_TForm(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TForm', 'TCustomForm');

    addGlobalFunc('procedure TForm.Init(TheOwner: TComponent);', @TForm_Init);
    addGlobalFunc('procedure TForm.Cascade();', @TForm_Cascade);
    addGlobalFunc('procedure TForm.Next();', @TForm_Next);
    addGlobalFunc('procedure TForm.Previous();', @TForm_Previous);
    addGlobalFunc('procedure TForm.Tile();', @TForm_Tile);
    addGlobalFunc('procedure TForm.Show();', @TForm_Show);
    addGlobalFunc('procedure TForm.Close();', @TForm_Close);
    addGlobalFunc('procedure TForm.Hide();', @TForm_Hide);
    addClassVar('TForm', 'ClientWidth', 'Integer', @TForm_ClientWidth_Read, @TForm_ClientWidth_Write);
    addClassVar('TForm', 'ClientHeight', 'Integer', @TForm_ClientHeight_Read, @TForm_ClientHeight_Write);
    addClassVar('TForm', 'OnClose', 'TCloseEvent', @TForm_OnClose_Read, @TForm_OnClose_Write);
    addClassVar('TForm', 'OnCreate', 'TNotifyEvent', @TForm_OnCreate_Read, @TForm_OnCreate_Write);
    addClassVar('TForm', 'OnDestroy', 'TNotifyEvent', @TForm_OnDestroy_Read, @TForm_OnDestroy_Write);
    addClassVar('TForm', 'OnHide', 'TNotifyEvent', @TForm_OnHide_Read, @TForm_OnHide_Write);
    addClassVar('TForm', 'OnPaint', 'TNotifyEvent', @TForm_OnPaint_Read, @TForm_OnPaint_Write);
    addClassVar('TForm', 'OnShow', 'TNotifyEvent', @TForm_OnShow_Read, @TForm_OnShow_Write);
    addClassVar('TForm', 'OnDblClick', 'TNotifyEvent', @TForm_OnDblClick_Read, @TForm_OnDblClick_Write);
    addClassVar('TForm', 'OnEnter', 'TNotifyEvent', @TForm_OnEnter_Read, @TForm_OnEnter_Write);
    addClassVar('TForm', 'OnExit', 'TNotifyEvent', @TForm_OnExit_Read, @TForm_OnExit_Write);
    addClassVar('TForm', 'OnClick', 'TNotifyEvent', @TForm_OnClick_Read, @TForm_OnClick_Write);
    addClassVar('TForm', 'OnResize', 'TNotifyEvent', @TForm_OnResize_Read, @TForm_OnResize_Write);
    addClassVar('TForm', 'Enabled', 'Boolean', @TForm_Enabled_Read, @TForm_Enabled_Write);
    addClassVar('TForm', 'Font', 'TFont', @TForm_Font_Read, @TForm_Font_Write);
    addClassVar('TForm', 'Visible', 'Boolean', @TForm_Visible_Read, @TForm_Visible_Write);
    addClassVar('TForm', 'Canvas', 'TCanvas', @TForm_Canvas_Read, @TForm_Canvas_Write);
    addClassVar('TForm', 'Left', 'Integer', @TForm_Left_Read, @TForm_Left_Write);
    addClassVar('TForm', 'Height', 'Integer', @TForm_Height_Read, @TForm_Height_Write);
    addClassVar('TForm', 'Top', 'Integer', @TForm_Top_Read, @TForm_Top_Write);
    addClassVar('TForm', 'Width', 'Integer', @TForm_Width_Read, @TForm_Width_Write);
    addClassVar('TForm', 'Caption', 'string', @TForm_Caption_Read, @TForm_Caption_Write);
    addClassVar('TForm', 'Position', 'TPosition', @TForm_Position_Read, @TForm_Position_Write);
    addGlobalFunc('procedure TForm.Free();', @TForm_Free);
    addClassVar('TForm', 'OnMouseMove', 'TMouseMoveEvent', @TForm_OnMouseMove_Read, @TForm_OnMouseMove_Write);
  end;
end;

//constructor Create();
procedure TScrollBox_Init(const Params: PParamArray); lape_extdecl
begin
  PScrollBox(Params^[0])^ := TScrollBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TScrollBox_Free(const Params: PParamArray); lape_extdecl
begin
  PScrollBox(Params^[0])^.Free();
end;

procedure Register_TScrollBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TScrollBox', 'TScrollingWinControl');

    addGlobalFunc('procedure TScrollBox.Init(AOwner: TComponent);', @TScrollBox_Init);
    addGlobalFunc('procedure TScrollBox.Free();', @TScrollBox_Free);
  end;
end;

procedure RegisterLCLForms(Compiler: TLapeCompiler);
begin
  with Compiler do
    begin
      AddGlobalType('(caNone, caHide, caFree, caMinimize)','TCloseAction');
      AddGlobalType('procedure(Sender: TObject; var CloseAction: TCloseAction)','TCloseEvent');
      AddGlobalType('procedure(Sender : TObject; var CanClose : boolean)','TCloseQueryEvent');
      AddGlobalType('(poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poMainFormCenter, poOwnerFormCenter)', 'TPosition');
      AddGlobalType('(biSystemMenu, biMinimize, biMaximize, biHelp)', 'TBorderIcon');
      AddGlobalType('set of TBorderIcon', 'TBorderIcons');
    end;
    Register_TSizeConstraints(Compiler);
    Register_TCustomForm(Compiler);
    Register_TForm(Compiler);
    Register_TScrollBox(Compiler);
end;

end.

