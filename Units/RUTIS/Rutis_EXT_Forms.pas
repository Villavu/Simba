Unit Rutis_EXT_Forms;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls, TypInfo,
  Rutis_Engine, Rutis_Defs;

Type
  TUnownedForm = Class(TForm)
  Protected
    Procedure CreateWnd; Override;
  End;

  TEXTF_EventType = (
    etClick, etChange, etResize, etTimer,
    etMouseDown, etMouseUp, etMouseMove,
    etKeyPress, etKeyDown, etKeyUp,
    etNone);
Const
  EventNames  : Array [TEXTF_EventType] Of String[64] = (
    'OnClick', 'OnChange', 'OnResize', 'OnTimer',
    'OnMouseDown', 'OnMouseUp', 'OnMouseMove',
    'OnKeyPress', 'OnKeyDown', 'OnKeyUp',
    '');

Type
  TEXTF_EventLink = Record
    EventType  : TEXTF_EventType;
    Control    : TControl;
    Address    : Cardinal;
    DoCall     : Boolean;
  End;

  PControl = ^TControl;
  PComponent = ^TComponent;

  TEventContentHolder = Class
  Public
    ASender  : TObject;
    AButton  : TMouseButton;
    AShift   : TShiftState;
    AX, AY   : Integer;
    AKey     : Word;
    Procedure OnClick(Sender : TObject);
    Procedure OnTimer(Sender : TObject);
    Procedure OnChange(Sender : TObject);
    Procedure OnResize(Sender : TObject);
    Procedure OnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    Procedure OnMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    Procedure OnMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    Procedure OnKeyPress(Sender : TObject; Var Key : Char);
    Procedure OnKeyDown(Sender : TObject; Var Key : Word; Shift : TShiftState);
    Procedure OnKeyUp(Sender : TObject; Var Key : Word; Shift : TShiftState);
  End;

Var
  EXTF_Forms       : Array Of TForm;
  EXTF_EventLinks  : Array Of TEXTF_EventLink;
  EXTF_Engine      : TRutisEngine;

Const
  ControlClasses  : Array [1..11] Of TControlClass = (
    TButton, TLabel, TEdit, TPanel, TMemo, TImage,
    TListBox, TCheckBox, TRadioButton, TComboBox, TScrollBar
    );
  ComponentClasses  : Array [1..1] Of TComponentClass = (
    TTimer
    );

Procedure RegisterEXTMethods(Engine : TRutisEngine);
Procedure EXTF_DestroyRutisForms;
Function EXTF_GetEventID(EventType : TEXTF_EventType; Control : TControl) : Integer;
//==============================================================================
//==============================================================================
Implementation

Var
  EventContentHolder   : TEventContentHolder;
  EXTF_CurrentForm     : TForm;
  EXTF_CurrentControl  : TControl;
 //==============================================================================
 //==============================================================================
 { TUnownedForm }

Procedure TUnownedForm.CreateWnd;
Var
  Params     : TCreateParams;
  TempClass  : TWndClass;
  ClassRegistered  : Boolean;
Begin
  CreateParams(Params);
  With Params Do
  Begin
    WndParent       := 0;
    DefWndProc      := WindowClass.lpfnWndProc;
    ClassRegistered := GetClassInfo(WindowClass.hInstance, WinClassName, TempClass);
    If not ClassRegistered or (TempClass.lpfnWndProc <> @InitWndProc) Then
    Begin
      If ClassRegistered Then Windows.UnregisterClass(WinClassName,
          WindowClass.hInstance);
      WindowClass.lpfnWndProc   := @InitWndProc;
      WindowClass.lpszClassName := WinClassName;
      If Windows.RegisterClass(WindowClass) = 0 Then RaiseLastOSError;
    End;
    CreationControl := Self;
    CreateWindowHandle(Params);
    If WindowHandle = 0 Then
      RaiseLastOSError;
    If (GetWindowLong(WindowHandle, GWL_STYLE) and WS_CHILD <> 0) and
      (GetWindowLong(WindowHandle, GWL_ID) = 0) Then
      SetWindowLong(WindowHandle, GWL_ID, WindowHandle);
  End;
  StrDispose(WindowText);
  WindowText := nil;
  UpdateBounds;
  Perform(WM_SETFONT, Font.Handle, 1);
  If AutoSize Then AdjustSize;
End;

//==============================================================================
//==============================================================================

Procedure EXTF_DestroyRutisForms;
Var i  : Integer;
Begin
  For I := 0 To high(EXTF_Forms) Do
    EXTF_Forms[i].Free;
  SetLength(EXTF_Forms, 0);
  SetLength(EXTF_EventLinks, 0);
End;

Function EXTF_GetEventID(EventType : TEXTF_EventType; Control : TControl) : Integer;
Begin
  For Result := 0 To high(EXTF_EventLinks) Do
    If (EXTF_EventLinks[Result].EventType = EventType) and
      (EXTF_EventLinks[Result].Control = Control) Then
      exit;
  Result := -1;
End;

Function EXTF_CallEvents : Boolean;
Var
  i    : Integer;
  CMD  : TRutisScriptCmd;
Begin
  Result := False;
  For i := 0 To high(EXTF_EventLinks) Do
    If EXTF_EventLinks[i].DoCall Then
    Begin
      Result := True;
      EXTF_EventLinks[i].DoCall := False;

      {CMD.Cmd := _Gen4;
      CMD.P1  := EXTF_EventLinks[i].Address;
      CMD.P2  := 0;
      CMD.P3  := 0;
      EXTF_Engine.ExecuteCMD(CMD);}
      
      CMD.Cmd := _Call;
      CMD.P1  := EXTF_EventLinks[i].Address;
      CMD.P2  := 1;
      CMD.P3  := 0;
      EXTF_Engine.ExecuteCMD(CMD);
    End;
End;

Procedure RegisterObjectEvent(Obj : TObject; EventName : String; Adr : Pointer);
Var
  PropInfo  : PPropInfo;
  AMethod   : TMethod;
Begin
  AMethod.Code := Adr;
  AMethod.Data := EventContentHolder;
  Try
    PropInfo := GetPropInfo(Obj, EventName);
    If PropInfo <> nil Then
      SetMethodProp(Obj, PropInfo, AMethod);
  Except
  End;
End;

//==============================================================================
//==============================================================================

//TEXTF_EventType = (etClick, etMouseDown, etMouseUp, etChange, etKeyPress, etKeyDown, etKeyUp);
Procedure TEventContentHolder.OnChange(Sender : TObject);
Var id  : Integer;
Begin
  ASender := Sender;
  id := EXTF_GetEventID(etChange, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

procedure TEventContentHolder.OnTimer(Sender: TObject);
Var id  : Integer;
Begin
  ASender := Sender;
  id := EXTF_GetEventID(etTimer, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
end;

Procedure TEventContentHolder.OnClick(Sender : TObject);
Var id  : Integer;
Begin
  ASender := Sender;
  id := EXTF_GetEventID(etClick, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnResize(Sender : TObject);
Var id  : Integer;
Begin
  ASender := Sender;
  id := EXTF_GetEventID(etResize, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Var id  : Integer;
Begin
  ASender := Sender;
  AButton := Button;
  AShift  := Shift;
  AX      := X;
  AY      := Y;
  id      := EXTF_GetEventID(etMouseDown, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Var id  : Integer;
Begin
  ASender := Sender;
  AButton := Button;
  AShift  := Shift;
  AX      := X;
  AY      := Y;
  id      := EXTF_GetEventID(etMouseUp, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
Var id  : Integer;
Begin
  ASender := Sender;
  AShift := Shift;
  AX     := X;
  AY     := Y;
  id     := EXTF_GetEventID(etMouseMove, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnKeyPress(Sender : TObject; Var Key : Char);
Var id  : Integer;
Begin
  ASender := Sender;
  AKey := Word(Key);
  id   := EXTF_GetEventID(etKeyPress, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnKeyDown(Sender : TObject; Var Key : Word; Shift : TShiftState);
Var id  : Integer;
Begin
  ASender := Sender;
  AKey   := Key;
  AShift := Shift;
  id     := EXTF_GetEventID(etKeyDown, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

Procedure TEventContentHolder.OnKeyUp(Sender : TObject; Var Key : Word; Shift : TShiftState);
Var id  : Integer;
Begin
  ASender := Sender;
  AKey   := Key;
  AShift := Shift;
  id     := EXTF_GetEventID(etKeyUp, TControl(Sender));
  If id > -1 Then
    EXTF_EventLinks[id].DoCall := True;
End;

//==============================================================================
//==============================================================================

Procedure _CreateForm(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Form  : TForm;
Begin
  Form := TUnownedForm.CreateNew(nil);

  Form.Hide;
  Form.Caption     := 'RUTIS Formular';
  Form.BorderStyle := bsSizeable;
  Form.Position    := poScreenCenter;
  If PBoolean(Params^[0].Data)^ Then
    Form.Show;

  EXTF_CurrentForm    := Form;
  EXTF_CurrentControl := EXTF_CurrentForm;

  SetLength(EXTF_Forms, length(EXTF_Forms) + 1);
  EXTF_Forms[high(EXTF_Forms)] := Form;

  PPointer(Result^.Data)^ := Form;
End;

Procedure _CreateControl(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  ClassName  : String;
  Control  : TControl;
  ControlParent  : TControl;
  i  : Integer;
Begin
  Control       := nil;
  PControl(Result^.Data)^ := Control;
  ClassName     := LowerCase(PAnsiString(Params^[1].Data)^);
  ControlParent := PControl(Params^[0].Data)^;
  If ControlParent = nil Then ControlParent := EXTF_CurrentControl;

  For I := Low(ControlClasses) To high(ControlClasses) Do
    If ClassName = LowerCase(ControlClasses[i].ClassName) Then
      Control := ControlClasses[i].Create(ControlParent);
  If Control = nil Then exit;

  While not (ControlParent is TWinControl) Do
    ControlParent := ControlParent.Parent;
  Control.Parent := TWinControl(ControlParent);

  PControl(Result^.Data)^ := Control;
End;

Procedure _CreateComponent(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  ClassName       : String;
  Component       : TComponent;
  ComponentParent : TControl;
  i  : Integer;
Begin
  Component                 := nil;
  PComponent(Result^.Data)^ := nil;
  ClassName                 := LowerCase(PAnsiString(Params^[1].Data)^);
  ComponentParent           := PControl(Params^[0].Data)^;
  If ComponentParent = nil Then ComponentParent := EXTF_CurrentControl;

  For I := Low(ComponentClasses) To high(ComponentClasses) Do
    If ClassName = LowerCase(ComponentClasses[i].ClassName) Then
      Component := ComponentClasses[i].Create(ComponentParent);
  If Component = nil Then exit;

  PComponent(Result^.Data)^ := Component;
End;

Procedure _DestroyControl(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  i        : Integer;
  Control  : TControl;
Begin
  Control := TControl(PPointer(Params^[0].Data)^);
  For I := 0 To high(EXTF_Forms) Do
    If EXTF_Forms[i] = Control Then
    Begin
      EXTF_Forms[i] := EXTF_Forms[high(EXTF_Forms)];
      SetLength(EXTF_Forms, length(EXTF_Forms) - 1);
      break;
    End;
  Control.Free;
End;

Procedure _RegisterEvent(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  EName  : String;
  EType  : TEXTF_EventType;
  Crtl   : TControl;
  I      : Integer;
Begin
  Crtl  := PControl(Params^[0].Data)^;
  EName := LowerCase(PAnsiString(Params^[1].Data)^);

  For EType := TEXTF_EventType(0) To etNone Do
    If LowerCase(EventNames[EType]) = EName Then
      break;
  If EType = etNone Then exit;

  RegisterObjectEvent(Crtl, 'Onclick', @TEventCOntentHolder.Onclick);
  RegisterObjectEvent(Crtl, 'OnChange', @TEventCOntentHolder.OnChange);
  RegisterObjectEvent(Crtl, 'OnResize', @TEventCOntentHolder.OnResize);
  RegisterObjectEvent(Crtl, 'OnTimer', @TEventCOntentHolder.OnTimer);
  RegisterObjectEvent(Crtl, 'OnMousedown', @TEventCOntentHolder.OnMousedown);
  RegisterObjectEvent(Crtl, 'OnMouseup', @TEventCOntentHolder.OnMouseup);
  RegisterObjectEvent(Crtl, 'OnMouseMove', @TEventCOntentHolder.OnMouseMove);
  RegisterObjectEvent(Crtl, 'OnKeypress', @TEventCOntentHolder.OnKeypress);
  RegisterObjectEvent(Crtl, 'OnKeydown', @TEventCOntentHolder.OnKeydown);
  RegisterObjectEvent(Crtl, 'OnKeyup', @TEventCOntentHolder.OnKeyup);

  For I := 0 To high(EXTF_EventLinks) Do
    If (EXTF_EventLinks[i].Control = Crtl) and
      (EXTF_EventLinks[i].EventType = EType) Then
    Begin
      If PCardinal(Params^[2].Data)^ = 0 Then
      Begin
        EXTF_EventLinks[i] := EXTF_EventLinks[high(EXTF_EventLinks)];
        SetLength(EXTF_EventLinks, length(EXTF_EventLinks) - 1);
        exit;
      End;
      EXTF_EventLinks[i].Address := PCardinal(Params^[2].Data)^;
      exit;
    End;

  SetLength(EXTF_EventLinks, length(EXTF_EventLinks) + 1);
  With EXTF_EventLinks[high(EXTF_EventLinks)] Do
  Begin
    Control   := Crtl;
    Address   := PCardinal(Params^[2].Data)^;
    EventType := EType;
    DoCall    := False;
  End;
End;

Procedure _CallEvents(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  EXTF_CallEvents;
End;

Procedure _WaitForEvent(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  While not EXTF_CallEvents Do
  Begin
    Application.ProcessMessages;
    sleep(10);
  End;
End;

Procedure _EventParamX(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PInteger(Result^.Data)^ := EventContentHolder.Ax;
End;

Procedure _EventParamY(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PInteger(Result^.Data)^ := EventContentHolder.Ay;
End;

Procedure _EventParamSender(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  TObject(PPointer(Result^.Data)^) := EventContentHolder.ASender;
End;

Procedure _EventKey(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PWord(Result^.Data)^ := EventContentHolder.AKey;
End;

Procedure _EventParamShiftState(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Type
  PShiftState = ^TShiftState;
Begin
  PShiftState(Result^.Data)^ := EventContentHolder.AShift;
End;

Procedure _EventParamIsControl(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := ssCtrl in EventContentHolder.AShift;
End;

Procedure _EventParamIsAlt(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := ssAlt in EventContentHolder.AShift;
End;

Procedure _EventParamIsShift(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := ssShift in EventContentHolder.AShift;
End;

Procedure _EventParamIsLeft(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := ssLeft in EventContentHolder.AShift;
End;

Procedure _EventParamIsRight(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := ssRight in EventContentHolder.AShift;
End;

Procedure _EventParamIsMiddle(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := ssMiddle in EventContentHolder.AShift;
End;

//==============================================================================
//==============================================================================

Procedure _SetPos(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TControl Then
  Begin
    Control.Left := PInteger(Params^[1].Data)^;
    Control.Top := PInteger(Params^[2].Data)^;
  End;
End;

Procedure _SetSize(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TForm Then
  Begin
    TForm(Control).ClientWidth := PInteger(Params^[1].Data)^;
    TForm(Control).ClientHeight := PInteger(Params^[2].Data)^;
    exit;
  End;
  If Control is TControl Then
  Begin
    Control.Width  := PInteger(Params^[1].Data)^;
    Control.Height := PInteger(Params^[2].Data)^;
  End;
End;

Procedure _SetVisibility(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control         := TControl(PPointer(Params^[0].Data)^);
  Control.Visible := PBoolean(Params^[0].Data)^;
End;

Procedure _GetVisibility(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := TControl(PPointer(Params^[0].Data)^);
  PBoolean(Result^.Data)^ := Control.Visible;
End;

Procedure _GetDC(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PCardinal(Result^.Data)^ := GetDC(PCardinal(Params^[0].Data)^);
End;

Procedure _GetHandle(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TWinControl Then
    PCardinal(Result^.Data)^ := TWinControl(Control).Handle
  Else
  If Control is TControl Then
    PCardinal(Result^.Data)^ := Control.Parent.Handle;
End;

Procedure _AddLine(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TCustomListBox Then
    TCustomListBox(Control).Items.Add(PAnsiString(Params^[1].Data)^);
  If Control is TCustomMemo Then
    TCustomMemo(Control).Lines.Add(PAnsiString(Params^[1].Data)^);
End;

Procedure _InsertLine(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TCustomListBox Then
    TCustomListBox(Control).Items.Insert(PInteger(Params^[1].Data)^, PAnsiString(Params^[2].Data)^);
  If Control is TCustomMemo Then
    TCustomMemo(Control).Lines.Insert(PInteger(Params^[1].Data)^, PAnsiString(Params^[2].Data)^);
End;

Procedure _DeleteLine(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TCustomListBox Then
    TCustomListBox(Control).Items.Delete(PInteger(Params^[1].Data)^);
  If Control is TCustomMemo Then
    TCustomMemo(Control).Lines.Delete(PInteger(Params^[1].Data)^);
End;

Procedure _SetLine(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TCustomListBox Then
    TCustomListBox(Control).Items[PInteger(Params^[1].Data)^] := PAnsiString(Params^[2].Data)^;
  If Control is TCustomMemo Then
    TCustomMemo(Control).Lines[PInteger(Params^[1].Data)^] := PAnsiString(Params^[2].Data)^;
End;

Procedure _SetLineCount(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Control  : TControl;
Begin
  Control := PControl(Params^[0].Data)^;
  If Control is TCustomListBox Then
    While TCustomListBox(Control).Items.Count > PInteger(Params^[1].Data)^ Do
      TCustomListBox(Control).Items.Delete(TCustomListBox(Control).Items.Count - 1);
  If Control is TCustomMemo Then
    While TCustomMemo(Control).Lines.Count > PInteger(Params^[1].Data)^ Do
      TCustomMemo(Control).Lines.Delete(TCustomMemo(Control).Lines.Count - 1);
End;

//==============================================================================
//==============================================================================

Procedure RegisterEXTMethods(Engine : TRutisEngine);
Var
  Decl  : TRutisVarType;
Begin
  OutputDebugString(PChar('Rutis_EXT_Formulars.RegisterEXTMethods  -  Registering RUTIS Formular Support'));

  Engine.RegExtMethod('CreateForm',{$IfDef FPC}@{$EndIf}_CreateForm, ['Boolean'], 'TControl',
    'Creates a RUTIS-Form' + sLineBreak +
    '#1: Visibility of form after creation' + sLineBreak +
    'Returns: Created form');
  Engine.RegExtMethod('CrtlCreate',{$IfDef FPC}@{$EndIf}_CreateControl, ['TControl', 'String'], 'TControl',
    'Creates a new Control' + sLineBreak +
    '#1 : Owner for the new Control' + sLineBreak +
    '#2 : Class-Name of the new Control' + sLineBreak +
    'Returns: Created object');
  Engine.RegExtMethod('CrtlCreateComponent',{$IfDef FPC}@{$EndIf}_CreateComponent, ['TComponent', 'String'], 'TComponent',
    'Creates a new Component' + sLineBreak +
    '#1 : Owner for the new Component' + sLineBreak +
    '#2 : Class-Name of the new Component' + sLineBreak +
    'Returns: Created Component');
  Engine.RegExtMethod('CrtlDestroy',{$IfDef FPC}@{$EndIf}_DestroyControl, [], 'TControl',
    'Destroys a Control');

  Engine.RegExtMethod('CrtlRegisterEvent',{$IfDef FPC}@{$EndIf}_RegisterEvent, ['TControl', 'String', 'Method'], '',
    'Registers an Event' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Event Name' + sLineBreak +
    '#3 : Event Handler' + sLineBreak +
    'Possible Values for #2 are:' + sLineBreak +
    '- OnClick, OnMouseDown, OnMouseUp, OnMouseMove,' + sLineBreak +
    '- OnChange,' + sLineBreak +
    '- OnKeyPress, OnKeyDown, OnKeyUp');
  Engine.RegExtMethod('CallEvents',{$IfDef FPC}@{$EndIf}_CallEvents, [], '',
    'Calls all activated events' + sLineBreak +
    'You need to link events to actions with RegisterEvent first');
  Engine.RegExtMethod('WaitForEvent',{$IfDef FPC}@{$EndIf}_WaitForEvent, [], '',
    'Waits for any Event to happen' + sLineBreak +
    'You need to link events to actions with RegisterEvent first');

  Engine.RegExtMethod('EventParamX',{$IfDef FPC}@{$EndIf}_EventParamX, [], 'Integer',
    'Returns the "X" parameter of an occurred event');
  Engine.RegExtMethod('EventParamY',{$IfDef FPC}@{$EndIf}_EventParamY, [], 'Integer',
    'Returns the "Y" parameter of an occurred event');
  Engine.RegExtMethod('EventParamSender',{$IfDef FPC}@{$EndIf}_EventParamSender, [], 'TControl',
    'Returns the "Sender" parameter of an occurred event');
  Engine.RegExtMethod('EventParamKey',{$IfDef FPC}@{$EndIf}_EventKey, [], 'Word',
    'Returns the "Key" parameter of an occurred event');
  Engine.RegExtMethod('EventParamShiftState',{$IfDef FPC}@{$EndIf}_EventParamShiftState, [], 'Cardinal',
    'Returns the "ShiftState" parameter of an occurred event');
  Engine.RegExtMethod('EventParamIsControl',{$IfDef FPC}@{$EndIf}_EventParamIsControl, [], 'Boolean',
    'Returns true, if "ssCrtl" is in he ShiftState parameter of an occurred event');
  Engine.RegExtMethod('EventParamIsAlt',{$IfDef FPC}@{$EndIf}_EventParamIsAlt, [], 'Boolean',
    'Returns true, if "ssAlt" is in he ShiftState parameter of an occurred event');
  Engine.RegExtMethod('EventParamIsShift',{$IfDef FPC}@{$EndIf}_EventParamIsShift, [], 'Boolean',
    'Returns true, if "ssShift" is in he ShiftState parameter of an occurred event');
  Engine.RegExtMethod('EventParamIsLeft',{$IfDef FPC}@{$EndIf}_EventParamIsLeft, [], 'Boolean',
    'Returns true, if "ssLeft" is in he ShiftState parameter of an occurred event');
  Engine.RegExtMethod('EventParamIsRight',{$IfDef FPC}@{$EndIf}_EventParamIsRight, [], 'Boolean',
    'Returns true, if "ssRight" is in he ShiftState parameter of an occurred event');
  Engine.RegExtMethod('EventParamIsMiddle',{$IfDef FPC}@{$EndIf}_EventParamIsMiddle, [], 'Boolean',
    'Returns true, if "ssMiddle" is in he ShiftState parameter of an occurred event');

  Engine.RegExtMethod('CrtlSetVisibility',{$IfDef FPC}@{$EndIf}_SetVisibility, ['TControl', 'Boolean'], '',
    'Sets the Visibility of an Control' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Visibility');
  Engine.RegExtMethod('CrtlGetVisibility',{$IfDef FPC}@{$EndIf}_GetVisibility, ['TControl'], 'Boolean',
    'Sets the Visibility of an Control' + sLineBreak +
    '#1 : Control' + sLineBreak +
    'Returns : Visibility of the Control');
  Engine.RegExtMethod('CrtlSetPos',{$IfDef FPC}@{$EndIf}_SetPos, ['TControl', 'Integer', 'Integer'], '',
    'Sets the Position of an Control' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Left' + sLineBreak +
    '#3 : Top');
  Engine.RegExtMethod('CrtlSetSize',{$IfDef FPC}@{$EndIf}_SetSize, ['TControl', 'Integer', 'Integer'], '',
    'Sets the Position of an Control' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Width' + sLineBreak +
    '#3 : Height');

  Engine.RegExtMethod('GetDC',{$IfDef FPC}@{$EndIf}_GetDC, ['Cardinal'], 'Cardinal',
    'Returns the DeviceContext for a specific Handle' + sLineBreak +
    '#1 : Handle' + sLineBreak +
    'Returns : DC');
  Engine.RegExtMethod('GetHandle',{$IfDef FPC}@{$EndIf}_GetHandle, ['TControl'], 'Cardinal',
    'Returns the Handle of a control (e.g. for GetDC)' + sLineBreak +
    '#1 : Control' + sLineBreak +
    'Returns : Handle');

  Engine.RegExtMethod('CrtlAddLine',{$IfDef FPC}@{$EndIf}_AddLine, ['TControl', 'String'], '',
    'Adds a new Line/Item in TMemo/TListBox/etc.' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Line to add');
  Engine.RegExtMethod('CrtlInsertLine',{$IfDef FPC}@{$EndIf}_InsertLine, ['TControl', 'Integer', 'String'], '',
    'Adds a new Line/Item in TMemo/TListBox/etc.' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Insert place' + sLineBreak +
    '#3 : Line to add');

  Engine.RegExtMethod('CrtlDeleteLine',{$IfDef FPC}@{$EndIf}_DeleteLine, ['TControl', 'Integer'], '',
    'Deletes a Line/Item in TMemo/TListBox/etc.' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Line to delete');
  Engine.RegExtMethod('CrtlSetLine',{$IfDef FPC}@{$EndIf}_SetLine, ['TControl', 'Integer', 'String'], '',
    'Sets the text of a Line/Item in TMemo/TListBox/etc.' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Line' + sLineBreak +
    '#2 : New text');
  Engine.RegExtMethod('CrtlSetLineCount',{$IfDef FPC}@{$EndIf}_SetLineCount, ['TControl', 'Integer'], '',
    'Sets the number of Lines/Items in TMemo/TListBox/etc.' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Line-count');   //CrtlSetLineCount

  OutputDebugString(PChar('Rutis_EXT_Formulars.RegisterEXTMethods  -  Successfully registered RUTIS Formular Support'));
End;

//==============================================================================
//==============================================================================

Initialization
  EventContentHolder := TEventContentHolder.Create;
Finalization
  EventContentHolder.Free;
End.

