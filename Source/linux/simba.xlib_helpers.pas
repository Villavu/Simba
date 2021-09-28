unit simba.xlib_helpers;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, x,
  simba.xlib;

type
  TWindowArray = array of TWindow;

function XQueryTree(Display: PDisplay; Window: TWindow; out Root: TWindow; out Parent: TWindow; out Children: TWindowArray; out Count: Int32): Boolean;
function XGetWindowProperty(Display: PDisplay; Window: TWindow; Prop: TAtom): Int64;
function XHasWindowProperty(Display: PDisplay; Window: TWindow; Name: String): Boolean;
procedure XGetKeyCode(Display: PDisplay; Key: Char; out KeyCode, ModifierCode: TKeyCode); overload;
function XGetKeyCode(Display: PDisplay; VirtualKey: UInt16): TKeyCode; overload;
function XGetWindowTitle(Display: PDisplay; Window: TWindow): String;
function XGetWindowClass(Display: PDisplay; Window: TWindow): String;
function XGetRootWindow(Display: PDisplay; Window: TWindow): TWindow;
procedure XSetActiveWindow(Display: PDisplay; Window: TWindow);
function XGetActiveWindow(Display: PDisplay): TWindow;
function XGetChildren(Display: PDisplay; Window: TWindow; Recursive: Boolean): TWindowArray;

implementation

uses
  lcltype, keysym;

function XQueryTree(Display: PDisplay; Window: TWindow; out Root: TWindow; out Parent: TWindow; out Children: TWindowArray; out Count: Int32): Boolean;
var
  Data: ^TWindow;
  i: Int32;
begin
  SetLength(Children, 0);

  if (simba.xlib.XQueryTree(Display, Window, @Root, @Parent, @Data, @Count) <> 0) then
  begin
    if (Data <> nil) then
    begin
      SetLength(Children, Count);
      for i := 0 to High(Children) do
        Children[i] := Data[i];

      XFree(Data);
     end;

    Exit(True);
  end;

  Exit(False);
end;

function XGetWindowProperty(Display: PDisplay; Window: TWindow; Prop: TAtom): Int64;
var
  Atom: TAtom;
  Format, Count, Bytes: Int32;
  Data: PByte;
begin
  Result := -1;

  if simba.xlib.XGetWindowProperty(Display, Window, Prop, 0, 2, 0, AnyPropertyType, @Atom, @Format, @Count, @Bytes, @Data) = Success then
  begin
    if (Data <> nil) then
    begin
      Result := PInt64(Data)^;

      XFree(Data);
    end;
  end;
end;

function XHasWindowProperty(Display: PDisplay; Window: TWindow; Name: String): Boolean;
begin
  Result := XGetWindowProperty(Display, Window, XInternAtom(Display, PChar(Name), TBool(False))) > 0;
end;

procedure XGetKeyCode(Display: PDisplay; Key: Char; out KeyCode, ModifierCode: TKeyCode);

  function GetSpecialKeySym(Key: Char): TKeySym;
  begin
    Result := 0;

    case Key of
      #9 : Result := XK_TAB;
      #10: Result := XK_RETURN;
      #32: Result := XK_SPACE;
      #34: Result := XK_QUOTEDBL;
      #39: Result := XK_APOSTROPHE;
      '!': Result := XK_EXCLAM;
      '#': Result := XK_NUMBERSIGN;
      '%': Result := XK_PERCENT;
      '$': Result := XK_DOLLAR;
      '&': Result := XK_AMPERSAND;
      '(': Result := XK_PARENLEFT;
      ')': Result := XK_PARENRIGHT;
      '=': Result := XK_EQUAL;
      ',': Result := XK_COMMA;
      '.': Result := XK_PERIOD;
      ':': Result := XK_COLON;
      ';': Result := XK_SEMICOLON;
      '<': Result := XK_LESS;
      '>': Result := XK_GREATER;
      '?': Result := XK_QUESTION;
      '@': Result := XK_AT;
      '[': Result := XK_BRACKETLEFT;
      ']': Result := XK_BRACKETRIGHT;
      '\': Result := XK_BACKSLASH;
      '^': Result := XK_ASCIICIRCUM;
      '_': Result := XK_UNDERSCORE;
      '`': Result := XK_GRAVE;
      '{': Result := XK_BRACELEFT;
      '|': Result := XK_BAR;
      '}': Result := XK_BRACERIGHT;
      '~': Result := XK_ASCIITILDE;
      '+': Result := XK_PLUS;
      '-': Result := XK_MINUS;
      '*': Result := XK_ASTERISK;
      '/': Result := XK_SLASH;
    end;
  end;

var
  Symbol: TKeySym;
  Index: Int32;
begin
  KeyCode := 0;
  ModifierCode := 0;

  if GetSpecialKeySym(Key) <> 0 then
    Symbol := GetSpecialKeySym(Key)
  else
    Symbol := XStringToKeysym(PChar(String(Key)));

  KeyCode := XKeySymToKeycode(Display, Symbol);
  if (KeyCode = 0) then
    raise Exception.Create('Unknown keycode for "' + Key + '"');

  Index := 0;
  while (Index < 8) and (XKeyCodeToKeySym(Display, KeyCode, Index) <> Symbol) do
    Inc(Index);

  if (Index <> 0) then
  begin
    Dec(Index);

    if (Index = ShiftMapIndex) then
      ModifierCode := XKeysymToKeycode(Display, XK_Shift_L)
    else
    if (Index >= Mod1MapIndex) then
      ModifierCode := XKeysymToKeycode(Display, XK_ISO_Level3_Shift);

    if (ModifierCode = 0) then
      raise Exception.Create('Unknown keycode for *modifier* "' + Key + '"');
  end;
end;

function XGetWindowTitle(Display: PDisplay; Window: TWindow): String;
var
  Struct: TXTextProperty;
begin
  Result := '';

  if (XGetWMName(Display, Window, @Struct) <> 0) and (Struct.NItems > 0) and (Struct.Value <> nil) then
  begin
    SetLength(Result, Struct.NItems);
    Move(Struct.value^, Result[1], Length(Result));

    XFree(Struct.Value);
  end;
end;

function XGetWindowClass(Display: PDisplay; Window: TWindow): String;
var
  Hint: TXClassHint;
begin
  Result := '';

  if (XGetClassHint(Display, Window, @Hint) <> 0) then
  begin
    Result := Hint.Res_Name;

    if (Hint.Res_Name <> nil) then
      XFree(Hint.Res_Name);
    if (Hint.Res_Class <> nil) then
      XFree(Hint.Res_Class);
  end;
end;

function XGetRootWindow(Display: PDisplay; Window: TWindow): TWindow;
var
  Root, Parent: TWindow;
  Children: TWindowArray;
  Count: Int32;
begin
  Result := Window;
  if XHasWindowProperty(Display, Result, 'WM_STATE') then
    Exit;

  while (Window > 0) and XQueryTree(Display, Window, Root, Parent, Children, Count) do
  begin
    if (Parent > 0) and XHasWindowProperty(Display, Parent, 'WM_STATE') then
      Exit(Parent);

    Window := Parent;
  end;
end;

procedure XSetActiveWindow(Display: PDisplay; Window: TWindow);
var
  Event: TXClientMessageEvent;
  Struct: TXWindowAttributes;
begin
  Window := XGetRootWindow(Display, Window);

  Event := Default(TXClientMessageEvent);
  Event._Type := ClientMessage;
  Event.Display := Display;
  Event.Window := Window;
  Event.Message_Type := XInternAtom(Display, '_NET_ACTIVE_WINDOW', TBool(False));
  Event.Format := 32;
  Event.Data.L[0] := 2;
  Event.Data.L[1] := CurrentTime;

  if XGetWindowAttributes(Display, Window, @Struct) <> 0 then
    XSendEvent(Display, Struct.Screen^.Root, TBool(False), SubstructureNotifyMask or SubstructureRedirectMask, @Event);

  XSync(Display, 0);
end;

function XGetActiveWindow(Display: PDisplay): TWindow;
begin
  Result := XGetWindowProperty(Display, XDefaultRootWindow(Display), XInternAtom(Display, '_NET_ACTIVE_WINDOW', TBool(False)));
end;

function XGetKeyCode(Display: PDisplay; VirtualKey: UInt16): TKeyCode;
var
  Symbol: TKeySym;
begin
  case VirtualKey of
    VK_BACK:      Symbol := XK_BackSpace;
    VK_TAB:       Symbol := XK_Tab;
    VK_CLEAR:     Symbol := XK_Clear;
    VK_RETURN:    Symbol := XK_Return;
    VK_SHIFT:     Symbol := XK_Shift_L;
    VK_CONTROL:   Symbol := XK_Control_L;
    VK_MENU:      Symbol := XK_Alt_R;
    VK_CAPITAL:   Symbol := XK_Caps_Lock;

    VK_ESCAPE:    Symbol := XK_Escape;
    VK_SPACE:     Symbol := XK_Space;
    VK_PRIOR:     Symbol := XK_Prior;
    VK_NEXT:      Symbol := XK_Next;
    VK_END:       Symbol := XK_End;
    VK_HOME:      Symbol := XK_Home;
    VK_LEFT:      Symbol := XK_Left;
    VK_UP:        Symbol := XK_Up;
    VK_RIGHT:     Symbol := XK_Right;
    VK_DOWN:      Symbol := XK_Down;
    VK_SELECT:    Symbol := XK_Select;
    VK_PRINT:     Symbol := XK_Print;
    VK_EXECUTE:   Symbol := XK_Execute;

    VK_INSERT:    Symbol := XK_Insert;
    VK_DELETE:    Symbol := XK_Delete;
    VK_HELP:      Symbol := XK_Help;
    VK_0:         Symbol := XK_0;
    VK_1:         Symbol := XK_1;
    VK_2:         Symbol := XK_2;
    VK_3:         Symbol := XK_3;
    VK_4:         Symbol := XK_4;
    VK_5:         Symbol := XK_5;
    VK_6:         Symbol := XK_6;
    VK_7:         Symbol := XK_7;
    VK_8:         Symbol := XK_8;
    VK_9:         Symbol := XK_9;

    VK_A:         Symbol := XK_A;
    VK_B:         Symbol := XK_B;
    VK_C:         Symbol := XK_C;
    VK_D:         Symbol := XK_D;
    VK_E:         Symbol := XK_E;
    VK_F:         Symbol := XK_F;
    VK_G:         Symbol := XK_G;
    VK_H:         Symbol := XK_H;
    VK_I:         Symbol := XK_I;
    VK_J:         Symbol := XK_J;
    VK_K:         Symbol := XK_K;
    VK_L:         Symbol := XK_L;
    VK_M:         Symbol := XK_M;
    VK_N:         Symbol := XK_N;
    VK_O:         Symbol := XK_O;
    VK_P:         Symbol := XK_P;
    VK_Q:         Symbol := XK_Q;
    VK_R:         Symbol := XK_R;
    VK_S:         Symbol := XK_S;
    VK_T:         Symbol := XK_T;
    VK_U:         Symbol := XK_U;
    VK_V:         Symbol := XK_V;
    VK_W:         Symbol := XK_W;
    VK_X:         Symbol := XK_X;
    VK_Y:         Symbol := XK_Y;
    VK_Z:         Symbol := XK_Z;

    VK_NUMPAD0:   Symbol := XK_KP_0;
    VK_NUMPAD1:   Symbol := XK_KP_1;
    VK_NUMPAD2:   Symbol := XK_KP_2;
    VK_NUMPAD3:   Symbol := XK_KP_3;
    VK_NUMPAD4:   Symbol := XK_KP_4;
    VK_NUMPAD5:   Symbol := XK_KP_5;
    VK_NUMPAD6:   Symbol := XK_KP_6;
    VK_NUMPAD7:   Symbol := XK_KP_7;
    VK_NUMPAD8:   Symbol := XK_KP_8;
    VK_NUMPAD9:   Symbol := XK_KP_9;
    VK_MULTIPLY:  Symbol := XK_KP_Multiply;
    VK_ADD:       Symbol := XK_KP_Add;
    VK_SEPARATOR: Symbol := XK_KP_Separator;
    VK_SUBTRACT:  Symbol := XK_KP_Subtract;
    VK_DECIMAL:   Symbol := XK_KP_Decimal;
    VK_DIVIDE:    Symbol := XK_KP_Divide;
    VK_F1:        Symbol := XK_F1;
    VK_F2:        Symbol := XK_F2;
    VK_F3:        Symbol := XK_F3;
    VK_F4:        Symbol := XK_F4;
    VK_F5:        Symbol := XK_F5;
    VK_F6:        Symbol := XK_F6;
    VK_F7:        Symbol := XK_F7;
    VK_F8:        Symbol := XK_F8;
    VK_F9:        Symbol := XK_F9;
    VK_F10:       Symbol := XK_F10;
    VK_F11:       Symbol := XK_F11;
    VK_F12:       Symbol := XK_F12;
    VK_F13:       Symbol := XK_F13;
    VK_F14:       Symbol := XK_F14;
    VK_F15:       Symbol := XK_F15;
    VK_F16:       Symbol := XK_F16;
    VK_F17:       Symbol := XK_F17;
    VK_F18:       Symbol := XK_F18;
    VK_F19:       Symbol := XK_F19;
    VK_F20:       Symbol := XK_F20;
    VK_F21:       Symbol := XK_F21;
    VK_F22:       Symbol := XK_F22;
    VK_F23:       Symbol := XK_F23;
    VK_F24:       Symbol := XK_F24;
    VK_NUMLOCK:   Symbol := XK_Num_Lock;
    VK_SCROLL:    Symbol := XK_Scroll_Lock;
  else
    Symbol := XK_VoidSymbol;
  end;

  Result := XKeySymToKeyCode(Display, Symbol);
end;

function XGetChildren(Display: PDisplay; Window: TWindow; Recursive: Boolean): TWindowArray;

  procedure GetChildren(Window: TWindow);
  var
    Parent, Root: TWindow;
    Children: TWindowArray;
    Count, i, j: Int32;
  begin
    if (Window > 0) then
    begin
      if XQueryTree(Display, Window, Root, Parent, Children, Count) then
      begin
        for i := 0 to High(Children) do
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Children[i];

          if Recursive then
            GetChildren(Children[i]);
        end;
      end;
    end;
  end;

begin
  SetLength(Result, 0);

  GetChildren(Window);
end;

end.

