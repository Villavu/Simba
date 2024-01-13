unit simba.import_windowhandle;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportWindowHandle(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.windowhandle;

(*
Window Handle
=============
TWindowHandle stores a handle to a native window with methods for simple window manipulation.

Example:

```
  var
    Win: TWindowHandle;
  begin
    Win := GetActiveWindow();
    WriteLn('Active window: ');
    WriteLn('Title:  ', Win.GetTitle());
    Writeln('PID:    ', Win.GetPID());
    WriteLn('Bounds: ', Win.GetBounds());
  end;
```
*)

(*
TWindowHandle.Activate
~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.Activate: Boolean;
*)
procedure _LapeWindowHandle_Activate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.Activate();
end;

(*
TWindowHandle.IsValid
~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.IsValid: Boolean;
*)
procedure _LapeWindowHandle_IsValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.IsValid();
end;

(*
TWindowHandle.IsActive
~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.IsActive: Boolean;
*)
procedure _LapeWindowHandle_IsActive(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.IsActive();
end;

(*
TWindowHandle.IsVisible
~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.IsVisible: Boolean;
*)
procedure _LapeWindowHandle_IsVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.IsVisible();
end;

(*
TWindowHandle.GetPID
~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetPID: UInt32;
*)
procedure _LapeWindowHandle_GetPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PWindowHandle(Params^[0])^.GetPID();
end;

(*
TWindowHandle.GetRootWindow
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetRootWindow: TWindowHandle;
*)
procedure _LapeWindowHandle_GetRootWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Result)^ := PWindowHandle(Params^[0])^.GetRootWindow();
end;

(*
TWindowHandle.GetTitle
~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetTitle: String;
*)
procedure _LapeWindowHandle_GetTitle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PWindowHandle(Params^[0])^.GetTitle();
end;

(*
TWindowHandle.GetClassName
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetClassName: String;
*)
procedure _LapeWindowHandle_GetClassName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PWindowHandle(Params^[0])^.GetClassName();
end;

(*
TWindowHandle.GetTitleW
~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetTitleW: WideString;
*)
procedure _LapeWindowHandle_GetTitleW(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWideString(Result)^ := PWindowHandle(Params^[0])^.GetTitleW();
end;

(*
TWindowHandle.GetClassNameW
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetClassNameW: WideString;
*)
procedure _LapeWindowHandle_GetClassNameW(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWideString(Result)^ := PWindowHandle(Params^[0])^.GetClassNameW();
end;

(*
TWindowHandle.GetBounds
~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetBounds: TBox;
*)
procedure _LapeWindowHandle_GetBounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PWindowHandle(Params^[0])^.GetBounds();
end;

(*
TWindowHandle.GetChildren
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TWindowHandle.GetChildren(Recursive: Boolean = True): TWindowHandleArray;
*)
procedure _LapeWindowHandle_GetChildren(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandleArray(Result)^ := PWindowHandle(Params^[0])^.GetChildren(PBoolean(Params^[1])^);
end;

(*
TWindowHandle.SetBounds
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TWindowHandle.SetBounds(Bounds: TBox);
*)
procedure _LapeWindowHandle_SetBounds(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Params^[0])^.SetBounds(PBox(Params^[1])^);
end;

(*
TWindowHandle.Kill
~~~~~~~~~~~~~~~~~~
> procedure TWindowHandle.Kill;
*)
procedure _LapeWindowHandle_Kill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Params^[0])^.Kill();
end;

(*
GetTopWindows
~~~~~~~~~~~~~
> function GetTopWindows: TWindowHandleArray;
*)
procedure _LapeGetTopWindows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandleArray(Result)^ := GetTopWindows();
end;

(*
GetVisibleWindows
~~~~~~~~~~~~~~~~~
> function GetVisibleWindows: TWindowHandleArray;
*)
procedure _LapeGetVisibleWindows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandleArray(Result)^ := GetVisibleWindows();
end;

(*
GetWindows
~~~~~~~~~~
> function GetWindows: TWindowHandleArray;
*)
procedure _LapeGetWindows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandleArray(Result)^ := GetWindows();
end;

(*
GetActiveWindow
~~~~~~~~~~~~~~~
> function GetActiveWindow: TWindowHandle;
*)
procedure _LapeGetActiveWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Result)^ := GetActiveWindow();
end;

(*
GetDesktopWindow
~~~~~~~~~~~~~~~~
> function GetDesktopWindow: TWindowHandle;
*)
procedure _LapeGetDesktopWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Result)^ := GetDesktopWindow();
end;

(*
GetWindowAtCursor
~~~~~~~~~~~~~~~~~
> function GetWindowAtCursor: TWindowHandle;
*)
procedure _LapeGetWindowAtCursor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Result)^ := GetWindowAtCursor();
end;

(*
FindWindow
~~~~~~~~~~
> function FindWindow(Title: String; out Window: TWindowHandle): Boolean;
*)
procedure _LapeFindWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := FindWindow(PString(Params^[0])^, PWindowHandle(Params^[1])^);
end;

(*
FindWindows
~~~~~~~~~~~
> function FindWindows(Title: String): TWindowHandleArray;
*)
procedure _LapeFindWindows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandleArray(Result)^ := FindWindows(PString(Params^[0])^);
end;

(*
FindChildWindow
~~~~~~~~~~~~~~~
> function FindChildWindow(Title: String; ClassName: String; out Child: TWindowHandle): Boolean;
*)
procedure _LapeFindChildWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := FindChildWindow(PString(Params^[0])^, PString(Params^[1])^, PWindowHandle(Params^[2])^);
end;

(*
FindChildWindows
~~~~~~~~~~~~~~~~
> function FindChildWindows(Title: String; ClassName: String): TWindowHandleArray;
*)
procedure _LapeFindChildWindows(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandleArray(Result)^ := FindChildWindows(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure ImportWindowHandle(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TWindowHandle';

    addGlobalType('type UInt64', 'TWindowHandle');
    addGlobalType('array of TWindowHandle', 'TWindowHandleArray');

    addGlobalFunc('function TWindowHandle.Activate: Boolean;', @_LapeWindowHandle_Activate);
    addGlobalFunc('function TWindowHandle.IsValid: Boolean;', @_LapeWindowHandle_IsValid);
    addGlobalFunc('function TWindowHandle.IsActive: Boolean;', @_LapeWindowHandle_IsActive);
    addGlobalFunc('function TWindowHandle.IsVisible: Boolean;', @_LapeWindowHandle_IsVisible);
    addGlobalFunc('function TWindowHandle.GetPID: UInt32;', @_LapeWindowHandle_GetPID);
    addGlobalFunc('function TWindowHandle.GetRootWindow: TWindowHandle; ', @_LapeWindowHandle_GetRootWindow);
    addGlobalFunc('function TWindowHandle.GetClassName: String;', @_LapeWindowHandle_GetClassName);
    addGlobalFunc('function TWindowHandle.GetTitle: String;', @_LapeWindowHandle_GetTitle);
    addGlobalFunc('function TWindowHandle.GetClassNameW: WideString;', @_LapeWindowHandle_GetClassNameW);
    addGlobalFunc('function TWindowHandle.GetTitleW: WideString;', @_LapeWindowHandle_GetTitleW);
    addGlobalFunc('function TWindowHandle.GetBounds: TBox;', @_LapeWindowHandle_GetBounds);
    addGlobalFunc('function TWindowHandle.GetChildren(Recursive: Boolean = True): TWindowHandleArray;', @_LapeWindowHandle_GetChildren);
    addGlobalFunc('procedure TWindowHandle.SetBounds(Bounds: TBox);', @_LapeWindowHandle_SetBounds);
    addGlobalFunc('procedure TWindowHandle.Kill;', @_LapeWindowHandle_Kill);
    addGlobalFunc('function GetTopWindows: TWindowHandleArray', @_LapeGetTopWindows);
    addGlobalFunc('function GetVisibleWindows: TWindowHandleArray', @_LapeGetVisibleWindows);
    addGlobalFunc('function GetWindows: TWindowHandleArray', @_LapeGetWindows);
    addGlobalFunc('function GetActiveWindow: TWindowHandle', @_LapeGetActiveWindow);
    addGlobalFunc('function GetDesktopWindow: TWindowHandle', @_LapeGetDesktopWindow);
    addGlobalFunc('function GetWindowAtCursor: TWindowHandle', @_LapeGetWindowAtCursor);
    addGlobalFunc('function FindWindow(Title: String; out Window: TWindowHandle): Boolean; overload', @_LapeFindWindow);
    addGlobalFunc('function FindWindows(Title: String): TWindowHandleArray; overload', @_LapeFindWindows);
    addGlobalFunc('function FindChildWindow(Title: String; ClassName: String; out Child: TWindowHandle): Boolean; overload', @_LapeFindChildWindow);
    addGlobalFunc('function FindChildWindows(Title: String; ClassName: String): TWindowHandleArray; overload', @_LapeFindChildWindows);

    ImportingSection := '';
  end;
end;

end.