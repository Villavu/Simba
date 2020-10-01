unit simbascript.import_oswindow;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_OSWindow(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.oswindow;

procedure Lape_OSWindow_Activate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.Activate();
end;

procedure Lape_OSWindow_IsVaild(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsValid();
end;

procedure Lape_OSWindow_IsActive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsActive();
end;

procedure Lape_OSWindow_IsActiveEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsActive(PInt32(Params^[1])^);
end;

procedure Lape_OSWindow_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsVisible();
end;

procedure Lape_OSWindow_GetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := POSWindow(Params^[0])^.GetPID();
end;

procedure Lape_OSWindow_GetRootWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := POSWindow(Params^[0])^.GetRootWindow();
end;

procedure Lape_OSWindow_GetTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := POSWindow(Params^[0])^.GetTitle();
end;

procedure Lape_OSWindow_GetClassName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := POSWindow(Params^[0])^.GetClassName();
end;

procedure Lape_OSWindow_GetBounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := POSWindow(Params^[0])^.GetBounds();
end;

procedure Lape_OSWindow_GetChildren(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindow(Params^[0])^.GetChildren(PBoolean(Params^[1])^);
end;

procedure Lape_OSWindow_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Params^[0])^.SetBounds(PBox(Params^[1])^);
end;

procedure Lape_OSWindow_Kill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Params^[0])^.Kill();
end;

procedure Lape_OSWindowArray_GetByTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[0])^.GetByTitle(PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure Lape_OSWindowArray_GetByTitleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[0])^.GetByTitle(PString(Params^[1])^);
end;

procedure Lape_OSWindowArray_GetByClass(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[0])^.GetByClass(PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure Lape_OSWindowArray_GetByClassEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[0])^.GetByClass(PString(Params^[1])^);
end;

procedure Lape_OSWindowArray_GetByTitleAndClass(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[0])^.GetByTitleAndClass(PString(Params^[1])^, PString(Params^[2])^, POSWindow(Params^[3])^);
end;

procedure Lape_OSWindowArray_GetByTitleAndClassEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[0])^.GetByTitleAndClass(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_OSWindowArray_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := POSWindowArray(Params^[0])^.ToString();
end;

procedure Lape_GetTopWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetTopWindows();
end;

procedure Lape_GetVisibleWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetVisibleWindows();
end;

procedure Lape_GetWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetWindows();
end;

procedure Lape_GetActiveWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetActiveWindow();
end;

procedure Lape_GetDesktopWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetDesktopWindow();
end;

procedure Lape_FindWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := FindWindow(PString(Params^[0])^);
end;

procedure Lape_FindWindowEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindWindow(PString(Params^[0])^, POSWindow(Params^[1])^);
end;

procedure Lape_FindChildWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := FindChildWindow(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_FindChildWindowEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindChildWindow(PString(Params^[0])^, PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure Lape_GetWindowAtCursor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetWindowAtCursor();
end;

procedure Lape_Import_OSWindow(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'OSWindow';

    addGlobalType('type PtrUInt', 'TOSWindow');
    addGlobalType('array of TOSWindow', 'TOSWindowArray');

    addGlobalFunc('function TOSWindow.Activate: Boolean; constref;', @Lape_OSWindow_Activate);
    addGlobalFunc('function TOSWindow.IsVaild: Boolean; constref;', @Lape_OSWindow_IsVaild);
    addGlobalFunc('function TOSWindow.IsActive: Boolean; constref; overload;', @Lape_OSWindow_IsActive);
    addGlobalFunc('function TOSWindow.IsActive(Time: Int32): Boolean; constref; overload;', @Lape_OSWindow_IsActiveEx);
    addGlobalFunc('function TOSWindow.IsVisible: Boolean; constref;', @Lape_OSWindow_IsVisible);
    addGlobalFunc('function TOSWindow.GetPID: UInt32; constref;', @Lape_OSWindow_GetPID);
    addGlobalFunc('function TOSWindow.GetRootWindow: TOSWindow; constref; ', @Lape_OSWindow_GetRootWindow);
    addGlobalFunc('function TOSWindow.GetClassName: WideString; constref;', @Lape_OSWindow_GetClassName);
    addGlobalFunc('function TOSWindow.GetTitle: WideString; constref;', @Lape_OSWindow_GetTitle);
    addGlobalFunc('function TOSWindow.GetBounds: TBox; constref;', @Lape_OSWindow_GetBounds);
    addGlobalFunc('function TOSWindow.GetChildren(Recursive: Boolean = True): TOSWindowArray; constref;', @Lape_OSWindow_GetChildren);
    addGlobalFunc('procedure TOSWindow.SetBounds(Bounds: TBox); constref;', @Lape_OSWindow_SetBounds);
    addGlobalFunc('procedure TOSWindow.Kill; constref;', @Lape_OSWindow_Kill);

    addGlobalFunc('function TOSWindowArray.GetByTitle(Title: String; out Window: TOSWindow): Boolean; constref; overload;', @Lape_OSWindowArray_GetByTitle);
    addGlobalFunc('function TOSWindowArray.GetByTitle(Title: String): TOSWindowArray; constref; overload;', @Lape_OSWindowArray_GetByTitleEx);

    addGlobalFunc('function TOSWindowArray.GetByClass(ClassName: String; out Window: TOSWindow): Boolean; constref; overload;', @Lape_OSWindowArray_GetByClass);
    addGlobalFunc('function TOSWindowArray.GetByClass(ClassName: String): TOSWindowArray; constref; overload;', @Lape_OSWindowArray_GetByClassEx);

    addGlobalFunc('function TOSWindowArray.GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean; overload;', @Lape_OSWindowArray_GetByTitleAndClass);
    addGlobalFunc('function TOSWindowArray.GetByTitleAndClass(Title, ClassName: String): TOSWindowArray; overload;', @Lape_OSWindowArray_GetByTitleAndClassEx);

    addGlobalFunc('function TOSWindowArray.ToString: String; constref;', @Lape_OSWindowArray_ToString);

    addGlobalFunc('function GetTopWindows: TOSWindowArray', @Lape_GetTopWindows);
    addGlobalFunc('function GetVisibleWindows: TOSWindowArray;', @Lape_GetVisibleWindows);
    addGlobalFunc('function GetWindows: TOSWindowArray;', @Lape_GetWindows);
    addGlobalFunc('function GetActiveWindow: TOSWindow;', @Lape_GetActiveWindow);
    addGlobalFunc('function GetDesktopWindow: TOSWindow;', @Lape_GetDesktopWindow);
    addGlobalFunc('function GetWindowAtCursor: TOSWindow;', @Lape_GetWindowAtCursor);

    addGlobalFunc('function FindWindow(Title: String): TOSWindowArray; overload;', @Lape_FindWindow);
    addGlobalFunc('function FindWindow(Title: String; out Window: TOSWindow): Boolean; overload;', @Lape_FindWindowEx);
    addGlobalFunc('function FindChildWindow(Title: String; ClassName: String): TOSWindowArray; overload;', @Lape_FindChildWindow);
    addGlobalFunc('function FindChildWindow(Title: String; ClassName: String; out Child: TOSWindow): Boolean; overload;', @Lape_FindChildWindowEx);
  end;
end;

end.


