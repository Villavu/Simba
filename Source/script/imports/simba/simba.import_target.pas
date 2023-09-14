unit simba.import_target;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes, ffi,
  simba.image, simba.target, simba.externalimage;

(*
Target
======
Target related methods.
*)

(*
TTarget.SetDesktop
~~~~~~~~~~~~~~~~~~
> procedure TTarget.SetDesktop;

Sets the desktop as the target.
*)
procedure _LapeTarget_SetDesktop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetDesktop();
end;

(*
TTarget.SetImage
~~~~~~~~~~~~~~~~
> procedure TTarget.SetImage(TImage: TImage);

Sets the TSimbaImage as a target.

Note:: Ownership of the TSimbaImage is **not** taken. Make sure you do not free the image while using this target.
*)
procedure _LapeTarget_SetImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetImage(PSimbaImage(Params^[1])^);
end;

(*
TTarget.SetWindow
~~~~~~~~~~~~~~~~~
> procedure TTarget.SetWindow(Window: TWindowHandle);

Sets a window handle as a target.
*)
procedure _LapeTarget_SetWindow(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetWindow(PWindowHandle(Params^[1])^);
end;

(*
TTarget.SetEIOS
~~~~~~~~~~~~~~~
> procedure TTarget.SetEIOS(Plugin, Args: String);

Sets a plugin (via EIOS API) as the target.
*)
procedure _LapeTarget_SetEIOS(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetEIOS(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeTarget_SetPlugin1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetPlugin(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeTarget_SetPlugin2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetPlugin(PString(Params^[1])^, PString(Params^[2])^, PSimbaExternalImage(Params^[3])^);
end;

(*
TTarget.GetImage
~~~~~~~~~~~~~~~~
> function TTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TImage;
*)
procedure _LapeTarget_GetImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaTarget(Params^[0])^.GetImage(PBox(Params^[1])^);
end;

(*
TTarget.GetDimensions
~~~~~~~~~~~~~~~~~~~~~
> procedure TTarget.GetDimensions(out Width, Height: Integer);
*)
procedure _LapeTarget_GetDimensions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.GetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TTarget.GetWidth
~~~~~~~~~~~~~~~~
> function TTarget.GetWidth: Integer;
*)
procedure _LapeTarget_GetWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.GetWidth();
end;

(*
TTarget.GetHeight
~~~~~~~~~~~~~~~~~
> function TTarget.GetHeight: Integer;
*)
procedure _LapeTarget_GetHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.GetHeight();
end;

(*
TTarget.IsValid
~~~~~~~~~~~~~~~
> function TTarget.IsValid: Boolean;
*)
procedure _LapeTarget_IsValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsValid();
end;

(*
TTarget.IsFocused
~~~~~~~~~~~~~~~~~
> function TTarget.IsFocused: Boolean;
*)
procedure _LapeTarget_IsFocused(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsFocused();
end;

(*
TTarget.Focus
~~~~~~~~~~~~~
> function TTarget.Focus: Boolean;
*)
procedure _LapeTarget_Focus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.Focus();
end;

(*
TTarget.IsWindowTarget
~~~~~~~~~~~~~~~~~~~~~~
> function TTarget.IsWindowTarget: Boolean;
*)
procedure _LapeTarget_IsWindowTarget1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsWindowTarget();
end;

(*
TTarget.IsWindowTarget
~~~~~~~~~~~~~~~~~~~~~~
> function TTarget.IsWindowTarget(out Window: TWindowHandle): Boolean;
*)
procedure _LapeTarget_IsWindowTarget2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsWindowTarget(PWindowHandle(Params^[1])^)
end;

(*
TTarget.IsImageTarget
~~~~~~~~~~~~~~~~~~~~~
> function TTarget.IsImageTarget: Boolean;
*)
procedure _LapeTarget_IsImageTarget1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsImageTarget();
end;

(*
TTarget.IsImageTarget
~~~~~~~~~~~~~~~~~~~~~
> function TTarget.IsImageTarget(out TImage: TImage): Boolean;
*)
procedure _LapeTarget_IsImageTarget2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsImageTarget(PSimbaImage(Params^[1])^);
end;

(*
TTarget.IsEIOSTarget
~~~~~~~~~~~~~~~~~~~~
> function TTarget.IsEIOSTarget: Boolean;
*)
procedure _LapeTarget_IsEIOSTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsEIOSTarget();
end;

procedure _LapeTarget_IsPluginTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsPluginTarget();
end;

(*
TTarget.IsDefault
~~~~~~~~~~~~~~~~~
> function TTarget.IsDefault: Boolean;
*)
procedure _LapeTarget_IsDefault(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := CompareMem(PSimbaTarget(Params^[0]), @Default(TSimbaTarget), SizeOf(TSimbaTarget));
end;

(*
TTarget.ClearCustomClientArea
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TTarget.ClearCustomClientArea;
*)
procedure _LapeTarget_ClearCustomClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.ClearCustomClientArea();
end;

(*
TTarget.SetCustomClientArea
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TTarget.SetCustomClientArea(B: TBox);

Set a custom client area within the bounds of the target.

```
Target.SetCustomClientArea([100,100,600,600]);
Input.MouseMove([1,1]); // Will move the mouse to [101,101] on the "real" bounds
```
*)
procedure _LapeTarget_SetCustomClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.CustomClientArea := PBox(Params^[1])^;
end;

(*
TTarget.GetCustomClientArea
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TTarget.GetCustomClientArea: TBox;
*)
procedure _LapeTarget_GetCustomClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PSimbaTarget(Params^[0])^.CustomClientArea;
end;

(*
TTarget.SetAutoFocus
~~~~~~~~~~~~~~~~~~~~
> procedure TTarget.SetAutoFocus(Value: Boolean);
*)
procedure _LapeTarget_SetAutoFocus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.AutoSetFocus := PBoolean(Params^[1])^;
end;

(*
TTarget.GetAutoFocus
~~~~~~~~~~~~~~~~~~~~
> function TTarget.GetAutoFocus: Boolean;
*)
procedure _LapeTarget_GetAutoFocus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.AutoSetFocus;
end;

(*
TTarget.AddHandlerOnInvalidTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TTarget.AddHandlerOnInvalidTarget(Event: TInvalidTargetEvent): TInvalidTargetEvent;
*)
procedure _LapeTarget_AddOnInvalidTargetEvent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaTarget.TInvalidTargetEvent(Result^) := PSimbaTarget(Params^[0])^.AddOnInvalidTargetEvent(TSimbaTarget.TInvalidTargetEvent(Params^[1]^));
end;

(*
TTarget.RemoveHandlerOnInvalidTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TTarget.RemoveHandlerOnInvalidTarget(Event: TInvalidTargetEvent);
*)
procedure _LapeTarget_RemoveOnInvalidTargetEvent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveOnInvalidTargetEvent(TSimbaTarget.TInvalidTargetEvent(Params^[1]^));
end;

procedure _LapeTarget_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaTarget(Params^[0])^.ToString();
end;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Target';

    addGlobalType([
      'packed record',
      '  {%CODETOOLS OFF}',
      '  InternalData: array[1..' + IntToStr(SizeOf(TSimbaTarget) - LapeTypeSize[ltDynArray])  + '] of Byte;',
      '  {%CODETOOLS ON}',
      '  InvalidTargetEvents: array of TMethod;',
      'end;'],
      'TTarget'
    );

    with addGlobalVar('TTarget', '[]', 'Target') do
    begin
      Used := duTrue;

      if (Size <> SizeOf(TSimbaTarget)) then
        SimbaException('SizeOf(TTarget) is wrong!');
    end;

    addGlobalType('procedure(var Target: TTarget) of object', 'TInvalidTargetEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('function TTarget.AddOnInvalidTargetEvent(Event: TInvalidTargetEvent): TInvalidTargetEvent', @_LapeTarget_AddOnInvalidTargetEvent);
    addGlobalFunc('procedure TTarget.RemoveOnInvalidTargetEvent(Event: TInvalidTargetEvent)', @_LapeTarget_RemoveOnInvalidTargetEvent);

    addGlobalFunc('function TTarget.GetAutoFocus: Boolean', @_LapeTarget_GetAutoFocus);
    addGlobalFunc('procedure TTarget.SetAutoFocus(Value: Boolean)', @_LapeTarget_SetAutoFocus);

    addGlobalFunc('procedure TTarget.ClearCustomClientArea', @_LapeTarget_ClearCustomClientArea);
    addGlobalFunc('procedure TTarget.SetCustomClientArea(B: TBox)', @_LapeTarget_SetCustomClientArea);
    addGlobalFunc('function TTarget.GetCustomClientArea: TBox', @_LapeTarget_GetCustomClientArea);

    addGlobalFunc('procedure TTarget.SetDesktop', @_LapeTarget_SetDesktop);
    addGlobalFunc('procedure TTarget.SetImage(TImage: TImage)', @_LapeTarget_SetImage);
    addGlobalFunc('procedure TTarget.SetWindow(Window: TWindowHandle)', @_LapeTarget_SetWindow);
    addGlobalFunc('procedure TTarget.SetEIOS(Plugin, Args: String)', @_LapeTarget_SetEIOS);
    addGlobalFunc('procedure TTarget.SetPlugin(Plugin, Args: String); overload', @_LapeTarget_SetPlugin1);
    addGlobalFunc('procedure TTarget.SetPlugin(Plugin, Args: String; out DebugImage: TExternalImage); overload', @_LapeTarget_SetPlugin2);

    addGlobalFunc('function TTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TImage', @_LapeTarget_GetImage);
    addGlobalFunc('procedure TTarget.GetDimensions(out Width, Height: Integer)', @_LapeTarget_GetDimensions);
    addGlobalFunc('function TTarget.GetWidth: Integer', @_LapeTarget_GetWidth);
    addGlobalFunc('function TTarget.GetHeight: Integer', @_LapeTarget_GetHeight);

    addGlobalFunc('function TTarget.IsValid: Boolean', @_LapeTarget_IsValid);
    addGlobalFunc('function TTarget.IsFocused: Boolean', @_LapeTarget_IsFocused);
    addGlobalFunc('function TTarget.Focus: Boolean', @_LapeTarget_Focus);

    addGlobalFunc('function TTarget.IsWindowTarget: Boolean; overload', @_LapeTarget_IsWindowTarget1);
    addGlobalFunc('function TTarget.IsWindowTarget(out Window: TWindowHandle): Boolean; overload', @_LapeTarget_IsWindowTarget2);
    addGlobalFunc('function TTarget.IsImageTarget: Boolean; overload', @_LapeTarget_IsImageTarget1);
    addGlobalFunc('function TTarget.IsImageTarget(out TImage: TImage): Boolean; overload', @_LapeTarget_IsImageTarget2);
    addGlobalFunc('function TTarget.IsEIOSTarget: Boolean', @_LapeTarget_IsEIOSTarget);
    addGlobalFunc('function TTarget.IsPluginTarget: Boolean', @_LapeTarget_IsPluginTarget);

    addGlobalFunc('function TTarget.IsDefault: Boolean', @_LapeTarget_IsDefault);

    addGlobalFunc('function ToString(constref Target: TTarget): String; override;', @_LapeTarget_ToString);

    ImportingSection := 'Image';

    addGlobalFunc(
      'function TImage.CreateFromTarget(Target: TTarget; Bounds: TBox = [-1,-1,-1,-1]): TImage; static; overload;', [
      'begin',
      '  Result := Target.GetImage(Bounds);',
      'end;'
    ]);
    addGlobalFunc(
      'function TImage.CreateFromTarget(Bounds: TBox = [-1,-1,-1,-1]): TImage; static; overload;', [
      'begin',
      '  Result := TImage.CreateFromTarget(System.Target, Bounds);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure TImage.DrawTarget(Target: TTarget; P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;', [
      'var',
      '  Image: TImage;',
      'begin',
      '  Image := TImage.CreateFromTarget(Target, Bounds);',
      '  Self.Draw(Image, P);',
      '  Image.Free();',
      'end;'
    ]);
    addGlobalFunc(
      'procedure TImage.DrawTarget(P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;', [
      'begin',
      '  Self.DrawTarget(System.Target, P, Bounds);',
      'end;'
    ]);
  end;
end;

end.
