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
  simba.bitmap, simba.target;

(*
Target
======
Target related methods.
*)

(*
TSimbaTarget.SetDesktop
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetDesktop;

Sets the desktop as the target.
*)
procedure _LapeSimbaTarget_SetDesktop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetDesktop();
end;

(*
TSimbaTarget.SetImage
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetImage(TSimbaImage: TSimbaImage);

Sets the TSimbaImage as a target.

Note: Ownership of the TSimbaImage is **not** taken. Make sure you do not free the image while using this target.
*)
procedure _LapeSimbaTarget_SetImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetImage(PSimbaImage(Params^[1])^);
end;

(*
TSimbaTarget.SetWindow
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetWindow(Window: TWindowHandle);

Sets a window handle as a target.
*)
procedure _LapeSimbaTarget_SetWindow(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetWindow(PWindowHandle(Params^[1])^);
end;

(*
TSimbaTarget.SetEIOS
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetEIOS(Plugin, Args: String);

Sets a plugin (via EIOS API) as the target.
*)
procedure _LapeSimbaTarget_SetEIOS(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetEIOS(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaTarget.GetImage
~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage;
*)
procedure _LapeSimbaTarget_GetImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaTarget(Params^[0])^.GetImage(PBox(Params^[1])^);
end;

(*
TSimbaTarget.GetDimensions
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.GetDimensions(out Width, Height: Integer);
*)
procedure _LapeSimbaTarget_GetDimensions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.GetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaTarget.GetWidth
~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.GetWidth: Integer;
*)
procedure _LapeSimbaTarget_GetWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.GetWidth();
end;

(*
TSimbaTarget.GetHeight
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.GetHeight: Integer;
*)
procedure _LapeSimbaTarget_GetHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.GetHeight();
end;

(*
TSimbaTarget.IsValid
~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsValid: Boolean;
*)
procedure _LapeSimbaTarget_IsValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsValid();
end;

(*
TSimbaTarget.IsFocused
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsFocused: Boolean;
*)
procedure _LapeSimbaTarget_IsFocused(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsFocused();
end;

(*
TSimbaTarget.Focus
~~~~~~~~~~~~~~~~~~
function TSimbaTarget.Focus: Boolean;
*)
procedure _LapeSimbaTarget_Focus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.Focus();
end;

(*
TSimbaTarget.IsWindowTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsWindowTarget: Boolean;
*)
procedure _LapeSimbaTarget_IsWindowTarget1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsWindowTarget();
end;

(*
TSimbaTarget.IsWindowTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsWindowTarget(out Window: TWindowHandle): Boolean;
*)
procedure _LapeSimbaTarget_IsWindowTarget2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsWindowTarget(PWindowHandle(Params^[1])^)
end;

(*
TSimbaTarget.IsImageTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsImageTarget: Boolean;
*)
procedure _LapeSimbaTarget_IsImageTarget1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsImageTarget();
end;

(*
TSimbaTarget.IsImageTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsImageTarget(out Image: TSimbaImage): Boolean;
*)
procedure _LapeSimbaTarget_IsImageTarget2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsImageTarget(PSimbaImage(Params^[1])^);
end;

(*
TSimbaTarget.IsEIOSTarget
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsEIOSTarget: Boolean;
*)
procedure _LapeSimbaTarget_IsEIOSTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsEIOSTarget();
end;

(*
TSimbaTarget.IsDefault
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsDefault: Boolean;
*)
procedure _LapeSimbaTarget_IsDefault(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := CompareMem(PSimbaTarget(Params^[0]), @Default(TSimbaTarget), SizeOf(TSimbaTarget));
end;

procedure _LapeSimbaTarget_ClearClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.ClearClientArea();
end;

procedure _LapeSimbaTarget_SetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.ClientArea := PBox(Params^[1])^;
end;

procedure _LapeSimbaTarget_GetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PSimbaTarget(Params^[0])^.ClientArea;
end;

procedure _LapeSimbaTarget_SetAutoFocus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.AutoSetFocus := PBoolean(Params^[1])^;
end;

procedure _LapeSimbaTarget_GetAutoFocus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.AutoSetFocus;
end;

procedure _LapeSimbaTarget_AddHandlerOnInvalidTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaTarget.PInvalidTargetEvent(Result)^ := PSimbaTarget(Params^[0])^.AddHandlerOnInvalidTarget(TSimbaTarget.PInvalidTargetEvent(Params^[1])^);
end;

procedure _LapeSimbaTarget_RemoveHandlerOnInvalidTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveHandlerOnInvalidTarget(TSimbaTarget.PInvalidTargetEvent(Params^[1])^);
end;

procedure _LapeSimbaTarget_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
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
      '  InternalData: array[0..' + IntToStr(SizeOf(TSimbaTarget) - 1)  + '] of Byte;',
      '  {%CODETOOLS ON}',
      'end;'],
      'TSimbaTarget'
    );
    if (getGlobalType('TSimbaTarget').Size <> SizeOf(TSimbaTarget)) then
      raise Exception.Create('SizeOf(TSimbaTarget) is wrong!');

    with addGlobalVar('TSimbaTarget', '[]', 'Target') do
      Used := duTrue;

    addGlobalType('procedure(var Target: TSimbaTarget) of object', 'TInvalidTargetEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('function TSimbaTarget.AddHandlerOnInvalidTarget(Event: TInvalidTargetEvent): TInvalidTargetEvent', @_LapeSimbaTarget_AddHandlerOnInvalidTarget);
    addGlobalFunc('procedure TSimbaTarget.RemoveHandlerOnInvalidTarget(Event: TInvalidTargetEvent)', @_LapeSimbaTarget_RemoveHandlerOnInvalidTarget);

    addGlobalFunc('function TSimbaTarget.GetAutoFocus: Boolean', @_LapeSimbaTarget_GetAutoFocus);
    addGlobalFunc('procedure TSimbaTarget.SetAutoFocus(Value: Boolean)', @_LapeSimbaTarget_SetAutoFocus);

    addGlobalFunc('procedure TSimbaTarget.ClearClientArea', @_LapeSimbaTarget_ClearClientArea);
    addGlobalFunc('procedure TSimbaTarget.SetClientArea(B: TBox)', @_LapeSimbaTarget_SetClientArea);
    addGlobalFunc('function TSimbaTarget.GetClientArea: TBox', @_LapeSimbaTarget_GetClientArea);

    addGlobalFunc('procedure TSimbaTarget.SetDesktop', @_LapeSimbaTarget_SetDesktop);
    addGlobalFunc('procedure TSimbaTarget.SetImage(TSimbaImage: TSimbaImage)', @_LapeSimbaTarget_SetImage);
    addGlobalFunc('procedure TSimbaTarget.SetWindow(Window: TWindowHandle)', @_LapeSimbaTarget_SetWindow);
    addGlobalFunc('procedure TSimbaTarget.SetEIOS(Plugin, Args: String)', @_LapeSimbaTarget_SetEIOS);

    addGlobalFunc('function TSimbaTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage', @_LapeSimbaTarget_GetImage);
    addGlobalFunc('procedure TSimbaTarget.GetDimensions(out Width, Height: Integer)', @_LapeSimbaTarget_GetDimensions);
    addGlobalFunc('function TSimbaTarget.GetWidth: Integer', @_LapeSimbaTarget_GetWidth);
    addGlobalFunc('function TSimbaTarget.GetHeight: Integer', @_LapeSimbaTarget_GetHeight);

    addGlobalFunc('function TSimbaTarget.IsValid: Boolean', @_LapeSimbaTarget_IsValid);
    addGlobalFunc('function TSimbaTarget.IsFocused: Boolean', @_LapeSimbaTarget_IsFocused);
    addGlobalFunc('function TSimbaTarget.Focus: Boolean', @_LapeSimbaTarget_Focus);

    addGlobalFunc('function TSimbaTarget.IsWindowTarget: Boolean; overload', @_LapeSimbaTarget_IsWindowTarget1);
    addGlobalFunc('function TSimbaTarget.IsWindowTarget(out Window: TWindowHandle): Boolean; overload', @_LapeSimbaTarget_IsWindowTarget2);
    addGlobalFunc('function TSimbaTarget.IsImageTarget: Boolean; overload', @_LapeSimbaTarget_IsImageTarget1);
    addGlobalFunc('function TSimbaTarget.IsImageTarget(out TSimbaImage: TSimbaImage): Boolean; overload', @_LapeSimbaTarget_IsImageTarget2);
    addGlobalFunc('function TSimbaTarget.IsEIOSTarget: Boolean', @_LapeSimbaTarget_IsEIOSTarget);

    addGlobalFunc('function TSimbaTarget.IsDefault: Boolean', @_LapeSimbaTarget_IsDefault);

    addGlobalFunc('function ToString(constref Target: TSimbaTarget): String; override;', @_LapeSimbaTarget_ToString);


    ImportingSection := 'TSimbaImage';

    addGlobalFunc(
      'function TSimbaImage.CreateFromTarget(Target: TSimbaTarget; Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage; static; overload;', [
      'begin',
      '  Result := Target.GetImage(Bounds);',
      'end;'
    ]);
    addGlobalFunc(
      'function TSimbaImage.CreateFromTarget(Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage; static; overload;', [
      'begin',
      '  Result := TSimbaImage.CreateFromTarget(System.Target, Bounds);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure TSimbaImage.DrawTarget(Target: TSimbaTarget; P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;', [
      'var',
      '  Image: TSimbaImage;',
      'begin',
      '  Image := TSimbaImage.CreateFromTarget(Target, Bounds);',
      '  Self.Draw(Image, P);',
      '  Image.Free();',
      'end;'
    ]);
    addGlobalFunc(
      'procedure TSimbaImage.DrawTarget(P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;', [
      'begin',
      '  Self.DrawTarget(System.Target, P, Bounds);',
      'end;'
    ]);
  end;
end;

end.

