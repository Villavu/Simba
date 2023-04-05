unit simba.import_target;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

implementation

uses
  lptypes,
  simba.script_compiler, simba.mufasatypes, simba.bitmap, simba.target;

(*
Target
======
Target related methods.
*)

(*
TSimbaTarget.SetDesktop
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetDesktop;
*)
procedure _LapeSimbaTarget_SetDesktop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetDesktop();
end;

(*
TSimbaTarget.SetBitmap
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetBitmap(Bitmap: TMufasaBitmap);
*)
procedure _LapeSimbaTarget_SetBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetBitmap(PMufasaBitmap(Params^[1])^);
end;

(*
TSimbaTarget.SetWindow
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetWindow(Window: TWindowHandle);
*)
procedure _LapeSimbaTarget_SetWindow(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetWindow(PWindowHandle(Params^[1])^);
end;

(*
TSimbaTarget.SetEIOS
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaTarget.SetEIOS(Plugin, Args: String);
*)
procedure _LapeSimbaTarget_SetEIOS(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetEIOS(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaTarget.GetImage
~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TMufasaBitmap;
*)
procedure _LapeSimbaTarget_GetImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := PSimbaTarget(Params^[0])^.GetImage(PBox(Params^[1])^);
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
TSimbaTarget.IsDefault
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaTarget.IsDefault: Boolean;
*)
procedure _LapeSimbaTarget_IsDefault(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := CompareMem(PSimbaTarget(Params^[0]), @Default(TSimbaTarget), SizeOf(TSimbaTarget));
end;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Target';

    addGlobalType([
      'packed record',
      '  InternalData: array[0..' + IntToStr(SizeOf(TSimbaTarget) - 1)  + '] of Byte;',
      'end;'],
      'TSimbaTarget'
    );

    with addGlobalVar('TSimbaTarget', '[]', 'Target') do
      Used := duTrue;

    addGlobalFunc('procedure TSimbaTarget.SetDesktop', @_LapeSimbaTarget_SetDesktop);
    addGlobalFunc('procedure TSimbaTarget.SetBitmap(Bitmap: TMufasaBitmap)', @_LapeSimbaTarget_SetBitmap);
    addGlobalFunc('procedure TSimbaTarget.SetWindow(Window: TWindowHandle)', @_LapeSimbaTarget_SetWindow);
    addGlobalFunc('procedure TSimbaTarget.SetEIOS(Plugin, Args: String)', @_LapeSimbaTarget_SetEIOS);

    addGlobalFunc('function TSimbaTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TMufasaBitmap', @_LapeSimbaTarget_GetImage);
    addGlobalFunc('procedure TSimbaTarget.GetDimensions(out Width, Height: Integer)', @_LapeSimbaTarget_GetDimensions);
    addGlobalFunc('function TSimbaTarget.GetWidth: Integer', @_LapeSimbaTarget_GetWidth);
    addGlobalFunc('function TSimbaTarget.GetHeight: Integer', @_LapeSimbaTarget_GetHeight);

    addGlobalFunc('function TSimbaTarget.IsDefault: Boolean', @_LapeSimbaTarget_IsDefault);

    ImportingSection := 'TMufasaBitmap';

    addGlobalFunc(
      'function TMufasaBitmap.CreateFromTarget: TMufasaBitmap; static; overload;', [
      'begin',
      '  Result := System.Target.GetImage();',
      'end;'
    ]);
    addGlobalFunc(
      'function TMufasaBitmap.CreateFromTarget(Target: TSimbaTarget): TMufasaBitmap; static; overload;', [
      'begin',
      '  Result := Target.GetImage();',
      'end;'
    ]);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportTarget);

end.

