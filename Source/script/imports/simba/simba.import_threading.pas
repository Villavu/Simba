{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportThreading(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpparser, ffi,
  simba.threading;

procedure _LapeCurrentThreadID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TThreadID(Result^) := GetCurrentThreadID();
end;

procedure _LapeMainThreadID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TThreadID(Result^) := MainThreadID;
end;

procedure _LapeRunInMainThread(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  RunInMainThread(TThreadMethod(Params^[0]^));
end;

procedure _LapeRunInThread(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  RunInThread(TThreadMethod(Params^[0]^), True);
end;

procedure ImportThreading(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Threading';

    addGlobalVar(CPUCount, 'CPU_COUNT').isConstant := True;

    addGlobalType(getBaseType(DetermineIntType(SizeOf(TThreadID), False)).createCopy(), 'TThreadID');

    addGlobalFunc('function CurrentThreadID: TThreadID', @_LapeCurrentThreadID);
    addGlobalFunc('function MainThreadID: TThreadID', @_LapeMainThreadID);

    addGlobalType('procedure() of object', 'TThreadMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
    addGlobalFunc('procedure RunInMainThread(Method: TThreadMethod)', @_LapeRunInMainThread);
    addGlobalFunc('procedure RunInThread(Method: TThreadMethod)', @_LapeRunInThread);

    ImportingSection := '';
  end;
end;

end.

