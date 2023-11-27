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
  lptypes, lpparser, ffi;

// All this is because we use cdecl on win32...
type
  TScriptThreadMethod = procedure() of object; cdecl;

  TSyncObject = object
    FMethod: TScriptThreadMethod;

    procedure Execute;
  end;

  TThreadObject = class(TThread)
  protected
    FMethod: TScriptThreadMethod;

    procedure Execute; override;
  public
    constructor Create(Method: TScriptThreadMethod); reintroduce;
  end;

procedure TThreadObject.Execute;
begin
  try
    if Assigned(FMethod) then
      FMethod();
  except
    on E: Exception do
      DebugLn('RunInThread exception: ' + E.Message);
  end;
end;

constructor TThreadObject.Create(Method: TScriptThreadMethod);
begin
  inherited Create(False, DefaultStackSize div 2);

  FMethod := Method;

  FreeOnTerminate := True;
end;

procedure TSyncObject.Execute;
begin
  try
    if Assigned(FMethod) then
      FMethod();
  except
    on E: Exception do
      DebugLn('RunOnMainThread exception: ' + E.Message);
  end;
end;

procedure _LapeCurrentThreadID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TThreadID(Result^) := GetCurrentThreadID();
end;

procedure _LapeMainThreadID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TThreadID(Result^) := MainThreadID;
end;

procedure _LapeRunInMainThread(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
var
  {%H-}SyncObject: TSyncObject;
begin
  SyncObject := Default(TSyncObject);
  SyncObject.FMethod := TScriptThreadMethod(Params^[0]^);

  TThread.Synchronize(nil, @SyncObject.Execute);
end;

procedure _LapeRunInThread(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TThreadObject.Create(TScriptThreadMethod(Params^[0]^));
end;

procedure ImportThreading(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Threading';

    addGlobalVar(CPUCount, 'CPU_COUNT').isConstant := True;

    addGlobalType(getBaseType(DetermineIntType(SizeOf(TThreadID), False)).createCopy(), 'TThreadID');
    addGlobalType('procedure() of object', 'TThreadMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});

    addGlobalFunc('function CurrentThreadID: TThreadID', @_LapeCurrentThreadID);
    addGlobalFunc('function MainThreadID: TThreadID', @_LapeMainThreadID);

    addGlobalFunc('procedure RunInMainThread(Method: TThreadMethod)', @_LapeRunInMainThread);
    addGlobalFunc('procedure RunInThread(Method: TThreadMethod)', @_LapeRunInThread);

    ImportingSection := '';
  end;
end;

end.

