unit lpTIOManager_Windows;
//Depends: TIOManager, TIOManager_Abstract, string, TNativeWindow, TSysProc, Hwnd

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, script_imports;

procedure Register_TIOManager(Compiler: TLapeCompiler);

implementation

uses
  iomanager, os_windows, MufasaTypes;

type
  PIOManager = ^TIOManager;
  PNativeWindow = ^TNativeWindow;

//constructor Create;
procedure TIOManager_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^ := TIOManager.Create();
end;

//constructor Create(plugin_dir: string);
procedure TIOManager_InitEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^ := TIOManager.Create(PlpString(Params^[1])^);
end;

//function SetTarget(target: TNativeWindow): integer; overload;
procedure TIOManager_SetTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PNativeWindow(Params^[1])^);
end;

//procedure SetDesktop; override;
procedure TIOManager_SetDesktop(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetDesktop;
end;

//function GetProcesses: TSysProcArr; override;
procedure TIOManager_GetProcesses(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSysProcArr(Result)^ := PIOManager(Params^[0])^.GetProcesses();
end;

//procedure SetTargetEx(Proc: TSysProc); overload;
procedure TIOManager_SetTargetEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetTargetEx(PSysProc(Params^[1])^);
end;

//procedure Free();
procedure TIOManager_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.Free();
end;

procedure Register_TIOManager(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TIOManager', 'TIOManager_Abstract');

    addGlobalType('UInt32', 'TNativeWindow');

    addGlobalFunc('procedure TIOManager.Init(); overload;', @TIOManager_Init);
    addGlobalFunc('procedure TIOManager.Init(plugin_dir: string); overload;', @TIOManager_InitEx);
    addGlobalFunc('function TIOManager.SetTarget2(target: TNativeWindow): integer; constref;', @TIOManager_SetTarget); //inheritence issue...
    addGlobalFunc('procedure TIOManager.SetDesktop(); constref;', @TIOManager_SetDesktop);
    addGlobalFunc('function TIOManager.GetProcesses(): TSysProcArr; constref;', @TIOManager_GetProcesses);
    addGlobalFunc('procedure TIOManager.SetTargetEx(Proc: TSysProc); constref;', @TIOManager_SetTargetEx);
    addGlobalFunc('procedure TIOManager.Free(); constref;', @TIOManager_Free);
  end;
end;

end.

