unit script_import_script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes;

procedure Lape_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Write(PString(Params^[1])^);
end;

procedure Lape_WriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).WriteLn();
end;

procedure Lape_DebugLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  WriteLn(PString(Params^[1])^);
end;

procedure Lape_SetWriteTimeStamp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PBoolean(Params^[1])^ then
    TMMLScriptThread(Params^[0]).Options := TMMLScriptThread(Params^[0]).Options + [soWriteTimeStamp]
  else
    TMMLScriptThread(Params^[0]).Options := TMMLScriptThread(Params^[0]).Options - [soWriteTimeStamp];
end;

procedure Lape_GetTimeRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := GetTickCount64() - TMMLScriptThread(Params^[0]).StartTime;
end;

procedure Lape_TerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).State := ssStop;
end;

procedure Lape_Import_Script(Compiler: TLapeCompiler; Data: Pointer);
var
  Client: Pointer = nil;
  ScriptPath: String = '';
  ScriptFile: String = '';
begin
  if (Data <> nil) then
  begin
    Client := TMMLScriptThread(Data).Client;
    ScriptPath := TMMLScriptThread(Data).ScriptFile;
    ScriptFile := TMMLScriptThread(Data).ScriptFile;
  end;

  with Compiler do
  begin
    addGlobalVar(ScriptPath, 'ScriptPath').isConstant := True;
    addGlobalVar(ScriptFile, 'ScriptFile').isConstant := True;
    addGlobalVar('TClient', Client, 'Client');

    addGlobalMethod('procedure _Write(S: string); override;', @Lape_Write, Data);
    addGlobalMethod('procedure _WriteLn; override;', @Lape_WriteLn, Data);
    addGlobalMethod('procedure DebugLn(s: string);', @Lape_DebugLn, Data);
    addGlobalMethod('procedure WriteTimeStamp(Enable: Boolean);', @Lape_SetWriteTimeStamp, Data);
    addGlobalMethod('function GetTimeRunning: UInt64;', @Lape_GetTimeRunning, Data);
    addGlobalMethod('procedure TerminateScript;', @Lape_TerminateScript, Data);

    addDelayedCode('var __OnTerminateStrings: array of String;'                       + LineEnding +
                   'var __OnTerminateMethods: array of procedure;'                    + LineEnding +
                   'var __OnTerminateObjects: array of procedure of object;'          + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure __OnTerminate;'                                         + LineEnding +
                   'var i: Int32;'                                                    + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  for i := 0 to High(__OnTerminateStrings) do'                    + LineEnding +
                   '    VariantInvoke(__OnTerminateStrings[i], []);'                  + LineEnding +
                   '  for i := 0 to High(__OnTerminateMethods) do'                    + LineEnding +
                   '    __OnTerminateMethods[i]();'                                   + LineEnding +
                   '  for i := 0 to High(__OnTerminateObjects) do'                    + LineEnding +
                   '    __OnTerminateObjects[i]();'                                   + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure AddOnTerminate(Method: String); overload;'              + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  __OnTerminateStrings += Method;'                                + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure AddOnTerminate(Method: procedure); overload;'           + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  __OnTerminateMethods += @Method;'                               + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure AddOnTerminate(Method: procedure of object); overload;' + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  __OnTerminateObjects += @Method;'                               + LineEnding +
                   'end;', 'OnTerminate');
  end;
end;

initialization
  ScriptImports.Add('Script', @Lape_Import_Script);

end.

