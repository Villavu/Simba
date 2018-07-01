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

procedure Lape_IsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PBoolean(Params^[1])^ then
    PBoolean(Result)^ := (stoTerminated in TMMLScriptThread(Params^[0]).TerminateOptions) and (stoUserTerminated in TMMLScriptThread(Params^[0]).TerminateOptions)
  else
    PBoolean(Result)^ := (stoTerminated in TMMLScriptThread(Params^[0]).TerminateOptions);
end;

procedure Lape_Import_Script(Compiler: TLapeCompiler; Data: Pointer);
var
  Client: Pointer = nil;
  ScriptPath: String = '';
  ScriptFile: String = '';
begin
  if (Data <> nil) then
  begin
    Client := @TMMLScriptThread(Data).Client;
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
    addGlobalMethod('function IsTerminated(UserTerminated: Boolean = False): Boolean;', @Lape_IsTerminated, Data);

    addDelayedCode('var OnTerminateStrings: array of String;'                         + LineEnding +
                   'var OnTerminateMethods: array of procedure;'                      + LineEnding +
                   'var OnTerminateObjects: array of procedure of object;'            + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure __OnTerminate;'                                         + LineEnding +
                   'var i: Int32;'                                                    + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  for i := 0 to High(OnTerminateStrings) do'                      + LineEnding +
                   '    VariantInvoke(OnTerminateStrings[i], []);'                    + LineEnding +
                   '  for i := 0 to High(OnTerminateMethods) do'                      + LineEnding +
                   '    OnTerminateMethods[i]();'                                     + LineEnding +
                   '  for i := 0 to High(OnTerminateObjects) do'                      + LineEnding +
                   '    OnTerminateObjects[i]();'                                     + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure AddOnTerminate(Method: String); overload;'              + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  OnTerminateStrings += Method;'                                  + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure AddOnTerminate(Method: procedure); overload;'           + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  OnTerminateMethods += @Method;'                                 + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure AddOnTerminate(Method: procedure of object); overload;' + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  OnTerminateObjects += @Method;'                                 + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure DeleteOnTerminate(Method: String); overload;'           + LineEnding +
                   'var i: Int32;'                                                    + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  for i := High(OnTerminateStrings) downto 0 do'                  + LineEnding +
                   '    if SameText(OnTerminateStrings[i], Method) then'              + LineEnding +
                   '      Delete(OnTerminateStrings, i, 1);'                          + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure DeleteOnTerminate(Method: procedure); overload;'        + LineEnding +
                   'var i: Int32;'                                                    + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  for i := High(OnTerminateMethods) downto 0 do'                  + LineEnding +
                   '    if @OnTerminateMethods[i] = @Method then'                     + LineEnding +
                   '      Delete(OnTerminateMethods, i, 1);'                          + LineEnding +
                   'end;'                                                             + LineEnding +
                   ''                                                                 + LineEnding +
                   'procedure DeleteOnTerminate(Method: procedure of object); overload;'           + LineEnding +
                   'var i: Int32;'                                                    + LineEnding +
                   'begin'                                                            + LineEnding +
                   '  for i := High(OnTerminateObjects) downto 0 do'                  + LineEnding +
                   '    if @OnTerminateObjects[i] = @Method then'                       + LineEnding +
                   '      Delete(OnTerminateObjects, i, 1);'                          + LineEnding +
                   'end;'                                                             + LineEnding +
                   '',
                   'OnTerminate');
  end;
end;

initialization
  ScriptImports.Add('Script', @Lape_Import_Script);

end.

