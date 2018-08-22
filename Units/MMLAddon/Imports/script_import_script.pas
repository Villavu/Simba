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
  OnTerminate: String;
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

    OnTerminate :=
      'type'                                                                                                    + LineEnding +
      '  TTerminateMethod = record CallOnException: Boolean; end;'                                              + LineEnding +
      '  TTerminateMethod_String = record(TTerminateMethod) Method: String; end;'                               + LineEnding +
      '  TTerminateMethod_Procedure = record(TTerminateMethod) Method: procedure; end;'                         + LineEnding +
      '  TTerminateMethod_ProcedureOfObject = record(TTerminateMethod) Method: procedure of object; end;'       + LineEnding +
      ''                                                                                                        + LineEnding +
      'var'                                                                                                     + LineEnding +
      '  OnTerminateStrings: array of TTerminateMethod_String;'                                                 + LineEnding +
      '  OnTerminateProcedures: array of TTerminateMethod_Procedure;'                                           + LineEnding +
      '  OnTerminateProceduresOfObject: array of TTerminateMethod_ProcedureOfObject;'                           + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure __OnTerminate;'                                                                                + LineEnding +
      'var i: Int32;'                                                                                           + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  for i := 0 to High(OnTerminateStrings) do'                                                             + LineEnding +
      '    VariantInvoke(OnTerminateStrings[i].Method, []);'                                                    + LineEnding +
      '  for i := 0 to High(OnTerminateProcedures) do'                                                          + LineEnding +
      '    OnTerminateProcedures[i].Method();'                                                                  + LineEnding +
      '  for i := 0 to High(OnTerminateProceduresOfObject) do'                                                  + LineEnding +
      '    OnTerminateProceduresOfObject[i].Method();'                                                          + LineEnding +
      'end;'                                                                                                    + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure __OnTerminate_Exception;'                                                                      + LineEnding +
      'var i: Int32;'                                                                                           + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  for i := 0 to High(OnTerminateStrings) do'                                                             + LineEnding +
      '    if OnTerminateStrings[i].CallOnException then VariantInvoke(OnTerminateStrings[i].Method, []);'      + LineEnding +
      '  for i := 0 to High(OnTerminateProcedures) do'                                                          + LineEnding +
      '    if OnTerminateProcedures[i].CallOnException then OnTerminateProcedures[i].Method();'                 + LineEnding +
      '  for i := 0 to High(OnTerminateProceduresOfObject) do'                                                  + LineEnding +
      '    if OnTerminateProceduresOfObject[i].CallOnException then OnTerminateProceduresOfObject[i].Method();' + LineEnding +
      'end;'                                                                                                    + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure AddOnTerminate(Method: String; CallOnException: Boolean = False); overload;'                   + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  OnTerminateStrings += TTerminateMethod_String([CallOnException, Method]);'                             + LineEnding +
      'end;'                                                                                                    + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure AddOnTerminate(Method: procedure; CallOnException: Boolean = False); overload;'                + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  OnTerminateProcedures += TTerminateMethod_Procedure([CallOnException, @Method]);'                      + LineEnding +
      'end;'                                                                                                    + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure AddOnTerminate(Method: procedure of object; CallOnException: Boolean = False); overload;'      + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  OnTerminateProceduresOfObject += TTerminateMethod_ProcedureOfObject([CallOnException, @Method]);'      + LineEnding +
      'end;'                                                                                                    + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure DeleteOnTerminate(Method: String); overload;'                                                  + LineEnding +
      'var i: Int32;'                                                                                           + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  for i := High(OnTerminateStrings) downto 0 do'                                                         + LineEnding +
      '    if SameText(OnTerminateStrings[i].Method, Method) then'                                              + LineEnding +
      '      Delete(OnTerminateStrings, i, 1);'                                                                 + LineEnding +
      'end;'                                                                                                    + LineEnding +
      ''                                                                                                        + LineEnding +
      'procedure DeleteOnTerminate(Method: procedure); overload;'                                               + LineEnding +
      'var i: Int32;'                                                                                           + LineEnding +
      'begin'                                                                                                   + LineEnding +
      '  for i := High(OnTerminateProcedures) downto 0 do'                                                      + LineEnding +
      '    if @OnTerminateProcedures[i].Method = @Method then'                                                  + LineEnding +
      '      Delete(OnTerminateProcedures, i, 1);'                                                              + LineEnding +
      'end;'                                                                                                    + LineEnding +
     ''                                                                                                         + LineEnding +
     'procedure DeleteOnTerminate(Method: procedure of object); overload;'                                      + LineEnding +
     'var i: Int32;'                                                                                            + LineEnding +
     'begin'                                                                                                    + LineEnding +
     '  for i := High(OnTerminateProceduresOfObject) downto 0 do'                                               + LineEnding +
     '    if @OnTerminateProceduresOfObject[i].Method = @Method then'                                           + LineEnding +
     '      Delete(OnTerminateProceduresOfObject, i, 1);'                                                       + LineEnding +
     'end;';

     addDelayedCode(OnTerminate, 'OnTerminate', False, True);
  end;
end;

initialization
  ScriptImports.Add('Script', @Lape_Import_Script);

end.

