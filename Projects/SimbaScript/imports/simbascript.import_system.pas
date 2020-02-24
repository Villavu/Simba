unit simbascript.import_system;

{$mode objfpc}{$H+}
{$macro ON}

interface

{$i import_uses.inc}

implementation

uses
  forms, simba.misc, simba.mufasabase, lazutf8, simba.script_common;

procedure Lape_Write(const Params: PParamArray);
begin
  Script._Write(PString(Params^[0])^);
end;

procedure Lape_WriteLn(const Params: PParamArray);
begin
  Script._WriteLn('');
end;

procedure Lape_GetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariableUTF8(PString(Params^[0])^);
end;

procedure Lape_GetCommandLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetCommandLine();
end;

procedure Lape_GetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 // PPtrUInt(Result)^ := GetCurrentThreadID();
end;

procedure Lape_Wait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

type
  TSync = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSync.Execute;
type
  TSyncProcedure = procedure; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(PPointer(Params^[0])^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_Sync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := TSync.Create();
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
end;

type
  TSyncObject = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
type
  TSyncProcedure = procedure of object; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(Params^[0]^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_SyncObject(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSyncObject;
begin
  Sync := TSyncObject.Create();
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
end;

procedure Lape_RunCommandInDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RunCommand(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_RunCommandInDirEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RunCommand(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_RunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RunCommand(GetCurrentDir(), PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_RunCommandEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RunCommand(GetCurrentDir(), PString(Params^[0])^);
end;

procedure Lape_WriteTimeStamp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.WriteTimeStamp := PBoolean(Params^[0])^;
end;

procedure Lape_GetTimeRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := GetTickCount64() - Script.StartTime;
end;

procedure Lape_PauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 // Script.State := SCRIPT_STATE_PAUSED;
end;

procedure Lape_TerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 // Script.State := SCRIPT_STATE_STOPPING;
end;

procedure Lape_IsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PBoolean(Params^[1])^ then
    PBoolean(Result)^ := (Script.IsTerminating and Script.IsUserTerminated)
  else
    PBoolean(Result)^ := Script.IsTerminating;
end;

procedure Lape_Import_System(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    Section := 'System';

    addBaseDefine('SIMBA', Format('%d', [SimbaVersion]));
    addBaseDefine('SIMBAMAJOR', Format('%d', [SimbaMajor]));

    addBaseDefine('FPCFULLVERSION', Format('%d', [FPC_FULLVERSION]));
    addBaseDefine('FPCVERSION', Format('%d', [FPC_VERSION]));
    addBaseDefine('FPCRELEASE', Format('%d', [FPC_RELEASE]));
    addBaseDefine('FPCPATCH', Format('%d', [FPC_PATCH]));

    addBaseDefine('MUFASA');
    addBaseDefine('COGAT');
    addBaseDefine('DGROCKS');
    addBaseDefine('SIMBA' + IntToStr(SimbaVersion));
    addBaseDefine('SIMBAMAJOR' + IntToStr(SimbaMajor));

    {$IFDEF CPU32}
    addBaseDefine('CPU32');
    addBaseDefine('CPU386');
    {$ENDIF}
    {$IFDEF CPU64}
    addBaseDefine('CPU64');
    {$ENDIF}
    {$IFDEF CPUAARCH64}
    addBaseDefine('CPUAARCH64');
    {$ENDIF}
    {$IFDEF WINDOWS}
    addBaseDefine('WINDOWS');
    {$ENDIF}
    {$IFDEF WIN32}
    addBaseDefine('WIN32');
    {$ENDIF}
    {$IFDEF WIN64}
    addBaseDefine('WIN64');
    {$ENDIF}
    {$IFDEF LINUX}
    addBaseDefine('LINUX');
    {$ENDIF}
    {$IFDEF DARWIN}
    addBaseDefine('DARWIN');
    {$ENDIF}

    addGlobalConst(Script.ScriptFile, 'ScriptFile');
    addGlobalConst(Script.ScriptName, 'ScriptName');
    addGlobalConst(Script.IncludePath, 'IncludePath');
    addGlobalConst(Script.PluginPath, 'PluginPath');
    addGlobalConst(Script.FontPath, 'FontPath');
    addGlobalConst(Script.ScriptPath, 'ScriptPath');
    addGlobalConst(Script.AppPath, 'AppPath');

    addGlobalConst(LineEnding, 'LineEnding');

    addGlobalFunc('procedure _Write(S: string); override;', @Lape_Write);
    addGlobalFunc('procedure _WriteLn; override;', @Lape_WriteLn);

    addGlobalType('record'                                                       + LineEnding +
                  '  CurrencyFormat: Byte;'                                      + LineEnding +
                  '  NegCurrFormat: Byte;'                                       + LineEnding +
                  '  ThousandSeparator: Char;'                                   + LineEnding +
                  '  DecimalSeparator: Char;'                                    + LineEnding +
                  '  CurrencyDecimals: Byte;'                                    + LineEnding +
                  '  DateSeparator: Char;'                                       + LineEnding +
                  '  TimeSeparator: Char;'                                       + LineEnding +
                  '  ListSeparator: Char;'                                       + LineEnding +
                  '  CurrencyString: String;'                                    + LineEnding +
                  '  ShortDateFormat: String;'                                   + LineEnding +
                  '  LongDateFormat: String;'                                    + LineEnding +
                  '  TimeAMString: String;'                                      + LineEnding +
                  '  TimePMString: String;'                                      + LineEnding +
                  '  ShortTimeFormat: String;'                                   + LineEnding +
                  '  LongTimeFormat: String;'                                    + LineEnding +
                  '  ShortMonthNames: array[1..12] of String;'                   + LineEnding +
                  '  LongMonthNames: array[1..12] of String;'                    + LineEnding +
                  '  ShortDayNames: array[1..7] of String;'                      + LineEnding +
                  '  LongDayNames: array[1..7] of String;'                       + LineEnding +
                  '  TwoDigitYearCenturyWindow: Word;'                           + LineEnding +
                  'end;', 'TFormatSettings');

    addGlobalConst(TThread.ProcessorCount, 'ProcessorCount');
    //addGlobalConst(MainThreadID, 'MainThreadID');

    addGlobalVar('TFormatSettings', @FormatSettings, 'FormatSettings');
    addGlobalVar('TClient', @Script.Client, 'Client');

    addGlobalFunc('function GetCurrThreadID: PtrUInt;', @Lape_GetCurrentThreadID);
    addGlobalFunc('function GetCurrentThreadID: PtrUInt;', @Lape_GetCurrentThreadID);
    addGlobalFunc('function GetEnvironmentVariable(const Name: String): String;', @Lape_GetEnvironmentVariable);
    addGlobalFunc('procedure Wait(Milliseconds: UInt32);', @Lape_Wait);

     {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}
    addGlobalType('procedure();', 'TSyncMethod', FFI_CDECL);
    addGlobalType('procedure() of object;', 'TSyncObjectMethod', FFI_CDECL);
    {$ELSE}
    addGlobalType('procedure();', 'TSyncMethod', FFI_DEFAULT_ABI);
    addGlobalType('procedure() of object;', 'TSyncObjectMethod', FFI_DEFAULT_ABI);
    {$ENDIF}

    addGlobalFunc('procedure Sync(Method: TSyncMethod); overload;', @Lape_Sync);
    addGlobalFunc('procedure Sync(Method: TSyncObjectMethod); overload;', @Lape_SyncObject);

    addGlobalFunc('function GetCommandLine: TStringArray;', @Lape_GetCommandLine);

    addGlobalFunc('function RunCommandInDir(CurrentDirectory: String; CommandLine: String; out Output: String): Int32; overload;', @Lape_RunCommandInDir);
    addGlobalFunc('procedure RunCommandInDir(CurrentDirectory: String; CommandLine: String); overload;', @Lape_RunCommandInDirEx);

    addGlobalFunc('function RunCommand(CommandLine: String; out Output: String): Int32; overload;', @Lape_RunCommand);
    addGlobalFunc('procedure RunCommand(CommandLine: String); overload;', @Lape_RunCommandEx);

    addGlobalFunc('procedure WriteTimeStamp(Enable: Boolean);', @Lape_WriteTimeStamp);
    addGlobalFunc('procedure TerminateScript;', @Lape_TerminateScript);
    addGlobalFunc('procedure PauseScript;', @Lape_PauseScript);
    addGlobalFunc('function IsTerminated(UserTerminated: Boolean = False): Boolean;', @Lape_IsTerminated);

    addDelayedCode(
      '{$IFNDEF CODEINSIGHT}'                                                                 + LineEnding +
      'var'                                                                                   + LineEnding +
      '  OnTerminateStrings: array of String;'                                                + LineEnding +
      '  OnTerminateProcedures: array of procedure;'                                          + LineEnding +
      '  OnTerminateProceduresOfObject: array of procedure of object;'                        + LineEnding +
      ''                                                                                      + LineEnding +
      'procedure __OnTerminate;'                                                              + LineEnding +
      'var i: Int32;'                                                                         + LineEnding +
      'begin'                                                                                 + LineEnding +
      '  for i := 0 to High(OnTerminateStrings) do'                                           + LineEnding +
      '    VariantInvoke(OnTerminateStrings[i], []);'                                         + LineEnding +
      '  for i := 0 to High(OnTerminateProcedures) do'                                        + LineEnding +
      '    OnTerminateProcedures[i]();'                                                       + LineEnding +
      '  for i := 0 to High(OnTerminateProceduresOfObject) do'                                + LineEnding +
      '    OnTerminateProceduresOfObject[i]()'                                                + LineEnding +
      'end;'                                                                                  + LineEnding +
      '{$ENDIF}'                                                                              + LineEnding +
      ''                                                                                      + LineEnding +
      'procedure AddOnTerminate(Method: String); overload;'                                   + LineEnding +
      'begin'                                                                                 + LineEnding +
      '  OnTerminateStrings += Method;'                                                       + LineEnding +
      'end;'                                                                                  + LineEnding +
      ''                                                                                      + LineEnding +
      'procedure AddOnTerminate(Method: procedure); overload;'                                + LineEnding +
      'begin'                                                                                 + LineEnding +
      '  OnTerminateProcedures += @Method;'                                                   + LineEnding +
      'end;'                                                                                  + LineEnding +
      ''                                                                                      + LineEnding +
      'procedure AddOnTerminate(Method: procedure of object); overload;'                      + LineEnding +
      'begin'                                                                                 + LineEnding +
      '  OnTerminateProceduresOfObject += @Method;'                                           + LineEnding +
      'end;'                                                                                  + LineEnding +
      ''                                                                                      + LineEnding +
      'procedure DeleteOnTerminate(Method: String); overload;'                                + LineEnding +
      'var i: Int32;'                                                                         + LineEnding +
      'begin'                                                                                 + LineEnding +
      '  for i := High(OnTerminateStrings) downto 0 do'                                       + LineEnding +
      '    if SameText(OnTerminateStrings[i], Method) then'                                   + LineEnding +
      '      Delete(OnTerminateStrings, i, 1);'                                               + LineEnding +
      'end;'                                                                                  + LineEnding +
      ''                                                                                      + LineEnding +
      'procedure DeleteOnTerminate(Method: procedure); overload;'                             + LineEnding +
      'var i: Int32;'                                                                         + LineEnding +
      'begin'                                                                                 + LineEnding +
      '  for i := High(OnTerminateProcedures) downto 0 do'                                    + LineEnding +
      '    if @OnTerminateProcedures[i] = @Method then'                                       + LineEnding +
      '      Delete(OnTerminateProcedures, i, 1);'                                            + LineEnding +
      'end;'                                                                                  + LineEnding +
     ''                                                                                       + LineEnding +
     'procedure DeleteOnTerminate(Method: procedure of object); overload;'                    + LineEnding +
     'var i: Int32;'                                                                          + LineEnding +
     'begin'                                                                                  + LineEnding +
     '  for i := High(OnTerminateProceduresOfObject) downto 0 do'                             + LineEnding +
     '    if @OnTerminateProceduresOfObject[i] = @Method then'                                + LineEnding +
     '      Delete(OnTerminateProceduresOfObject, i, 1);'                                     + LineEnding +
     'end;',
     'OnTerminate', False, True);
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_System);

end.

