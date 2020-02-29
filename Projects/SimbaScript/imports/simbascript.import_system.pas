unit simbascript.import_system;

{$mode objfpc}{$H+}
{$macro ON}

interface

{$i import_uses.inc}

implementation

uses
  forms, simba.mufasabase, lazutf8;

procedure Lape_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script._Write(PString(Params^[0])^);
end;

procedure Lape_WriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script._WriteLn('');
end;

procedure Lape_GetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariableUTF8(PString(Params^[0])^);
end;

procedure Lape_GetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IFDEF DARWIN}
  PPtrUInt(Result)^ := 0;
  {$ELSE}
  PPtrUInt(Result)^ := GetCurrentThreadID();
  {$ENDIF}
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

procedure Lape_PauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.State := bUnknown;
end;

procedure Lape_TerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.State := bFalse;
end;

procedure Lape_IsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := Script.IsTerminating;
end;

procedure Lape_IsUserTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := (Script.IsTerminating and Script.IsUserTerminated)
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
    addGlobalConst(Script.IncludePath, 'IncludePath');
    addGlobalConst(Script.PluginPath, 'PluginPath');
    addGlobalConst(Script.FontPath, 'FontPath');
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

    {$IFDEF DARWIN}
    addGlobalConst(0, 'MainThreadID');
    {$ELSE}
    addGlobalConst(MainThreadID, 'MainThreadID');
    {$ENDIF}

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

    addGlobalFunc('procedure TerminateScript;', @Lape_TerminateScript);
    addGlobalFunc('procedure PauseScript;', @Lape_PauseScript);
    addGlobalFunc('function IsTerminated: Boolean;', @Lape_IsTerminated);
    addGlobalFunc('function IsUserTerminated: Boolean;', @Lape_IsUserTerminated);

    addDelayedCode(
      'var'                                                                      + LineEnding +
      '  OnTerminateStrings: array of String;'                                   + LineEnding +
      '  OnTerminateProcedures: array of procedure;'                             + LineEnding +
      '  OnTerminateProceduresOfObject: array of procedure of object;'           + LineEnding +
      ''                                                                         + LineEnding +
      'procedure __OnTerminate;'                                                 + LineEnding +
      'var i: Int32;'                                                            + LineEnding +
      'begin'                                                                    + LineEnding +
      '  for i := 0 to High(OnTerminateStrings) do'                              + LineEnding +
      '    VariantInvoke(OnTerminateStrings[i], []);'                            + LineEnding +
      '  for i := 0 to High(OnTerminateProcedures) do'                           + LineEnding +
      '    OnTerminateProcedures[i]();'                                          + LineEnding +
      '  for i := 0 to High(OnTerminateProceduresOfObject) do'                   + LineEnding +
      '    OnTerminateProceduresOfObject[i]()'                                   + LineEnding +
      'end;'                                                                     + LineEnding +
      ''                                                                         + LineEnding +
      'procedure AddOnTerminate(Method: String); overload;'                      + LineEnding +
      'begin'                                                                    + LineEnding +
      '  OnTerminateStrings += Method;'                                          + LineEnding +
      'end;'                                                                     + LineEnding +
      ''                                                                         + LineEnding +
      'procedure AddOnTerminate(Method: procedure); overload;'                   + LineEnding +
      'begin'                                                                    + LineEnding +
      '  OnTerminateProcedures += @Method;'                                      + LineEnding +
      'end;'                                                                     + LineEnding +
      ''                                                                         + LineEnding +
      'procedure AddOnTerminate(Method: procedure of object); overload;'         + LineEnding +
      'begin'                                                                    + LineEnding +
      '  OnTerminateProceduresOfObject += @Method;'                              + LineEnding +
      'end;'                                                                     + LineEnding +
      ''                                                                         + LineEnding +
      'procedure DeleteOnTerminate(Method: String); overload;'                   + LineEnding +
      'var i: Int32;'                                                            + LineEnding +
      'begin'                                                                    + LineEnding +
      '  for i := High(OnTerminateStrings) downto 0 do'                          + LineEnding +
      '    if SameText(OnTerminateStrings[i], Method) then'                      + LineEnding +
      '      Delete(OnTerminateStrings, i, 1);'                                  + LineEnding +
      'end;'                                                                     + LineEnding +
      ''                                                                         + LineEnding +
      'procedure DeleteOnTerminate(Method: procedure); overload;'                + LineEnding +
      'var i: Int32;'                                                            + LineEnding +
      'begin'                                                                    + LineEnding +
      '  for i := High(OnTerminateProcedures) downto 0 do'                       + LineEnding +
      '    if @OnTerminateProcedures[i] = @Method then'                          + LineEnding +
      '      Delete(OnTerminateProcedures, i, 1);'                               + LineEnding +
      'end;'                                                                     + LineEnding +
      ''                                                                         + LineEnding +
      'procedure DeleteOnTerminate(Method: procedure of object); overload;'      + LineEnding +
      'var i: Int32;'                                                            + LineEnding +
      'begin'                                                                    + LineEnding +
      '  for i := High(OnTerminateProceduresOfObject) downto 0 do'               + LineEnding +
      '    if @OnTerminateProceduresOfObject[i] = @Method then'                  + LineEnding +
      '      Delete(OnTerminateProceduresOfObject, i, 1);'                       + LineEnding +
      'end;', 'OnTerminate', False);

    addDelayedCode(
      'procedure MemMove(constref Src; var Dst; Size: SizeInt);'                + LineEnding +
      'begin'                                                                   + LineEnding +
      '  Move(Src, Dst, Size);'                                                 + LineEnding +
      'end;', 'MemMove', False);
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_System);

end.

