unit script_import_system;

{$mode objfpc}{$H+}
{$MACRO ON}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, lpcompiler, lptypes, lpffi, lputils, mufasatypes, mufasabase,
  LazUTF8;

procedure Lape_GetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariableUTF8(PString(Params^[1])^);
end;

procedure Lape_GetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := GetCurrentThreadID();
end;

procedure Lape_Wait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[1])^);
end;

type
  __TLapeCompiler = class(TLapeCompiler);

procedure Lape_Import_System(Compiler: TLapeCompiler; Data: Pointer);
begin
  InitializeFFI(Compiler);
  InitializePascalScriptBasics(Compiler, [psiTypeAlias, psiSettings, psiMagicMethod, psiFunctionWrappers, psiExceptions]);

  ExposeGlobals(Compiler);

  with __TLapeCompiler(Compiler) do
  begin
    FBaseDefines.Values['SIMBA_VERSION'] := IntToStr(SimbaVersion);
    FBaseDefines.Values['SIMBA_MAJOR'] := IntToStr(SimbaMajor);

    FBaseDefines.Values['FPC_VERSION'] := Format('%d', [FPC_VERSION]);
    FBaseDefines.Values['FPC_RELEASE'] := Format('%d', [FPC_RELEASE]);
    FBaseDefines.Values['FPC_PATCH'] := Format('%d', [FPC_PATCH]);

    addBaseDefine('MUFASA');
    addBaseDefine('COGAT');
    addBaseDefine('DGROCKS');
    addBaseDefine('SIMBA');
    addBaseDefine('SIMBA' + IntToStr(SimbaVersion));
    addBaseDefine('SIMBAMAJOR' + IntToStr(SimbaMajor));
    {$IFDEF CPU32}
    addBaseDefine('CPU32');
    addBaseDefine('CPU386');
    {$ENDIF}
    {$IFDEF CPU64}
    addBaseDefine('CPU64');
    {$ENDIF}
    {$IFDEF WINDOWS}
    addBaseDefine('WINDOWS');
    {$ENDIF}
    {$IFDEF LINUX}
    addBaseDefine('LINUX');
    {$ENDIF}

    addGlobalType('array of String', 'TStringArray');
    addGlobalType('array of TStringArray', 'T2DStringArray');
    addGlobalType('array of Int32', 'TIntegerArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of TIntegerArray', 'T2DIntArray');
    addGlobalType('array of T2DIntegerArray', 'T3DIntegerArray');
    addGlobalType('array of Char', 'TCharArray');
    addGlobalType('array of TCharArray', 'T2DCharArray');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of TByteArray', 'T2DByteArray');
    addGlobalType('array of Extended', 'TExtendedArray');
    addGlobalType('array of TExtendedArray', 'T2DExtendedArray');
    addGlobalType('array of Boolean', 'TBoolArray');
    addGlobalType('array of TBoolArray', 'T2DBoolArray');
    addGlobalType('array of Variant', 'TVariantArray');
    addGlobalType('array of TVariantArray', 'T2DVariantArray');

    addGlobalType('record X1, Y1, X2, Y2: Int32; end', 'TBox');
    addGlobalType('^TBox', 'PBox');
    addGlobalType('array of TBox', 'TBoxArray');
    addGlobalType('array of TBoxArray', 'T2DBoxArray');

    addGlobalType('record X, Y: Int32; end', 'TPoint');
    addGlobalType('^TPoint', 'PPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record R, T: Extended; end', 'TPolarPoint');
    addGlobalType('^TPolarPoint', 'PPolarPoint');

    addGlobalType('Int32', 'TColor');
    addGlobalType('UInt32', 'DWord');

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

    addGlobalVar(TThread.ProcessorCount, 'ProcessorCount').isConstant := True;
    addGlobalVar(MainThreadID, 'MainThreadID').isConstant := True;
    addGlobalVar('TFormatSettings', @FormatSettings, 'FormatSettings');

    addGlobalMethod('function GetCurrThreadID: PtrUInt;', @Lape_GetCurrentThreadID, Data);
    addGlobalMethod('function GetCurrentThreadID: PtrUInt;', @Lape_GetCurrentThreadID, Data);
    addGlobalMethod('function GetEnvironmentVariable(const Name: String): String;', @Lape_GetEnvironmentVariable, Data);
    addGlobalMethod('procedure Wait(Milliseconds: UInt32);', @Lape_Wait, Data);
  end;
end;

initialization
  ScriptImports.Add('System', @Lape_Import_System);

end.

