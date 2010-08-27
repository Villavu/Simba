{/==============================================================================
//==============================================================================

RUTIS-Engine (RunTimeScript-Engine)

RUTIS_Compiler_Delphi.pas (part of) RUTIS-Engine

--> This unit contains the RUTIS-Compiler for Pascal-Syntax code


//==============================================================================

Author : Björn Zeutzheim
E-Mail : bjoern@zeutzheim-boppard.de

//==============================================================================

License: Dieses Projekt (RUTIS-Engine) steht unter einer /
         This project is licensed under the
         Creative Commons 3.0 BY-NC-SA (German) License

Diese Lizens beinhaltet                  / This license includes:
-> Namensnennung                         / Attribution
-> Keine kommerzielle Nutzung            / Noncommercial
-> Weitergabe unter gleichen Bedingungen / Share Alike

Ein ausformulierter Lizensvertrag ist über folgende Adresse erreichbar:
The Legal Code (the full license) can be read at the following Address:
> http: //creativecommons.org/licenses/by-nc-sa/3.0/de/

//==============================================================================
//==============================================================================}

Unit Rutis_Compiler_Delphi;

Interface

{$i Delphi_Versions.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

Uses
  Forms, Classes,
{$ifdef DELPHI_7_UP}Variants,{$endif}
  SysUtils, Math,
  Rutis_Defs, Rutis_Errors, RUTIS_Classes;

Type
//==============================================================================
//==============================================================================
  TRutisSymbol = (sUnknown, sIdent, sInteger, sFloat, sString,
    sPlus, sMinus, sStar, sSlash, sMod, sDiv, sNil,
    sEqual, sSmaller, sBigger, sBiggerEqual, sSmallerEqual, sUnEqual,
    sOpenBracket, sCloseBracket, sOpenBracketR, sCloseBracketR, sComma, sDot,
    sSemiColon, sBecomes, sDopDot, sHook, sAt,
    sPacked, sRecord,
    sSet, sArray, sClass,
    sTrue, sFalse,
    sProgram, sUnit, sInterface, sImplement, sInitialization, sFinalization,
    sUses, sType, sVar, sConst, sOf,
    sProcedure, sFunction, sExternal,
    sIf, sThen, sElseIf, sElse, sBegin, sEnd,
    sWhile, sDo, SRepeat, sUntil, sfor,
    sTo, sDownTo, sContinue, sBreak, sExit, sTerminate,
    sopAnd, sopOr, sopXOr, sopNot,
    sInc, sDec, sWrite, sLength, sHigh, sSetLength, sSizeOf,
    sGenMem, sReallocMem, sFreeMem, sMemSize, sNew, sDispose,
    sNop, sNone);

  TRutisSymbols = Set Of TRutisSymbol;

Const
  cSymbols  : Array[TRutisSymbol] Of ShortString = ('', '', '', '', '',
    '+', '-', '*', '/', 'MOD', 'DIV', 'NIL',
    '=', '<', '>', '>=', '<=', '<>',
    '(', ')', '[', ']', ',', '.',
    ';', ':=', ':', '^', '@',
    'PACKED', 'RECORD',
    'SET', 'ARRAY', 'CLASS',
    'TRUE', 'FALSE',
    'PROGRAM', 'UNIT', 'INTERFACE', 'IMPLEMENTATION', 'INITIALIZATION', 'FINALIZATION',
    'USES', 'TYPE', 'VAR', 'CONST', 'OF',
    'PROCEDURE', 'FUNCTION', 'EXTERNAL',
    'IF', 'THEN', 'ELSEIF', 'ELSE', 'BEGIN', 'END',
    'WHILE', 'DO', 'REPEAT', 'UNTIL', 'FOR',
    'TO', 'DOWNTO', 'CONTINUE', 'BREAK', 'EXIT', 'TERMINATE',
    'AND', 'OR', 'XOR', 'NOT',
    'INC', 'DEC', 'WRITE', 'LENGTH', 'HIGH', 'SETLENGTH', 'SIZEOF',
    'GENMEM', 'REALLOCMEM', 'FREEMEM', 'MEMSIZE', 'NEW', 'DISPOSE',
    'NOP', '');

Const
  errSymbols  : Array[TRutisSymbol] Of ShortString =
    ('Unknown', 'Identifier', 'Integer', 'Float', 'String',
    '+', '-', '*', '/', 'mod', 'div', 'nil',
    '=', '<', '>', '>=', '<=', '<>',
    '(', ')', '[', ']', 'Comma', 'Dot',
    'Semicolon', ':=', ':', '^', '@',
    'packed', 'RECORD',
    'SET', 'ARRAY', 'CLASS',
    'TRUE', 'FALSE',
    'PROGRAM', 'UNIT', 'INTERFACE', 'IMPLEMENTATION', 'INITIALIZATION', 'FINALIZATION',
    'USES', 'TYPE', 'VAR', 'CONST', 'OF',
    'PROCEDURE', 'FUNCTION', 'EXTERNAL',
    'IF', 'THEN', 'ElseIf', 'else', 'BEGIN', 'END',
    'WHILE', 'DO', 'REPEAT', 'UNTIL', 'FOR',
    'To', 'DownTo', 'Continue', 'Break', 'Exit', 'Terminate',
    'AND', 'OR', 'XOR', 'NOT',
    'inc', 'dec', 'write', 'length', 'high', 'SetLength', 'SizeOf',
    'GenMem', 'ReallocMem', 'FreeMem', 'MemSize', 'New', 'Dispose',
    'NOP', '!none!');

Type
  TCompilerState = Record
    CSScannerLine    : Integer;
    CSScannerStrPos  : Integer;
    CSScannerCh      : Char;
    CSIdentSym       : TRutisSymbol;
    CSCodePos        : Integer;
  End;

  TRutisResStringInfo = Record
    Address : Integer;
    Value   : AnsiString;
  End;

  TRutisCompilerDelphi = Class(TRutisCompiler)
  Private
  Protected
    StackVarPos         : Integer;
    UnitNamespaces      : Array Of AnsiString;
    CompiledUnits       : Array Of AnsiString;
    ResStrings          : Array Of TRutisResStringInfo;
    CurrentNamespace    : AnsiString;
    CurrentUnit         : AnsiString;
    CompilingUnit       : Boolean;
    fCompilerLevel      : Integer;
    InitializationJmp,
    FinalizationJmpPos  : Integer;
    fCompilingDefines   : Boolean;
    ExitParamsSize      : Integer;
    LoopDepth           : Smallint;
    //========================
    ScannerLine         : Integer;
    ScannerStrPos       : Integer;
    LastScannerLine     : Integer;
    LastScannerStrPos   : Integer;
    ScannerCh           : Char;
    IdentSym            : TRutisSymbol;
    IdentStr            : ShortString;
    IdentNum            : Variant;
    ExprResult          : TRutisTypeDecl;
    //========================
    PushScannerLine     : Integer;
    PushScannerStrPos   : Integer;
    PushScannerCh       : Char;
    PushIdentSym        : TRutisSymbol;
    CompilerStates      : Array Of TCompilerState;
    //========================
    Procedure CompilerMessage(Const Msg : AnsiString; Const Args : Array Of Const; ErrorType : TRutisErrorType = etCompilerError); Overload;
    Procedure CompilerMessage(Msg : AnsiString; ErrorType : TRutisErrorType = etCompilerError); Overload;
    //========================
    Procedure GetSym;
    Procedure PushScanner;
    Procedure PopScanner;
    Procedure PopCompilerState;
    Procedure PushCompilerState;
    //========================
    Procedure RaiseError(Const Msg : AnsiString); Overload;
    Procedure RaiseError(Const Msg : AnsiString; Const Args : Array Of Const); Overload;
    Procedure ErrorExpected(Expected : TRutisSymbols; Found : TRutisSymbol);
    Function Expect(Expected : TRutisSymbols) : Boolean;
    //========================
    Function CheckRedefinition(Name : AnsiString) : Boolean;
    Function GetDeclaration(Name : AnsiString) : TRutisDecl; Overload;
    Function GetDeclaration(Name : AnsiString; Exclude : Array Of TRutisDecl) : TRutisDecl; Overload;
    Function GetDeclarationID(Name : AnsiString) : Integer; Overload;
    //========================
    Procedure GenLoadSaveCode(AType : TIdentType; Address, Level, Size : Integer; DoLoad : Boolean); Overload;
    Procedure GenLoadSaveCode(AType : TIdentType; Address, Level : Integer; AIntType : TRutisIntType; DoLoad : Boolean); Overload;
    //========================
    Procedure C_Program;
    Procedure C_Unit;
    Procedure C_Uses;
    //=== Find / Analyse Types ===
    Function GetPointerType(AType : TRutisTypeDecl; WantedName : AnsiString = '') : TRutisPointerType;
    Function GetArrayType(AType : TRutisTypeDecl; WantedName : AnsiString = '') : TRutisArrayType;
    Function GetSetType(AType : TRutisEnumType; WantedName : AnsiString = '') : TRutisSetType;
    Function C_DeclTypeInline(NewTypeName : AnsiString = '') : TRutisTypeDecl;
    //=== Declarations ===
    Procedure C_DeclType;
    Procedure C_DeclTypes;
    Procedure C_DeclParams;
    Procedure C_DeclVars;
    Procedure C_DeclMethod;
    Procedure C_DeclConsts;
    Function C_DeclarationSeq : Boolean;
    Procedure C_Declarations;
    //========================
    Function C_Method : Boolean;
    Procedure C_Implementation;
    //========================
    Function C_AnalyzeIdent(IsExpression : Boolean; WantType : TRutisIntType; Var Inf : TAnalyzeIdentInfo) : TIdentType;
    //========================
    procedure C_Expression(WantType : TRutisTypeDecl);
    //Function C_Expression(WantType : TRutisIntType; WantTypeDecl : TRutisTypeDecl = nil) : Boolean;
    //procedure C_StrExpression;
    //========================
    Procedure C_StatementSequence;
  Public
    Constructor Create(AOwner : TRutisEngineBase);
    Destructor Destroy; Override;
    //========================
    Procedure Compile; Override;
  End;

  //==============================================================================
  //==============================================================================
Implementation

//==============================================================================
//============ TRutisCompilerDelphi ============================================
//==============================================================================

Constructor TRutisCompilerDelphi.Create(AOwner : TRutisEngineBase);
Begin
  Inherited;

  optArrangeFields := True;
  optArrangeSize   := 8;
End;

Destructor TRutisCompilerDelphi.Destroy;
Begin
  Inherited;
End;

Procedure TRutisCompilerDelphi.Compile;
Var
  oldDecimalSeparator  : Char;
  I, j     : Integer;
  tmpDecl  : TRutisDecl;
Begin
  Inherited Compile;

  oldDecimalSeparator := DecimalSeparator;
  DecimalSeparator    := '.';

  CurrentUnit := '';

  ScriptData.CompilerLine := 0;
  IdentNum       := 0;
  IdentStr       := '';
  ScannerCh      := #0;
  ScannerStrPos  := 0;
  ScannerLine    := -1;
  FinalizationJmpPos := -5;
  fCompilerLevel := 0;
  StackVarPos    := 0;
  LoopDepth      := 0;    
  SetLength(CompiledUnits, 0);
  SetLength(UnitNamespaces, 0);
  SetLength(ResStrings, 0);

  For I := 0 To ScriptData.DeclarationCount - 1 Do
    If (ScriptData.DeclarationList[i] is TRutisExtMethodType) Then
    Begin
      tmpDecl := ScriptData.DeclarationList[i];
      TRutisExtMethodType(tmpDecl).MethodTableID := -1;
      TRutisExtMethodType(tmpDecl).MethodResult.TypeData := nil;
      For j := 0 To high(TRutisExtMethodType(tmpDecl).Params) Do
        TRutisExtMethodType(tmpDecl).Params[j].TypeData := nil;
    End;

  Try
    {$ifdef DEBUG}OutputDebugString('TRutisCompilerDelphi.Compile  -  Compiling...');{$endif DEBUG}
    C_Program;

    Owner.CompilerMessage('Script sucessfully compiled!', etHint);
    Owner.CompilerMessage('Raw CMD-count       :' + IntToStr(length(ScriptData.Code)), etHint);
    {$ifdef DEBUG}OutputDebugString('TRutisCompilerDelphi.Compile  -  Optimizing...');{$endif DEBUG}
    Optimize;
    Owner.CompilerMessage('Optimized CMD-count :' + IntToStr(length(ScriptData.Code)), etHint);
  Finally
    DecimalSeparator := oldDecimalSeparator;
  End;
End;

//==============================================================================

Procedure TRutisCompilerDelphi.CompilerMessage(Const Msg : AnsiString; Const Args : Array Of Const; ErrorType : TRutisErrorType);
Begin
  Try
    CompilerMessage(Format(Msg, Args), ErrorType);
  Except
    on EConvertError Do
    Begin
      CompilerMessage('Wrong Arguments');
    End;
  End;
End;

Procedure TRutisCompilerDelphi.CompilerMessage(Msg : AnsiString; ErrorType : TRutisErrorType);
Begin
  If ErrorType = etCompilerError Then
    Msg := 'Error: ' + CurrentNamespace + '(' + IntToStr(ScriptData.CompilerLine + 1) + '): ' + Msg;
  If ErrorType = etWarning Then
    Msg := 'Warning: ' + CurrentNamespace + '(' + IntToStr(ScriptData.CompilerLine + 1) + '): ' + Msg;
  If ErrorType = etHint Then
    Msg := 'Hint: ' + CurrentNamespace + '(' + IntToStr(ScriptData.CompilerLine + 1) + '): ' + Msg;

  Owner.CompilerMessage(Msg, ErrorType);
End;

Procedure TRutisCompilerDelphi.RaiseError(Const Msg : AnsiString; Const Args : Array Of Const);
Begin
  Raise ERutisCompilerError.CreateFmt(Msg, Args, CurrentNamespace, LastScannerLine, LastScannerStrPos);
End;

Procedure TRutisCompilerDelphi.RaiseError(Const Msg : AnsiString);
Begin
  Raise ERutisCompilerError.Create(Msg, CurrentNamespace, LastScannerLine, LastScannerStrPos);
End;

Procedure TRutisCompilerDelphi.ErrorExpected(Expected : TRutisSymbols; Found : TRutisSymbol);
Var
  SymStr  : AnsiString;
  GotOne  : Boolean;
  sym     : TRutisSymbol;
Begin
  SymStr := '';
  GotOne := False;
  For sym := sUnknown To sNone Do
    If sym in Expected Then
    Begin
      If GotOne Then
        SymStr := SymStr + ''', ''' + errSymbols[sym]
      Else
        SymStr := errSymbols[sym];
    End;
  RaiseError(ERR_EXPECTED_FOUND, [SymStr, errSymbols[Found]]);
End;

Function TRutisCompilerDelphi.Expect(Expected : TRutisSymbols) : Boolean;
Begin
  Result := True;
  If IdentSym in Expected Then exit;
  ErrorExpected(Expected, IdentSym);
  Result := False;
End;

//==============================================================================
//==============================================================================

Procedure TRutisCompilerDelphi.GenLoadSaveCode(AType : TIdentType; Address, Level, Size : Integer; DoLoad : Boolean);
Begin
  If DoLoad Then
  Begin
    Case AType Of
      itVar  : ScriptData.GenCode(_lod, Address, Level, Size);
      itPVar  : ScriptData.GenCode(_lodp, Address, Level, Size);
      itConst  : ScriptData.GenCode(_lodr, Address, Size, 0);
      itError  : RaiseError(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]);
      itMethod :
      Begin
      //CompilerError(ERR_VAR_EXPECTED);
        exit;
      End;
    End;
  End
  Else
    Case AType Of
      itVar  : ScriptData.GenCode(_sto, Address, Level, Size);
      itPVar  : If Level < 0 Then
          ScriptData.GenCode(_stop, Address - 3 - Size, Level, Size)
        Else
          ScriptData.GenCode(_stop, Address, Level, Size);
      itConst  : RaiseError(ERR_NO_CONST_ALLOWED);
      itError  : RaiseError(Format(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]));
      itMethod  : RaiseError(ERR_VAR_EXPECTED);
    End;
End;

Procedure TRutisCompilerDelphi.GenLoadSaveCode(AType : TIdentType; Address, Level : Integer; AIntType : TRutisIntType; DoLoad : Boolean);
Var Size  : Integer;
Begin
  Size := IntTypeSizes[AIntType];
  If Size < 0 Then RaiseError(ERR_UNEXPECTED_ERROR);
  If DoLoad Then
  Begin
    Case AType Of
      itVar  : ScriptData.GenCode(_lod, Address, Level, Size);
      itPVar  : If Level < 0 Then
          ScriptData.GenCode(_lodp, Address - 3, Level, Size)
        Else
          ScriptData.GenCode(_lodp, Address, Level, Size);
      itConst  : ScriptData.GenCode(_lodr, Address, Size, 0);
      itError  : RaiseError(Format(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]));
      itMethod  : RaiseError(ERR_VAR_EXPECTED);
    End;
  End
  Else
    Case AType Of
      itVar  : ScriptData.GenCode(_sto, Address, Level, Size);
      itPVar  : If Level < 0 Then
          ScriptData.GenCode(_stop, Address - 3 - Size, Level, Size)
        Else
          ScriptData.GenCode(_stop, Address, Level, Size);
      itConst  : RaiseError(ERR_NO_CONST_ALLOWED);
      itError  : RaiseError(Format(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]));
      itMethod  : RaiseError(ERR_VAR_EXPECTED);
    End;
End;

//==============================================================================

Procedure TRutisCompilerDelphi.C_Program;
Var
  str        : AnsiString;
  Namespace  : TRutisNamespace;
Begin
  GetSym;
  If IdentSym = sUnit Then
  Begin
    InitializationJmp := -1;
    ScannerStrPos     := 0;
    ScriptData.CompilerLine := 0;
    C_Unit;
    If not optCanCompileUnits then
      SetLength(ScriptData.Code, 0);
    exit;
  End;

  Expect([sProgram]);
  GetSym;
  Expect([sIdent]);
  If CurrentUnit <> '' Then
    If UpperCase(CurrentUnit) <> UpperCase(IdentStr) Then
    Begin
      CompilerMessage(ERR_FILENAME_NOT_UNITNAME, etWarning);
      exit;
    End;
  CurrentUnit      := IdentStr;
  CurrentNamespace := CurrentUnit;
  ScriptData.ScriptName := IdentStr;

  Namespace := ScriptData.GetNamespace(CurrentUnit);
  If Namespace = nil Then
  Begin
    Namespace := TRutisNamespace.Create;
    SetLength(ScriptData.Namespaces, Length(ScriptData.Namespaces) + 1);
    ScriptData.Namespaces[high(ScriptData.Namespaces)] := Namespace;
    Namespace.Name      := UpperCase(CurrentUnit);
    Namespace.CodeFile  := CurrentUnit;
    Namespace.CodeStart := 0;
  End;

  SetLength(UnitNamespaces, 1);
  UnitNamespaces[0] := CurrentNamespace;

  GetSym;
  Expect([sSemiColon]);
  GetSym;
  Expect([sInterface]);
  GetSym;
  ScriptData.CompilerLine := ScannerLine;

  ScriptData.GenCode(_gen, 0, 0, 0);
  ScriptData.GenCode(_jmp, 0, 0, 0);
  InitializationJmp := high(ScriptData.Code);

  str := ScriptData.ScriptName;
  C_Uses;
  ScriptData.ScriptName := str;

  fCompilingDefines       := True;
  C_Declarations;
  fCompilingDefines       := False;
  ScriptData.CompilerLine := ScannerLine;

  Expect([sImplement]);
  GetSym;
  ScriptData.CompilerLine := ScannerLine;

  C_Implementation;

  ScriptData.Code[0].P1 := StackVarPos;

  ScriptData.CompilerLine := ScannerLine;
  Expect([sBegin]);
  GetSym;

  ScriptData.Code[InitializationJmp].P1 := Length(ScriptData.Code);

  C_StatementSequence;

  ScriptData.CompilerLine := ScannerLine;
  Expect([sEnd]);
  GetSym;
  Expect([sDot]);

  //Und Ende...
  ScriptData.GenCode(_jmp, FinalizationJmpPos, 0, 0);

  Namespace.CodeEnd := Length(ScriptData.Code);

  GetSym;
  If IdentSym <> sNone Then
    CompilerMessage(ERR_CODE_AFTER_PROGRAM_END, etHint);

  {$ifdef DEBUG}OutputDebugString(PAnsiChar('TRutisCompilerDelphi.C_Program  -  Compiled Program ' + ScriptData.ScriptName));{$endif DEBUG}
End;

Procedure TRutisCompilerDelphi.C_Unit;
Var
  int       : Integer;
  str       : AnsiString;
  Namespace : TRutisNamespace;
  IsUnitApp : Boolean;
Begin
  CompilingUnit := True;

  If length(ScriptData.Code) = 0 Then
  Begin
    ScriptData.GenCode(_gen, 0, 0, 0);
    ScriptData.GenCode(_jmp, 0, 0, 0);
    InitializationJmp := high(ScriptData.Code);
    IsUnitApp := true;
  End
  else
    IsUnitApp := false;

  GetSym;
  Expect([sUnit]);
  GetSym;
  Expect([sIdent]);
  If CurrentUnit <> '' Then
    If UpperCase(CurrentUnit) <> UpperCase(IdentStr) Then
    Begin
      RaiseError(ERR_FILENAME_NOT_UNITNAME);
      exit;
    End;
  CurrentUnit      := IdentStr;
  CurrentNamespace := CurrentUnit;
  If IsUnitApp then
    ScriptData.ScriptName := IdentStr;

  Namespace := ScriptData.GetNamespace(CurrentUnit);
  If Namespace = nil Then
  Begin
    Namespace := TRutisNamespace.Create;
    SetLength(ScriptData.Namespaces, Length(ScriptData.Namespaces) + 1);
    ScriptData.Namespaces[high(ScriptData.Namespaces)] := Namespace;
    Namespace.Name      := UpperCase(CurrentUnit);
    Namespace.CodeFile  := CurrentUnit;
    Namespace.CodeStart := 0;
  End;

  SetLength(UnitNamespaces, 1);
  UnitNamespaces[0] := CurrentNamespace;

  GetSym;
  Expect([sSemiColon]);
  GetSym;
  Expect([sInterface]);
  GetSym;
  //ScriptData.CompilerLine := ScannerLine;

  //{!!}Owner.OnError(CurrentUnit + ': C_Unit -> C_Uses', etHint);
  str := ScriptData.ScriptName;
  C_Uses;
  ScriptData.ScriptName := str;

  //{!!}Owner.OnError(CurrentUnit + ': C_Unit -> C_Declarations', etHint);
  fCompilingDefines := True;
  C_Declarations;
  fCompilingDefines := False;

  //ScriptData.GenCode(_jmp, 0, 0, 0);
  //InitializationJmp := high(ScriptData.Code);

  Expect([sImplement]);
  GetSym;

  //{!!}Owner.OnError(CurrentUnit + ': C_Unit -> C_Implementation', etHint);
  C_Implementation;
  //{!!}Owner.OnError(CurrentUnit + ': C_Unit <- C_Implementation', etHint);

  If IdentSym = sInitialization Then
  Begin
    GetSym;
    ScriptData.Code[InitializationJmp].P1 := high(ScriptData.Code) + 1;
    C_StatementSequence;
    ScriptData.GenCode(_jmp, 0, 0, 0);
    InitializationJmp := high(ScriptData.Code);

    If IdentSym = sFinalization Then
    Begin
      GetSym;
      int := high(ScriptData.Code) + 1;
      C_StatementSequence;
      ScriptData.GenCode(_jmp, FinalizationJmpPos, 0, 0);
      FinalizationJmpPos := int;
    End;
  End;
  ScriptData.Code[InitializationJmp].P1 := high(ScriptData.Code) + 1;

  If IsUnitApp then
    ScriptData.Code[0].P1 := StackVarPos;

  Namespace.CodeEnd := Length(ScriptData.Code);

  Expect([sEnd]);
  GetSym;
  Expect([sDot]);

  GetSym;
  If IdentSym <> sNone Then
    CompilerMessage(ERR_CODE_AFTER_UNIT_END, etHint);

  CompilingUnit := False;

  {$ifdef DEBUG}OutputDebugString(PAnsiChar('TRutisCompilerDelphi.C_Unit  -  Compiled Unit ' + ScriptData.ScriptName));{$endif DEBUG}
End;

Procedure TRutisCompilerDelphi.C_Uses;
Var
  i          : Integer;
  UName      : AnsiString;
  savCurrUnit  : AnsiString;
  savScriptCode  : TStrings;
  savScannerStrPos  : Integer;
  savLine    : Integer;
  savScannerLine  : Integer;
  savScannerCh  : Char;
  savIdentSym  : TRutisSymbol;
  savIdentStr  : ShortString;
  savIdentNum  : Variant;
  savUnitNamespaces  : Array Of AnsiString;
  UnitNames  : Array Of AnsiString;
  j          : Integer;
Begin
  If IdentSym = sUses Then
  Begin
    SetLength(savUnitNamespaces, Length(UnitNamespaces));
    For i := 0 To high(UnitNamespaces) Do
      savUnitNamespaces[i] := UnitNamespaces[i];

    SetLength(UnitNames, 0);
    Repeat
      GetSym;
      ScriptData.CompilerLine    := ScannerLine;
      Expect([sIdent]);
      SetLength(UnitNames, Length(UnitNames) + 1);
      UnitNames[high(UnitNames)] := IdentStr;
      GetSym;
    Until IdentSym <> sComma;
    Expect([sSemiColon]);
    GetSym;
    ScriptData.CompilerLine := ScannerLine;

    savCurrUnit    := CurrentUnit;
    savScriptCode  := ScriptCode;
    savScannerStrPos := ScannerStrPos;
    savLine        := ScriptData.CompilerLine;
    savScannerLine := ScannerLine;
    savScannerCh   := ScannerCh;
    savIdentSym    := IdentSym;
    savIdentStr    := IdentStr;
    savIdentNum    := IdentNum;

    ScriptCode := TStringList.Create;
    Try
      For i := 0 To high(UnitNames) Do
      Begin
        UnitNames[i] := lowerCase(UnitNames[i]);
        For j := 0 To high(CompiledUnits) Do
          If CompiledUnits[j] = UnitNames[i] Then
            Break;
        If j <= high(CompiledUnits) Then
          If CompiledUnits[j] = UnitNames[i] Then
            Continue;

        SetLength(CompiledUnits, length(CompiledUnits) + 1);
        CompiledUnits[high(CompiledUnits)] := UnitNames[i];

        UName := Owner.UnitFileManager.SearchFile(UnitNames[i]);
        If not FileExists(UName) Then
        Begin
          RaiseError(ERR_UNIT_NOT_FOUND, [UnitNames[i]]);
          exit;
        End;
        ScriptCode.LoadFromFile(UName);

        SetLength(UnitNamespaces, 0);
        IdentSym                := sNone;
        IdentStr                := '';
        IdentNum                := 0;
        ScannerCh               := #0;
        ScannerStrPos           := 0;
        ScriptData.CompilerLine := 0;
        ScannerLine             := -1;
        CurrentUnit             := ExtractFileName(UName);
        If Pos('.', CurrentUnit) > 0 Then
          CurrentUnit := copy(CurrentUnit, 1, Pos('.', CurrentUnit) - 1);

        C_Unit;
      End;
    Finally
      ScriptCode.Free;
    End;

    ScriptCode       := savScriptCode;
    ScannerStrPos    := savScannerStrPos;
    ScriptData.CompilerLine := savLine;
    ScannerLine      := savScannerLine;
    ScannerCh        := savScannerCh;
    IdentSym         := savIdentSym;
    IdentStr         := savIdentStr;
    IdentNum         := savIdentNum;
    CurrentUnit      := savCurrUnit;
    CurrentNamespace := CurrentUnit;

    SetLength(UnitNamespaces, Length(UnitNames) + Length(savUnitNamespaces));
    For i := 0 To high(UnitNames) Do
      UnitNamespaces[i] := UnitNames[i];
    For i := 0 To high(savUnitNamespaces) Do
      UnitNamespaces[i + length(UnitNames)] := savUnitNamespaces[i];
  End;
End;

//==============================================================================
//==============================================================================

Function TRutisCompilerDelphi.GetPointerType(AType : TRutisTypeDecl; WantedName : AnsiString) : TRutisPointerType;
Var
  i, j  : Integer;
Begin
  For i := 0 To high(ScriptData.Namespaces) Do
    For j := 0 To high(ScriptData.Namespaces[i].Declarations) Do
      If ScriptData.Namespaces[i].Declarations[j] is TRutisPointerType Then
        If TRutisPointerType(ScriptData.Namespaces[i].Declarations[j]).PointerType = AType Then
        Begin
          Result := TRutisPointerType(ScriptData.Namespaces[i].Declarations[j]);
          exit;
        End;
  // No matching Pointer-Type found so create one
  Result := TRutisPointerType.Create;
  If (WantedName = '') Then
    Result.Name := 'P' + AType.Name + '_auto'
  Else
    Result.Name := WantedName;
  Result.PointerType := AType;
  Result.ExplicitCast := False;
  ScriptData.AddDeclaration(Result, '$SYSTEM');
End;

Function TRutisCompilerDelphi.GetArrayType(AType : TRutisTypeDecl; WantedName : AnsiString) : TRutisArrayType;
Var
  i, j  : Integer;
Begin
  For i := 0 To high(ScriptData.Namespaces) Do
    For j := 0 To high(ScriptData.Namespaces[i].Declarations) Do
      If ScriptData.Namespaces[i].Declarations[j] is TRutisArrayType Then
        If TRutisArrayType(ScriptData.Namespaces[i].Declarations[j]).ArrayType = AType Then
        Begin
          Result := TRutisArrayType(ScriptData.Namespaces[i].Declarations[j]);
          exit;
        End;
  // No matching Pointer-Type found so create one
  Result := TRutisArrayType.Create;
  If (WantedName = '') Then
    Result.Name := 'T' + AType.Name + 'Array_auto'
  Else
    Result.Name := WantedName;
  Result.ArrayType := AType;
  Result.ExplicitCast := True;
  ScriptData.AddDeclaration(Result, '$SYSTEM');
End;

Function TRutisCompilerDelphi.GetSetType(AType : TRutisEnumType; WantedName : AnsiString) : TRutisSetType;
Var
  i, j  : Integer;
Begin
  For i := 0 To high(ScriptData.Namespaces) Do
    For j := 0 To high(ScriptData.Namespaces[i].Declarations) Do
      If ScriptData.Namespaces[i].Declarations[j] is TRutisSetType Then
        If TRutisSetType(ScriptData.Namespaces[i].Declarations[j]).EnumType = AType Then
        Begin
          Result := TRutisSetType(ScriptData.Namespaces[i].Declarations[j]);
          exit;
        End;
  // No matching Pointer-Type found so create one
  Result := TRutisSetType.Create;
  If (WantedName = '') Then
    Result.Name := 'T' + AType.Name + 's_auto'
  Else
    Result.Name := WantedName;
  Result.EnumType := AType;
  Result.ExplicitCast := True;
  ScriptData.AddDeclaration(Result, '$SYSTEM');

  Result.Size := 1;
  For I := 0 To high(Result.EnumType.Enums) Do
    If Result.EnumType.Enums[i].Value >= Result.Size * 8 Then
    Begin
      Case Result.Size Of
        2  : Result.Size := 4;
        4  : Result.Size := 8;
      Else
        Inc(Result.Size);
      End;
    End;
End;

//==============================================================================

Function TRutisCompilerDelphi.C_DeclTypeInline(NewTypeName : AnsiString = '') : TRutisTypeDecl;
Var
  TypeDecl      : TRutisTypeDecl;
  ExplicitCast  : Boolean;

  Function GetEnumType : TRutisTypeDecl;
  Var
    int, i   : Integer;
    Bool     : Boolean;
    VarDecl  : TRutisVarDecl;
  Begin
    GetSym;
    Result := TRutisEnumType.Create;

    If NewTypeName <> '' Then
      Result.Name := NewTypeName
    Else
      Result.Name := 'TEnum_ID' + IntToStr(random(1024));
    ScriptData.AddDeclaration(Result, CurrentNamespace);

    Result.Size := 1;
    With TRutisEnumType(Result) Do
    Begin
      While True Do
      Begin
        Expect([sIdent]);
        If CheckRedefinition(IdentStr) Then
          RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);

        VarDecl      := TRutisVarDecl.Create;
        VarDecl.Name := IdentStr;
        ScriptData.AddDeclaration(VarDecl, CurrentNamespace);

        VarDecl.IsConst := True;
        VarDecl.Level   := 0;
        VarDecl.Address := ScriptData.Ressources.Top;
        VarDecl.VarType := Result;

        GetSym;
        If IdentSym = sEqual Then
        Begin
          GetSym;
          Expect([sInteger]);
          int := 0;
          For I := 0 To high(Enums) Do
            If Enums[i].Value = int Then
              RaiseError('The value %d already exists in this enumaration type', [int]);
        End
        Else
        Begin
          int := 0;
          Repeat
            Bool := True;
            For I := 0 To high(Enums) Do
              If Enums[i].Value = int Then
              Begin
                Bool := False;
                Inc(int);
              End;
          Until Bool;
        End;
        If int < 0 Then
          RaiseError('The enum-value ''%d'' is too low. It needs to be positive.', [int]);
        If int >= 32 Then
          RaiseError('The enum-value ''%d'' is too big. It needs to be lower than 32.', [int]);

        SetLength(Enums, length(Enums) + 1);
        Enums[high(Enums)].Name  := VarDecl.Name;
        Enums[high(Enums)].Value := int;
        ScriptData.Ressources.PushByte(int);

        If IdentSym = sCloseBracket Then break;
        Expect([sComma]);
        GetSym;
      End;
    End;
    GetSym;
  End;

Begin
  Result       := nil;
  ExplicitCast := False;

  If IdentSym = sType Then
  Begin
    ExplicitCast := True;
    GetSym;
  End;

  // Enum-Types
  If IdentSym = sOpenBracket Then
  Begin
    Result := GetEnumType;
    exit;
  End;

  // Set-Types
  If IdentSym = sSet Then
  Begin
    GetSym;
    Expect([sOf]);
    GetSym;
    TypeDecl := C_DeclTypeInline();
    If not (TypeDecl is TRutisEnumType) Then
      RaiseError(ERR_NEEDED_FOUND, [RutisVarTypeTOString[intEnum], RutisVarTypeTOString[TypeDecl.InternalType]]);

    Result := GetSetType(TRutisEnumType(TypeDecl), NewTypeName);
    exit;
  End;

  If IdentSym = sArray Then
  Begin
    GetSym;
    Expect([sOf]);
    GetSym;
    TypeDecl := C_DeclTypeInline();

    Result := GetArrayType(TypeDecl, NewTypeName);
    exit;
  End;

  If IdentSym = sHook Then
  Begin
    GetSym;
    TypeDecl := C_DeclTypeInline();

    Result := GetPointerType(TypeDecl, NewTypeName);
    Result.ExplicitCast := ExplicitCast;
    exit;
  End;

  Expect([sIdent]);

  Result := TRutisTypeDecl(GetDeclaration(IdentStr));
  If Result = nil Then
  Begin
    RaiseError(ERR_UNKNOWN_IDENT, [IdentStr]);
    exit;
  End;
  If not (Result is TRutisTypeDecl) Then
  Begin
    RaiseError(ERR_TYPE_EXPECTED);
    exit;
  End;

  GetSym;

  If (NewTypeName <> '') and (NewTypeName <> Result.Name) Then
  Begin
    TypeDecl      := TRutisTypeCopy.Create;
    TypeDecl.Name := NewTypeName;
    TypeDecl.ExplicitCast := ExplicitCast;
    TRutisTypeCopy(TypeDecl).CopyType := Result;
    TRutisTypeCopy(TypeDecl).Size := Result.Size;

    //ScriptData.AddDeclaration(TypeDecl, '$SYSTEM');
    ScriptData.AddDeclaration(TypeDecl, CurrentNamespace);
    Result := TypeDecl;
  End;
End;

Procedure TRutisCompilerDelphi.C_DeclType;
Var
  int, i, j    : Integer;
  RasterSize   : Byte;
  TypeName     : AnsiString;
  VType        : TRutisTypeDecl;
  Parent       : TRutisStructType;
  StructType   : TRutisStructType;
  AClassType   : TRutisClassType;
  ParentClass  : TRutisClassType;
  VarNames     : Array Of AnsiString;
  fIsPacked    : Boolean;
Begin
  Expect([sIdent]);

  TypeName := IdentStr;
  If CheckRedefinition(TypeName) Then
  Begin
    i := ScriptData.GetDeclarationId(TypeName, '$STATIC');
    If i >= 0 Then
    Begin
      {Repeat
        GetSym;
        If IdentSym = sRecord Then
        Begin
          Repeat
            GetSym;
          Until IdentSym in [sEnd, sNone, sUnknown];
        End;
      Until IdentSym in [sSemiColon, sNone, sUnknown];
      GetSym;
      exit; }
    End
    Else
    Begin
      RaiseError(ERR_INDENT_REDEFINED, [TypeName]);
      exit;
    End;
  End;
  GetSym;

  Expect([sEqual]);
  GetSym;

  If IdentSym = sPacked Then
  Begin
    fIsPacked := True;
    GetSym;
  End
  Else
    fIsPacked := False;

  If IdentSym = sExternal Then
  Begin
    GetSym;
    Expect([sClass]);
    GetSym;

    StructType := TRutisStructType.Create;
    ScriptData.AddDeclaration(StructType, CurrentNamespace);
    With StructType Do
    Begin
      Name       := TypeName + '_CLASSDATA';
      IsPacked   := fIsPacked;

      If IdentSym = sOpenBracket Then
      Begin
        GetSym;
        Expect([sIdent]);

        ParentClass := GetDeclaration(IdentStr) as TRutisClassType;
        If not (ParentClass is TRutisClassType) Then
        Begin
          RaiseError('Class-Type expected'); exit;
        End;
        GetSym;
        Expect([sCloseBracket]);
        GetSym;
      End
      Else
        ParentClass := Owner.decl_TObject;

      Parent     := ParentClass.ClassStruct;
      Size       := Parent.Size;
      fAlignSize := Parent.fAlignSize;
      //IsPacked   := Parent.IsPacked;

      SetLength(StructTypes, Length(TRutisStructType(Parent).StructTypes));
      For i := 0 To high(TRutisStructType(Parent).StructTypes) Do
      Begin
        StructTypes[i]         := TRutisVarDecl.Create;
        StructTypes[i].VarType := TRutisStructType(Parent).StructTypes[i].VarType;
        StructTypes[i].Address := TRutisStructType(Parent).StructTypes[i].Address;
        StructTypes[i].isConst := TRutisStructType(Parent).StructTypes[i].isConst;
        StructTypes[i].Level   := TRutisStructType(Parent).StructTypes[i].Level;
        StructTypes[i].Name    := TRutisStructType(Parent).StructTypes[i].Name;
      End;
      {
      else
      begin
        SetLength(StructTypes, 1);
        StructTypes[0]         := TRutisVarDecl.Create;
        StructTypes[0].VarType := Owner.decl_VMT;
        StructTypes[0].Address := 0;
        StructTypes[0].Name    := 'VMT';
        Size := 4;
      end;
      }

      While True Do
      Begin
        SetLength(VarNames, 0);
        While True Do
        Begin
          Expect([sIdent]);
          SetLength(VarNames, Length(VarNames) + 1);
          VarNames[High(VarNames)] := IdentStr;
          GetSym;
          If IdentSym <> scomma Then Break;
          GetSym;
        End;
        Expect([sDopDot]);
        GetSym;

        VType := C_DeclTypeInline();
        If VType = nil Then exit;

        int := high(StructTypes);
        SetLength(StructTypes, Length(StructTypes) + Length(VarNames));

        If optArrangeFields and not IsPacked Then
        Begin
          RasterSize := Max(Min(VType.AlignSize, optArrangeSize), 1);
          fAlignSize := max(AlignSize, RasterSize);
          j := RasterSize - (Size mod RasterSize);
          If j < RasterSize Then
          Begin
          //ScriptData.GenCode(_gen, j, 0, 0);
            Size := Size + j;
          End;
        End;
        For i := int + 1 To high(StructTypes) Do
        Begin
          StructTypes[i] := TRutisVarDecl.Create;
          StructTypes[i].VarType := VType;
          StructTypes[i].Address := Size;
          StructTypes[i].Name := VarNames[i - int - 1];
          Size := Size + VType.Size;
        End;

        //GetSym;
        Expect([sSemiColon]);
        GetSym;
        If not (IdentSym in [sIdent, sSemiColon]) Then break;
      End;
      If optArrangeFields and not IsPacked Then
      Begin
        j := AlignSize - (Size mod AlignSize);
        If j < AlignSize Then
        Begin
        //ScriptData.GenCode(_gen, j, 0, 0);
          Size := Size + j;
        End;
      End;

      Expect([sEnd]);
      GetSym;
      Expect([sSemiColon]);
      GetSym;
    End;
    AClassType      := TRutisClassType.Create;
    AClassType.Name := TypeName;
    AClassType.ClassStruct := StructType;
    AClassType.IsExternal := True;
    AClassType.ParentClass := ParentClass;
    ScriptData.AddDeclaration(AClassType, CurrentNamespace);
    exit;
  End;

  If fIsPacked Then
    Expect([sRecord]);

  If IdentSym = sRecord Then
  Begin
    GetSym;

    StructType := TRutisStructType.Create;
    ScriptData.AddDeclaration(StructType, CurrentNamespace);
    With StructType Do
    Begin
      Name       := TypeName;
      Size       := 0;
      IsPacked   := fIsPacked;
      fAlignSize := 1;

      If IdentSym = sOpenBracket Then
      Begin
        GetSym;
        Expect([sIdent]);

        Parent := TRutisStructType(GetDeclaration(IdentStr));
        If not (Parent is TRutisStructType) Then
        Begin
          RaiseError('Struct-Type expected'); exit;
        End;
        Size       := TRutisStructType(Parent).Size;
        fAlignSize := TRutisStructType(Parent).fAlignSize;
        //IsPacked   := TRutisStructType(AParent).IsPacked;

        SetLength(StructTypes, Length(TRutisStructType(Parent).StructTypes));
        For i := 0 To high(TRutisStructType(Parent).StructTypes) Do
        Begin
          StructTypes[i]         := TRutisVarDecl.Create;
          StructTypes[i].VarType := TRutisStructType(Parent).StructTypes[i].VarType;
          StructTypes[i].Address := TRutisStructType(Parent).StructTypes[i].Address;
          StructTypes[i].isConst := TRutisStructType(Parent).StructTypes[i].isConst;
          StructTypes[i].Level   := TRutisStructType(Parent).StructTypes[i].Level;
          StructTypes[i].Name    := TRutisStructType(Parent).StructTypes[i].Name;
        End;

        GetSym;
        Expect([sCloseBracket]);
        GetSym;
      End;

      While True Do
      Begin
        SetLength(VarNames, 0);
        While True Do
        Begin
          Expect([sIdent]);
          SetLength(VarNames, Length(VarNames) + 1);
          VarNames[High(VarNames)] := IdentStr;
          GetSym;
          If IdentSym <> scomma Then Break;
          GetSym;
        End;
        Expect([sDopDot]);
        GetSym;

        VType := C_DeclTypeInline();
        If VType = nil Then exit;

        int := high(StructTypes);
        SetLength(StructTypes, Length(StructTypes) + Length(VarNames));

        If optArrangeFields and not IsPacked Then
        Begin
          RasterSize := Max(Min(VType.AlignSize, optArrangeSize), 1);
          fAlignSize := max(AlignSize, RasterSize);
          j := RasterSize - (Size mod RasterSize);
          If j < RasterSize Then
          Begin
          //ScriptData.GenCode(_gen, j, 0, 0);
            Size := Size + j;
          End;
        End;
        For i := int + 1 To high(StructTypes) Do
        Begin
          StructTypes[i] := TRutisVarDecl.Create;
          StructTypes[i].VarType := VType;
          StructTypes[i].Address := Size;
          StructTypes[i].Name := VarNames[i - int - 1];
          Size := Size + VType.Size;
        End;

        //GetSym;
        Expect([sSemiColon]);
        GetSym;
        If not (IdentSym in [sIdent, sSemiColon]) Then break;
      End;
      If optArrangeFields and not IsPacked Then
      Begin
        j := AlignSize - (Size mod AlignSize);
        If j < AlignSize Then
        Begin
        //ScriptData.GenCode(_gen, j, 0, 0);
          Size := Size + j;
        End;
      End;

      Expect([sEnd]);
      GetSym;
      Expect([sSemiColon]);
      GetSym;
    End;
    exit;
  End;

  If IdentSym = sProcedure Then
  Begin
    GetSym;
    RaiseError(ERR_NOT_IMPLEMENTED_YET);
    exit;
  End;

  // No record type so get "normal" type

  C_DeclTypeInline(TypeName);
  Expect([sSemiColon]);
  GetSym;
End;

Procedure TRutisCompilerDelphi.C_DeclTypes;
Begin
  While IdentSym = sIdent Do
  Begin
    C_DeclType;
  End;
End;

Procedure TRutisCompilerDelphi.C_DeclVars;
Var
  i, j        : Integer;
  VarDecl     : TRutisVarDecl;
  VType       : TRutisTypeDecl;
  VarNames    : Array Of AnsiString;
  RasterSize  : Byte;
Begin
  While True Do
  Begin
    SetLength(VarNames, 0);
    While True Do
    Begin
      Expect([sIdent]);

      If CheckRedefinition(IdentStr) Then
        RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);
      for I := 0 to high(VarNames) do
        If VarNames[i] = IdentStr then
          RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);

      SetLength(VarNames, Length(VarNames) + 1);
      VarNames[High(VarNames)] := IdentStr;
      
      GetSym;
      If IdentSym <> scomma Then Break;
      GetSym;
    End;
    Expect([sDopDot]);
    GetSym;

    VType := C_DeclTypeInline();

    If VType = nil Then exit;

    For i := 0 To high(VarNames) Do
    Begin
      VarDecl := TRutisVarDecl.Create;
      ScriptData.AddDeclaration(VarDecl, CurrentNamespace);

      If optArrangeFields Then
      Begin
        RasterSize := Max(Min(VType.AlignSize, optArrangeSize), 1);
        j := RasterSize - (StackVarPos mod RasterSize);
        If j < RasterSize Then
        Begin
        //ScriptData.GenCode(_gen, j, 0, 0);
          StackVarPos := StackVarPos + j;
        End;
      End;

      VarDecl.Name    := VarNames[i];
      VarDecl.IsConst := False;
      VarDecl.VarType := VType;
      VarDecl.Level   := fCompilerLevel;
      VarDecl.Address := StackVarPos;
      //ScriptData.GenCode(_gen, VType.Size, 0, 0);
      StackVarPos     := StackVarPos + VType.Size;
    End;

    //GetSym;
    Expect([sSemiColon]);
    While IdentSym = sSemiColon Do GetSym;

    If not (IdentSym in [sIdent, sSemiColon]) Then break;
  End;
End;

Procedure TRutisCompilerDelphi.C_DeclParams;
Var
  i         : Integer;
  VarDecl   : TRutisVarDecl;
  VType     : TRutisTypeDecl;
  VarNames  : Array Of AnsiString;
Begin
  If IdentSym in [sSemiColon, sDopDot] Then exit;

  Expect([sOpenBracket]);
  GetSym;
  If IdentSym = sCloseBracket Then
  Begin
    GetSym;
    exit;
  End;

  While True Do
  Begin
    SetLength(VarNames, 0);
    While True Do
    Begin
      Expect([sIdent]);
      If CheckRedefinition(IdentStr) Then
      Begin
        RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);
        exit;
      End;
      SetLength(VarNames, Length(VarNames) + 1);
      VarNames[High(VarNames)] := IdentStr;
      GetSym;
      If IdentSym <> scomma Then Break;
      GetSym;
    End;
    Expect([sDopDot]);
    GetSym;

    VType := C_DeclTypeInline();

    If VType = nil Then exit;

    For i := 0 To high(VarNames) Do
    Begin
      VarDecl := TRutisVarDecl.Create;
      ScriptData.AddDeclaration(VarDecl, CurrentNamespace);

      VarDecl.Name    := VarNames[i];
      VarDecl.IsConst := False;
      VarDecl.VarType := VType;
      VarDecl.Level   := fCompilerLevel;
    End;

    If IdentSym <> sSemiColon Then break;
    While IdentSym = sSemiColon Do GetSym;
    //GetSym;
  End;

  Expect([sCloseBracket]);
  GetSym;
End;

Procedure TRutisCompilerDelphi.C_DeclConsts;
Var
  VarDecl  : TRutisVarDecl;
  VarType  : TRutisTypeDecl;
  signum   : Shortint;
  //Str      : PAnsiString;
Begin
  While True Do
  Begin
    Expect([sIdent]);
    If CheckRedefinition(IdentStr) Then
      RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);

    VarDecl := TRutisVarDecl.Create;
    ScriptData.AddDeclaration(VarDecl, CurrentNamespace);

    VarDecl.Name    := IdentStr;
    VarDecl.IsConst := True;
    VarDecl.Level   := 0;
    VarDecl.Address := ScriptData.Ressources.Top;

    GetSym;

    VarType := nil;
    If IdentSym = sDopDot Then
    Begin
      GetSym;
      Expect([sIdent]);

      VarType := C_DeclTypeInline;
      If VarType = nil Then
        RaiseError(ERR_TYPE_EXPECTED);
    End;

    Expect([sEqual]);
    GetSym;

    If IdentSym = sMinus Then
    Begin
      GetSym;
      signum := -1;
    End
    Else
      signum := 1;

    If not (IdentSym in [sInteger..sString, sTrue, sFalse]) Then exit;

    Case IdentSym Of
      sString :
      Begin
        If length(IdentStr) = 1 Then
        Begin
        // Save Ressource
          ScriptData.Ressources.PushByte(Byte(IdentStr[1]));
          VarDecl.ConstVal := IdentStr[1];
          // Get Ressource-Type
          If VarType <> nil Then
          Begin
            If VarType.Size <> 1 Then
              RaiseError(ERR_INCOMPATIBLE_TYPES, [VarType.Name, Owner.decl_AChar.Name]);
            VarDecl.VarType := VarType;
          End
          Else
            VarDecl.VarType := TRutisTypeDecl(Owner.decl_AChar);
        End
        Else
        Begin
        // Save Ressource
          VarDecl.Address := ScriptData.Ressources.Top + 4;
          IdentStr := IdentStr;
          ScriptData.Ressources.PushAStringData(IdentStr);
          VarDecl.ConstVal := IdentStr;
          // Get Ressource-Type
          If VarType <> nil Then
          Begin
          //If VarType.Size <> 4 then
            RaiseError(ERR_INCOMPATIBLE_TYPES, [VarType.Name, Owner.decl_AString.Name]);
          End
          Else
            VarDecl.VarType := TRutisTypeDecl(Owner.decl_AString);
        End;
        If (VarDecl.VarType = nil) or
          (not (VarDecl.VarType is TRutisTypeDecl)) Then
        Begin
          RaiseError(ERR_NOT_AVAILABLE);
          exit;
        End;
      End;
      sInteger :
      Begin
      // Save Ressource
        IdentNum := signum * IdentNum;

        VarDecl.ConstVal := IdentNum;
        // Get Ressource-Type
        If VarType <> nil Then
        Begin
          VarDecl.VarType := VarType;
          If VarType.InternalType = intByte Then
          Begin
            If (IdentNum < 0) or (IdentNum >= 256) Then
              RaiseError('Value must be between 0 and 255');
            ScriptData.Ressources.PushByte(IdentNum);
          End
          Else
          If VarType.InternalType = intShortInt Then
          Begin
            If (IdentNum <= -128) or (IdentNum >= 128) Then
              RaiseError('Value must be between -127 and 127');
            ScriptData.Ressources.PushShortInt(IdentNum);
          End
          Else
          If VarType.InternalType = intWord Then
          Begin
            If (IdentNum < 0) or (IdentNum >= 65536) Then
              RaiseError('Value must be between 0 and 65535');
            ScriptData.Ressources.PushWord(IdentNum);
          End
          Else
          If VarType.InternalType = intSmallint Then
          Begin
            If (IdentNum <= -32768) or (IdentNum >= 32768) Then
              RaiseError('Value must be between -32767 and 32767');
            ScriptData.Ressources.PushSmallint(IdentNum);
          End
          Else
          If VarType.InternalType = intCardinal Then
            ScriptData.Ressources.PushCardinal(IdentNum)
          Else
          If VarType.InternalType = intInteger Then
            ScriptData.Ressources.PushInteger(IdentNum)
          Else
            RaiseError(ERR_INCOMPATIBLE_TYPES, [VarType.Name, Owner.decl_Integer.Name]);
        End
        Else
        Begin
          ScriptData.Ressources.PushCardinal(Cardinal(IdentNum));
          VarDecl.VarType := Owner.decl_Integer;
        End;
      End;
      sFloat :
      Begin
      // Save Ressource
        IdentNum         := signum * IdentNum;
        VarDecl.ConstVal := IdentNum;
        // Get Ressource-Type
        If VarType <> nil Then
        Begin
          VarDecl.VarType := VarType;
          If VarType.InternalType = intSingle Then
            ScriptData.Ressources.PushSingle(IdentNum)
          Else
          If VarType.InternalType = intDouble Then
            ScriptData.Ressources.PushDouble(IdentNum)
          Else
          If VarType.InternalType = intExtended Then
            ScriptData.Ressources.PushExtended(IdentNum)
          Else
            RaiseError(ERR_INCOMPATIBLE_TYPES, [VarType.Name, Owner.decl_Integer.Name]);
        End
        Else
        Begin
          ScriptData.Ressources.PushSingle(IdentNum);
          VarDecl.VarType := Owner.decl_Single;
        End;
      End;
      sTrue, sFalse :
      Begin
      // Save Ressource
        VarDecl.ConstVal := Boolean(IdentSym = sTrue);
        ScriptData.Ressources.PushByte(Byte(IdentSym = sTrue));
        // Get Ressource-Type
        VarDecl.VarType  := Owner.decl_Boolean;
        If VarType <> nil Then
        Begin
          If VarType.Size <> 1 Then
            RaiseError(ERR_INCOMPATIBLE_TYPES, [VarType.Name, VarDecl.VarType.Name]);
          VarDecl.VarType := VarType;
        End;
      End;
    Else
      RaiseError(ERR_NOT_AVAILABLE_C_TYPE);
      exit;
    End;

    {!! Arrange Res-Fields}
    while ScriptData.Ressources.Top mod 4 <> 0 do
      ScriptData.Ressources.PushByte(0);

    GetSym;
    Expect([sSemiColon]);
    GetSym;
    While IdentSym = sSemiColon Do GetSym;

    If not (IdentSym = sIdent) Then break;
  End;
End;

Procedure TRutisCompilerDelphi.C_DeclMethod;
Var
  bool  : Boolean;
  MethodDecl  : TRutisMethodType;
  DllMethodDecl  : TRutisDllMethodType;
  MethodVar  : TRutisVarDecl;
  I  : Integer;

  Function GetMethodParams : Boolean;
  Var
    i         : Integer;
    isVar     : Boolean;
    VType     : TRutisTypeDecl;
    VarNames  : Array Of AnsiString;
  Begin
    Result := False;
    If IdentSym <> sOpenBracket Then
    Begin
      Result := True;
      MethodDecl.ParamsSize := 0;
      exit;
    End;
    GetSym;

    While True Do
    Begin
      isVar := True;
      If IdentSym = sVar Then
      Begin
        isVar := True;
        GetSym;
      End;
      SetLength(VarNames, 0);
      While True Do
      Begin
        Expect([sIdent]);
        If CheckRedefinition(IdentStr) Then
        Begin
          RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);
          exit;
        End;
        SetLength(VarNames, Length(VarNames) + 1);
        VarNames[High(VarNames)] := IdentStr;
        GetSym;
        If IdentSym <> scomma Then Break;
        GetSym;
      End;
      Expect([sDopDot]);
      GetSym;

      VType := C_DeclTypeInline();

      If VType = nil Then exit;

      For i := 0 To high(VarNames) Do
      Begin
        SetLength(MethodDecl.Params, Length(MethodDecl.Params) + 1);
        With MethodDecl.Params[high(MethodDecl.Params)] Do
        Begin
          TypeData   := VType;
          isVarParam := isVar;
        End;
        MethodDecl.ParamsSize := MethodDecl.ParamsSize + VType.Size;
      End;

      If IdentSym <> sSemiColon Then break;
      While IdentSym = sSemiColon Do GetSym;
    End;

    Expect([sCloseBracket]);
    GetSym;
    Result := True;
  End;

Begin
  If not (IdentSym in [sProcedure, sFunction]) Then exit;

  bool := IdentSym = sFunction;
  GetSym;
  Expect([sIdent]);
  If CheckRedefinition(IdentStr) Then
  Begin
    RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);
    exit;
  End;

  MethodVar := TRutisVarDecl.Create;
  ScriptData.AddDeclaration(MethodVar, CurrentNamespace);

  MethodVar.Name    := IdentStr;
  MethodVar.IsConst := True;
  MethodVar.Level   := 0;
  MethodVar.VarType := TRutisMethodType.Create;
  MethodDecl        := TRutisMethodType(MethodVar.VarType);

  MethodVar.Address := ScriptData.Ressources.Top;
  ScriptData.Ressources.PushCardinal(Cardinal(high(ScriptData.Code) + 1));

  Try
    With MethodDecl Do
    Begin
      ParamsSize := 0;
      IsFunction := bool;
      If IsFunction Then
        Name := 'function'
      Else
        Name := 'procedure';

      GetSym;

      If not GetMethodParams Then
      Begin
        MethodDecl.Free; exit;
      End;

      If IsFunction Then
      Begin
        Expect([sDopDot]);
        GetSym;

        MethodResult.TypeData := TRutisTypeDecl(GetDeclaration(IdentStr));
        If (MethodResult.TypeData = nil) or (not (MethodResult.TypeData is TRutisTypeDecl)) Then
        Begin
          RaiseError(ERR_TYPE_EXPECTED);
          MethodDecl.Free;
          exit;
        End;
        GetSym;
      End;

      Expect([sSemiColon]);
      GetSym;
    End;

  Except
    MethodDecl.Free;
    Raise;
  End;

  //function MeineFunc(...); external 'MeineDll.dll' name 'MeinFunc';
  If IdentSym = sExternal Then
  Begin
    GetSym;
    DllMethodDecl        := TRutisDllMethodType.Create;
    DllMethodDecl.Method_Name := MethodDecl.Method_Name;
    DllMethodDecl.Description := MethodDecl.Description;
    DllMethodDecl.Params := MethodDecl.Params;
    DllMethodDecl.ParamsSize := MethodDecl.ParamsSize;
    DllMethodDecl.IsFunction := MethodDecl.IsFunction;
    DllMethodDecl.MethodResult.TypeData := MethodDecl.MethodResult.TypeData;
    DllMethodDecl.Declarations := MethodDecl.Declarations;
    DllMethodDecl.Size   := MethodDecl.Size;
    DllMethodDecl.Name   := MethodDecl.Name;
    MethodVar.VarType    := DllMethodDecl;
    MethodDecl.Free;
    ScriptData.AddDeclaration(DllMethodDecl, CurrentNamespace);

    If IdentSym = sIdent Then
    Begin
      RaiseError('DLL-Name and Method-Name can not be Variables or Constants. Only Strings with '' '' are usable'); exit;
    End;
    Expect([sString]);
    DllMethodDecl.DllName := IdentStr;
    GetSym;

    Expect([sIdent]);
    If IdentStr <> 'NAME' Then
    Begin
      RaiseError(ERR_EXPECTED_FOUND, ['name', IdentStr]);
      exit;
    End;
    GetSym;

    If IdentSym = sIdent Then
    Begin
      RaiseError('DLL-Name and Method-Name can not be Variables or Constants. Only Strings with '' '' are usable'); exit;
    End;
    Expect([sString]);
    DllMethodDecl.ProcName := IdentStr;
    GetSym;

    Expect([sSemiColon]);
    GetSym;

    For I := 0 To high(DllMethodDecl.Params) Do
      If DllMethodDecl.Params[i].isVarParam Then
      Begin
        DllMethodDecl.ParamsSize         := DllMethodDecl.ParamsSize -
          DllMethodDecl.Params[i].TypeData.Size + 4;
        DllMethodDecl.Params[i].TypeData :=
          GetPointerType(DllMethodDecl.Params[i].TypeData);
        //DllMethodDecl.Params[i].isVarParam := false;
      End;

    exit;
  End;

  ScriptData.AddDeclaration(MethodDecl, CurrentNamespace);
  exit;
End;

Function TRutisCompilerDelphi.C_DeclarationSeq : Boolean;
Begin
  Result := False;

  If IdentSym = sType Then
  Begin
    Result := True;
    GetSym;
    C_DeclTypes;
    exit;
  End;

  If IdentSym = sVar Then
  Begin
    Result := True;
    GetSym;
    C_DeclVars;
    exit;
  End;

  If (IdentSym in [sProcedure, sFunction]) and fCompilingDefines Then
  Begin
    Result := True;
    C_DeclMethod;
    exit;
  End;

  If IdentSym = sConst Then
  Begin
    Result := True;
    GetSym;
    C_DeclConsts;
    exit;
  End;
End;

Procedure TRutisCompilerDelphi.C_Declarations;
Begin
  Repeat Until not C_DeclarationSeq;
End;

//==============================================================================

Function TRutisCompilerDelphi.C_Method : Boolean;
Var
  i, j          : Integer;
  bool          : Boolean;
  MethodDecl    : TRutisMethodType;
  MethodVar     : TRutisVarDecl;
  VarDecl       : TRutisVarDecl;
  OldStackVarPos  : Integer;
  OldNamespace  : AnsiString;
  Namespace     : TRutisNamespace;
  MethodInitAddr  : Integer;

Begin
  Result := False;
  If not (IdentSym in [sProcedure, sFunction]) Then exit;

  bool := IdentSym = sFunction;
  GetSym;
  Expect([sIdent]);
  If CheckRedefinition(IdentStr) Then
  Begin
    RaiseError(ERR_INDENT_REDEFINED, [IdentStr]);
    exit;
  End;

  //{!!}Owner.OnError('-> C_Method ' + IdentStr, etHint);

  If fCompilerLevel > 0 Then
  Begin
    ScriptData.GenCode(_jmp, 0, 0, 0);
    MethodInitAddr := high(ScriptData.Code);
  End
  Else
    MethodInitAddr := -1;

  Inc(fCompilerLevel);

  MethodVar := TRutisVarDecl.Create;
  ScriptData.AddDeclaration(MethodVar, CurrentNamespace);

  MethodVar.Name    := IdentStr;
  MethodVar.IsConst := True;
  MethodVar.Level   := fCompilerLevel;
  MethodVar.VarType := TRutisMethodType.Create;
  MethodVar.Address := ScriptData.Ressources.Top;
  ScriptData.Ressources.PushCardinal(Cardinal(high(ScriptData.Code) + 1));

  MethodDecl := TRutisMethodType(MethodVar.VarType);

  // Create Namespace...
  OldNamespace     := CurrentNamespace;
  CurrentNamespace := UpperCase(CurrentNamespace + '.' + MethodVar.Name);
  Namespace        := TRutisNamespace.Create;
  Namespace.Name   := CurrentNamespace;
  Namespace.CodeFile := CurrentUnit;
  Namespace.CodeStart := length(ScriptData.Code);

  SetLength(ScriptData.Namespaces, length(ScriptData.Namespaces) + 1);
  ScriptData.Namespaces[high(ScriptData.Namespaces)] := Namespace;
  // ... and add it to the Current-Uses List
  SetLength(UnitNamespaces, length(UnitNamespaces) + 1);
  UnitNamespaces[high(UnitNamespaces)] := CurrentNamespace;

  OldStackVarPos := StackVarPos;
  StackVarPos    := 0;
  VarDecl        := nil;

  Try
    With MethodDecl Do
    Begin
      ParamsSize := 0;
      IsFunction := bool;
      MethodResult.TypeData := nil;
      If IsFunction Then
        Name := 'function'
      Else
        Name := 'procedure';

      GetSym;

      If IsFunction Then
      Begin
        VarDecl := TRutisVarDecl.Create;
        ScriptData.AddDeclaration(VarDecl, Namespace.Name);
        With VarDecl Do
        Begin
          Name    := 'RESULT';
          IsConst := False;
          Address := StackVarPos;
          Level   := fCompilerLevel;
        End;
        Inc(StackVarPos);
      End;

      If IdentSym = sOpenBracket Then
        C_DeclParams;

      If IsFunction Then
      Begin
        Expect([sDopDot]);

        GetSym;

        MethodResult.TypeData := TRutisTypeDecl(GetDeclaration(IdentStr));
        VarDecl.VarType       := MethodResult.TypeData;
        If (VarDecl.VarType = nil) or
          (not (VarDecl.VarType is TRutisTypeDecl)) Then
        Begin
          RaiseError(ERR_TYPE_EXPECTED);
          MethodDecl.Free;
          exit;
        End;
        GetSym;
      End;

      //Get Parameters
      If IsFunction Then
        i   := 1
      Else
        i   := 0;
      ParamsSize := 0;
      SetLength(Params, 0);
      For i := i To high(Namespace.Declarations) Do
        If Namespace.Declarations[i] is TRutisVarDecl Then
        Begin
          ParamsSize := ParamsSize + TRutisVarDecl(Namespace.Declarations[i]).VarType.Size;
          SetLength(Params, length(Params) + 1);
          Params[high(Params)].TypeData := TRutisVarDecl(Namespace.Declarations[i]).VarType;
        End;

      //Get Parameter-Addresses
      j := -ParamsSize;
      If IsFunction Then
        j := j - MethodResult.TypeData.Size;
      For i := 0 To high(Namespace.Declarations) Do
        If Namespace.Declarations[i] is TRutisVarDecl Then
        Begin
          TRutisVarDecl(Namespace.Declarations[i]).Address := j;
          j := j + TRutisVarDecl(Namespace.Declarations[i]).VarType.Size;
        End;
      StackVarPos := 0;

      Expect([sSemiColon]);
      GetSym;
    End;

    Repeat
      bool := C_DeclarationSeq or C_Method;
    Until not bool;

    Expect([sBegin]);
    GetSym;

    ExitParamsSize := MethodDecl.ParamsSize;
    // MethodenfCode ======

    If StackVarPos > 0 Then
    Begin
      ScriptData.GenCode(_gen, StackVarPos, 0, 0);
      StackVarPos := 0;
    End;

    C_StatementSequence;

    ScriptData.GenCode(_ret, MethodDecl.ParamsSize, 0, 0);

    If MethodInitAddr >= 0 Then
      ScriptData.Code[MethodInitAddr].P1 := high(ScriptData.Code) + 1;

    Expect([sEnd]);
    GetSym;
    Expect([sSemiColon]);
    GetSym;

    // Reset Namespace
    Namespace.CodeEnd := high(ScriptData.Code);
    SetLength(UnitNamespaces, length(UnitNamespaces) - 1);
    CurrentNamespace  := OldNamespace;

    //Reset StackVarPos and CurrentNamespace and decrement CompilerLevel
    StackVarPos := OldStackVarPos;
    Dec(fCompilerLevel);

    bool := False;
    For i := 0 To ScriptData.DeclarationCount - 1 Do
      If ScriptData.DeclarationList[i].ClassType = TRutisMethodType Then
        With TRutisMethodType(ScriptData.DeclarationList[i]) Do
        Begin
          If (IsFunction <> MethodDecl.IsFunction) Then Continue;
          If (length(Params) <> length(MethodDecl.Params)) Then Continue;
          If (ParamsSize <> MethodDecl.ParamsSize) Then Continue;
          If (MethodResult.TypeData <> MethodDecl.MethodResult.TypeData) Then Continue;
          bool := True;
          For j := 0 To high(Params) Do
            If Params[j].TypeData <> MethodDecl.Params[j].TypeData Then
            Begin
              bool := False;
              Break;
            End;
          If not bool Then Continue;
          Break;
        End;
    If bool Then
    Begin
      MethodDecl.Free;
      MethodVar.VarType := TRutisMethodType(ScriptData.DeclarationList[i]);
    End
    Else
      ScriptData.AddDeclaration(MethodDecl, '$SYSTEM');

  Except
    MethodDecl.Free;
    Raise;
  End;

  //{!!}Owner.OnError('<- C_Method ' + MethodVar.Name, etHint);
  Result := True;
End;

Procedure TRutisCompilerDelphi.C_Implementation;
Begin
  Repeat
    C_Declarations;
  Until not C_Method;
End;

//==============================================================================

Function TRutisCompilerDelphi.C_AnalyzeIdent(IsExpression : Boolean; WantType : TRutisIntType; Var Inf : TAnalyzeIdentInfo) : TIdentType;
Var
  HadPointer   : Boolean;
  HadAt        : Boolean;
  ExcludeList  : Array Of TRutisDecl;

  Procedure AnalyzeVar;
  Var id  : Integer;
  Begin //AnalyzeVar
    With Inf Do
    Begin
      //===========================================
      // Copy-Types
      //===========================================
      While (Inf.TypeData is TRutisTypeCopy) Do
      Begin
        Inf.TypeData := TRutisTypeCopy(Inf.TypeData).CopyType;
      End;

      //===========================================
      // Analyze Arrays
      //===========================================
      If (Inf.TypeData is TRutisArrayType) and (IdentSym = sOpenBracketR) Then
      Begin
        GetSym;

        If HadPointer Then
        Begin
          If (Inf.Adr <> 0) Then
          Begin
            ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
            ScriptData.GenCodeV(_add, intInteger, 0, 0);
          End;
          ScriptData.GenCode(_lodp, -4, -1, 4);
        End
        Else
          ScriptData.GenCode(_lod, Inf.Adr, Inf.Lvl, 4);

        C_Expression(Owner.decl_Integer);

        ScriptData.GenCode(_gen4, TRutisArrayType(Inf.TypeData).ArrayType.Size, 0, 0);
        ScriptData.GenCodeV(_mult, intInteger, 0, 0);
        ScriptData.GenCodeV(_add, intInteger, 0, 0);
        If optArrayRangeCheck Then
          ScriptData.GenCodeV(_CPtr, 0, 0, 0);

        HadPointer   := True;
        Inf.Adr      := 0;
        Inf.Lvl      := -1;
        Inf.TypeData := TRutisArrayType(Inf.TypeData).ArrayType;
        Inf.InternalType := TypeData.InternalType;

        Expect([sCloseBracketR]);
        GetSym;
        Result := itPVar;
        AnalyzeVar;
        exit;
      End;

      //===========================================
      // Analyze Strings with Index
      //===========================================
      Inf.InternalType := TypeData.InternalType;
      If (Inf.InternalType = intAString) and (IdentSym = sOpenBracketR) Then
      Begin
        GetSym;

        //PChar(Pointer(PCardinal(PPointer(fLastAdress)^)^ + 0))^

        If Result = itConst Then
        Begin
          ScriptData.GenCode(_lodr, Inf.Adr, 1, 0);
          ScriptData.GenCode(_at, 0, 0, 0);
          ScriptData.GenCode(_lodp, -4, -1, 4);
          //ScriptData.GenCode(_lodp, -4, -1, 4);
        End
        Else
        Begin
          If HadPointer Then
          Begin
            If (Inf.Adr <> 0) Then
            Begin
              ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
              ScriptData.GenCodeV(_add, intInteger, 0, 0);
            End;
            ScriptData.GenCode(_lodp, -4, -1, 4);
          End
          Else
            ScriptData.GenCode(_lod, Inf.Adr, Inf.Lvl, 4);
          //ScriptData.GenCode(_lodp, -4, -1, 4);
        End;

        C_Expression(Owner.decl_Integer);

        ScriptData.GenCode(_gen4, 1, 0, 0);
        ScriptData.GenCodeV(_Sub, intInteger, 0, 0);
        ScriptData.GenCodeV(_add, intInteger, 0, 0);

        HadPointer := True;
        Inf.Adr    := 0;
        Inf.Lvl    := -1;
        TypeData   := Owner.decl_AChar;
        Inf.InternalType := TypeData.InternalType;

        Expect([sCloseBracketR]);
        GetSym;
        Result := itPVar;
        //AnalyzeVar;
        //exit;
      End;

      If (Inf.InternalType = intWString) and (IdentSym = sOpenBracketR) Then
      Begin
        GetSym;
        If Result = itConst Then
        Begin
          ScriptData.GenCode(_lodr, Inf.Adr, 1, 0);
          ScriptData.GenCode(_at, 0, 0, 0);
          ScriptData.GenCode(_lodp, -4, -1, 4);
          //ScriptData.GenCode(_lodp, -4, -1, 4);
        End
        Else
        Begin
          If HadPointer Then
          Begin
            If (Inf.Adr <> 0) Then
            Begin
              ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
              ScriptData.GenCodeV(_add, intInteger, 0, 0);
            End;
            ScriptData.GenCode(_lodp, -4, -1, 4);
          End
          Else
            ScriptData.GenCode(_lod, Inf.Adr, Inf.Lvl, 4);
          //ScriptData.GenCode(_lodp, -4, -1, 4);
        End;

        C_Expression(Owner.decl_Integer);

        ScriptData.GenCode(_gen4, 1, 0, 0);
        ScriptData.GenCodeV(_Sub, intInteger, 0, 0);
        ScriptData.GenCode(_gen4, 2, 0, 0);
        ScriptData.GenCodeV(_Mult, intInteger, 0, 0);
        ScriptData.GenCodeV(_add, intInteger, 0, 0);

        HadPointer := True;
        Inf.Adr    := 0;
        Inf.Lvl    := -1;
        TypeData   := Owner.decl_WChar;
        Inf.InternalType := TypeData.InternalType;

        Expect([sCloseBracketR]);
        GetSym;
        Result := itPVar;
        //AnalyzeVar;
        //exit;
      End;

      If IdentSym = sOpenBracketR Then
      Begin
        RaiseError(ERR_NEEDED_FOUND, ['Array or AnsiString', RutisVarTypeTOString[TypeData.InternalType]]); exit;
      End;

      //===========================================
      // Analyze Pointers
      //===========================================
      If (TypeData is TRutisPointerType) Then
        If IdentSym in [sHook, sDot] Then
        Begin
          If IdentSym = sHook Then GetSym;

          If HadPointer Then
          Begin
            If Inf.Adr <> 0 Then
            Begin
              ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
              ScriptData.GenCodeV(_add, intInteger, 0, 0);
            End;
            ScriptData.GenCode(_lodp, -4, Inf.Lvl, 4);
          End
          Else
            ScriptData.GenCode(_lod, Inf.Adr, Inf.Lvl, 4);

          Inf.Adr := 0;
          Inf.Lvl := -1;

          HadPointer := True;
          Result     := itPVar;

          TypeData := TRutisPointerType(TypeData).PointerType;
          AnalyzeVar;
          exit;
        End;
      If IdentSym = sHook Then
      Begin
        RaiseError(ERR_NEEDED_FOUND, ['Pointer', RutisVarTypeTOString[TypeData.InternalType]]); exit;
      End;

      //===========================================
      // Analyze Classes
      //===========================================
      If (TypeData is TRutisClassType) Then
        If IdentSym = sDot Then
        Begin
          If HadPointer Then
          Begin
            If Inf.Adr <> 0 Then
            Begin
              ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
              ScriptData.GenCodeV(_add, intInteger, 0, 0);
            End;
            ScriptData.GenCode(_lodp, -4, Inf.Lvl, 4);
          End
          Else
            ScriptData.GenCode(_lod, Inf.Adr, Inf.Lvl, 4);

          Inf.Adr := 0;
          Inf.Lvl := -1;

          HadPointer := True;
          Result     := itPVar;

          TypeData := TRutisClassType(TypeData).ClassStruct;

          PushScanner;
          GetSym;
          If (IdentSym = sIdent) and
            ((IdentStr = 'CLASSNAME') or
            (IdentStr = 'INSTANCESIZE') or
            (IdentStr = 'CLASSPARENT')) Then
          Begin
            TypeData := TRutisStructType(TypeData).StructTypes[0].VarType;
            ScriptData.GenCode(_lodp, -4, Inf.Lvl, 4);
            TypeData := TRutisPointerType(TypeData).PointerType;

            id       := TRutisStructType(TypeData).VarID(IdentStr);
            Inf.Adr  := Inf.Adr + TRutisStructType(TypeData).StructTypes[id].Address;
            TypeData := TRutisStructType(TypeData).StructTypes[id].VarType;

            If (IdentStr = 'CLASSNAME') Then
            Begin
              If Inf.Adr <> 0 Then
              Begin
                ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
                ScriptData.GenCodeV(_add, intInteger, 0, 0);
              End;
              ScriptData.GenCode(_lodp, -4, Inf.Lvl, 4);
              Inf.Adr  := 0;
              TypeData := TRutisPointerType(TypeData).PointerType;
            End;

            GetSym;
          End
          Else
            PopScanner;

          AnalyzeVar;
          exit;
        End;
      If IdentSym = sHook Then
      Begin
        RaiseError(ERR_NEEDED_FOUND, ['Pointer', RutisVarTypeTOString[TypeData.InternalType]]); exit;
      End;


      //===========================================
      // Analyze Record-Structures
      //===========================================
      If (TypeData is TRutisStructType) and (IdentSym = sDot) Then
      Begin
        GetSym;
        Expect([sIdent]);

        id := TRutisStructType(TypeData).VarID(IdentStr);
        If id = -1 Then
        Begin
          RaiseError(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]); exit;
        End;
        GetSym;

        Inf.Adr  := Inf.Adr + TRutisStructType(TypeData).StructTypes[id].Address;
        TypeData := TRutisStructType(TypeData).StructTypes[id].VarType;

        AnalyzeVar;
        exit;
      End;
      If IdentSym = sDot Then
      Begin
        RaiseError(ERR_NEEDED_FOUND, ['Record', RutisVarTypeTOString[TypeData.InternalType]]); exit;
      End;

      If HadPointer Then
      Begin
        If (Inf.Adr <> 0) Then
        Begin
          ScriptData.GenCode(_gen4, Inf.Adr, 0, 0);
          ScriptData.GenCodeV(_add, intInteger, 0, 0);
        End;
        Inf.Adr := -4;
        Inf.Lvl := -1;
        Result  := itPVar;
        exit;
      End;

      Inf.InternalType := TypeData.InternalType;
    End;
  End;

  Procedure CallVarMethod;
  Begin
    GenLoadSaveCode(Inf.IdentType, Inf.Adr, Inf.Lvl, Inf.TypeData.Size, True);

    Result        := itMethod;
    Inf.IdentType := Result;
    {If isFunction and IsExpression Then
    Begin
      Inf.Lvl := -1;
      Inf.Adr := 0;
      Result  := itFixVal;
//AnalyzeVar;
    End;}

    ScriptData.GenCode(_call, -1, 0, 0);
  End;

  Procedure CallExtMethod;
  Var
    TempType  : TRutisExtMethodType;
    i         : Integer;
  Begin
    TempType := TRutisExtMethodType(Inf.TypeData);
    With TempType Do
    Begin
      If IsExpression and not IsFunction Then
      Begin
        RaiseError(ERR_EXPECTED_FOUND, ['Function', 'Procedure']); exit;
      End;
      If IsFunction Then
      Begin
        If MethodResult.TypeData = nil Then
        Begin
          MethodResult.TypeData := TRutisTypeDecl(GetDeclaration(MethodResult.Name));
          If (MethodResult.TypeData = nil) Then
            RaiseError(ERR_UNKNOWN_TYPE, [MethodResult.Name]);
          If not (MethodResult.TypeData is TRutisTypeDecl) Then
            RaiseError(ERR_TYPE_EXPECTED);
          MethodResult.Size         := MethodResult.TypeData.Size;
          MethodResult.InternalType := MethodResult.TypeData.InternalType;
        End;
        Inf.TypeData := MethodResult.TypeData;
        ScriptData.GenCode(_gen, Inf.TypeData.Size, 0, 0);
      End;

      ParamsSize := 0;
      If length(Params) > 0 Then
      Begin
        Expect([sOpenBracket]);
        For i := 0 To high(Params) Do
        Begin
          If Params[i].TypeData = nil Then
          Begin
            Params[i].TypeData := TRutisTypeDecl(GetDeclaration(Params[i].Name));
            If (Params[i].TypeData = nil) Then
              RaiseError(ERR_UNKNOWN_TYPE, [Params[i].Name]);
            If not (Params[i].TypeData is TRutisTypeDecl) Then
              RaiseError(ERR_TYPE_EXPECTED);
          End;
          Params[i].Size := Params[i].TypeData.Size;
          Params[i].InternalType := Params[i].TypeData.InternalType;
          ParamsSize := ParamsSize + Params[i].TypeData.Size;

          If Params[i].isVarParam Then
            IdentSym := sAt
          Else
            GetSym;
          C_Expression(Params[i].TypeData);
          If (i < high(Params)) Then
          Begin
            If IdentSym = sCloseBracket Then
            Begin
              RaiseError(ERR_NOT_ENOUGH_PARAMETERS); exit;
            End;
            Expect([sComma]);
          End;
        End;
        If IdentSym = sComma Then
        Begin
          RaiseError(ERR_TOO_MANY_PARAMETERS);
          exit;
        End;
        Expect([sCloseBracket]);
        GetSym;
      End
      Else
      If IdentSym = sOpenBracket Then
      Begin
        GetSym;
        Expect([sCloseBracket]);
      End;

      If MethodTableID < 0 Then
      Begin
        SetLength(ScriptData.MethodTable, Length(ScriptData.MethodTable) + 1);
        ScriptData.MethodTable[high(ScriptData.MethodTable)] := TempType;
        MethodTableID := high(ScriptData.MethodTable);
      End;

      If not IsExpression and IsFunction Then
        ScriptData.GenCodeV(_ext, MethodTableID, 1, 0)
      Else
        ScriptData.GenCodeV(_ext, MethodTableID, 0, 0);

      Result := itMethod;
      If isFunction and IsExpression Then
      Begin
        Inf.Lvl := -1;
        Inf.Adr := 0;
        Result  := itFixVal;
        //AnalyzeVar;
      End;

      Inf.IdentType := Result;
      exit;
    End;
  End;

  Procedure CallRutisMethod;
  Var
    TempType  : TRutisMethodType;
    i         : Integer;
  Begin
    TempType := TRutisMethodType(Inf.TypeData);
    With TempType Do
    Begin
      If IsExpression and not IsFunction Then
      Begin
        RaiseError(ERR_EXPECTED_FOUND, ['Function', 'Procedure']); exit;
      End;
      If IsFunction Then
      Begin
        Inf.TypeData := MethodResult.TypeData;
        ScriptData.GenCode(_gen, Inf.TypeData.Size, 0, 0);
      End;

      If length(Params) > 0 Then
      Begin
        Expect([sOpenBracket]);
        For i := 0 To high(Params) Do
        Begin
          GetSym;
          C_Expression(Params[i].TypeData);
          If (i < high(Params)) Then
          Begin
            If IdentSym = sCloseBracket Then
            Begin
              RaiseError(ERR_NOT_ENOUGH_PARAMETERS); exit;
            End;
            Expect([sComma]);
          End;
        End;
        If IdentSym = sComma Then
        Begin
          RaiseError(ERR_TOO_MANY_PARAMETERS);
          exit;
        End;
        Expect([sCloseBracket]);
        GetSym;
      End
      Else
      If IdentSym = sOpenBracket Then
      Begin
        GetSym;
        Expect([sCloseBracket]);
      End;

      //Prozeduraufruf
//      ScriptData.GenCode(_lodr, Inf.Adr, 4, 0);
//      ScriptData.GenCode(_call, -1, Inf.Lvl, 0);
      ScriptData.GenCode(_call, ScriptData.Ressources.GetCardinal(Inf.Adr)^, Inf.Lvl, 0);

      If not IsExpression and IsFunction Then
        ScriptData.GenCode(_pupo, -Inf.TypeData.Size, 0, 0);

      Result := itMethod;
      If isFunction and IsExpression Then
      Begin
        Inf.Lvl := -1;
        Inf.Adr := 0;
        Result  := itFixVal;
        //AnalyzeVar;
      End;

      Inf.IdentType := Result;
      exit;
    End;
  End;

Var
  CastInf  : TAnalyzeIdentInfo;
  Decl     : TRutisDecl;
Begin
  Result        := itError;
  //==================================================================
  Inf.TypeData  := nil;
  Inf.VarDecl   := nil;
  Inf.Adr       := 0;
  Inf.Lvl       := 0;
  Inf.IdentType := Result;
  HadPointer    := False;
  HadAt         := False;

  If IdentSym = sAt Then
  Begin
    HadAt := True;
    GetSym;
  End;

  Decl := GetDeclaration(IdentStr);
  If Decl = nil Then
    RaiseError(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]);

  If (Decl is TRutisTypeDecl) Then
  Begin
    GetSym;
    Expect([sOpenBracket]);
    GetSym;

//    C_Expression(nil);
//    If not ExprResult.ConvertTo(TRutisTypeDecl(Decl), ScriptData) Then
//      If ExprResult.Size <> TRutisTypeDecl(Decl).Size Then
//        RaiseError(ERR_INCOMPATIBLE_TYPES, [TRutisTypeDecl(Decl).Name, ExprResult.Name]);
//    Inf.TypeData  := TRutisTypeDecl(Decl);
//    Inf.Adr       := -Inf.TypeData.Size;
//    Inf.Lvl       := 0;
//    Inf.IdentType := itFixVal;

    Expect([sIdent]);
    C_AnalyzeIdent(True, intNone, CastInf);
    If not CastInf.TypeData.ConvertTo(TRutisTypeDecl(Decl), ScriptData) Then
      If CastInf.TypeData.Size <> TRutisTypeDecl(Decl).Size Then
        RaiseError(ERR_INCOMPATIBLE_TYPES, [TRutisTypeDecl(Decl).Name, CastInf.TypeData.Name]);
    Inf := CastInf;
    Inf.TypeData := TRutisTypeDecl(Decl);


    Expect([sCloseBracket]);
  End
  Else
  Begin
    If not (Decl is TRutisVarDecl) Then
      RaiseError(ERR_VAR_EXPECTED, [IdentStr]);

    Inf.VarDecl  := TRutisVarDecl(Decl);
    Inf.TypeData := Inf.VarDecl.VarType;
    Inf.Adr      := Inf.VarDecl.Address;
    Inf.Lvl      := Inf.VarDecl.Level;
    Result       := itVar;
  End;

  // Get next symbol after Ident
  GetSym;

  //==================================================================
  //==================================================================
  If HadAt Then
  Begin
    AnalyzeVar;

    If (Inf.VarDecl.isConst) Then
    Begin
      If not (Inf.TypeData is TRutisMethodType) Then
      Begin
        RaiseError(ERR_VAR_EXPECTED); exit;
      End;
      If (TRutisMethodType(Inf.TypeData).IsFunction) or (TRutisMethodType(Inf.TypeData).ParamsSize > 0) Then
      Begin
        RaiseError(ERR_ONLY_PARAMLESS_PROCS); exit;
      End;

      Result        := itConst;
      ScriptData.GenCode(_lodr, Inf.Adr, 4, 0);
      Inf.TypeData  := Owner.decl_Method;
      Result        := itFixVal;
      Inf.IdentType := Result;
      Inf.InternalType := Inf.TypeData.InternalType;
      exit;
    End;
    If not IsExpression Then
    Begin
      RaiseError(ERR_UNALLOWED_STATEMENT); exit;
    End;

    Case Result Of
      itVar :
      Begin
      //If Inf.Adr < 0 Then
      //  ScriptData.GenCode(_ptr, Inf.Adr - 3, Inf.Lvl, 1)
      //Else
        ScriptData.GenCode(_ptr, Inf.Adr, Inf.Lvl, 1);
      End;
      itPVar  :;
      itConst :
      Begin
        If (Inf.TypeData is TRutisMethodType) Then
          ScriptData.GenCode(_lodr, Inf.Adr, 4, 0)
        Else
        Begin
          ScriptData.GenCode(_lodr, Inf.Adr, 1, 0);
          ScriptData.GenCode(_at, 0, 0, 0);
        End;
      End;
    Else
      RaiseError(ERR_POINTER_ONLY_FOR_VAR); exit;
    End;
    Inf.TypeData  := Owner.decl_Pointer;
    Result        := itFixVal;
    Inf.IdentType := Result;
    Inf.InternalType := Inf.TypeData.InternalType;

    exit;
  End;

  //==================================================================
  //==================================================================
  If (Inf.TypeData is TRutisMethodType) Then
  Begin
    If TRutisMethodType(Inf.TypeData).Overloaded Then
    Begin
      SetLength(ExcludeList, 0);
      While True Do
      Begin
        PushCompilerState;
        SetLength(ExcludeList, length(ExcludeList) + 1);
        ExcludeList[high(ExcludeList)] := Inf.VarDecl;
        Try
          If (Inf.TypeData is TRutisExtMethodType) or
            (Inf.TypeData is TRutisDllMethodType) Then
            CallExtMethod
          Else
            CallRutisMethod;
        Except
          Inf.VarDecl  := TRutisVarDecl(GetDeclaration(Inf.VarDecl.Name, ExcludeList));
          Inf.TypeData := Inf.VarDecl.VarType;
          If Inf.VarDecl = nil Then
            RaiseError(ERR_NO_OVERLOADED_FUNC);
          PopCompilerState;
          Continue;
        End;
        exit;
      End;
    End
    Else
    Begin
      If (Inf.TypeData is TRutisExtMethodType) or
        (Inf.TypeData is TRutisDllMethodType) Then
        CallExtMethod
      Else
        CallRutisMethod;
      exit;
    End;
  End;
  //==================================================================
  //==================================================================

  //If IdentPos >= 0 Then
  Begin
    Result := itVar;
    If inf.VarDecl.isConst and not IsExpression Then
    Begin
      RaiseError(ERR_VAR_EXPECTED); exit;
    End;
    If inf.VarDecl.isConst Then
    Begin
      Result := itConst;
      If inf.VarDecl.VarType.InternalType = intAString Then
      Begin
        ScriptData.GenCode(_rstr, Inf.Adr, 0, 0);
        Result := itFixVal;
      End;
    End;

    AnalyzeVar;

    Inf.IdentType    := Result;
    Inf.InternalType := Inf.TypeData.InternalType;

    If (IdentSym <> sBecomes) and (Inf.InternalType = intMethod) Then
      CallVarMethod;

    exit;
  End;
End;

procedure TRutisCompilerDelphi.C_Expression(WantType : TRutisTypeDecl);
Var
  OldWantType  : TRutisTypeDecl;
  gotStr       : Boolean;
  //  LastIntTypes : Array of TRutisIntType;

  Procedure Term;
  Var
    FactorType  : TRutisTypeDecl;

    Procedure Factor;
    Var
      Inf         : TAnalyzeIdentInfo;
      bool        : Boolean;
      //PAnsiStr  : PAnsiString;
      ConvPos, i  : Integer;
      Decl        : TRutisDecl;
    Begin (*Factor*)
      FactorType := nil;
      ConvPos    := high(ScriptData.Code);
      Case IdentSym Of
        sIdent, sAt :
        Begin
          C_AnalyzeIdent(True, WantType.InternalType, Inf);
          FactorType := Inf.TypeData;

          If FactorType = nil Then
            RaiseError(ERR_VAR_EXPECTED);

          GenLoadSaveCode(Inf.IdentType, Inf.Adr, Inf.Lvl, FactorType.Size, True);
        End;
        sSizeOf :
        Begin
          GetSym;
          Expect([sOpenBracket]);
          GetSym;

          Decl := GetDeclaration(IdentStr);
          If Decl is TRutisTypeDecl Then
          Begin
            ScriptData.GenCode(_Gen4, TRutisTypeDecl(Decl).Size, 0, 0);
            GetSym;
          End
          Else
          Begin
            i := length(ScriptData.Code);
            C_AnalyzeIdent(False, intNone, Inf);
            SetLength(ScriptData.Code, i);
            ScriptData.GenCode(_Gen4, Inf.TypeData.Size, 0, 0);
          End;

          FactorType := Owner.decl_Integer;

          Expect([sCloseBracket]);
          GetSym;
        End;
        sLength,
        sHigh  : With Inf Do
          Begin
            bool := IdentSym = sHigh;

            FactorType := Owner.decl_Integer;

            GetSym;
            Expect([sOpenBracket]);
            GetSym;

            If IdentSym = sString Then
            Begin
              ScriptData.GenCode(_gen4, length(IdentStr), 0, 0);
              If bool Then
              Begin
                ScriptData.GenCode(_gen4, 1, 0, 0);
                ScriptData.GenCodeV(_sub, intInteger, 0, 0);
              End;
              GetSym;
              Expect([sCloseBracket]);
              GetSym;
            End
            Else
            Begin
              Expect([sIdent]);

              //AllowedIdents := [vString, vArray];
              C_AnalyzeIdent(True, intNone, Inf);
              Case Inf.IdentType Of
                itVar :
                Begin
                  If Inf.TypeData is TRutisArrayType Then
                  Begin
                    ScriptData.GenCode(_lod, Adr, Lvl, 4);
                    ScriptData.GenCode(_mems, 0, 0, 1);
                    //If (TRutisArrayType(Inf.TypeData).ArrayType.Size > 1) Then
                    //Begin
                    //  ScriptData.GenCode(_gen4, TRutisArrayType(Inf.TypeData).ArrayType.Size, 0, 0);
                    //  ScriptData.GenCodeV(_div, intInteger, 0, 0);
                    //End;
                  End
                  Else
                  Begin
                    case Inf.TypeData.InternalType of
                      intAString:
                      begin
                        ScriptData.GenCode(_lod, Adr, Lvl, 4);
                        ScriptData.GenCode(_GASL, 0, 0, 0);
                      end;
                      intWString:
                      begin
                        ScriptData.GenCode(_lod, Adr, Lvl, 4);
                        ScriptData.GenCode(_GWSL, 0, 0, 0);
                      end;
                    else
                      RaiseError(ERR_EXPECTED, ['AnsiString, WideString, Array']);
                    end;
                  End;
                End;
                itPVar :
                Begin
                  If Inf.TypeData is TRutisArrayType Then
                  Begin
                    ScriptData.GenCode(_lodP, Adr, Lvl, 4);
                    ScriptData.GenCode(_mems, 0, 0, 1);
                    //If (TRutisArrayType(Inf.TypeData).ArrayType.Size > 1) Then
                    //Begin
                    //  ScriptData.GenCode(_gen4, TRutisArrayType(Inf.TypeData).ArrayType.Size, 0, 0);
                    //  ScriptData.GenCodeV(_div, intInteger, 0, 0);
                    //End;
                  End
                  Else
                  Begin
                    case Inf.TypeData.InternalType of
                      intAString:
                      begin
                        ScriptData.GenCode(_lodP, Adr, Lvl, 4);
                        ScriptData.GenCode(_GASL, 0, 0, 0);
                      end;
                      intWString:
                      begin
                        ScriptData.GenCode(_lodP, Adr, Lvl, 4);
                        ScriptData.GenCode(_GWSL, 0, 0, 0);
                      end;
                    else
                      RaiseError(ERR_EXPECTED, ['AnsiString, WideString, Array']);
                    end;
                  End;
                End;
                itConst :
                Begin
               { If VarInf^.VTyp = vString Then
                Begin
                  ScriptData.GenCode(_lodr, Adr, 1, 0);
                  ScriptData.GenCode(_GASL, 0, 0, 0);
                End
                Else    }
                  RaiseError(ERR_EXPECTED, ['AnsiString, WideString, Array']);
                End;
                itMethod  : RaiseError(ERR_VAR_EXPECTED);
                itError  : RaiseError(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]);
              End;

              If bool Then
              Begin
                ScriptData.GenCode(_gen4, 1, 0, 0);
                ScriptData.GenCodeV(_sub, intInteger, 0, 0);
              End;

              Expect([sCloseBracket]);
              GetSym;
            End;
          End;
        sTrue, sFalse :
        Begin
          ScriptData.GenCode(_gen1, Integer(Boolean(IdentSym = sTrue)), 0, 0);
          FactorType := Owner.decl_Boolean;
          GetSym;
        End;
        sopAnd :
        Begin
          GetSym;
          Factor;
          If WantType = nil Then
            RaiseError('ERROR - C_Expression - Term - Factor');
          ScriptData.GenCodeV(_And, WantType.Size, 0, 0);
        End;
        sopOr :
        Begin
          GetSym;
          Factor;
          If WantType = nil Then
            RaiseError('ERROR - C_Expression - Term - Factor');
          ScriptData.GenCodeV(_Or, WantType.Size, 0, 0);
        End;
        sopXOr :
        Begin
          GetSym;
          Factor;
          If WantType = nil Then
            RaiseError('ERROR - C_Expression - Term - Factor');
          ScriptData.GenCodeV(_XOr, WantType.Size, 0, 0);
        End;
        sopNot :
        Begin
          GetSym;
          Factor;
          If WantType = nil Then
            RaiseError('ERROR - C_Expression - Term - Factor');
          If WantType.InternalType = intBoolean Then
            ScriptData.GenCodeV(_Not, WantType.Size, 1, 0)
          Else
            ScriptData.GenCodeV(_Not, WantType.Size, 0, 0);
        End;
        sOpenBracket :
        Begin
          GetSym;
          C_Expression(WantType);
          FactorType := ExprResult;

          Expect([sCloseBracket]);
          GetSym;
        End;
        sOpenBracketR :
        Begin
          If WantType = nil then WantType := Owner.decl_Set4;
          //If WantType.InternalType = intAString Then
          //  RaiseError(ERR_UNALLOWED_STRING_ACTION);
          If not (WantType.InternalType in Internalintegers + [intSet]) Then
            RaiseError(ERR_UNALLOWED_STRING_ACTION);

          GetSym;
          ScriptData.GenCode(_gen, WantType.Size, 0, 0);
          If IdentSym <> sCloseBracketR Then
            While True Do
            Begin
              C_Expression(Owner.decl_Enum);
              ScriptData.GenCodeV(_ets, WantType.Size, 0, 0);
              ScriptData.GenCodeV(_add, intSet, WantType.Size, 0);

              If IdentSym = sCloseBracketR Then break;
              Expect([sComma]);
              GetSym;
            End;
          FactorType := Owner.decl_Set4;

          Expect([sCloseBracketR]);
          GetSym;
        End;
        sInteger :
        Begin
        //If WantType.InternalType = intAString Then
        //  RaiseError(ERR_UNALLOWED_STRING_ACTION);
          If WantType.InternalType = intSingle then
          begin
            FactorType := Owner.decl_Single;
            ScriptData.GenCodeV(_gen4, Single(IdentNum), 0, 0);
          end
          else
          begin
            FactorType := Owner.decl_Integer;
            ScriptData.GenCode(_gen4, IdentNum, 0, 0);
          end;
          GetSym;
        End;
        sFloat :
        Begin
          FactorType := Owner.decl_Single;
          ScriptData.GenCodeV(_gen4, Single(IdentNum), 0, 0);
          GetSym;
        End;
        sNil :
        Begin
          FactorType := Owner.decl_Pointer;
          ScriptData.GenCode(_gen4, 0, 0, 0);
          GetSym;
        End;
        sString :
        Begin
          If (length(IdentStr) = 1) and (WantType.InternalType in [intAChar, intNone]) Then
          Begin
            ScriptData.GenCode(_Gen1, Ord(IdentStr[1]), 0, 0);
            FactorType := Owner.decl_AChar;
          End
          Else
          Begin
            FactorType := Owner.decl_AString;
            Bool       := False;
            For I := 0 To high(ResStrings) Do
              If ResStrings[i].Value = IdentStr Then
              Begin
                ScriptData.GenCode(_rstr, ResStrings[i].Address, 0, 0);
                Bool := True;
                break;
              End;   
            If not Bool Then
            Begin
              SetLength(ResStrings, length(ResStrings) + 1);
              ResStrings[high(ResStrings)].Address := ScriptData.Ressources.Top + 4;
              ResStrings[high(ResStrings)].Value   := IdentStr;
              ScriptData.GenCode(_rstr, ScriptData.Ressources.Top + 4, 0, 0);
              ScriptData.Ressources.PushAStringData(IdentStr);
            End;
          End;
          GetSym;
        End;
      Else
        ErrorExpected([sInteger, sFloat, sString, sIdent, sOpenBracket], IdentSym);
      End;

      If (WantType = nil) Then
        WantType := FactorType
      Else
      If (FactorType <> nil) and (WantType <> FactorType) Then
      Begin
        If (OldWantType.InternalType = intNone) and
          (WantType.InternalType in InternalIntegers) and
          (FactorType.InternalType in InternalFloats) Then
        Begin
          ScriptData.CodeInsertPos := ConvPos;
          WantType.ConvertTo(FactorType, ScriptData);
          ScriptData.CodeInsertPos := -1;
          WantType := FactorType;
        End
        Else
        If not FactorType.ConvertTo(WantType, ScriptData) Then
          RaiseError(ERR_INCOMPATIBLE_TYPES,
            [WantType.Name, FactorType.Name]);
      End;
    End;

       (*Factor*)
  Var
    Operation  : TRutisSymbol;
  Begin(*Term*)
    Factor;
    If not gotStr then
    begin
      If (WantType.InternalType = intAString) Then
        ScriptData.GenCode(_copyastr, 0, 0, 0);
      If (WantType.InternalType = intWString) Then
        ScriptData.GenCode(_copywstr, 0, 0, 0);
      gotStr := True;
    end;

    While IdentSym in [sStar, sSlash, sMod, sopAnd, sopOr, sopXOr] Do
    Begin
      Operation := IdentSym;
      GetSym;
      Factor;
      If (WantType.InternalType in [intAString, intWString]) {and (Operation in [sStar, sSlash])} Then
        RaiseError(ERR_UNALLOWED_STRING_ACTION);

      Case Operation Of
        sStar  : ScriptData.GenCodeV(_mult, WantType.InternalType, 0, 0);
        sSlash  : ScriptData.GenCodeV(_div, WantType.InternalType, 0, 0);
        sMod :
        Begin
          If not (WantType.InternalType in InternalIntegers) Then
            RaiseError(ERR_NEEDED_FOUND, ['ordinal type', WantType.Name]);
          ScriptData.GenCodeV(_mod, WantType.InternalType, 0, 0);
        End;
        sopAnd  : ScriptData.GenCodeV(_And, WantType.Size, 0, 0);
        sopOr  : ScriptData.GenCodeV(_Or, WantType.Size, 0, 0);
        sopXOr  : ScriptData.GenCodeV(_XOr, WantType.Size, 0, 0);
      End;
    End;
  End;

      (*Term*)
      //==============================================================================
Var
  Operation    : TRutisSymbol;
  TempCodepos  : Integer;
Begin (*Expresion*)
  gotStr  := False;
  OldWantType := WantType;
  If WantType <> nil Then
    If WantType.InternalType = intBoolean Then
      WantType := nil;

  If (IdentSym in [sPlus, sMinus]) Then
  Begin
    Operation := IdentSym;

    TempCodepos := 0;
    If Operation = sMinus Then
      TempCodepos := ScriptData.GenCode(_gen, -1, 0, 0);

    GetSym;
    Term;

    If Operation = sMinus Then
    Begin
      ScriptData.Code[TempCodepos].P1 := WantType.Size;
      ScriptData.GenCodeV(_sub, WantType.InternalType, 0, 0);
    End;
  End
  Else
    Term;
  While (IdentSym in [sPlus, sMinus]) Do
  Begin
    Operation := IdentSym;
    GetSym;
    Term;
    Case Operation Of
      sPlus  : ScriptData.GenCodeV(_add, WantType.InternalType, WantType.Size, 0);
      sMinus  : ScriptData.GenCodeV(_sub, WantType.InternalType, WantType.Size, 0);
    End;
  End;
  If (IdentSym in [sEqual..sUnEqual]) Then
  Begin
    If not (OldWantType.InternalType in [intNone, intBoolean]) Then
      RaiseError(
        ERR_INCOMPATIBLE_TYPES,
        [OldWantType.Name, Owner.decl_Boolean.Name]);

    Operation := IdentSym;
    GetSym;
    //C_Expression(nil);
    C_Expression(WantType);

    If (ExprResult.InternalType in [intNone, intRecord, intShortString]) Then
      RaiseError('Value expected');
    Case Operation Of
      sEqual  : ScriptData.GenCodeV(_opr, ocEqual, WantType.InternalType, ExprResult.InternalType);
      sBigger  : ScriptData.GenCodeV(_opr, ocGreater, WantType.InternalType, ExprResult.InternalType);
      sSmaller  : ScriptData.GenCodeV(_opr, ocLess, WantType.InternalType, ExprResult.InternalType);
      sBiggerEqual  : ScriptData.GenCodeV(_opr, ocGEqual, WantType.InternalType, ExprResult.InternalType);
      sSmallerEqual  : ScriptData.GenCodeV(_opr, ocLEqual, WantType.InternalType, ExprResult.InternalType);
      sUnEqual  : ScriptData.GenCodeV(_opr, ocUnequal, WantType.InternalType, ExprResult.InternalType);
    End;
    WantType := Owner.decl_Boolean;
  End;
  ExprResult := WantType;
End; (*Expression*)

Procedure TRutisCompilerDelphi.C_StatementSequence;

  Procedure Statement;
  Var
    Inf  : TAnalyzeIdentInfo;
    //================================
    (*Condition*)
    Procedure IfStruct;
    Var
      FailJumpPos, fCodePosition2  : Integer;
    Begin
      GetSym;

      C_Expression(Owner.decl_Boolean);

      Expect([sThen]);
      GetSym;

      ScriptData.GenCodeV(_JZ, 0, intBoolean, 0);
      FailJumpPos := high(ScriptData.Code);

      Statement;

      If IdentSym <> sSemicolon Then
      Begin
        ScriptData.GenCode(_jmp, 0, 0, 0);
        fCodePosition2 := high(ScriptData.Code);

        ScriptData.Code[FailJumpPos].P1 := high(ScriptData.Code) + 1;

        While IdentSym = sElseIf Do
        Begin
          GetSym;
          C_Expression(Owner.decl_Boolean);
          Expect([sThen]);
          GetSym;

          ScriptData.GenCodeV(_JZ, 0, intBoolean, 0);
          FailJumpPos := high(ScriptData.Code);

          Statement;

          ScriptData.GenCode(_jmp, 0, 0, 0);
          ScriptData.Code[fCodePosition2].P1 := high(ScriptData.Code);
          fCodePosition2 := high(ScriptData.Code);

          ScriptData.Code[FailJumpPos].P1 := high(ScriptData.Code) + 1;
        End;
        If IdentSym = sElse Then
        Begin
          GetSym;
          Statement;
        End
        Else
        If IdentSym <> sElseIf Then
          RaiseError('If-Semicolon');

        ScriptData.Code[fCodePosition2].P1 := high(ScriptData.Code) + 1;
      End
      Else
        ScriptData.Code[FailJumpPos].P1    := high(ScriptData.Code) + 1;
    End;
    //================================
    Procedure WhileStruct;
    Var
      StartPos,
      EndPos, i  : Integer;
    Begin
      GetSym;
      StartPos := high(ScriptData.Code) + 1;

      //==== Check Loop-Condition
      C_Expression(Owner.decl_Boolean);

      Expect([sDo]);
      GetSym;

      ScriptData.GenCodeV(_JZ, 0, intBoolean, 0);
      EndPos := high(ScriptData.Code);

      //==== Loop-Code
      Inc(LoopDepth);
      Statement;
      Dec(LoopDepth);

      //==== Jump to Loop-Start
      ScriptData.GenCode(_jmp, StartPos, 0, 0);
      ScriptData.Code[EndPos].P1 := high(ScriptData.Code) + 1;

      //==== Continue / Break Jumps
      For i := EndPos + 1 To high(ScriptData.Code) Do
      Begin
        If (ScriptData.Code[i].Cmd = _jmp) and
          (ScriptData.Code[i].P1 = -10) Then
          ScriptData.Code[i].P1 := StartPos;
        If (ScriptData.Code[i].Cmd = _jmp) and
          (ScriptData.Code[i].P1 = -20) Then
          ScriptData.Code[i].P1 := high(ScriptData.Code) + 1;
      End;
    End;

    Procedure RepeatStruct;
    Var
      StartPos, i  : Integer;
    Begin
      GetSym;
      StartPos := high(ScriptData.Code) + 1;

      //==== Loop-Code
      Inc(LoopDepth);
      C_StatementSequence;
      Dec(LoopDepth);

      Expect([sUntil]);
      GetSym;

      //==== Check Loop-Condition
      C_Expression(Owner.decl_Boolean);

      ScriptData.GenCodeV(_JZ, StartPos, intBoolean, 0);

      //==== Continue / Break Jumps
      For i := StartPos To high(ScriptData.Code) Do
      Begin
        If (ScriptData.Code[i].Cmd = _jmp) and
          (ScriptData.Code[i].P1 = -10) Then
          ScriptData.Code[i].P1 := StartPos;
        If (ScriptData.Code[i].Cmd = _jmp) and
          (ScriptData.Code[i].P1 = -20) Then
          ScriptData.Code[i].P1 := high(ScriptData.Code) + 1;
      End;
    End;

    Procedure ForStruct;
    Var
      i,
      StartPos    : Integer;
      downtoloop  : Boolean;
      IdentVar    : TRutisVarDecl;
    Begin
      GetSym;

      Expect([sIdent]);

      IdentVar := TRutisVarDecl(GetDeclaration(IdentStr));

      If (IdentVar = nil) Then
      Begin
        RaiseError(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]);
        exit;
      End;
      If not (IdentVar is TRutisVarDecl) Then
      Begin
        RaiseError(ERR_VAR_EXPECTED);
        exit;
      End;
      If not (IdentVar.VarType is TRutisVarType) Then
      Begin
        RaiseError(ERR_VAR_EXPECTED);
        exit;
      End;
      If not (IdentVar.VarType.InternalType in InternalIntegers) Then
      Begin
        RaiseError(ERR_EXPECTED, ['Integer']);
        exit;
      End;
      GetSym;

      Expect([sBecomes]);
      GetSym;

      //==== Loop-Start Value
      C_Expression(IdentVar.VarType);
      With IdentVar Do
        ScriptData.GenCode(_sto, Address, Level, IdentVar.VarType.Size);

      downtoloop := IdentSym = sDownTo;
      If not downtoloop and not Expect([sTo]) Then exit;
      GetSym;

      //==== Loop-End Value
      C_Expression(IdentVar.VarType);
      case IntTypeSizes[IdentVar.VarType.InternalType] of
        1 : ScriptData.GenCode(_gen1, 1, 0, 0);
        2 : ScriptData.GenCode(_gen2, 1, 0, 0);
        4 : ScriptData.GenCode(_gen4, 1, 0, 0);
      else
        RaiseError(ERR_UNEXPECTED_ERROR);
      end;
      ScriptData.GenCodeV(_add, IdentVar.VarType.InternalType, 0, 0);

      Expect([sDo]);
      GetSym;

      StartPos := high(ScriptData.Code) + 1;

      //==== Check Loop-End
      With IdentVar Do
      Begin
        ScriptData.GenCode(_lod, -VarType.Size, 0, VarType.Size);
        ScriptData.GenCode(_lod, Address, Level, VarType.Size);
        ScriptData.GenCodeV(_sub, VarType.InternalType, 0, 0);
        If downtoloop Then
          ScriptData.GenCodeV(_JGZ, 0, VarType.InternalType, 0)
        Else
          ScriptData.GenCodeV(_JLZ, 0, VarType.InternalType, 0);
      End;

      //==== Loop-Code
      Inc(LoopDepth);
      Statement;
      Dec(LoopDepth);

      //==== i := i + 1  // i := i - 1
      If downtoloop Then
        ScriptData.GenCode(_gen4, -1, 0, 0)
      Else
        ScriptData.GenCode(_gen4, 1, 0, 0);
      With IdentVar Do
        ScriptData.GenCode(_inc, Address, Level, Word(VarType.InternalType));  //}

      //==== Jump to Loop-Start
      ScriptData.GenCode(_jmp, StartPos, 0, 0);
      ScriptData.Code[StartPos + 3].P1 := high(ScriptData.Code) + 1;

      //==== Continue / Break Jumps
      For i := StartPos To high(ScriptData.Code) Do
      Begin
        If (ScriptData.Code[i].Cmd = _jmp) and
          (ScriptData.Code[i].P1 = -10) Then
          ScriptData.Code[i].P1 := high(ScriptData.Code) - 2;

        If (ScriptData.Code[i].Cmd = _jmp) and
          (ScriptData.Code[i].P1 = -20) Then
          ScriptData.Code[i].P1 := high(ScriptData.Code) + 1;
      End;

      ScriptData.GenCode(_pupo, -IdentVar.VarType.Size, 0, 0);
    End;
    //================================
    Procedure IdentFoundHandler;
    Var
      InsertPos  : Integer;
    Begin
      If IdentStr = '' Then exit;

      With Inf Do
      Begin
        InsertPos := high(ScriptData.Code) + 1;

        Case C_AnalyzeIdent(False, intNone, Inf) Of
          itVar :
          Begin
            Expect([sBecomes]);
            GetSym;

            case Inf.TypeData.InternalType of
              intAString: ScriptData.GenCode(_StoAStr, Adr, Lvl, 0);
              intWString: ScriptData.GenCode(_StoWStr, Adr, Lvl, 0);
            Else
              ScriptData.GenCode(_sto, Adr, Lvl, Inf.TypeData.Size);
            end;

            // Insert expression code before store-statements
            ScriptData.CodeInsertPos := InsertPos;
            C_Expression(Inf.TypeData);
          End;
          itPVar :
          Begin
            Expect([sBecomes]);
            GetSym;

            If Lvl = -1 Then Adr := -4;

            case Inf.TypeData.InternalType of
              intAString: ScriptData.GenCode(_StoAStr, Adr, Lvl, 1);
              intWString: ScriptData.GenCode(_StoWStr, Adr, Lvl, 1);
            Else
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
            end;

            // Insert expression code before store-statements
            ScriptData.CodeInsertPos := InsertPos;
            C_Expression(Inf.TypeData);
          End;
          itConst  : RaiseError(ERR_NO_CONST_ALLOWED);
          itError  : RaiseError(ERR_UNKNOWN_IDENT, [LowerCase(IdentStr)]);
        End;
        // Reset Insert-Pos
        ScriptData.CodeInsertPos := -1;
      End;
    End;
    //================================
    Procedure Code_SetLength;
    Begin
      With Inf Do
      Begin
        GetSym;
        Expect([sOpenBracket]);
        GetSym;

        //AllowedIdents := [VArray, VString];
        C_AnalyzeIdent(False, intNone, Inf);
        If not (Inf.TypeData.InternalType in [intArray, intAString]) Then
        Begin
          RaiseError(ERR_EXPECTED, ['Array, AnsiString']);
          exit;
        End;

        Case Inf.IdentType Of
          itVar  : ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
              ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
            ScriptData.GenCode(_lodp, Adr, Lvl, Inf.TypeData.Size);
          End;
        Else
          RaiseError('ERROR - SetLength');
          exit;
        End;

        Expect([sComma]);
        GetSym;
        C_Expression(Owner.decl_Integer);
        If not (ExprResult.InternalType in InternalIntegers) Then
          RaiseError(ERR_EXPECTED_FOUND, ['Integer', ExprResult.Name]);

        Expect([sCloseBracket]);
        GetSym;

        If TypeData is TRutisArrayType Then
        Begin
          ScriptData.GenCode(_gen4, TRutisArrayType(TypeData).ArrayType.Size, 0, 0);
          ScriptData.GenCode(_smem, 0, 0, 0);
        End
        Else
        Begin
          If Inf.TypeData.InternalType = intAString Then
          Begin
            ScriptData.GenCode(_SASL, 0, 0, 0);
          End
          Else
          Begin
            RaiseError('ERROR'); exit;
          End;
        End;

        Case IdentType Of
          itVar  : ScriptData.GenCode(_sto, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
            Begin
              ScriptData.GenCode(_lod, -8, -1, 4);
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
              ScriptData.GenCode(_pupo, -4, 0, 0);
            End
            Else
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
          End;
        End;
        //If MethodResult.TypeData = VArray then
        //begin

        //end;
      End;
    End;

    Procedure Code_ReallocMem;
    Begin
      With Inf Do
      Begin
        GetSym;
        Expect([sOpenBracket]);
        GetSym;

        C_AnalyzeIdent(False, intPointer, Inf);

        If not (Inf.TypeData.InternalType in [intPointer, intArray]) Then
        Begin
          RaiseError(ERR_EXPECTED, ['Pointer']);
          exit;
        End;

        Case Inf.IdentType Of
          itVar  : ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
              ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
            ScriptData.GenCode(_lodp, Adr, Lvl, Inf.TypeData.Size);
          End;
        Else
          RaiseError('ERROR - ReallocMem');
          exit;
        End;

        Expect([sComma]);
        GetSym;
        C_Expression(Owner.decl_Integer);
        If not (ExprResult.InternalType in InternalIntegers) Then
          RaiseError(ERR_EXPECTED_FOUND, ['Integer', ExprResult.Name]);

        Expect([sCloseBracket]);
        GetSym;

        ScriptData.GenCode(_gen4, 1, 0, 0);
        ScriptData.GenCode(_smem, 0, 0, 0);

        Case IdentType Of
          itVar  : ScriptData.GenCode(_sto, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
            Begin
              ScriptData.GenCode(_lod, -8, -1, 4);
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
              ScriptData.GenCode(_pupo, -4, 0, 0);
            End
            Else
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
          End;
        End;
      End;
    End;

    Procedure Code_New;
    Begin
      With Inf Do
      Begin
        GetSym;
        Expect([sOpenBracket]);
        GetSym;

        C_AnalyzeIdent(False, intPointer, Inf);

        If not (Inf.TypeData is TRutisPointerType) Then
        Begin
          RaiseError(ERR_EXPECTED, ['typed Pointer']);
          exit;
        End;

        Case Inf.IdentType Of
          itVar  : ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
              ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
            ScriptData.GenCode(_lodp, Adr, Lvl, Inf.TypeData.Size);
          End;
        Else
          RaiseError('ERROR - New');
          exit;
        End;
        Expect([sCloseBracket]);
        GetSym;

        ScriptData.GenCode(_gen4, TRutisPointerType(Inf.TypeData).PointerType.Size, 0, 0);
        ScriptData.GenCode(_gen4, 1, 0, 0);
        ScriptData.GenCode(_smem, 0, 0, 0);

        Case IdentType Of
          itVar  : ScriptData.GenCode(_sto, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
            Begin
              ScriptData.GenCode(_lod, -8, -1, 4);
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
              ScriptData.GenCode(_pupo, -4, 0, 0);
            End
            Else
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
          End;
        End;
      End;
    End;

    Procedure Code_Dispose;
    Begin
      With Inf Do
      Begin
        GetSym;
        Expect([sOpenBracket]);
        GetSym;

        C_AnalyzeIdent(False, intPointer, Inf);

        If not (Inf.TypeData.InternalType in [intPointer, intArray]) Then
        Begin
          RaiseError(ERR_EXPECTED, ['Pointer']);
          exit;
        End;

        Case Inf.IdentType Of
          itVar  : ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
              ScriptData.GenCode(_lod, Adr, Lvl, Inf.TypeData.Size);
            ScriptData.GenCode(_lodp, Adr, Lvl, Inf.TypeData.Size);
          End;
        Else
          RaiseError('ERROR - New');
          exit;
        End;
        Expect([sCloseBracket]);
        GetSym;

        ScriptData.GenCode(_gen4, 0, 0, 0);
        ScriptData.GenCode(_gen4, 0, 0, 0);
        ScriptData.GenCode(_smem, 0, 0, 0);

        Case IdentType Of
          itVar  : ScriptData.GenCode(_sto, Adr, Lvl, Inf.TypeData.Size);
          itPVar :
          Begin
            If Lvl = -1 Then
            Begin
              ScriptData.GenCode(_lod, -8, -1, 4);
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
              ScriptData.GenCode(_pupo, -4, 0, 0);
            End
            Else
              ScriptData.GenCode(_stop, Adr, Lvl, Inf.TypeData.Size);
          End;
        End;
      End;
    End;
    //================================
  Var
    i    : Integer;
    sym  : TRutisSymbol;
  Begin (*Statement*)
    If CompilingUnit Then
      ScriptData.CompilerLine := -1
    Else
      ScriptData.CompilerLine := ScannerLine;

    sym := IdentSym;
    Case sym Of
      sIdent  : IdentFoundHandler;
      sIf  : IfStruct;
      sWhile  : WhileStruct;
      sRepeat  : RepeatStruct;
      sFor  : ForStruct;
      sWrite :
      Begin
        GetSym;
        C_Expression(nil);
        ScriptData.GenCodeV(_wri, ExprResult.InternalType, 0, 0);
      End;
      sNop :
      Begin
        ScriptData.GenCode(_nocmd, 0, 0, 0);
        GetSym;
      End;
      sTerminate :
      Begin
        ScriptData.GenCode(_jmp, -5, 0, 0);
        GetSym;
      End;
      sInc,
      sDec :
      Begin
        GetSym;
        Expect([sOpenBracket]);
        GetSym;

        If sym = sInc Then
          ScriptData.GenCode(_Gen4, 1, 0, 0)
        Else
          ScriptData.GenCode(_Gen4, -1, 0, 0);
        i := high(ScriptData.Code);

        C_AnalyzeIdent(False, intNone, Inf);

        If IdentSym = sComma Then
        Begin
          GetSym;
          If i < high(ScriptData.Code) Then
          Begin
            RaiseError('ERROR - Statement'); exit;
          End;
          SetLength(ScriptData.Code, length(ScriptData.Code) - 1);
          If sym = sDec Then
            ScriptData.GenCode(_Gen4, 0, 0, 0);
          C_Expression(Owner.decl_Integer);
          If sym = sDec Then
            ScriptData.GenCodeV(_Sub, intInteger, 0, 0);
        End;

        With Inf Do
          Case Inf.IdentType Of
            itVar  : ScriptData.GenCodeV(_inc, Adr, Lvl, Inf.TypeData.InternalType);
            //itPVar  : ScriptData.GenCode(_incp, Adr, Lvl, Inf.TypeData.Size);
          Else
            RaiseError('ERROR - Value expected'); exit;
            exit;
          End;

        Expect([sCloseBracket]);
        GetSym;
      End;
      sSetLength  : Code_SetLength;
      sReallocMem  : Code_ReallocMem;
      sNew  : Code_New;
      sDispose  : Code_Dispose;
      sContinue :
      Begin
        If LoopDepth <= 0 Then
        Begin
          RaiseError('Continue and Break can only be used in loops');
          exit;
        End;
        ScriptData.GenCode(_jmp, -10, 0, 0);
        GetSym;
      End;
      sBreak :
      Begin
        If LoopDepth <= 0 Then
        Begin
          RaiseError('Continue and Break can only be used in loops');
          exit;
        End;
        ScriptData.GenCode(_jmp, -20, 0, 0);
        GetSym;
      End;
      sExit :
      Begin
        ScriptData.GenCode(_ret, ExitParamsSize, 0, 0);
        GetSym;
      End;
      sBegin :
      Begin
        GetSym;
        C_StatementSequence;

        If IdentSym <> sEnd Then
        Begin
          RaiseError(ERR_OP_OR_SEMI_EXPECTED);
          exit;
        End;
        //Expect([sEnd]);
        GetSym;
      End;
    Else
        (*Der Fehler darf getrost ignoriert werden*)
        //Error(ERR_UNALLOWED_STATEMENT)
    End;
  End; (*Statement*)

Begin (*Statement Sequence*)
  Statement;
  While IdentSym = sSemiColon Do
  Begin
    GetSym;
    Statement;
  End;
End; (*Statement Sequence*)

//==============================================================================
//==============================================================================

Function TRutisCompilerDelphi.GetDeclaration(Name : AnsiString) : TRutisDecl;
Var
  i  : Integer;
Begin
  For i := high(UnitNamespaces) Downto 0 Do
  Begin
    Result := ScriptData.GetDeclaration(Name, UnitNamespaces[i]);
    If Result <> nil Then exit;
  End;
  Result := ScriptData.GetDeclaration(Name, '$SYSTEM');
  If Result <> nil Then exit;
  Result := ScriptData.GetDeclaration(Name, '$STATIC');
End;

Function TRutisCompilerDelphi.GetDeclaration(Name : AnsiString; Exclude : Array Of TRutisDecl) : TRutisDecl;
Var
  i  : Integer;
Begin
  For i := high(UnitNamespaces) Downto 0 Do
  Begin
    Result := ScriptData.GetDeclaration(Name, UnitNamespaces[i], Exclude);
    If Result <> nil Then exit;
  End;
  Result := ScriptData.GetDeclaration(Name, '$SYSTEM', Exclude);
  If Result <> nil Then exit;
  Result := ScriptData.GetDeclaration(Name, '$STATIC', Exclude);
End;

Function TRutisCompilerDelphi.GetDeclarationID(Name : AnsiString) : Integer;
Var
  i  : Integer;
Begin
  For i := high(UnitNamespaces) Downto 0 Do
  Begin
    Result := ScriptData.GetDeclarationId(Name, UnitNamespaces[i]);
    If Result <> -1 Then exit;
  End;
  Result := ScriptData.GetDeclarationId(Name, '$SYSTEM');
End;

Function TRutisCompilerDelphi.CheckRedefinition(Name : AnsiString) : Boolean;
Begin
  Result := ScriptData.GetDeclarationId(Name, CurrentNamespace) <> -1;
  If not Result Then
    Result := ScriptData.GetDeclarationId(Name, '$SYSTEM') <> -1;
  If not Result Then
    Result := ScriptData.GetDeclarationId(Name, '$STATIC') <> -1;
  //If Result Then
  //  RaiseError(ERR_INDENT_REDEFINED, [Name]);
End;

//==============================================================================
//==============================================================================

Procedure TRutisCompilerDelphi.PushScanner;
Begin
  PushScannerLine   := ScannerLine;
  PushScannerStrPos := ScannerStrPos;
  PushScannerCh     := ScannerCh;
  PushIdentSym      := IdentSym;
End;

Procedure TRutisCompilerDelphi.PopScanner;
Begin
  ScannerLine   := PushScannerLine;
  ScannerStrPos := PushScannerStrPos;
  ScannerCh     := PushScannerCh;
  IdentSym      := PushIdentSym;
End;

//==============================================================================

Procedure TRutisCompilerDelphi.PushCompilerState;
Begin
  SetLength(CompilerStates, length(CompilerStates) + 1);
  With CompilerStates[high(CompilerStates)] Do
  Begin
    CSScannerLine   := ScannerLine;
    CSScannerStrPos := ScannerStrPos;
    CSScannerCh     := ScannerCh;
    CSIdentSym      := IdentSym;
    CSCodePos       := length(ScriptData.Code);
  End;
End;

Procedure TRutisCompilerDelphi.PopCompilerState;
Begin
  With CompilerStates[high(CompilerStates)] Do
  Begin
    ScannerLine   := CSScannerLine;
    ScannerStrPos := CSScannerStrPos;
    ScannerCh     := CSScannerCh;
    IdentSym      := CSIdentSym;
    SetLength(ScriptData.Code, CSCodePos);
  End;
  SetLength(CompilerStates, length(CompilerStates) - 1);
End;

//==============================================================================

Procedure TRutisCompilerDelphi.GetSym;
Var
  doUpCase  : Boolean;

  Procedure GetCh;
  Begin
    If ScannerStrPos < 1 Then
      ScannerStrPos := 1;
    If ScannerLine > ScriptCode.Count - 1 Then
    Begin
      ScannerCh     := #255;
      exit;
    End;
    If ScannerLine < 0 Then
    Begin
      ScannerLine   := 0;
      ScannerStrPos := 1;
      ScannerCh     := ' ';
      exit;
    End;
    If ScannerStrPos > length(ScriptCode[ScannerLine]) Then
    Begin
      Inc(ScannerLine);
      ScannerStrPos := 1;
      ScannerCh     := ' ';
      exit;
    End;
    ScannerCh := ScriptCode[ScannerLine][ScannerStrPos];
    If doUpCase Then
      ScannerCh := UpCase(ScannerCh); //Case in-sensitiVe
    Inc(ScannerStrPos);
  End;

Var
  sym  : TRutisSymbol;
Begin
  LastScannerLine := ScannerLine;
  LastScannerStrPos := ScannerStrPos;
  doUpCase := True;
  While True Do
  Begin
    IdentSym := sNone;
    IdentStr := '';
    While ScannerCh in [' ', #0, #13, #10] Do
      GetCh;

    Case ScannerCh Of
      'A'..'Z', '_'  : (*Ident/ReserVed Word*)
      Begin
        While ScannerCh in ['A'..'Z', '_', '0'..'9'] Do
        Begin
          IdentStr := IdentStr + ScannerCh;
          GetCh;
        End;
        IdentSym := sIdent;

        For sym := sUnknown To sNone Do
          If IdentStr = cSymbols[sym] Then
          Begin
            IdentSym := sym;
            Break;
          End;
        Exit;
      End;

      ';', '+', '-', '=', ',', '.', '*', '(', ')', '^', '@', '[', ']' :
      Begin (*IdentSyme die nur aus 1 Zeichen bestehen können*)
        IdentStr := ScannerCh;
        IdentSym := sUnknown;
        For sym := sUnknown To sNone Do
          If IdentStr = cSymbols[sym] Then
          Begin
            IdentSym := sym;
            Break;
          End;
        GetCh;
        Exit;
      End; (*IdentSyme die nur aus 1 Zeichen bestehen können*)

      ':', '<', '>' :
      Begin
        IdentStr := ScannerCh;
        GetCh;
        If ScannerCh in ['=', '<', '>'] Then
        Begin
          IdentStr := IdentStr + ScannerCh;
          GetCh;
        End;
        For sym := sUnknown To sNone Do
          If IdentStr = cSymbols[sym] Then
          Begin
            IdentSym := sym;
            Break;
          End;
        exit;
      End;

      '/'  : (* Zeichen die ein naScannerChfolgendes Zeichen haben können(in diesm Falle ein = )*)
      Begin
        IdentStr := ScannerCh;
        GetCh;
        If ScannerCh = '/' Then
        Begin
          Inc(ScannerLine);
          ScannerStrPos := 1;
          GetCh;
          continue;
        End
        Else
        Begin
          IdentSym := sSlash;
          exit;
        End;
      End;

      '{' :
      Begin
        Repeat
          GetCh;
        Until (ScannerCh = '}') or (ScannerCh = #255);
        GetCh;
        continue;
      End;

      ''''{'} :(*Strings*)
      Begin
        IdentSym := sString;
        doUpCase := False;
        GetCh;
        IdentStr := '';
        If (ScannerCh = '''') Then
          GetCh
        Else
          While True Do
          Begin
            IdentStr := IdentStr + ScannerCh;
            GetCh;
            If (ScannerCh = '''') Then
            Begin
              GetCh;
              If (ScannerCh <> '''') Then
                Break;
            End;
            If ScannerCh = #255 Then
              exit;
          End;
        doUpCase := True;
        exit;
      End;

      '0'..'9', '$'  : (*Zahlen*)
      Begin
        IdentSym := sInteger;
        IdentStr := ScannerCh;
        GetCh;
        If (IdentStr = '$') Then
        Begin
        //HexZahl
          While ScannerCh in ['0'..'9', 'A'..'F'] Do
          Begin
            IdentStr := IdentStr + ScannerCh;
            GetCh;
          End;
          IdentNum := StrToInt(IdentStr);
          Exit;
        End
        Else
        Begin
        //NormaleZahl
          While ScannerCh in ['0'..'9', '.', 'E'] Do
          Begin
            IdentStr := IdentStr + ScannerCh;
            If (ScannerCh = '.') or
              (ScannerCh = 'E') Then
              IdentSym := sFloat;
            GetCh;
          End;
          If IdentSym = sInteger Then
            IdentNum := StrToInt(IdentStr)
          Else
            IdentNum := StrToFloat(IdentStr);
          Exit;
        End;
      End; (*Zahlen*)
      #255  : exit;
    Else
      RaiseError(ERR_SCANNER_UNEXPECTED_CHAR + ': ''' + ScannerCh + '''');
      exit;
    End;
  End;
End;

//==============================================================================

End.

