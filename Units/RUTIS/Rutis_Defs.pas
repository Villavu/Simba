{/==============================================================================
//==============================================================================

RUTIS-Engine (RunTimeScript-Engine)

Rutis_Defs.pas (part of) RUTIS-Engine

--> This unit contains the types, defines, functions, etc.
    which are used by the RUTIS-Engine

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
The Legal Code (the full license) can be read at the following adress:
> http: //creativecommons.org/licenses/by-nc-sa/3.0/de/

//==============================================================================
//==============================================================================}

Unit Rutis_Defs;

Interface

{$i Delphi_Versions.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

Uses
  lclintf, {$ifdef DELPHI_7_UP}Variants, {$endif}
  SysUtils, Rutis_Stack;

 //====================================================================
 //====================================================================

Type
  TOperatorCode = (ocEqual, ocGreater, ocLess, ocGEqual, ocLEqual, ocUnequal{, ocAnd, ocOr, ocXOr, ocNot});

  TVariantArray = Array Of Variant;
  PVariantArray = ^TVariantArray;

  //====================================================================
{$REGION 'Exceptions'}
  ERutisCompilerError = Class(Exception)
  Public
    ENamespace  : String;
    ELine       : Integer;
    EChrPos     : Integer;
    Constructor Create(Msg, Namespace : String); Overload;
    Constructor Create(Msg, Namespace : String; CLine, CPos : Integer); Overload;
    Constructor CreateFmt(Const Msg : String; Const Args : Array Of Const; Namespace : String); Overload;
    Constructor CreateFmt(Const Msg : String; Const Args : Array Of Const; Namespace : String; CLine, CPos : Integer); Overload;
    Procedure Assign(E : ERutisCompilerError);
  End;

{$ENDREGION}

//====================================================================
{$REGION 'Commands'}
//====================================================================
//====================================================================
//Byte-Code Commands

Type
  TRutisCmd = (_gen, _gen1, _gen2, _gen4,
    _lod, _sto, _mov,                    // Stack Variables
    _lodp, _stop,
    _at, _at2, _Ptr, _PtrP, _CPtr,
    _CopyAStr, _CopyWStr, _StoAStr, _StoWStr,
    _SMem, _MemS,
    _getCh, _setCh, _GASL, _SASL, _GWSL, _SWSL,
    _pupo,
    _call, _ret, _ext,                   // Call
    _add, _sub, _mult, _div, _mod, _ets, // math
    _inc, _conv,                         // other math
    _opr, _And, _Or, _XOr, _Not,         // Opr
    _jmp, _JZ, _JL, _JG, _JLZ, _JGZ,
    _wri, _lodr, _RStr,                  // others (write,ressource)
    _nocmd);

Const
  cCmd  : Array[TRutisCmd] Of ShortString   = (
    'gen', 'gen1', 'gen2', 'gen4',
    'lod', 'sto', 'mov',                       // Stack Global Vriables
    'lodp', 'stop',                            {'movp',}
    'at', 'at2', 'ptr', 'ptrp', 'cptr',
    'castr', 'cwstr', 'sastr', 'swstr',
    'smem', 'meml',
    'getch', 'setch', 'getasl', 'setasl', 'getwsl', 'setwsl',
    'pupo',
    'call', 'ret', 'ext',                      // Call
    'add', 'sub', 'mult', 'div', 'mod', 'ets', // math
    'inc', 'conv',                             // other math
    'opr', 'and', 'or', 'xor', 'not',          // Opr
    'jmp', 'jz', 'jl', 'jg', 'jlz', 'jgz',
    'wri', 'lodr', 'rstr',                     // others (write,ressource)
    'nop');

  CmdParamCount  : Array[TRutisCmd] Of Byte = (
    1, 1, 1, 1,
    3, 3, 3,                       // Stack Global Vriables
    3, 3,
    0, 0, 2, 2, 0,
    0, 0, 3, 3,
    0, 0,
    0, 0, 0, 0, 0, 0,
    1,
    2, 1, 2,                          // Call
    2, 2, 1, 1, 1, 1,                 // math
    3, 2,                             // other math
    3, 1, 1, 1, 2,                    // Opr
    1, 2, 2, 2, 2, 2,
    1, 2, 1,                          // others (write,ressource)
    0);

Const
  LongCMD  : Array[TRutisCmd] Of ShortString = (
    'GenVar     ', 'GenByte    ', 'GenWord    ',
    'GenCardinal', 'Load       ', 'Store      ',
    'Move       ', 'LoadFromPtr', 'StoreAtPtr ',
    'GetAddr    ', 'GetAddr2   ', 'GetPtr     ',
    'PtrP       ', 'CheckPtr   ',
    'CopyAStr   ', 'CopyWStr   ',
    'StoreAStr  ', 'StoreWStr  ',
    'ScaleMem   ', 'MemSize    ',
    'GetChar    ', 'SetChar    ',
    'GetAStrLen ', 'SetAStrLen ',
    'GetWStrLen ', 'SetWStrLen ',
    'PushOrPop  ',
    'Call       ', 'Return     ', 'ExtCall    ',
    'Add        ', 'Sub        ', 'Mult       ',
    'Div        ', 'Modulo     ', 'EnumToSet  ',
    'Inc        ', 'Convert    ',
    'Operand    ', 'And        ', 'Or         ',
    'XOr        ', 'Not        ',
    'Jump       ', 'JumpFalse  ',
    'JmpLess    ', 'JmpGreater ',
    'JmpLessZ   ', 'JmpGreaterZ',
    'Write      ', 'LoadRes    ', 'LoadResStr ',
    'NoOperation');

Type
  TRutisScriptCmd = Record
    Cmd       : TRutisCmd;     // Befehl
    P1        : Integer;       // Parameter 1
    P2        : Integer;       // Parameter 2
    P3        : Word;          // Parameter 3
    CodeLine  : Smallint;      // Debug-Zeile
    //StatementID : Word;      // Debug-Statement ID
  End;

  TRutisCode = Array Of TRutisScriptCmd;
  PRutisCode = ^TRutisCode;

{$ENDREGION}

//====================================================================
{$REGION 'Rutis-Internal-Types'}
//====================================================================
//====================================================================
// Rutis-Internal-Types

Type
  TRutisIntType  = (
    intByte, intShortInt,
    intWord, intSmallint,
    intCardinal, intInteger,
    intSingle, intDouble, intExtended,
    intBoolean, intPointer, intArray, intRecord,
    //intEnum, intSet1, intSet2, intSet4, intSet8, intSetN,
    intEnum, intSet,
    intShortString, intAChar, intAString, intWChar, intWString,
    intMethod, intObjMethod,
    intNone);
  TRutisIntTypes = Set Of TRutisIntType;

Const
  IntTypeSizes  : Array [TRutisIntType] Of Word =
    (1, 1,
    2, 2,
    4, 4,
    //intSingle, intDouble, intExtended,
    4, 8, 10,
    //intBoolean, intPointer, intArray, intRecord,
    1, 4, 4, 0,
    //intEnum, intSet,
    1, 4,
    //intShortString, intAChar, intAString, intWChar, intWString,
    256, 1, 4, 2, 4,
    4, 8,
    0);
  IntTypeAlignSizes  : Array [TRutisIntType] Of Word =
    (1, 1,
    2, 2,
    4, 4,
    //intSingle, intDouble, intExtended,
    4, 8, 16,
    //intBoolean, intPointer, intArray, intRecord,
    1, 4, 4, 1,
    //intEnum, intSet,
    1, 4,
    //intShortString, intAChar, intAString, intWChar, intWString,
    1, 1, 4, 2, 4,
    4, 8,
    1);

  InternalIntegers  : TRutisIntTypes = [intByte..intInteger, intEnum];
  InternalFloats  : TRutisIntTypes  = [intSingle, intDouble, intExtended];
  InternalNumbers  : TRutisIntTypes = [intByte..intInteger, intEnum] + [intSingle, intDouble, intExtended];

  RutisVarTypeTOString  : Array [TRutisIntType] Of String =
    ('Byte', 'ShortInt',
    'Word', 'Smallint',
    'Cardinal', 'Integer',
    'Single', 'Double', 'Extended',
    'Boolean', 'Pointer', 'Array', 'Record',
    'Enumeration', 'Set',
    'ShortString', 'AnsiChar', 'AnsiString', 'WideChar', 'WideString',
    'Method', 'Method-of-Object',
    'None');

  RutisTypeConvertTable  : Array [TRutisIntType] Of TRutisIntTypes =
    ({intByte}[intByte..intExtended, intBoolean, intAChar, intEnum],
    {intShortInt}[intByte..intExtended, intBoolean, intAChar, intEnum],
    {intWord}[intByte..intExtended, intBoolean, intAChar, intEnum],
    {intSmallint}[intByte..intExtended, intBoolean, intAChar, intEnum],
    {intCardinal}[intByte..intExtended, intBoolean, intAChar, intMethod, intEnum],
    {intInteger}[intByte..intExtended, intBoolean, intAChar, intMethod, intEnum],
    //=============================
    {intSingle}[intSingle..intExtended],
    {intDouble}[intSingle..intExtended],
    {intExtended}[intSingle..intExtended],
    //=============================
    {intBoolean}[intByte..intInteger, intBoolean],
    {intPointer}[intCardinal, intInteger, intPointer, intArray, intEnum],
    {intArray}[intCardinal, intPointer, intArray],
    {intRecord}[],
    {intEnum}[intByte..intInteger, intSet],
    {intSet}[intByte..intInteger],
    {intShortString}[intAString],
    {intAChar}[intByte..intInteger, intAString{, intShortString}],
    {intAString}[intCardinal, intPointer, intArray, intWString],
    {intWChar}[intWord..intInteger, intAString, intWString],
    {intWString}[intCardinal, intPointer, intAString],
    {intMethod}[intCardinal, intInteger, intPointer],
    {intObjMethod}[intCardinal, intInteger, intPointer],
    {intNone}[]);

{$ENDREGION}

//====================================================================
{$REGION 'Rutis Stack'}
Type
  TRutisStack = Class(TRutisBasicStack)
  Public
    Function ReadToStr(intType : TRutisIntType; Adr : Integer) : String;
    Function ReadToVariant(intType : TRutisIntType; Adr : Integer) : Variant;
    Procedure WriteFromVariant(intType : TRutisIntType; Adr : Integer; Val : Variant);
  End;

{$ENDREGION}

//====================================================================
{$REGION 'Rutis Declarations'}
//====================================================================
//====================================================================
//Rutis Declarations

Type
  TRutisScriptData = Class;

  TRutisDecl       = Class
  Public
    Name  : String;
  End;

  TRutisDeclArray = Array Of TRutisDecl;
  PRutisDeclArray = ^TRutisDeclArray;

  //==============================================================================

  TRutisNamespace = Class(TRutisDecl)
  Public
    Declarations  : TRutisDeclArray;
    CodeFile      : String;
    CodeStart,
    CodeEnd       : Integer;
    Destructor Destroy; Override;
    Procedure AddDeclaration(Decl : TRutisDecl);
    Function GetDeclaration(AName : String) : TRutisDecl; Overload;
    Function GetDeclaration(AName : String; Exclude : Array Of TRutisDecl) : TRutisDecl; Overload;
    Function GetDeclarationID(AName : String) : Integer;
  End;

  TRutisNamespaceArray = Array Of TRutisNamespace;
  PRutisNamespaceArray = ^TRutisNamespaceArray;

  //==============================================================================

  TRutisTypeDecl      = Class;

  TRutisTypeDeclClass = Class Of TRutisTypeDecl;
  TRutisTypeArray     = Array Of TRutisTypeDecl;

  TRutisTypeDecl = Class(TRutisDecl)
  Private
    Function GetAlignSize : Byte; Virtual;
  Public
    Size          : Word;
    ExplicitCast  : Boolean;
    Function ConvertTo(WantType : TRutisTypeDecl; ScriptData : TRutisScriptData) : Boolean; Virtual;
    Function GetInternalType : TRutisIntType; Virtual;
    Function InternalType : TRutisIntType;
    Property AlignSize  : Byte Read GetAlignSize;
  End;

  //==============================================================================

  TRutisVarDecl = Class(TRutisDecl)
    VarType   : TRutisTypeDecl;
    Address   : Integer;
    Level     : Byte;
    isConst   : Boolean;
    ConstVal  : Variant;
  End;

  //==============================================================================
  TRutisParamInfo = Record
    Data     : Pointer;
    Adr      : Integer;
    IntType  : TRutisIntType;
    Size     : Word;
  End;
  PRutisParamInfo = ^TRutisParamInfo;

  TRutisParamInfoArray = Array Of TRutisParamInfo;
  PRutisParamInfoArray = ^TRutisParamInfoArray;

  TExtVariMethod  = Procedure(Params : PVariantArray; Result : PVariant);
  TExtStackMethod = Procedure(Params : PRutisParamInfoArray; Result : PRutisParamInfo);

  //==============================================================================

  TRutisTypeCopy = Class(TRutisTypeDecl)
  Public
    CopyType  : TRutisTypeDecl;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TRutisVarType = Class(TRutisTypeDecl)
  Public
    IntType  : TRutisIntType;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TEnumVal = Record
    Name   : String;
    Value  : Integer;
  End;

  TRutisEnumType = Class(TRutisTypeDecl)
  Public
    Enums  : Array Of TEnumVal;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TRutisSetType = Class(TRutisTypeDecl)
  Public
    EnumType  : TRutisEnumType;
    Function GetInternalType : TRutisIntType; Override;
    Function ConvertTo(WantType : TRutisTypeDecl; ScriptData : TRutisScriptData) : Boolean; Override;
  End;

  TRutisPointerType = Class(TRutisTypeDecl)
  Public
    PointerType  : TRutisTypeDecl;
    Constructor Create;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TRutisArrayType = Class(TRutisTypeDecl)
  Public
    ArrayType  : TRutisTypeDecl;
    Constructor Create;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TRutisStructType = Class(TRutisTypeDecl)
  Private
    Function GetAlignSize : Byte; Override;
  Public
    StructTypes  : Array Of TRutisVarDecl;
    IsPacked     : Boolean;
    fAlignSize   : Byte;
    Destructor Destroy; Override;
    Function GetInternalType : TRutisIntType; Override;
    Function ConvertTo(WantType : TRutisTypeDecl; ScriptData : TRutisScriptData) : Boolean; Override;
    Function VarID(VarName : String) : Integer;
  End;

  TRutisClassType = Class(TRutisTypeDecl)
  Public
    IsExternal   : Boolean;
    ClassStruct  : TRutisStructType;
    ParentClass  : TRutisClassType;
    Constructor Create;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TRutisTypeLink = Record
    Name          : String;
    Size          : Word;
    InternalType  : TRutisIntType;
    TypeData      : TRutisTypeDecl;
    isVarParam    : Boolean;
  End;
  TRutisMethodParamArray = Array Of TRutisTypeLink;

  TRutisMethodType = Class(TRutisTypeDecl)
  Public
    Method_Name  : String;
    Description  : String;

    IsFunction    : Boolean;
    Overloaded    : Boolean;
    Params        : TRutisMethodParamArray;
    ParamsSize    : Word;
    MethodResult  : TRutisTypeLink;

    Declarations   : TRutisDeclArray;
    MethodTableID  : Integer;
    Constructor Create;
    Destructor Destroy; Override;
    Function GetInternalType : TRutisIntType; Override;
  End;

  TRutisExtMethodType = Class(TRutisMethodType)
  Public
    VariMethod   : TExtVariMethod;
    StackMethod  : TExtStackMethod;
  End;

  TRutisDllMethodType = Class(TRutisMethodType)
  Public
    ProcAddress  : Pointer;
    DllName      : String;
    ProcName     : String;
  End;

{$ENDREGION}

//====================================================================
//====================================================================

  TIdentType = (itVar, itPVar, itConst, itFixVal, itMethod, itError);

  TAnalyzeIdentInfo = Record
  //AllowedIdents  : TRutisIntTypes;
    TypeData          : TRutisTypeDecl;
    InternalType      : TRutisIntType;
    VarDecl           : TRutisVarDecl;
    Adr, Lvl{, Size}  : Integer;
    IdentType         : TIdentType;
  End;

  //====================================================================

  TRutisCallStackItem = Record
    CallLine   : Integer;
    StackBase  : Integer;
    Level      : Integer;
  End;
  TRutisCallStack = Array Of TRutisCallStackItem;

  //====================================================================

  TMemoryRange = Record
    Start  : PByte;
    Size   : Integer;
  End;

  //====================================================================

  TRutisErrorType = (etHint, etWarning, etCompilerError, etRuntimeError);

  TErrorProcedure = Procedure(s : String; ErrorType : TRutisErrorType) Of Object;

  //====================================================================

  TRutisScriptData = Class
  Private
    Function GetDeclListCount : Integer;
    Function GetDeclListItem(Index : Integer) : TRutisDecl;
    Procedure SetDeclListItem(Index : Integer; Value : TRutisDecl);
    Function IsCompiled : Boolean;
  Public
    ScriptName     : String;
    Code           : TRutisCode;
    Ressources     : TRutisStack;
    MethodTable    : Array Of TRutisMethodType;
    //========================
    Running        : Boolean;
    Paused         : Boolean;
    Stack          : TRutisStack;
    CallStack      : TRutisCallStack;
    AddrRanges     : Array Of TMemoryRange;
    //========================
    CodeLine       : Integer;
    CurrCmd        : TRutisScriptCmd;
    //========================
    CompilerLine   : Integer;
    CodeInsertPos  : Integer;
    Namespaces     : TRutisNamespaceArray;
    //========================
    Constructor Create(StackBlockSize : Cardinal = 1024);
    Destructor Destroy; Override;
    Procedure Clear;
    Procedure FreeExtData;
    //========================
    Function GenCodeV(Command : TRutisCmd; Param1, Param2, Param3 : Variant; LineDiff : Integer = 0) : Integer;
    Function GenCode(Command : TRutisCmd; Param1, Param2 : Integer; Param3 : Word; LineDiff : Integer = 0) : Integer;
    Procedure InsertCodeV(Index : Integer; Command : TRutisCmd; Param1, Param2, Param3 : Variant; LineDiff : Integer = 0);
    Procedure InsertCode(Index : Integer; Command : TRutisCmd; Param1, Param2 : Integer; Param3 : Word; LineDiff : Integer = 0);
    Procedure DeleteInstruction(index : Integer);
    //========================
    Function GetNamespace(Name : String) : TRutisNamespace;
    Procedure AddDeclaration(Decl : TRutisDecl; Namespace : String);
    Function GetDeclarationID(Name : String; Namespace : String) : Integer; //overload;
    Function GetDeclaration(Name : String; Namespace : String) : TRutisDecl; Overload;
    Function GetDeclaration(Name : String; Namespace : String; Exclude : Array Of TRutisDecl) : TRutisDecl; Overload;
    {Function GetDeclarationID(Name: String): Integer; overload;
    Function GetDeclaration(Name: String): TRutisDecl; overload;}
    //========================
    Property Compiled  : Boolean Read IsCompiled;
    Property DeclarationCount  : Integer Read GetDeclListCount;
    Property DeclarationList[Index  : Integer]  : TRutisDecl Read GetDeclListItem Write SetDeclListItem;
  End;

  PRutisScriptData = ^TRutisScriptData;

 //====================================================================
 //====================================================================

{$ifndef FPC}
//Function OutputDebugString(lpDebugStrA : PAnsiChar) : Cardinal; Stdcall;
//  External 'kernel32.dll' Name 'OutputDebugStringA';
{$endif}


//==============================================================================
//==============================================================================
{$ifndef FPC}
Function DynamicDllCall(Dll : String; Const Name : String; HasResult : Boolean; Var Returned : Cardinal; Const Parameters : Array Of Pointer) : Boolean; Overload;
Function DynamicDllCall(hDll : THandle; Const Name : String; HasResult : Boolean; Var Returned : Cardinal; Const Parameters : Array Of Pointer) : Boolean; Overload;
Function DynamicDllCall(Proc : Pointer; HasResult : Boolean; Var Returned : Cardinal; Const Parameters : Array Of Pointer) : Boolean; Overload;
{$endif}
//====================================================================
//====================================================================
Function SymToStr(Sym : TRutisCmd) : String;
Function StrToSym(Sym : String) : TRutisCmd;
//====================================================================
Function FindDeclarationID(Name : String; Decl : PRutisDeclArray) : Integer;
Function FindDeclaration(Name : String; Decl : PRutisDeclArray) : TRutisDecl;
//====================================================================

//====================================================================
//====================================================================
Implementation
//====================================================================
//====================================================================

{$ifndef FPC}
// Calls a function from a library.
// if it's not loaded yet, it will call LoadLibrary() to load it.
Function DynamicDllCall(Dll : String; Const Name : String; HasResult : Boolean; Var Returned : Cardinal; Const Parameters : Array Of Pointer) : Boolean;
Var
  Proc  : Pointer;
  x, n  : Integer;
  p     : Pointer;
  hDll  : THandle;
Begin
  Result   := False;
  Returned := 0;
  //Get Libary
  hDll     := GetModuleHandle(PChar(Dll));
  If hDll = 0 Then
    hDll := LoadLibrary(PChar(Dll));
  If hDll = 0 Then
    exit;
  //Get Proc-Address
  Proc := GetProcAddress(hDll, PChar(Name));
  If not Assigned(Proc) Then
    exit;
  //Load Parameters
  n    := High(Parameters);
  If n > -1 Then
  Begin
    x := n;
    Repeat
      p := Parameters[x];
      Asm
        PUSH p
      End;
      Dec(x);
    Until x = -1;
  End;
  //Call Procedure
  Asm
    CALL Proc
  End;
  //Get Result
  If HasResult Then
  Begin
    Asm
      MOV p, EAX
    End;
    Returned := Cardinal(p);
  End;
  Result := True;
End;
// Calls a function from a loaded library
Function DynamicDllCall(hDll : THandle; Const Name : String; HasResult : Boolean; Var Returned : Cardinal; Const Parameters : Array Of Pointer) : Boolean;
Var
  Proc  : Pointer;
  x, n  : Integer;
  p     : Pointer;
Begin
  Result   := False;
  Returned := 0;
  //Get Proc-Address
  Proc     := GetProcAddress(hDll, PChar(Name));
  If not Assigned(Proc) Then
    exit;
  //Load Parameters
  n        := High(Parameters);
  If n > -1 Then
  Begin
    x := n;
    Repeat
      p := Parameters[x];
      Asm
        PUSH p
      End;
      Dec(x);
    Until x = -1;
  End;
  //Call Procedure
  Asm
    CALL Proc
  End;
  //Get Result
  If HasResult Then
  Begin
    Asm
      MOV p, EAX
    End;
    Returned := Cardinal(p);
  End;
  Result := True;
End;
// Calls a function from a Proc-Address
Function DynamicDllCall(Proc : Pointer; HasResult : Boolean; Var Returned : Cardinal; Const Parameters : Array Of Pointer) : Boolean;
Var
  x, n  : Integer;
  p     : Pointer;
Begin
  Result   := False;
  Returned := 0;
  //Check Proc-Address
  If not Assigned(Proc) Then
    exit;
  //Load Parameters
  n        := High(Parameters);
  If n > -1 Then
  Begin
    x := n;
    Repeat
      p := Parameters[x];
      Asm
        PUSH p
      End;
      Dec(x);
    Until x = -1;
  End;
  //Call Procedure
  Asm
    CALL Proc
  End;
  //Get Result
  If HasResult Then
  Begin
    Asm
      MOV p, EAX
    End;
    Returned := Cardinal(p);
  End;
  Result := True;
End;

{$endif}


//==============================================================================
 { ERutisCompilerError }

Constructor ERutisCompilerError.Create(Msg, Namespace : String);
Begin
  Inherited Create(Msg);
  ENamespace := Namespace;
  ELine      := -1;
  EChrPos    := 0;
End;

Constructor ERutisCompilerError.Create(Msg, Namespace : String; CLine, CPos : Integer);
Begin
  Inherited Create(Msg);
  ENamespace := Namespace;
  ELine      := CLine;
  EChrPos    := CPos;
End;

Constructor ERutisCompilerError.CreateFmt(Const Msg : String; Const Args : Array Of Const; Namespace : String);
Begin
  Inherited CreateFmt(Msg, Args);
  ENamespace := Namespace;
  ELine      := -1;
  EChrPos    := 0;
End;

Constructor ERutisCompilerError.CreateFmt(Const Msg : String; Const Args : Array Of Const; Namespace : String; CLine, CPos : Integer);
Begin
  Inherited CreateFmt(Msg, Args);
  ENamespace := Namespace;
  ELine      := CLine;
  EChrPos    := CPos;
End;

Procedure ERutisCompilerError.Assign(E : ERutisCompilerError);
Begin
  Message    := E.Message;
  ENamespace := E.ENamespace;
  ELine      := E.ELine;
  EChrPos    := E.EChrPos;
End;

//====================================================================
{$REGION 'Rutis Declarations'}
//====================================================================

//====================================================================
 { TRutisNamespaceDecl }

Destructor TRutisNamespace.Destroy;
Var
  i  : Integer;
Begin
  For I := 0 To high(Declarations) Do
    Declarations[i].Free;
  //SetLength(Declarations, 0);
  Inherited;
End;

Procedure TRutisNamespace.AddDeclaration(Decl : TRutisDecl);
Begin
  If Decl = nil Then exit;
  SetLength(Declarations, Length(Declarations) + 1);
  Declarations[high(Declarations)] := Decl;
  Decl.Name := UpperCase(Decl.Name);
End;

Function TRutisNamespace.GetDeclaration(AName : String) : TRutisDecl;
Var i  : Integer;
Begin
  Result := nil;
  AName  := UpperCase(AName);
  For I := high(Declarations) Downto 0 Do
    If Declarations[i].Name = AName Then
    Begin
      Result := Declarations[i];
      exit;
    End;
End;

Function TRutisNamespace.GetDeclaration(AName : String; Exclude : Array Of TRutisDecl) : TRutisDecl;
Var
  i, j      : Integer;
  Excluded  : Boolean;
Begin
  Result := nil;
  AName  := UpperCase(AName);
  For I := high(Declarations) Downto 0 Do
    If (Declarations[i].Name = AName) Then
    Begin
      Excluded := False;
      For j := 0 To high(Exclude) Do
        If Declarations[i] = Exclude[j] Then
        Begin
          Excluded := True;
          break;
        End;
      If Excluded Then Continue;
      Result := Declarations[i];
      exit;
    End;
End;

Function TRutisNamespace.GetDeclarationID(AName : String) : Integer;
Begin
  AName := UpperCase(AName);
  For Result := high(Declarations) Downto 0 Do
    If Declarations[Result].Name = AName Then
      exit;
  Result := -1;
End;

//====================================================================
 { TRutisTypeDecl }

Function TRutisTypeDecl.ConvertTo(WantType : TRutisTypeDecl; ScriptData : TRutisScriptData) : Boolean;
Begin
  Result := True;
  If (WantType = self) Then exit;
  If not ExplicitCast Then
  Begin
    If (WantType.InternalType = InternalType) and not (InternalType in [intRecord]) Then exit;
    Result := False;
    If not (WantType.InternalType in RutisTypeConvertTable[InternalType]) Then exit;
    ScriptData.GenCodeV(_conv, InternalType, WantType.InternalType, 0);
    Result := True;
  End
  Else
    Result := False;
End;

Function TRutisTypeDecl.GetAlignSize : Byte;
Begin
  Result := IntTypeAlignSizes[InternalType];
End;

Function TRutisTypeDecl.GetInternalType : TRutisIntType;
Begin
  Result := intNone;
End;

Function TRutisTypeDecl.InternalType : TRutisIntType;
Begin
  If self = nil Then
    Result := intNone
  Else
    Result := GetInternalType;
End;

//====================================================================
 { TRutisTypeCopy }

Function TRutisTypeCopy.GetInternalType : TRutisIntType;
Begin
  Result := self.CopyType.InternalType;
End;

//====================================================================
 { TRutisVarType }

Function TRutisVarType.GetInternalType : TRutisIntType;
Begin
  Result := IntType;
End;

//==============================================================================
 { TRutisEnumType }

Function TRutisEnumType.GetInternalType : TRutisIntType;
Begin
  Result := intEnum;
End;

//==============================================================================
 { TRutisSetType }

Function TRutisSetType.GetInternalType : TRutisIntType;
Begin
  Result := intSet;
End;

Function TRutisSetType.ConvertTo(WantType : TRutisTypeDecl; ScriptData : TRutisScriptData) : Boolean;
Begin
  Result := True;
  If (WantType = self) Then exit;
  If not ExplicitCast Then
  Begin
    If (WantType.InternalType = InternalType) and
      (WantType.Size = Size) Then
      exit;
    Result := False;
    If not (WantType.InternalType in RutisTypeConvertTable[InternalType]) Then exit;
    If (WantType.Size <> Size) Then exit;
    Case Size Of
      1  : ScriptData.GenCodeV(_conv, intByte, WantType.InternalType, 0);
      2  : ScriptData.GenCodeV(_conv, intWord, WantType.InternalType, 0);
      4  : ScriptData.GenCodeV(_conv, intCardinal, WantType.InternalType, 0);
    Else
      exit;
    End;
    Result := True;
  End
  Else
    Result := False;
End;

//====================================================================
 { TRutisPointerType }

Constructor TRutisPointerType.Create;
Begin
  Size := 4;
End;

Function TRutisPointerType.GetInternalType : TRutisIntType;
Begin
  Result := intPointer;
  //  Result := PointerType.InternalType;
End;

//====================================================================
{ TRutisArrayType }

Constructor TRutisArrayType.Create;
Begin
  Size := 4;
End;

Function TRutisArrayType.GetInternalType : TRutisIntType;
Begin
  Result := intArray;
  //Result := ArrayType.InternalType;
End;

//====================================================================
{ TRutisStructType }

Destructor TRutisStructType.Destroy;
Var i  : Integer;
Begin
  For i := 0 To high(StructTypes) Do
    StructTypes[i].Free;
  Inherited;
End;

Function TRutisStructType.GetInternalType : TRutisIntType;
Begin
  Result := intRecord;
End;

Function TRutisStructType.ConvertTo(WantType : TRutisTypeDecl; ScriptData : TRutisScriptData) : Boolean;
Begin
  Result := WantType = self;
End;

Function TRutisStructType.GetAlignSize : Byte;
Begin
  Result := fAlignSize;
End;

Function TRutisStructType.VarID(VarName : String) : Integer;
Begin
  VarName := UpperCase(VarName);
  For Result := 0 To high(StructTypes) Do
    If StructTypes[Result].Name = VarName Then
      exit;
  Result := -1;
End;

//==============================================================================
 { TRutisClassType }

Constructor TRutisClassType.Create;
Begin
  Size := 4;
End;

Function TRutisClassType.GetInternalType : TRutisIntType;
Begin
  Result := intPointer;
End;

//====================================================================
 { TRutisMethodType }

Constructor TRutisMethodType.Create;
Begin
  MethodTableID := -1;
End;

Destructor TRutisMethodType.Destroy;
Var i  : Integer;
Begin
  For i := 0 To high(Declarations) Do
    Declarations[i].Free;
  Inherited;
End;

Function TRutisMethodType.GetInternalType : TRutisIntType;
Begin
  Result := intMethod;
  //Result := ResultType.InternalType;
End;

{$ENDREGION}

//==============================================================================
{$REGION 'Rutis Stack'}
//==============================================================================

Function TRutisStack.ReadToStr(intType : TRutisIntType; Adr : Integer) : String;
Begin
  If Adr >= Top Then
  Begin
    Result := 'NA';
    exit;
  End;
  Case intType Of
    intByte  : Result     := IntToStr(ReadByte(Adr));
    intShortInt  : Result := IntToStr(ReadShortInt(Adr));
    intWord  : Result     := IntToStr(ReadWord(Adr));
    intSmallint  : Result := IntToStr(ReadSmallint(Adr));
    intCardinal  : Result := IntToStr(ReadCardinal(Adr));
    intInteger  : Result  := IntToStr(ReadInteger(Adr));
    intSingle  : Result   := FloatToStr(ReadSingle(Adr));
    intDouble  : Result   := FloatToStr(ReadDouble(Adr));
    intExtended  : Result := FloatToStr(ReadExtended(Adr));
    intPointer  : Result  := IntToHex(ReadCardinal(Adr), 6);
    intArray  : Result    := IntToHex(ReadCardinal(Adr), 6);
    intRecord  : Result   := 'RECORD';
    intEnum  : Result     := IntToStr(ReadByte(Adr));
    intBoolean  : If ReadByte(Adr) = 0 Then
        Result := 'false'
      Else
        Result := 'true';
    intAChar  : Result := ReadAChar(Adr);
    intAString :
    Begin
      Try
        Result := Ansistring(ReadString(Adr - 4));
      Except
        On E : EAccessViolation Do
        Begin
          If Adr = 0 Then Result := '';
        End;
      End;
    End;
    intMethod  : Result := '@' + IntToStr(ReadCardinal(Adr));
    //intMethod    : Result := 'METHOD';//Result := '@' + ;
  Else
    Result := '';
  End;
End;

Function TRutisStack.ReadToVariant(intType : TRutisIntType; Adr : Integer) : Variant;
Begin
  If Adr >= Top Then
  Begin
    Result := 0;
    exit;
  End;
  Case intType Of
    intByte  : Result := ReadByte(Adr);
    intShortInt  : Result := ReadShortInt(Adr);
    intWord  : Result := ReadWord(Adr);
    intSmallint  : Result := ReadSmallint(Adr);
    intCardinal  : Result := ReadCardinal(Adr);
    intInteger  : Result := ReadInteger(Adr);
    intSingle  : Result := ReadSingle(Adr);
    intDouble  : Result := ReadSingle(Adr);
    intExtended  : Result := ReadSingle(Adr);
    intPointer  : Result := ReadCardinal(Adr);
    //intArray     : Result := ReadCardinal(Adr);
    intRecord  : Result := 0;
    intBoolean  : Result := Boolean(GetByte(Adr));
    intAString  : Result := Ansistring(GetString(Adr));
    intAChar  : Result := ReadAChar(Adr);
    intWChar  : Result := ReadWChar(Adr);
    intMethod  : Result := 0;
  Else
    Result := 0;
  End;
End;

Procedure TRutisStack.WriteFromVariant(intType : TRutisIntType; Adr : Integer; Val : Variant);
Begin
  If Adr >= Top Then exit;
  Case intType Of
    intByte  : WriteByte(Adr, Val);
    intShortInt  : WriteShortInt(Adr, Val);
    intWord  : WriteWord(Adr, Val);
    intSmallint  : WriteSmallint(Adr, Val);
    intCardinal,
    intPointer,
    intArray  : WriteCardinal(Adr, Val);
    intInteger  : WriteInteger(Adr, Val);
    intSingle  : WriteSingle(Adr, Val);
    intDouble  : WriteSingle(Adr, Val);
    intExtended  : WriteSingle(Adr, Val);
    intBoolean  : WriteByte(Adr, Val);
    intAString  : WriteString(Adr, Pointer(Ansistring(Val)));
    intAChar  : WriteAChar(Adr, AnsiChar(Byte(Val)));
    intWChar  : WriteWChar(Adr, Widechar(Word(Val)));
  End;
End;

{$ENDREGION}

//==============================================================================
//==============================================================================

Function SymToStr(Sym : TRutisCmd) : String;
Begin
  Result := cCmd[Sym];
End;

Function StrToSym(Sym : String) : TRutisCmd;
Begin
  For Result := _gen To _nocmd Do
    If cCmd[Result] = Sym Then
      exit;
  Result := _nocmd;
End;

//==============================================================================
//==============================================================================

Function FindDeclarationID(Name : String; Decl : PRutisDeclArray) : Integer;
Begin
  Name := UpperCase(Name);
  For Result := 0 To high(Decl^) Do
    If Decl^[Result].Name = Name Then
      exit;
  Result := -1;
End;

Function FindDeclaration(Name : String; Decl : PRutisDeclArray) : TRutisDecl;
Var id  : Integer;
Begin
  id := FindDeclarationID(Name, Decl);
  If id >= 0 Then
    Result := Decl^[id]
  Else
    Result := nil;
End;

//==============================================================================
//==============================================================================
 { TRutisScriptData }

Constructor TRutisScriptData.Create(StackBlockSize : Cardinal);
Begin
  If StackBlockSize < 256 Then StackBlockSize := 256;
  Stack := TRutisStack.Create(StackBlockSize);
  Ressources := TRutisStack.Create(1024 * 2);
End;

Destructor TRutisScriptData.Destroy;
Var i  : Integer;
Begin
  For i := 0 To high(Namespaces) Do
  Begin
    If Namespaces[i].Name = '$STATIC' Then
      SetLength(Namespaces[i].Declarations, 0);
    Namespaces[i].Free;
  End;
  SetLength(Namespaces, 0);

  FreeAndNil(Ressources);

  FreeAndNil(Stack);

  FreeExtData;

  Inherited;
End;

Procedure TRutisScriptData.Clear;
Var i  : Integer;
Begin
  For i := 0 To high(Namespaces) Do
  Begin
    If Namespaces[i].Name = '$STATIC' Then
      SetLength(Namespaces[i].Declarations, 0);
    Namespaces[i].Free;
  End;
  SetLength(Namespaces, 0);

  For i := 0 To high(MethodTable) Do
    MethodTable[i].MethodTableID := -1;
  SetLength(MethodTable, 0);

  SetLength(Code, 0);

  FreeExtData;
  If Stack <> nil Then
    Stack.Clear;
  Try
    Ressources.Clear;
  Except End;
End;

Procedure TRutisScriptData.FreeExtData;
Var
  i  : Integer;
Begin
  For i := 0 To high(AddrRanges) Do
    FreeMem(AddrRanges[i].Start{, ScriptData.AddrRanges[i].Size});
  SetLength(AddrRanges, 0);
End;

//==============================================================================

Function TRutisScriptData.GenCodeV(Command : TRutisCmd; Param1, Param2, Param3 : Variant; LineDiff : Integer = 0) : Integer;
Var
  sng       : Single;
  pp1, pp2  : Integer;
Begin
  If VarType(Param1) in [VarSingle, VarDouble] Then
  Begin
    sng := Param1;
    pp1 := PInteger(@sng)^;
  End
  Else
    pp1 := Param1;

  If VarType(Param2) in [VarSingle, VarDouble] Then
  Begin
    sng := Param2;
    pp2 := PInteger(@sng)^;
  End
  Else
    pp2 := Param2;

  Result := GenCode(Command, pp1, pp2, Param3, LineDiff);
End;

Function TRutisScriptData.GenCode(Command : TRutisCmd; Param1, Param2 : Integer; Param3 : Word; LineDiff : Integer = 0) : Integer;
Var i  : Integer;
Begin
  SetLength(Code, length(Code) + 1);
  If CodeInsertPos > 0 Then
  Begin
    Result := CodeInsertPos;
    Inc(CodeInsertPos);
    For I := high(Code) Downto Result + 1 Do
      Code[i] := Code[i - 1];
  End
  Else
    Result := high(Code);

  With Code[Result] Do
  Begin
    Cmd      := Command;
    P1       := Param1;
    P2       := Param2;
    P3       := Param3;
    CodeLine := CompilerLine + LineDiff;
    //StatementID := CurrStatementID;
  End;
End;

Procedure TRutisScriptData.InsertCodeV(Index : Integer; Command : TRutisCmd; Param1, Param2, Param3 : Variant; LineDiff : Integer = 0);
Var
  sng       : Single;
  pp1, pp2  : Integer;
Begin
  If VarType(Param1) in [VarSingle, VarDouble] Then
  Begin
    sng := Param1;
    pp1 := PInteger(@sng)^;
  End
  Else
    pp1 := Param1;

  If VarType(Param2) in [VarSingle, VarDouble] Then
  Begin
    sng := Param2;
    pp2 := PInteger(@sng)^;
  End
  Else
    pp2 := Param2;

  InsertCode(Index, Command, pp1, pp2, Param3, LineDiff);
End;

Function TRutisScriptData.IsCompiled : Boolean;
Begin
  Result := length(Code) > 0;
End;

Procedure TRutisScriptData.InsertCode(Index : Integer; Command : TRutisCmd; Param1, Param2 : Integer; Param3 : Word; LineDiff : Integer = 0);
Var
  i  : Integer;
Begin
  SetLength(Code, length(Code) + 1);
  For I := high(Code) Downto Index + 1 Do
    Code[i] := Code[i - 1];

  For i := Index + 1 To high(Code) Do
    If (Code[i].Cmd in [_jmp, _jz, _JL, _jg, _call]) and (Code[i].P1 > index) Then
      Inc(Code[i].P1);

  With Code[Index] Do
  Begin
    Cmd      := Command;
    P1       := Param1;
    P2       := Param2;
    P3       := Param3;
    CodeLine := CompilerLine + LineDiff;
  End;
End;

Procedure TRutisScriptData.DeleteInstruction(index : Integer);
Var
  i, j  : Integer;
  jpos  : PInteger;
Begin
  For i := 0 To high(Code) Do
    If (Code[i].Cmd in [_jmp, _jz, _JL, _JG, _JLZ, _JGZ, _call]) and (Code[i].P1 > index) Then
      Dec(Code[i].P1);

  For i := 0 To high(Namespaces) Do
    For j := 0 To high(Namespaces[i].Declarations) Do
      If Namespaces[i].Declarations[j] is TRutisVarDecl Then
        If TRutisVarDecl(Namespaces[i].Declarations[j]).VarType.ClassType = TRutisMethodType Then
        Begin
          jpos := Ressources.GetInteger(TRutisVarDecl(Namespaces[i].Declarations[j]).Address);
          If (jpos^ > index) Then
            jpos^ := jpos^ - 1;
        End;

  For i := index To high(Code) - 1 Do
    Code[i] := Code[i + 1];
  SetLength(Code, length(Code) - 1);
End;

//==============================================================================

Function TRutisScriptData.GetNamespace(Name : String) : TRutisNamespace;
Var i  : Integer;
Begin
  Result := nil;
  Name   := UpperCase(Name);
  For i := 0 To high(Namespaces) Do
    If Namespaces[i].Name = Name Then
    Begin
      Result := Namespaces[i];
      exit;
    End;
End;

Procedure TRutisScriptData.AddDeclaration(Decl : TRutisDecl; Namespace : String);
Var
  Namespacedecl  : TRutisNamespace;
Begin
  If Decl = nil Then exit;
  Namespacedecl := GetNamespace(Namespace);
  If Namespacedecl = nil Then
  Begin
    SetLength(Namespaces, length(Namespaces) + 1);
    Namespacedecl      := TRutisNamespace.Create;
    Namespacedecl.Name := UpperCase(Namespace);
    Namespaces[high(Namespaces)] := Namespacedecl;
  End;
  Namespacedecl.AddDeclaration(Decl);
End;

Function TRutisScriptData.GetDeclarationID(Name : String; Namespace : String) : Integer;
Var
  Namespacedecl  : TRutisNamespace;
Begin
  Result        := -1;
  Namespacedecl := GetNamespace(Namespace);
  If (Namespacedecl = nil) Then exit;
  Result        := Namespacedecl.GetDeclarationID(Name);
End;

Function TRutisScriptData.GetDeclaration(Name : String; Namespace : String) : TRutisDecl;
Var
  Namespacedecl  : TRutisNamespace;
Begin
  Result        := nil;
  Namespacedecl := GetNamespace(Namespace);
  If (Namespacedecl = nil) Then exit;
  Result        := Namespacedecl.GetDeclaration(Name);
End;

Function TRutisScriptData.GetDeclaration(Name : String; Namespace : String; Exclude : Array Of TRutisDecl) : TRutisDecl;
Var
  Namespacedecl  : TRutisNamespace;
Begin
  Result        := nil;
  Namespacedecl := GetNamespace(Namespace);
  If (Namespacedecl = nil) Then exit;
  Result        := Namespacedecl.GetDeclaration(Name, Exclude);
End;

{
Function TRutisScriptData.GetDeclarationID(Name: String) : Integer;
var
  i : Integer;
Begin
  for I := high(Namespaces) downto 0 do
  begin
    Result := Namespaces[i].GetDeclarationID(Name);
    If Result <> -1 then exit;
  end;
  Result := -1;
End;
Function TRutisScriptData.GetDeclaration(Name: String) : TRutisDecl;
var
  i : Integer;
Begin
  for I := high(Namespaces) downto 0 do
  begin
    Result := Namespaces[i].GetDeclaration(Name);
    If Result <> nil then exit;
  end;
  Result := nil;
End;
}
//==============================================================================

Function TRutisScriptData.GetDeclListCount : Integer;
Var i  : Integer;
Begin
  Result := 0;
  For I := 0 To high(Namespaces) Do
    Result := Result + length(Namespaces[i].Declarations);
End;

Function TRutisScriptData.GetDeclListItem(Index : Integer) : TRutisDecl;
Var i  : Integer;
Begin
  Result := nil;
  If length(Namespaces) = 0 Then exit;
  i      := 0;
  While Index >= length(Namespaces[i].Declarations) Do
  Begin
    Index := Index - length(Namespaces[i].Declarations);
    Inc(i);
    If i > high(Namespaces) Then exit;
  End;
  Result := Namespaces[i].Declarations[Index];
End;

Procedure TRutisScriptData.SetDeclListItem(Index : Integer; Value : TRutisDecl);
Var i  : Integer;
Begin
  If length(Namespaces) = 0 Then exit;
  i := 0;
  While Index >= length(Namespaces[i].Declarations) Do
  Begin
    Index := Index - length(Namespaces[i].Declarations);
    Inc(i);
    If i > high(Namespaces) Then exit;
  End;
  Namespaces[i].Declarations[Index] := Value;
End;

//==============================================================================
End.

