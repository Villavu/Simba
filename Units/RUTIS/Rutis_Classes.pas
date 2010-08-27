{/==============================================================================
//==============================================================================

RUTIS-Engine (RunTimeScript-Engine)

Rutis_Classes.pas (part of) RUTIS-Engine

--> This unit contains the basic classes of the RUTIS-Engine


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

Unit Rutis_Classes;

Interface

{$i Delphi_Versions.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

Uses
  lclintf, Forms, Classes,
  {$ifdef DELPHI_7_UP}Variants,{$endif}
  SysUtils,
  Rutis_Defs, File_Manager;

Type
  TRutisCompiler   = Class;

  TRutisEngineBase = Class
  Private
    fCompilerLine  : Integer;
    Function GetCompilerError : Boolean;
  Protected
    fCompilerError  : ERutisCompilerError;
    fScriptError    : Boolean;
    //================================================
    Function GetStackBase : Integer;
    //================================================
    Function CheckStackIndex(Address : Integer) : Boolean;
  Public
    ScriptData            : TRutisScriptData;
    StaticDeclarations    : TRutisDeclArray;
    //================================================
    Compiler              : TRutisCompiler;
    UnitFileManager       : TFileManager;
    ScriptCode            : TStrings;
    ScriptFilePath        : String;
    //================================================
    OnError               : TErrorProcedure;
    OnCodeTimer           : TNotifyEvent;
    OptProcessTimer       : Boolean;
    OptProcessTimerCount  : Integer;
    //================================================
    decl_BYTE             : TRutisVarType;
    decl_Word             : TRutisVarType;
    decl_Cardinal         : TRutisVarType;
    decl_ShortInt         : TRutisVarType;
    decl_SmallInt         : TRutisVarType;
    decl_Integer          : TRutisVarType;
    decl_Single           : TRutisVarType;
    decl_Double           : TRutisVarType;
    decl_Extended         : TRutisVarType;
    decl_Boolean          : TRutisVarType;
    decl_Enum             : TRutisVarType;
    decl_Set4             : TRutisVarType;
    decl_Pointer          : TRutisVarType;
    decl_Method           : TRutisVarType;
    decl_ObjMethod        : TRutisVarType;
    decl_ShortString      : TRutisVarType;
    decl_AChar            : TRutisVarType;
    decl_AString          : TRutisVarType;
    decl_WChar            : TRutisVarType;
    decl_WString          : TRutisVarType;
    decl_PShortString     : TRutisPointerType;
    decl_TClass           : TRutisPointerType;
    decl_TObject          : TRutisClassType;
    //================================================
    Constructor Create(StackBlockSize: Cardinal);
    Destructor Destroy; Override;
    Procedure ClearAll; Virtual;
    //================================================
    Procedure CompilerMessage(Msg : String; ErrorCode : TRutisErrorType = etHint);
    Procedure ScriptMessage(Msg : String; ErrorType : TRutisErrorType = etHint);
    Procedure AddStaticDeclaration(Decl : TRutisDecl);
    Function GetStackLvlAddress(Address, Level : Integer) : Integer;
    //================================================
    Procedure RegExtMethodV(AName : String; AMethod : TExtVariMethod; AParams : Array Of String; AResult : String; ADescription : String = ''); Overload;
    Procedure RegExtMethod(AName : String; AMethod : TExtStackMethod; AParams : Array Of String; AResult : String; ADescription : String = ''); Overload;
    //================================================
    Procedure LoadFromStream(Stream : TStream; CanReadProtected : Boolean = False);
    //================================================
    Procedure LoadScriptFromStream(Stream : TStream; CanReadProtected : Boolean = False);
    Procedure SaveScriptToStream(Stream : TStream; SaveProtected : Boolean = False);
    Procedure CodeUnProtect(val : Integer);
    Procedure CodeProtect(val : Integer);
    Procedure CodeProtego(val : Integer);
    //================================================
    Procedure LoadBCodeFromStream(Stream : TStream);
    Procedure SaveBCodeToStream(Stream : TStream);
    //================================================
    Procedure LoadFromFile(FileName : String; CanReadProtected : Boolean = False);
    Procedure LoadScriptFromFile(FileName : String; CanReadProtected : Boolean = False);
    Procedure SaveScriptToFile(FileName : String; SaveProtected : Boolean = False);
    Procedure LoadBCodeFromFile(FileName : String);
    Procedure SaveBCodeToFile(FileName : String);
    //================================================
    Procedure Compile; Overload;
    Procedure Compile(AScriptCode : TStrings); Overload;
    Procedure Compile(FileName : String); Overload;
    //================================================
    Property CompilerError  : Boolean Read GetCompilerError;
    Property Error  : ERutisCompilerError Read fCompilerError;
  End;

  TRutisCompiler = Class
    Constructor Create(AOwner : TRutisEngineBase);
  Private
  Protected
  //CurrStatementID : Cardinal;
  Public
    Owner               : TRutisEngineBase;
    ScriptData          : TRutisScriptData;
    ScriptCode          : TStrings;
    //========================
    //Options
    optArrayRangeCheck  : Boolean;
    optArrangeFields    : Boolean;
    optArrangeSize      : Integer;
    optCanCompileUnits  : Boolean;
    //========================
    Procedure Compile; Virtual;
    Procedure Optimize; Virtual;
  End;

 //==============================================================================
Procedure StreamWriteString(s : String; Stream : TStream);
Function StreamReadString(Stream : TStream) : String;
//==============================================================================
Implementation
//==============================================================================
//==============================================================================

Procedure StreamWriteString(s : String; Stream : TStream);
Var Len  : Integer;
Begin
  Len := Length(s);
  Stream.Write(Len, SizeOf(Len));
  Stream.Write(PChar(s)^, Len);
End;

Function StreamReadString(Stream : TStream) : String;
Var Len  : Integer;
Begin
  Stream.Read(Len, SizeOf(Len));
  If len > 20000 Then exit;
  SetLength(Result, Len);
  Stream.Read(PChar(Result)^, Len);
End;

//==============================================================================
//==============================================================================
{ TRutisEngineBase }

Constructor TRutisEngineBase.Create(StackBlockSize: Cardinal);
Var
  Decl_Copy  : TRutisTypeCopy;
  Decl_Ptr  : TRutisPointerType;
  Decl_Struct  : TRutisStructType;
Begin
  ScriptCode      := TStringList.Create;
  ScriptData      := TRutisScriptData.Create(StackBlockSize);
  UnitFileManager := TFileManager.Create;

  {$REGION 'Default Types'}
  // UNSIGNED INTEGERS

  decl_BYTE         := TRutisVarType.Create;
  decl_Byte.Name    := 'Byte';
  decl_Byte.Size    := 1;
  decl_Byte.IntType := intByte;
  AddStaticDeclaration(Decl_Byte);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PByte';
  Decl_Ptr.PointerType := Decl_Byte;
  AddStaticDeclaration(Decl_Ptr);


  decl_Word         := TRutisVarType.Create;
  decl_Word.Name    := 'Word';
  decl_Word.Size    := 2;
  decl_Word.IntType := intWord;
  AddStaticDeclaration(Decl_Word);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PWord';
  Decl_Ptr.PointerType := Decl_Word;
  AddStaticDeclaration(Decl_Ptr);


  decl_Cardinal         := TRutisVarType.Create;
  decl_Cardinal.Name    := 'Cardinal';
  decl_Cardinal.Size    := 4;
  decl_Cardinal.IntType := intCardinal;
  AddStaticDeclaration(decl_Cardinal);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'LongWord';
  Decl_Copy.Size := decl_Cardinal.size;
  Decl_Copy.CopyType := decl_Cardinal;
  AddStaticDeclaration(Decl_Copy);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PCardinal';
  Decl_Ptr.PointerType := decl_Cardinal;
  AddStaticDeclaration(Decl_Ptr);


  // SIGNED INTEGERS

  decl_ShortInt         := TRutisVarType.Create;
  decl_ShortInt.Name    := 'ShortInt';
  decl_ShortInt.Size    := 1;
  decl_ShortInt.IntType := intShortInt;
  AddStaticDeclaration(decl_ShortInt);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PShortInt';
  Decl_Ptr.PointerType := decl_ShortInt;
  AddStaticDeclaration(Decl_Ptr);


  decl_SmallInt         := TRutisVarType.Create;
  decl_SmallInt.Name    := 'SmallInt';
  decl_SmallInt.Size    := 2;
  decl_SmallInt.IntType := intSmallInt;
  AddStaticDeclaration(decl_SmallInt);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PSmallInt';
  Decl_Ptr.PointerType := decl_SmallInt;
  AddStaticDeclaration(Decl_Ptr);


  decl_Integer         := TRutisVarType.Create;
  decl_Integer.Name    := 'Integer';
  decl_Integer.Size    := 4;
  decl_Integer.IntType := intInteger;
  AddStaticDeclaration(decl_Integer);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'LongInt';
  Decl_Copy.Size := decl_Integer.size;
  Decl_Copy.CopyType := decl_Integer;
  AddStaticDeclaration(Decl_Copy);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PInteger';
  Decl_Ptr.PointerType := decl_Integer;
  AddStaticDeclaration(Decl_Ptr);

  // FLOATS

  decl_Single         := TRutisVarType.Create;
  decl_Single.Name    := 'Single';
  decl_Single.Size    := 4;
  decl_Single.IntType := intSingle;
  AddStaticDeclaration(decl_Single);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PSingle';
  Decl_Ptr.PointerType := decl_Single;
  AddStaticDeclaration(Decl_Ptr);


  decl_Double         := TRutisVarType.Create;
  decl_Double.Name    := 'Double';
  decl_Double.Size    := 8;
  decl_Double.IntType := intDouble;
  AddStaticDeclaration(decl_Double);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PDouble';
  Decl_Ptr.PointerType := decl_Double;
  AddStaticDeclaration(Decl_Ptr);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'Real';
  Decl_Copy.Size := decl_Double.size;
  Decl_Copy.CopyType := decl_Double;
  AddStaticDeclaration(Decl_Copy);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PReal';
  Decl_Ptr.PointerType := decl_Double;
  AddStaticDeclaration(Decl_Ptr);


  decl_Extended         := TRutisVarType.Create;
  decl_Extended.Name    := 'Extended';
  decl_Extended.Size    := 10;
  decl_Extended.IntType := intExtended;
  AddStaticDeclaration(decl_Extended);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PExtended';
  Decl_Ptr.PointerType := decl_Extended;
  AddStaticDeclaration(Decl_Ptr);


  // OTHER

  decl_Boolean         := TRutisVarType.Create;
  decl_Boolean.Name    := 'Boolean';
  decl_Boolean.Size    := 1;
  decl_Boolean.IntType := intBoolean;
  AddStaticDeclaration(decl_Boolean);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PExtended';
  Decl_Ptr.PointerType := decl_Extended;
  AddStaticDeclaration(Decl_Ptr);


  decl_Enum            := TRutisVarType.Create;
  decl_Enum.Name       := '$Enum';
  decl_Enum.Size       := 1;
  decl_Enum.IntType    := intEnum;
  AddStaticDeclaration(decl_Enum);

  decl_Set4            := TRutisVarType.Create;
  decl_Set4.Name       := '$Set4';
  decl_Set4.Size       := 4;
  decl_Set4.IntType    := intSet;
  AddStaticDeclaration(decl_Set4);

  decl_Pointer         := TRutisVarType.Create;
  decl_Pointer.Name    := 'Pointer';
  decl_Pointer.Size    := 4;
  decl_Pointer.IntType := intPointer;
  AddStaticDeclaration(decl_Pointer);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PPointer';
  Decl_Ptr.PointerType := decl_Pointer;
  AddStaticDeclaration(Decl_Ptr);


  // STRINGS AND CHARS

  decl_ShortString         := TRutisVarType.Create;
  decl_ShortString.Name    := 'ShortString';
  decl_ShortString.Size    := 256;
  decl_ShortString.IntType := intShortString;
  AddStaticDeclaration(decl_ShortString);

  decl_PShortString      := TRutisPointerType.Create;
  decl_PShortString.Name := 'PShortString';
  decl_PShortString.PointerType := decl_ShortString;
  AddStaticDeclaration(decl_PShortString);


  decl_AChar         := TRutisVarType.Create;
  decl_AChar.Name    := 'AnsiChar';
  decl_AChar.Size    := 1;
  decl_AChar.IntType := intAChar;
  AddStaticDeclaration(decl_AChar);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'Char';
  Decl_Copy.Size := decl_AChar.size;
  Decl_Copy.CopyType := decl_AChar;
  AddStaticDeclaration(Decl_Copy);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PAnsiChar';
  Decl_Ptr.PointerType := decl_AChar;
  AddStaticDeclaration(Decl_Ptr);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'PChar';
  Decl_Copy.Size := Decl_Ptr.size;
  Decl_Copy.CopyType := Decl_Ptr;
  AddStaticDeclaration(Decl_Copy);


  decl_AString         := TRutisVarType.Create;
  decl_AString.Name    := 'AnsiString';
  decl_AString.Size    := 4;
  decl_AString.IntType := intAString;
  AddStaticDeclaration(decl_AString);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'String';
  Decl_Copy.Size := decl_AString.size;
  Decl_Copy.CopyType := decl_AString;
  AddStaticDeclaration(Decl_Copy);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PAnsiString';
  Decl_Ptr.PointerType := decl_AString;
  AddStaticDeclaration(Decl_Ptr);

  Decl_Copy      := TRutisTypeCopy.Create;
  Decl_Copy.Name := 'PString';
  Decl_Copy.Size := Decl_Ptr.size;
  Decl_Copy.CopyType := Decl_Ptr;
  AddStaticDeclaration(Decl_Copy);



  decl_WChar         := TRutisVarType.Create;
  decl_WChar.Name    := 'WideChar';
  decl_WChar.Size    := 1;
  decl_WChar.IntType := intWChar;
  AddStaticDeclaration(decl_WChar);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PWideChar';
  Decl_Ptr.PointerType := decl_WChar;
  AddStaticDeclaration(Decl_Ptr);


  decl_WString         := TRutisVarType.Create;
  decl_WString.Name    := 'WideString';
  decl_WString.Size    := 4;
  decl_WString.IntType := intWString;
  AddStaticDeclaration(decl_WString);

  Decl_Ptr      := TRutisPointerType.Create;
  Decl_Ptr.Name := 'PWideString';
  Decl_Ptr.PointerType := decl_WString;
  AddStaticDeclaration(Decl_Ptr);

  {$ENDREGION}

  {$REGION 'Spezial Types'}
  //==================================================================
  decl_Method         := TRutisVarType.Create;
  decl_Method.Name    := 'Method';
  decl_Method.Size    := 4;
  decl_Method.IntType := intMethod;
  AddStaticDeclaration(decl_Method);

  //==================================================================
  decl_ObjMethod         := TRutisVarType.Create;
  decl_ObjMethod.Name    := 'MethodOfObject';
  decl_ObjMethod.Size    := 8;
  decl_ObjMethod.IntType := intObjMethod;
  AddStaticDeclaration(decl_ObjMethod);

  //==================================================================
  Decl_Struct      := TRutisStructType.Create;
  Decl_Struct.Name := 'TClass_OBJDATA';
  AddStaticDeclaration(Decl_Struct);

  SetLength(Decl_Struct.StructTypes, 3);
  Decl_Struct.StructTypes[0]         := TRutisVarDecl.Create;
  Decl_Struct.StructTypes[0].VarType := decl_PShortString;
  Decl_Struct.StructTypes[0].Address := vmtClassName;
  Decl_Struct.StructTypes[0].isConst := False;
  Decl_Struct.StructTypes[0].Level   := 0;
  Decl_Struct.StructTypes[0].Name    := 'CLASSNAME';

  Decl_Struct.StructTypes[1]         := TRutisVarDecl.Create;
  Decl_Struct.StructTypes[1].VarType := decl_Cardinal;
  Decl_Struct.StructTypes[1].Address := vmtInstanceSize;
  Decl_Struct.StructTypes[1].isConst := False;
  Decl_Struct.StructTypes[1].Level   := 0;
  Decl_Struct.StructTypes[1].Name    := 'INSTANCESIZE';

  Decl_Struct.StructTypes[2]         := TRutisVarDecl.Create;
  Decl_Struct.StructTypes[2].VarType := decl_Cardinal;
  Decl_Struct.StructTypes[2].Address := vmtParent;
  Decl_Struct.StructTypes[2].isConst := False;
  Decl_Struct.StructTypes[2].Level   := 0;
  Decl_Struct.StructTypes[2].Name    := 'CLASSPARENT';

  //==================================================================
  decl_TClass              := TRutisPointerType.Create;
  decl_TClass.Name         := 'TClass';
  decl_TClass.PointerType  := Decl_Struct;
  AddStaticDeclaration(decl_TClass);

  //==================================================================
  Decl_Struct           := TRutisStructType.Create;
  Decl_Struct.Name      := 'TObject_CLASSDATA';
  Decl_Struct.Size      := 4;
  Decl_Struct.IsPacked  := False;
  AddStaticDeclaration(Decl_Struct);

  SetLength(Decl_Struct.StructTypes, 1);
  Decl_Struct.StructTypes[0]         := TRutisVarDecl.Create;
  Decl_Struct.StructTypes[0].VarType := decl_TClass;
  Decl_Struct.StructTypes[0].Address := 0;
  Decl_Struct.StructTypes[0].isConst := False;
  Decl_Struct.StructTypes[0].Level   := 0;
  Decl_Struct.StructTypes[0].Name    := 'CLASSTYPE';

  //==================================================================
  decl_TObject             := TRutisClassType.Create;
  decl_TObject.Name        := 'TObject';
  decl_TObject.ClassStruct := Decl_Struct;
  decl_TObject.IsExternal  := True;
  AddStaticDeclaration(decl_TObject);
  {$ENDREGION}

  {$REGION 'Test'}
(*  {$REGION 'Byte ConvertTable'}
  i := 0;
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_Byte.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_Byte.ConvertTable[i].ConvSeq, 1);
  decl_Byte.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Byte.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Byte.IntType);
  decl_Byte.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_Byte.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_Byte.ConvertTable, i+1);
  decl_Byte.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_Byte.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'Word ConvertTable'}
  i := 0;
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_Word.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_Word.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_Word.ConvertTable, i+1);
  decl_Word.ConvertTable[i].ConvType      := decl_AnsiChar;
  SetLength(decl_Word.ConvertTable[i].ConvSeq, 1);
  decl_Word.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Word.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Word.IntType);
  decl_Word.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_AnsiChar.IntType);
  decl_Word.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'Cardinal ConvertTable'}
  i := 0;
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_Cardinal.ConvertTable[i].ConvSeq, 1);
  decl_Cardinal.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_Cardinal.IntType);
  decl_Cardinal.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_Cardinal.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_Cardinal.ConvertTable, i+1);
  decl_Cardinal.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_Cardinal.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'XXXX ConvertTable'}
  i := 0;
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'XXXX ConvertTable'}
  i := 0;
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'XXXX ConvertTable'}
  i := 0;
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'XXXX ConvertTable'}
  i := 0;
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  {$REGION 'XXXX ConvertTable'}
  i := 0;
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Byte;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Byte.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Word;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Word.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Cardinal;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Cardinal.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_ShortInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_ShortInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_SmallInt;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_SmallInt.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Integer;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Integer.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Single;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Single.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Double;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Double.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Extended;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Extended.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := false;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_Boolean;
  SetLength(decl_XXXX.ConvertTable[i].ConvSeq, 1);
  decl_XXXX.ConvertTable[i].ConvSeq[0]    := CONV_CMD;
  decl_XXXX.ConvertTable[i].ConvSeq[0].P1 := Integer(decl_XXXX.IntType);
  decl_XXXX.ConvertTable[i].ConvSeq[0].P2 := Integer(decl_Boolean.IntType);
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  SetLength(decl_XXXX.ConvertTable, i+1);
  decl_XXXX.ConvertTable[i].ConvType      := decl_AnsiChar;
  decl_XXXX.ConvertTable[i].Explicit      := true;
  inc(i);
  {$ENDREGION}
  *)
  {$ENDREGION}
End;

Destructor TRutisEngineBase.Destroy;
Var i  : Integer;
Begin
  For i := 0 To high(StaticDeclarations) Do
    StaticDeclarations[i].Free;
  SetLength(StaticDeclarations, 0);

  FreeAndNil(ScriptData);
  FreeAndNil(Compiler);
  FreeAndNil(ScriptCode);
  FreeAndNil(fCompilerError);

  Inherited;
End;

Procedure TRutisEngineBase.ClearAll;
Begin
  ScriptData.Clear;
  ScriptData.Stack.Clear;
End;

//==============================================================================

Procedure TRutisEngineBase.ScriptMessage(Msg : String; ErrorType : TRutisErrorType = etHint);
Begin
  Msg := 'Runtime Error: (' + ScriptData.ScriptName + ') ' + Msg;
  fScriptError := fScriptError or (ErrorType = etRuntimeError);

  {$ifdef DEBUG}OutputDebugString(PChar(Msg));{$endif}
  If Assigned(OnError) Then
    OnError(Msg, ErrorType)
  Else
    {$ifdef WINCE}
    MessageBox(0, PWideChar(Msg), 'Error!', 0);
    {$else WINCE}
    MessageBox(0, PChar(Msg), 'Error!', 0);
    {$endif}
End;

Procedure TRutisEngineBase.AddStaticDeclaration(Decl : TRutisDecl);
Begin
  If Decl = nil Then exit;
  SetLength(StaticDeclarations, length(StaticDeclarations) + 1);
  StaticDeclarations[high(StaticDeclarations)] := Decl;
  Decl.Name := UpperCase(Decl.Name);
  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngineBase.AddStaticDeclaration  -  Added Declaration : ' + Decl.Name));{$endif}
End;

//==============================================================================

Procedure TRutisEngineBase.RegExtMethodV(AName : String; AMethod : TExtVariMethod; AParams : Array Of String; AResult : String; ADescription : String = '');
Var
  i          : Integer;
  Dat        : TRutisExtMethodType;
  MethodVar  : TRutisVarDecl;
  DoOverload  : Boolean;
Begin
  DoOverload := False;
  Dat        := TRutisExtMethodType(FindDeclaration(AName, @StaticDeclarations));
  If Dat <> nil Then
  Begin
    If Dat is TRutisExtMethodType Then
    Begin
      Dat.Overloaded := True;
      DoOverload     := True;
    End
    Else
      exit;
  End;

  Dat := TRutisExtMethodType.Create;
  With Dat Do
  Begin
    Name := '$ExtVariantMethod';
    Description       := ADescription;
    Method_Name       := AName;
    VariMethod        := AMethod;
    StackMethod       := nil;
    ParamsSize        := 0;
    Overloaded        := DoOverload;
    MethodResult.Name := AResult;
    IsFunction        := AResult <> '';

    SetLength(Params, Length(AParams));
    For i := 0 To high(AParams) Do
    Begin
      Params[i].Name       := AParams[i];
      Params[i].isVarParam := False;

      {id := FindDeclarationID(AParams[i], @StaticDeclarations);
      If (id < 0) or
        (not (StaticDeclarations[id] is TRutisTypeDecl)) Then
      Begin
        Dat.Free;
        exit;
      End;
      Params[i].TypeData := TRutisTypeDecl(StaticDeclarations[id]);
      ParamsSize := ParamsSize + TRutisTypeDecl(StaticDeclarations[id]).Size; }
    End;
    {
    If IsFunction Then
    Begin
      id         := FindDeclarationID(AResult, @StaticDeclarations);
      If (id < 0) or
      //false then
        (not (StaticDeclarations[id] is TRutisTypeDecl)) Then
      Begin
        Dat.Free;
        exit;
      End;
      ResultType := TRutisTypeDecl(StaticDeclarations[id]); 
    End;
    }
  End;

  MethodVar         := TRutisVarDecl.Create;
  MethodVar.Name    := UpperCase(AName);
  MethodVar.IsConst := True;
  MethodVar.Level   := 0;
  MethodVar.VarType := dat;

  SetLength(StaticDeclarations, length(StaticDeclarations) + 1);
  StaticDeclarations[high(StaticDeclarations)] := MethodVar;

  SetLength(StaticDeclarations, length(StaticDeclarations) + 1);
  StaticDeclarations[high(StaticDeclarations)] := dat;

  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngine.RegExtMethod  -  Added Ext-Variant-Method : ' + AName));{$endif}
End;

Procedure TRutisEngineBase.RegExtMethod(AName : String; AMethod : TExtStackMethod; AParams : Array Of String; AResult : String; ADescription : String = '');
Var
  i          : Integer;
  Dat        : TRutisExtMethodType;
  MethodVar  : TRutisVarDecl;
  DoOverload  : Boolean;
Begin
  DoOverload := False;
  Dat        := TRutisExtMethodType(FindDeclaration(AName, @StaticDeclarations));
  If Dat <> nil Then
  Begin
    If Dat is TRutisExtMethodType Then
    Begin
      Dat.Overloaded := True;
      DoOverload     := True;
    End
    Else
      exit;
  End;

  Dat := TRutisExtMethodType.Create;
  With Dat Do
  Begin
    Name := '$ExtMethod';
    Description       := ADescription;
    Method_Name       := AName;
    VariMethod        := nil;
    StackMethod       := AMethod;
    ParamsSize        := 0;
    Overloaded        := DoOverload;
    MethodResult.Name := AResult;
    IsFunction        := AResult <> '';

    SetLength(Params, Length(AParams));
    For i := 0 To high(AParams) Do
    Begin
      Params[i].Name       := AParams[i];
      Params[i].isVarParam := False;
      {id := FindDeclarationID(AParams[i], @StaticDeclarations);
      If (id < 0) or
        (not (StaticDeclarations[id] is TRutisTypeDecl)) Then
      Begin
        Dat.Free;
        exit;
      End;
      Params[i].TypeData := TRutisTypeDecl(StaticDeclarations[id]);
      ParamsSize := ParamsSize + TRutisTypeDecl(StaticDeclarations[id]).Size; }
    End;

    {
    If IsFunction Then
    Begin
      id         := FindDeclarationID(AResult, @StaticDeclarations);
      If (id < 0) or
      //false then
        (not (StaticDeclarations[id] is TRutisTypeDecl)) Then
      Begin
        Dat.Free;
        exit;
      End;
      ResultType := TRutisTypeDecl(StaticDeclarations[id]); 
    End;
    }
  End;

  MethodVar         := TRutisVarDecl.Create;
  MethodVar.Name    := UpperCase(AName);
  MethodVar.IsConst := True;
  MethodVar.Level   := 0;
  MethodVar.VarType := dat;

  SetLength(StaticDeclarations, length(StaticDeclarations) + 1);
  StaticDeclarations[high(StaticDeclarations)] := MethodVar;

  SetLength(StaticDeclarations, length(StaticDeclarations) + 1);
  StaticDeclarations[high(StaticDeclarations)] := dat;

  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngine.RegExtMethod  -  Added Ext-Stack-Method : ' + AName));{$endif}
End;

//==============================================================================
//==============================================================================

Function TRutisEngineBase.GetStackBase : Integer;
Begin
  Result := 0;
  If length(ScriptData.CallStack) = 0 Then exit;
  Result := ScriptData.CallStack[high(ScriptData.CallStack)].StackBase;
End;

Function TRutisEngineBase.CheckStackIndex(Address : Integer) : Boolean;
Begin
  Result := (Address < 0) or (Address > ScriptData.Stack.Top);
  If Result Then
    ScriptMessage('Address Error (Address ID = ' + IntToStr(Address) + ')');
End;

Function TRutisEngineBase.GetStackLvlAddress(Address, Level : Integer) : Integer;
Var
  i  : Integer;
Begin
  If (Level <= 0) Then
    If Address < 0 Then
      Result := ScriptData.Stack.Top + Address
    Else
      Result := Address
  Else
  Begin
    i := high(ScriptData.CallStack);
    If i < 0 Then
    Begin
      Result := -1;
      exit;
    End;
    While ScriptData.CallStack[i].Level > Level Do
      Dec(i);
    Result := ScriptData.CallStack[i].StackBase + Address;
  End;
End;

//==============================================================================

Function TRutisEngineBase.GetCompilerError : Boolean;
Begin
  Result := fCompilerError <> nil;
End;

//==============================================================================

Procedure TRutisEngineBase.CompilerMessage(Msg : String; ErrorCode : TRutisErrorType = etHint);
Begin
  If Assigned(OnError) Then
    OnError(Msg, ErrorCode)
  Else
    {$ifdef WINCE}
    MessageBox(0, PWideChar(Msg), 'Error!', 0);
    {$else WINCE}
    MessageBox(0, PChar(Msg), 'Error!', 0);
    {$endif}
End;

//==============================================================================

Procedure TRutisEngineBase.Compile;
Var
  i    : Integer;
  Msg  : String;
Begin
  If Compiler = nil Then exit;

  Compiler.ScriptCode := ScriptCode;
  UnitFileManager.AddPath(ScriptFilePath);

  ScriptData.Clear;

  For i := 0 To high(StaticDeclarations) Do
    ScriptData.AddDeclaration(StaticDeclarations[i], '$STATIC');

  FreeAndNil(fCompilerError);
  Try
    Compiler.Compile;
  Except
    On E : ERutisCompilerError Do
    Begin
      SetLength(ScriptData.Code, 0);

      fCompilerError := ERutisCompilerError.Create('');
      fCompilerError.Assign(E);

      Msg := Format('Error: %s (%d; %d): %s', [E.ENamespace, E.ELine + 1, E.EChrPos, E.Message]);
      //Msg := Format('Error: %s : %s', [E.ENamespace, E.Message]);
      CompilerMessage(Msg, etCompilerError);
    End;
    //on E: EAccessViolation do
    //  CompilerMessage(Format('Error: Access Violation at %p', [E.ExceptionRecord.ExceptionAddress]), etCompilerError);
    Else
    Begin
      SetLength(ScriptData.Code, 0);

      Raise;
      {fCompilerError := ERutisCompilerError.Create('');
      fCompilerError.Assign(E);

      Msg := Format('Error: %s (%d; %d): %s', [E.ENamespace, E.ELine + 1, E.EChrPos, E.Message]);
      //Msg := Format('Error: %s : %s', [E.ENamespace, E.Message]);
      CompilerMessage(Msg, 0);}
    End;
  End;
       
  {$ifdef DEBUG}OutputDebugString('TRutisEngineBase.Compile  -  Finished compiling');{$endif}

  fCompilerLine := ScriptData.CompilerLine;
End;

Procedure TRutisEngineBase.Compile(AScriptCode : TStrings);
Begin
  If Compiler = nil Then exit;
  ScriptCode.Clear;
  ScriptCode.AddStrings(AScriptCode);

  Compile;
End;

Procedure TRutisEngineBase.Compile(FileName : String);
Begin
  LoadFromFile(FileName);
  Compile;
End;

//==============================================================================
//==============================================================================

Procedure TRutisEngineBase.LoadFromStream(Stream : TStream; CanReadProtected : Boolean = False);
Var
  ln         : String;
  ProtCode   : Integer;
  StreamPos  : Int64;
Begin
  StreamPos := Stream.Position;
  If StreamReadString(Stream) = 'Compiled RUTIS-Engine Script - (c) Björn Zeutzheim' Then
  Begin
    Stream.Position := StreamPos;
    LoadBCodeFromStream(Stream);
    exit;
  End;
  Stream.Position := StreamPos;
  ScriptCode.LoadFromStream(Stream);
  If ScriptCode.Count <= 0 Then exit;

  ln := ScriptCode.Strings[0];
  If ln = 'protego' Then
  Begin
    If not CanReadProtected Then
    Begin
      ScriptCode.Clear;
      exit;
    End;
    ln       := ScriptCode.Strings[1];
    ln       := copy(ln, 1, pos(' ', ln) - 1);
    ProtCode := StrToInt(ln);

    ScriptCode.Delete(0);
    ScriptCode.Delete(0);

    CodeUnProtect(ProtCode);
  End;
End;

//==============================================================================

Procedure TRutisEngineBase.LoadScriptFromStream(Stream : TStream; CanReadProtected : Boolean = False);
Var
  ln        : String;
  ProtCode  : Integer;
Begin
  ScriptCode.LoadFromStream(Stream);
  If ScriptCode.Count <= 0 Then exit;

  ln := ScriptCode.Strings[0];
  If ln = 'protego' Then
  Begin
    If not CanReadProtected Then
    Begin
      ScriptCode.Clear;
      exit;
    End;
    ln       := ScriptCode.Strings[1];
    ln       := copy(ln, 1, pos(' ', ln) - 1);
    ProtCode := StrToInt(ln);

    ScriptCode.Delete(0);
    ScriptCode.Delete(0);

    CodeUnProtect(ProtCode);
  End;
End;

Procedure TRutisEngineBase.SaveScriptToStream(Stream : TStream; SaveProtected : Boolean = False);
Var CodeSave  : TStringList;
Begin
  If SaveProtected Then
  Begin
    CodeSave := TStringList.Create;
    CodeSave.Assign(ScriptCode);
    Try
      CodeProtego(random(255));
      ScriptCode.SaveToStream(Stream);
    Finally
      ScriptCode.Assign(CodeSave);
      CodeSave.Free;
    End;
  End
  Else
    ScriptCode.SaveToStream(Stream);
End;

Procedure TRutisEngineBase.CodeProtego(val : Integer);
Var
  ln, ln2  : String;
  i        : Integer;
Begin
  ln := ScriptCode.Text;

  ln  := ln + ' ';
  ln2 := '';
  For I := 1 To length(ln) - 1 Do
    If (not ((ln[i] = ' ') and (ln[i + 1] = ' '))) and (ln[i] <> #13) Then
      ln2 := ln2 + ln[i];

  {ln := '';
  for I := 0 to ScriptCode.Count - 1 do
    ln := ln + ScriptCode[i];   }

  ScriptCode.Text := ln2;

  CodeProtect(val);
  ScriptCode.Insert(0, IntToStr(val) + '  ');
  ScriptCode.Insert(0, 'protego');
End;

Procedure TRutisEngineBase.CodeProtect(val : Integer);
Var
  i   : Integer;
  ln  : String;
Begin
  ln := ScriptCode.Text;
  If length(ln) > 4 Then
  Begin
    If (ln[length(ln) - 1] = #13) and (ln[length(ln)] = #10) Then
      ln := copy(ln, 1, length(ln) - 2);
  End;
  For I := 1 To length(ln) Do
  Begin
    ln[i] := Chr(Ord(ln[i]) + val);
  End;
  ScriptCode.Text := ln;
End;

Procedure TRutisEngineBase.CodeUnProtect(val : Integer);
Begin
  CodeProtect(-val);
End;

//==============================================================================

Procedure TRutisEngineBase.LoadBCodeFromStream(Stream : TStream);
Var
  i, j, k      : Integer;
  IsExtMethod  : Boolean;
  DllMethod    : TRutisDllMethodType;
  TempSize     : Word;
  TempStr      : String;
  IntType      : TRutisIntType;
Begin
  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngine.LoadCodeFromStream  -  Loading compiled Script'));{$endif}
  If StreamReadString(Stream) <> 'Compiled RUTIS-Engine Script - (c) Björn Zeutzheim' Then exit;

  ScriptCode.Clear;
  ClearAll;

  ScriptData.ScriptName := StreamReadString(Stream);

  //===========================================
  If StreamReadString(Stream) <> '#CODEDATA#' Then exit;
  Stream.Read(i, sizeof(i));
  SetLength(ScriptData.Code, i);
  For i := 0 To high(ScriptData.Code) Do
    Stream.Read(ScriptData.Code[i], sizeof(TRutisScriptCmd));

  //===========================================
  If StreamReadString(Stream) <> '#METHODINFOTABLE#' Then exit;
  Stream.Read(i, sizeof(i));
  SetLength(ScriptData.MethodTable, i);
  For i := 0 To high(ScriptData.MethodTable) Do
  Begin
    Stream.Read(IsExtMethod, sizeof(IsExtMethod));
    If IsExtMethod Then
    Begin
      TempStr := StreamReadString(Stream);
      For j := 0 To high(StaticDeclarations) Do
        If StaticDeclarations[j] is TRutisExtMethodType Then
          If LowerCase(TRutisExtMethodType(StaticDeclarations[j]).Method_Name) = LowerCase(TempStr) Then
          Begin
            ScriptData.MethodTable[i] := TRutisExtMethodType(StaticDeclarations[j]);
            With ScriptData.MethodTable[i] Do
            Begin
              MethodTableID := i;
              ParamsSize := 0;
              Stream.Read(MethodResult.Size, SizeOf(MethodResult.Size));
              Stream.Read(MethodResult.InternalType, SizeOf(MethodResult.InternalType));
              Stream.Read(k, SizeOf(k));
              SetLength(Params, k);
              For k := 0 To high(Params) Do
              Begin
                Stream.Read(Params[k].Size, SizeOf(Params[k].Size));
                Stream.Read(Params[k].InternalType, SizeOf(Params[k].InternalType));
                ParamsSize := ParamsSize + Params[k].Size;
              End;
            End;
            break;
          End;
    End
    Else
    Begin
      DllMethod         := TRutisDllMethodType.Create;
      ScriptData.AddDeclaration(DllMethod, '$SYSTEM');
      ScriptData.MethodTable[i] := DllMethod;
      DllMethod.MethodTableID := i;
      DllMethod.DllName := StreamReadString(Stream);
      DllMethod.ProcName := StreamReadString(Stream);
      Stream.Read(DllMethod.ParamsSize, SizeOf(Word));
      Stream.Read(DllMethod.IsFunction, SizeOf(Boolean));

      Stream.Read(j, sizeof(j));
      SetLength(DllMethod.Params, j);
      For j := 0 To high(DllMethod.Params) Do
      Begin
        Stream.Read(TempSize, SizeOf(TempSize));
        Stream.Read(IntType, SizeOf(IntType));
        DllMethod.Params[j].TypeData      := TRutisVarType.Create;
        ScriptData.AddDeclaration(DllMethod.Params[j].TypeData, '$SYSTEM');
        DllMethod.Params[j].TypeData.Size := TempSize;
        TRutisVarType(DllMethod.Params[j].TypeData).IntType := IntType;
      End;
      Stream.Read(TempSize, SizeOf(TempSize));
      Stream.Read(IntType, SizeOf(IntType));
      If TempSize > 0 Then
      Begin
        DllMethod.MethodResult.TypeData      := TRutisVarType.Create;
        ScriptData.AddDeclaration(DllMethod.MethodResult.TypeData, '$SYSTEM');
        DllMethod.MethodResult.TypeData.Size := TempSize;
        TRutisVarType(DllMethod.MethodResult.TypeData).IntType := IntType;
      End;
    End;
  End;

  //===========================================
  If StreamReadString(Stream) <> '#RESOURCES#' Then exit;

  ScriptData.Ressources.Free;
  Stream.Read(i, 4);
  ScriptData.Ressources := TRutisStack.Create(i);

  Stream.Read(i, 4);
  SetLength(ScriptData.Ressources.Blocks, i);
  With ScriptData.Ressources Do
    For i := 0 To high(Blocks) Do
    Begin
      If Blocks[i].Data = nil Then
        Blocks[i].Data := AllocMem(StackBlockSize);
      Stream.Read(Blocks[i].Data^, StackBlockSize);
      Stream.Read(Blocks[i].Top, 4);
    End;
  ScriptData.Ressources.TopBlock := high(ScriptData.Ressources.Blocks);
  Stream.Read(ScriptData.Ressources.Top, sizeof(ScriptData.Ressources.Top));
  Stream.Read(ScriptData.Ressources.DecTop, sizeof(ScriptData.Ressources.DecTop));

  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngine.SaveCodeToStream  -  Loaded compiled Script'));{$endif}
End;

Procedure TRutisEngineBase.SaveBCodeToStream(Stream : TStream);
Var
  i, j         : Integer;
  IsExtMethod  : Boolean;
  DllMethod    : TRutisDllMethodType;
  IntType      : TRutisIntType;
Begin
  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngine.SaveCodeToStream  -  Saving compiled Script'));{$endif}
  StreamWriteString('Compiled RUTIS-Engine Script - (c) Björn Zeutzheim', Stream);

  StreamWriteString(ScriptData.ScriptName, Stream);

  //===========================================
  StreamWriteString('#CODEDATA#', Stream);
  i := length(ScriptData.Code);
  Stream.Write(i, sizeof(i));
  For i := 0 To high(ScriptData.Code) Do
    Stream.Write(ScriptData.Code[i], sizeof(TRutisScriptCmd));

  //===========================================
  StreamWriteString('#METHODINFOTABLE#', Stream);
  i := length(ScriptData.MethodTable);
  Stream.Write(i, sizeof(i));
  For i := 0 To high(ScriptData.MethodTable) Do
  Begin
    IsExtMethod := (ScriptData.MethodTable[i] is TRutisExtMethodType);
    Stream.Write(IsExtMethod, sizeof(IsExtMethod));
    If IsExtMethod Then
      With ScriptData.MethodTable[i] Do
      Begin
        StreamWriteString(Method_Name, Stream);
        Stream.Write(MethodResult.Size, SizeOf(MethodResult.Size));
        Stream.Write(MethodResult.InternalType, SizeOf(MethodResult.InternalType));
        j := length(Params);
        Stream.Write(j, SizeOf(j));
        For j := 0 To high(Params) Do
        Begin
          Stream.Write(Params[j].Size, SizeOf(Params[j].Size));
          Stream.Write(Params[j].InternalType, SizeOf(Params[j].InternalType));
        End;
      End
    Else
    Begin
      DllMethod := TRutisDllMethodType(ScriptData.MethodTable[i]);
      StreamWriteString(DllMethod.DllName, Stream);
      StreamWriteString(DllMethod.ProcName, Stream);
      Stream.Write(DllMethod.ParamsSize, SizeOf(Word));
      Stream.Write(DllMethod.IsFunction, SizeOf(Boolean));
      j := length(DllMethod.Params);
      Stream.Write(j, sizeof(j));
      For j := 0 To high(DllMethod.Params) Do
      Begin
        Stream.Write(DllMethod.Params[j].TypeData.Size, SizeOf(Word));
        IntType := DllMethod.Params[j].TypeData.InternalType;
        Stream.Write(IntType, SizeOf(IntType));
      End;
      Stream.Write(DllMethod.MethodResult.TypeData.Size, SizeOf(Word));
      IntType := DllMethod.MethodResult.TypeData.InternalType;
      Stream.Write(IntType, SizeOf(IntType));
    End;
  End;

  //===========================================
  StreamWriteString('#RESOURCES#', Stream);
  Stream.Write(ScriptData.Ressources.StackBlockSize, 4);
  i := length(ScriptData.Ressources.Blocks);
  Stream.Write(i, 4);
  With ScriptData Do
    For i := 0 To high(ScriptData.Ressources.Blocks) Do
    begin
      Stream.Write(ScriptData.Ressources.Blocks[i].Data^, ScriptData.Ressources.StackBlockSize);
      Stream.Write(ScriptData.Ressources.Blocks[i].Top, 4);
    end;
  Stream.Write(ScriptData.Ressources.Top, sizeof(ScriptData.Ressources.Top));
  Stream.Write(ScriptData.Ressources.DecTop, sizeof(ScriptData.Ressources.DecTop));

  {$ifdef DEBUG}OutputDebugString(PChar('TRutisEngine.SaveCodeToStream  -  Saved compiled Script'));{$endif}
End;

//==============================================================================

Procedure TRutisEngineBase.LoadFromFile(FileName : String; CanReadProtected : Boolean = False);
Var
  FS  : TFileStream;
Begin
  If not FileExists(FileName) Then exit;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(FS, CanReadProtected);
  Finally
    FS.Free;
  End;
End;

Procedure TRutisEngineBase.LoadScriptFromFile(FileName : String; CanReadProtected : Boolean = False);
Var
  FS  : TFileStream;
Begin
  If not FileExists(FileName) Then exit;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadScriptFromStream(FS, CanReadProtected);
  Finally
    FS.Free;
  End;
End;

Procedure TRutisEngineBase.SaveScriptToFile(FileName : String; SaveProtected : Boolean = False);
Var
  FS  : TFileStream;
Begin
  FS := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  Try
    SaveScriptToStream(FS, SaveProtected);
  Finally
    FS.Free;
  End;
End;

Procedure TRutisEngineBase.LoadBCodeFromFile(FileName : String);
Var
  FS  : TFileStream;
Begin
  If not FileExists(FileName) Then exit;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadBCodeFromStream(FS);
  Finally
    FS.Free;
  End;
End;

Procedure TRutisEngineBase.SaveBCodeToFile(FileName : String);
Var
  FS  : TFileStream;
Begin
  FS := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  Try
    SaveBCodeToStream(FS);
  Finally
    FS.Free;
  End;
End;

//==============================================================================
//==============================================================================

 { TRutisCompiler }

Constructor TRutisCompiler.Create(AOwner : TRutisEngineBase);
Begin
  Owner := AOwner;

  optArrayRangeCheck := True;
  optArrangeFields   := True;
  optArrangeSize     := 4;

  Owner.UnitFileManager.AddExtension('.rutis');
  //Owner.UnitFileManager.AddExtension('.pas');
End;

//==============================================================================

Procedure TRutisCompiler.Compile;
Begin
  ScriptData := Owner.ScriptData;
  ScriptData.CompilerLine := 0;
  ScriptData.CodeInsertPos := -1;
End;

Procedure TRutisCompiler.Optimize;
Var
  i    : Integer;
//  j,k : Integer;
//  sng  : Single;
Begin
  i := 0;
  //exit;
  While i <= high(ScriptData.Code) Do
  Begin

    If (i >= 0) and (i < high(ScriptData.Code)) and
      (ScriptData.Code[i].Cmd = _jmp) and
      (ScriptData.Code[i].P1 = i + 1) Then
    Begin
      ScriptData.DeleteInstruction(i);
      Dec(i);
      Continue;
    End;

    {
    //There are Problems with In ... then structs
    //If this optimizatiion occurs at the end of an If-Block with the last
    //Action of the If and he first after the IF, the Stack changes size
    If (i > 0) and (i < high(ScriptData.Code)) and
       (ScriptData.Code[i-1].Cmd = _sto) and (ScriptData.Code[i].Cmd = _lod) and
       (ScriptData.Code[i-1].P1  = ScriptData.Code[i].P1) and
       (ScriptData.Code[i-1].P2  = ScriptData.Code[i].P2) and
       (ScriptData.Code[i-1].P3  = ScriptData.Code[i].P3) then
    begin
      ScriptData.Code[i-1].Cmd := _Mov;
      ScriptData.DeleteInstruction(i);
      dec(i);
      Continue;
    end;
    //}

    If (i >= 0) and (i < high(ScriptData.Code) - 2) and
      (ScriptData.Code[i].Cmd = _gen4) and
      (ScriptData.Code[i].P1 = 0) and
      (ScriptData.Code[i + 1].Cmd = _gen4) and
      (ScriptData.Code[i + 2].Cmd = _sub) Then
    Begin
      If TRutisIntType(ScriptData.Code[i + 2].P1) = intInteger Then
        ScriptData.Code[i + 1].P1 := -ScriptData.Code[i + 1].P1;
      If TRutisIntType(ScriptData.Code[i + 2].P1) = intSingle Then
        PSingle(@ScriptData.Code[i + 1].P1)^ := -PSingle(@ScriptData.Code[i + 1].P1)^;
      //Delete [gen4 0]
      ScriptData.DeleteInstruction(i);
      //Delete [sub]
      ScriptData.DeleteInstruction(i + 1);
      Dec(i);
      Continue;
    End;
    //}

    {
    //BUG with SetLength(TestArray[Index], Count)
    If (i > 0) and
       (ScriptData.Code[i - 1].Cmd = _lod) and
       (ScriptData.Code[i].Cmd = _lodp) and
       (ScriptData.Code[i].P1 = -4) and
       (ScriptData.Code[i].P2 = -1) Then
    Begin
      ScriptData.Code[i - 1].Cmd := _lodp;
      ScriptData.Code[i - 1].P3  := ScriptData.Code[i].P3;
      ScriptData.DeleteInstruction(i);
      Dec(i);
      Continue;
    End;
    //}
    {
    //Not needed any more
    If (ScriptData.Code[i].Cmd   = _gen) and
       (ScriptData.Code[i+1].Cmd = _gen) and
       (ScriptData.Code[i+1].P1  = ScriptData.Code[i].P1) and
       (ScriptData.Code[i+1].P2  = ScriptData.Code[i].P2) then
    begin
      j := 1;
      k := ScriptData.Code[i+1].P3;
      while (ScriptData.Code[i+j+1].Cmd = _gen) and
            (ScriptData.Code[i+j+1].P1  = ScriptData.Code[i].P1) and
            (ScriptData.Code[i+j+1].P2  = ScriptData.Code[i].P2) do
      begin
        k := k + ScriptData.Code[i+j+1].P3;
        inc(j);
      end;
      ScriptData.Code[i].P3 := ScriptData.Code[i].P3+k;
      for k := i+1 to i+j do
        ScriptData.DeleteInstruction(i+1);
      i := i + j;
    end;
    //}

    Inc(i);
  End;
End;

//==============================================================================
End.

