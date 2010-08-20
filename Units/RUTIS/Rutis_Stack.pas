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

Unit Rutis_Stack;

Interface

{$i Delphi_Versions.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

Uses
  SysUtils;

 //====================================================================
 //====================================================================

Type
  {$ifndef DELPHI_7_UP}
  PInteger = ^Integer;
  TVarType = Word;
  {$endif}

  PPAnsiString = ^PAnsiString;
  RutisString  = Pointer;
  PRutisString = ^RutisString;

  {$IFDEF FPC}
  PBoolean = ^Boolean;
  {$ENDIF}

//====================================================================
Type
  TRutisStackBlock = Record
    Top   : Integer;
    Data  : PByte;
  End;
                      
  TRutisStackFreeInfo = Record
    Adr    : Integer;
    IsStr  : Boolean;
  End;

  TRutisBasicStack = Class
  Private
    fStackBlockSize : Integer;
    Function GetData(Index : Integer) : Pointer;
  Public
    Blocks    : Array Of TRutisStackBlock;
    TopBlock  : Integer;
    Top       : Integer;
    DecTop    : Integer;
    FreeInfo  : Array Of TRutisStackFreeInfo;
    Property Data[Index  : Integer]  : Pointer Read GetData;

    Procedure Copy(Src, Dst, Size : Integer);
    Function Push(Size : Integer; SetZero : Boolean = False) : Integer;
    Procedure Pop(Size : Integer);
    Procedure PopTo(Size : Integer);

    {$REGION 'Get-Var-Pointer functions'}
    Function GetByte(Const adr : Integer) : PByte; Inline;
    Function GetWord(Const adr : Integer) : PWord; Inline;
    Function GetCardinal(Const adr : Integer) : PCardinal; Inline;
    Function GetShortInt(Const adr : Integer) : PShortInt; Inline;
    Function GetSmallInt(Const adr : Integer) : PSmallInt; Inline;
    Function GetInteger(Const adr : Integer) : PInteger; Inline;
    Function GetInt64(Const adr : Integer) : PInt64; Inline;
    Function GetSingle(Const adr : Integer) : PSingle; Inline;
    Function GetDouble(Const adr : Integer) : PDouble; Inline;
    Function GetExtended(Const adr : Integer) : PExtended; Inline;
    Function GetAChar(Const adr : Integer) : PAnsiChar; Inline;
    Function GetWChar(Const adr : Integer) : PWideChar; Inline;
    Function GetString(Const adr : Integer) : PRutisString; Inline;
    Function GetShortString(Const adr : Integer) : PShortString; Inline;
    {$ENDREGION}

    {$REGION 'Read functions'}
    Function ReadByte(Const adr : Integer) : Byte; Inline;
    Function ReadWord(Const adr : Integer) : Word; Inline;
    Function ReadCardinal(Const adr : Integer) : Cardinal; Inline;
    Function ReadShortInt(Const adr : Integer) : Shortint; Inline;
    Function ReadSmallInt(Const adr : Integer) : Smallint; Inline;
    Function ReadInteger(Const adr : Integer) : Integer; Inline;
    Function ReadInt64(Const adr : Integer) : Int64; Inline;
    Function ReadSingle(Const adr : Integer) : Single; Inline;
    Function ReadDouble(Const adr : Integer) : Double; Inline;
    Function ReadExtended(Const adr : Integer) : Extended; Inline;
    Function ReadAChar(Const adr : Integer) : AnsiChar; Inline;
    Function ReadWChar(Const adr : Integer) : Widechar; Inline;
    Function ReadString(Const adr : Integer) : RutisString; Inline;
    Function ReadShortString(Const adr : Integer) : ShortString; Inline;
    {$ENDREGION}

    {$REGION 'Write functions'}
    Procedure WriteByte(Const adr : Integer; Const Val : Byte); Inline;
    Procedure WriteWord(Const adr : Integer; Const Val : Word); Inline;
    Procedure WriteCardinal(Const adr : Integer; Const Val : Cardinal); Inline;
    Procedure WriteShortInt(Const adr : Integer; Const Val : Shortint); Inline;
    Procedure WriteSmallInt(Const adr : Integer; Const Val : Smallint); Inline;
    Procedure WriteInteger(Const adr : Integer; Const Val : Integer); Inline;
    Procedure WriteInt64(Const adr : Integer; Const Val : Int64); Inline;
    Procedure WriteSingle(Const adr : Integer; Const Val : Single); Inline;
    Procedure WriteDouble(Const adr : Integer; Const Val : Double); Inline;
    Procedure WriteExtended(Const adr : Integer; Const Val : Extended); Inline;
    Procedure WriteAChar(Const adr : Integer; Const Val : AnsiChar); Inline;
    Procedure WriteWChar(Const adr : Integer; Const Val : Widechar); Inline;
    Procedure WriteString(Const adr : Integer; Const Val : RutisString); Inline;
    Procedure WriteShortString(Const adr : Integer; Const Val : ShortString); Inline;
    {$ENDREGION}

    {$REGION 'Push functions'}
    Procedure PushByte(Const Val : Byte);
    Procedure PushWord(Const Val : Word);
    Procedure PushCardinal(Const Val : Cardinal);
    Procedure PushShortInt(Const Val : Shortint);
    Procedure PushSmallInt(Const Val : Smallint);
    Procedure PushInteger(Const Val : Integer);
    Procedure PushInt64(Const Val : Int64);
    Procedure PushSingle(Const Val : Single);
    Procedure PushDouble(Const Val : Double);
    Procedure PushExtended(Const Val : Extended);
    Procedure PushAChar(Const Val : AnsiChar);
    Procedure PushWChar(Const Val : Widechar);
    Procedure PushString(Const Val : RutisString; Const AutoFree : Boolean);
    Procedure PushAStringData(Const Val : Ansistring);
    Procedure PushShortString(Const Val : ShortString);
    {$ENDREGION}

    {$REGION 'Pop functions'}
    Function PopByte : Byte;
    Function PopWord : Word;
    Function PopCardinal : Cardinal;
    Function PopShortInt : Shortint;
    Function PopSmallInt : Smallint;
    Function PopInteger : Integer;
    Function PopInt64 : Int64;
    Function PopSingle : Single;
    Function PopDouble : Double;
    Function PopExtended : Extended;
    Function PopAChar : AnsiChar;
    Function PopWChar : Widechar;
    Function PopString : RutisString;
    Function PopShortString : ShortString;
    {$ENDREGION}

    Procedure AddFreeData(Adr : Integer; IsStr : Boolean);
    Procedure Update_FreeData;
    Function PointerInBlockData(p : Pointer) : Boolean;

    Procedure Clear;
    Constructor Create(AStackBlockSize : Cardinal);
    Destructor Destroy; Override;
    Property StackBlockSize : Integer Read fStackBlockSize;
  End;


//====================================================================
Procedure CopyStackData(Src, Dst : PByte; Size : Integer);

//====================================================================
{$REGION 'Pointer-variable access functions'}
//Pointer-variable access functions for compability with Windows-CE
//====================================================================
Function GetPByte(Const Adr : PByte) : Byte; Inline;
Function GetPWord(Const Adr : PWord) : Word; Inline;
Function GetPCardinal(Const Adr : PCardinal) : Cardinal; Inline;
Function GetPShortInt(Const Adr : PShortInt) : ShortInt; Inline;
Function GetPSmallInt(Const Adr : PSmallInt) : Smallint; Inline;
Function GetPInteger(Const Adr : PInteger) : Integer; Inline;
Function GetPInt64(Const Adr : PInt64) : Int64; Inline;
Function GetPSingle(Const Adr : PSingle) : Single; Inline;
Function GetPDouble(Const Adr : PDouble) : Double; Inline;
Function GetPExtended(Const Adr : PExtended) : Extended; Inline;
Function GetPAnsiChar(Const Adr : PAnsiChar) : AnsiChar; Inline;
Function GetPWideChar(Const Adr : PWideChar) : Widechar; Inline;
Function GetPAnsiString(Const Adr : PAnsiString) : AnsiString; Inline;
Function GetPWideString(Const Adr : PWideString) : WideString; Inline;
Function GetPShortString(Const Adr : PShortString) : ShortString; Inline;
Function GetPPointer(Const Adr : PPointer) : Pointer; Inline;
//====================================================================
Procedure SetPByte(Const Adr : PByte; Const Val : Byte); Inline;
Procedure SetPWord(Const Adr : PWord; Const Val : Word); Inline;
Procedure SetPCardinal(Const Adr : PCardinal; Const Val : Cardinal); Inline;
Procedure SetPShortInt(Const Adr : PShortInt; Const Val : ShortInt); Inline;
Procedure SetPSmallInt(Const Adr : PSmallInt; Const Val : Smallint); Inline;
Procedure SetPInteger(Const Adr : PInteger; Const Val : Integer); Inline;
Procedure SetPInt64(Const Adr : PInt64; Const Val : Int64); Inline;
Procedure SetPSingle(Const Adr : PSingle; Const Val : Single); Inline;
Procedure SetPDouble(Const Adr : PDouble; Const Val : Double); Inline;
Procedure SetPExtended(Const Adr : PExtended; Const Val : Extended); Inline;
Procedure SetPAnsiChar(Const Adr : PAnsiChar; Const Val : AnsiChar); Inline;
Procedure SetPWideChar(Const Adr : PWideChar; Const Val : Widechar); Inline;
Procedure SetPAnsiString(Const Adr : PPointer; Const Val : AnsiString); Inline;
Procedure SetPWideString(Const Adr : PPointer; Const Val : WideString); Inline;
Procedure SetPShortString(Const Adr : PShortString; Const Val : ShortString); Inline;
Procedure SetPPointer(Const Adr : PPointer; Const Val : Pointer); Inline;
//====================================================================
{$ENDREGION}

Implementation

//==============================================================================
{$REGION 'Pointer-variable access functions'}
//Pointer-variable access functions for compability with Windows-CE
//Each one is defined as inline so it does not waste any performance when not
//compiling for Windows-CE
//==============================================================================

Function GetPByte(Const Adr : PByte) : Byte;
Begin
  Result := Adr^;
End;

Function GetPWord(Const Adr : PWord) : Word;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPCardinal(Const Adr : PCardinal) : Cardinal;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPShortInt(Const Adr : PShortInt) : ShortInt;
Begin
  Result := Adr^;
End;

Function GetPSmallInt(Const Adr : PSmallInt) : Smallint;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPInteger(Const Adr : PInteger) : Integer;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPInt64(Const Adr : PInt64) : Int64;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPSingle(Const Adr : PSingle) : Single;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPDouble(Const Adr : PDouble) : Double;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPExtended(Const Adr : PExtended) : Extended;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPAnsiChar(Const Adr : PAnsiChar) : AnsiChar;
Begin
  Result := Adr^;
End;

Function GetPWideChar(Const Adr : PWideChar) : Widechar;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPAnsiString(Const Adr : PAnsiString) : AnsiString;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPWideString(Const Adr : PWideString) : WideString;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPShortString(Const Adr : PShortString) : ShortString;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

Function GetPPointer(Const Adr : PPointer) : Pointer;
Begin
  {$ifdef WinCE}
  Result := unaligned(Adr^);
  {$else WinCE}
  Result := Adr^;
  {$endif}
End;

//==============================================================================

Procedure SetPByte(Const Adr : PByte; Const Val : Byte);
Begin
  Adr^ := Val;
End;

Procedure SetPWord(Const Adr : PWord; Const Val : Word);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPCardinal(Const Adr : PCardinal; Const Val : Cardinal);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPShortInt(Const Adr : PShortInt; Const Val : ShortInt);
Begin
  Adr^ := Val;
End;

Procedure SetPSmallInt(Const Adr : PSmallInt; Const Val : Smallint);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPInteger(Const Adr : PInteger; Const Val : Integer);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPInt64(Const Adr : PInt64; Const Val : Int64);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPSingle(Const Adr : PSingle; Const Val : Single);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPDouble(Const Adr : PDouble; Const Val : Double);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPExtended(Const Adr : PExtended; Const Val : Extended);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPAnsiChar(Const Adr : PAnsiChar; Const Val : AnsiChar);
Begin
  Adr^ := Val;
End;

Procedure SetPWideChar(Const Adr : PWideChar; Const Val : Widechar);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPAnsiString(Const Adr : PPointer; Const Val : AnsiString);
var StrCopy : Pointer;
Begin
  StrCopy := nil;
  AnsiString(StrCopy) := Val;
  {$ifdef WinCE}
  unaligned(Adr^) := StrCopy;
  {$else WinCE}
  Adr^ := StrCopy;
  {$endif}
End;

Procedure SetPWideString(Const Adr : PPointer; Const Val : WideString);
var StrCopy : Pointer;
Begin
  StrCopy := nil;
  WideString(StrCopy) := Val;
  {$ifdef WinCE}
  unaligned(Adr^) := StrCopy;
  {$else WinCE}
  Adr^ := StrCopy;
  {$endif}
End;

Procedure SetPShortString(Const Adr : PShortString; Const Val : ShortString);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

Procedure SetPPointer(Const Adr : PPointer; Const Val : Pointer);
Begin
  {$ifdef WinCE}
  unaligned(Adr^) := Val;
  {$else WinCE}
  Adr^ := Val;
  {$endif}
End;

{$ENDREGION}

//====================================================================
Procedure CopyStackData(Src, Dst : PByte; Size : Integer);
Begin
  move(Src^, Dst^, Size);
End;

{Procedure CopyStackData(Src, Dst : PByte; Size : Integer);
Begin
  Case Size Of
    1  : dst^         := src^;
    2  : PWord(dst)^  := PWord(src)^;
    4  : PCardinal(dst)^ := PCardinal(src)^;
    8  : PInt64(dst)^ := PInt64(src)^;
  Else
    While Size > 0 Do
    Begin
      If Size >= 8 Then
      Begin
        PInt64(dst)^ := PInt64(src)^;
        Inc(dst, 8);
        Inc(src, 8);
        Size         := Size - 8;
        Continue;
      End;
      If Size >= 4 Then
      Begin
        PCardinal(dst)^ := PCardinal(src)^;
        Inc(dst, 4);
        Inc(src, 4);
        Size := Size - 4;
        Continue;
      End;
      If Size >= 2 Then
      Begin
        PWord(dst)^ := PWord(src)^;
        Inc(dst, 2);
        Inc(src, 2);
        Size        := Size - 2;
        Continue;
      End;
      dst^ := src^;
      Size := Size - 1;
    End;
  End;
End;  //}
//==============================================================================
 { TRutisStack }

Constructor TRutisBasicStack.Create(AStackBlockSize : Cardinal);
Begin
  fStackBlockSize := AStackBlockSize;

  SetLength(Blocks, 1);
  TopBlock       := 0;
  Blocks[0].Data := AllocMem(fStackBlockSize);
End;

Destructor TRutisBasicStack.Destroy;
Begin
  Clear;
  FreeMem(Blocks[0].Data);
  Inherited;
End;

Procedure TRutisBasicStack.Clear;
Var i  : Integer;
Begin
  Try
    Top := 0;
    Update_FreeData;
    For i := 1 To high(Blocks) Do
      FreeMem(Blocks[i].Data);
  Finally
    DecTop   := 0;
    SetLength(Blocks, 1);
    TopBlock := 0;
    //FillChar(Blocks[0].Data^, fStackBlockSize, 0);
  End;
End;

//==============================================================================

Function TRutisBasicStack.GetData(Index : Integer) : Pointer;
Var i  : Integer;
Begin
  i     := TopBlock;
  Index := Index - DecTop;
  While (Index < 0) and (i > 0) Do
  Begin
    Index := Index + Blocks[i - 1].Top;
    Dec(i);
  End;
  Result := Blocks[i].Data;
  inc(Cardinal(Result), Index);
End;

//==============================================================================

Procedure TRutisBasicStack.Copy(Src, Dst, Size : Integer);
Begin
  CopyStackData(Data[Src], Data[Dst], Size);
End;

Function TRutisBasicStack.Push(Size : Integer; SetZero : Boolean) : Integer;
Var
  Ptr  : PByte;
Begin
  Update_FreeData;
  Result := Top;
  If Top - DecTop + Size > fStackBlockSize - 1 Then
  Begin
    Blocks[TopBlock].Top := Top - DecTop;
    DecTop := DecTop + Blocks[TopBlock].Top;
    Inc(TopBlock);
    If TopBlock > high(Blocks) Then
    Begin
      SetLength(Blocks, length(Blocks) + 1);
      Blocks[TopBlock].Data := AllocMem(fStackBlockSize);
      //FillChar(Blocks[TopBlock].Data^, fStackBlockSize, 0);
    End;
  End;
  Top := Top + Size;
  If SetZero Then
  Begin
    Ptr := Data[Result];
    FillChar(Ptr^, Size, 0);
  End;
End;

Procedure TRutisBasicStack.Pop(Size : Integer);
Var int  : Integer;
Begin
  Update_FreeData;
  Top := Top - Size;
  If Top < 0 Then Top := 0;
  While Top - DecTop < -fStackBlockSize Do
  Begin
    If DecTop = 0 Then
    Begin
      Top := 0;
      exit;
    End;
    int := TopBlock;
    DecTop := DecTop - Blocks[int - 1].Top;
    Dec(TopBlock);
    //SetLength(Blocks, int);
  End;
End;

Procedure TRutisBasicStack.PopTo(Size : Integer);
Begin
  If Size < Top Then
    Pop(Top - Size);
End;

//==============================================================================
{$REGION 'Get-Var-Pointer functions'}

Function TRutisBasicStack.GetByte(Const adr : Integer) : PByte;
Begin
  Result := PByte(Data[Adr]);
End;

Function TRutisBasicStack.GetWord(Const adr : Integer) : PWord;
Begin
  Result := PWord(Data[Adr]);
End;

Function TRutisBasicStack.GetCardinal(Const adr : Integer) : PCardinal;
Begin
  Result := PCardinal(Data[Adr]);
End;

Function TRutisBasicStack.GetShortInt(Const adr : Integer) : PShortInt;
Begin
  Result := PShortInt(Data[Adr]);
End;

Function TRutisBasicStack.GetSmallInt(Const adr : Integer) : PSmallInt;
Begin
  Result := PSmallInt(Data[Adr]);
End;

Function TRutisBasicStack.GetInteger(Const adr : Integer) : PInteger;
Begin
  Result := PInteger(Data[Adr]);
End;

Function TRutisBasicStack.GetInt64(Const adr : Integer) : PInt64;
Begin
  Result := PInt64(Data[Adr]);
End;

Function TRutisBasicStack.GetSingle(Const adr : Integer) : PSingle;
Begin
  Result := PSingle(Data[Adr]);
End;

Function TRutisBasicStack.GetDouble(Const adr : Integer) : PDouble;
Begin
  Result := PDouble(Data[Adr]);
End;

Function TRutisBasicStack.GetExtended(Const adr : Integer) : PExtended;
Begin
  Result := PExtended(Data[Adr]);
End;

Function TRutisBasicStack.GetAChar(Const adr : Integer) : PAnsiChar;
Begin
  Result := PAnsiChar(Data[Adr]);
End;

Function TRutisBasicStack.GetWChar(Const adr : Integer) : PWideChar;
Begin
  Result := PWideChar(Data[Adr]);
End;

Function TRutisBasicStack.GetString(Const adr : Integer) : PRutisString;
Begin
  Result := PRutisString(Data[Adr]);
End;

Function TRutisBasicStack.GetShortString(Const adr : Integer) : PShortString;
Begin
  Result := PShortString(Data[Adr]);
End;

{$ENDREGION}

//==============================================================================
{$REGION 'Read functions'}

Function TRutisBasicStack.ReadByte(Const adr : Integer) : Byte;
Begin
  Result := PByte(Data[Adr])^;
End;

Function TRutisBasicStack.ReadWord(Const adr : Integer) : Word;
Begin
  Result := GetPWord(Data[Adr]);
End;

Function TRutisBasicStack.ReadCardinal(Const adr : Integer) : Cardinal;
Begin
  Result := GetPCardinal(Data[Adr]);
End;

Function TRutisBasicStack.ReadShortInt(Const adr : Integer) : Shortint;
Begin
  Result := PShortInt(Data[Adr])^;
End;

Function TRutisBasicStack.ReadSmallInt(Const adr : Integer) : Smallint;
Begin
  Result := GetPSmallInt(Data[Adr]);
End;

Function TRutisBasicStack.ReadInteger(Const adr : Integer) : Integer;
Begin
  Result := GetPInteger(Data[Adr]);
End;

Function TRutisBasicStack.ReadInt64(Const adr : Integer) : Int64;
Begin
  Result := GetPInt64(Data[Adr]);
End;

Function TRutisBasicStack.ReadSingle(Const adr : Integer) : Single;
Begin
  Result := GetPSingle(Data[Adr]);
End;

Function TRutisBasicStack.ReadDouble(Const adr : Integer) : Double;
Begin
  Result := GetPDouble(Data[Adr]);
End;

Function TRutisBasicStack.ReadExtended(Const adr : Integer) : Extended;
Begin
  Result := GetPExtended(Data[Adr]);
End;

Function TRutisBasicStack.ReadAChar(Const adr : Integer) : AnsiChar;
Begin
  Result := PAnsiChar(Data[Adr])^;
End;

Function TRutisBasicStack.ReadWChar(Const adr : Integer) : Widechar;
Begin
  Result := GetPWideChar(Data[Adr]);
End;

Function TRutisBasicStack.ReadString(Const adr : Integer) : RutisString;
Begin
  Result := GetPPointer(Data[Adr]);
End;

Function TRutisBasicStack.ReadShortString(Const adr : Integer) : ShortString;
Begin
  Result := GetPShortString(Data[Adr]);
End;

{$ENDREGION}

//==============================================================================
{$REGION 'Write functions'}

Procedure TRutisBasicStack.WriteByte(Const adr : Integer; Const Val : Byte);
Begin
  PByte(Data[adr])^ := Val;
End;

Procedure TRutisBasicStack.WriteWord(Const adr : Integer; Const Val : Word);
Begin
  SetPWord(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteCardinal(Const adr : Integer; Const Val : Cardinal);
Begin
  SetPCardinal(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteShortInt(Const adr : Integer; Const Val : Shortint);
Begin
  PShortInt(Data[adr])^ := Val;
End;

Procedure TRutisBasicStack.WriteSmallInt(Const adr : Integer; Const Val : Smallint);
Begin
  SetPSmallInt(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteInteger(Const adr : Integer; Const Val : Integer);
Begin
  SetPInteger(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteInt64(Const adr : Integer; Const Val : Int64);
Begin
  SetPInt64(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteSingle(Const adr : Integer; Const Val : Single);
Begin
  SetPSingle(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteDouble(Const adr : Integer; Const Val : Double);
Begin
  SetPDouble(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteExtended(Const adr : Integer; Const Val : Extended);
Begin
  SetPExtended(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteAChar(Const adr : Integer; Const Val : AnsiChar);
Begin
  PAnsiChar(Data[adr])^ := Val;
End;

Procedure TRutisBasicStack.WriteWChar(Const adr : Integer; Const Val : Widechar);
Begin
  SetPWideChar(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteString(Const adr : Integer; Const Val : RutisString);
Begin
  SetPPointer(Data[adr], Val);
End;

Procedure TRutisBasicStack.WriteShortString(Const adr : Integer; Const Val : ShortString);
Begin
  SetPShortString(Data[adr], Val);
End;

{$ENDREGION}

//==============================================================================
{$REGION 'Pop functions'}

Function TRutisBasicStack.PopByte : Byte;
Begin
  Result := PByte(Data[Top - 1])^;
  Pop(1);
End;

Function TRutisBasicStack.PopWord : Word;
Begin
  Result := GetPWord(Data[Top - 2]);
  Pop(2);
End;

Function TRutisBasicStack.PopCardinal : Cardinal;
Begin
  Result := GetPCardinal(Data[Top - 4]);
  Pop(4);
End;

Function TRutisBasicStack.PopShortInt : Shortint;
Begin
  Result := PShortInt(Data[Top - 1])^;
  Pop(1);
End;

Function TRutisBasicStack.PopSmallInt : Smallint;
Begin
  Result := GetPSmallInt(Data[Top - 2]);
  Pop(2);
End;

Function TRutisBasicStack.PopInteger : Integer;
Begin
  Result := GetPInteger(Data[Top - 4]);
  Pop(4);
End;

Function TRutisBasicStack.PopInt64 : Int64;
Begin
  Result := GetPInt64(Data[Top - 8]);
  Pop(8);
End;

Function TRutisBasicStack.PopSingle : Single;
Begin
  Result := GetPSingle(Data[Top - 4]);
  Pop(4);
End;

Function TRutisBasicStack.PopDouble : Double;
Begin
  Result := GetPDouble(Data[Top - 8]);
  Pop(8);
End;

Function TRutisBasicStack.PopExtended : Extended;
Begin
  Result := GetPExtended(Data[Top - 10]);
  Pop(10);
End;

Function TRutisBasicStack.PopAChar : AnsiChar;
Begin
  Result := PAnsiChar(Data[Top - 1])^;
  Pop(1);
End;

Function TRutisBasicStack.PopWChar : Widechar;
Begin
  Result := GetPWideChar(Data[Top - 2]);
  Pop(2);
End;

Function TRutisBasicStack.PopString : RutisString;
Begin
  Result := GetPPointer(Data[Top - 4]);
  Pop(4);
End;

Function TRutisBasicStack.PopShortString : ShortString;
Begin
  Result := GetPShortString(Data[Top - 256]);
  Pop(256);
End;

{$ENDREGION}

//==============================================================================
{$REGION 'Push functions'}

Procedure TRutisBasicStack.PushByte(Const Val : Byte);
Begin
  PByte(Data[Push(1, False)])^ := Val;
End;

Procedure TRutisBasicStack.PushWord(Const Val : Word);
Begin
  SetPWord(Data[Push(2, False)], Val);
End;

Procedure TRutisBasicStack.PushCardinal(Const Val : Cardinal);
Begin
  SetPCardinal(Data[Push(4, False)], Val);
End;

Procedure TRutisBasicStack.PushShortInt(Const Val : Shortint);
Begin
  PShortInt(Data[Push(1, False)])^ := Val;
End;

Procedure TRutisBasicStack.PushSmallInt(Const Val : Smallint);
Begin
  SetPSmallInt(Data[Push(2, False)], Val);
End;

Procedure TRutisBasicStack.PushInteger(Const Val : Integer);
Begin
  SetPInteger(Data[Push(4, False)], Val);
End;

Procedure TRutisBasicStack.PushInt64(Const Val : Int64);
Begin
  SetPInt64(Data[Push(8, False)], Val);
End;

Procedure TRutisBasicStack.PushSingle(Const Val : Single);
Begin
  SetPSingle(Data[Push(4, False)], Val);
End;

Procedure TRutisBasicStack.PushDouble(Const Val : Double);
Begin
  SetPDouble(Data[Push(8, False)], Val);
End;

Procedure TRutisBasicStack.PushExtended(Const Val : Extended);
Begin
  SetPExtended(Data[Push(10, False)], Val);
End;

Procedure TRutisBasicStack.PushAChar(Const Val : AnsiChar);
Begin
  PAnsiChar(Data[Push(1, False)])^ := Val;
End;

Procedure TRutisBasicStack.PushWChar(Const Val : Widechar);
Begin
  SetPWideChar(Data[Push(2, False)], Val);
End;

Procedure TRutisBasicStack.PushString(Const Val : RutisString; Const AutoFree : Boolean);
Begin
  SetPPointer(Data[Push(4, False)], Val);
  If AutoFree Then
    AddFreeData(Top - 4, True);
End;

Procedure TRutisBasicStack.PushShortString(Const Val : ShortString);
Begin
  SetPShortString(Data[Push(SizeOf(ShortString), False)], Val);
End;

Procedure TRutisBasicStack.PushAStringData(Const Val : Ansistring);
Begin
  If Val = '' Then
    PushCardinal(0)
  Else
    CopyStackData(PByte(Cardinal(Val) - 4), Data[Push(length(Val) + 4, False)], length(Val) + 4);
  PushByte(0);
  {$ifdef WinCE}
  while Top mod 4 <> 0 do
    PushByte(0);
  {$endif WinCE}
End;

{$ENDREGION}

//==============================================================================

Function TRutisBasicStack.PointerInBlockData(p : Pointer) : Boolean;
Var i  : Integer;
Begin
  Result := True;
  For i := 0 To TopBlock Do
  Begin
    If (Cardinal(p) >= Cardinal(Blocks[i].Data)) and
      (Cardinal(p) < Cardinal(Blocks[i].Data) + fStackBlockSize) Then exit;
  End;
  Result := False;
End;

//==============================================================================

Procedure TRutisBasicStack.AddFreeData(adr : Integer; IsStr : Boolean);
Begin
  SetLength(FreeInfo, length(FreeInfo) + 1);
  FreeInfo[high(FreeInfo)].Adr   := Adr;
  FreeInfo[high(FreeInfo)].IsStr := IsStr;
End;

Procedure TRutisBasicStack.Update_FreeData;
Var
  i    : Integer;
  dst  : PPointer;
Begin
  dst := nil;
  For i := high(FreeInfo) Downto 0 Do
    If Top <= FreeInfo[i].Adr Then
    Begin
      Try
        dst := PPointer(GetCardinal(FreeInfo[i].Adr));

        If dst^ <> nil Then
        Begin
          If FreeInfo[i].IsStr Then
            Ansistring(dst^) := ''
          Else
            FreeMem(dst^);
        End;
      Finally
        If dst <> nil Then
          dst^ := nil;
        FreeInfo[i] := FreeInfo[high(FreeInfo)];
        SetLength(FreeInfo, length(FreeInfo) - 1);
      End;
    End;
End;

{$ENDREGION}

End.



