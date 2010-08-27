{/==============================================================================
//==============================================================================

RUTIS-Engine (RunTimeScript-Engine)

Rutis_Engine.pas (part of) RUTIS-Engine

--> This unit contains the main part of the RUTIS-Engine


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

Unit Rutis_Engine;

Interface

{$i Delphi_Versions.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{.$define DEBUG}

Uses
  lclintf, Forms, Classes,
  {$ifdef DELPHI_7_UP}Variants,{$endif}
  SysUtils, Math,
  Rutis_Stack, Rutis_Defs, Rutis_Errors, Rutis_Classes, Rutis_Compiler_Delphi,
  Rutis_EXT_Common, Rutis_ExtMethodInfoWin;

Type
  TRutisEngine    = Class;

  TScriptLanguage = (slDelphi);

  //==============================================================================
  TRegisterEXTMethods = Procedure(Engine : TRutisEngineBase);
  TScriptAction       = Procedure;

  TExtDllLink   = Record
    DllHandle           : THandle;
    RegisterEXTMethods  : TRegisterEXTMethods;
    StartScript         : TScriptAction;
    StopScript          : TScriptAction;
    PauseScript         : TScriptAction;
    Destroy             : TScriptAction;
  End;

  //==============================================================================
  TWriProcedure = Procedure(s : String) Of Object;

  //==============================================================================
  TRutisBreakpoint = Record
    Line  : Integer;
    //CMD       : Integer;
    //Rule   : Integer;
  End;

  TRutisEngine = Class(TRutisEngineBase)
  Private
  Protected
    fLastAdress    : Pointer;
    fLastAdressId  : Integer;
    //================================================
    fExtDlls       : Array Of TExtDllLink;
    //================================================
    Function GetExtAddrRange(P : Pointer; Exact : Boolean = False) : Integer;
    //================================================
    {$REGION 'OpCodes'}
    Procedure OpGen;
    //==============
    Procedure OpGen1;
    Procedure OpGen2;
    Procedure OpGen4;
    //==============
    Procedure OpAt;
    Procedure OpAt2;
    Procedure OpPtr;
    Procedure OpPtrP;
    Procedure OpCheckPtr;
    //==============
    Procedure OpLodR;
    Procedure OpResStr;
    //==============
    Procedure OpLod;
    Procedure OpSto;
    Procedure OpMov;
    //==============
    Procedure OpLodP;
    Procedure OpStoP;
    //==============
    Procedure OpGenStr;
    Procedure OpCopyAStr;
    Procedure OpCopyWStr;
    Procedure OpStoAStr;
    Procedure OpStoWStr;
    //==============
    Procedure OpScaleMem;
    Procedure OpMemSize;
    //==============
    Procedure OpGetAStrLength;
    Procedure OpSetAStrLength;
    Procedure OpGetWStrLength;
    Procedure OpSetWStrLength;
    //==============
    Procedure OpConv;
    Procedure OpInc;
    //==============
    Procedure OpAdd;
    Procedure OpSub;
    Procedure OpMult;
    Procedure OpDiv;
    Procedure OpModulo;
    Procedure OpEnumToSet;
    //==============
    Procedure OpAnd;
    Procedure OpNot;
    Procedure OpOr;
    Procedure OpXOr;
    //==============
    Procedure OpWri;
    Procedure OpExt;
    Procedure OpCall;
    Procedure OpRet;
    //==============
    Procedure OpJmp;
    Procedure OpOpr;
    Procedure OpJZ;
    Procedure OpJL;
    Procedure OpJG;
    Procedure OpJGZ;
    Procedure OpJLZ;
    {$ENDREGION}
    //================================================
  Public
    ScriptLanguage  : TScriptLanguage;
    Breakpoints     : Array Of TRutisBreakpoint;
    //================================================
    OnWrite         : TWriProcedure;
    //================================================
    Constructor Create(StackBlockSize : Cardinal = 1024);
    Destructor Destroy; Override;
    //================================================
    Procedure ExecuteCMD(CMD : TRutisScriptCmd);
    Procedure Run;
    Procedure Stop;
    Procedure Pause;
    Procedure StepCmd;
    Procedure StepLine;
    Procedure ResetScriptState;
    //================================================
    Procedure ToggleBreakpoint(Line : Integer);
    Function GetBreakpoint(Line : Integer) : Integer;
    Function GetBreakpointToCmd(CMD : Integer) : Integer;
    //================================================
    Function DebugVarValue(AVarDecl : TRutisVarDecl) : String;
    Function DebugVarName(AVarDecl : TRutisVarDecl) : String;
    //================================================
    Function RegisterExtDll(FileName : String) : Boolean;
    Procedure UnloadExtDlls;
    //================================================
    Procedure GetByteCode(ByteCode : TStrings);
    Function ExtMethodLinkToStr(ExtLink : TRutisExtMethodType; IncludeProc : Boolean) : String;
    Function ShowExtMethodListWindow : TRutisExtMethodType;
    //================================================
    //================================================
  End;

  //==============================================================================
  //==============================================================================
Implementation

Var
  TempStrAdress  : RutisString;
 //==============================================================================
 //============ TRutisEngine ===================================================
 //==============================================================================

Constructor TRutisEngine.Create(StackBlockSize : Cardinal);
Begin
  Inherited Create(StackBlockSize);

  Compiler := TRutisCompilerDelphi.Create(self);

  OptProcessTimer      := True;
  OptProcessTimerCount := 100000;

  UnitFileManager.AddPath(ExtractFilePath(ParamStr(0)));

  Rutis_EXT_Common.RegisterEXTMethods(self);
End;

Destructor TRutisEngine.Destroy;
Begin
  UnitFileManager.Free;
  UnloadExtDlls;
  Inherited;
End;

Function TRutisEngine.GetExtAddrRange(P : Pointer; Exact : Boolean = False) : Integer;
Begin
  With ScriptData Do
    If Exact Then
    Begin
      For Result := 0 To high(ScriptData.AddrRanges) Do
        If P = ScriptData.AddrRanges[Result].Start Then
          exit;
    End
    Else
      For Result := 0 To high(ScriptData.AddrRanges) Do
      Begin
        If (Cardinal(P) >= Cardinal(ScriptData.AddrRanges[Result].Start)) and
          (Cardinal(P) < Cardinal(ScriptData.AddrRanges[Result].Start) + ScriptData.AddrRanges[Result].Size) Then
        Begin
          exit;
        End;
      End;
  If ScriptData.Stack.PointerInBlockData(P) Then
  Begin
    Result := -2;
    exit;
  End;
  Result := -1;
End;

//===================================
//============ OpCodes ==============
//===================================

{$REGION 'OpCodes'}

Procedure TRutisEngine.OpGen;
Begin
  ScriptData.Stack.Push(ScriptData.CurrCmd.P1, True);
End;

Procedure TRutisEngine.OpGen1;
Begin
  ScriptData.Stack.PushByte(ScriptData.CurrCmd.P1);
End;

Procedure TRutisEngine.OpGen2;
Begin
  ScriptData.Stack.PushWord(ScriptData.CurrCmd.P1);
End;

Procedure TRutisEngine.OpGen4;
Begin
  ScriptData.Stack.PushCardinal(ScriptData.CurrCmd.P1);
End;

//===================================

Procedure TRutisEngine.OpAt;
Begin
  ScriptData.Stack.PopByte;
  ScriptData.Stack.PushCardinal(Cardinal(fLastAdress));
End;

Procedure TRutisEngine.OpAt2;
Begin
  ScriptData.Stack.PopByte;
  ScriptData.Stack.PushCardinal(Cardinal(@fLastAdress));
End;

Procedure TRutisEngine.OpPtr;
Var
  src  : Cardinal;
Begin
  src := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);

  fLastAdressId := src;
  fLastAdress   := ScriptData.Stack.Data[src];

  ScriptData.Stack.PushCardinal(Cardinal(fLastAdress));
End;

Procedure TRutisEngine.OpPtrP;
Var
  src  : Cardinal;
Begin
  src := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(src + 3) Then exit;

  fLastAdressId := src;
  fLastAdress   := Pointer(ScriptData.Stack.ReadCardinal(src));
  If GetExtAddrRange(fLastAdress) = -1 Then
  Begin
    ScriptMessage('Address Error');
    exit;
  End;

  ScriptData.Stack.PushCardinal(Cardinal(fLastAdress));
End;

Procedure TRutisEngine.OpCheckPtr;
Begin
  fLastAdress := Pointer(ScriptData.Stack.ReadCardinal(ScriptData.Stack.Top - 4));
  If GetExtAddrRange(fLastAdress) = -1 Then
  Begin
    ScriptMessage('Address Error');
    exit;
  End;
End;

//===================================

Procedure TRutisEngine.OpLodR;
Var src  : Integer;
Begin
  If ScriptData.CurrCmd.P1 >= ScriptData.Ressources.Top Then exit;
  src         := ScriptData.Stack.Push(ScriptData.CurrCmd.P2);
  fLastAdressId := ScriptData.CurrCmd.P1;
  fLastAdress := ScriptData.Ressources.Data[ScriptData.CurrCmd.P1];

  CopyStackData(
    fLastAdress,
    ScriptData.Stack.Data[src],
    ScriptData.CurrCmd.P2);
End;

Procedure TRutisEngine.OpResStr;
Begin
  fLastAdressId := ScriptData.CurrCmd.P1;
  TempStrAdress := ScriptData.Ressources.Data[ScriptData.CurrCmd.P1];
  ScriptData.Stack.PushString(TempStrAdress, False);
End;

//===================================

Procedure TRutisEngine.OpLod;
Var
  src, dst  : Integer;
Begin
  src := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(src + ScriptData.CurrCmd.P3 - 1) Then exit;

  dst := ScriptData.Stack.Push(ScriptData.CurrCmd.P3);

  fLastAdressId := src;
  fLastAdress   := ScriptData.Stack.Data[src];

  CopyStackData(
    fLastAdress,
    ScriptData.Stack.Data[dst],
    ScriptData.CurrCmd.P3);
  //ScriptData.Stack.Move(src, dst, ScriptData.CurrCmd.P3);
End;

Procedure TRutisEngine.OpSto;
Begin
  OpMov;
  ScriptData.Stack.Pop(ScriptData.CurrCmd.P3);
End;

Procedure TRutisEngine.OpMov;
Var
  dst, src  : Integer;
Begin
  dst := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(dst + ScriptData.CurrCmd.P3 - 1) Then exit;

  src := ScriptData.Stack.Top - ScriptData.CurrCmd.P3;

  fLastAdressId := dst;
  fLastAdress   := ScriptData.Stack.Data[dst];

  CopyStackData(
    ScriptData.Stack.Data[src],
    fLastAdress,
    ScriptData.CurrCmd.P3);
  //ScriptData.Stack.Move(src, dst, ScriptData.CurrCmd.P3);
End;

//===================================

Procedure TRutisEngine.OpLodP;
Var
  src, dst  : Integer;
Begin
  src := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(src + 3) Then exit;

  fLastAdressId := src;
  fLastAdress   := Pointer(ScriptData.Stack.ReadCardinal(src));
  {If GetExtAddrRange(fLastAdress) = -1 Then
  Begin
    ScriptError('Address Error');
    exit;
  End;   }

  If ScriptData.CurrCmd.P2 = -1 Then
    ScriptData.Stack.PopCardinal;

  dst := ScriptData.Stack.Push(ScriptData.CurrCmd.P3);

  CopyStackData(
    fLastAdress,
    ScriptData.Stack.Data[dst],
    ScriptData.CurrCmd.P3);
End;

Procedure TRutisEngine.OpStoP;
Var
  src, dst  : Integer;
Begin
  If (ScriptData.CurrCmd.P2 = -1) and (ScriptData.CurrCmd.P1 = -4) Then
    fLastAdress := PByte(ScriptData.Stack.PopCardinal)
  Else
  Begin
    dst         := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
    If CheckStackIndex(dst + 3) Then exit;
    fLastAdress := Pointer(ScriptData.Stack.ReadCardinal(dst));
  End;

  {If GetExtAddrRange(fLastAdress) = -1 Then
  Begin
    ScriptError('Address Error');
    exit;
  End; }

  src := ScriptData.Stack.Top - ScriptData.CurrCmd.P3;

  CopyStackData(
    ScriptData.Stack.Data[src],
    fLastAdress,
    ScriptData.CurrCmd.P3);

  ScriptData.Stack.Pop(ScriptData.CurrCmd.P3);
End;

//===================================

Procedure TRutisEngine.OpGenStr;
Var
  PStr  : PAnsiString;
Begin
  New(PStr);
  PStr^ := '';
  ScriptData.Stack.PushString(PStr, True);
End;

Procedure TRutisEngine.OpCopyAStr;
Var
  StrSrc  : RutisString;
  StrDst  : RutisString;
Begin
  StrSrc := ScriptData.Stack.PopString;
  StrDst := nil;
  If StrSrc <> nil Then
    Ansistring(StrDst) := PAnsiChar(StrSrc);
  ScriptData.Stack.PushString(StrDst, True);
End;

Procedure TRutisEngine.OpCopyWStr;
Var
  StrSrc  : RutisString;
  StrDst  : RutisString;
Begin
  StrSrc := ScriptData.Stack.PopString;
  StrDst := nil;
  If StrSrc <> nil Then
    WideString(StrDst) := PWideChar(StrSrc);
  ScriptData.Stack.PushString(StrDst, True);
End;

Procedure TRutisEngine.OpStoAStr;
Var
  dst, src  : Integer;
  I         : Integer;
Begin
  dst := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(dst + 4 - 1) Then exit;

  src := ScriptData.Stack.Top - 4;

  fLastAdressId := dst;
  fLastAdress   := ScriptData.Stack.Data[dst];

  If ScriptData.CurrCmd.P3 = 1 Then
    fLastAdress := GetPPointer(fLastAdress);

  //Free old string data
  PAnsiString(fLastAdress)^ := '';

  CopyStackData(
    ScriptData.Stack.Data[src],
    fLastAdress,
    4);

  For I := 0 To high(ScriptData.Stack.FreeInfo) Do
    If (ScriptData.Stack.FreeInfo[i].Adr = src) Then
    Begin
      ScriptData.Stack.FreeInfo[i].Adr := dst;
      break;
    End;
  ScriptData.Stack.Pop(4);
End;

Procedure TRutisEngine.OpStoWStr;
Var
  dst, src  : Integer;
  I         : Integer;
Begin
  dst := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(dst + 4 - 1) Then exit;

  src := ScriptData.Stack.Top - 4;

  fLastAdressId := dst;
  fLastAdress   := ScriptData.Stack.Data[dst];

  If ScriptData.CurrCmd.P3 = 1 Then
    fLastAdress := GetPPointer(fLastAdress);

  //Free old string data
  PWideString(fLastAdress)^ := '';

  CopyStackData(
    ScriptData.Stack.Data[src],
    fLastAdress,
    4);

  For I := 0 To high(ScriptData.Stack.FreeInfo) Do
    If (ScriptData.Stack.FreeInfo[i].Adr = src) Then
    Begin
      ScriptData.Stack.FreeInfo[i].Adr := dst;
      break;
    End;
  ScriptData.Stack.Pop(4);
End;

//===================================

Procedure TRutisEngine.OpScaleMem;
{
Parameters
1) Cardinal -> Mem Pointer
2) Integer  -> New Mem Block-count
3) Integer  -> Mem Block-Size
Returns
1) Cardinal -> New Array Pointer
}
Var
  i,
  qSize,
  ItemSize  : Integer;
  AChar     : PChar;
  old       : TMemoryRange;
Begin
  ItemSize := ScriptData.Stack.PopInteger;
  If ItemSize < 0 Then
  Begin
    ScriptMessage('Mem Item-Size should be >= 0', etWarning); //etRuntimeError
    ItemSize := 0;
  End;
  qSize := ScriptData.Stack.PopInteger;
  If qSize < 0 Then
  Begin
    ScriptMessage('Mem length should be >= 0', etWarning); //etRuntimeError
    qSize := 0;
  End;

  i := GetExtAddrRange(Pointer(ScriptData.Stack.PopCardinal - 4), True);
  If i = -1 Then
  Begin
    SetLength(ScriptData.AddrRanges, length(ScriptData.AddrRanges) + 1);
    i := high(ScriptData.AddrRanges);
  End;

  With ScriptData.AddrRanges[i] Do
  Begin
    old := ScriptData.AddrRanges[i];

    //Neuen Speicher zuweisen und nullen
    Size := qSize * ItemSize + 4;
    ReallocMem(Start, Size);

    If old.size < Size Then
    Begin
      AChar := Pointer(Start);
      Inc(Cardinal(AChar), old.Size);
      FillChar(AChar^, Size - old.size, 0);
    End;
    PCardinal(Start)^ := qSize;

    ScriptData.Stack.PushCardinal(Cardinal(Start) + 4);
  End;
  If ScriptData.AddrRanges[i].Size = 0 Then
  Begin
    If length(ScriptData.AddrRanges) > 0 Then
      ScriptData.AddrRanges[i] := ScriptData.AddrRanges[high(ScriptData.AddrRanges)];
    SetLength(ScriptData.AddrRanges, length(ScriptData.AddrRanges) - 1);
  End;
End;

Procedure TRutisEngine.OpMemSize;
{
Parameters
1) Cardinal -> Array Pointer
Returns
1) Integer -> Array Length
}
Var
  addr  : PInteger;
Begin
  addr := Pointer(ScriptData.Stack.PopCardinal);
  If addr = nil Then
    ScriptData.Stack.PushInteger(0)
  Else
  Begin
    Dec(Cardinal(addr), 4);
    ScriptData.Stack.PushInteger(addr^);
  End;
End;

//===================================

Procedure TRutisEngine.OpGetAStrLength;
{
Parameters
1) String
Returns
1) Integer -> Array Length
}
Begin
  ScriptData.Stack.PushInteger(length(Ansistring(ScriptData.Stack.PopString)));
End;

Procedure TRutisEngine.OpSetAStrLength;
Var
  Str  : RutisString;
  len  : Integer;
Begin
  len := ScriptData.Stack.PopInteger;
  Str := ScriptData.Stack.PopString;
  If len < 0 Then len := 0;
  SetLength(Ansistring(Str), len);
  ScriptData.Stack.PushString(Str, False);
End;

Procedure TRutisEngine.OpGetWStrLength;
{
Parameters
1) String
Returns
1) Integer -> Array Length
}
Begin
  ScriptData.Stack.PushInteger(length(WideString(ScriptData.Stack.PopString)));
End;

Procedure TRutisEngine.OpSetWStrLength;
Var
  Str  : RutisString;
  len  : Integer;
Begin
  len := ScriptData.Stack.PopInteger;
  Str := ScriptData.Stack.PopString;
  If len < 0 Then len := 0;
  SetLength(WideString(Str), len);
  ScriptData.Stack.PushString(Str, False);
End;

//===================================

Procedure TRutisEngine.OpConv;
Var Str  : RutisString;
Begin
  If ScriptData.CurrCmd.P1 = ScriptData.CurrCmd.P2 Then exit;
  With ScriptData.Stack Do
    Case TRutisIntType(ScriptData.CurrCmd.P1) Of
      intByte,
      intEnum,
      intAChar,
      intBoolean  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
      //intByte,
      //intEnum,
      //intAChar,
      //intBoolean: PushByte(PopByte);
          intShortInt  : PushShortInt(PopByte);
          intWChar,
          intWord  : PushWord(PopByte);
          intSmallint  : PushSmallint(PopByte);
          intCardinal,
          intArray,
          intPointer  : PushCardinal(PopByte);
          intInteger  : PushInteger(PopByte);
          intSingle  : PushSingle(PopByte);
          intDouble  : PushDouble(PopByte);
          intExtended  : PushExtended(PopByte);
          intSet  : PushCardinal(1 shl PopByte);
          intAString :
          Begin
            Str := nil;
            If TRutisIntType(ScriptData.CurrCmd.P1) = intAChar Then
              Ansistring(Str) := PopAChar
            Else
              Ansistring(Str) := IntToStr(PopByte);
            PushString(Str, True);
          End;
        End;
      intShortInt  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(PopShortInt);
          //intShortInt : PushShortInt(PopShortInt);
          intWChar,
          intWord  : PushWord(PopShortInt);
          intSmallint  : PushSmallint(PopShortInt);
          intCardinal,
          intArray,
          intPointer  : PushCardinal(PopShortInt);
          intInteger  : PushInteger(PopShortInt);
          intSingle  : PushSingle(PopShortInt);
          intDouble  : PushDouble(PopShortInt);
          intExtended  : PushExtended(PopShortInt);
        End;
      intWChar,
      intWord  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(PopWord);
          intShortInt  : PushShortInt(PopWord);
          //intWord     : PushWord(PopWord);
          intSmallint  : PushSmallint(PopWord);
          intCardinal,
          intArray,
          intPointer  : PushCardinal(PopWord);
          intInteger  : PushInteger(PopWord);
          intSingle  : PushSingle(PopWord);
          intDouble  : PushDouble(PopWord);
          intExtended  : PushExtended(PopWord);
          intWString :
          Begin
            Str := nil;
            If TRutisIntType(ScriptData.CurrCmd.P1) = intWChar Then
              WideString(Str) := PopWChar
            Else
              WideString(Str) := IntToStr(PopWord);
            PushString(Str, True);
          End;
        End;
      intSmallint  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(PopSmallint);
          intShortInt  : PushShortInt(PopSmallint);
          intWChar,
          intWord  : PushWord(PopSmallint);
          //intSmallint : PushSmallint(PopSmallint);
          intCardinal,
          intArray,
          intPointer  : PushCardinal(PopSmallint);
          intInteger  : PushInteger(PopSmallint);
          intSingle  : PushSingle(PopSmallint);
          intDouble  : PushDouble(PopSmallint);
          intExtended  : PushExtended(PopSmallint);
        End;
      intCardinal,
      intPointer  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(PopCardinal);
          intShortInt  : PushShortInt(PopCardinal);
          intWChar,
          intWord  : PushWord(PopCardinal);
          intSmallint  : PushSmallint(PopCardinal);
          //intCardinal,
          //intArray,
          //intPointer  : PushCardinal(PopCardinal);
          intInteger  : PushInteger(PopCardinal);
          intSingle  : PushSingle(PopCardinal);
          intDouble  : PushDouble(PopCardinal);
          intExtended  : PushExtended(PopCardinal);
        End;
      intInteger  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(PopInteger);
          intShortInt  : PushShortInt(PopInteger);
          intWChar,
          intWord  : PushWord(PopInteger);
          intSmallint  : PushSmallint(PopInteger);
          intCardinal,
          intArray,
          intPointer  : PushCardinal(PopInteger);
          //intInteger  : PushInteger(PopInteger);
          intSingle  : PushSingle(PopInteger);
          intDouble  : PushDouble(PopInteger);
          intExtended  : PushExtended(PopInteger);
        End;
      intSingle  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(round(PopSingle));
          intShortInt  : PushShortInt(round(PopSingle));
          intWChar,
          intWord  : PushWord(round(PopSingle));
          intSmallint  : PushSmallint(round(PopSingle));
          intCardinal,
          intArray,
          intPointer  : PushCardinal(round(PopSingle));
          intInteger  : PushInteger(round(PopSingle));
          //intSingle   : PushSingle(PopSingle);
          intDouble  : PushDouble(PopSingle);
          intExtended  : PushExtended(PopSingle);
        End;
      intDouble  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(round(PopDouble));
          intShortInt  : PushShortInt(round(PopDouble));
          intWChar,
          intWord  : PushWord(round(PopDouble));
          intSmallint  : PushSmallint(round(PopDouble));
          intCardinal,
          intArray,
          intPointer  : PushCardinal(round(PopDouble));
          intInteger  : PushInteger(round(PopDouble));
          intSingle  : PushSingle(PopDouble);
          //intDouble   : PushDouble(PopDouble);
          intExtended  : PushExtended(PopDouble);
        End;
      intExtended  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intByte,
          intEnum,
          intAChar,
          intBoolean  : PushByte(round(PopExtended));
          intShortInt  : PushShortInt(round(PopExtended));
          intWChar,
          intWord  : PushWord(round(PopExtended));
          intSmallint  : PushSmallint(round(PopExtended));
          intCardinal,
          intArray,
          intPointer  : PushCardinal(round(PopExtended));
          intInteger  : PushInteger(round(PopExtended));
          intSingle  : PushSingle(PopExtended);
          intDouble  : PushDouble(PopExtended);
          //intExtended : PushExtended(PopExtended);
        End;
      intShortString  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intAString :
          Begin
            Str := nil;
            Ansistring(Str) := PopShortString;
            PushString(Str, True);
          End;
          intWString :
          Begin
            Str := nil;
            WideString(Str) := PopShortString;
            PushString(Str, True);
          End;
        End;
      intAString  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intShortString :
          Begin
            Str := PopString;
            PushShortString(Ansistring(Str));
          End;
          intWString :
          Begin
            Str := nil;
            WideString(Str) := Ansistring(PopString);
            PushString(Str, True);
          End;
        End;
      intWString  : Case TRutisIntType(ScriptData.CurrCmd.P2) Of
          intShortString :
          Begin
            PushShortString(WideString(PopString));
          End;
          intAString :
          Begin
            Str := nil;
            Ansistring(Str) := WideString(PopString);
            PushString(Str, True);
          End;
        End;
    Else
    End;
End;

Procedure TRutisEngine.OpInc;
Var
  src     : Cardinal;
  incVal  : Integer;
Begin
  src := GetStackLvlAddress(ScriptData.CurrCmd.P1, ScriptData.CurrCmd.P2);
  If CheckStackIndex(src + IntTypeSizes[TRutisIntType(ScriptData.CurrCmd.P3)] - 1) Then exit;

  fLastAdress := ScriptData.Stack.Data[src];

  With ScriptData.Stack Do
  Begin
    incVal := PopInteger;

    Case TRutisIntType(ScriptData.CurrCmd.P3) Of
      intByte,
      intAChar  : SetPByte(fLastAdress, GetPByte(fLastAdress) + incVal);
      intShortInt  : SetPShortInt(fLastAdress, GetPShortInt(fLastAdress) + incVal);
      intWord  : SetPWord(fLastAdress, GetPWord(fLastAdress) + incVal);
      intSmallint  : SetPSmallint(fLastAdress, GetPSmallint(fLastAdress) + incVal);
      intCardinal,
      intPointer,
      intArray  : SetPCardinal(fLastAdress, GetPCardinal(fLastAdress) + incVal);
      intInteger  : SetPInteger(fLastAdress, GetPInteger(fLastAdress) + incVal);
      intSingle  : SetPSingle(fLastAdress, GetPSingle(fLastAdress) + incVal);
      intDouble  : SetPDouble(fLastAdress, GetPDouble(fLastAdress) + incVal);
      intExtended  : SetPExtended(fLastAdress, GetPExtended(fLastAdress) + incVal);
    Else
    End;
  End;
End;

//===================================

Procedure TRutisEngine.OpAdd;
Var
  dst1  : Pointer;
  Size  : Byte;
  temp  : Pointer;
Begin
  Size := IntTypeSizes[TRutisIntType(ScriptData.CurrCmd.P1)];
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - 2 * Size];
    Case TRutisIntType(ScriptData.CurrCmd.P1) Of
      intByte,
      intEnum,
      intAChar  : SetPByte(dst1, GetPByte(dst1) + PopByte);
      intShortInt  : SetPShortInt(dst1, GetPShortInt(dst1) + PopShortInt);
      intWord  : SetPWord(dst1, GetPWord(dst1) + PopWord);
      intSmallint  : SetPSmallint(dst1, GetPSmallint(dst1) + PopSmallint);
      intCardinal,
      intPointer,
      intArray  : SetPCardinal(dst1, GetPCardinal(dst1) + PopCardinal);
      intInteger  : SetPInteger(dst1, GetPInteger(dst1) + PopInteger);
      intSingle  : SetPSingle(dst1, GetPSingle(dst1) + PopSingle);
      intDouble  : SetPDouble(dst1, GetPDouble(dst1) + PopDouble);
      intExtended  : SetPExtended(dst1, GetPExtended(dst1) + PopExtended);
      intAString :
      Begin
        temp := GetPPointer(dst1);
        AnsiString(temp) := AnsiString(temp) + PAnsiChar(PopString);
        SetPPointer(dst1, temp);
        //SetPPointer(dst1, Pointer(AnsiString(GetPPointer(dst1)) + PAnsiChar(PopString)));
        //PAnsiString(dst1)^ := PAnsiString(dst1)^ + PAnsiChar(PopString);
      End;
      intWString :
      Begin
        temp := GetPPointer(dst1);
        WideString(temp) := WideString(temp) + PWideChar(PopString);
        SetPPointer(dst1, temp);
        //SetPPointer(dst1, Pointer(WideString(GetPPointer(dst1)) + PWideChar(PopString)));
      End;
      intSet :
      Begin
        ScriptData.CurrCmd.P1 := ScriptData.CurrCmd.P2;
        OpOr;
      End;
    Else
    End;
  End;
End;

Procedure TRutisEngine.Opsub;
Var
  dst1  : Pointer;
  Size  : Byte;
Begin
  Size := IntTypeSizes[TRutisIntType(ScriptData.CurrCmd.P1)];
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case TRutisIntType(ScriptData.CurrCmd.P1) Of
      intByte,
      intAChar  : SetPByte(dst1, GetPByte(dst1) - PopByte);
      intShortInt  : SetPShortInt(dst1, GetPShortInt(dst1) - PopShortInt);
      intWord  : SetPWord(dst1, GetPWord(dst1) - PopWord);
      intSmallint  : SetPSmallint(dst1, GetPSmallint(dst1) - PopSmallint);
      intCardinal,
      intPointer,
      intArray  : SetPCardinal(dst1, GetPCardinal(dst1) - PopCardinal);
      intInteger  : SetPInteger(dst1, GetPInteger(dst1) - PopInteger);
      intSingle  : SetPSingle(dst1, GetPSingle(dst1) - PopSingle);
      intDouble  : SetPDouble(dst1, GetPDouble(dst1) - PopDouble);
      intExtended  : SetPExtended(dst1, GetPExtended(dst1) - PopExtended);
      intSet :
      Begin
        ScriptData.CurrCmd.P1 := ScriptData.CurrCmd.P2;
        OpNot;
        OpAnd;
      End;
    Else
    End;
  End;
End;

Procedure TRutisEngine.Opmult;
Var
  dst1  : Pointer;
  Size  : Byte;
Begin
  Size := IntTypeSizes[TRutisIntType(ScriptData.CurrCmd.P1)];
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case TRutisIntType(ScriptData.CurrCmd.P1) Of
      intByte,
      intAChar  : SetPByte(dst1, GetPByte(dst1) * PopByte);
      intShortInt  : SetPShortInt(dst1, GetPShortInt(dst1) * PopShortInt);
      intWord  : SetPWord(dst1, GetPWord(dst1) * PopWord);
      intSmallint  : SetPSmallint(dst1, GetPSmallint(dst1) * PopSmallint);
      intCardinal,
      intPointer,
      intArray  : SetPCardinal(dst1, GetPCardinal(dst1) * PopCardinal);
      intInteger  : SetPInteger(dst1, GetPInteger(dst1) * PopInteger);
      intSingle  : SetPSingle(dst1, GetPSingle(dst1) * PopSingle);
      intDouble  : SetPDouble(dst1, GetPDouble(dst1) * PopDouble);
      intExtended  : SetPExtended(dst1, GetPExtended(dst1) * PopExtended);
    Else
    End;
  End;
End;

Procedure TRutisEngine.Opdiv;
Var
  dst1  : Pointer;
  Size  : Byte;
Begin
  Size := IntTypeSizes[TRutisIntType(ScriptData.CurrCmd.P1)];
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case TRutisIntType(ScriptData.CurrCmd.P1) Of
      intByte,
      intAChar  : SetPByte(dst1, GetPByte(dst1) div PopByte);
      intShortInt  : SetPShortInt(dst1, GetPShortInt(dst1) div PopShortInt);
      intWord  : SetPWord(dst1, GetPWord(dst1) div PopWord);
      intSmallint  : SetPSmallint(dst1, GetPSmallint(dst1) div PopSmallint);
      intCardinal,
      intPointer,
      intArray  : SetPCardinal(dst1, GetPCardinal(dst1) div PopCardinal);
      intInteger  : SetPInteger(dst1, GetPInteger(dst1) div PopInteger);
      intSingle  : SetPSingle(dst1, GetPSingle(dst1) / PopSingle);
      intDouble  : SetPDouble(dst1, GetPDouble(dst1) / PopDouble);
      intExtended  : SetPExtended(dst1, GetPExtended(dst1) / PopExtended);
    Else
    End;
  End;
End;

Procedure TRutisEngine.OpEnumToSet;
Var
  bit  : Cardinal;
  adr  : PByte;
Begin
  If ScriptData.CurrCmd.P1 <= 0 Then
  Begin
    ScriptMessage('Error - OpEnumToSet');
    exit;
  End;
  bit := ScriptData.Stack.PopByte;
  adr := ScriptData.Stack.Data[ScriptData.Stack.Push(ScriptData.CurrCmd.P1, True)];
  {$ifndef FPC}
  Asm
    MOV EAX, adr
    MOV EDX, bit
    BTS [EAX],EDX // setzt CF=1 wenn das Bit count = 1 ist und setzt dann das Bit=1
  End;
  {$else FPC}
  inc(Cardinal(adr), bit div 8);
  bit := bit mod 8;
  bit := 1 shl bit;
  adr^ := adr^ or bit;
  {$endif FPC}
End;

Procedure TRutisEngine.OpModulo;
Var
  dst1 : Pointer;
  Size : Byte;
Begin
  Size := IntTypeSizes[TRutisIntType(ScriptData.CurrCmd.P1)];
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case TRutisIntType(ScriptData.CurrCmd.P1) Of
      intByte,
      intAChar  : SetPByte(dst1, GetPByte(dst1) mod PopByte);
      intShortInt  : SetPShortInt(dst1, GetPShortInt(dst1) mod PopShortInt);
      intWord  : SetPWord(dst1, GetPWord(dst1) mod PopWord);
      intSmallint  : SetPSmallint(dst1, GetPSmallint(dst1) mod PopSmallint);
      intCardinal,
      intPointer,
      intArray  : SetPCardinal(dst1, GetPCardinal(dst1) mod PopCardinal);
      intInteger  : SetPInteger(dst1, GetPInteger(dst1) mod PopInteger);
      //intSingle   : SetPSingle(dst1,    GetPSingle(dst1) mod PopSingle);
      //intDouble   : SetPDouble(dst1,    GetPDouble(dst1) mod PopDouble);
      //intExtended : SetPExtended(dst1,  GetPExtended(dst1) mod PopExtended);
    Else
    End;
  End;
End;

//===================================

Procedure TRutisEngine.OpAnd;
Var
  dst1,
  dst2     : Pointer;
  Size,
  PartSize : Word;
Begin
  Size := ScriptData.CurrCmd.P1;
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case Size Of
      1  : SetPByte(dst1, GetPByte(dst1) and PopByte);
      2  : SetPWord(dst1, GetPWord(dst1) and PopWord);
      4  : SetPCardinal(dst1, GetPCardinal(dst1) and PopCardinal);
      8  : SetPInt64(dst1, GetPInt64(dst1) and PopInteger);
    Else
      dst1     := Data[Top - Size - Size];
      dst2     := Data[Top - Size];
      PartSize := 0;
      While Size > 0 Do
      Begin
        Case Size Of
          1 :
          Begin
            SetPByte(dst1, GetPByte(dst1) and GetPByte(dst2));
            exit;
          End;
          2..3 :
          Begin
            SetPWord(dst1, GetPWord(dst1) and GetPWord(dst2));
            PartSize := 2;
          End;
          4..7 :
          Begin
            SetPCardinal(dst1, GetPCardinal(dst1) and GetPCardinal(dst2));
            PartSize := 4;
          End;
        Else
          SetPInt64(dst1, GetPInt64(dst1) and GetPInt64(dst1));
          PartSize := 8;
        End;
        Inc(Cardinal(dst1), PartSize);
        Inc(Cardinal(dst2), PartSize);
      End;
    End;
  End;
End;

Procedure TRutisEngine.OpOr;
Var
  dst1,
  dst2     : Pointer;
  Size,
  PartSize : Word;
Begin
  Size := ScriptData.CurrCmd.P1;
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case Size Of
      1  : SetPByte(dst1, GetPByte(dst1) or PopByte);
      2  : SetPWord(dst1, GetPWord(dst1) or PopWord);
      4  : SetPCardinal(dst1, GetPCardinal(dst1) or PopCardinal);
      8  : SetPInt64(dst1, GetPInt64(dst1) or PopInteger);
    Else
      dst1     := Data[Top - Size - Size];
      dst2     := Data[Top - Size];
      PartSize := 0;
      While Size > 0 Do
      Begin
        Case Size Of
          1 :
          Begin
            SetPByte(dst1, GetPByte(dst1) or GetPByte(dst2));
            exit;
          End;
          2..3 :
          Begin
            SetPWord(dst1, GetPWord(dst1) or GetPWord(dst2));
            PartSize := 2;
          End;
          4..7 :
          Begin
            SetPCardinal(dst1, GetPCardinal(dst1) or GetPCardinal(dst2));
            PartSize := 4;
          End;
        Else
          SetPInt64(dst1, GetPInt64(dst1) or GetPInt64(dst2));
          PartSize := 8;
        End;
        Inc(Cardinal(dst1), PartSize);
        Inc(Cardinal(dst2), PartSize);
      End;
    End;
  End;
End;

Procedure TRutisEngine.OpXOr;
Var
  dst1,
  dst2     : Pointer;
  Size,
  PartSize : Word;
Begin
  Size := ScriptData.CurrCmd.P1;
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size - Size];
    Case Size Of
      1  : SetPByte(dst1, GetPByte(dst1)xor PopByte);
      2  : SetPWord(dst1, GetPWord(dst1)xor PopWord);
      4  : SetPCardinal(dst1, GetPCardinal(dst1)xor PopCardinal);
      8  : SetPInt64(dst1, GetPInt64(dst1)xor PopInteger);
    Else
      dst1     := Data[Top - Size - Size];
      dst2     := Data[Top - Size];
      PartSize := 0;
      While Size > 0 Do
      Begin
        Case Size Of
          1 :
          Begin
            SetPByte(dst1, GetPByte(dst1)xor GetPByte(dst2));
            exit;
          End;
          2..3 :
          Begin
            SetPWord(dst1, GetPWord(dst1)xor GetPWord(dst2));
            PartSize := 2;
          End;
          4..7 :
          Begin
            SetPCardinal(dst1, GetPCardinal(dst1)xor GetPCardinal(dst2));
            PartSize := 4;
          End;
        Else
          SetPInt64(dst1, GetPInt64(dst1)xor GetPInt64(dst2));
          PartSize := 8;
        End;
        Inc(Cardinal(dst1), PartSize);
        Inc(Cardinal(dst2), PartSize);
      End;
    End;
  End;
End;

Procedure TRutisEngine.OpNot;
Var
  dst1 : Pointer;
  Size : Word;
Type
  PByteBool = ^Bytebool;
Begin
  Size := ScriptData.CurrCmd.P1;
  With ScriptData.Stack Do
  Begin
    dst1 := Data[Top - Size];
    Case Size Of
      1 :
      Begin
        If ScriptData.CurrCmd.P2 = 1 Then
          PByteBool(dst1)^ := not PByteBool(dst1)^
        Else
          SetPByte(dst1, not GetPByte(dst1));
      End;
      2  : SetPWord(dst1, not GetPWord(dst1));
      4  : SetPCardinal(dst1, not GetPCardinal(dst1));
      8  : SetPInt64(dst1, not GetPInt64(dst1));
    Else
      dst1 := Data[Top - Size];
      While Size > 0 Do
      Begin
        Case Size Of
          1 :
          Begin
            SetPByte(dst1, not GetPByte(dst1));
            exit;
          End;
          2..3 :
          Begin
            SetPWord(dst1, not GetPWord(dst1));
            Inc(Cardinal(dst1), 2);
          End;
          4..7 :
          Begin
            SetPCardinal(dst1, not GetPCardinal(dst1));
            Inc(Cardinal(dst1), 4);
          End;
        Else
          SetPInt64(dst1, not GetPInt64(dst1));
          Inc(Cardinal(dst1), 8);
        End;
      End;
    End;
  End;
End;

//===================================

Procedure TRutisEngine.OpExt;
Var
  i, j       : Integer;
  VParams    : TVariantArray;
  RParams    : TRutisParamInfoArray;
  PParams    : Array Of Pointer;
  VRes       : Variant;
  RRes       : TRutisParamInfo;
  PRes       : Cardinal;
  ExtMethod  : TRutisExtMethodType;
  DllMethod  : TRutisDllMethodType;
  hDll       : THandle;
Begin
  If (ScriptData.CurrCmd.P1 < 0) or (ScriptData.CurrCmd.P1 > high(ScriptData.MethodTable)) Then
  Begin
    ScriptMessage('Could not find Method-Info entry in Method-Table');
    exit;
  End;
  ExtMethod := TRutisExtMethodType(ScriptData.MethodTable[ScriptData.CurrCmd.P1]);
  DllMethod := TRutisDllMethodType(ExtMethod);

  //If (ExtMethod is TRutisExtMethodType) then
  If (ExtMethod.ClassType = TRutisExtMethodType) Then
  Begin
    If Assigned(ExtMethod.VariMethod) Then
    Begin
      SetLength(VParams, length(ExtMethod.Params));
      j := ScriptData.Stack.Top - ExtMethod.ParamsSize;
      For i := 0 To high(VParams) Do
      Begin
        VParams[i] := ScriptData.Stack.ReadToVariant(
          ExtMethod.Params[i].InternalType, j);
        j := j + ExtMethod.Params[i].Size;
      End;

      ScriptData.Stack.Pop(ExtMethod.ParamsSize);

      ExtMethod.VariMethod(@VParams, @VRes);

      If ExtMethod.IsFunction Then
      Begin
        j := ScriptData.Stack.Top - ExtMethod.MethodResult.Size;
        ScriptData.Stack.WriteFromVariant(ExtMethod.MethodResult.InternalType, j, VRes);
        If ExtMethod.MethodResult.InternalType = intAString Then
          ScriptData.Stack.AddFreeData(j, True);
      End;

      If ExtMethod.IsFunction and (ScriptData.CurrCmd.P2 = 1) Then
        ScriptData.Stack.Pop(ExtMethod.MethodResult.Size);
    End
    Else
    Begin
      SetLength(RParams, length(ExtMethod.Params));
      j := ScriptData.Stack.Top - ExtMethod.ParamsSize;
      For i := 0 To high(RParams) Do
      Begin
        RParams[i].Adr := j;
        RParams[i].Data := ScriptData.Stack.GetByte(j);
        RParams[i].Size := ExtMethod.Params[i].Size;
        RParams[i].IntType := ExtMethod.Params[i].InternalType;
        j := j + ExtMethod.Params[i].Size;
      End;

      If ExtMethod.IsFunction Then
      Begin
        j         := ScriptData.Stack.Top - ExtMethod.ParamsSize - ExtMethod.MethodResult.Size;
        RRes.Adr  := j;
        RRes.Data := ScriptData.Stack.GetByte(j);
        RRes.Size := ExtMethod.MethodResult.Size;
        RRes.IntType := ExtMethod.MethodResult.InternalType;
        If RRes.IntType = intAString Then
          ScriptData.Stack.AddFreeData(j, True);
      End;

      ExtMethod.StackMethod(@RParams, @RRes);

      ScriptData.Stack.Pop(ExtMethod.ParamsSize);

      If ExtMethod.IsFunction and (ScriptData.CurrCmd.P2 = 1) Then
        ScriptData.Stack.Pop(ExtMethod.MethodResult.Size);
    End;
    exit;
  End;
  {$ifndef FPC}
  If (DllMethod is TRutisDllMethodType) Then
  Begin
    SetLength(PParams, length(DllMethod.Params));
    j := ScriptData.Stack.Top - DllMethod.ParamsSize;
    For i := 0 To high(PParams) Do
    Begin
      PParams[i] := PPointer(ScriptData.Stack.GetCardinal(j))^;
      j := j + DllMethod.Params[i].Size;
    End;

    If DllMethod.ProcAddress = nil Then
    Begin
    //Get Libary
      hDll := GetModuleHandle(PChar(DllMethod.DllName));
      If hDll = 0 Then
        hDll := LoadLibrary(PChar(DllMethod.DllName));
      //Get Proc-Address
      If hDll <> 0 Then
        DllMethod.ProcAddress := GetProcAddress(hDll, PChar(DllMethod.ProcName));
    End;
    //PParams[0] := @j;
    DynamicDllCall(DllMethod.ProcAddress, DllMethod.IsFunction, PRes, PParams);
    //OnWrite(IntToStr(j));

    ScriptData.Stack.Pop(DllMethod.ParamsSize);

    If DllMethod.IsFunction and (ScriptData.CurrCmd.P2 <> 1) Then
    Begin
      j := ScriptData.Stack.Top - DllMethod.ParamsSize - DllMethod.MethodResult.Size;
      CopyStackData(@PRes, ScriptData.Stack.GetByte(j), DllMethod.MethodResult.Size);
      {If DllMethod.ResultType.InternalType = intAString then
      begin
        New(PStr);
        PPAnsiString(RRes.Data)^ := PStr;
        ScriptData.Stack.AddFreeData(j, true);
      end;}
    End;

    If DllMethod.IsFunction and (ScriptData.CurrCmd.P2 = 1) Then
      ScriptData.Stack.Pop(DllMethod.MethodResult.Size);
  End;
  {$endif}
End;

Procedure TRutisEngine.OpWri;
Var str  : String;
Begin
  If not Assigned(OnWrite) Then exit;
  Case TRutisIntType(ScriptData.CurrCmd.P1) Of
    intByte  : str     := IntToStr(ScriptData.Stack.PopByte);
    intWord  : str     := IntToStr(ScriptData.Stack.PopWord);
    intCardinal,
    IntPointer,
    intArray  : str    := IntToStr(ScriptData.Stack.PopCardinal);
    intShortInt  : str := IntToStr(ScriptData.Stack.PopShortInt);
    intSmallint  : str := IntToStr(ScriptData.Stack.PopSmallint);
    intInteger  : str  := IntToStr(ScriptData.Stack.PopInteger);
    intSingle  : str   := FloatToStr(ScriptData.Stack.PopSingle);
    intDouble  : str   := FloatToStr(ScriptData.Stack.PopDouble);
    intExtended  : str := FloatToStr(ScriptData.Stack.PopExtended);
    intAChar  : str    := ScriptData.Stack.PopAChar;
    intAString  : str  := Ansistring(ScriptData.Stack.PopString);
    intWChar  : str    := ScriptData.Stack.PopWChar;
    intWString  : str  := WideString(ScriptData.Stack.PopString);
  End;
  OnWrite(str);
End;

//===================================

Procedure TRutisEngine.OpCall;
Var NewLine  : Cardinal;
Begin
  If ScriptData.CurrCmd.P1 < 0 Then
    NewLine := ScriptData.Stack.PopCardinal - 1
  Else
    NewLine := ScriptData.CurrCmd.P1 - 1;
  // first Save last States
  SetLength(ScriptData.CallStack, length(ScriptData.CallStack) + 1);
  ScriptData.CallStack[high(ScriptData.CallStack)].CallLine := ScriptData.CodeLine;   //Save CallLine
  ScriptData.CallStack[high(ScriptData.CallStack)].StackBase := ScriptData.Stack.Top;  //Set new StackBase
  ScriptData.CallStack[high(ScriptData.CallStack)].Level := ScriptData.CurrCmd.P2; //Set Method-Level
  // change line
  ScriptData.CodeLine := NewLine;
End;

Procedure TRutisEngine.OpRet;
Begin
  If length(ScriptData.CallStack) = 0 Then
  Begin
    ScriptData.CodeLine := -1;
    exit;
  End;
  // Reset old States
  ScriptData.CodeLine := ScriptData.CallStack[high(ScriptData.CallStack)].CallLine;
  ScriptData.Stack.PopTo(ScriptData.CallStack[high(ScriptData.CallStack)].StackBase);
  //ScriptData.Stack.Top      := ScriptData.CallStack[high(ScriptData.CallStack)].StackBase;
  SetLength(ScriptData.CallStack, length(ScriptData.CallStack) - 1);
  //PopStack for Parameters
  ScriptData.Stack.Pop(ScriptData.CurrCmd.P1);
End;

Procedure TRutisEngine.OpJmp;
Begin
  ScriptData.CodeLine := ScriptData.CurrCmd.P1 - 1;
End;

Procedure TRutisEngine.OpJZ;
Var bool  : Boolean;
Begin
  Case TRutisIntType(ScriptData.CurrCmd.P2) Of
    intBoolean,
    intByte  : Bool := ScriptData.Stack.PopByte = 0;
    intWord  : Bool := ScriptData.Stack.PopWord = 0;
    intCardinal  : Bool := ScriptData.Stack.PopCardinal = 0;
    intShortInt  : Bool := ScriptData.Stack.PopShortInt = 0;
    intSmallint  : Bool := ScriptData.Stack.PopSmallint = 0;
    intInteger  : Bool := ScriptData.Stack.PopInteger = 0;
    intSingle  : Bool := ScriptData.Stack.PopSingle = 0;
    intDouble  : Bool := ScriptData.Stack.PopDouble = 0;
    intExtended  : Bool := ScriptData.Stack.PopExtended = 0;
  Else
    Bool := False;
  End;
  If bool Then
    ScriptData.CodeLine := ScriptData.CurrCmd.P1 - 1;
End;

Procedure TRutisEngine.OpJL;
Var bool  : Boolean;
Begin
  Bool := False;
  Case TRutisIntType(ScriptData.CurrCmd.P2) Of
    intByte,
    intBoolean  : ScriptData.Stack.PopByte;
    intWord  : ScriptData.Stack.PopWord;
    intCardinal  : ScriptData.Stack.PopCardinal;
    intShortInt  : Bool := ScriptData.Stack.PopShortInt < 0;
    intSmallint  : Bool := ScriptData.Stack.PopSmallint < 0;
    intInteger  : Bool  := ScriptData.Stack.PopInteger < 0;
    intSingle  : Bool   := ScriptData.Stack.PopSingle < 0;
    intDouble  : Bool   := ScriptData.Stack.PopDouble < 0;
    intExtended  : Bool := ScriptData.Stack.PopExtended < 0;
  End; //}
  If bool Then
    ScriptData.CodeLine := ScriptData.CurrCmd.P1 - 1;
End;

Procedure TRutisEngine.OpJG;
Var bool  : Boolean;
Begin
  Bool := True;
  Case TRutisIntType(ScriptData.CurrCmd.P2) Of
    intByte,
    intBoolean  : ScriptData.Stack.PopByte;
    intWord  : ScriptData.Stack.PopWord;
    intCardinal  : ScriptData.Stack.PopCardinal;
    intShortInt  : Bool := ScriptData.Stack.PopShortInt > 0;
    intSmallint  : Bool := ScriptData.Stack.PopSmallint > 0;
    intInteger  : Bool  := ScriptData.Stack.PopInteger > 0;
    intSingle  : Bool   := ScriptData.Stack.PopSingle > 0;
    intDouble  : Bool   := ScriptData.Stack.PopDouble > 0;
    intExtended  : Bool := ScriptData.Stack.PopExtended > 0;
  End;
  If bool Then
    ScriptData.CodeLine := ScriptData.CurrCmd.P1 - 1;
End;

Procedure TRutisEngine.OpJLZ;
Var bool  : Boolean;
Begin
  Bool := False;
  Case TRutisIntType(ScriptData.CurrCmd.P2) Of
    intByte,
    intBoolean  : Bool := ScriptData.Stack.PopByte = 0;
    intWord  : Bool := ScriptData.Stack.PopWord = 0;
    intCardinal  : Bool := ScriptData.Stack.PopCardinal = 0;
    intShortInt  : Bool := ScriptData.Stack.PopShortInt <= 0;
    intSmallint  : Bool := ScriptData.Stack.PopSmallint <= 0;
    intInteger  : Bool  := ScriptData.Stack.PopInteger <= 0;
    intSingle  : Bool   := ScriptData.Stack.PopSingle <= 0;
    intDouble  : Bool   := ScriptData.Stack.PopDouble <= 0;
    intExtended  : Bool := ScriptData.Stack.PopExtended <= 0;
  End; //}
  If bool Then
    ScriptData.CodeLine := ScriptData.CurrCmd.P1 - 1;
End;

Procedure TRutisEngine.OpJGZ;
Var bool  : Boolean;
Begin
  Bool := True;
  Case TRutisIntType(ScriptData.CurrCmd.P2) Of
    intByte,
    intBoolean  : ScriptData.Stack.PopByte;
    intWord  : ScriptData.Stack.PopWord;
    intCardinal  : ScriptData.Stack.PopCardinal;
    intShortInt  : Bool := ScriptData.Stack.PopShortInt >= 0;
    intSmallint  : Bool := ScriptData.Stack.PopSmallint >= 0;
    intInteger  : Bool  := ScriptData.Stack.PopInteger >= 0;
    intSingle  : Bool   := ScriptData.Stack.PopSingle >= 0;
    intDouble  : Bool   := ScriptData.Stack.PopDouble >= 0;
    intExtended  : Bool := ScriptData.Stack.PopExtended >= 0;
  End;
  If bool Then
    ScriptData.CodeLine := ScriptData.CurrCmd.P1 - 1;
End;

Procedure TRutisEngine.OpOpr;
Var
  Val1, Val2  : Variant;
Begin
  Case TRutisIntType(ScriptData.CurrCmd.P3) Of
    intByte,
    intEnum  : Val2     := ScriptData.Stack.PopByte;
    intAChar  : Val2    := ScriptData.Stack.PopByte; //PopAChar
    intBoolean  : Val2  := Boolean(ScriptData.Stack.PopByte);
    intWord  : Val2     := ScriptData.Stack.PopWord;
    intCardinal,
    intPointer,
    intArray  : Val2    := ScriptData.Stack.PopCardinal;
    intAString  : Val2  := Ansistring(ScriptData.Stack.PopString);
    intShortInt  : Val2 := ScriptData.Stack.PopShortInt;
    intSmallint  : Val2 := ScriptData.Stack.PopSmallint;
    intInteger  : Val2  := ScriptData.Stack.PopInteger;
    intSingle  : Val2   := ScriptData.Stack.PopSingle;
    intDouble  : Val2   := ScriptData.Stack.PopDouble;
    intExtended  : Val2 := ScriptData.Stack.PopExtended;
  Else
    ScriptMessage('Comparison Error');
  End;

  Case TRutisIntType(ScriptData.CurrCmd.P2) Of
    intByte,
    intEnum  : Val1     := ScriptData.Stack.PopByte;
    intAChar  : Val1    := ScriptData.Stack.PopByte; //PopAChar
    intBoolean  : Val1  := Boolean(ScriptData.Stack.PopByte);
    intWord  : Val1     := ScriptData.Stack.PopWord;
    intCardinal,
    intPointer,
    intArray  : Val1    := ScriptData.Stack.PopCardinal;
    intAString  : Val1  := Ansistring(ScriptData.Stack.PopString);
    intShortInt  : Val1 := ScriptData.Stack.PopShortInt;
    intSmallint  : Val1 := ScriptData.Stack.PopSmallint;
    intInteger  : Val1  := ScriptData.Stack.PopInteger;
    intSingle  : Val1   := ScriptData.Stack.PopSingle;
    intDouble  : Val1   := ScriptData.Stack.PopDouble;
    intExtended  : Val1 := ScriptData.Stack.PopExtended;
  Else
    ScriptMessage('Comparison Error');
  End;

  Case TOperatorCode(ScriptData.CurrCmd.P1) Of
    ocEqual  : Val1   := (Val1 = Val2);
    ocGreater  : Val1 := (Val1 > Val2);
    ocLess  : Val1    := (Val1 < Val2);
    ocGEqual  : Val1  := (Val1 >= Val2);
    ocLEqual  : Val1  := (Val1 <= Val2);
    ocUnequal  : Val1 := (Val1 <> Val2);
  End;

  ScriptData.Stack.PushByte(Byte(Boolean(Val1)));
End;

{$ENDREGION}

//===================================
//============ RUN ==================
//===================================

Procedure TRutisEngine.ExecuteCMD(CMD : TRutisScriptCmd);
Begin
  ScriptData.CurrCmd := CMD;
  Case ScriptData.CurrCmd.Cmd Of
    _gen  : OpGen;

    _gen1  : OpGen1;
    _gen2  : OpGen2;
    _gen4  : OpGen4;

    _Ptr  : OpPtr;
    _PtrP  : OpPtrP;
    _CPtr  : OpCheckPtr;
    _at  : OpAt;
    _at2  : OpAt2;

    _lodR  : OpLodR;
    _RStr  : OpResStr;
    _lod  : OpLod;
    _sto  : OpSto;
    _mov  : OpMov;

    _lodp  : OpLodP;
    _stop  : OpStoP;

    _CopyAStr  : OpCopyAStr;
    _CopyWStr  : OpCopyWStr;
    _StoAStr  : OpStoAStr;
    _StoWStr  : OpStoWStr;

    _SMem  : OpScaleMem;
    _MemS  : OpMemSize;

    _GASL  : OpGetAStrLength;
    _SASL  : OpSetAStrLength;
    _GWSL  : OpGetWStrLength;
    _SWSL  : OpSetWStrLength;

    _inc  : OpInc;

    _conv  : OpConv;
    _add  : OpAdd;
    _sub  : OpSub;
    _mult  : OpMult;
    _div  : OpDiv;
    _mod  : OpModulo;
    _ets  : OpEnumToSet;

    _call  : OpCall;
    _ret  : OpRet;

    _jmp  : OpJmp;
    _opr  : OpOpr;
    _JZ  : OpJZ;
    _JL  : OpJL;
    _JG  : OpJG;
    _JLZ  : OpJLZ;
    _JGZ  : OpJGZ;

    _and  : OpAnd;
    _or  : OpOr;
    _xor  : OpXOr;
    _not  : OpNot;

    _wri  : OpWri;
    _ext  : OpExt;

    _pupo  : If ScriptData.CurrCmd.P1 >= 0 Then
        ScriptData.Stack.Push(ScriptData.CurrCmd.P1)
      Else
        ScriptData.Stack.Pop(-ScriptData.CurrCmd.P1);
  End;
End;

Procedure TRutisEngine.ResetScriptState;
Begin
  If ScriptData = nil Then exit;
  ScriptData.Stack.Clear;
  SetLength(ScriptData.CallStack, 0);
  ScriptData.FreeExtData;

  ScriptData.CodeLine := 0;
  ScriptData.Running  := False;
  ScriptData.Paused   := False;
  fScriptError        := False;

  SetCurrentDir(ScriptFilePath);
End;

Procedure TRutisEngine.Run;
Var
  ProcessCount  : Integer;
Begin
  If ScriptData = nil Then exit;
  If ScriptData.Running and not ScriptData.Paused Then
  Begin
    ScriptData.Running := False;
    exit;
  End;
  If not ScriptData.Compiled Then
    exit;

  If not ScriptData.Running Then
    ResetScriptState;

  {$ifdef DEBUG}OutputDebugString('TRutisEngine.Run  -  Running Script');{$endif}

  ProcessCount       := 0;
  ScriptData.Running := True;
  ScriptData.Paused  := False;
  Repeat
    Inc(ProcessCount);
    If OptProcessTimer and (ProcessCount > OptProcessTimerCount) Then
    Begin
      If Assigned(OnCodeTimer) Then
        OnCodeTimer(self)
      Else
        Application.ProcessMessages;
      ProcessCount := 0;
    End;
    If not ScriptData.Running or ScriptData.Paused Then break;

    If ScriptData.CodeLine > high(ScriptData.Code) Then exit;
    ExecuteCMD(ScriptData.Code[ScriptData.CodeLine]);
    ScriptData.CodeLine := ScriptData.CodeLine + 1;

    If GetBreakpointToCMD(ScriptData.CodeLine) > -1 Then
      ScriptData.Paused := True;

  Until (ScriptData.CodeLine <= 0) or (ScriptData.CodeLine > high(ScriptData.Code)) or fScriptError;

  If fScriptError Then
  Begin
    ScriptData.Running := False;
    exit;
    {$ifdef DEBUG}OutputDebugString('TRutisEngine.Run  -  Error while running Script');{$endif}
  End;
  {$ifdef DEBUG}
  If ScriptData.Paused Then
    OutputDebugString('TRutisEngine.Run  -  Script paused')
  Else
    OutputDebugString('TRutisEngine.Run  -  Script stopped');
  {$endif}

  If (ScriptData.CodeLine <= 0) or (ScriptData.CodeLine > high(ScriptData.Code)) Then
    ScriptData.Running := False;
End;

Procedure TRutisEngine.Stop;
Begin
  If ScriptData = nil Then exit;
  ScriptData.Running := False;
  ScriptData.Paused  := False;
End;

Procedure TRutisEngine.Pause;
Begin
  If ScriptData = nil Then exit;
  If not ScriptData.Running Then exit;
  ScriptData.Paused := True;
End;

Procedure TRutisEngine.StepCmd;
Begin
  If ScriptData = nil Then exit;
  If not ScriptData.Running Then
    ResetScriptState;
  ScriptData.Running := True;
  ScriptData.Paused  := True;

  {$ifdef DEBUG}OutputDebugString('TRutisEngine.StepCmd  -  Executing CMD');{$endif}

  If ScriptData.CodeLine > high(ScriptData.Code) Then exit;
  ExecuteCMD(ScriptData.Code[ScriptData.CodeLine]);
  ScriptData.CodeLine := ScriptData.CodeLine + 1;

  If fScriptError Then
  Begin
    ScriptData.Running := False;
    exit;
  End;

  If (ScriptData.CodeLine <= 0) or (ScriptData.CodeLine > high(ScriptData.Code)) Then
    ScriptData.Running := False;
End;

Procedure TRutisEngine.StepLine;
Var
  LastCMD,
  ProcessCount,
  StartCodeLine  : Integer;
Begin
  If ScriptData = nil Then exit;
  If not ScriptData.Running Then
    ResetScriptState;
  ScriptData.Running := True;
  ScriptData.Paused  := True;

  {$ifdef DEBUG}OutputDebugString('TRutisEngine.StepCmd  -  Executing Line');{$endif}

  StartCodeLine := ScriptData.Code[ScriptData.CodeLine].CodeLine;
  LastCMD       := ScriptData.CodeLine;
  While True Do
  Begin
    If (ScriptData.Code[LastCMD].Cmd = _ret) Then
    Begin
      LastCMD := -1;
      break;
    End;
    If (ScriptData.Code[LastCMD].Cmd = _call) Then
    Begin
      LastCMD := LastCMD + 1;
      break;
    End;
    Inc(LastCMD);
    If (ScriptData.Code[LastCMD].CodeLine <> StartCodeLine) Then
      break;
  End;

  ScriptData.Paused := False;
  ProcessCount      := 0;
  Repeat
    Inc(ProcessCount);
    If OptProcessTimer and (ProcessCount > OptProcessTimerCount) Then
    Begin
      If Assigned(OnCodeTimer) Then
        OnCodeTimer(self)
      Else
        Application.ProcessMessages;
      ProcessCount := 0;
    End;

    ExecuteCMD(ScriptData.Code[ScriptData.CodeLine]);
    ScriptData.CodeLine := ScriptData.CodeLine + 1;

    If GetBreakpointToCMD(ScriptData.CodeLine) > -1 Then
      ScriptData.Paused := True;

  Until (ScriptData.CodeLine <= 0) or
    (ScriptData.CodeLine > high(ScriptData.Code)) or
    fScriptError or
    (ScriptData.CodeLine = LastCMD) or not ScriptData.Running or
    ScriptData.Paused;

  ScriptData.Paused := True;

  If fScriptError Then
  Begin
    ScriptData.Running := False;
    exit;
  End;

  If (ScriptData.CodeLine <= 0) or
    (ScriptData.CodeLine > high(ScriptData.Code)) Then
    ScriptData.Running := False;
End;

//===================================

Procedure TRutisEngine.ToggleBreakpoint(Line : Integer);
Var
  i  : Integer;
Begin
  i := GetBreakpoint(Line);
  If i > -1 Then
  Begin
    Breakpoints[i] := Breakpoints[high(Breakpoints)];
    SetLength(Breakpoints, length(Breakpoints) - 1);
    {$ifdef DEBUG}OutputDebugString('TRutisEngine.ToggleBreakpoint  -  Breakpoint removed at line ' + IntToStr(Line));{$endif}
  End
  Else
  Begin
    SetLength(Breakpoints, length(Breakpoints) + 1);
    Breakpoints[high(Breakpoints)].Line := Line;
    {$ifdef DEBUG}OutputDebugString('TRutisEngine.ToggleBreakpoint  -  Breakpoint set at line ' + IntToStr(Line));{$endif}
  End;
End;

Function TRutisEngine.GetBreakpoint(Line : Integer) : Integer;
Begin
  For Result := 0 To high(Breakpoints) Do
    If Breakpoints[Result].Line = Line Then
      exit;
  Result := -1;
End;

Function TRutisEngine.GetBreakpointToCmd(CMD : Integer) : Integer;
Begin
  For Result := 0 To high(Breakpoints) Do
    If (Breakpoints[Result].Line = ScriptData.Code[CMD].CodeLine) and
      (ScriptData.Code[CMD - 1].CodeLine <> ScriptData.Code[CMD].CodeLine) Then
      exit;
  Result := -1;
End;

//===================================

Function TRutisEngine.DebugVarValue(AVarDecl : TRutisVarDecl) : String;
Var
  I, addr  : Integer;
Begin
  If not (AVarDecl is TRutisVarDecl) Then exit;

  If AVarDecl.Level = 0 Then
    addr := AVarDecl.Address
  Else
    addr := GetStackLvlAddress(AVarDecl.Address, AVarDecl.Level);

  Result := '';
  If (AVarDecl.VarType is TRutisVarType) or
    (AVarDecl.VarType is TRutisEnumType) or
    (AVarDecl.VarType is TRutisMethodType) Then
  Begin
    If AVarDecl.isConst Then
    Begin
      If AVarDecl.VarType.InternalType = intAString Then
      Begin
        Result := PChar(ScriptData.Ressources.GetCardinal(Addr));
      End
      Else
        Result := ScriptData.Ressources.ReadToStr(AVarDecl.VarType.InternalType, Addr);
    End
    Else
      Result := ScriptData.Stack.ReadToStr(AVarDecl.VarType.InternalType, Addr);
  End
  Else
  If AVarDecl.VarType is TRutisStructType Then
    With TRutisStructType(AVarDecl.VarType) Do
    Begin
      Result := '[';
      For I := 0 To high(StructTypes) Do
      Begin
        If AVarDecl.isConst Then
          Result := Result +
            ScriptData.Ressources.ReadToStr(StructTypes[i].VarType.InternalType, Addr + StructTypes[i].Address)
        Else
          Result := Result +
            ScriptData.Stack.ReadToStr(StructTypes[i].VarType.InternalType, Addr + StructTypes[i].Address);

        If i < high(StructTypes) Then
          Result := Result + ',';
      End;
      Result := Result + ']';
    End
  Else
  If AVarDecl.VarType is TRutisPointerType Then
  Begin
    If AVarDecl.isConst Then
      Result := '@' + ScriptData.Ressources.ReadToStr(AVarDecl.VarType.InternalType, Addr)
    Else
      Result := '@' + ScriptData.Stack.ReadToStr(AVarDecl.VarType.InternalType, Addr);
    {try
    except
    end;}
  End
  Else
  If AVarDecl.VarType is TRutisArrayType Then
  Begin
    If AVarDecl.isConst Then
      Result := '@' + ScriptData.Ressources.ReadToStr(AVarDecl.VarType.InternalType, Addr)
    Else
      Result := '@' + ScriptData.Stack.ReadToStr(AVarDecl.VarType.InternalType, Addr);
  End;
End;

Function TRutisEngine.DebugVarName(AVarDecl : TRutisVarDecl) : String;
Begin
  If AVarDecl.VarType is TRutisArrayType Then
    Result := 'Array of ' + TRutisArrayType(AVarDecl.VarType).ArrayType.Name

  Else If AVarDecl.VarType is TRutisPointerType Then
    Result := '^' + TRutisPointerType(AVarDecl.VarType).PointerType.Name

  Else If AVarDecl.VarType is TRutisExtMethodType Then
  Begin
    If TRutisMethodType(AVarDecl.VarType).IsFunction Then
      Result := 'Function '
    Else
      Result := 'Procedure ';
    //Result := '^' + TRutisPointerType(AVarDecl.VarType).PointerType.Name
  End

  Else If AVarDecl.VarType is TRutisMethodType Then
  Begin
    If TRutisMethodType(AVarDecl.VarType).IsFunction Then
      Result := 'Ext-Function '
    Else
      Result := 'Ext-Procedure ';
    //Result := '^' + TRutisPointerType(AVarDecl.VarType).PointerType.Name
  End
  Else
    Result := AVarDecl.VarType.Name;
End;

//==============================================================================
//======================= Compiler =============================================
//==============================================================================

/// XXX TODO OMG WTF
{{$ifdef FPC}
function LoadLibraryA(lpLibFileName: LPCSTR): HINST; external KernelDLL name 'LoadLibraryA';
{$endif FPC}              }

Function TRutisEngine.RegisterExtDll(FileName : String) : Boolean;
Var i  : Integer;
Begin
  Result := False;

  If not FileExists(FileName) Then exit;

  i := length(fExtDlls);
  SetLength(fExtDlls, i + 1);

 // fExtDlls[i].DllHandle := LoadLibraryA(PAnsiChar(FileName));

  If fExtDlls[i].DllHandle = 0 Then
  Begin
    SetLength(fExtDlls, i);
    exit;
  End;

  With fExtDlls[i] Do
  Begin
 {   RegisterEXTMethods := TRegisterEXTMethods(GetProcAddress(DllHandle, 'RegisterEXTMethods'));
    StartScript := TScriptAction(GetProcAddress(DllHandle, 'StartScript'));
    StopScript := TScriptAction(GetProcAddress(DllHandle, 'StopScript'));
    PauseScript := TScriptAction(GetProcAddress(DllHandle, 'PauseScript'));
    Destroy := TScriptAction(GetProcAddress(DllHandle, 'Destroy'));            }
  End;

  fExtDlls[i].RegisterEXTMethods(self);

  Result := True;
End;

Procedure TRutisEngine.UnloadExtDlls;
Var i  : Integer;
Begin
 { For i := 0 To high(fExtDlls) Do
    FreeLibrary(fExtDlls[i].DllHandle);       }
  SetLength(fExtDlls, 0);
End;

//==============================================================================

Procedure TRutisEngine.GetByteCode(ByteCode : TStrings);
Var
  i, pos     : Integer;
  s          : String;
  ValSingle  : Boolean;
  sng        : Single;
Begin
  SetLength(ScriptData.Code, 0);
  For i := 0 To ByteCode.Count - 1 Do
  Begin
    If length(ByteCode.Strings[i]) = 0 Then
      continue;
    If (ByteCode.Strings[i][1] = '/') and (ByteCode.Strings[i][2] = '/') Then
      continue;
    s   := '';
    pos := 1;
    While (ByteCode.Strings[i][pos] <> ' ') and (pos <= length(ByteCode.Strings[i])) Do
    Begin
      s := s + ByteCode.Strings[i][pos];
      Inc(pos);
    End;

    SetLength(ScriptData.Code, Length(ScriptData.Code) + 1);
    With ScriptData.Code[high(ScriptData.Code)] Do
    Begin
      Cmd := StrToSym(s);
      If Cmd = _nocmd Then
      Begin
        ScriptMessage('Error reading ByteCode in line ' + IntToStr(i));
        exit;
      End;
    End;

    If pos >= length(ByteCode.Strings[i]) Then
      continue;

    s := '';
    While (ByteCode.Strings[i][pos] = ' ') and (pos < length(ByteCode.Strings[i])) Do
      Inc(pos);
    If pos > length(ByteCode.Strings[i]) Then
      continue;
    Repeat
      s := s + ByteCode.Strings[i][pos];
      Inc(pos);
    Until (ByteCode.Strings[i][pos] = ' ') or (pos > length(ByteCode.Strings[i]));

    ScriptData.Code[high(ScriptData.Code)].P1 := StrToInt(s);
    If pos > length(ByteCode.Strings[i]) Then
      continue;

    s         := '';
    ValSingle := False;
    While (ByteCode.Strings[i][pos] = ' ') and (pos < length(ByteCode.Strings[i])) Do
      Inc(pos);
    If pos > length(ByteCode.Strings[i]) Then
      continue;
    Repeat
      s := s + ByteCode.Strings[i][pos];
      If ByteCode.Strings[i][pos] = ',' Then
        ValSingle := True;
      Inc(pos);
    Until (ByteCode.Strings[i][pos] = ' ') or (pos > length(ByteCode.Strings[i]));
    If ValSingle Then
    Begin
      sng := StrToFloat(s);
      ScriptData.Code[high(ScriptData.Code)].P2 := PInteger(@sng)^;
    End
    Else
      ScriptData.Code[high(ScriptData.Code)].P2 := StrToInt(s);
  End;
End;

//==============================================================================

Function TRutisEngine.ExtMethodLinkToStr(ExtLink : TRutisExtMethodType; IncludeProc : Boolean) : String;
Var
  i  : Integer;
Begin
  Result := '';
  If IncludeProc Then
  Begin
    If ExtLink.IsFunction Then
      Result := 'Function  '
    Else
      Result := 'Procedure ';
  End;
  Result := Result + ExtLink.Method_Name;

  If length(ExtLink.Params) > 0 Then
  Begin
    Result := Result + ' (';

    //    For i := 0 To high(ExtLink.Params) - 1 Do
    //      Result := Result + ExtLink.Params[i].Name + '; ';
    //    Result := Result + ExtLink.Params[high(ExtLink.Params)].Name;
    For i := 0 To high(ExtLink.Params) - 1 Do
      Result := Result + ExtLink.Params[i].Name + '; ';
    Result := Result + ExtLink.Params[high(ExtLink.Params)].Name;

    Result := Result + ')';
  End;

  If ExtLink.IsFunction Then
    Result := Result + ': ' + ExtLink.MethodResult.Name + ';'
  Else
    Result := Result + ';';
End;

Function TRutisEngine.ShowExtMethodListWindow : TRutisExtMethodType;
Var
  Win  : TFExtMethodListInfo;
  i    : Integer;
Begin
  Result := nil;
  Win    := nil;
  Try
    Win := TFExtMethodListInfo.Create(nil);
    Win.Hide;

    For i := 0 To high(StaticDeclarations) Do
      If StaticDeclarations[i] is TRutisExtMethodType Then
      Begin
        SetLength(Win.ExtMethods, length(Win.ExtMethods) + 1);
        Win.ExtMethods[high(Win.ExtMethods)].ExtMethodType := StaticDeclarations[i];
        Win.ExtMethods[high(Win.ExtMethods)].Name        := ExtMethodLinkToStr(TRutisExtMethodType(StaticDeclarations[i]), False);
        Win.ExtMethods[high(Win.ExtMethods)].Description := TRutisExtMethodType(StaticDeclarations[i]).Description;
        Win.ExtMethods[high(Win.ExtMethods)].IsFunction  := TRutisExtMethodType(StaticDeclarations[i]).IsFunction;
      End;

    Win.ShowModal;
    Result := TRutisExtMethodType(Win.ResultExtMethod);

  Finally
    Win.Free;
  End;
End;

//==============================================================================


End.

