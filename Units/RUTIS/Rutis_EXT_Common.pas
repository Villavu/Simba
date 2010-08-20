Unit Rutis_EXT_Common;

Interface

{$i Delphi_Versions.inc}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

Uses
  Windows, Forms, Math, Dialogs, SysUtils,
  Rutis_Stack, Rutis_Defs, Rutis_Classes, TypInfo;

Type
  PObject    = ^TObject;
  PSearchRec = ^TSearchRec;
  PClass     = ^TClass;

Procedure RegisterEXTMethods(Engine : TRutisEngineBase);

Implementation

//==============================================================================
//======== Strings
//==============================================================================
{$REGION 'Strings'}

Procedure _ToStr(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Case Params^[0].IntType Of
//    intCardinal : PAnsiString(Result^.Data)^ := IntToStr(GetPCardinal(Params^[0].Data));
//    intInteger  : PAnsiString(Result^.Data)^ := IntToStr(GetPInteger(Params^[0].Data));
//    intExtended : PAnsiString(Result^.Data)^ := FloatToStr(GetPExtended(Params^[0].Data));

    intCardinal : SetPAnsiString(Result^.Data, IntToStr(GetPCardinal(Params^[0].Data)));
    intInteger  : SetPAnsiString(Result^.Data, IntToStr(GetPInteger(Params^[0].Data)));
    intExtended : SetPAnsiString(Result^.Data, FloatToStr(GetPExtended(Params^[0].Data)));

//    intCardinal : PAnsiString(Result^.Data)^ := IntToStr(PCardinal(Params^[0].Data)^);
//    intInteger  : PAnsiString(Result^.Data)^ := IntToStr(PInteger(Params^[0].Data)^);
//    intExtended : PAnsiString(Result^.Data)^ := FloatToStr(PExtended(Params^[0].Data)^);
  End;
End;

Procedure _ToInt(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Try
    SetPInteger(Result^.Data, StrToInt(AnsiString(GetPPointer(Params^[0].Data))));
  Except
    SetPInteger(Result^.Data, 0);
  End;
End;

Procedure _ToFloat(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Try
    SetPExtended(Result^.Data, StrToFloat(AnsiString(GetPPointer(Params^[0].Data))));
  Except
    SetPExtended(Result^.Data, 0);
  End;
End;

Procedure _SetDecimalSeparator(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  DecimalSeparator := PAnsiChar(Params^[0].Data)^;
End;

Procedure _GetDecimalSeparator(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PAnsiChar(Result^.Data)^ := DecimalSeparator;
End;

Procedure _Chr(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PAnsiChar(Result^.Data)^ := Chr(PByte(Params^[0].Data)^);
End;

Procedure _Ord(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PByte(Result^.Data)^ := Ord(PAnsiChar(Params^[0].Data)^);
End;

Procedure _UpperCase(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPAnsiString(Result^.Data, UpperCase(AnsiString(GetPPointer(Params^[0].Data))));
End;

Procedure _LowerCase(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPAnsiString(Result^.Data, LowerCase(AnsiString(GetPPointer(Params^[0].Data))));
End;

Procedure _StringReplace(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Type PReplaceFlags = ^TReplaceFlags;
Begin
  PAnsiString(Result^.Data)^ := StringReplace(
    AnsiString(GetPPointer(Params^[0].Data)),
    AnsiString(GetPPointer(Params^[1].Data)),
    AnsiString(GetPPointer(Params^[2].Data)),
    {$ifdef WinCe}unaligned({$endif}PReplaceFlags(Params^[3].Data)^{$ifdef WinCe}){$endif});
End;

Procedure _ExtractFilePath(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPAnsiString(Result^.Data, ExtractFilePath(AnsiString(GetPPointer(Params^[0].Data))));
End;

Procedure _ExtractFileName(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPAnsiString(Result^.Data, ExtractFileName(AnsiString(GetPPointer(Params^[0].Data))));
End;

Procedure _ExtractFileExt(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPAnsiString(Result^.Data, ExtractFileExt(AnsiString(GetPPointer(Params^[0].Data))));
End;

Procedure _ExpandFileName(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPAnsiString(Result^.Data, ExpandFileName(AnsiString(GetPPointer(Params^[0].Data))));
End;

{$ENDREGION}

//======== Internal ========
{$REGION 'Internal'}

Procedure _InheritsClass(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
var
  AObject : TObject;
  AClass  : TClass;
Begin
  AObject := TObject(GetPPointer(Params^[0].Data));
  If AObject = nil then exit;
  AClass  := TClass(GetPPointer(Params^[1].Data));
  If AClass = nil then exit;
  PBoolean(Result^.Data)^ := AObject is AClass;
End;

{$ENDREGION}

//======== Files ========
{$REGION 'Files'}

Procedure _FileExists(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := FileExists(AnsiString(GetPPointer(Params^[0].Data)));
End;

Procedure _FindFirst(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPInteger(Result^.Data, FindFirst(
    AnsiString(GetPPointer(Params^[0].Data)),
    GetPInteger(Params^[1].Data),
    PSearchRec(Params^[2].Data)^));
End;

Procedure _FindNext(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPInteger(Result^.Data, FindNext(PSearchRec(Params^[0].Data)^));
End;

Procedure _FindClose(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  FindClose(PSearchRec(Params^[0].Data)^);
End;

{$ENDREGION}

//==============================================================================
//======== Windows
//==============================================================================
{$REGION 'Windows'}

Procedure _Sleep(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Sleep(GetPInteger(Params^[0].Data));
End;

Procedure _Delay(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  GTC  : Cardinal;
Begin
  GTC := GetTickCount + GetPInteger(Params^[0].Data);
  While GTC >= GetTickCount Do
  Begin
    //Application.ProcessMessages;
    sleep(1);
  End;
End;

Procedure _RVal(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PByte(Result^.Data)^ := GetRValue(GetPCardinal(Params^[0].Data));
End;

Procedure _GVal(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PByte(Result^.Data)^ := GetRValue(GetPCardinal(Params^[0].Data));
End;

Procedure _BVal(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PByte(Result^.Data)^ := GetRValue(GetPCardinal(Params^[0].Data));
End;

Procedure _RGB(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPCardinal(Result^.Data, Cardinal(rgb(PByte(Params^[0].Data)^, PByte(Params^[1].Data)^, PByte(Params^[2].Data)^)));
End;

{$ifndef FPC}
Procedure _DynCallDLLProc(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Type
  TPointerArray = Array Of Pointer;
  PPointerArray = ^TPointerArray;
Var
  DLLName     : PAnsiString;
  ProcName    : PAnsiString;
  ProcParams  : TPointerArray;
  HasResult   : Boolean;
  ProcResult  : Cardinal;
Begin
  DLLName    := PAnsiString(Params^[0].Data);
  ProcName   := PAnsiString(Params^[1].Data);
  HasResult  := PBoolean(Params^[2].Data)^;
  ProcParams := PPointerArray(PPointer(Params^[3].Data))^;
  If length(ProcParams) > 100 Then exit;
  If not DynamicDllCall(DLLName^, ProcName^, HasResult, ProcResult, ProcParams) Then
  Begin
  //ShowMessage('Function could not be found!');
    PCardinal(Result^.Data)^ := 0;
  End
  Else
  Begin
    PCardinal(Result^.Data)^ := ProcResult;
  End;
End;
{$endif}

Procedure _GetTickCount(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPCardinal(Result^.Data, GetTickCount);
End;

Procedure _GetAsyncKeyState(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := Boolean(GetAsyncKeyState(GetPWord(Params^[0].Data)) <> 0);
End;

{$ifndef WINCE}
Procedure _Beep(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Windows.Beep(GetPCardinal(Params^[0].Data), GetPCardinal(Params^[1].Data));
End;
{$endif WINCE}

Procedure _InputDlg(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  str    : AnsiString;
  StrOut : Pointer;
Begin
  str    := AnsiString(GetPPointer(Params^[1].Data));
  If not InputQuery('Input', AnsiString(GetPPointer(Params^[0].Data)), str) Then
    SetPAnsiString(Result^.Data, '')
  else
    SetPAnsiString(Result^.Data, Str);
End;

{$ENDREGION}

//==============================================================================
//======== Properties
//==============================================================================
{$REGION 'Properties'}

Procedure _SetIntProperty(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Obj       : TObject;
  PropInfo  : PPropInfo;
  PropName      : AnsiString;
Begin
  Obj := TObject(GetPPointer(Params^[0].Data));
  PropName := AnsiString(GetPPointer(Params^[1].Data));
  Try
    PropInfo := GetPropInfo(Obj, PropName);
    If PropInfo <> nil Then
      SetOrdProp(Obj, PropInfo, GetPInteger(Params^[2].Data));
  Except
  End;
End;

Procedure _GetIntProperty(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Obj       : TObject;
  PropInfo  : PPropInfo;
  PropName      : AnsiString;
Begin
  Obj := TObject(GetPPointer(Params^[0].Data));
  PropName := AnsiString(GetPPointer(Params^[1].Data));
  Try
    PropInfo := GetPropInfo(Obj, PropName);
    If PropInfo <> nil Then
      SetPInteger(Result^.Data, GetOrdProp(Obj, PropInfo));
  Except
  End;
End;

Procedure _SetBoolProperty(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Obj       : TObject;
  PropInfo  : PPropInfo;
  PropName      : AnsiString;
Begin
  Obj := TObject(GetPPointer(Params^[0].Data));
  PropName := AnsiString(GetPPointer(Params^[1].Data));
  Try
    PropInfo := GetPropInfo(Obj, PropName);
    If PropInfo <> nil Then
      SetOrdProp(Obj, PropInfo, PByte(Params^[2].Data)^);
  Except
  End;
End;

Procedure _GetBoolProperty(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Obj       : TObject;
  PropInfo  : PPropInfo;
  PropName      : AnsiString;
Begin
  Obj := TObject(GetPPointer(Params^[0].Data));
  PropName := AnsiString(GetPPointer(Params^[1].Data));
  Try
    PropInfo := GetPropInfo(Obj, PropName);
    If PropInfo <> nil Then
      PByte(Result^.Data)^ := GetOrdProp(Obj, PropInfo);
  Except
  End;
End;

Procedure _SetStrProperty(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Obj       : TObject;
  PropInfo  : PPropInfo;
  PropName      : AnsiString;
Begin
  Obj := TObject(GetPPointer(Params^[0].Data));
  PropName := AnsiString(GetPPointer(Params^[1].Data));
  Try
    PropInfo := GetPropInfo(Obj, PropName);
    If PropInfo <> nil Then
      SetStrProp(Obj, PropInfo, AnsiString(GetPPointer(Params^[2].Data)));
  Except
  End;
End;

Procedure _GetStrProperty(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  Obj       : TObject;
  PropInfo  : PPropInfo;
  PropName      : AnsiString;
Begin
  Obj := TObject(GetPPointer(Params^[0].Data));
  PropName := AnsiString(GetPPointer(Params^[1].Data));
  Try
    PropInfo := GetPropInfo(Obj, PropName);
    If PropInfo <> nil Then
      SetPAnsiString(Result^.Data, GetStrProp(Obj, PropInfo));
  Except
  End;
End;

{$ENDREGION}

//==============================================================================
//======== Math
//==============================================================================
{$REGION 'Maths'}

Procedure _Round(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPInteger(Result^.Data, round(GetPSingle(Params^[0].Data)));
End;

Procedure _abs(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  Case Params^[0].IntType Of
    intInteger : SetPInteger(Result^.Data, abs(GetPInteger(Params^[0].Data)));
    intSingle  : SetPSingle(Result^.Data, abs(GetPSingle(Params^[0].Data)));
    intDouble  : SetPDouble(Result^.Data, abs(GetPDouble(Params^[0].Data)));
  End;
End;

Procedure _RandomF(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPDouble(Result^.Data, random);
End;

Procedure _Random(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPInteger(Result^.Data, random(GetPInteger(Params^[0].Data)));
End;

Procedure _sqr(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var d  : Single;
Begin
  d := GetPSingle(Params^[0].Data);
  SetPSingle(Result^.Data, d * d);
End;

Procedure _sqrt(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPSingle(Result^.Data, sqrt(GetPSingle(Params^[0].Data)));
End;

Procedure _sqrtvari(Params : PVariantArray; Result : PVariant);
Begin
  Result^ := sqrt(Params^[0]);
End;

Procedure _Sin(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPSingle(Result^.Data, Sin(GetPSingle(Params^[0].Data)));
End;

Procedure _Cos(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPSingle(Result^.Data, Cos(GetPSingle(Params^[0].Data)));
End;

Procedure _ArcSin(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPSingle(Result^.Data, ArcSin(GetPSingle(Params^[0].Data)));
End;

Procedure _ArcCos(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPSingle(Result^.Data, ArcCos(GetPSingle(Params^[0].Data)));
End;

Procedure _Arctan2(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  SetPSingle(Result^.Data, Arctan2(GetPSingle(Params^[0].Data), GetPSingle(Params^[1].Data)));
End;

{$ENDREGION}

//==============================================================================
//==============================================================================

Procedure RegisterEXTMethods(Engine : TRutisEngineBase);
Begin
  //======== Strings ========
  {$REGION 'Strings'}

  Engine.RegExtMethod('IntToStr',{$IfDef FPC}@{$EndIf}_ToStr, ['Integer'], 'String');
  Engine.RegExtMethod('FloatToStr',{$IfDef FPC}@{$EndIf}_ToStr, ['Extended'], 'String');
  Engine.RegExtMethod('StrToInt',{$IfDef FPC}@{$EndIf}_ToInt, ['String'], 'Integer');
  Engine.RegExtMethod('StrToFloat',{$IfDef FPC}@{$EndIf}_ToFloat, ['String'], 'Extended');
  Engine.RegExtMethod('DecimalSeparator',{$IfDef FPC}@{$EndIf}_GetDecimalSeparator, [], 'Char');
  Engine.RegExtMethod('DecimalSeparator',{$IfDef FPC}@{$EndIf}_SetDecimalSeparator, ['Char'], '');

  Engine.RegExtMethod('Chr',{$IfDef FPC}@{$EndIf}_Chr, ['Byte'], 'AnsiChar');
  Engine.RegExtMethod('Ord',{$IfDef FPC}@{$EndIf}_Ord, ['AnsiChar'], 'Byte');
  Engine.RegExtMethod('UpperCase',{$IfDef FPC}@{$EndIf}_UpperCase, ['AnsiString'], 'AnsiString');
  Engine.RegExtMethod('LowerCase',{$IfDef FPC}@{$EndIf}_LowerCase, ['AnsiString'], 'AnsiString');

  Engine.RegExtMethod('StringReplace',{$IfDef FPC}@{$EndIf}_LowerCase, ['AnsiString', 'AnsiString', 'AnsiString', 'Cardinal'], 'AnsiString');

  Engine.RegExtMethod('ExtractFilePath',{$IfDef FPC}@{$EndIf}_ExtractFilePath, ['AnsiString'], 'AnsiString');
  Engine.RegExtMethod('ExtractFileName',{$IfDef FPC}@{$EndIf}_ExtractFileName, ['AnsiString'], 'AnsiString');
  Engine.RegExtMethod('ExtractFileExt',{$IfDef FPC}@{$EndIf}_ExtractFileExt, ['AnsiString'], 'AnsiString');
  Engine.RegExtMethod('ExpandFileName',{$IfDef FPC}@{$EndIf}_ExpandFileName, ['AnsiString'], 'AnsiString');

  {$ENDREGION}

  //======== Internal ========
  {$REGION 'Internal'}
  Engine.RegExtMethod('InheritsClass',{$IfDef FPC}@{$EndIf}_InheritsClass, ['TObject', 'TClass'], 'Boolean');
  {$ENDREGION}

  //======== MATHS ========
  {$REGION 'Maths'}

  Engine.RegExtMethod('Round',{$IfDef FPC}@{$EndIf}_Round, ['Single'], 'Integer');
  Engine.RegExtMethod('abs_int',{$IfDef FPC}@{$EndIf}_abs, ['Integer'], 'Integer',
    'Makes an integer value positive');
  Engine.RegExtMethod('abs',{$IfDef FPC}@{$EndIf}_abs, ['Double'], 'Double',
    'Makes a float value positive');

  Engine.RegExtMethod('RandomF',{$IfDef FPC}@{$EndIf}_RandomF, [], 'Double',
    'Returns a random float value in the Intervall [0..1]');
  Engine.RegExtMethod('Random',{$IfDef FPC}@{$EndIf}_Random, ['Integer'], 'Integer',
    'Returns a random Integer number in the Intervall [0..a-1]');

  Engine.RegExtMethod('sin',{$IfDef FPC}@{$EndIf}_sin, ['Single'], 'Single');
  Engine.RegExtMethod('cos',{$IfDef FPC}@{$EndIf}_cos, ['Single'], 'Single');
  Engine.RegExtMethod('Arcsin',{$IfDef FPC}@{$EndIf}_Arcsin, ['Single'], 'Single');
  Engine.RegExtMethod('Arccos',{$IfDef FPC}@{$EndIf}_Arccos, ['Single'], 'Single');
  Engine.RegExtMethod('Arctan2',{$IfDef FPC}@{$EndIf}_Arctan2, ['Single', 'Single'], 'Single',
    'Returns the Angle of an triangle with the two given sides');

  Engine.RegExtMethod('sqr',{$IfDef FPC}@{$EndIf}_sqr, ['Single'], 'Single');
  Engine.RegExtMethod('sqrt',{$IfDef FPC}@{$EndIf}_sqrt, ['Single'], 'Single',
    'Returns the squareroot of the value');
  Engine.RegExtMethodV('sqrtvari',{$IfDef FPC}@{$EndIf}_sqrtvari, ['Single'], 'Single',
    'for testing only');

  {$ENDREGION}

  //======== Properties ========
  {$REGION 'Properties'}

  Engine.RegExtMethod('SetIntProperty',{$IfDef FPC}@{$EndIf}_SetIntProperty, ['TObject', 'String', 'Integer'], '',
    'Sets the (Integer) property value for an Object' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Property-Name' + sLineBreak +
    '#3 : New Value for Property');
  Engine.RegExtMethod('GetIntProperty',{$IfDef FPC}@{$EndIf}_GetIntProperty, ['TObject', 'String'], 'Integer',
    'Returns the (Integer) property value for an Object' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Property-Name' + sLineBreak +
    'Returns : Value of Property');

  Engine.RegExtMethod('SetStrProperty',{$IfDef FPC}@{$EndIf}_SetStrProperty, ['TObject', 'String', 'String'], '',
    'Sets the (String) property value for an Object' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Property-Name' + sLineBreak +
    '#3 : New Value for Property');
  Engine.RegExtMethod('GetStrProperty',{$IfDef FPC}@{$EndIf}_GetStrProperty, ['TObject', 'String'], 'String',
    'Returns the (String) property value for an Object' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Property-Name' + sLineBreak +
    'Returns : Value of Property');

  Engine.RegExtMethod('SetBoolProperty',{$IfDef FPC}@{$EndIf}_SetBoolProperty, ['TObject', 'String', 'Boolean'], '',
    'Sets the (Boolean) property value for an Object' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Property-Name' + sLineBreak +
    '#3 : New Value for Property');
  Engine.RegExtMethod('GetBoolProperty',{$IfDef FPC}@{$EndIf}_GetBoolProperty, ['TObject', 'String'], 'Boolean',
    'Returns the (Boolean) property value for an Object' + sLineBreak +
    '#1 : Control' + sLineBreak +
    '#2 : Property-Name' + sLineBreak +
    'Returns : Value of Property');

  {$ENDREGION}

  //======== Files ========
  {$REGION 'Files'}

  Engine.RegExtMethod('FileExists',{$IfDef FPC}@{$EndIf}_FileExists, ['AnsiString'], 'Boolean');
  Engine.RegExtMethod('FindFirst',{$IfDef FPC}@{$EndIf}_FindFirst, ['AnsiString', 'Integer', 'PSearchRec'], 'Integer');
  Engine.RegExtMethod('FindNext',{$IfDef FPC}@{$EndIf}_FindNext, ['PSearchRec'], 'Integer');
  Engine.RegExtMethod('FindClose',{$IfDef FPC}@{$EndIf}_FindClose, ['PSearchRec'], '');

  {$ENDREGION}

  //======== Windows ========
  {$REGION 'Windows'}

  {$ifndef FPC}
  Engine.RegExtMethod('CallDLLProc',{$IfDef FPC}@{$EndIf}_DynCallDLLProc, ['String', 'String', 'Boolean', 'Pointer'], 'Cardinal',
    'Executes a DLL Method');
  {$endif}

  Engine.RegExtMethod('GetAsyncKeyState',{$IfDef FPC}@{$EndIf}_GetAsyncKeyState, ['Word'], 'Boolean',
    'Checks whether a certain key is pressed' + sLineBreak + 'Returns true, if the key was pressed since the last call of GetAsyncKeyState for this key');

  Engine.RegExtMethod('GetTickCount',{$IfDef FPC}@{$EndIf}_GetTickCount, [], 'Cardinal',
    'Returns the current System-Time in milli-seconds');

  {$ifndef WINCE}
  Engine.RegExtMethod('Beep',{$IfDef FPC}@{$EndIf}_Beep, ['Cardinal', 'Cardinal'], '');
  {$endif WINCE}

  Engine.RegExtMethod('Sleep',{$IfDef FPC}@{$EndIf}_Sleep, ['Integer'], '');
  Engine.RegExtMethod('Delay',{$IfDef FPC}@{$EndIf}_Delay, ['Integer'], '',
    'Delays the execution of the script for a certain time');

  Engine.RegExtMethod('InputDlg',{$IfDef FPC}@{$EndIf}_InputDlg, ['String', 'String'], 'String',
    'Shows an Input-Dialog for the user' + sLineBreak +
    '#1: Question' + sLineBreak +
    '#2: Default value' + sLineBreak +
    'Returns the string the user has entered or '' if the' + sLineBreak + 'user has pressed the Cancel-Button');

  Engine.RegExtMethod('RVal',{$IfDef FPC}@{$EndIf}_RVal, ['Cardinal'], 'Byte',
    'Extracts the Red-Component of a TColor value');
  Engine.RegExtMethod('GVal',{$IfDef FPC}@{$EndIf}_GVal, ['Cardinal'], 'Byte',
    'Extracts the Green-Component of a TColor value');
  Engine.RegExtMethod('BVal',{$IfDef FPC}@{$EndIf}_BVal, ['Cardinal'], 'Byte',
    'Extracts the Blue-Component of a TColor value');
  Engine.RegExtMethod('RGB',{$IfDef FPC}@{$EndIf}_RGB, ['Byte', 'Byte', 'Byte'], 'Cardinal',
    'Creates a TColor Value out of the red, green and blue components');

  {$ENDREGION}
End;

Initialization
  randomize;
End.

