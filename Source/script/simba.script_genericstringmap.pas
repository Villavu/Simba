{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_genericstringmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpvartypes_array, lpvartypes_record, lptree, lpcompiler, lpmessages,
  simba.script_genericbase;

procedure InitializeStringMap(Compiler: TLapeCompiler);

implementation

type
  TItem = packed record
    Key: String;
    Hash: UInt64;
    // Value: _T
  end;
  PItem = ^TItem;

function _StringMapHash(const Str: String): UInt64;
var
  Ptr: PChar;
begin
  {$PUSH}
  {$Q-}{$R-}
  Result := UInt64($CBF29CE484222325);

  if (Length(Str) > 0) then
  begin
    Ptr := @Str[1];
    while (Ptr^ <> #0) do
    begin
      Result := (Result xor Byte(Ptr^)) * $0100000001B3;
      Inc(Ptr);
    end;
  end;
  {$POP}
end;

function _StringMapBSearch(Arr: PByte; ElSize: Int32; Hi: Int32; Hash: UInt64): Int32;
var
  Val: UInt64;
  Index, Lo: Int32;
begin
  Lo := 0;
  while (Lo <= Hi) do
  begin
    Index := (Lo + Hi) div 2;
    Val := PItem(@Arr[Index * ElSize])^.Hash;

    if (Hash = Val) then
    begin
      // if duplicate hashes go back to the first
      while (Index > Lo) and (PItem(@Arr[(Index - 1) * ElSize])^.Hash = Hash) do
        Dec(Index);

      Exit(Index);
    end
    else if (Hash < Val) then
      Hi := Index - 1
    else
      Lo := Index + 1;
  end;
  Result := -(Lo + 1);
end;

function _StringMapIndexOf(Arr: PByte; ElSize: Int32; Hi: Int32; CaseSens: Boolean; Key: String): Int32;
var
  Hash: UInt64;
  Index: Int32;
begin
  if not CaseSens then
    Key := UpperCase(Key);
  Hash := _StringMapHash(Key);

  Index := _StringMapBSearch(Arr, ElSize, Hi, Hash);
  if (Index > -1) then
    while (Index <= Hi) and (PItem(@Arr[Index * ElSize])^.Hash = Hash) do
    begin
      if (PItem(@Arr[Index * ElSize])^.Key = Key) then
        Exit(Index);
      Inc(Index);
    end;

  Result := -1;
end;

// the item to insert *must* be at Arr[Hi*ElSize].
// returns true if key was inserted otherwise return false if value was modified (aka no new item was inserted)
function _StringMapInsert(Arr: PByte; ElSize: Int32; Hi: Int32; CaseSens: Boolean; TempBuffer: Pointer): Boolean;
var
  Item: PItem;
  Index, Temp: Int32;
begin
  Item := PItem(@Arr[Hi * ElSize]); // the item to insert
  if not CaseSens then
    Item^.Key := UpperCase(Item^.Key);
  Item^.Hash := _StringMapHash(Item^.Key);

  Index := _StringMapBSearch(Arr, ElSize, Hi, Item^.Hash);

  // check if key is already added
  // if so just swap the items
  if (Index > -1) then
  begin
    Temp := Index;
    while (Index < Hi) and (PItem(@Arr[Index * ElSize])^.Hash = Item^.Hash) do
    begin
      if (PItem(@Arr[Index * ElSize])^.Key = Item^.Key) then
      begin
        Move(Arr[Index * ElSize], TempBuffer^, ElSize);
        Move(Item^, Arr[Index * ElSize], ElSize);
        Move(TempBuffer^, Arr[Hi * ElSize], ElSize);

        Result := False;
        Exit;
      end;

      Inc(Index);
    end;
    Index := Temp;
  end;

  // doesn't exist - move it them into place
  if (Index < 0) then
    Index := -(Index + 1);

  Move(Item^, TempBuffer^, ElSize);
  Move(Arr[Index * ElSize], Arr[(Index + 1) * ElSize], (Hi - Index) * ElSize);
  Move(TempBuffer^, Arr[Index * ElSize], ElSize);

  Result := True;
end;

procedure _LapeStringMapIndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := _StringMapIndexOf(PPointer(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PBoolean(Params^[3])^, PString(Params^[4])^);
end;

procedure _LapeStringMapInsert(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := _StringMapInsert(PPointer(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PBoolean(Params^[3])^, PPointer(Params^[4])^);
end;

procedure _LapeStringMapHash(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := _StringMapHash(PChar(PString(Params^[0])^));
end;

type
  TStringMapType = class(TLapeType_Record)
  protected
    FValueType: TLapeType;
    FItemRec: TLapeType;
    FValueToStrType: TLapeType;
    FStrToValueType: TLapeType;
  public
    //record
    //  FBuffer: array[0..ItemSize-1] of Byte;
    //  FInvalidVal: _T;
    //  FCaseSens: Boolean;
    //  FCount: Int32;
    //  FItems: array of record
    //    Key: String;
    //    KeyHash: UInt64;
    //    Value: _T;
    //  end;
    //end;
    constructor Create(ACompiler: TLapeCompilerBase; AValueType: TLapeType); reintroduce;

    property ValueType: TLapeType read FValueType;
    property ValueToStrType: TLapeType read FValueToStrType;
    property StrToValueType: TLapeType read FStrToValueType;
  end;

constructor TStringMapType.Create(ACompiler: TLapeCompilerBase; AValueType: TLapeType);
var
  Range: TLapeRange;
begin
  inherited Create(ACompiler, nil);

  FValueType := AValueType;
  FValueToStrType := FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [FValueType], [lptNormal], [TLapeGlobalVar(nil)], FCompiler.getBaseType(ltString)));
  FStrToValueType := FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [FCompiler.getBaseType(ltString)], [lptNormal], [TLapeGlobalVar(nil)], FValueType));

  FItemRec := FCompiler.addManagedType(TLapeType_Record.Create(FCompiler, nil, 'TItem'));
  TLapeType_Record(FItemRec).addField(FCompiler.getBaseType(ltString), 'Key');
  TLapeType_Record(FItemRec).addField(FCompiler.getBaseType(ltUInt64), 'KeyHash');
  TLapeType_Record(FItemRec).addField(FValueType, 'Value');

  Range.Lo := 0;
  Range.Hi := FItemRec.Size;

  addField(FCompiler.addManagedType(TLapeType_StaticArray.Create(Range, FCompiler.getBaseType(ltUInt8), FCompiler)), 'FBuffer');
  addField(FValueType, 'FInvalidVal');
  addField(FCompiler.getBaseType(ltBoolean), 'FCaseSens');
  addField(FCompiler.getBaseType(ltInt32), 'FCount');

  addField(FCompiler.addManagedType(TLapeType_DynArray.Create(FItemRec, FCompiler)), 'FItems');
end;

type
  TLapeTree_InternalMethod_StringMap = class(TGenericMethod)
  public
    function resType: TLapeType; override;
  end;

function TLapeTree_InternalMethod_StringMap.resType: TLapeType;

  function FindOurType(Typ: TLapeType): TLapeType;
  var
    Decl: TLapeDeclaration;
  begin
    for Decl in FCompiler.GlobalDeclarations.GetByClass(TStringMapType, bFalse) do
      if (TStringMapType(Decl).ValueType = Typ) then
        Exit(TLapeType(Decl));
    Result := nil;
  end;

  function FindArrayType(Typ: TLapeType): TLapeType;
  var
    Decl: TLapeDeclaration;
  begin
    for Decl in FCompiler.GlobalDeclarations.GetByClass(TLapeType_DynArray, bFalse) do
      if (TLapeType_DynArray(Decl).PType = Typ) then
        Exit(TLapeType(Decl));
    Result := nil;
  end;

var
  ValueType: TLapeType;
  Builder: TLapeMethodBuilder;
begin
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FResType = nil) then
  begin
    ValueType := getParamType(0);

    // Already built?
    FResType := FindOurType(ValueType);
    if (FResType <> nil) then
    begin
      Result := inherited;
      Exit;
    end;
    FResType := FCompiler.addGlobalDecl(TStringMapType.Create(FCompiler, ValueType)) as TLapeType;

    Builder := TLapeMethodBuilder.Create(FResType, [TStringMapType(FResType).FItemRec]);

    // OutOfRangeException(Index: Int32)
    Builder.Name := 'RangeCheckException';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'raise Format("Index %d out of range (Low:0, High:%d)", [Index, Self.FCount - 1]);'
    ];
    Builder.Build();

    // Value[Key: _K]: _V (write)
    Builder.Name := 'Value';
    Builder.addParam('AKey', FCompiler.getBaseType(ltString), lptConstRef);
    Builder.addParam('AValue', ValueType, lptConstRef);
    Builder.Body := [
      'if (FCount >= Length(FItems)) then',
      '  SetLength(FItems, 4 + (Length(FItems) * 2));',
      'FItems[FCount].Key := AKey;',
      'FItems[FCount].Value := AValue;',
      '',
      'if _StringMapInsert(Pointer(FItems), SizeOf(TItem), FCount, FCaseSens, @FBuffer[0]) then',
      '  Inc(FCount);'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // Value[Key: _K]: _V (read)
    Builder.Name := 'Value';
    Builder.ResultType := ValueType;
    Builder.addParam('AKey', FCompiler.getBaseType(ltString), lptConstRef);
    Builder.Body := [
      'var i := _StringMapIndexOf(Pointer(FItems), SizeOf(TItem), FCount - 1, FCaseSens, AKey);',
      'if (i >= 0) then',
      '  Result := FItems[i].Value',
      'else',
      '  Result := FInvalidVal;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // ValueFromIndex[Index: Int32]: _V (read)
    Builder.Name := 'ValueFromIndex';
    Builder.ResultType := ValueType;
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
      '  RangeCheckException(Index);',
      'Result := FItems[Index].Value;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // ValueFromIndex[Index: Int32]: _V (write)
    Builder.Name := 'ValueFromIndex';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.addParam('Val', ValueType);
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
       '  RangeCheckException(Index);',
       'FItems[Index].Value := Val;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // KeyFromIndex[Index: Int32]: _K (read)
    Builder.Name := 'KeyFromIndex';
    Builder.ResultType := FCompiler.getBaseType(ltString);
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
      '  RangeCheckException(Index);',
      'Result := FItems[Index].Key;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // KeyFromIndex[Index: SizeInt]: _K (write)
    Builder.Name := 'KeyFromIndex';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.addParam('Key', FCompiler.getBaseType(ltString));
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
      '  RangeCheckException(Index);',
      'var Temp: TItem := FItems[Index];',
      'Self.Delete(Index);',
      'Self.Value[Key] := Temp.Value;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // Count: Int32
    Builder.Name := 'Count';
    Builder.ResultType := FCompiler.getBaseType(ltInt32);
    Builder.Body := [
      'Result := FCount;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // Clear
    Builder.Name := 'Clear';
    Builder.Body := [
      'FCount := 0;'
    ];
    Builder.Build();

    // CaseSens: Boolean (read)
    Builder.Name := 'CaseSens';
    Builder.ResultType := FCompiler.getBaseType(ltBoolean);
    Builder.Body := [
      'Result := FCaseSens;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // CaseSens: Boolean (write)
    Builder.Name := 'CaseSens';
    Builder.addParam('Val', FCompiler.getBaseType(ltBoolean));
    Builder.Body := [
      'FCaseSens := Val;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // InvalidVal: _T (read)
    Builder.Name := 'InvalidVal';
    Builder.ResultType := ValueType;
    Builder.Body := [
      'Result := FInvalidVal;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // InvalidVal: _T (write)
    Builder.Name := 'InvalidVal';
    Builder.addParam('Val', ValueType, lptConstRef);
    Builder.Body := [
      'FInvalidVal := Val;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // IndexOf(key: String): Int32
    Builder.Name := 'IndexOf';
    Builder.ResultType := FCompiler.getBaseType(ltInt32);
    Builder.addParam('Key', FCompiler.getBaseType(ltString), lptConstRef);
    Builder.Body := [
      'Result := _StringMapIndexOf(Pointer(FItems), SizeOf(TItem), FCount - 1, FCaseSens, Key);'
    ];
    Builder.Build();

    // Exists(Key: String): Boolean
    Builder.Name := 'Exists';
    Builder.ResultType := FCompiler.getBaseType(ltBoolean);
    Builder.addParam('Key', FCompiler.getBaseType(ltString), lptConstRef);
    Builder.Body := [
      'Result := _StringMapIndexOf(Pointer(FItems), SizeOf(TItem), FCount - 1, FCaseSens, Key) > -1;'
    ];
    Builder.Build();

    // Delete(Index: Int32);
    Builder.Name := 'Delete';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
      '  RangeCheckException(Index);',
      'Dec(FCount);',
      'if (Index < FCount) then',
      '  Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TItem));'
    ];
    Builder.isOverload := True;
    Builder.Build();

    // Delete(Key: String);
    Builder.Name := 'Delete';
    Builder.addParam('Key', FCompiler.getBaseType(ltString));
    Builder.Body := [
      'var i := IndexOf(Key);',
      'if (i > -1) then',
      '  Delete(i);'
    ];
    Builder.isOverload := True;
    Builder.Build();

    // Load(FileName: String; Sep: String; StrToValue: function(Str: String): _T)
    Builder.Name := 'Load';
    Builder.addParam('FileName', FCompiler.getBaseType(ltString));
    Builder.addParam('Sep', FCompiler.getBaseType(ltString));
    Builder.addParam('StrToValue', TStringMapType(FResType).StrToValueType);
    Builder.Body := [
      'var Lines: TStringArray := FileReadLines(FileName);',
      'var Line: String;',
      'Clear();',
      'SetLength(FItems, Length(Lines));',
      'for Line in Lines do',
      'begin',
      '  var Pieces := Line.Partition(Sep);',
      '  if (Pieces[0] <> "") and (Pieces[1] <> "") then',
      '    Value[Pieces[0]] := StrToValue(Pieces[2]);',
      'end;'
    ];
    Builder.Build();

    // Save(FileName: String; Sep: String; ValueToStr: function(Value: _T): String)
    Builder.Name := 'Save';
    Builder.addParam('FileName', FCompiler.getBaseType(ltString));
    Builder.addParam('Sep', FCompiler.getBaseType(ltString));
    Builder.addParam('ValueToStr', TStringMapType(FResType).ValueToStrType);
    Builder.Body := [
      'var i: Int32;',
      'var Data: String;',
      'for i := 0 to FCount - 1 do',
      '  Data := Data + FItems[i].Key + Sep + ValueToStr(FItems[i].Value) + LINE_SEP;',
      'FileWrite(FileName, Data);'
    ];
    Builder.Build();

    // Values: array of _T (read)
    Builder.Name := 'Values';
    Builder.ResultType := FindArrayType(ValueType); // re-use an array if exists as it might have methods
    if (Builder.ResultType = nil) then
      Builder.ResultType := FCompiler.addManagedType(TLapeType_DynArray.Create(ValueType, FCompiler));
    Builder.Body := [
      'SetLength(Result, FCount);',
      'var i: Int32;',
      'for i := 0 to FCount - 1 do',
      '  Result[i] := FItems[i].Value;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // Keys: TStringArray (read)
    Builder.Name := 'Keys';
    Builder.ResultType := FCompiler.getGlobalType('TStringArray');
    Builder.Body := [
      'SetLength(Result, FCount);',
      'var i: Int32;',
      'for i := 0 to FCount - 1 do',
      '  Result[i] := FItems[i].Key;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // ToString: String
    Builder.Name := 'ToString';
    Builder.ResultType := FCompiler.getBaseType(ltString);
    Builder.Body := [
      'var i: Int32;',
      'Result := "Count=" + IntToStr(FCount);',
      'for i := 0 to FCount - 1 do',
      '  Result := Result + LINE_SEP + " [Key=" + FItems[i].Key + ", Value=" + System.ToString(FItems[i].Value) + "]";'
    ];
    Builder.Build();
    Builder.Free();

    addToStringOverride();
  end;

  Result := inherited;
end;

procedure InitializeStringMap(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalFunc('function _StringMapInsert(Arr: Pointer; ElSize: Int32; Hi: Int32; CaseSens: Boolean; TempBuffer: Pointer): Boolean', @_LapeStringMapInsert);
  Compiler.addGlobalFunc('function _StringMapIndexOf(Arr: Pointer; ElSize: Int32; Hi: Int32; CaseSens: Boolean; Key: String): Int32', @_LapeStringMapIndexOf);
  Compiler.addGlobalFunc('function _StringMapHash(Str: String): UInt64', @_LapeStringMapHash);

  Compiler.InternalMethodMap['StringMap'] := TLapeTree_InternalMethod_StringMap;
end;

end.

