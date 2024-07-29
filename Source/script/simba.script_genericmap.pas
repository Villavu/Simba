{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_genericmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpvartypes_array, lpvartypes_record, lptree, lpcompiler, lpmessages,
  simba.script_genericbase;

procedure InitializeMap(Compiler: TLapeCompiler);

implementation

type
  TMapType = class(TLapeType_Record)
  protected
    FValueType: TLapeType;
    FKeyType: TLapeType;
  public
    //record
    //  FInvalidVal: _T;
    //  FCount: Integer;
    //  FItems: array of record
    //    Key: _K;
    //    Value: _T;
    //  end;
    //end;
    constructor Create(ACompiler: TLapeCompilerBase; AKeyType, AValueType: TLapeType); reintroduce;

    property ValueType: TLapeType read FValueType;
    property KeyType: TLapeType read FKeyType;
  end;

constructor TMapType.Create(ACompiler: TLapeCompilerBase; AKeyType, AValueType: TLapeType);
var
  Rec: TLapeType_Record;
begin
  inherited Create(ACompiler, nil);

  FKeyType := AKeyType;
  FValueType := AValueType;

  addField(FValueType, 'FInvalidVal');
  addField(FCompiler.getBaseType(ltSizeInt), 'FCount');

  Rec := FCompiler.addManagedType(TLapeType_Record.Create(FCompiler, nil)) as TLapeType_Record;
  Rec.addField(FKeyType, 'Key');
  Rec.addField(FValueType, 'Value');

  addField(FCompiler.addManagedType(TLapeType_DynArray.Create(Rec, FCompiler)), 'FItems');
end;

type
  TLapeTree_InternalMethod_Map = class(TGenericMethod)
  public
    function resType: TLapeType; override;
  end;

function TLapeTree_InternalMethod_Map.resType: TLapeType;

  function FindOurType(Typ: TLapeType): TLapeType;
  var
    Decl: TLapeDeclaration;
  begin
    for Decl in FCompiler.GlobalDeclarations.GetByClass(TMapType, bFalse) do
      if (TMapType(Decl).ValueType = Typ) then
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
  KeyType, ValueType: TLapeType;
  Builder: TLapeMethodBuilder;
begin
  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);

  if (FResType = nil) then
  begin
    KeyType := getParamType(0);
    ValueType := getParamType(1);
    RequireOperators(FCompiler, [op_cmp_Equal], KeyType, DocPos);

    // Already built?
    FResType := FindOurType(ValueType);
    if (FResType <> nil) then
    begin
      Result := inherited;
      Exit;
    end;
    FResType := FCompiler.addGlobalDecl(TMapType.Create(FCompiler, KeyType, ValueType)) as TLapeType;

    Builder := TLapeMethodBuilder.Create(FResType);

    // OutOfRangeException(Index: Int32)
    Builder.Name := 'RangeCheckException';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'raise Format("Index %d out of range (Low:0, High:%d)", [Index, Self.FCount - 1]);'
    ];
    Builder.Build();

    // Value[Key: _K]: _V (write)
    Builder.Name := 'Value';
    Builder.addParam('AKey', KeyType, lptConstRef);
    Builder.addParam('AValue', ValueType, lptConstRef);
    Builder.Body := [
      'var i: Int32;',
      'for i := 0 to FCount - 1 do',
      '  if (FItems[i].Key = AKey) then',
      '  begin',
      '    FItems[i].Value := AValue;',
      '    Exit;',
      '  end;',
      '',
      'if (FCount >= Length(FItems)) then',
      '  SetLength(FItems, 4 + (Length(FItems) * 2));',
      'FItems[FCount].Key := AKey;',
      'FItems[FCount].Value := AValue;',
      'Inc(FCount);'
    ];
    Builder.isProperty := True;
    Builder.Build();

    // Value[Key: _K]: _V (read)
    Builder.Name := 'Value';
    Builder.ResultType := ValueType;
    Builder.addParam('AKey', KeyType, lptConstRef);
    Builder.Body := [
      'var i: Int32;',
      'for i := 0 to FCount - 1 do',
      '  if (FItems[i].Key = AKey) then',
      '    Exit(FItems[i].Value);',
      '',
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
    Builder.ResultType := KeyType;
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
    Builder.addParam('Key', KeyType, lptConstRef);
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
      '  RangeCheckException(Index);',
      'FItems[Index].Key := Key;'
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
    Builder.addParam('AKey', KeyType, lptConstRef);
    Builder.Body := [
      'var i: Int32;',
      'for i := 0 to FCount - 1 do',
      '  if (FItems[i].Key = AKey) then',
      '   Exit(i);',
      'Result := -1;'
    ];
    Builder.Build();

    // Exists(Key: String): Boolean
    Builder.Name := 'Exists';
    Builder.ResultType := FCompiler.getBaseType(ltBoolean);
    Builder.addParam('AKey', KeyType, lptConstRef);
    Builder.Body := [
      'var i: Int32;',
      'for i := 0 to FCount - 1 do',
      '  if (FItems[i].Key = AKey) then',
      '   Exit(True);'
    ];
    Builder.Build();

    // DeleteIndex(Index: Int32);
    Builder.Name := 'DeleteIndex';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'if (Index < 0) or (Index >= FCount) then',
      '  RangeCheckException(Index);',
      '',
      'FItems[Index] := [];',
      'if (Index < FCount - 1) then',
      '  Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(FItems[0]));',
      'Dec(FCount);'
    ];
    Builder.Build();

    // Delete(Key: String);
    Builder.Name := 'Delete';
    Builder.addParam('Key', KeyType, lptConstRef);
    Builder.Body := [
      'var i := IndexOf(Key);',
      'if (i > -1) then',
      '  Self.Delete(i);'
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
    Builder.ResultType := FindArrayType(KeyType); // re-use an array if exists as it might have methods
    if (Builder.ResultType = nil) then
      Builder.ResultType := FCompiler.addManagedType(TLapeType_DynArray.Create(KeyType, FCompiler));
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
      'var i: Integer;',
      'Result := "Count=" + ToStr(Self.FCount);',
      'for i := 0 to Self.FCount - 1 do',
      '  Result := Result + LINE_SEP + " [Key=" + System.ToString(Self.FItems[i].Key) + ", Value=" + System.ToString(Self.FItems[i].Value) + "]";'
    ];
    Builder.Build();
    Builder.Free();

    addToStringOverride();
  end;

  Result := inherited;
end;

procedure InitializeMap(Compiler: TLapeCompiler);
begin
  Compiler.InternalMethodMap['Map'] := TLapeTree_InternalMethod_Map;
end;

end.

