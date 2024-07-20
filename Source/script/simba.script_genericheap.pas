{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_genericheap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpvartypes_array, lpvartypes_record, lptree, lpcompiler, lpmessages,
  simba.script_genericbase;

procedure InitializeHeap(Compiler: TLapeCompiler);

implementation

type
  THeapType = class(TLapeType_Record)
  protected
    FValueType: TLapeType;
    FItemRec: TLapeType;
    FItemArr: TLapeType;
  public
    //record
    //  FCount: Int32;
    //  FItems: array of record
    //    Index: Int32;
    //    Value: _T;
    //  end;
    //end;
    constructor Create(ACompiler: TLapeCompilerBase; AValueType: TLapeType); reintroduce;
  end;

constructor THeapType.Create(ACompiler: TLapeCompilerBase; AValueType: TLapeType);
begin
  inherited Create(ACompiler, nil);

  FValueType := AValueType;
  FItemRec := FCompiler.addManagedType(TLapeType_Record.Create(FCompiler, nil, 'TItem')) as TLapeType_Record;
  FItemArr := FCompiler.addManagedType(TLapeType_DynArray.Create(FItemRec, FCompiler, 'TItemArray'));
  with TLapeType_Record(FItemRec) do
  begin
    addField(FCompiler.getBaseType(ltInt32), 'Index');
    addField(FValueType, 'Value');
  end;

  addField(FCompiler.getBaseType(ltInt32), 'FCount');
  addField(FItemArr, 'FItems');
end;

type
  TLapeTree_InternalMethod_Heap = class(TGenericMethod)
  public
    function resType: TLapeType; override;
  end;

function TLapeTree_InternalMethod_Heap.resType: TLapeType;

  function FindOurType(Typ: TLapeType): TLapeType;
  var
    Decl: TLapeDeclaration;
  begin
    for Decl in FCompiler.GlobalDeclarations.GetByClass(THeapType, bFalse) do
      if (THeapType(Decl).FValueType = Typ) then
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
    RequireOperators(FCompiler, [op_cmp_LessThan, op_cmp_GreaterThan], ValueType, DocPos);

    // Already built?
    FResType := FindOurType(ValueType);
    if (FResType <> nil) then
    begin
      Result := inherited;
      Exit;
    end;
    FResType := FCompiler.addGlobalDecl(THeapType.Create(FCompiler, ValueType)) as TLapeType;

    // OutOfRangeException(Index: Int32)
    Builder := TLapeMethodBuilder.Create(FResType, [THeapType(FResType).FItemRec]);
    Builder.Name := 'RangeCheckException';
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'raise Format("Index %d out of range (Low:0, High:%d)", [Index, Self.FCount - 1]);'
    ];
    Builder.Build();

    Builder.Name := '_MoveDownHI';
    Builder.addParam('StartPos', FCompiler.getBaseType(ltInt32));
    Builder.addParam('Pos', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'begin',
      '  var NewItem := FItems[Pos];',
      '',
      '  while (Pos > StartPos) do',
      '  begin',
      '    var ParentPos := (Pos - 1) shr 1;',
      '    var Parent := FItems[ParentPos];',
      '    if (NewItem.Value < Parent.Value) then',
      '    begin',
      '      FItems[Pos] := parent;',
      '      Pos := ParentPos;',
      '      Continue;',
      '    end;',
      '    Break;',
      '  end;',
      '',
      '  FItems[Pos] := NewItem;',
      'end;'
    ];
    Builder.Build();

    Builder.Name := '_MoveUpHI';
    Builder.addParam('Pos', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'begin',
      '  var EndPos := FCount;',
      '  var StartPos := Pos;',
      '  var NewItem := FItems[Pos];',
      '  var ChildPos := 2 * Pos + 1;',
      '  while (ChildPos < EndPos) do',
      '  begin',
      '    var RightPos := ChildPos + 1;',
      '    if (RightPos < EndPos) and not (FItems[ChildPos].Value < FItems[RightPos].Value) then',
      '      ChildPos := RightPos;',
      '    FItems[Pos] := FItems[ChildPos];',
      '    Pos := ChildPos;',
      '    ChildPos := 2 * Pos + 1;',
      '  end;',
      '  FItems[Pos] := NewItem;',
      '',
      ' _MoveDownHI(StartPos, Pos);',
      'end;'
    ];
    Builder.Build();

    Builder.Name := 'Push';
    Builder.addParam('Value', ValueType, lptConstRef);
    Builder.addParam('Index', FCompiler.getBaseType(ltInt32));
    Builder.Body := [
      'if (FCount >= Length(FItems)) then',
      '  SetLength(FItems, 4 + (Length(FItems) * 2));',
      '',
      'FItems[FCount].Value := Value;',
      'FItems[FCount].Index := Index;',
      'if (FCount > 0) then',
      '  _MoveDownHI(0, FCount);',
      'Inc(FCount);'
    ];
    Builder.Build();

    Builder.Name := 'Pop';
    Builder.ResultType := THeapType(FResType).FItemRec;
    Builder.Body := [
      'if (FCount = 0) then',
      '  RangeCheckException(0);',
      'Result := FItems[0];',
      '',
      'Dec(FCount);',
      'FItems[0] := FItems[FCount];',
      '_MoveUpHI(0);'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'Peek';
    Builder.ResultType := THeapType(FResType).FItemRec;
    Builder.Body := [
      'if (FCount = 0) then',
      '  RangeCheckException(0);',
      'Result := FItems[0];'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'Items';
    Builder.ResultType := THeapType(FResType).FItemArr;
    Builder.Body := [
      'Result := FItems;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'Count';
    Builder.ResultType := FCompiler.getBaseType(ltInt32);
    Builder.Body := [
      'Result := FCount;'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'Clear';
    Builder.Body := [
      'FCount := 0;'
    ];
    Builder.Build();

    // ToString: String
    Builder.Name := 'ToString';
    Builder.ResultType := FCompiler.getBaseType(ltString);
    Builder.Body := [
      'var i: Int32;',
      'Result := "Count=" + System.ToString(FCount);',
      'for i := 0 to FCount - 1 do',
      '  Result := Result + LINE_SEP + " [Index=" + System.ToString(FItems[i].Index) + ", Value=" + System.ToString(FItems[i].Value) + "]";'
    ];
    Builder.Build();
    Builder.Free();

    addToStringOverride();
  end;

  Result := inherited;
end;

procedure InitializeHeap(Compiler: TLapeCompiler);
begin
  Compiler.InternalMethodMap['Heap'] := TLapeTree_InternalMethod_Heap;
end;

end.

