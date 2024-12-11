{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_genericarraybuffer;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpvartypes_array, lpvartypes_record, lptree, lpcompiler, lpmessages,
  simba.script_genericbase;

procedure InitializeArrayBuffer(Compiler: TLapeCompiler);

implementation

type
  TArrayBufferType = class(TLapeType_Record)
  protected
    FValueType: TLapeType;
    FItemArr: TLapeType;
  public
    //record
    //  FCount: Int32;
    //  FItems: array of _T;
    //end;
    constructor Create(ACompiler: TLapeCompilerBase; AValueType: TLapeType); reintroduce;
  end;

constructor TArrayBufferType.Create(ACompiler: TLapeCompilerBase; AValueType: TLapeType);
begin
  inherited Create(ACompiler, nil);

  FValueType := AValueType;
  FItemArr := FCompiler.addManagedType(TLapeType_DynArray.Create(FValueType, FCompiler, 'TItemArray'));

  addField(FCompiler.getBaseType(ltInt32), 'Count');
  addField(FItemArr, 'Items');
end;

type
  TLapeTree_InternalMethod_ArrayBuffer = class(TGenericMethod)
  public
    function resType: TLapeType; override;
  end;

function TLapeTree_InternalMethod_ArrayBuffer.resType: TLapeType;

  function FindOurType(Typ: TLapeType): TLapeType;
  var
    Decl: TLapeDeclaration;
  begin
    for Decl in FCompiler.GlobalDeclarations.GetByClass(TArrayBufferType, bFalse) do
      if (TArrayBufferType(Decl).FValueType = Typ) then
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
    FResType := FCompiler.addGlobalDecl(TArrayBufferType.Create(FCompiler, ValueType)) as TLapeType;

    Builder := TLapeMethodBuilder.Create(FResType);

    Builder.Name := 'First';
    Builder.ResultType := TArrayBufferType(FResType).FValueType;
    Builder.Body := [
      'Result := Items[0];'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'Last';
    Builder.ResultType := TArrayBufferType(FResType).FValueType;
    Builder.Body := [
      'Result := Items[Count - 1];'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'Pop';
    Builder.ResultType := TArrayBufferType(FResType).FValueType;
    Builder.Body := [
      'Result := Items[Count - 1];',
      'SetLength(Items, Count - 1);',
      'Dec(Count);'
    ];
    Builder.isProperty := True;
    Builder.Build();

    Builder.Name := 'ToArray';
    Builder.ResultType := TArrayBufferType(FResType).FItemArr;
    Builder.Body := [
      'Result := Copy(Items, 0, Count);'
    ];
    Builder.Build();

    Builder.Name := 'Add';
    Builder.addParam('Value', ValueType, lptConstRef);
    Builder.Body := [
      'if (Count >= Length(Items)) then',
      '  SetLength(Items, 4 + (Length(Items) * 2));',
      '',
      'Items[Count] := Value;',
      'Inc(Count);'
    ];
    Builder.isOverload := True;
    Builder.Build();

    Builder.Name := 'Add';
    Builder.addParam('Values', TArrayBufferType(FResType).FItemArr, lptConstRef);
    Builder.Body := [
      'var Len: Int32 := Length(Values);',
      'if (Count + Len >= Length(Items)) then',
      '  SetLength(Items, 4 + (Len + (Length(Items) * 2)));',
      'var i: Int32;',
      'for i := 0 to Len - 1 do',
      'begin',
      '  Items[Count] := Values[i];',
      '  Inc(Count);',
      'end;'
    ];
    Builder.isOverload := True;
    Builder.Build();

    Builder.Name := 'Clear';
    Builder.Body := [
      'Count := 0;'
    ];
    Builder.Build();

    // ToString: String
    Builder.Name := 'ToString';
    Builder.ResultType := FCompiler.getBaseType(ltString);
    Builder.Body := [
      'Result := "Count=" + System.ToString(Count) + LINE_SEP + "Items=" + System.ToString(System.Copy(Items, 0, Count));'
    ];
    Builder.Build();
    Builder.Free();

    addToStringOverride();
  end;

  Result := inherited;
end;

procedure InitializeArrayBuffer(Compiler: TLapeCompiler);
begin
  Compiler.InternalMethodMap['TArrayBuffer'] := TLapeTree_InternalMethod_ArrayBuffer;
end;

end.

