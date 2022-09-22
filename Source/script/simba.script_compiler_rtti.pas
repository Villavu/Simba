{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler_rtti;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lpcompiler;

procedure InitializeRTTI(Compiler: TLapeCompiler);

implementation

uses
  lptypes, lptree, lpvartypes, lpmessages, lpvartypes_record, lpvartypes_array;

type
  TLapeTree_InternalMethod_RTTIClassFields = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
  end;

  TLapeTree_InternalMethod_RTTIFields = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
  end;

procedure TLapeTree_InternalMethod_RTTIFields.ClearCache;
begin
  FConstant := bUnknown;

  inherited;
end;

function TLapeTree_InternalMethod_RTTIFields.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.addManagedType(TLapeType_DynArray.Create(FCompiler.getBaseType(ltString), FCompiler));

  Result := inherited;
end;

function TLapeTree_InternalMethod_RTTIFields.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  RecordType: TLapeType_Record;
  RecordVar, FieldVar: TResVar;
begin
  Dest := NullResVar;
  Result := _ResVar.New(FCompiler.getTempVar(resType()));

  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1]);
  if (not FParams[0].CompileToTempVar(Offset, RecordVar)) or (not (RecordVar.VarType is TLapeType_Record)) then
    LapeExceptionFmt(lpeExpected, ['Record type']);

  RecordType := TLapeType_Record(RecordVar.VarType);
  for i := 0 to RecordType.FieldMap.Count - 1 do
  begin
    with TLapeTree_Operator.Create(op_Dot, Self) do
    try
      Left := TLapeTree_ResVar.Create(RecordVar.IncLock(), Self);
      Right := TLapeTree_String.Create(FCompiler.getConstant(RecordType.FieldMap.Key[I]), Self);

      FieldVar := Compile(Offset);
    finally
      Free();
    end;

    // field name
    with TLapeTree_Operator.Create(op_Plus, Self) do
    try
      Dest := Result;
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Right := TLapeTree_String.Create(FCompiler.getConstant(RecordType.FieldMap.Key[I]), Self);

      Result := Compile(Offset);
    finally
      Free();
    end;

    // field type
    with TLapeTree_Operator.Create(op_Plus, Self) do
    try
      Dest := Result;
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      if FieldVar.HasType() then
        if (FieldVar.VarType.Name <> '') then
          Right := TLapeTree_String.Create(FieldVar.VarType.Name, Self)
        else
          Right := TLapeTree_String.Create(FieldVar.VarType.AsString, Self)
      else
        Right := TLapeTree_String.Create('', Self);

      Result := Compile(Offset);
    finally
      Free();
    end;

    // field value
    with TLapeTree_Operator.Create(op_Plus, Self) do
    try
      Dest := Result;

      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Right := TLapeTree_InternalMethod_ToStr.Create(Self);
      with TLapeTree_InternalMethod_ToStr(Right) do
        addParam(TLapeTree_ResVar.Create(FieldVar.IncLock(), Self));

      Result := Compile(Offset);
    finally
      Free();
    end;

    FieldVar.Spill(1);
  end;
end;

function TLapeTree_InternalMethod_RTTIFields.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) and (FParams[0] is TLapeTree_VarType) then
      FConstant := bTrue
    else
      FConstant := bFalse;

  Result := inherited;
end;

function TLapeTree_InternalMethod_RTTIFields.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
  RecordType: TLapeType_Record;
  i: Integer;
  ArrayVar: TLapeGlobalVar;
  Field: TRecordField;
  FieldType, FieldValue: TLapeGlobalVar;
begin
  if (FRes = nil) and isConstant() then
  begin
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (not (ParamType is TLapeType_Type)) then
      LapeException(lpeTypeExpected, DocPos);
    ParamType := TLapeType_Type(ParamType).TType;
    if (not (ParamType is TLapeType_Record)) then
      LapeExceptionFmt(lpeExpected, ['Record type']);

    RecordType := TLapeType_Record(ParamType);

    FRes := FCompiler.addManagedDecl(resType().NewGlobalVarP()) as TLapeGlobalVar;
    for i := 0 to RecordType.FieldMap.Count - 1 do
    begin
      Field := RecordType.FieldMap.ItemsI[i];
      if (Field.FieldType <> nil) then
      begin
        if (Field.FieldType.Name <> '') then
          FieldType := FCompiler.getConstant(Field.FieldType.Name)
        else
          FieldType := FCompiler.getConstant(Field.FieldType.AsString);

        with RecordType.FieldMap.ItemsI[i].FieldType.NewGlobalVarP() do
        try
          FieldValue := FCompiler.getConstant(AsString);
        finally
          Free();
        end;
      end else
      begin
        FieldType := FCompiler.getConstant('');
        FieldValue := FCompiler.getConstant('');
      end;

      // field name
      ArrayVar := FRes; FRes := TLapeType_DynArray(FRes.VarType).EvalConst(op_Plus, FRes, FCompiler.getConstant(RecordType.FieldMap.Key[I]), []);
      ArrayVar.Free();

      // field type
      ArrayVar := FRes; FRes := TLapeType_DynArray(FRes.VarType).EvalConst(op_Plus, FRes, FieldType, []);
      ArrayVar.Free();

      // field var
      ArrayVar := FRes; FRes := TLapeType_DynArray(FRes.VarType).EvalConst(op_Plus, FRes, FieldValue, []);
      ArrayVar.Free();
    end;
  end;

  Result := inherited;
end;

procedure TLapeTree_InternalMethod_RTTIClassFields.ClearCache;
begin
  FConstant := bUnknown;

  inherited;
end;

function TLapeTree_InternalMethod_RTTIClassFields.resType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count > 0) and (not isEmpty(FParams[0])) then
    FResType := FCompiler.addManagedType(TLapeType_DynArray.Create(FCompiler.getBaseType(ltString), FCompiler));

  Result := inherited;
end;

function TLapeTree_InternalMethod_RTTIClassFields.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  RecordType: TLapeType_Record;
  RecordVar: TResVar;
  FieldVar: TLapeGlobalVar;
  Decls: TLapeDeclArray;
begin
  Dest := NullResVar;
  Result := _ResVar.New(FCompiler.getTempVar(resType()));

  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1]);
  if (not FParams[0].CompileToTempVar(Offset, RecordVar)) or (not (RecordVar.VarType is TLapeType_Record)) then
    LapeExceptionFmt(lpeExpected, ['Record type']);

  RecordType := TLapeType_Record(RecordVar.VarType);
  Decls := RecordType.ManagedDeclarations.GetByClass(TLapeGlobalVar, bTrue);
  for i := 0 to High(Decls) do
  begin
    FieldVar := TLapeGlobalVar(Decls[i]);
    if (FieldVar.BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
      Continue;

    // field name
    with TLapeTree_Operator.Create(op_Plus, Self) do
    try
      Dest := Result;
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Right := TLapeTree_String.Create(FieldVar.Name, Self);

      Result := Compile(Offset);
    finally
      Free();
    end;

    // field type
    with TLapeTree_Operator.Create(op_Plus, Self) do
    try
      Dest := Result;
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      if FieldVar.HasType() then
        if (FieldVar.VarType.Name <> '') then
          Right := TLapeTree_String.Create(FieldVar.VarType.Name, Self)
        else
          Right := TLapeTree_String.Create(FieldVar.VarType.AsString, Self)
      else
        Right := TLapeTree_String.Create('', Self);

      Result := Compile(Offset);
    finally
      Free();
    end;

    // field value
    with TLapeTree_Operator.Create(op_Plus, Self) do
    try
      Dest := Result;

      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Right := TLapeTree_InternalMethod_ToStr.Create(Self);
      with TLapeTree_InternalMethod_ToStr(Right) do
        addParam(TLapeTree_GlobalVar.Create(FieldVar, Self));

      Result := Compile(Offset);
    finally
      Free();
    end;
  end;

  RecordVar.Spill();
end;

function TLapeTree_InternalMethod_RTTIClassFields.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) and (FParams[0] is TLapeTree_VarType) then
      FConstant := bTrue
    else
      FConstant := bFalse;

  Result := inherited;
end;

function TLapeTree_InternalMethod_RTTIClassFields.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
  i: Integer;
  FieldVar, ArrayVar: TLapeGlobalVar;
  FieldType: TLapeGlobalVar;
  Decls: TLapeDeclArray;
begin
  if (FRes = nil) and isConstant() then
  begin
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (not (ParamType is TLapeType_Type)) then
      LapeException(lpeTypeExpected, DocPos);
    ParamType := TLapeType_Type(ParamType).TType;
    if (not (ParamType is TLapeType_Record)) then
      LapeExceptionFmt(lpeExpected, ['Record type']);

    FRes := FCompiler.addManagedDecl(resType().NewGlobalVarP()) as TLapeGlobalVar;

    Decls := ParamType.ManagedDeclarations.GetByClass(TLapeGlobalVar, bTrue);
    for i := 0 to High(Decls) do
    begin
      FieldVar := TLapeGlobalVar(Decls[i]);
      if (FieldVar.BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
        Continue;

      if FieldVar.HasType() then
        if (FieldVar.VarType.Name <> '') then
          FieldType := FCompiler.getConstant(FieldVar.VarType.Name)
        else
          FieldType := FCompiler.getConstant(FieldVar.VarType.AsString)
      else
        FieldType := FCompiler.getConstant('');

      // field name
      ArrayVar := FRes; FRes := TLapeType_DynArray(FRes.VarType).EvalConst(op_Plus, FRes, FCompiler.getConstant(FieldVar.Name), []);
      ArrayVar.Free();

      // field type
      ArrayVar := FRes; FRes := TLapeType_DynArray(FRes.VarType).EvalConst(op_Plus, FRes, FieldType, []);
      ArrayVar.Free();

      // field var
      ArrayVar := FRes; FRes := TLapeType_DynArray(FRes.VarType).EvalConst(op_Plus, FRes, FCompiler.getConstant(FieldVar.AsString), []);
      ArrayVar.Free();
    end;
  end;

  Result := inherited;
end;

procedure InitializeRTTI(Compiler: TLapeCompiler);
begin
  Compiler.InternalMethodMap['RTTIClassFields'] := TLapeTree_InternalMethod_RTTIClassFields;
  Compiler.InternalMethodMap['RTTIFields']      := TLapeTree_InternalMethod_RTTIFields;
end;

end.

