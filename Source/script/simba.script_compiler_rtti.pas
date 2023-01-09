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
  lptypes, lptree, lpvartypes, lpmessages, lpvartypes_record, lpinternalmethods;

type
  TLapeTree_InternalMethod_RTTIFields = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
    function isConstant: Boolean; override;
  end;

  TLapeTree_InternalMethod_RTTIClassFields = class(TLapeTree_InternalMethod_RTTIFields)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

function TLapeTree_InternalMethod_RTTIFields.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getGlobalType('TRTTIFields');

  Result := inherited;
end;

function TLapeTree_InternalMethod_RTTIFields.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  RecordVar: TResVar;
  FieldName: lpString;
  RecordType, FieldType: TLapeType;
  NameExpr, TypeExpr, ValueExpr: TLapeTree_ExprBase;
  FieldVar: TLapeTree_Operator;
begin
  Dest := NullResVar;
  Result := _ResVar.New(FCompiler.getTempVar(resType())).IncLock();

  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1]);

  RecordVar := FParams[0].Compile(Offset);
  RecordType := RecordVar.VarType;
  if (RecordType is TLapeType_Type) then
    RecordType := TLapeType_Type(RecordType).TType;

  if (RecordType is TLapeType_Record) then
    with RecordType as TLapeType_Record do
      for i := 0 to FieldMap.Count - 1 do
      begin
        FieldName := FieldMap.Key[i];
        FieldType := FieldMap.ItemsI[i].FieldType;

        if (FieldType <> nil) then
          if (FieldType.Name <> '') then
            TypeExpr := TLapeTree_String.Create(FieldType.Name, Self)
          else
            TypeExpr := TLapeTree_String.Create(FieldType.AsString, Self)
        else
          TypeExpr := TLapeTree_String.Create('', Self);

        NameExpr  := TLapeTree_String.Create(FieldName, Self);
        ValueExpr := TLapeTree_InternalMethod_ToStr.Create(Self);

        with TLapeTree_InternalMethod_ToStr(ValueExpr) do
        begin
          if (RecordVar.VarType is TLapeType_Type) then
            addParam(TLapeTree_GlobalVar.Create(FieldType.NewGlobalVarP(), Self))
          else
          begin
            FieldVar := TLapeTree_Operator.Create(op_Dot, Self);
            FieldVar.Left := TLapeTree_ResVar.Create(RecordVar.IncLock(), Self);
            FieldVar.Right := TLapeTree_String.Create(FieldName, Self);

            addParam(FieldVar);
          end;
        end;

        with TLapeTree_Operator.Create(op_Plus, Self) do
        try
          Dest := Result;

          Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
          Right := TLapeTree_OpenArray.Create(Self);
          with TLapeTree_OpenArray(Right) do
          begin
            addValue(NameExpr);
            addValue(TypeExpr);
            addValue(ValueExpr);
          end;

          Result := Compile(Offset);
        finally
          Free();
        end;
      end
  else
    LapeExceptionFmt(lpeExpected, ['Record']);
end;

function TLapeTree_InternalMethod_RTTIFields.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    FConstant := bFalse;

  Result := inherited;
end;

function TLapeTree_InternalMethod_RTTIClassFields.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  Decls: TLapeDeclArray;
  RecordVar: TResVar;
  FieldName: lpString;
  RecordType, FieldType: TLapeType;
  NameExpr, TypeExpr, ValueExpr: TLapeTree_ExprBase;
  FieldVar: TLapeGlobalVar;
begin
  Dest := NullResVar;
  Result := _ResVar.New(FCompiler.getTempVar(resType())).IncLock();

  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1]);

  RecordVar := FParams[0].Compile(Offset);
  RecordType := RecordVar.VarType;
  if (RecordType is TLapeType_Type) then
    RecordType := TLapeType_Type(RecordType).TType;

  if (RecordType is TLapeType_Record) then
  begin
    Decls := RecordType.ManagedDeclarations.GetByClass(TLapeGlobalVar, bTrue);

    for i := 0 to High(Decls) do
    begin
      FieldVar := TLapeGlobalVar(Decls[i]);
      if (FieldVar.BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
        Continue;

      FieldName := FieldVar.Name;
      FieldType := FieldVar.VarType;

      if (FieldType <> nil) then
        if (FieldType.Name <> '') then
          TypeExpr := TLapeTree_String.Create(FieldType.Name, Self)
        else
          TypeExpr := TLapeTree_String.Create(FieldType.AsString, Self)
      else
        TypeExpr := TLapeTree_String.Create('', Self);

      NameExpr  := TLapeTree_String.Create(FieldName, Self);
      ValueExpr := TLapeTree_InternalMethod_ToStr.Create(Self);
      with TLapeTree_InternalMethod_ToStr(ValueExpr) do
        addParam(TLapeTree_GlobalVar.Create(FieldVar, Self));

      with TLapeTree_Operator.Create(op_Plus, Self) do
      try
        Dest := Result;

        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        Right := TLapeTree_OpenArray.Create(Self);
        with TLapeTree_OpenArray(Right) do
        begin
          addValue(NameExpr);
          addValue(TypeExpr);
          addValue(ValueExpr);
        end;

        Result := Compile(Offset);
      finally
        Free();
      end;
    end;
  end else
    LapeExceptionFmt(lpeExpected, ['Record']);
end;

procedure InitializeRTTI(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalType('record Name, VarType, Value: String; end;', 'TRTTIField');
  Compiler.addGlobalType('array of TRTTIField;', 'TRTTIFields');

  Compiler.InternalMethodMap['RTTIClassFields'] := TLapeTree_InternalMethod_RTTIClassFields;
  Compiler.InternalMethodMap['RTTIFields']      := TLapeTree_InternalMethod_RTTIFields;
end;

end.

