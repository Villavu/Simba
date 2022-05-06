pushSection('Variant');

addGlobalType('enum(VarUnknown, VarUnassigned, VarNull, VarInt8, VarInt16, VarInt32, VarInt64, VarUInt8, VarUInt16, VarUInt32, VarUInt64, VarSingle, VarDouble, VarDate, VarCurrency, VarBoolean, VarVariant, VarString, VarUnicodeString)', 'EVarType');

addGlobalVar(Variants.Null, 'Null').isConstant := True;
addGlobalVar(Variants.Unassigned, 'Unassigned').isConstant := True;

addGlobalFunc('function VarType(const V: Variant): EVarType;', @_LapeVarType);
addGlobalFunc('function VarIsOrdinal(const V: Variant): EvalBool;', @_LapeVarIsOrdinal);
addGlobalFunc('function VarIsFloat(const V: Variant): EvalBool;', @_LapeVarIsFloat);
addGlobalFunc('function VarIsNumeric(const V: Variant): EvalBool;', @_LapeVarIsNumeric);
addGlobalFunc('function VarIsStr(const V: Variant): EvalBool;', @_LapeVarIsStr);

popSection();

