addClass('TCustomFloatSpinEdit', 'TCustomEdit');
addGlobalFunc('function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double; constref', @_LapeCustomFloatSpinEdit_GetLimitedValue);
addGlobalFunc('function TCustomFloatSpinEdit.ValueToStr(const AValue: Double): String; constref', @_LapeCustomFloatSpinEdit_ValueToStr);
addGlobalFunc('function TCustomFloatSpinEdit.StrToValue(const S: String): Double; constref', @_LapeCustomFloatSpinEdit_StrToValue);
addClassVar('TCustomFloatSpinEdit', 'DecimalPlaces', 'Integer', @_LapeCustomFloatSpinEdit_DecimalPlaces_Read, @_LapeCustomFloatSpinEdit_DecimalPlaces_Write);
addClassVar('TCustomFloatSpinEdit', 'Increment', 'Double', @_LapeCustomFloatSpinEdit_Increment_Read, @_LapeCustomFloatSpinEdit_Increment_Write);
addClassVar('TCustomFloatSpinEdit', 'MinValue', 'Double', @_LapeCustomFloatSpinEdit_MinValue_Read, @_LapeCustomFloatSpinEdit_MinValue_Write);
addClassVar('TCustomFloatSpinEdit', 'MaxValue', 'Double', @_LapeCustomFloatSpinEdit_MaxValue_Read, @_LapeCustomFloatSpinEdit_MaxValue_Write);
addClassVar('TCustomFloatSpinEdit', 'Value', 'Double', @_LapeCustomFloatSpinEdit_Value_Read, @_LapeCustomFloatSpinEdit_Value_Write);
addClassVar('TCustomFloatSpinEdit', 'ValueEmpty', 'Boolean', @_LapeCustomFloatSpinEdit_ValueEmpty_Read, @_LapeCustomFloatSpinEdit_ValueEmpty_Write);
addGlobalFunc('procedure TCustomFloatSpinEdit.Init(TheOwner: TComponent); override', @_LapeCustomFloatSpinEdit_Init);
//addGlobalFunc('procedure TCustomFloatSpinEdit.Free; constref', @_LapeCustomFloatSpinEdit_Free);

addClass('TFloatSpinEdit', 'TCustomFloatSpinEdit');
addGlobalFunc('procedure TFloatSpinEdit.Init(TheOwner: TComponent); override', @_LapeFloatSpinEdit_Init);
//addGlobalFunc('procedure TFloatSpinEdit.Free; constref', @_LapeFloatSpinEdit_Free);

addClass('TCustomSpinEdit', 'TCustomFloatSpinEdit');
addClassVar('TCustomSpinEdit', 'Value', 'integer', @_LapeCustomSpinEdit_Value_Read, @_LapeCustomSpinEdit_Value_Write);
addClassVar('TCustomSpinEdit', 'MinValue', 'integer', @_LapeCustomSpinEdit_MinValue_Read, @_LapeCustomSpinEdit_MinValue_Write);
addClassVar('TCustomSpinEdit', 'MaxValue', 'integer', @_LapeCustomSpinEdit_MaxValue_Read, @_LapeCustomSpinEdit_MaxValue_Write);
addClassVar('TCustomSpinEdit', 'Increment', 'integer', @_LapeCustomSpinEdit_Increment_Read, @_LapeCustomSpinEdit_Increment_Write);
addGlobalFunc('procedure TCustomSpinEdit.Init(TheOwner: TComponent); override', @_LapeCustomSpinEdit_Init);
//addGlobalFunc('procedure TCustomSpinEdit.Free; constref', @_LapeCustomSpinEdit_Free);

addClass('TSpinEdit', 'TCustomSpinEdit');
addGlobalFunc('procedure TSpinEdit.Init(TheOwner: TComponent); override', @_LapeSpinEdit_Init);
// addGlobalFunc('procedure TSpinEdit.Free; constref', @_LapeSpinEdit_Free);

