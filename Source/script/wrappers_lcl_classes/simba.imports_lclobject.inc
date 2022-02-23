pushSection('Classes');

addClass('TObject', 'Pointer');

addGlobalFunc('procedure TObject.Init;', @_LapeObject_Init);
addGlobalFunc('procedure TObject.Free; constref;', @_LapeObject_Free);
addGlobalFunc('function TObject.ClassName: string; constref;', @_LapeObject_ClassName);

popSection();
