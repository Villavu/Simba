pushSection('Classes');

addClass('TObject', 'Pointer');

addGlobalFunc('procedure TObject.Init;', @_LapeObject_Init);
addGlobalFunc('procedure TObject.Free;', @_LapeObject_Free);
addGlobalFunc('function TObject.ClassName: string;', @_LapeObject_ClassName);

popSection();
