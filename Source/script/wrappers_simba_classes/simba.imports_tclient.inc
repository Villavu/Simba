pushSection('Classes');

addClass('TClient');
addClassVar('TClient', 'IOManager', 'TIOManager', @_LapeClient_IOManager_Read, @_LapeClient_IOManager_Write);
addClassVar('TClient', 'MFiles', 'TMFiles', @_LapeClient_MFiles_Read, @_LapeClient_MFiles_Write);
addClassVar('TClient', 'MFinder', 'TMFinder', @_LapeClient_MFinder_Read, @_LapeClient_MFinder_Write);
addClassVar('TClient', 'MBitmaps', 'TMBitmaps', @_LapeClient_MBitmaps_Read, @_LapeClient_MBitmaps_Write);
addClassVar('TClient', 'MDTMs', 'TMDTMS', @_LapeClient_MDTMs_Read, @_LapeClient_MDTMs_Write);
addClassVar('TClient', 'MOCR', 'TMOCR', @_LapeClient_MOCR_Read, @_LapeClient_MOCR_Write);
addGlobalVar('TClient', nil, 'Client');
addGlobalFunc('procedure TClient.Init(const plugin_dir: string = ''''; const UseIOManager: TIOManager = nil)', @_LapeClient_Init);
//addGlobalFunc('procedure TClient.Free;', @_LapeClient_Free);

popSection();
