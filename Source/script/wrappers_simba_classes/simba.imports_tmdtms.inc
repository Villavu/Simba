pushSection('Classes');

addClass('TMDTMS');
addGlobalType('record x, y, Color, Tolerance, AreaSize, AreaShape: UInt32; end', 'TSDTMPointDef');
addGlobalType('array of TSDTMPointDef', 'TSDTMPointDefArray');
addGlobalType('record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end', 'TSDTM');
addGlobalFunc('function TMDTMS.AddDTM(const d: TSDTM): Integer;', @_LapeMDTMS_AddDTM);
addGlobalFunc('function TMDTMS.AddDTM(const d: TMDTM): Integer; overload', @_LapeMDTMS_AddDTMEx);
addGlobalFunc('function TMDTMS.ExistsDTM(index : integer): boolean;', @_LapeMDTMS_ExistsDTM);
addGlobalFunc('function TMDTMS.GetDTM(index: Integer): TMDTM;', @_LapeMDTMS_GetDTM);
addGlobalFunc('procedure TMDTMS.FreeDTM(DTM: Integer);', @_LapeMDTMS_FreeDTM);
addGlobalFunc('function TMDTMS.StringToDTM(const S: String): Integer;', @_LapeMDTMS_StringToDTM);
addGlobalFunc('procedure TMDTMS.Init(Owner: TObject)', @_LapeMDTMS_Init);
//addGlobalFunc('procedure TMDTMS.Free;', @_LapeMDTMS_Free);

popSection();

