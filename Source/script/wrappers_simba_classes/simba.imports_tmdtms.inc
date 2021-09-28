addClass('TMDTMS');
addGlobalType('record x, y, Color, Tolerance, AreaSize, AreaShape: UInt32; end', 'TSDTMPointDef');
addGlobalType('array of TSDTMPointDef', 'TSDTMPointDefArray');
addGlobalType('record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end', 'TSDTM');
addGlobalFunc('function TMDTMS.AddDTM(const d: TSDTM): Integer; constref', @_LapeMDTMS_AddDTM);
addGlobalFunc('function TMDTMS.AddDTM(const d: TMDTM): Integer; constref; overload', @_LapeMDTMS_AddDTMEx);
addGlobalFunc('function TMDTMS.ExistsDTM(index : integer): boolean; constref', @_LapeMDTMS_ExistsDTM);
addGlobalFunc('function TMDTMS.GetDTM(index: Integer): TMDTM; constref', @_LapeMDTMS_GetDTM);
addGlobalFunc('procedure TMDTMS.FreeDTM(DTM: Integer); constref', @_LapeMDTMS_FreeDTM);
addGlobalFunc('function TMDTMS.StringToDTM(const S: String): Integer; constref', @_LapeMDTMS_StringToDTM);
addGlobalFunc('procedure TMDTMS.Init(Owner: TObject)', @_LapeMDTMS_Init);
//addGlobalFunc('procedure TMDTMS.Free; constref', @_LapeMDTMS_Free);

