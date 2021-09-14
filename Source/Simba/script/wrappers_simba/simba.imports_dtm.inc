FSection := 'DTM';
addGlobalFunc('procedure SetDTMName(DTM: Int32; Name: String)', @_LapeSetDTMName);
addGlobalFunc('function DTMFromString(S: String): Int32', @_LapeDTMFromString);
addGlobalFunc('procedure FreeDTM(DTM: Int32)', @_LapeFreeDTM);
addGlobalFunc('function GetDTM(Index: Int32): TMDTM', @_LapeGetDTM);
addGlobalFunc('function AddSDTM(d: TSDTM): Int32', @_LapeAddTSDTM);
addGlobalFunc('function AddDTM(d: TMDTM): Int32', @_LapeAddDTM);
addGlobalFunc('function DTMExists(Index : Int32): Boolean', @_LapeDTMExists);
addGlobalFunc('function MDTMToSDTM(DTM: TMDTM): TSDTM', @_LapeMDTMToSDTM);
addGlobalFunc('function SDTMToMDTM(DTM: TSDTM): TMDTM', @_LapeSDTMToMDTM);
addGlobalFunc('function CreateDTMPoint(x,y,c,t,asz: Int32; bp: Boolean): TMDTMPoint', @_LapeCreateDTMPoint);

