pushSection('Classes');

addClass('TMDTM');
addGlobalType('record x, y, c, t, asz: integer; bp: boolean; end', 'TMDTMPoint');
addGlobalType('^TMDTMPoint', 'PMDTMPoint');
addGlobalType('array of TMDTMPoint', 'TMDTMPointArray');
addClassVar('TMDTM', 'Name', 'string', @_LapeMDTM_Name_Read, @_LapeMDTM_Name_Write);
addClassVar('TMDTM', 'Index', 'integer', @_LapeMDTM_Index_Read, @_LapeMDTM_Index_Write);
addGlobalFunc('function TMDTM.ToString: string;', @_LapeMDTM_ToString);
addGlobalFunc('function TMDTM.SaveToFile(const FileName : string): boolean;', @_LapeMDTM_SaveToFile);
addGlobalFunc('function TMDTM.LoadFromString(const s : string): boolean;', @_LapeMDTM_LoadFromString);
addGlobalFunc('procedure TMDTM.Normalize;', @_LapeMDTM_Normalize);
addGlobalFunc('function TMDTM.Valid: boolean;', @_LapeMDTM_Valid);
addGlobalFunc('procedure TMDTM.DeletePoint(Point : integer);', @_LapeMDTM_DeletePoint);
addGlobalFunc('procedure TMDTM.SwapPoint(p1, p2: integer);', @_LapeMDTM_SwapPoint);
addGlobalFunc('procedure TMDTM.MovePoint(fromIndex, toIndex: integer);', @_LapeMDTM_MovePoint);
addGlobalFunc('function TMDTM.AddPoint(Point: TMDTMPoint): integer;', @_LapeMDTM_AddPoint);
addClassVar('TMDTM', 'PPoints', 'PMDTMPoint', @_LapeMDTM_PPoints_Read);
addClassVar('TMDTM', 'Count', 'integer', @_LapeMDTM_Count_Read, @_LapeMDTM_Count_Write);
addClassVar('TMDTM', 'Points', 'TMDTMPointArray', @_LapeMDTM_Points_Read);
addGlobalFunc('procedure TMDTM.Init()', @_LapeMDTM_Init);
//addGlobalFunc('procedure TMDTM.Free;', @_LapeMDTM_Free);

popSection();

