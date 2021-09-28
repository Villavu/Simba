addClass('TMFont');
addGlobalType('record xoff, yoff, width, height, index: Int32; inited: boolean; end', 'TOCRGlyphMetric');
addGlobalType('record ascii: array[0..255] of TOCRGlyphMetric; pos: T2DIntegerArray; pos_adj: array of Double; neg: T2DIntegerArray; neg_adj: array of Double; map: array of char; width, height, max_width, max_height, inputs, outputs: integer; end', 'TOCRData');
addGlobalType('array of TOCRData', 'TOCRDataArray');
addClassVar('TMFont', 'Name', 'String', @_LapeMFont_Name_Read, @_LapeMFont_Name_Write);
addClassVar('TMFont', 'Data', 'TOcrData', @_LapeMFont_Data_Read, @_LapeMFont_Data_Write);
//addGlobalFunc('procedure TMFont.Init()', @_LapeMFont_Init);
addGlobalFunc('function TMFont.Copy: TMFont; constref', @_LapeMFont_Copy);
//addGlobalFunc('procedure TMFont.Free; constref', @_LapeMFont_Free);

