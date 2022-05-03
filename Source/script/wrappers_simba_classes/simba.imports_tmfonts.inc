pushSection('Classes');

addClass('TMFonts');
addGlobalFunc('procedure TMFonts.Init(Owner : TObject)', @_LapeMFonts_Init);
addGlobalFunc('function TMFonts.GetFont(const Name: String): TOcrData;', @_LapeMFonts_GetFont);
addGlobalFunc('function TMFonts.FreeFont(const Name: String): Boolean;', @_LapeMFonts_FreeFont);
addGlobalFunc('function TMFonts.LoadFont(const Name: String; Shadow: Boolean): boolean;', @_LapeMFonts_LoadFont);
addGlobalFunc('function TMFonts.LoadSystemFont(const SysFont : TFont; const FontName : string): boolean;', @_LapeMFonts_LoadSystemFont);
addGlobalFunc('function TMFonts.Copy(Owner : TObject): TMFonts;', @_LapeMFonts_Copy);
addGlobalFunc('function TMFonts.Count: integer;', @_LapeMFonts_Count);
addClassVar('TMFonts', 'Path', 'string', @_LapeMFonts_Path_Read, @_LapeMFonts_Path_Write);
//addGlobalFunc('procedure TMFonts.Free;', @_LapeMFonts_Free);

popSection();

