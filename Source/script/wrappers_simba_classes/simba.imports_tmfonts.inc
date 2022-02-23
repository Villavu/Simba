pushSection('Classes');

addClass('TMFonts');
addGlobalFunc('procedure TMFonts.Init(Owner : TObject)', @_LapeMFonts_Init);
addGlobalFunc('function TMFonts.GetFont(const Name: String): TOcrData; constref', @_LapeMFonts_GetFont);
addGlobalFunc('function TMFonts.FreeFont(const Name: String): Boolean; constref', @_LapeMFonts_FreeFont);
addGlobalFunc('function TMFonts.LoadFont(const Name: String; Shadow: Boolean): boolean; constref', @_LapeMFonts_LoadFont);
addGlobalFunc('function TMFonts.LoadSystemFont(const SysFont : TFont; const FontName : string): boolean; constref', @_LapeMFonts_LoadSystemFont);
addGlobalFunc('function TMFonts.Copy(Owner : TObject): TMFonts; constref', @_LapeMFonts_Copy);
addGlobalFunc('function TMFonts.Count: integer; constref', @_LapeMFonts_Count);
addClassVar('TMFonts', 'Path', 'string', @_LapeMFonts_Path_Read, @_LapeMFonts_Path_Write);
//addGlobalFunc('procedure TMFonts.Free; constref', @_LapeMFonts_Free);

popSection();

