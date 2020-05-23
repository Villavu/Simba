unit simbascript.import_tmfonts;
//Depends: TMFonts, TObject, String, Boolean, TFont, string, integer]: TMfont

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Register_TMFonts(Compiler: TScriptCompiler);

implementation

uses
  simba.fontloader, simba.ocrutil, graphics;

type
  PMFonts = ^TMFonts;
  PObject = ^TObject;
  PFont = ^TFont;
  POCRData = ^TOCRData;

//constructor Create(Owner : TObject);
procedure TMFonts_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^ := TMFonts.Create(PObject(Params^[1])^);
end;

//function GetFont(const Name: String): TOcrData;
procedure TMFonts_GetFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POcrData(Result)^ := PMFonts(Params^[0])^.GetFontData(PlpString(Params^[1])^);
end;

//function FreeFont(const Name: String): Boolean;
procedure TMFonts_FreeFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFonts(Params^[0])^.FreeFont(PlpString(Params^[1])^);
end;

//function LoadFont(const Name: String; Shadow: Boolean): boolean;
procedure TMFonts_LoadFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadFont(PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

//function LoadSystemFont(const SysFont : TFont; const FontName : string) : boolean;
procedure TMFonts_LoadSystemFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadSystemFont(PFont(Params^[1])^, PlpString(Params^[2])^);
end;

//function Copy(Owner : TObject): TMFonts;
procedure TMFonts_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Result)^ := PMFonts(Params^[0])^.Copy(PObject(Params^[1])^);
end;

//function Count : integer;
procedure TMFonts_Count(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMFonts(Params^[0])^.Count();
end;

//Read: property Path : string read GetPath write SetPath;
procedure TMFonts_Path_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMFonts(Params^[0])^.Path;
end;

//Write: property Path : string read GetPath write SetPath;
procedure TMFonts_Path_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^.Path := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TMFonts_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^.Free();
end;

procedure Register_TMFonts(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    addClass('TMFonts');

    addGlobalFunc('procedure TMFonts.Init(Owner : TObject);', @TMFonts_Init);
    addGlobalFunc('function TMFonts.GetFont(const Name: String): TOcrData; constref;', @TMFonts_GetFont);
    addGlobalFunc('function TMFonts.FreeFont(const Name: String): Boolean; constref;', @TMFonts_FreeFont);
    addGlobalFunc('function TMFonts.LoadFont(const Name: String; Shadow: Boolean): boolean; constref;', @TMFonts_LoadFont);
    addGlobalFunc('function TMFonts.LoadSystemFont(const SysFont : TFont; const FontName : string): boolean; constref;', @TMFonts_LoadSystemFont);
    addGlobalFunc('function TMFonts.Copy(Owner : TObject): TMFonts; constref;', @TMFonts_Copy);
    addGlobalFunc('function TMFonts.Count(): integer; constref;', @TMFonts_Count);
    addClassVar('TMFonts', 'Path', 'string', @TMFonts_Path_Read, @TMFonts_Path_Write);
    addGlobalFunc('procedure TMFonts.Free(); constref;', @TMFonts_Free);
  end;
end;

initialization
  RegisterScriptImport(@Register_TMFonts);

end.

