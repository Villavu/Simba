unit lpTMFonts;
//Depends: TMFonts, TObject, String, Boolean, TFont, string, integer]: TMfont

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMFonts(Compiler: TLapeCompiler);

implementation

uses
  fontloader, ocrutil, graphics, MufasaTypes;

type
  PMFonts = ^TMFonts;
  PObject = ^TObject;
  PFont = ^TFont;
  POCRData = ^TOCRData;

//constructor Create(Owner : TObject);
procedure TMFonts_Init(const Params: PParamArray); lape_extdecl
begin
  PMFonts(Params^[0])^ := TMFonts.Create(PObject(Params^[1])^);
end;

//function GetFont(const Name: String): TOcrData;
procedure TMFonts_GetFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  POcrData(Result)^ := PMFonts(Params^[0])^.GetFont(PlpString(Params^[1])^);
end;

//function FreeFont(const Name: String): Boolean;
procedure TMFonts_FreeFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMFonts(Params^[0])^.FreeFont(PlpString(Params^[1])^);
end;

//function LoadFont(const Name: String; Shadow: Boolean): boolean;
procedure TMFonts_LoadFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadFont(PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

//function LoadSystemFont(const SysFont : TFont; const FontName : string) : boolean;
procedure TMFonts_LoadSystemFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadSystemFont(PFont(Params^[1])^, PlpString(Params^[2])^);
end;

//function Copy(Owner : TObject): TMFonts;
procedure TMFonts_Copy(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMFonts(Result)^ := PMFonts(Params^[0])^.Copy(PObject(Params^[1])^);
end;

//function Count : integer;
procedure TMFonts_Count(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMFonts(Params^[0])^.Count();
end;

//Read: property Path : string read GetPath write SetPath;
procedure TMFonts_Path_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMFonts(Params^[0])^.Path;
end;

//Write: property Path : string read GetPath write SetPath;
procedure TMFonts_Path_Write(const Params: PParamArray); lape_extdecl
begin
  PMFonts(Params^[0])^.Path := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TMFonts_Free(const Params: PParamArray); lape_extdecl
begin
  PMFonts(Params^[0])^.Free();
end;

procedure Register_TMFonts(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMFonts', 'TObject');

    addGlobalFunc('procedure TMFonts.Init(Owner : TObject);', @TMFonts_Init);
    addGlobalFunc('function TMFonts.GetFont(const Name: String): TOcrData;', @TMFonts_GetFont);
    addGlobalFunc('function TMFonts.FreeFont(const Name: String): Boolean;', @TMFonts_FreeFont);
    addGlobalFunc('function TMFonts.LoadFont(const Name: String; Shadow: Boolean): boolean;', @TMFonts_LoadFont);
    addGlobalFunc('function TMFonts.LoadSystemFont(const SysFont : TFont; const FontName : string): boolean;', @TMFonts_LoadSystemFont);
    addGlobalFunc('function TMFonts.Copy(Owner : TObject): TMFonts;', @TMFonts_Copy);
    addGlobalFunc('function TMFonts.Count(): integer;', @TMFonts_Count);
    addClassVar(Compiler, 'TMFonts', 'Path', 'string', @TMFonts_Path_Read, @TMFonts_Path_Write);
    addGlobalFunc('procedure TMFonts.Free();', @TMFonts_Free);
  end;
end;

end.

