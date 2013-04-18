unit lpTMFont;
//Depends: TMFont, TObject, String, TOcrData

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMFont(Compiler: TLapeCompiler);

implementation

uses
  fontloader, ocrutil, MufasaTypes;

type
  PMFont = ^TMFont;
  POCRData = ^TOCRData;

//Read: Name: String;
procedure TMFont_Name_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMFont(Params^[0])^.Name;
end;

//Write: Name: String;
procedure TMFont_Name_Write(const Params: PParamArray); lape_extdecl
begin
  PMFont(Params^[0])^.Name := PlpString(Params^[1])^;
end;

//Read: Data: TOcrData;
procedure TMFont_Data_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  POcrData(Result)^ := PMFont(Params^[0])^.Data;
end;

//Write: Data: TOcrData;
procedure TMFont_Data_Write(const Params: PParamArray); lape_extdecl
begin
  PMFont(Params^[0])^.Data := POcrData(Params^[1])^;
end;

//constructor Create;
procedure TMFont_Init(const Params: PParamArray); lape_extdecl
begin
  PMFont(Params^[0])^ := TMFont.Create();
end;

//function Copy: TMFont;
procedure TMFont_Copy(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMFont(Result)^ := PMFont(Params^[0])^.Copy();
end;

//procedure Free();
procedure TMFont_Free(const Params: PParamArray); lape_extdecl
begin
  PMFont(Params^[0])^.Free();
end;

procedure Register_TMFont(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMFont', 'TObject');

    addGlobalType('record xoff, yoff, width, height, index: Int32; inited: boolean; end;', 'TOCRGlyphMetric');
    addGlobalType('record ascii: array[0..255] of TOCRGlyphMetric; pos: T2DIntegerArray; pos_adj: array of Double; neg: T2DIntegerArray; neg_adj: array of Double; map: array of char; width, height, max_width, max_height, inputs, outputs: integer; end;', 'TOCRData');
    addGlobalType('array of TOCRData', 'TOCRDataArray');

    addClassVar(Compiler, 'TMFont', 'Name', 'String', @TMFont_Name_Read, @TMFont_Name_Write);
    addClassVar(Compiler, 'TMFont', 'Data', 'TOcrData', @TMFont_Data_Read, @TMFont_Data_Write);
    addGlobalFunc('procedure TMFont.Init();', @TMFont_Init);
    addGlobalFunc('function TMFont.Copy(): TMFont;', @TMFont_Copy);
    addGlobalFunc('procedure TMFont.Free();', @TMFont_Free);
  end;
end;

end.

