unit lpTMBitmaps;
//Depends: TMBitmaps, TObject, Array of integer, TMufasaBmpArray, integer, integer]: TMufasaBitmap, TMufasaBitmap, Integer, TBmpMirrorStyle, string

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMBitmaps(Compiler: TLapeCompiler);

implementation

uses
  bitmaps, MufasaTypes;

type
  PMBitmaps = ^TMBitmaps;
  PObject = ^TObject;
  PBmpMirrorStyle = ^TBmpMirrorStyle;

//function GetBMP(Index : integer) : TMufasaBitmap;
procedure TMBitmaps_GetBMP(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMufasaBitmap(Result)^ := PMBitmaps(Params^[0])^.GetBMP(Pinteger(Params^[1])^);
end;

//function CreateBMP(w, h: integer): Integer;
procedure TMBitmaps_CreateBMP(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMP(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function ExistsBMP(Index : integer) : boolean;
procedure TMBitmaps_ExistsBMP(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMBitmaps(Params^[0])^.ExistsBMP(Pinteger(Params^[1])^);
end;

//function AddBMP(_bmp: TMufasaBitmap): Integer;
procedure TMBitmaps_AddBMP(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.AddBMP(PMufasaBitmap(Params^[1])^);
end;

//function CopyBMP( Bitmap : integer) : Integer;
procedure TMBitmaps_CopyBMP(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.CopyBMP(Pinteger(Params^[1])^);
end;

//function CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer;
procedure TMBitmaps_CreateMirroredBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.CreateMirroredBitmap(PInteger(Params^[1])^, PBmpMirrorStyle(Params^[2])^);
end;

//function CreateBMPFromFile(const Path : string) : integer;
procedure TMBitmaps_CreateBMPFromFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMPFromFile(PlpString(Params^[1])^);
end;

//function CreateBMPFromString(width,height : integer; Data : string) : integer;overload;
procedure TMBitmaps_CreateBMPFromString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMPFromString(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PlpString(Params^[3])^);
end;

//function CreateBMPFromString(BmpName : string; width,height : integer; Data : string) : integer;overload;
procedure TMBitmaps_CreateBMPFromStringEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMPFromString(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, PlpString(Params^[4])^);
end;

//procedure FreeBMP( Number : integer);
procedure TMBitmaps_FreeBMP(const Params: PParamArray); lape_extdecl
begin
  PMBitmaps(Params^[0])^.FreeBMP(Pinteger(Params^[1])^);
end;

//constructor Create(Owner : TObject);
procedure TMBitmaps_Init(const Params: PParamArray); lape_extdecl
begin
  PMBitmaps(Params^[0])^ := TMBitmaps.Create(PObject(Params^[1])^);
end;

//procedure Free();
procedure TMBitmaps_Free(const Params: PParamArray); lape_extdecl
begin
  PMBitmaps(Params^[0])^.Free();
end;

procedure Register_TMBitmaps(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMBitmaps', 'TObject');

    addGlobalFunc('function TMBitmaps.GetBMP(Index : integer): TMufasaBitmap;', @TMBitmaps_GetBMP);
    addGlobalFunc('function TMBitmaps.CreateBMP(w, h: integer): Integer;', @TMBitmaps_CreateBMP);
    addGlobalFunc('function TMBitmaps.ExistsBMP(Index : integer): boolean;', @TMBitmaps_ExistsBMP);
    addGlobalFunc('function TMBitmaps.AddBMP(_bmp: TMufasaBitmap): Integer;', @TMBitmaps_AddBMP);
    addGlobalFunc('function TMBitmaps.CopyBMP( Bitmap : integer): Integer;', @TMBitmaps_CopyBMP);
    addGlobalFunc('function TMBitmaps.CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer;', @TMBitmaps_CreateMirroredBitmap);
    addGlobalFunc('function TMBitmaps.CreateBMPFromFile(const Path : string): integer;', @TMBitmaps_CreateBMPFromFile);
    addGlobalFunc('function TMBitmaps.CreateBMPFromString(width,height : integer; Data : string): integer;', @TMBitmaps_CreateBMPFromString);
    addGlobalFunc('function TMBitmaps.CreateBMPFromString(BmpName : string; width,height : integer; Data : string): integer; overload;', @TMBitmaps_CreateBMPFromStringEx);
    addGlobalFunc('procedure TMBitmaps.FreeBMP( Number : integer);', @TMBitmaps_FreeBMP);
    addGlobalFunc('procedure TMBitmaps.Init(Owner : TObject);', @TMBitmaps_Init);
    addGlobalFunc('procedure TMBitmaps.Free();', @TMBitmaps_Free);
  end;
end;

end.

