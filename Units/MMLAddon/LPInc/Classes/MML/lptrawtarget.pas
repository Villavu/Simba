unit lpTRawTarget;
//Depends: TRawTarget, TTarget, prgb32, integer, boolean, Integer

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TRawTarget(Compiler: TLapeCompiler);

implementation

type
  PRawTarget = ^TRawTarget;

//constructor Create(rgb: prgb32; w,h: integer; CopyData : boolean = false);
procedure TRawTarget_Init(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^ := TRawTarget.Create(Pprgb32(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure GetTargetDimensions(out w, h: integer); override;
procedure TRawTarget_GetTargetDimensions(out w, h: integer); override(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.GetTargetDimensions(out w, h: integer); override();
end;

//procedure GetTargetPosition(out left, top: integer); override;
procedure TRawTarget_GetTargetPosition(out left, top: integer); override(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.GetTargetPosition(out left, top: integer); override();
end;

//function ReturnData(xs, ys, width, height: Integer): TRetData; override;
procedure TRawTarget_ReturnData(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRetData; override(Result)^ := PRawTarget(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//function ImageSetClientArea(x1, y1, x2, y2: integer): boolean; override;
procedure TRawTarget_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean; override(Result)^ := PRawTarget(Params^[0])^.ImageSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure ImageResetClientArea; override;
procedure TRawTarget_ImageResetClientArea; override(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.ImageResetClientArea; override();
end;

//Read: rgb: prgb32;
procedure TRawTarget_rgb_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pprgb32(Result)^ := PRawTarget(Params^[0])^.rgb;
end;

//Write: rgb: prgb32;
procedure TRawTarget_rgb_Write(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.rgb := Pprgb32(Params^[1])^;
end;

//Read: freedata : boolean;
procedure TRawTarget_freedata_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PRawTarget(Params^[0])^.freedata;
end;

//Write: freedata : boolean;
procedure TRawTarget_freedata_Write(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.freedata := Pboolean(Params^[1])^;
end;

//Read: w,h: integer;
procedure TRawTarget_w,h_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PRawTarget(Params^[0])^.w,h;
end;

//Write: w,h: integer;
procedure TRawTarget_w,h_Write(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.w,h := Pinteger(Params^[1])^;
end;

//procedure Free();
procedure TRawTarget_Free(const Params: PParamArray); lape_extdecl
begin
  PRawTarget(Params^[0])^.Free();
end;

procedure Register_TRawTarget(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TRawTarget', 'TTarget');

    addGlobalFunc('procedure TRawTarget.Init(rgb: prgb32; w,h: integer; CopyData : boolean = false);', @TRawTarget_Init);
    addGlobalFunc('procedure TRawTarget.GetTargetDimensions(out w, h: integer); override(); constref;', @TRawTarget_GetTargetDimensions(out w, h: integer); override);
    addGlobalFunc('procedure TRawTarget.GetTargetPosition(out left, top: integer); override(); constref;', @TRawTarget_GetTargetPosition(out left, top: integer); override);
    addGlobalFunc('function TRawTarget.ReturnData(xs, ys, width, height: Integer): TRetData; constref; override;', @TRawTarget_ReturnData);
    addGlobalFunc('function TRawTarget.ImageSetClientArea(x1, y1, x2, y2: integer): boolean; constref; override;', @TRawTarget_ImageSetClientArea);
    addGlobalFunc('procedure TRawTarget.ImageResetClientArea; override(); constref;', @TRawTarget_ImageResetClientArea; override);
    addClassVar(Compiler, 'TRawTarget', 'rgb', 'prgb32', @TRawTarget_rgb_Read, @TRawTarget_rgb_Write);
    addClassVar(Compiler, 'TRawTarget', 'freedata', 'boolean', @TRawTarget_freedata_Read, @TRawTarget_freedata_Write);
    addClassVar(Compiler, 'TRawTarget', 'w,h', 'integer', @TRawTarget_w,h_Read, @TRawTarget_w,h_Write);
    addGlobalFunc('procedure TRawTarget.Free();', @TRawTarget_Free);
  end;
end;

end.

