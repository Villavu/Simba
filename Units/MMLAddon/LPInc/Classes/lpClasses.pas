unit lpClasses;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler;

procedure RegisterLCLClasses(Compiler: TLapeCompiler);
procedure RegisterMMLClasses(Compiler: TLapeCompiler);

implementation

uses
  lpTObject,

  lplclsystem,
  lplclgraphics,
  lplclforms,
  lplclcontrols,
  lplclstdctrls,
  lplclextctrls,
  lplclcomctrls,
  lplcldialogs,
  lplclmenus,
  lplclspin,
  lplclprocess,
  lplclregexpr,

  lpTMDTM, lpTMDTMS,

  lpTMufasaBitmap, lpTMBitmaps,

  lpTMFiles,

  lpTMFinder,
  
  lpTMFont, lpTMFonts,

  lpTMOCR,

  lpTTarget,

  lpTIOManager_Abstract

  {$IFDEF WINDOWS}, lpTIOManager_Windows {$ENDIF}
  {$IFDEF LINUX}, lpTIOManager_Linux {$ENDIF}

  , lpTClient,

  lpTMMLSettingsSandbox,

  lpTMMLTimer;

procedure RegisterLCLClasses(Compiler: TLapeCompiler);
begin
  Register_TObject(Compiler);

  RegisterLCLSystem(Compiler);
  RegisterLCLGraphics(Compiler);
  RegisterLCLControls(Compiler);
  RegisterLCLForms(Compiler);
  RegisterLCLStdCtrls(Compiler);
  RegisterLCLExtCtrls(Compiler);
  RegisterLCLComCtrls(Compiler);
  RegisterLCLDialogs(Compiler);
  RegisterLCLMenus(Compiler);
  RegisterLCLSpinCtrls(Compiler);
  RegisterLCLProcess(Compiler);
  RegisterLCLTRegExpr(Compiler);
end;

procedure RegisterMMLClasses(Compiler: TLapeCompiler);
begin
  { MML DTM }
  Register_TMDTM(Compiler);
  Register_TMDTMS(Compiler);

  { MML Bitmap }
  Register_TMufasaBitmap(Compiler);
  Register_TMBitmaps(Compiler);

  { MML File }
  Register_TMFiles(Compiler);
  
  { MML Finder }
  Register_TMFinder(Compiler);

  { MML Font }
  Register_TMFont(Compiler);
  Register_TMFonts(Compiler);

  { MML OCR }
  Register_TMOCR(Compiler);

  { MML Target }
  Register_TTarget(Compiler);

  { MML IOManager }
  Register_TIOManager_Abstract(Compiler);
  Register_TIOManager(Compiler);

  { MML Client }
  Register_TClient(Compiler);

  Register_TMMLSettingsSandbox(Compiler);

  Register_TMMLTimer(Compiler);
end;

end.
