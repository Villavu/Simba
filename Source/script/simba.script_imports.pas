{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_imports;

{$i simba.inc}

interface

uses
  simba.script;

procedure AddSimbaInternalMethods(Script: TSimbaScript);
procedure AddSimbaImports(Script: TSimbaScript);

implementation

uses
  lpffi,
  // Lape Internal methods
  simba.script_compiler_sleepuntil,
  simba.script_compiler_rtti,
  simba.script_compiler_imagefromstring,
  simba.script_genericmap,
  simba.script_genericstringmap,
  simba.script_genericheap,
  simba.script_genericarraybuffer,

  // Simba
  simba.import_base, simba.import_colormath,simba.import_matrix, simba.import_windowhandle,
  simba.import_quad, simba.import_box, simba.import_point,
  simba.import_circle, simba.import_datetime,
  simba.import_encoding, simba.import_file, simba.import_process,
  simba.import_target, simba.import_math, simba.import_misc, simba.import_slacktree, simba.import_string,
  simba.import_random, simba.import_debugimage, simba.import_web, simba.import_threading,
  simba.import_async,

  // Simba classes
  simba.import_image, simba.import_externalcanvas, simba.import_dtm, simba.import_matchtemplate,
  simba.import_json, simba.import_imagebox, simba.import_shapebox,

  // LCL
  simba.import_lcl_system, simba.import_lcl_graphics, simba.import_lcl_controls,
  simba.import_lcl_form, simba.import_lcl_stdctrls, simba.import_lcl_extctrls,
  simba.import_lcl_comctrls, simba.import_lcl_misc;

procedure AddSimbaInternalMethods(Script: TSimbaScript);
begin
  InitializeImageFromString(Script.Compiler);
  InitializeSleepUntil(Script.Compiler);
  InitializeFFI(Script.Compiler);
  InitializeRTTI(Script.Compiler);
  InitializeMap(Script.Compiler);
  InitializeStringMap(Script.Compiler);
  InitializeHeap(Script.Compiler);
  InitializeArrayBuffer(Script.Compiler);
end;

procedure AddSimbaImports(Script: TSimbaScript);
begin
  ImportBase(Script);
  ImportColorMath(Script);
  ImportMatrix(Script);
  ImportWindowHandle(Script);
  ImportQuad(Script);
  ImportCircle(Script);
  ImportBox(Script);
  ImportPoint(Script);

  ImportLCLSystem(Script);
  ImportLCLGraphics(Script);
  ImportLCLControls(Script);
  ImportLCLForm(Script);
  ImportLCLStdCtrls(Script);
  ImportLCLExtCtrls(Script);
  ImportLCLComCtrls(Script);
  ImportLCLMisc(Script);

  ImportDTM(Script);
  ImportSimbaImage(Script);
  ImportExternalCanvas(Script);
  ImportMatchTemplate(Script);
  ImportJSON(Script);

  ImportDateTime(Script);
  ImportEncoding(Script);
  ImportFile(Script);
  ImportProcess(Script);
  ImportTarget(Script);
  ImportMath(Script);
  ImportSlackTree(Script);
  ImportString(Script);
  ImportRandom(Script);
  ImportDebugImage(Script);
  ImportWeb(Script);
  ImportMisc(Script);
  ImportThreading(Script);
  ImportASync(Script);

  ImportSimbaImageBox(Script);
  ImportSimbaShapeBox(Script);
end;

end.

