{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_imports;

{$i simba.inc}

interface

uses
  simba.script_compiler;

procedure AddSimbaImports(Compiler: TSimbaScript_Compiler);

implementation

uses
  // Simba
  simba.import_system, simba.import_colormath,
  simba.import_matrix, simba.import_windowhandle,
  simba.import_quad, simba.import_box, simba.import_boxarray, simba.import_point,
  simba.import_circle,

  // LCL
  simba.import_lcl_system, simba.import_lcl_graphics, simba.import_lcl_controls,
  simba.import_lcl_form, simba.import_lcl_stdctrls, simba.import_lcl_extctrls,
  simba.import_lcl_comctrls, simba.import_lcl_misc,

  // Simba classes
  simba.import_class_image, simba.import_externalimage, simba.import_class_dtm, simba.import_matchtemplate,
  simba.import_class_json, simba.import_class_imagebox, simba.import_class_shapebox,

  // Simba
  simba.import_timing, simba.import_tpa, simba.import_atpa, simba.import_encoding,
  simba.import_file, simba.import_process,
  simba.import_internal, simba.import_target, simba.import_finder,
  simba.import_math, simba.import_misc, simba.import_input,
  simba.import_script,  simba.import_slacktree, simba.import_string,
  simba.import_variant, simba.import_random,
  simba.import_debugimage, simba.import_web, simba.import_dialogs, simba.import_threading,
  simba.import_pointbuffer;

procedure AddSimbaImports(Compiler: TSimbaScript_Compiler);
begin
  ImportSystem(Compiler);
  ImportColorMath(Compiler);
  ImportMatrix(Compiler);
  ImportWindowHandle(Compiler);
  ImportQuad(Compiler);
  ImportCircle(Compiler);
  ImportBox(Compiler);
  ImportBoxArray(Compiler);
  ImportPoint(Compiler);

  ImportLCLSystem(Compiler);
  ImportLCLGraphics(Compiler);
  ImportLCLControls(Compiler);
  ImportLCLForm(Compiler);
  ImportLCLStdCtrls(Compiler);
  ImportLCLExtCtrls(Compiler);
  ImportLCLComCtrls(Compiler);
  ImportLCLMisc(Compiler);

  ImportSimbaImage(Compiler);
  ImportSimbaExternalImage(Compiler);
  ImportDTM(Compiler);
  ImportMatchTemplate(Compiler);
  ImportJSON(Compiler);
  ImportSimbaImageBox(Compiler);
  ImportSimbaShapeBox(Compiler);

  ImportTiming(Compiler);
  ImportTPA(Compiler);
  ImportATPA(Compiler);
  ImportEncoding(Compiler);
  ImportFile(Compiler);
  ImportProcess(Compiler);
  ImportInternal(Compiler);
  ImportTarget(Compiler);
  ImportFinder(Compiler);
  ImportMath(Compiler);
  ImportMisc(Compiler);
  ImportInput(Compiler);
  ImportScript(Compiler);
  ImportSlackTree(Compiler);
  ImportString(Compiler);
  ImportVariant(Compiler);
  ImportRandom(Compiler);
  ImportDebugImage(Compiler);
  ImportWeb(Compiler);
  ImportDialogs(Compiler);
  ImportThreading(Compiler);
  ImportPointBuffer(Compiler);
end;

end.

