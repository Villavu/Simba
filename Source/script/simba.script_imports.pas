{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  --------------------------------------------------------------------------

  All the script import units so their initialization sections can be called.
  The `uses` order is also the order things will be imported into the compiler,
  so remember order does matter else script compiling errors!
}
unit simba.script_imports;

{$i simba.inc}

interface

implementation

uses
  simba.import_system, simba.import_colormath, simba.import_colors,
  simba.import_matrix,
  simba.import_quad, simba.import_box, simba.import_boxarray, simba.import_point,

  // LCL
  simba.import_lcl_system, simba.import_lcl_graphics, simba.import_lcl_controls,
  simba.import_lcl_form, simba.import_lcl_stdctrls, simba.import_lcl_extctrls,
  simba.import_lcl_comctrls, simba.import_lcl_misc,

  // Simba classes
  simba.import_class_bitmap, simba.import_class_dtm, simba.import_matchtemplate,
  simba.import_class_xml, simba.import_class_json,
  simba.import_class_imagebox, simba.import_class_shapebox,

  // Simba
  simba.import_timing, simba.import_tpa, simba.import_atpa,
  simba.import_hash, simba.import_compress, simba.import_windowhandle,
  simba.import_dialogs, simba.import_file, simba.import_internal,
  simba.import_target, simba.import_finder, simba.import_math,
  simba.import_other, simba.import_input, simba.import_process,
  simba.import_script,  simba.import_slacktree, simba.import_string,
  simba.import_variant, simba.import_simba, simba.import_random,
  simba.import_debugimage, simba.import_internet;

end.

