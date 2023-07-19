{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_colors;

{$i simba.inc}

interface

implementation

uses
  Classes, SysUtils, Graphics,
  simba.script_compiler;

procedure ImportColors(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'System';

    addGlobalFunc(
      'function TColor.R: Byte;', [
      'type TRGB = packed record B,G,R,A: Byte; end;',
      'begin',
      '  Result := TRGB(Self).R;',
      'end;'
    ]);
    addGlobalFunc(
      'function TColor.G: Byte;', [
      'type TRGB = packed record B,G,R,A: Byte; end;',
      'begin',
      '  Result := TRGB(Self).G;',
      'end;'
    ]);
    addGlobalFunc(
      'function TColor.B: Byte;', [
      'type TRGB = packed record B,G,R,A: Byte; end;',
      'begin',
      '  Result := TRGB(Self).B;',
      'end;'
    ]);

    addGlobalType([
      'record',
      '  const ALICEBLUE             = TColor($FFF8F0);',
      '  const ANTIQUEWHITE          = TColor($D7EBFA);',
      '  const AQUA                  = TColor($FFFF00);',
      '  const AQUAMARINE            = TColor($D4FF7F);',
      '  const AZURE                 = TColor($FFFFF0);',
      '  const BEIGE                 = TColor($DCF5F5);',
      '  const BISQUE                = TColor($C4E4FF);',
      '  const BLACK                 = TColor($000000);',
      '  const BLANCHEDALMOND        = TColor($CDEBFF);',
      '  const BLUE                  = TColor($FF0000);',
      '  const BLUEVIOLET            = TColor($E22B8A);',
      '  const BROWN                 = TColor($2A2AA5);',
      '  const BURLYWOOD             = TColor($87B8DE);',
      '  const CADET_BLUE            = TColor($A09E5F);',
      '  const CHARTREUSE            = TColor($00FF7F);',
      '  const CHOCOLATE             = TColor($1E69D2);',
      '  const CORAL                 = TColor($507FFF);',
      '  const CORNFLOWER_BLUE       = TColor($ED9564);',
      '  const CORNSILK              = TColor($DCF8FF);',
      '  const CRIMSON               = TColor($3C14DC);',
      '  const CYAN                  = TColor($FFFF00);',
      '  const DARK_BLUE             = TColor($8B0000);',
      '  const DARK_CYAN             = TColor($8B8B00);',
      '  const DARK_GOLDENROD        = TColor($0B86B8);',
      '  const DARK_GRAY             = TColor($A9A9A9);',
      '  const DARK_GREEN            = TColor($006400);',
      '  const DARK_GREY             = TColor($A9A9A9);',
      '  const DARK_KHAKI            = TColor($6BB7BD);',
      '  const DARK_MAGENTA          = TColor($8B008B);',
      '  const DARK_OLIVEGREEN       = TColor($2F6B55);',
      '  const DARK_ORANGE           = TColor($008CFF);',
      '  const DARK_ORCHID           = TColor($CC3299);',
      '  const DARK_RED              = TColor($00008B);',
      '  const DARK_SALMON           = TColor($7A96E9);',
      '  const DARK_SEAGREEN         = TColor($8FBC8F);',
      '  const DARK_SLATEBLUE        = TColor($8B3D48);',
      '  const DARK_SLATEGRAY        = TColor($4F4F2F);',
      '  const DARK_SLATEGREY        = TColor($4F4F2F);',
      '  const DARK_TURQUOISE        = TColor($D1CE00);',
      '  const DARK_VIOLET           = TColor($D30094);',
      '  const DEEPPINK              = TColor($9314FF);',
      '  const DEEPSKYBLUE           = TColor($FFBF00);',
      '  const DIMGRAY               = TColor($696969);',
      '  const DIMGREY               = TColor($696969);',
      '  const DODGERBLUE            = TColor($FF901E);',
      '  const FIREBRICK             = TColor($2222B2);',
      '  const FLORALWHITE           = TColor($F0FAFF);',
      '  const FORESTGREEN           = TColor($228B22);',
      '  const FUCHSIA               = TColor($FF00FF);',
      '  const GAINSBORO             = TColor($DCDCDC);',
      '  const GHOSTWHITE            = TColor($FFF8F8);',
      '  const GOLD                  = TColor($00D7FF);',
      '  const GOLDENROD             = TColor($20A5DA);',
      '  const GRAY                  = TColor($808080);',
      '  const GREEN                 = TColor($008000);',
      '  const GREENYELLOW           = TColor($2FFFAD);',
      '  const GREY                  = TColor($808080);',
      '  const HONEYDEW              = TColor($F0FFF0);',
      '  const HOTPINK               = TColor($B469FF);',
      '  const INDIANRED             = TColor($5C5CCD);',
      '  const INDIGO                = TColor($82004B);',
      '  const IVORY                 = TColor($F0FFFF);',
      '  const KHAKI                 = TColor($8CE6F0);',
      '  const LAVENDER              = TColor($FAE6E6);',
      '  const LAVENDERBLUSH         = TColor($F5F0FF);',
      '  const LAWNGREEN             = TColor($00FC7C);',
      '  const LEMONCHIFFON          = TColor($CDFAFF);',
      '  const LIGHT_BLUE            = TColor($E6D8AD);',
      '  const LIGHT_CORAL           = TColor($8080F0);',
      '  const LIGHT_CYAN            = TColor($FFFFE0);',
      '  const LIGHT_GOLDENRODYELLOW = TColor($D2FAFA);',
      '  const LIGHT_GRAY            = TColor($D3D3D3);',
      '  const LIGHT_GREEN           = TColor($90EE90);',
      '  const LIGHT_GREY            = TColor($D3D3D3);',
      '  const LIGHT_PINK            = TColor($C1B6FF);',
      '  const LIGHT_SALMON          = TColor($7AA0FF);',
      '  const LIGHT_SEAGREEN        = TColor($AAB220);',
      '  const LIGHT_SKYBLUE         = TColor($FACE87);',
      '  const LIGHT_SLATEGRAY       = TColor($998877);',
      '  const LIGHT_SLATEGREY       = TColor($998877);',
      '  const LIGHT_STEELBLUE       = TColor($DEC4B0);',
      '  const LIGHT_YELLOW          = TColor($E0FFFF);',
      '  const LIME                  = TColor($00FF00);',
      '  const LIMEGREEN             = TColor($32CD32);',
      '  const LINEN                 = TColor($E6F0FA);',
      '  const MAGENTA               = TColor($FF00FF);',
      '  const MAROON                = TColor($000080);',
      '  const MEDIUM_AQUAMARINE     = TColor($AACD66);',
      '  const MEDIUM_BLUE           = TColor($CD0000);',
      '  const MEDIUM_ORCHID         = TColor($D355BA);',
      '  const MEDIUM_PURPLE         = TColor($DB7093);',
      '  const MEDIUM_SEAGREEN       = TColor($71B33C);',
      '  const MEDIUM_SLATEBLUE      = TColor($EE687B);',
      '  const MEDIUM_SPRINGGREEN    = TColor($9AFA00);',
      '  const MEDIUM_TURQUOISE      = TColor($CCD148);',
      '  const MEDIUM_VIOLETRED      = TColor($8515C7);',
      '  const MIDNIGHTBLUE          = TColor($701919);',
      '  const MINTCREAM             = TColor($FAFFF5);',
      '  const MISTYROSE             = TColor($E1E4FF);',
      '  const MOCCASIN              = TColor($B5E4FF);',
      '  const NAVAJOWHITE           = TColor($ADDEFF);',
      '  const NAVY                  = TColor($800000);',
      '  const OLDLACE               = TColor($E6F5FD);',
      '  const OLIVE                 = TColor($008080);',
      '  const OLIVEDRAB             = TColor($238E6B);',
      '  const ORANGE                = TColor($00A5FF);',
      '  const ORANGERED             = TColor($0045FF);',
      '  const ORCHID                = TColor($D670DA);',
      '  const PALE_GOLDENROD        = TColor($AAE8EE);',
      '  const PALE_GREEN            = TColor($98FB98);',
      '  const PALE_TURQUOISE        = TColor($EEEEAF);',
      '  const PALE_VIOLETRED        = TColor($9370DB);',
      '  const PAPAYAWHIP            = TColor($D5EFFF);',
      '  const PEACHPUFF             = TColor($B9DAFF);',
      '  const PERU                  = TColor($3F85CD);',
      '  const PINK                  = TColor($CBC0FF);',
      '  const PLUM                  = TColor($DDA0DD);',
      '  const POWDERBLUE            = TColor($E6E0B0);',
      '  const PURPLE                = TColor($800080);',
      '  const REBECCAPURPLE         = TColor($993366);',
      '  const RED                   = TColor($0000FF);',
      '  const ROSYBROWN             = TColor($8F8FBC);',
      '  const ROYALBLUE             = TColor($E16941);',
      '  const SADDLEBROWN           = TColor($13458B);',
      '  const SALMON                = TColor($7280FA);',
      '  const SANDYBROWN            = TColor($60A4F4);',
      '  const SEAGREEN              = TColor($578B2E);',
      '  const SEASHELL              = TColor($EEF5FF);',
      '  const SIENNA                = TColor($2D52A0);',
      '  const SILVER                = TColor($C0C0C0);',
      '  const SKYBLUE               = TColor($EBCE87);',
      '  const SLATEBLUE             = TColor($CD5A6A);',
      '  const SLATEGRAY             = TColor($908070);',
      '  const SLATEGREY             = TColor($908070);',
      '  const SNOW                  = TColor($FAFAFF);',
      '  const SPRINGGREEN           = TColor($7FFF00);',
      '  const STEELBLUE             = TColor($B48246);',
      '  const TAN                   = TColor($8CB4D2);',
      '  const TEAL                  = TColor($808000);',
      '  const THISTLE               = TColor($D8BFD8);',
      '  const TOMATO                = TColor($4763FF);',
      '  const TURQUOISE             = TColor($D0E040);',
      '  const VIOLET                = TColor($EE82EE);',
      '  const WHEAT                 = TColor($B3DEF5);',
      '  const WHITE                 = TColor($FFFFFF);',
      '  const WHITESMOKE            = TColor($F5F5F5);',
      '  const YELLOW                = TColor($00FFFF);',
      '  const YELLOWGREEN           = TColor($32CD9A);',
      'end;'],
      'Colors');

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportColors);

end.

