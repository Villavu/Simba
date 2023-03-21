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
      'class const',
      '  ALICEBLUE             = TColor($FFF8F0);',
      '  ANTIQUEWHITE          = TColor($D7EBFA);',
      '  AQUA                  = TColor($FFFF00);',
      '  AQUAMARINE            = TColor($D4FF7F);',
      '  AZURE                 = TColor($FFFFF0);',
      '  BEIGE                 = TColor($DCF5F5);',
      '  BISQUE                = TColor($C4E4FF);',
      '  BLACK                 = TColor($000000);',
      '  BLANCHEDALMOND        = TColor($CDEBFF);',
      '  BLUE                  = TColor($FF0000);',
      '  BLUEVIOLET            = TColor($E22B8A);',
      '  BROWN                 = TColor($2A2AA5);',
      '  BURLYWOOD             = TColor($87B8DE);',
      '  CADET_BLUE            = TColor($A09E5F);',
      '  CHARTREUSE            = TColor($00FF7F);',
      '  CHOCOLATE             = TColor($1E69D2);',
      '  CORAL                 = TColor($507FFF);',
      '  CORNFLOWER_BLUE       = TColor($ED9564);',
      '  CORNSILK              = TColor($DCF8FF);',
      '  CRIMSON               = TColor($3C14DC);',
      '  CYAN                  = TColor($FFFF00);',
      '  DARK_BLUE             = TColor($8B0000);',
      '  DARK_CYAN             = TColor($8B8B00);',
      '  DARK_GOLDENROD        = TColor($0B86B8);',
      '  DARK_GRAY             = TColor($A9A9A9);',
      '  DARK_GREEN            = TColor($006400);',
      '  DARK_GREY             = TColor($A9A9A9);',
      '  DARK_KHAKI            = TColor($6BB7BD);',
      '  DARK_MAGENTA          = TColor($8B008B);',
      '  DARK_OLIVEGREEN       = TColor($2F6B55);',
      '  DARK_ORANGE           = TColor($008CFF);',
      '  DARK_ORCHID           = TColor($CC3299);',
      '  DARK_RED              = TColor($00008B);',
      '  DARK_SALMON           = TColor($7A96E9);',
      '  DARK_SEAGREEN         = TColor($8FBC8F);',
      '  DARK_SLATEBLUE        = TColor($8B3D48);',
      '  DARK_SLATEGRAY        = TColor($4F4F2F);',
      '  DARK_SLATEGREY        = TColor($4F4F2F);',
      '  DARK_TURQUOISE        = TColor($D1CE00);',
      '  DARK_VIOLET           = TColor($D30094);',
      '  DEEPPINK              = TColor($9314FF);',
      '  DEEPSKYBLUE           = TColor($FFBF00);',
      '  DIMGRAY               = TColor($696969);',
      '  DIMGREY               = TColor($696969);',
      '  DODGERBLUE            = TColor($FF901E);',
      '  FIREBRICK             = TColor($2222B2);',
      '  FLORALWHITE           = TColor($F0FAFF);',
      '  FORESTGREEN           = TColor($228B22);',
      '  FUCHSIA               = TColor($FF00FF);',
      '  GAINSBORO             = TColor($DCDCDC);',
      '  GHOSTWHITE            = TColor($FFF8F8);',
      '  GOLD                  = TColor($00D7FF);',
      '  GOLDENROD             = TColor($20A5DA);',
      '  GRAY                  = TColor($808080);',
      '  GREEN                 = TColor($008000);',
      '  GREENYELLOW           = TColor($2FFFAD);',
      '  GREY                  = TColor($808080);',
      '  HONEYDEW              = TColor($F0FFF0);',
      '  HOTPINK               = TColor($B469FF);',
      '  INDIANRED             = TColor($5C5CCD);',
      '  INDIGO                = TColor($82004B);',
      '  IVORY                 = TColor($F0FFFF);',
      '  KHAKI                 = TColor($8CE6F0);',
      '  LAVENDER              = TColor($FAE6E6);',
      '  LAVENDERBLUSH         = TColor($F5F0FF);',
      '  LAWNGREEN             = TColor($00FC7C);',
      '  LEMONCHIFFON          = TColor($CDFAFF);',
      '  LIGHT_BLUE            = TColor($E6D8AD);',
      '  LIGHT_CORAL           = TColor($8080F0);',
      '  LIGHT_CYAN            = TColor($FFFFE0);',
      '  LIGHT_GOLDENRODYELLOW = TColor($D2FAFA);',
      '  LIGHT_GRAY            = TColor($D3D3D3);',
      '  LIGHT_GREEN           = TColor($90EE90);',
      '  LIGHT_GREY            = TColor($D3D3D3);',
      '  LIGHT_PINK            = TColor($C1B6FF);',
      '  LIGHT_SALMON          = TColor($7AA0FF);',
      '  LIGHT_SEAGREEN        = TColor($AAB220);',
      '  LIGHT_SKYBLUE         = TColor($FACE87);',
      '  LIGHT_SLATEGRAY       = TColor($998877);',
      '  LIGHT_SLATEGREY       = TColor($998877);',
      '  LIGHT_STEELBLUE       = TColor($DEC4B0);',
      '  LIGHT_YELLOW          = TColor($E0FFFF);',
      '  LIME                  = TColor($00FF00);',
      '  LIMEGREEN             = TColor($32CD32);',
      '  LINEN                 = TColor($E6F0FA);',
      '  MAGENTA               = TColor($FF00FF);',
      '  MAROON                = TColor($000080);',
      '  MEDIUM_AQUAMARINE     = TColor($AACD66);',
      '  MEDIUM_BLUE           = TColor($CD0000);',
      '  MEDIUM_ORCHID         = TColor($D355BA);',
      '  MEDIUM_PURPLE         = TColor($DB7093);',
      '  MEDIUM_SEAGREEN       = TColor($71B33C);',
      '  MEDIUM_SLATEBLUE      = TColor($EE687B);',
      '  MEDIUM_SPRINGGREEN    = TColor($9AFA00);',
      '  MEDIUM_TURQUOISE      = TColor($CCD148);',
      '  MEDIUM_VIOLETRED      = TColor($8515C7);',
      '  MIDNIGHTBLUE          = TColor($701919);',
      '  MINTCREAM             = TColor($FAFFF5);',
      '  MISTYROSE             = TColor($E1E4FF);',
      '  MOCCASIN              = TColor($B5E4FF);',
      '  NAVAJOWHITE           = TColor($ADDEFF);',
      '  NAVY                  = TColor($800000);',
      '  OLDLACE               = TColor($E6F5FD);',
      '  OLIVE                 = TColor($008080);',
      '  OLIVEDRAB             = TColor($238E6B);',
      '  ORANGE                = TColor($00A5FF);',
      '  ORANGERED             = TColor($0045FF);',
      '  ORCHID                = TColor($D670DA);',
      '  PALE_GOLDENROD        = TColor($AAE8EE);',
      '  PALE_GREEN            = TColor($98FB98);',
      '  PALE_TURQUOISE        = TColor($EEEEAF);',
      '  PALE_VIOLETRED        = TColor($9370DB);',
      '  PAPAYAWHIP            = TColor($D5EFFF);',
      '  PEACHPUFF             = TColor($B9DAFF);',
      '  PERU                  = TColor($3F85CD);',
      '  PINK                  = TColor($CBC0FF);',
      '  PLUM                  = TColor($DDA0DD);',
      '  POWDERBLUE            = TColor($E6E0B0);',
      '  PURPLE                = TColor($800080);',
      '  REBECCAPURPLE         = TColor($993366);',
      '  RED                   = TColor($0000FF);',
      '  ROSYBROWN             = TColor($8F8FBC);',
      '  ROYALBLUE             = TColor($E16941);',
      '  SADDLEBROWN           = TColor($13458B);',
      '  SALMON                = TColor($7280FA);',
      '  SANDYBROWN            = TColor($60A4F4);',
      '  SEAGREEN              = TColor($578B2E);',
      '  SEASHELL              = TColor($EEF5FF);',
      '  SIENNA                = TColor($2D52A0);',
      '  SILVER                = TColor($C0C0C0);',
      '  SKYBLUE               = TColor($EBCE87);',
      '  SLATEBLUE             = TColor($CD5A6A);',
      '  SLATEGRAY             = TColor($908070);',
      '  SLATEGREY             = TColor($908070);',
      '  SNOW                  = TColor($FAFAFF);',
      '  SPRINGGREEN           = TColor($7FFF00);',
      '  STEELBLUE             = TColor($B48246);',
      '  TAN                   = TColor($8CB4D2);',
      '  TEAL                  = TColor($808000);',
      '  THISTLE               = TColor($D8BFD8);',
      '  TOMATO                = TColor($4763FF);',
      '  TURQUOISE             = TColor($D0E040);',
      '  VIOLET                = TColor($EE82EE);',
      '  WHEAT                 = TColor($B3DEF5);',
      '  WHITE                 = TColor($FFFFFF);',
      '  WHITESMOKE            = TColor($F5F5F5);',
      '  YELLOW                = TColor($00FFFF);',
      '  YELLOWGREEN           = TColor($32CD9A);',
      'end;'],
      'Colors');

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportColors);

end.

