unit MufasaTypes;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,plugins;
const
  DS = DirectorySeparator;
var
  MainDir : string;
type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TRetData = record
    Ptr : PRGB32;
    IncPtrWith : integer;
  end;
  TBmpMirrorStyle = (MirrorWidth,MirrorHeight,MirrorLine); //LineMirror is in line x=y;
  TTargetWindowMode = (w_BMP, w_Window, w_HDC, w_ArrayPtr, w_XWindow);
  TClickType = (mouse_Left, mouse_Right, mouse_Middle);
  TMousePress = (mouse_Down, mouse_Up);
  TPointArray = array of TPoint;
  TVariantArray = Array of Variant;
  TIntegerArray = Array of Integer;

  T2DExtendedArray = Array of Array of Extended;

  { DTM Types }
  pDTM = record
    p: TPointArray;
    c, t, asz, ash: TIntegerArray;
  end;

  { Other DTM Types }

  TDTMPointDef = record
    x, y, Color, Tolerance, AreaSize, AreaShape: integer;
  end;

  TDTMPointDefArray = Array Of TDTMPointDef;

  TDTM = record
    MainPoint: TDTMPointDef;
    SubPoints: TDTMPointDefArray;
  end;

var
  PluginsGlob : TMPlugins;

implementation

end.

