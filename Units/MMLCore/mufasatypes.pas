unit MufasaTypes;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils;
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

implementation

end.

