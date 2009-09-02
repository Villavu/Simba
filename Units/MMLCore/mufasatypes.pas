unit MufasaTypes;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils;

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TTargetWindowMode = (w_BMP, w_Window, w_HDC, w_ArrayPtr, w_XWindow);
  TClickType = (mouse_Left, mouse_Right, mouse_Middle);
  TMousePress = (mouse_Down, mouse_Up);



implementation

end.

