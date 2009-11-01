{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Type declarations for the Mufasa Macro Library
}

unit MufasaTypes;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,plugins;
const
  DS = DirectorySeparator;

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TRetData = record
    Ptr : PRGB32;
    IncPtrWith : integer;
    RowLen : integer;
  end;
  TBmpMirrorStyle = (MirrorWidth,MirrorHeight,MirrorLine); //LineMirror is in line x=y;
  TTargetWindowMode = (w_BMP, w_Window, w_HDC, w_ArrayPtr, w_XWindow);
  TClickType = (mouse_Left, mouse_Right, mouse_Middle);
  TMousePress = (mouse_Down, mouse_Up);

  TStringArray = array of String;
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;
  TVariantArray = Array of Variant;
  TIntegerArray = Array of Integer;
  T2DIntArray = array of TIntegerArray;
  T2DIntegerArray = T2DIntArray;
  TBoolArray = array of boolean;
  T2DBoolArray = Array of TBoolArray;
  TExtendedArray = Array of Extended;
  T2DExtendedArray = Array of Array of Extended;
  { Mask Types }
  TMask = record
    White, Black : TPointArray;
    WhiteHi,BlackHi : integer;
    W,H : integer;
  end;

  { DTM Types }
  {
    Possibly add .name too?
    Then one could give DTM names, which would be easy for debugging.
  }
  TBox = record
    x1, y1, x2, y2: Integer;
  end;

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

