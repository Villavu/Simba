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

    lpcompile.inc for the Mufasa Macro Library
}

addGlobalVar(AppPath, 'AppPath');
addGlobalVar(ScriptPath, 'ScriptPath');
addGlobalVar(IncludePath, 'IncludePath');
addGlobalVar(PluginPath, 'PluginPath');
addGlobalVar(FontPath, 'FontPath');
addGlobalVar(maxLongint, 'MaxLongInt');
addGlobalVar(maxLongint, 'MaxInt');

addGlobalVar(ps_mouse_right, 'mouse_Right');//0
addGlobalVar(ps_mouse_left, 'mouse_Left');//1
addGlobalVar(ps_mouse_middle, 'mouse_Middle');//2

addGlobalType('UInt8', 'Byte');
addGlobalType('Int8', 'ShortInt');
addGlobalType('UInt16', 'Word');
addGlobalType('Int16', 'SmallInt');
addGlobalType('UInt32', 'LongWord');
addGlobalType('Int32', 'LongInt');
addGlobalType('UInt64', 'QWord');
addGlobalType('LongInt', 'Integer');

addGlobalType('Integer', 'TColor');
addGlobalType('Double', 'TDateTime');

addGlobalType('(rfReplaceAll, rfIgnoreCase)', 'TReplaceFlag');
addGlobalType('set of TReplaceFlag', 'TReplaceFlags');

addGlobalType('(Numbers, Letters, Others)', 'StrExtr');
addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
addGlobalType('(mouse_Down, mouse_Up)', 'TMousePress');
addGlobalType('(SP_WriteTimeStamp, SP_OnTerminate)', 'TSP_Property');

addGlobalType('array of string', 'TStringArray');
addGlobalType('array of Integer', 'TIntegerArray');
addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
addGlobalType('array of T2DIntegerArray', 'T3DIntegerArray');
addGlobalType('array of byte', 'TByteArray');
addGlobalType('array of extended', 'TExtendedArray');
addGlobalType('array of TExtendedArray', 'T2DExtendedArray');
addGlobalType('array of T2DExtendedArray', 'T3DExtendedArray');
addGlobalType('array of boolean', 'TBoolArray');
addGlobalType('array of variant', 'TVariantArray');

addGlobalType('record X1, Y1, X2, Y2: integer; end', 'TBox');
addGlobalType('array of TBox', 'TBoxArray');

addGlobalType('record X, Y: integer; end', 'TPoint');
addGlobalType('array of TPoint', 'TPointArray');
addGlobalType('array of TPointArray', 'T2DPointArray');
addGlobalType('T2DPointArray', 'TPointArrayArray');

addGlobalType('record White, Black: TPointarray; WhiteHi, BlackHi: integer; W, H: integer; end', 'TMask');

addGlobalType('record R, T: extended; end', 'PPoint');

addGlobalType('record int1,int2,int3,int4,int5,int6,int7,int8,int9,int10,int11,int12,int13,int14,int15,int16: integer; end', 'TTarget_Exported');

addGlobalType('record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end', 'TSDTMPointDef');
addGlobalType('array of TSDTMPointDef', 'TSDTMPointDefArray');
addGlobalType('record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefarray; end', 'TSDTM');

addGlobalType('record x, y, c, t, asz: integer; bp: boolean; end', 'TMDTMPoint');
addGlobalType('array of TMDTMPoint', 'TMDTMPointArray');

addGlobalType('record Title: string; Handle: integer; Pid: integer; Width, Height: integer; end', 'TSysProc');
addGlobalType('array of TSysProc', 'TSysProcArr');
