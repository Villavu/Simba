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

addGlobalVar(True, 'True');
addGlobalVar(False, 'False');
addGlobalVar(AppPath, 'AppPath');
addGlobalVar(ScriptPath, 'ScriptPath');
addGlobalVar(IncludePath, 'IncludePath');
addGlobalVar(PluginPath, 'PluginPath');
addGlobalVar(FontPath, 'FontPath');

addGlobalVar(ps_mouse_right, 'mouse_Right');
addGlobalVar(ps_mouse_left, 'mouse_Left');
addGlobalVar(ps_mouse_middle, 'mouse_Middle');

for I := Fonts.Count - 1 downto 0 do
  addGlobalVar(Fonts[I].Name, Fonts[I].Name); // Why is this done?

for I := 0 to High(VirtualKeys) do
  addGlobalVar(VirtualKeys[I].Key, Format('VK_%S', [VirtualKeys[i].Str]));

addGlobalType(getBaseType(ltString), 'string');
addGlobalType(getBaseType(ltInt32), 'integer');
//addGlobalType(getBaseType(ltBoolean), 'boolean');
//addGlobalType(getBaseType(ltExtended), 'extended');
//addGlobalType(getBaseType(ltUInt8), 'Byte');
//addGlobalType(getBaseType(ltInt8), 'ShortInt');
//addGlobalType(getBaseType(ltUInt16), 'Word');
//addGlobalType(getBaseType(ltInt16), 'SmallInt');
//addGlobalType(getBaseType(ltUInt32), 'LongWord');
//addGlobalType(getBaseType(ltInt32), 'LongInt');

//addGlobalType('array of string', 'TStringArray');
//addGlobalType('integer', 'TColor');
{//addGlobalType('(rfReplaceAll, rfIgnoreCase)', 'TReplaceFlag');
//addGlobalType('set of TReplaceFlag', 'TReplaceFlags');
//addGlobalType('(Numbers, Letters, Others)', 'StrExtr');
addGlobalType('Double', 'TDateTime');
addGlobalType('array of integer', 'TIntegerArray');
addGlobalType('array of byte', 'TByteArray');
addGlobalType('array of extended', 'TExtendedArray');
addGlobalType('array of boolean', 'TBoolArray');
addGlobalType('record X1, Y1, X2, Y2: integer; end', 'TBox');
addGlobalType('array of TBox', 'TBoxArray');
addGlobalType('record X, Y: integer; end', 'TPoint');
addGlobalType('record R, T: extended; end', 'PPoint');
addGlobalType('array of TPoint', 'TPointArray');
addGlobalType('array of TPointArray', 'T2DPointArray');
addGlobalType('T2DPointArray', 'TPointArrayArray');
//addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
addGlobalType('record  White, Black: TPointArray; WhiteHi, BlackHi: integer; W, H: integer; end', 'TMask');
addGlobalType('record int1, int2, int3, int4, int5, int6, int7, int8, int9, int10, int11, int12, int13, int14, int15, int16 :integer; end', 'TTarget_Exported');
addGlobalType('array of TExtendedArray', 'T2DExtendedArray');
addGlobalType('array of T2DExtendedArray', 'T3DExtendedArray');
addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
addGlobalType('record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end', 'TSDTMPointDef');
addGlobalType('array of TSDTMPointDef', 'TSDTMPointDefArray');
addGlobalType('record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end', 'TSDTM');
addGlobalType('record x, y, c, t, asz: integer; bp: boolean; end', 'TMDTMPoint');
addGlobalType('array of TMDTMPoint', 'TMDTMPointArray');
//addGlobalType('(mouse_Down, mouse_Up)', 'TMousePress');
//addGlobalType('(SP_WriteTimeStamp, SP_OnTerminate)', 'TSP_Property');
addGlobalType('record Title: string; Handle: integer; Pid: integer; Width, Height: integer; end', 'TSysProc');
addGlobalType('array of TSysProc', 'TSysProcArr');}
