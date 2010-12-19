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

    PSCompile.inc for the Mufasa Macro Library
}

AddConstantN('AppPath','string').SetString(AppPath);
AddConstantN('ScriptPath','string').SetString(ScriptPath);
AddConstantN('IncludePath','string').SetString(IncludePath);
AddConstantN('PluginPath','string').SetString(PluginPath);
AddConstantN('FontPath','string').SetString(FontPath);
AddConstantN('MaxLongInt','integer').SetInt(maxLongint);
AddConstantN('MaxInt','integer').SetInt(maxLongint);
AddTypeS('TReplaceFlag', '(rfReplaceAll, rfIgnoreCase)');
AddTypeS('TReplaceFlags','set of TReplaceFlag');
AddTypeS('StrExtr','(Numbers, Letters, Others);');
AddTypeS('TDateTime','Double');
AddTypeS('TIntegerArray', 'Array of LongInt');
AddTypeS('TByteArray','Array of byte');
AddTypeS('TExtendedArray','Array of extended');
AddTypeS('TBoolArray', 'Array of Boolean');
AddTypes('TBox', 'record X1,Y1,X2,Y2 : Integer; end;');
AddTypes('TProc', 'record Title: string; Handle, Pid, Width, Height: integer; end;');
AddTypes('TProcArr', 'array of TProc;');
AddTypeS('TBoxArray','Array of TBox');
AddTypeS('TPointArray','Array of TPoint');
AddTypeS('T2DPointArray','Array of TPointArray');
AddTypeS('TPointArrayArray','Array of TPointArray');
AddTypeS('TBmpMirrorStyle','(MirrorWidth,MirrorHeight,MirrorLine)');
AddTypeS('TMask','record  White, Black : TPointArray; WhiteHi,BlackHi : integer; W,H : integer;end;');
addtypeS('PPoint','record R,T : extended; end;');
AddTypeS('TTarget_Exported','record int1,int2,int3,int4,int5,int6,int7,int8,int9,int10,int11,int12,int13,int14,int15,int16:integer; end;');

AddTypes('TSDTMPointDef', 'record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
AddTypes('TSDTMPointDefArray', 'Array Of TSDTMPointDef;');
AddTypes('TSDTM','record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end;');
AddTypes('TMDTMPoint','record x,y,c,t,asz : integer; bp : boolean; end;');
AddTypes('TMDTMPointArray','array of TMDTMPoint;');

AddTypeS('T2DExtendedArray', 'array of array of extended;');
AddTypeS('T3DExtendedArray','array of array of array of extended;');
AddTypeS('T2DIntegerArray','array of array of integer;');
AddTypeS('TStringArray','Array of string;');
AddTypeS('TMousePress', '(mouse_Down, mouse_Up);');
AddTypeS('Pointer', 'Integer');

AddTypeS('TSP_Property','(SP_WriteTimeStamp,SP_OnTerminate)');

AddConstantN('mouse_Right','integer').SetInt(ps_mouse_right); //0
AddConstantN('mouse_Left','integer').SetInt(ps_mouse_left);//1
AddConstantN('mouse_Middle','integer').SetInt(ps_mouse_middle);//2
