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

x.AddConstantN('AppPath','string').SetString(Self.AppPath);
x.AddConstantN('ScriptPath','string').SetString(Self.ScriptPath);
x.AddConstantN('IncludePath','string').SetString(Self.IncludePath);
x.AddConstantN('PluginPath','string').SetString(Self.PluginPath);
x.AddConstantN('FontPath','string').SetString(Self.FontPath);
x.AddConstantN('MaxLongInt','integer').SetInt(maxLongint);
x.AddConstantN('MaxInt','integer').SetInt(maxLongint);
x.AddTypeS('TReplaceFlag', '(rfReplaceAll, rfIgnoreCase)');
x.AddTypeS('TReplaceFlags','set of TReplaceFlag');
x.AddTypeS('StrExtr','(Numbers, Letters, Others);');
x.AddTypeS('TDateTime','Double');
x.AddTypeS('TIntegerArray', 'Array of LongInt');
x.AddTypeS('TByteArray','Array of byte');
x.AddTypeS('TExtendedArray','Array of extended');
x.AddTypeS('TBoolArray', 'Array of Boolean');
x.AddTypes('TBox', 'record X1,Y1,X2,Y2 : Integer; end;');
x.AddTypeS('TBoxArray','Array of TBox');
x.AddTypeS('TPointArray','Array of TPoint');
x.AddTypeS('T2DPointArray','Array of TPointArray');
x.AddTypeS('TPointArrayArray','Array of TPointArray');
x.AddTypeS('TBmpMirrorStyle','(MirrorWidth,MirrorHeight,MirrorLine)');
x.AddTypeS('TMask','record  White, Black : TPointArray; WhiteHi,BlackHi : integer; W,H : integer;end;');
x.addtypeS('PPoint','record R,T : extended; end;');
x.AddTypeS('TTarget_Exported','record int1,int2,int3,int4,int5,int6,int7,int8,int9,int10,int11,int12,int13,int14,int15:integer; end;');

x.AddTypes('TSDTMPointDef', 'record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
x.AddTypes('TSDTMPointDefArray', 'Array Of TSDTMPointDef;');
x.AddTypes('TSDTM','record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end;');
x.AddTypes('TMDTMPoint','record x,y,c,t,asz : integer; bp : boolean; end;');
x.AddTypes('TMDTMPointArray','array of TMDTMPoint;');

x.AddTypeS('T2DExtendedArray', 'array of array of extended;');
x.AddTypeS('T3DExtendedArray','array of array of array of extended;');
x.AddTypeS('T2DIntegerArray','array of array of integer;');
x.AddTypeS('TStringArray','Array of string;');
x.AddTypeS('TMousePress', '(mouse_Down, mouse_Up);');
x.AddTypeS('Pointer', 'Integer');

x.AddTypeS('TSP_Property','(SP_WriteTimeStamp,SP_OnTerminate)');


x.AddConstantN('mouse_Right','integer').SetInt(ps_mouse_right); //0
x.AddConstantN('mouse_Left','integer').SetInt(ps_mouse_left);//1
x.AddConstantN('mouse_Middle','integer').SetInt(ps_mouse_middle);//2
