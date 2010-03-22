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

Sender.Comp.AddConstantN('AppPath','string').SetString(CurrThread.AppPath);
Sender.Comp.AddConstantN('ScriptPath','string').SetString(CurrThread.ScriptPath);
Sender.Comp.AddConstantN('IncludePath','string').SetString(CurrThread.IncludePath);
Sender.Comp.AddConstantN('PluginPath','string').SetString(CurrThread.PluginPath);
Sender.Comp.AddConstantN('FontPath','string').SetString(CurrThread.FontPath);
Sender.Comp.AddTypeS('TReplaceFlag', '(rfReplaceAll, rfIgnoreCase)');
Sender.Comp.AddTypeS('TReplaceFlags','set of TReplaceFlag');
Sender.Comp.AddTypeS('StrExtr','(Numbers, Letters, Others);');
Sender.Comp.AddTypeS('TDateTime','Double');
Sender.Comp.AddTypeS('TIntegerArray', 'Array of LongInt');
Sender.Comp.AddTypeS('TExtendedArray','Array of extended');
Sender.Comp.AddTypeS('TBoolArray', 'Array of Boolean');
Sender.Comp.AddTypes('TBox', 'record X1,Y1,X2,Y2 : Integer; end;');
Sender.Comp.AddTypeS('TBoxArray','Array of TBox');
Sender.Comp.AddTypeS('TPointArray','Array of TPoint');
Sender.Comp.AddTypeS('T2DPointArray','Array of TPointArray');
Sender.Comp.AddTypeS('TPointArrayArray','Array of TPointArray');
Sender.Comp.AddTypeS('TBmpMirrorStyle','(MirrorWidth,MirrorHeight,MirrorLine)');
Sender.Comp.AddTypeS('TMask','record  White, Black : TPointArray; WhiteHi,BlackHi : integer; W,H : integer;end;');
Sender.Comp.addtypeS('PPoint','record R,T : extended; end;');
Sender.Comp.AddTypeS('TTarget_Exported','record int1,int2,int3,int4,int5,int6,int7,int8,int9,int10,int11,int12,int13,int14:integer; end;');

Sender.Comp.AddTypes('TDTMPointDef', 'record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
Sender.Comp.AddTypes('TDTMPointDefArray', 'Array Of TDTMPointDef;');
Sender.Comp.AddTypes('TDTM','record MainPoint: TDTMPointDef; SubPoints: TDTMPointDefArray; end;');
Sender.Comp.AddTypeS('pDTM','record l: Integer;p: TPointArray;c, t, asz, ash: TIntegerArray; bp: Array Of Boolean; n: String; end;');

Sender.Comp.AddTypeS('T2DExtendedArray', 'array of array of extended;');
Sender.Comp.AddTypeS('T3DExtendedArray','array of array of array of extended;');
Sender.Comp.AddTypeS('T2DIntegerArray','array of array of integer;');
Sender.Comp.AddTypeS('TStringArray','Array of string;');
Sender.Comp.AddTypeS('TMousePress', '(mouse_Down, mouse_Up);');
Sender.Comp.AddTypeS('Pointer', 'Integer');

Sender.Comp.AddTypeS('TSP_Property','(SP_WriteTimeStamp)');


Sender.Comp.AddConstantN('mouse_Right','integer').SetInt(ps_mouse_right); //0
Sender.Comp.AddConstantN('mouse_Left','integer').SetInt(ps_mouse_left);//1
Sender.Comp.AddConstantN('mouse_Middle','integer').SetInt(ps_mouse_middle);//2
