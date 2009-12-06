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
Sender.Comp.AddTypeS('TIntegerArray', 'Array of integer');
Sender.Comp.AddTypes('TBox', 'record X1,Y1,X2,Y2 : Integer; end;');
Sender.Comp.AddTypeS('TPointArray','Array of TPoint');
Sender.Comp.AddTypeS('T2DPointArray','Array of Array of TPoint');
Sender.Comp.AddTypeS('TPointArrayArray','Array of Array of TPoint');
Sender.Comp.AddTypeS('TBmpMirrorStyle','(MirrorWidth,MirrorHeight,MirrorLine)');
Sender.Comp.AddTypeS('TMask','record  White, Black : TPointArray; WhiteHi,BlackHi : integer; W,H : integer;end;');


Sender.Comp.AddTypes('TDTMPointDef', 'record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
Sender.Comp.AddTypes('TDTMPointDefArray', 'Array Of TDTMPointDef;');
Sender.Comp.AddTypes('TDTM','record MainPoint: TDTMPointDef; SubPoints: TDTMPointDefArray; end;');
Sender.Comp.AddTypeS('pDTM','record p: TPointArray; c, t, asz, ash: TIntegerArray; end;');
Sender.Comp.AddTypeS('T2DExtendedArray', 'array of array of extended;');
Sender.Comp.AddTypeS('TStringArray','Array of string;');
Sender.Comp.AddTypeS('TMousePress', '(mouse_Down, mouse_Up);');


Sender.Comp.AddConstantN('mouse_Right','integer').SetInt(ps_mouse_right); //0
Sender.Comp.AddConstantN('mouse_Left','integer').SetInt(ps_mouse_left);//1
Sender.Comp.AddConstantN('mouse_Middle','integer').SetInt(ps_mouse_middle);//2