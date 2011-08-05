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

    lpdefines.inc for the Mufasa Macro Library
}

BaseDefines.Add('LAPE');
{$IFDEF CPU386 }
BaseDefines.Add('CPU386');
{$ENDIF }
BaseDefines.Add('MUFASA');
BaseDefines.Add('COGAT');
BaseDefines.Add('SIMBA');
BaseDefines.Add('DGROCKS');
{$IFDEF MSWINDOWS }
BaseDefines.Add('MSWINDOWS');
BaseDefines.Add('WIN32');
BaseDefines.Add('WINDOWS');
{$ENDIF }
{$IFDEF LINUX }
BaseDefines.Add('LINUX');
{$ENDIF }