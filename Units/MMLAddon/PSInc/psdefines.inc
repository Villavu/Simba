{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venentië and Merlijn Wajer

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

    PSDefines.inc for the Mufasa Macro Library
}

{$IFDEF CPU386 }
PSScript.Defines.Add('CPU386');
{$ENDIF }
PSScript.Defines.Add('MUFASA');
PSScript.Defines.Add('COGAT');
PSScript.Defines.Add('RAYMONDPOWNS');
{$IFDEF MSWINDOWS }
PSScript.Defines.Add('MSWINDOWS');
PSScript.Defines.Add('WIN32');
PSScript.Defines.Add('WINDOWS');
{$ENDIF }
{$IFDEF LINUX }
PSScript.Defines.Add('LINUX');
{$ENDIF }
