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

    Simba.inc: File with directives for SimbaUnit.
}

{$DEFINE SIMBA}

{$IFDEF LINUX}
    {$DEFINE LINUX_HOTKEYS} // For global hotkeys, requires libkeybinder.
                            // See Units/Linux/keybinder.pas
{$ENDIF}

{$DEFINE SIMBA_VERBOSE} // For more verbosity.

//{$DEFINE NOTPORTABLE} // NOTPORTABLE makes it easier to build portable by
                        // default, yet keep it possible to disable it with
                        // command line flags.

{$DEFINE USE_DEBUGGER}
{$DEFINE USE_SCRIPTMANAGER} // TODO
//{$DEFINE USE_FORMDESIGNER}

{$DEFINE USE_LAPE}

{$DEFINE USE_SQLITE} // TO build with SQLite3 support.

{$DEFINE USE_EXTENSIONS}
//{$DEFINE USE_CODECOMPLETION} // TODO

//{$DEFINE TERMINALWRITELN} // Only used once. Remove from SimbaUnit?

{$MACRO ON}
{$define lape_extdecl:=cdecl;}

{$if defined(UNIX) or defined(cpu64)}
	{$UNDEF USE_EXTENSIONS}
{$ifend}
