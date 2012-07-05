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

addGlobalVar(ltString, @AppPath, 'AppPath').isConstant := True;
addGlobalVar(ltString, @DocPath, 'DocPath').isConstant := True;
addGlobalVar(ltString, @ScriptPath, 'ScriptPath').isConstant := True;
addGlobalVar(ltString, @ScriptFile, 'ScriptFile').isConstant := True;
addGlobalVar(ltString, @IncludePath, 'IncludePath').isConstant := True;
addGlobalVar(ltString, @PluginPath, 'PluginPath').isConstant := True;
addGlobalVar(ltString, @FontPath, 'FontPath').isConstant := True;

{$IFDEF USE_SQLITE}
// SQLite result codes returned by sqlite_errcode
addGlobalVar(0, 'SQLITE_OK').isConstant := true; // Successful result
addGlobalVar(1, 'SQLITE_ERROR').isConstant := true; // SQL error or missing database
addGlobalVar(2, 'SQLITE_INTERNAL').isConstant := true; // Internal logic error in SQLite
addGlobalVar(3, 'SQLITE_PERM').isConstant := true; // Access permission denied
addGlobalVar(4, 'SQLITE_ABORT').isConstant := true; // Callback routine requested an abort
addGlobalVar(5, 'SQLITE_BUSY').isConstant := true; // The database file is locked
addGlobalVar(6, 'SQLITE_LOCKED').isConstant := true; // A table in the database is locked
addGlobalVar(7, 'SQLITE_NOMEM').isConstant := true; // A malloc() failed
addGlobalVar(8, 'SQLITE_READONLY').isConstant := true; // Attempt to write a readonly database
addGlobalVar(9, 'SQLITE_INTERRUPT').isConstant := true; // Operation terminated by sqlite3_interrupt()
addGlobalVar(10, 'SQLITE_IOERR').isConstant := true; // Some kind of disk I/O error occurred
addGlobalVar(11, 'SQLITE_CORRUPT').isConstant := true; // The database disk image is malformed
addGlobalVar(12, 'SQLITE_NOTFOUND').isConstant := true; // Unknown opcode in sqlite3_file_control()
addGlobalVar(13, 'SQLITE_FULL').isConstant := true; // Insertion failed because database is full
addGlobalVar(14, 'SQLITE_CANTOPEN').isConstant := true; // Unable to open the database file
addGlobalVar(15, 'SQLITE_PROTOCOL').isConstant := true; // Database lock protocol error
addGlobalVar(16, 'SQLITE_EMPTY').isConstant := true; // Database is empty
addGlobalVar(17, 'SQLITE_SCHEMA').isConstant := true; // The database schema changed
addGlobalVar(18, 'SQLITE_TOOBIG').isConstant := true; // String or BLOB exceeds size limit
addGlobalVar(19, 'SQLITE_CONSTRAINT').isConstant := true; // Abort due to constraint violation
addGlobalVar(20, 'SQLITE_MISMATCH').isConstant := true; // Data type mismatch
addGlobalVar(21, 'SQLITE_MISUSE').isConstant := true; // Library used incorrectly
addGlobalVar(22, 'SQLITE_NOLFS').isConstant := true; // Uses OS features not supported on host
addGlobalVar(23, 'SQLITE_AUTH').isConstant := true; // Authorization denied
addGlobalVar(24, 'SQLITE_FORMAT').isConstant := true; // Auxiliary database format error
addGlobalVar(25, 'SQLITE_RANGE').isConstant := true; // 2nd parameter to sqlite3_bind out of range
addGlobalVar(26, 'SQLITE_NOTADB').isConstant := true; // File opened that is not a database file
addGlobalVar(27, 'SQLITE_ROW').isConstant := true; // sqlite3_step() has another row ready
addGlobalVar(28, 'SQLITE_DONE').isConstant := true; // sqlite3_step() has finished executing

// SQLite open flags for sqlite_open_v2
addGlobalVar(1, 'SQLITE_OPEN_READONLY').isConstant := true;
addGlobalVar(2, 'SQLITE_OPEN_READWRITE').isConstant := true;
addGlobalVar(4, 'SQLITE_OPEN_CREATE').isConstant := true;
addGlobalVar(40, 'SQLITE_OPEN_URI').isConstant := true;
addGlobalVar(8000, 'SQLITE_OPEN_NOMUTEX').isConstant := true;
addGlobalVar(10000, 'SQLITE_OPEN_FULLMUTEX').isConstant := true;
addGlobalVar(20000, 'SQLITE_OPEN_SHAREDCACHE').isConstant := true;
addGlobalVar(40000, 'SQLITE_OPEN_PRIVATECACHE').isConstant := true;
{$ENDIF}

//addGlobalVar(maxLongint, 'MaxLongInt').isConstant := True;  //Don't need these...
//addGlobalVar(maxLongint, 'MaxInt').isConstant := True;      //High works here....

addGlobalVar(ps_mouse_right, 'mouse_Right').isConstant := True;   //0
addGlobalVar(ps_mouse_left, 'mouse_Left').isConstant := True;     //1
addGlobalVar(ps_mouse_middle, 'mouse_Middle').isConstant := True; //2

addGlobalType('UInt32', 'TClickType');
addGlobalType('UInt32', 'DWord');

addGlobalType('Integer', 'TColor');

addGlobalType('(Numbers, Letters, Others)', 'StrExtr');
addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
addGlobalType('(mouse_Down, mouse_Up)', 'TMousePress');
addGlobalType('(SP_WriteTimeStamp, SP_OnTerminate)', 'TSP_Property');

addGlobalType('array of string', 'TStringArray');
addGlobalType('array of TStringArray', 'T2DStringArray');
addGlobalType('array of Integer', 'TIntegerArray');
addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
addGlobalType('array of TIntegerArray', 'T2DIntArray');
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

addGlobalType('packed record _type, is_text_color: integer; r_low,r_high,g_low,g_high,b_low,b_high,set_col: integer; ref_color,tol,cts: integer; end;', 'TOCRFilterData');

addGlobalType('array of TOCRFilterData', 'TOCRFilterDataArray');

{ This is a workaround to allow Lape to export TMufasaBitmaps.
  Also see lpexportedmethods.inc }
addGlobalType('Pointer', 'TMufasaBitmap');
