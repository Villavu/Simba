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
AddConstantN('DocPath', 'string').SetString(DocPath);
AddConstantN('ScriptPath','string').SetString(ScriptPath);
AddConstantN('ScriptFile','string').SetString(ScriptFile);
AddConstantN('IncludePath','string').SetString(SimbaSettings.Includes.Path.Value);
AddConstantN('PluginPath','string').SetString(SimbaSettings.Plugins.Path.Value);
AddConstantN('FontPath','string').SetString(SimbaSettings.Fonts.Path.Value);

{$IFDEF USE_SQLITE}
// SQLite result codes returned by sqlite_errcode
AddConstantN('SQLITE_OK', 'integer').SetInt(0); // Successful result
AddConstantN('SQLITE_ERROR', 'integer').SetInt(1); // SQL error or missing database
AddConstantN('SQLITE_INTERNAL', 'integer').SetInt(2); // Internal logic error in SQLite
AddConstantN('SQLITE_PERM', 'integer').SetInt(3); // Access permission denied
AddConstantN('SQLITE_ABORT', 'integer').SetInt(4); // Callback routine requested an abort
AddConstantN('SQLITE_BUSY', 'integer').SetInt(5); // The database file is locked
AddConstantN('SQLITE_LOCKED', 'integer').SetInt(6); // A table in the database is locked
AddConstantN('SQLITE_NOMEM', 'integer').SetInt(7); // A malloc() failed
AddConstantN('SQLITE_READONLY', 'integer').SetInt(8); // Attempt to write a readonly database
AddConstantN('SQLITE_INTERRUPT', 'integer').SetInt(9); // Operation terminated by sqlite3_interrupt()
AddConstantN('SQLITE_IOERR', 'integer').SetInt(10); // Some kind of disk I/O error occurred
AddConstantN('SQLITE_CORRUPT', 'integer').SetInt(11); // The database disk image is malformed
AddConstantN('SQLITE_NOTFOUND', 'integer').SetInt(12); // Unknown opcode in sqlite3_file_control()
AddConstantN('SQLITE_FULL', 'integer').SetInt(13); // Insertion failed because database is full
AddConstantN('SQLITE_CANTOPEN', 'integer').SetInt(14); // Unable to open the database file
AddConstantN('SQLITE_PROTOCOL', 'integer').SetInt(15); // Database lock protocol error
AddConstantN('SQLITE_EMPTY', 'integer').SetInt(16); // Database is empty
AddConstantN('SQLITE_SCHEMA', 'integer').SetInt(17); // The database schema changed
AddConstantN('SQLITE_TOOBIG', 'integer').SetInt(18); // String or BLOB exceeds size limit
AddConstantN('SQLITE_CONSTRAINT', 'integer').SetInt(19); // Abort due to constraint violation
AddConstantN('SQLITE_MISMATCH', 'integer').SetInt(20); // Data type mismatch
AddConstantN('SQLITE_MISUSE', 'integer').SetInt(21); // Library used incorrectly
AddConstantN('SQLITE_NOLFS', 'integer').SetInt(22); // Uses OS features not supported on host
AddConstantN('SQLITE_AUTH', 'integer').SetInt(23); // Authorization denied
AddConstantN('SQLITE_FORMAT', 'integer').SetInt(24); // Auxiliary database format error
AddConstantN('SQLITE_RANGE', 'integer').SetInt(25); // 2nd parameter to sqlite3_bind out of range
AddConstantN('SQLITE_NOTADB', 'integer').SetInt(26); // File opened that is not a database file
AddConstantN('SQLITE_ROW', 'integer').SetInt(27); // sqlite3_step() has another row ready
AddConstantN('SQLITE_DONE', 'integer').SetInt(28); // sqlite3_step() has finished executing

// SQLite open flags for sqlite_open_v2
AddConstantN('SQLITE_OPEN_READONLY', 'integer').SetInt(1);
AddConstantN('SQLITE_OPEN_READWRITE', 'integer').SetInt(2);
AddConstantN('SQLITE_OPEN_CREATE', 'integer').SetInt(4);
AddConstantN('SQLITE_OPEN_URI', 'integer').SetInt(40);
AddConstantN('SQLITE_OPEN_NOMUTEX', 'integer').SetInt(8000);
AddConstantN('SQLITE_OPEN_FULLMUTEX', 'integer').SetInt(10000);
AddConstantN('SQLITE_OPEN_SHAREDCACHE', 'integer').SetInt(20000);
AddConstantN('SQLITE_OPEN_PRIVATECACHE', 'integer').SetInt(40000);
{$ENDIF}

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
AddTypeS('T3DIntegerArray','array of array of array of integer;');
AddTypeS('TStringArray','Array of string;');
AddTypeS('T2DStringArray','array of TStringArray');
AddTypeS('TMousePress', '(mouse_Down, mouse_Up);');
AddTypeS('Pointer', 'Integer');

// XXX: THIS NEEDS FIXING, I THINK.
{$IFDEF CPU32}
AddTypeS('PtrUInt', 'LongInt');
{$ELSE}
AddTypeS('PtrUInt', 'Int64');
{$ENDIF}

AddTypeS('TSP_Property','(SP_WriteTimeStamp,SP_OnTerminate)');

AddConstantN('mouse_Right','integer').SetInt(ps_mouse_right); //0
AddConstantN('mouse_Left','integer').SetInt(ps_mouse_left);//1
AddConstantN('mouse_Middle','integer').SetInt(ps_mouse_middle);//2

AddTypes('TSysProc', 'record Title: string; Handle: integer; Pid: integer; Width, Height: integer; end;');
AddTypes('TSysProcArr', 'array of TSysProc;');

AddTypes('THashType', '(htHaval, htMD4, htMD5, htRIPEMD128, htRIPEMD160, htSHA1, htSHA256, htSHA384, htSHA512, htTiger);');

AddTypes('TOCRFilterData', 'record _type, is_text_color: integer; r_low,r_high,g_low,g_high,b_low,b_high,set_col: integer; ref_color,tol,cts: integer; end;');
AddTypes('TOCRFilterDataArray', 'array of TOCRFilterData');
