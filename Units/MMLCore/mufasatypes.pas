{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

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

    Type declarations for the Mufasa Macro Library
}

unit MufasaTypes;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils;
const
  DS = DirectorySeparator;
  MEOL = {$ifdef MSWINDOWS}#13+{$endif}#10;
  ps_mouse_right = 0;
  ps_mouse_left = 1;
  ps_mouse_middle = 2;

{ Overloaded Operators}

{ TPoint add }
operator + (PT1,PT2 : TPoint) : TPoint;

{ TPoint sub }
operator - (PT1,PT2 : TPoint) : TPoint;

{ TPoint comp}
operator = (PT1,PT2 : TPoint) : boolean;

operator >(PT1, PT2: TPoint): boolean;
operator <(PT1, PT2: TPoint): boolean;


type
  TRGB24 = packed record
    B, G, R : byte;
  end;
  PRGB24 = ^TRGB24;
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  PRGB32 = ^TRGB32;
  TRGB32Array = array of TRGB32;
  TPRGB32Array = array of PRGB32; //Array of Pointers

  THSL = record
    H, S, L: extended;
  end;
  PHSL = ^THSL;

  THSLArray = array of THSL;
  T2DHSLArray = array of array of THSL;

  TRetData = record
    Ptr : PRGB32;
    IncPtrWith : integer;
    RowLen : integer;
  end;
  TBmpMirrorStyle = (MirrorWidth,MirrorHeight,MirrorLine); //LineMirror is in line x=y;
  TTargetWindowMode = (w_BMP, w_Window, w_HDC, w_ArrayPtr, w_XWindow);
  TClickType = (mouse_Left, mouse_Right, mouse_Middle);
  TMousePress = (mouse_Down, mouse_Up);

  TStringArray = array of String;
  T2DStringArray = array of TStringArray;
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;
  TVariantArray = Array of Variant;
  PVariantArray = ^TVariantArray;
  TIntegerArray = Array of Integer;
  T2DIntArray = array of TIntegerArray;
  T2DIntegerArray = T2DIntArray;
  TBoolArray = array of boolean;
  TBooleanArray = TBoolArray;
  T2DBoolArray = Array of TBoolArray;
  TExtendedArray = Array of Extended;
  T2DExtendedArray = Array of Array of Extended;

  { Crypto }
  THashType = (htHaval, htMD4, htMD5, htRIPEMD128, htRIPEMD160,
               htSHA1, htSHA256, htSHA384, htSHA512, htTiger);

  { Mask Types }
  TMask = record
    White, Black : TPointArray;
    WhiteHi,BlackHi : integer;
    W,H : integer;
  end;
  { File types }

  TMufasaFile = record
    Path: String;
    FS: TFileStream;
    BytesRead, Mode: Integer;
  end;
  TMufasaFilesArray = Array Of TMufasaFile;

  { DTM Types }
  {
    Possibly add .name too?
    Then one could give DTM names, which would be easy for debugging.
  }
  TBox = record
    x1, y1, x2, y2: Integer;
  end;

  TSysProc = record
    Title: string;
    Handle: integer;
    Pid: integer;
    Width, Height: integer;
  end;
  TSysProcArr = array of TSysProc;
  PSysProcArr = ^TSysProcArr;
  PSysProc = ^TSysProc;

const
  TMDTMPointSize = 5*SizeOf(integer)+Sizeof(boolean);
type
  TMDTMPoint = record //TMufasaDTMPoint
    x,y,c,t,asz : integer;
    bp : boolean;
  end;

  PMDTMPoint = ^TMDTMPoint; //PointerMufasaDTMPoint
  TMDTMPointArray = array of TMDTMPoint; //TMufasaDTMPointArray


  { Other DTM Types }

  TSDTMPointDef = record
    x, y, Color, Tolerance, AreaSize, AreaShape: integer;
  end;

  TSDTMPointDefArray = Array Of TSDTMPointDef;

  TSDTM = record
    MainPoint: TSDTMPointDef;
    SubPoints: TSDTMPointDefArray;
  end;

  TWritelnProc = procedure(s: string);
  {events}
  TOpenFileEvent = procedure(Sender : TObject;var Filename : string; var Continue : boolean) of object;
  TWriteFileEvent = TOpenFileEvent;
  TOpenConnectionEvent = procedure(Sender : TObject; var url : string; var Continue : boolean) of object;
  TColourPickEvent = procedure(Sender : TObject; const Colour,colourx,coloury : integer) of object;
  TScriptStartEvent = procedure(Sender: TObject; var Script : string; var Continue : boolean) of object;
  TScriptCompileEvent = procedure(Sender: TObject; var Script : string; var Continue : boolean);
  TScriptExecuteEvent = procedure(Sender : TObject; const Script : string; var Continue : boolean);
  TScriptPauseEvent = TScriptExecuteEvent;
  TScriptStopEvent = TScriptExecuteEvent;
  TScriptOpenEvent = procedure(Sender: TObject; var Script : String) of object;

  TOpenConnectionData = record
    Sender : TObject;
    URL : PString;
    Continue : PBoolean;
  end;
  TOpenFileData = record
    Sender : TObject;
    FileName : PString;
    Continue : PBoolean;
  end;
  TWriteFileData = TOpenFileData;

  TScriptStartData = record
    Sender : TObject;
    Script : PString;
    Continue : PBoolean;
  end;

  TScriptOpenData = record
    Sender : TObject;
    Script : PString;
  end;


type
  VirtualKeyInfo = record
      Str : string;
      Key : byte;
  end;

type
   TBufferByteArray = Array[0..524287] of Byte;
   PBufferByteArray = ^TBufferByteArray;

   PPoint = ^TPoint;

  TOCRFilterData = packed record
      _type: integer;
      is_text_color: boolean;

      r_low,r_high,g_low,g_high,b_low,b_high,set_col: integer;

      ref_color,tol,cts: integer;
  end;

  POCRFilterData = ^TOCRFilterData;

  TOcrFilterDataArray = array of TOCRFilterData;
  POcrFilterDataArray = ^TOCRFilterDataArray;

var
  BufferString : PChar;
  BufferLen : LongWord;
  VirtualKeys : array[0..173] of VirtualKeyInfo = (
                (str :'UNKNOWN'; key :  0),
                (str :'LBUTTON'; key :  1),
                (str :'RBUTTON'; key :  2),
                (str :'CANCEL'; key :  3),
                (str :'MBUTTON'; key :  4),
                (str :'XBUTTON1'; key :  5),
                (str :'XBUTTON2'; key :  6),
                (str :'BACK'; key :  8),
                (str :'TAB'; key :  9),
                (str :'CLEAR'; key :  12),
                (str :'RETURN'; key :  13),
                (str :'SHIFT'; key :  16),
                (str :'CONTROL'; key :  17),
                (str :'MENU'; key :  18),
                (str :'PAUSE'; key :  19),
                (str :'CAPITAL'; key :  20),
                (str :'KANA'; key :  21),
                (str :'HANGUL'; key :  21),
                (str :'JUNJA'; key :  23),
                (str :'FINAL'; key :  24),
                (str :'HANJA'; key :  25),
                (str :'KANJI'; key :  25),
                (str :'ESCAPE'; key :  27),
                (str :'CONVERT'; key :  28),
                (str :'NONCONVERT'; key :  29),
                (str :'ACCEPT'; key :  30),
                (str :'MODECHANGE'; key :  31),
                (str :'SPACE'; key :  32),
                (str :'PRIOR'; key :  33),
                (str :'NEXT'; key :  34),
                (str :'END'; key :  35),
                (str :'HOME'; key :  36),
                (str :'LEFT'; key :  37),
                (str :'UP'; key :  38),
                (str :'RIGHT'; key :  39),
                (str :'DOWN'; key :  40),
                (str :'SELECT'; key :  41),
                (str :'PRINT'; key :  42),
                (str :'EXECUTE'; key :  43),
                (str :'SNAPSHOT'; key :  44),
                (str :'INSERT'; key :  45),
                (str :'DELETE'; key :  46),
                (str :'HELP'; key :  47),
                (str :'0'; key :  $30),
                (str :'1'; key :  $31),
                (str :'2'; key :  $32),
                (str :'3'; key :  $33),
                (str :'4'; key :  $34),
                (str :'5'; key :  $35),
                (str :'6'; key :  $36),
                (str :'7'; key :  $37),
                (str :'8'; key :  $38),
                (str :'9'; key :  $39),
                (str :'A'; key :  $41),
                (str :'B'; key :  $42),
                (str :'C'; key :  $43),
                (str :'D'; key :  $44),
                (str :'E'; key :  $45),
                (str :'F'; key :  $46),
                (str :'G'; key :  $47),
                (str :'H'; key :  $48),
                (str :'I'; key :  $49),
                (str :'J'; key :  $4A),
                (str :'K'; key :  $4B),
                (str :'L'; key :  $4C),
                (str :'M'; key :  $4D),
                (str :'N'; key :  $4E),
                (str :'O'; key :  $4F),
                (str :'P'; key :  $50),
                (str :'Q'; key :  $51),
                (str :'R'; key :  $52),
                (str :'S'; key :  $53),
                (str :'T'; key :  $54),
                (str :'U'; key :  $55),
                (str :'V'; key :  $56),
                (str :'W'; key :  $57),
                (str :'X'; key :  $58),
                (str :'Y'; key :  $59),
                (str :'Z'; key :  $5A),
                (str :'LWIN'; key :  $5B),
                (str :'RWIN'; key :  $5C),
                (str :'APPS'; key :  $5D),
                (str :'SLEEP'; key :  $5F),
                (str :'NUMPAD0'; key :  96),
                (str :'NUMPAD1'; key :  97),
                (str :'NUMPAD2'; key :  98),
                (str :'NUMPAD3'; key :  99),
                (str :'NUMPAD4'; key :  100),
                (str :'NUMPAD5'; key :  101),
                (str :'NUMPAD6'; key :  102),
                (str :'NUMPAD7'; key :  103),
                (str :'NUMPAD8'; key :  104),
                (str :'NUMPAD9'; key :  105),
                (str :'MULTIPLY'; key :  106),
                (str :'ADD'; key :  107),
                (str :'SEPARATOR'; key :  108),
                (str :'SUBTRACT'; key :  109),
                (str :'DECIMAL'; key :  110),
                (str :'DIVIDE'; key :  111),
                (str :'F1'; key :  112),
                (str :'F2'; key :  113),
                (str :'F3'; key :  114),
                (str :'F4'; key :  115),
                (str :'F5'; key :  116),
                (str :'F6'; key :  117),
                (str :'F7'; key :  118),
                (str :'F8'; key :  119),
                (str :'F9'; key :  120),
                (str :'F10'; key :  121),
                (str :'F11'; key :  122),
                (str :'F12'; key :  123),
                (str :'F13'; key :  124),
                (str :'F14'; key :  125),
                (str :'F15'; key :  126),
                (str :'F16'; key :  127),
                (str :'F17'; key :  128),
                (str :'F18'; key :  129),
                (str :'F19'; key :  130),
                (str :'F20'; key :  131),
                (str :'F21'; key :  132),
                (str :'F22'; key :  133),
                (str :'F23'; key :  134),
                (str :'F24'; key :  135),
                (str :'NUMLOCK'; key :  $90),
                (str :'SCROLL'; key :  $91),
                (str :'LSHIFT'; key :  $A0),
                (str :'RSHIFT'; key :  $A1),
                (str :'LCONTROL'; key :  $A2),
                (str :'RCONTROL'; key :  $A3),
                (str :'LMENU'; key :  $A4),
                (str :'RMENU'; key :  $A5),
                (str :'BROWSER_BACK'; key :  $A6),
                (str :'BROWSER_FORWARD'; key :  $A7),
                (str :'BROWSER_REFRESH'; key :  $A8),
                (str :'BROWSER_STOP'; key :  $A9),
                (str :'BROWSER_SEARCH'; key :  $AA),
                (str :'BROWSER_FAVORITES'; key :  $AB),
                (str :'BROWSER_HOME'; key :  $AC),
                (str :'VOLUME_MUTE'; key :  $AD),
                (str :'VOLUME_DOWN'; key :  $AE),
                (str :'VOLUME_UP'; key :  $AF),
                (str :'MEDIA_NEXT_TRACK'; key :  $B0),
                (str :'MEDIA_PREV_TRACK'; key :  $B1),
                (str :'MEDIA_STOP'; key :  $B2),
                (str :'MEDIA_PLAY_PAUSE'; key :  $B3),
                (str :'LAUNCH_MAIL'; key :  $B4),
                (str :'LAUNCH_MEDIA_SELECT'; key :  $B5),
                (str :'LAUNCH_APP1'; key :  $B6),
                (str :'LAUNCH_APP2'; key :  $B7),
                (str :'OEM_1'; key :  $BA),
                (str :'OEM_PLUS'; key :  $BB),
                (str :'OEM_COMMA'; key :  $BC),
                (str :'OEM_MINUS'; key :  $BD),
                (str :'OEM_PERIOD'; key :  $BE),
                (str :'OEM_2'; key :  $BF),
                (str :'OEM_3'; key :  $C0),
                (str :'OEM_4'; key :  $DB),
                (str :'OEM_5'; key :  $DC),
                (str :'OEM_6'; key :  $DD),
                (str :'OEM_7'; key :  $DE),
                (str :'OEM_8'; key :  $DF),
                (str :'OEM_102'; key :  $E2),
                (str :'PROCESSKEY'; key :  $E7),
                (str :'ATTN'; key :  $F6),
                (str :'CRSEL'; key :  $F7),
                (str :'EXSEL'; key :  $F8),
                (str :'EREOF'; key :  $F9),
                (str :'PLAY'; key :  $FA),
                (str :'ZOOM'; key :  $FB),
                (str :'NONAME'; key :  $FC),
                (str :'PA1'; key :  $FD),
                (str :'OEM_CLEAR'; key :  $FE),

                (str :'HIGHESTVALUE'; key :  $FE),
                (str :'UNDEFINED'; key :  $FF)
                );


implementation
operator+(PT1, PT2: TPoint): TPoint;
begin
  Result.x := PT1.x + PT2.x;
  Result.y := Pt1.y + PT2.y;
end;

operator-(PT1, PT2: TPoint): TPoint;
begin
  Result.x := PT1.x - PT2.x;
  Result.y := Pt1.y - PT2.y;
end;

operator=(PT1, PT2: TPoint): boolean;
begin
  result := ((PT1.x = PT2.x) and (pt1.y = pt2.y));
end;

operator >(PT1, PT2: TPoint): boolean;
begin
  Result := ((PT1.X > PT2.X) and (PT1.Y > PT2.Y));
end;

operator <(PT1, PT2: TPoint): boolean;
begin
  Result := ((PT1.X < PT2.X) and (PT1.Y < PT2.Y));
end;

initialization
  BufferString := StrAlloc(524288);
  BufferLen := 524288;
finalization
  StrDispose(bufferstring);

end.

