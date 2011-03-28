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

    lpexportedmethods.inc for the Mufasa Macro Library
}


addGlobalFunc('procedure WriteLn(s: string);', @lpWriteLn_Str);
addGlobalFunc('procedure WriteLn(i: int32); overload;', @lpWriteLn_Int32);
addGlobalFunc('procedure WriteLn(i: int64); overload;', @lpWriteLn_Int64);
addGlobalFunc('procedure WriteLn(b: boolean); overload;', @lpWriteLn_Bool);
addGlobalFunc('procedure WriteLn(e: extended); overload;', @lpWriteLn_Extended);

//addGlobalFunc('procedure WriteLn(s: string; fmt: array of const);', @lpWriteLn_FMT);

//addGlobalFunc('function CallProc(ProcName: string; var V: TVariantArray): Variant;', @lpCallProc)
//addGlobalFunc('function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;', @lpThreadSafeCall);

{ DTM }
addGlobalFunc('procedure SetDTMName(DTM : integer;const name : string);', @lpSetDTMName);
addGlobalFunc('function DTMFromString(const DTMString: String): Integer;', @lpDTMFromString);
addGlobalFunc('procedure FreeDTM(DTM: Integer);', @lpFreeDTM);
addGlobalFunc('function FindDTM(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;', @lpFindDTM);
addGlobalFunc('function FindDTMs(DTM: Integer; var p: TPointArray; xs, ys, xe, ye: Integer): Boolean;', @lpFindDTMs);
addGlobalFunc('function FindDTMRotatedSE(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean;', @lpFindDTMRotatedSE);
addGlobalFunc('function FindDTMRotatedAlternating(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean;', @lpFindDTMRotatedAlternating);
addGlobalFunc('function FindDTMsRotatedSE(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;', @lpFindDTMsRotatedSE);
addGlobalFunc('function FindDTMsRotatedAlternating(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;', @lpFindDTMsRotatedAlternating);
{addGlobalFunc('function AddMDTM(const d: TMDTM): Integer;', @lpaddDTM);
addGlobalFunc('function AddDTM(const d: TMDTM): Integer;', @lpaddDTM);
addGlobalFunc('function AddSDTM(const d: TSDTM): Integer;', @lpaddTSDTM);
addGlobalFunc('function GetDTM(index: Integer) : TMDTM', @lpGetDTM );
addGlobalFunc('function SDTMToMDTM(Const DTM: TSDTM): TMDTM;', @lpSDTMToMDTM);
addGlobalFunc('procedure PrintDTM(const DTM : TMDTM);', @lpPrintDTM);
addGlobalFunc('function MDTMToSDTM(Const DTM: TMDTM): TSDTM;', @lpMDTMToSDTM);
addGlobalFunc('function CreateDTMPoint(x,y,c,t,asz : integer; bp : boolean) : TMDTMPoint;', @lpCreateDTMPoint);}

{maths}
addGlobalFunc('function Random(i: int32): int32;', @lpRandom);
{addGlobalFunc('function Round(e:extended) : integer', @lpround);
addGlobalFunc('function ceil(e : extended) : integer', @lpceil);
addGlobalFunc('function floor(e : extended) : integer', @lpfloor);
addGlobalFunc('function pow(base,exponent : extended) : extended', @lppow);
addGlobalFunc('function exp(exponent : extended) : extended', @lpexp);
addGlobalFunc('function RiemannGauss(Xstart,StepSize,Sigma : extended; AmountSteps : integer) : extended;', @lpRiemannGauss);
addGlobalFunc('function DiscreteGauss(Xstart,Xend : integer; sigma : extended) : TExtendedArray;', @lpDiscreteGauss);
addGlobalFunc('function GaussMatrix(N : integer; sigma : extended) : T2DExtendedArray;', @lpGaussMatrix);
addGlobalFunc('function Max(a, b: Integer): Integer;', @lpmax);
addGlobalFunc('function Min(a, b: Integer): Integer;', @lpmin);
addGlobalFunc('function MinE(a, b: extended): Extended;', @lpminE);
addGlobalFunc('function MaxE(a, b: extended): Extended;', @lpmaxE);
addGlobalFunc('function iAbs(a : integer) : integer;', @lpiAbs);
addGlobalFunc('function ArcTan2(y,x : extended) : extended;', @lpArcTan2);
addGlobalFunc('function IntToBox(xs,ys,xe,ye : integer) : TBox;', @lpIntToBox);
addGlobalFunc('function IntInBox(x, y: Integer; Box: TBox): Boolean;', @lpIntInBox);
addGlobalFunc('function PointToBox(PT1,PT2 : TPoint): TBox;', @lpPointToBox);
addGlobalFunc('function PointInBox(PT : TPoint; Box: TBox): Boolean;', @lpPointInBox);
addGlobalFunc('function Sqr(e : extended) : extended;', @lpsqr);
addGlobalFunc('function Point(x,y:integer) : TPoint;', @lppoint);
addGlobalFunc('function Distance(xs,ys,xe,ye : integer) : integer;', @lpDistance);
addGlobalFunc('function Hypot(X, Y: Extended): Extended;', @lphypot);
addGlobalFunc('function RandomRange(aFrom,aTo: Integer): Integer;', @lpRandomRange);
addGlobalFunc('function Random(Int: integer): integer;', @lpRandom);
addGlobalFunc('function RandomE: extended;', @lpRandomE);
addGlobalFunc('procedure IncEx(var x : integer; increase : integer);', @lpincex);
addGlobalFunc('procedure DecEx(var x : integer; Decrease : integer);', @lpDecEx);
addGlobalFunc('function BinCoe(a, b: LongInt): Extended;', @lpBinCoe);
addGlobalFunc('function FixD(Degrees : extended) : Extended;', @lpFixD);
addGlobalFunc('function InRange(const value,min,max : integer) : boolean;', @lpInRange);
addGlobalFunc('function logn(base, x : extended): extended;', @lplogn);
addGlobalFunc('function log10(f : extended): extended;', @lplog10);
addGlobalFunc('function ln(x : extended) : extended;', @lpln);
addGlobalFunc('function IntToHex(number : integer) : string', @lpinttohex);
addGlobalFunc('function HexToInt(Hex : string) : integer', @lphextoint);
addGlobalFunc('function sar(AValue : longint; shift : byte) : longint;', @lpsar);
addGlobalFunc('function ror(num : longword; shift : byte) : LongWord;', @lpror);
addGlobalFunc('function rol(num : longword; shift : byte) : LongWord;', @lprol);
addGlobalFunc('function ps_tan(e: extended): extended;', @lptan);
addGlobalFunc('function radians(e: extended): extended;', @lpradians);
addGlobalFunc('function degrees(e: extended): extended;', @lpdegrees);
addGlobalFunc('function ArcSin(e: extended): extended;', @lpArcSin);
addGlobalFunc('function ArcCos(e: extended): extended;', @lpArcCos);
addGlobalFunc('function ArcTan(e: extended): extended;', @lpArcTan);
addGlobalFunc('function Cotan(e: extended): extended;', @lpCotan);
addGlobalFunc('function Secant(e: extended): extended;', @lpSecant);
addGlobalFunc('function Cosecant(e: extended): extended;', @lpCosecant);
addGlobalFunc('function Cot(e: extended): extended;', @lpCot);
addGlobalFunc('function Sec(e: extended): extended;', @lpSec);
addGlobalFunc('function Csc(e: extended): extended;', @lpCsc);
addGlobalFunc('function Cosh(e: extended): extended;', @lpCosh);
addGlobalFunc('function Sinh(e: extended): extended;', @lpSinh);
addGlobalFunc('function Tanh(e: extended): extended;', @lpTanh);
addGlobalFunc('function CotH(e: extended): extended;', @lpCotH);
addGlobalFunc('function SecH(e: extended): extended;', @lpSecH);
addGlobalFunc('function CscH(e: extended): extended;', @lpCscH);
addGlobalFunc('function ArcCosh(e: extended): extended;', @lpArcCosh);
addGlobalFunc('function ArcSinh(e: extended): extended;', @lpArcSinh);
addGlobalFunc('function DecRet(e: Extended): Extended;', @DecRet);}

{window}
addGlobalFunc('function Freeze: boolean;', @lpFreeze);
addGlobalFunc('function Unfreeze: boolean;', @lpUnfreeze);
addGlobalFunc('procedure GetClientDimensions(var w, h:integer);', @lpGetClientDimensions);
addGlobalFunc('procedure GetClientPosition(var left, top:integer);', @lpGetClientPosition);
addGlobalFunc('function SetTargetBitmap(Bitmap : integer): integer;', @lpSetTargetBitmap);
addGlobalFunc('function SetTargetArray(P: Integer; w, h: integer): integer;', @lpSetTargetArray);
//addGlobalFunc('function SetEIOSTarget(name: string; initargs: Variant): integer;', @lpSetEIOSTarget);
addGlobalFunc('procedure SetImageTarget(idx: integer);', @lpSetImageTarget);
addGlobalFunc('procedure SetKeyMouseTarget(idx: integer);', @lpSetKeyMouseTarget);
addGlobalFunc('function GetImageTarget: integer;', @lpGetImageTarget);
addGlobalFunc('function GetKeyMouseTarget: integer;', @lpGetKeyMouseTarget);
//addGlobalFunc('function ExportImageTarget : TTarget_Exported;', @lpExportImageTarget);
//addGlobalFunc('function ExportKeyMouseTarget : TTarget_Exported;', @lpExportKeyMouseTarget);
addGlobalFunc('procedure FreeTarget(idx: integer);', @lpFreeTarget);
addGlobalFunc('procedure SetDesktopAsClient;', @lpSetDesktopAsClient);
addGlobalFunc('procedure ActivateClient;', @lpActivateClient);
addGlobalFunc('function IsTargetValid: boolean;', @lpIsTargetValid);

{files}
addGlobalFunc('function CreateFile(const Path: string): Integer;', @lpCreateFile);
addGlobalFunc('function OpenFile(const Path: string; Shared: Boolean): Integer;', @lpOpenFile);
addGlobalFunc('function RewriteFile(const Path: string; Shared: Boolean): Integer;', @lpRewriteFile);
addGlobalFunc('function AppendFile(const Path: string): Integer;', @lpAppendFile);
addGlobalFunc('procedure CloseFile(FileNum: Integer);', @lpCloseFile);
addGlobalFunc('function EndOfFile(FileNum: Integer): Boolean;', @lpEndOfFile);
addGlobalFunc('function FileSize(FileNum: Integer): LongInt;', @lpFileSize);
addGlobalFunc('function ReadFileString(FileNum: Integer; var s: string; x: Integer): Boolean;', @lpReadFileString);
addGlobalFunc('function WriteFileString(FileNum: Integer; s: string): Boolean;', @lpWriteFileString);
addGlobalFunc('function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;', @lpSetFileCharPointer);
addGlobalFunc('function FilePointerPos(FileNum: Integer): Integer;', @lpFilePointerPos);
addGlobalFunc('function DirectoryExists(const DirectoryName : string ) : Boolean;', @lpDirectoryExists);
addGlobalFunc('function CreateDirectory(const DirectoryName : string) : boolean;', @lpCreateDirectory);
addGlobalFunc('function FileExists (const FileName : string ) : Boolean;', @lpFileExists);
addGlobalFunc('function ForceDirectories(const dir : string) : boolean;', @lpForceDirectores);
addGlobalFunc('function GetFiles(const Path, Ext : string) : TStringArray;', @lpGetFiles);
addGlobalFunc('function GetDirectories(const path : string) : TStringArray;', @lpGetDirectories);
addGlobalFunc('procedure WriteINI(const Section, KeyName, NewString, FileName: string);', @lpWriteINI);
addGlobalFunc('function ReadINI(const Section, KeyName, FileName: string): string;', @lpReadINI);
addGlobalFunc('procedure DeleteINI(const Section, KeyName, FileName: string);', @lpDeleteINI);
addGlobalFunc('function ExtractFileExt(const FileName: string): string;', @lpExtractFileExt);

{other}
addGlobalFunc('procedure Wait(t: LongWord);', @lpWait);
addGlobalFunc('procedure Wait(t, r: LongWord); overload;', @lpWait_Rand);
addGlobalFunc('procedure Sleep(t: LongWord);', @lpWait);
addGlobalFunc('procedure Sleep(t, r: LongWord); overload;', @lpWait_Rand);
{addGlobalFunc('function GetTClient : TClient;', @lpGetTClient);
addGlobalFunc('procedure SetSupressExceptions(Supress : boolean);', @lpSetSupressExceptions);
addGlobalFunc('procedure SaveScreenshot(FileName: string);', @lpSaveScreenshot);
addGlobalFunc('procedure TerminateScript;', @lpTerminateScript);
addGlobalFunc('function Now: TDateTime;', @lpnow);
addGlobalFunc('function Date : TDateTime;', @lpdate);
addGlobalFunc('function GetSystemTime: LongWord;', @lpGetTickCount);
addGlobalFunc('function GetTickCount: LongWord;', @lpGetTickCount);
addGlobalFunc('function GetTimeRunning: LongWord;', @lpGetTimeRunning);
addGlobalFunc('procedure DecodeTime(DateTime : TDateTime; var Hour,Min,Sec,MSec : word);', @lpDecodeTime);
addGlobalFunc('procedure DecodeDate ( const SourceDate  : TDateTime; var Year, Month, Day  : Word );', @lpDecodeDate);
addGlobalFunc('procedure ConvertTime(Time: integer; var h, m, s: integer);', @lpConvertTime);
addGlobalFunc('procedure HakunaMatata;', @lpHakunaMatata);
addGlobalFunc('procedure Simba;', @lpSimba);
addGlobalFunc('procedure PlaySound( Sound : string);', @lpPlaySound);
addGlobalFunc('procedure StopSound;', @lpStopSound);
addGlobalFunc('procedure DisplayDebugImgWindow(w, h: integer);', @lpDisplayDebugImgWindow);
addGlobalFunc('procedure DrawBitmapDebugImg(bmp: integer);', @lpDrawBitmapDebugImg);
addGlobalFunc('function GetDebugBitmap: integer;', @lpGetDebugBitmap);
addGlobalFunc('procedure ClearDebugImg;', @lpClearDebugImg);
addGlobalFunc('procedure ClearDebug;', @lpClearDebug);
addGlobalFunc('procedure Status(Status : string);', @lpStatus);
addGlobalFunc('procedure Disguise(Caption : string);', @lpDisguise);
addGlobalFunc('function SetScriptProp(Prop : TSP_Property; Value: TVariantArray): boolean;', @lpSetScriptProp);
addGlobalFunc('function GetScriptProp(Prop : TSP_Property;var Value: TVariantArray): boolean;', @lpGetScriptProp);
addGlobalFunc('function InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;', @lpInputQuery);
addGlobalFunc('procedure ShowMessage(msg : string);', @lpShowMessage);
addGlobalFunc('function MessageBox(Text, Caption: string; Flags: LongInt): Integer;', @lpmessageBox);
addGlobalFunc('function MessageDlg(const Caption, Msg: string; DlgType: TMsgDlgType;  Buttons: TMsgDlgButtons) : integer;', @lpMessageDlg);
addGlobalFunc('procedure SetClipBoard(const Data: string);', @lpSetClipBoard);
addGlobalFunc('function GetClipBoard: string;', @lpGetClipBoard);
addGlobalFunc('function GetProcesses: TSysProcArr;', @lpGetProcesses);
addGlobalFunc('procedure SetTarget(Proc: TSysProc);', @lpSetTarget);}

{string}
{addGlobalFunc('function Capitalize(str : string) : string;', @lpCapitalize);
addGlobalFunc('function CompressString(const Str : string) : string;', @lpCompressString);
addGlobalFunc('function DecompressString(const Compressed : string) : string;', @lpDecompressString);
addGlobalFunc('function Base64Encode(const str : string) : string;', @lpBase64Encode);
addGlobalFunc('function Base64Decode(const str : string) : string;', @lpBase64Decode);
addGlobalFunc('function Format(const fmt : string;const args : array of const) : string;', @lpFormat);
AddFunction(nil,'function ToStr(x) : string;');
addGlobalFunc('function Between(s1, s2, str: string): string;', @lpBetween);
addGlobalFunc('function IntToStr(value: Integer): String;', @lpIntToStr);
addGlobalFunc('function FloatToStr(value: Extended): String;', @lpFloatToStr);
addGlobalFunc('function BoolToStr(value: Boolean): String;', @lpBoolToStr);
addGlobalFunc('function StrToInt(value: String): Integer;', @lpStrToInt);
addGlobalFunc('function StrToIntDef(value: String; default: Integer): Integer;', @lpStrToIntDef);
addGlobalFunc('function StrToFloat(value: String): Extended;', @lpStrToFloat);
addGlobalFunc('function StrToFloatDef(value: String; default: Extended): Extended;', @lpStrToFloatDef);
addGlobalFunc('function StrToBool(value: String): Boolean;', @lpStrToBool);
addGlobalFunc('function StrToBoolDef(value: String; default: Boolean): Boolean;', @lpStrToBoolDef);
addGlobalFunc('function ExtractFromStr( Str : string; Extract : StrExtr) : string;', @lpExtractFromStr);
addGlobalFunc('function Replace(Text, FindStr, ReplaceStr: string; Flags: TReplaceFlags): string;', @lpReplace);
addGlobalFunc('function ReplaceWrap(Text, FindStr, ReplaceStr: string; Flags: TReplaceFlags): string;', @lpReplace);
addGlobalFunc('function Implode(Glue: string; Pieces: TStringArray): string;', @lpImplode);
addGlobalFunc('function Explode(del, str: string): TStringArray;', @lpExplode);
addGlobalFunc('procedure ExplodeWrap(del, str: string; var res : TStringArray);', @lpexplodewrap);
addGlobalFunc('function Padl(s: String; i: longInt): String;', @lpPadL);
addGlobalFunc('function Padz(s: String; i: longInt): String;', @lpPadz);
addGlobalFunc('function Padr(s: String; i: longInt): String;', @lppadR);
addGlobalFunc('function ExecRegExpr( const RegExpr, InputStr : String) : boolean;', @lpExecRegExpr);
addGlobalFunc('procedure SplitRegExpr( const RegExpr, InputStr : String; Pieces : TStrings);', @lpSplitRegExpr);
addGlobalFunc('function ReplaceRegExpr( const RegExpr, InputStr, ReplaceStr : String; UseSubstitution : boolean) : String;', @lpReplaceRegExpr);
addGlobalFunc('function PosEx(needle, haystack: String; offset: integer): integer;', @lpposex);}

{crypto}
addGlobalFunc('function haval(Data: string): string;', @lphaval);
addGlobalFunc('function md4(Data: string): string;', @lpmd4);
addGlobalFunc('function md5(Data: string): string;', @lpmd5);
addGlobalFunc('function ripemd128(Data: string): string;', @lpripemd128);
addGlobalFunc('function ripemd160(Data: string): string;', @lpripemd160);
addGlobalFunc('function sha1(Data: string): string;', @lpsha1);
addGlobalFunc('function sha256(Data: string): string;', @lpsha256);
addGlobalFunc('function sha384(Data: string): string;', @lpsha384);
addGlobalFunc('function sha512(Data: string): string;', @lpsha512);
addGlobalFunc('function tiger(Data: string): string;', @lptiger);

{web}
addGlobalFunc('procedure OpenWebPage(const url : string);', @lpOpenWebPage);
addGlobalFunc('function GetPage(const url : string): string;', @lpGetPage);
addGlobalFunc('function InitializeHTTPClient(HandleCookies: Boolean): Integer;', @lpInitializeHTTPClient);
addGlobalFunc('function InitializeHTTPClientWrap(HandleCookies: Boolean): Integer;', @lpInitializeHTTPClient);
addGlobalFunc('procedure FreeHTTPClient(Client: Integer);', @lpFreeHTTPClient);
addGlobalFunc('function GetHTTPPage(Client: Integer;const URL: string): string;', @lpGetHTTPPage);
addGlobalFunc('procedure SetHTTPUserAgent(Client: Integer;const Agent: string);', @lpSetHTTPUserAgent);
addGlobalFunc('function PostHTTPPage(Client: Integer;const Url,PostData: string): string;', @lpPostHTTPPage);
addGlobalFunc('function PostHTTPPageEx(Client: Integer;const Url: string): string;', @lpPostHTTPPageEx);
addGlobalFunc('procedure ClearPostData(Client: Integer);', @lpClearPostData);
addGlobalFunc('procedure AddPostVariable(Client: Integer;const VarName, VarValue: string);', @lpAddPostVariable);
addGlobalFunc('function GetRawHeaders(Client: Integer): string;', @lpGetRawHeaders);
addGlobalFunc('procedure SetProxy(Client : Integer; pHost, pPort : String);', @lpSetProxy);

{socket}
addGlobalFunc('function CreateSocket: integer;', @lpCreateSocket);
addGlobalFunc('procedure FreeSocket(Index: integer);', @lpFreeSocket);
addGlobalFunc('procedure ConnectSocket(Client: integer; IP, Port: string);', @lpConnectSocket);
addGlobalFunc('procedure BindSocket(Client: integer; IP, Port: string);', @lpBindSocket);
addGlobalFunc('procedure ListenSocket(Client: integer);', @lpListenSocket);
addGlobalFunc('function AcceptSocket(Client: integer): integer;', @lpAcceptSocket);
addGlobalFunc('procedure CloseSocket(Client: integer);', @lpCloseSocket);
addGlobalFunc('function RecvSocket(Client: integer): string;', @lpRecvSocket);
addGlobalFunc('function RecvSocketStr(Client: integer): string;', @lpRecvSocketStr);
addGlobalFunc('function RecvSocketEx(Client, Length: integer): string;', @lpRecvSocketEx);
addGlobalFunc('procedure SendSocket(Client: integer; Data: string);', @lpSendSocket);
addGlobalFunc('procedure SetTimeout(Client, Time: integer);', @lpSetSocketTimeout);
addGlobalFunc('procedure SocketInfo(Client: integer; out IP, Port: string);', @lpSocketInfo);

{ Color Conversions and Speed }
addGlobalFunc('procedure ColorToRGB(Color: integer; var r, g, b: Integer);', @lpColorToRGB);
addGlobalFunc('function RGBtoColor(r, g, b: Integer): TColor;', @lpRGBToColor);
addGlobalFunc('procedure ColorToHSL(Color: Integer; var h, s, l: Extended);', @lpColorToHSL);
addGlobalFunc('function HSLToColor(H, S, L: Extended): TColor;', @lpHSLToColor);
addGlobalFunc('procedure ColorToXYZ(Color: Integer; var x, y, z: Extended);', @lpColorToXYZ);
addGlobalFunc('function XYZToColor(X, Y, Z: Extended): TColor;', @lpXYZToColor);
addGlobalFunc('procedure RGBToHSL(R, G, B: Integer; var h, s, l: Extended);', @lpRGBToHSL);
addGlobalFunc('procedure HSLtoRGB(H, S, L: extended; var R, G ,B: Integer);', @lpHSLToRGB);
addGlobalFunc('procedure RGBToXYZ(R, G, B: Integer;var x, y ,z: Extended);', @lpRGBToXYZ);
addGlobalFunc('procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Integer);', @lpXYZToRGB);

{ Color Finding }
addGlobalFunc('procedure SetColorToleranceSpeed(cts: integer);', @lpSetColorToleranceSpeed);
addGlobalFunc('function GetToleranceSpeed: Integer;', @lpGetToleranceSpeed);
addGlobalFunc('procedure SetToleranceSpeed2Modifiers(nHue, nSat: Extended);', @lpSetToleranceSpeed2Modifiers);
addGlobalFunc('procedure GetToleranceSpeed2Modifiers(var hMod, sMod: Extended);', @lpGetToleranceSpeed2Modifiers);
addGlobalFunc('function GetColor(x, y: Integer): Integer;', @lpGetColor);
addGlobalFunc('function GetColors(const Coords : TPointArray) : TIntegerArray;', @lpGetColors);
addGlobalFunc('procedure GetColorsWrap(Coords : TPointArray; var Colors :TIntegerArray);', @lpGetColorsWrap);
addGlobalFunc('function FindColor(var x, y: integer; color, xs, ys, xe, ye: integer): boolean;', @lpFindColor);
addGlobalFunc('function FindColorToleranceOptimised(var x, y: integer; color, xs, ys, xe, ye, tol: integer): boolean;', @lpfindcolortoleranceOptimised);
addGlobalFunc('function FindColorTolerance(var x, y: integer; color, xs, ys, xe, ye, tol: integer): boolean;', @lpFindColorTolerance);
addGlobalFunc('function FindColors(var TPA: TPointArray; color, xs, ys, xe, ye: integer): boolean;', @lpFindColors);
addGlobalFunc('function SimilarColors(Col1, Col2, Tolerance: integer): boolean', @lpSimilarColors);
addGlobalFunc('function CountColor(Color, xs, ys, xe, ye: Integer): Integer;', @lpCountColor);
addGlobalFunc('function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;', @lpCountColorTolerance);
addGlobalFunc('function FindColorsToleranceOptimised(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;', @lpFindColorsToleranceOptimised);
addGlobalFunc('function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;', @lpFindColorsTolerance);
addGlobalFunc('function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;', @lpFindColorSpiral);
addGlobalFunc('function FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye,Tol: Integer): Boolean;', @lpFindColorSpiralTolerance);
addGlobalFunc('function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;', @lpFindColorsSpiralTolerance);
addGlobalFunc('function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye, MinArea: Integer): Boolean', @lpFindColoredArea);
addGlobalFunc('function FindColoredAreaTolerance(var x, y : Integer; color, xs, ys, xe, ye, MinArea, Tolerance : Integer): Boolean', @lpFindColoredAreaTolerance);

{Mouse etc.}
addGlobalFunc('procedure MoveMouse(x, y: integer);', @lpMoveMouse);
addGlobalFunc('procedure Scrollmouse(x,y : integer; Clicks : integer);', @lpScrollMouse);
addGlobalFunc('procedure GetMousePos(var x, y: integer);', @lpGetMousePos);
addGlobalFunc('procedure HoldMouse(x, y: integer; clickType: integer);', @lpHoldMouse);
addGlobalFunc('procedure ReleaseMouse(x, y: integer; clickType: integer);', @lpReleaseMouse);
addGlobalFunc('procedure ClickMouse(x, y: integer; clickType: integer);', @lpClickMouse);
addGlobalFunc('function IsMouseButtonDown( button : integer) : boolean;', @lpIsMouseButtonDown);

{Keyboard}
addGlobalFunc('procedure KeyDown(key: Word);', @lpKeyDown);
addGlobalFunc('procedure KeyUp(key: Word);', @lpKeyUp);
addGlobalFunc('procedure PressKey(key: Word);', @lpPressKey);
addGlobalFunc('procedure SendKeys(const s: string);', @lpSendKeys);
addGlobalFunc('function IsKeyDown(key: Word): Boolean;', @lpisKeyDown);
addGlobalFunc('function GetKeyCode(c : char) : integer;', @lpGetKeyCode);

{OCR}
addGlobalFunc('function rs_GetUpText: string;', @lprs_GetUpText);
addGlobalFunc('function rs_GetUpTextAt(x, y : integer): string;', @lprs_GetUpTextAt);
addGlobalFunc('function rs_GetUpTextAtEx(x, y: integer; shadow: boolean): string', @lprs_GetUpTextAtEx);
addGlobalFunc('function BitmapFromText(const text, font: String): integer;', @lpBitmapFromText);
addGlobalFunc('function TPAFromText(const text, font: String;var w,h : integer): TPointArray;', @lpTPAFromText);
addGlobalFunc('procedure TPAFromTextWrap(const text, font: String;var w,h : integer;var TPA : TPointArray);', @lpTPAFromTextWrap);
addGlobalFunc('function MaskFromText(const text, font: String): TMask;', @lpMaskFromText);
addGlobalFunc('function GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing,color, tol, len: integer;const font: string): string;', @lpGetTextAt);
addGlobalFunc('function GetTextAtEx(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing,color, tol: integer;const font: string): string;', @lpgetTextAtEx);
addGlobalFunc('function GetTextAtExWrap(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing,color, tol: integer;const font: string): string;', @lpGetTextAtEx);
addGlobalFunc('function GetTextATPA(const ATPA : T2DPointArray; const maxvspacing : integer; const font : string): string;', @lpgettextATPA);
//addGlobalFunc('function LoadSystemFont(const SysFont : TFont; const FontName : string) : boolean;', @lpLoadSystemFont);
addGlobalFunc('function LoadFont(const FontName: string; shadow: boolean): boolean;', @lpLoadFont);
addGlobalFunc('function FreeFont(const FontName: string): boolean;', @lpFreeFont);

{Bitmaps}
addGlobalFunc('function CreateBitmapString(bmp: integer): string;', @lpCreateBitmapString);
//addGlobalFunc('function GetMufasaBitmap(bmp: integer): TMufasaBitmap;', @lpGetMufasaBitmap);
addGlobalFunc('function CreateBitmap(w, h:integer): integer;', @lpCreateBitmap);
addGlobalFunc('procedure FreeBitmap(Bmp : integer);', @lpFreeBitmap);
addGlobalFunc('procedure SaveBitmap(Bmp: integer; path: string);', @lpSaveBitmap);
addGlobalFunc('function BitmapFromString(Width, Height: integer; Data: string): integer;', @lpBitmapFromString);
addGlobalFunc('function LoadBitmap(Path: string): integer;', @lpLoadBitmap);
addGlobalFunc('procedure SetBitmapSize(Bmp, NewW, NewH: integer);', @lpSetBitmapSize);
addGlobalFunc('procedure GetBitmapSize(Bmp: integer; var BmpW, BmpH: integer);', @lpGetBitmapSize);
addGlobalFunc('procedure StretchBitmapResize(Bmp, NewW, NewH: integer);', @lpStretchBitmapResize);
addGlobalFunc('function CreateMirroredBitmap(Bmp: integer): integer;', @lpCreateMirroredBitmap);
addGlobalFunc('function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;', @lpCreateMirroredBitmapEx);
addGlobalFunc('procedure FastSetPixel(bmp, x, y: integer; Color: TColor);', @lpFastSetPixel);
addGlobalFunc('procedure FastSetPixels(bmp: integer; TPA: TPointArray; Colors: TIntegerArray);', @lpFastSetPixels);
addGlobalFunc('function FastGetPixel(bmp, x, y: integer): TColor;', @lpFastGetPixel);
addGlobalFunc('function FastGetPixels(Bmp: integer; TPA: TPointArray): TIntegerArray;', @lpFastGetPixels);
addGlobalFunc('function GetBitmapAreaColors(bmp, xs, ys, xe, ye: Integer): T2DIntegerArray;', @lpGetBitmapAreaColors);
addGlobalFunc('procedure FastDrawClear(bmp: integer; Color: TColor);', @lpFastDrawClear);
addGlobalFunc('procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);', @lpFastDrawTransparent);
addGlobalFunc('procedure SetTransparentColor(bmp : integer; Color: TColor);', @lpSetTransparentColor);
addGlobalFunc('function GetTransparentColor(bmp: integer): TColor;', @lpGetTransparentColor);
addGlobalFunc('procedure FastReplaceColor(Bmp: integer; OldColor, NewColor: TColor);', @lpFastReplaceColor);
addGlobalFunc('procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer);', @lpCopyClientToBitmap);
addGlobalFunc('function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer;', @lpBitmapFromClient);
addGlobalFunc('procedure SetBitmapName(Bmp: integer; name: string);', @lpSetBitmapName);
addGlobalFunc('function FindBitmap(bitmap: integer; var x, y: Integer): Boolean;', @lpFindBitmap);
addGlobalFunc('function FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;', @lpFindBitmapIn);
addGlobalFunc('function FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;', @lpFindBitmapToleranceIn);
addGlobalFunc('function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;', @lpFindBitmapSpiral);
addGlobalFunc('function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;', @lpFindBitmapsSpiralTolerance);
addGlobalFunc('function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;', @lpFindBitmapSpiralTolerance);
addGlobalFunc('function RotateBitmap(bitmap: Integer; angle: Extended): Integer;', @lpRotateBitmap);
addGlobalFunc('function DesaturateBitmap(Bitmap: integer): integer;', @lpDesaturate);
addGlobalFunc('procedure InvertBitmap(Bitmap: integer);', @lpInvertBitmap);
addGlobalFunc('function CopyBitmap(Bitmap:  integer): integer;', @lpCopyBitmap);
addGlobalFunc('function GreyScaleBitmap(bitmap: integer): integer', @lpGreyScaleBitmap);
addGlobalFunc('function BrightnessBitmap(Bitmap, br: integer): integer;', @lpBrightnessBitmap);
addGlobalFunc('function ContrastBitmap(bitmap: integer; co: extended): integer;', @lpContrastBitmap);
addGlobalFunc('function PosterizeBitmap(Bitmap: integer; po: integer): integer;', @lpPosterizeBitmap);
addGlobalFunc('function CreateMaskFromBitmap(Bitmap : integer) : TMask;', @lpCreateMaskFromBitmap);
addGlobalFunc('function FindMaskTolerance(const mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;', @lpFindMaskTolerance);
addGlobalFunc('function FindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;', @lpFindBitmapMaskTolerance);
addGlobalFunc('function FindDeformedBitmapToleranceIn(bitmap: integer; var x,y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;', @lpFindDeformedBitmapToleranceIn);
addGlobalFunc('procedure DrawTPABitmap(bitmap: integer; TPA: TPointArray; Color: integer);', @lpDrawTPABitmap);
addGlobalFunc('procedure DrawATPABitmap(bitmap: integer; ATPA: T2DPointArray);', @lpDrawATPABitmap);
addGlobalFunc('procedure DrawATPABitmapEx(bitmap: integer; ATPA: T2DPointArray; Colors: TIntegerArray);', @lpDrawATPABitmapEx);
//addGlobalFunc('procedure DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);', @lpDrawBitmap);
addGlobalFunc('procedure RectangleBitmap(bitmap: integer; const box: TBox; Color: TColor);', @lpRectangleBitmap);
addGlobalFunc('procedure FloodFillBitmap(bitmap: integer; const StartPoint: TPoint; const SearchCol, ReplaceCol: TColor);', @lpFloodfillBitmap);
addGlobalFunc('function ConvoluteBitmap(bitmap: integer; matrix: T2DExtendedArray): integer;', @lpConvoluteBitmap);
addGlobalFunc('function CalculatePixelShift(Bmp1, Bmp2: Integer; CompareBox: TBox): integer;', @lpCalculatePixelShift);
addGlobalFunc('function CalculatePixelTolerance(Bmp1, Bmp2: Integer; CompareBox: TBox; CTS: integer): extended;', @lpCalculatePixelTolerance);

{tpa}
{addGlobalFunc('procedure Quicksort(var Arr : TIntegerArray);', @lpQuicksort);
addGlobalFunc('procedure tSwap(var a, b: TPoint);', @lptSwap);
addGlobalFunc('procedure tpaSwap(var a, b: TPointArray);', @lptpaSwap);
addGlobalFunc('procedure SwapE(var a, b: Extended);', @lpSwapE);
addGlobalFunc('procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);', @lpRAaSTPAEx);
addGlobalFunc('procedure RAaSTPA(var a: TPointArray; const Dist: Integer);', @lpRAaSTPA);
addGlobalFunc('function NearbyPointInArrayEx(const P: TPoint; w, h:Integer;const  a: TPointArray): Boolean;', @lpNearbyPointInArrayEx);
addGlobalFunc('function NearbyPointInArray(const P: TPoint; Dist:Integer;const  a: TPointArray): Boolean;', @lpNearbyPointInArray);
addGlobalFunc('procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);', @lpQuickTPASort);
addGlobalFunc('procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);', @lpQuickATPASort);
addGlobalFunc('procedure SortTPAFrom(var a: TPointArray; const From: TPoint);', @lpSortTPAFrom);
addGlobalFunc('procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);', @lpSortATPAFrom);
addGlobalFunc('procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);', @lpSortATPAFromFirstPoint);
addGlobalFunc('procedure InvertTPA(var a: TPointArray);', @lpInvertTPA);
addGlobalFunc('procedure InvertATPA(var a: T2DPointArray);', @lpInvertATPA);
addGlobalFunc('function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;', @lpMiddleTPAEx);
addGlobalFunc('function MiddleTPA(const tpa: TPointArray): TPoint;', @lpMiddleTPA);
addGlobalFunc('procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);', @lpSortATPASize);
addGlobalFunc('procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);', @lpSortATPAFromSize);
addGlobalFunc('function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;', @lpInIntArrayEx);
addGlobalFunc('function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;', @lpInIntArray);
addGlobalFunc('procedure ClearSameIntegers(var a: TIntegerArray);', @lpClearSameIntegers);
addGlobalFunc('procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);', @lpClearSameIntegersAndTPA);
addGlobalFunc('function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;', @lpSplitTPAEx);
addGlobalFunc('function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;', @lpSplitTPA);
addGlobalFunc('function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;', @lpFloodFillTPA);
addGlobalFunc('procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);', @lpFilterPointsPie);
addGlobalFunc('procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);', @lpFilterPointsLine);
addGlobalFunc('procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Integer);', @lpfilterpointsdist);
addGlobalFunc('function GetATPABounds(const ATPA: T2DPointArray): TBox;', @lpGetATPABounds);
addGlobalFunc('function GetTPABounds(const TPA: TPointArray): TBox;', @lpGetTPABounds);
addGlobalFunc('function FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;', @lpFindTPAinTPA);
addGlobalFunc('function GetSamePointsATPA(const  ATPA : T2DPointArray; var Matches : TPointArray) : boolean;', @lpGetSamePointsATPA);
addGlobalFunc('function FindTextTPAinTPA(Height : integer;const  SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;', @lpFindTextTPAinTPA);
addGlobalFunc('procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);', @lpSortCircleWise);
addGlobalFunc('procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);', @lpLinearSort);
addGlobalFunc('function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;', @lpRotatePoint);
addGlobalFunc('function ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;', @lpChangeDistPT);
addGlobalFunc('function ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean;', @lpChangeDistTPA);
addGlobalFunc('function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;', @lpFindGapsTPA);
addGlobalFunc('function RemoveDistTPointArray(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;', @lpRemoveDistTPointArray);
addGlobalFunc('function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;', @lpCombineTPA);
addGlobalFunc('function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;', @lpReArrangeandShortenArrayEx);
addGlobalFunc('function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;', @lpReArrangeandShortenArray);
addGlobalFunc('function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;', @lpTPAtoATPAEx);
addGlobalFunc('function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;', @lpTPAtoATPA);
addGlobalFunc('function CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray;', @lpCombineIntArray);
addGlobalFunc('function MergeATPA(const ATPA : T2DPointArray)  : TPointArray;', @lpMergeATPA);
addGlobalFunc('procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);', @lpAppendTPA);
addGlobalFunc('function TPAFromBox(const Box : TBox) : TPointArray;', @lpTPAFromBox);
addGlobalFunc('function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;', @lpRotatePoints);
addGlobalFunc('function FindTPAEdges(const p: TPointArray): TPointArray;', @lpFindTPAEdges);
addGlobalFunc('function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray;', @lpClearTPAFromTPA);
addGlobalFunc('function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;', @lpReturnPointsNotInTPA);
addGlobalFunc('function PointInTPA(p: TPoint;const  arP: TPointArray): Boolean;', @lpPointInTPA);
addGlobalFunc('procedure ClearDoubleTPA(var TPA: TPointArray);', @lpClearDoubleTPA);
addGlobalFunc('procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);', @lpTPACountSort);
addGlobalFunc('procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);', @lpTPACountSortBase);
addGlobalFunc('procedure InvertTIA(var tI: TIntegerArray);', @lpInvertTIA);
addGlobalFunc('function SumIntegerArray(const Ints : TIntegerArray): Integer;', @lpSumIntegerArray);
addGlobalFunc('function AverageTIA(const tI: TIntegerArray): Integer;', @lpAverageTIA);
addGlobalFunc('function AverageExtended(const tE: TExtendedArray): Extended;', @lpAverageExtended);
addGlobalFunc('procedure SplitTPAExWrap(const arr: TPointArray; w, h: Integer; var res : T2DPointArray);', @lpSplitTPAExWrap);
addGlobalFunc('procedure SplitTPAWrap(const arr: TPointArray; Dist: Integer; var res: T2DPointArray);', @lpSplitTPAWrap);
addGlobalFunc('procedure FindGapsTPAWrap(const TPA: TPointArray; MinPixels: Integer; var Res : T2DPointArray);', @lpFindGapsTPAWrap);
addGlobalFunc('procedure RemoveDistTPointArrayWrap(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean; var Res :  TPointArray);', @lpRemoveDistTPointArrayWrap);
addGlobalFunc('procedure CombineTPAWrap(const Ar1, Ar2: TPointArray; var Res :  TPointArray);', @lpCombineTPAWrap);
addGlobalFunc('procedure ReArrangeandShortenArrayExWrap(const a: TPointArray; w, h: Integer; var Res :  TPointArray);', @lpReArrangeandShortenArrayExWrap);
addGlobalFunc('procedure ReArrangeandShortenArrayWrap(const a: TPointArray; Dist: Integer; var Res :  TPointArray);', @lpReArrangeandShortenArrayWrap);
addGlobalFunc('procedure TPAtoATPAExWrap(const TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray);', @lpTPAtoATPAExWrap);
addGlobalFunc('procedure TPAtoATPAWrap(const TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray);', @lpTPAtoATPAWrap);
addGlobalFunc('procedure CombineIntArrayWrap(const Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray);', @lpCombineIntArrayWrap);
addGlobalFunc('procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray);', @lpReturnPointsNotInTPAWrap);
addGlobalFunc('procedure MergeATPAWrap(const ATPA : T2DPointArray; var Res: TPointArray);', @lpMergeATPAWrap);
addGlobalFunc('procedure TPAFromBoxWrap(const Box : TBox; var Res : TPointArray);', @lpTPAFromBoxWrap);
addGlobalFunc('procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray);', @lpRotatePointsWrap);
addGlobalFunc('procedure FindTPAEdgesWrap(const p: TPointArray; var Res :  TPointArray);', @lpFindTPAEdgesWrap);
addGlobalFunc('procedure ClearTPAFromTPAWrap(const arP, ClearPoints: TPointArray;  var Res :  TPointArray);', @lpClearTPAFromTPAWrap);
addGlobalFunc('function SameTPA(const aTPA, bTPA: TPointArray): Boolean;', @lpSameTPA);
addGlobalFunc('function TPAInATPA(const TPA: TPointArray;const  InATPA: T2DPointArray; var Index: LongInt): Boolean;', @lpTPAInATPA);
addGlobalFunc('procedure OffsetTPA(var TPA : TPointArray; const Offset : TPoint);', @lpoffsetTPA);
addGlobalFunc('procedure OffsetATPA(var ATPA : T2DPointArray; const Offset : TPoint);', @lpoffsetATPA);
addGlobalFunc('function CopyTPA(const TPA : TPointArray) : TPointArray;', @lpcopyTPA);
addGlobalFunc('function CopyATPA(const ATPA : T2DPointArray) : T2DPointArray;', @lpCopyATPA);}

{settings}
addGlobalFunc('function KeyIsSetting(const KeyName: String): Boolean;', @lpKeyIsSetting);
addGlobalFunc('function KeyIsDirectory(const KeyName: String): Boolean;', @lpKeyIsDirectory);
addGlobalFunc('function GetSettingValue(const KeyName: String): String;', @lpGetSettingValue);
addGlobalFunc('function GetSettingValueDef(const KeyName, defVal: String): String;', @lpGetSettingValueDef);
addGlobalFunc('function SetSettingValue(const KeyName, value: string) : boolean;', @lpSetSettingValue);
addGlobalFunc('function ListSettings(const KeyName: String; var KeyReturn: TStringArray) : boolean;', @lpListSettings);
addGlobalFunc('function DeleteSetting(const KeyName: String): Boolean;', @lpDeleteSetting);
addGlobalFunc('function DeleteSubSettings(const KeyName: String): Boolean;', @lpDeleteSubSettings);

