unit uPSC_mml;
interface
uses
  uPSCompiler;

procedure SIRegister_MML(cl: TPSPascalCompiler);

implementation
uses mmltimer;
procedure SIRegister_TMufasaBitmap(cl : TPSPascalCompiler);
begin
  with cl.AddClassN(cl.FindClass('TObject'),'TMufasaBitmap') do
  begin;
    RegisterMethod('procedure SetSize(AWidth,AHeight : integer);');
    RegisterMethod('procedure StretchResize(AWidth,AHeight : integer);');
    RegisterMethod('procedure SetPersistentMemory(mem: PtrUInt; awidth, aheight: integer);');
    RegisterMethod('procedure ResetPersistentMemory;');
    RegisterMethod('procedure FastSetPixel(x,y : integer; Color : TColor);');
    RegisterMethod('procedure FastSetPixels(TPA : TPointArray; Colors : TIntegerArray);');
    RegisterMethod('procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);');
    RegisterMethod('procedure DrawTPA(TPA : TPointArray; Color : TColor);');
    RegisterMethod('procedure DrawToCanvas(x, y: Integer; Canvas: TCanvas);');
    RegisterMethod('function FastGetPixel(x,y : integer) : TColor;');
    RegisterMethod('procedure CopyClientToBitmap(IOManager : TObject; Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);');
    RegisterMethod('procedure Rectangle(const Box : TBox; FillCol : TColor);');
    RegisterMethod('procedure FloodFill(const StartPT : TPoint; const SearchCol,ReplaceCol : TColor);');
//      function FastGetPixels(TPA : TPointArray) : TIntegerArray;
    RegisterMethod('procedure SetTransparentColor(Col : TColor);');
    RegisterMethod('function GetTransparentColor : TColor;');
    RegisterProperty('TransparentColorSet','Boolean',iptR);
    RegisterMethod('procedure FastDrawClear(Color : TColor);');
    RegisterMethod('procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);');
    RegisterMethod('procedure FastReplaceColor(OldColor, NewColor: TColor);');
    RegisterMethod('procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );');
    RegisterMethod('procedure Desaturate(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure GreyScale(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer);');
    RegisterMethod('procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);');
    RegisterMethod('procedure Invert(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);');
    RegisterMethod('function Copy(const xs,ys,xe,ye : integer) : TMufasaBitmap;');
    RegisterMethod('function ToString : string;');
    RegisterMethod('function ToTBitmap : TBitmap;');
    RegisterMethod('function CreateTMask : TMask;');
    RegisterMethod('constructor Create;');
    RegisterMethod('procedure Free;');
    RegisterMethod('function SaveToFile(const FileName : string) :boolean;');
    RegisterMethod('procedure LoadFromFile(const FileName : string);');
    RegisterMethod('procedure LoadFromTBitmap(bmp: TBitmap);');
    RegisterProperty('Width','Integer',iptR);
    RegisterProperty('Height','Integer',iptR);
    RegisterProperty('Index','Integer',iptR);
    RegisterProperty('Name','String',iptRW);
    RegisterProperty('FData','PtrUInt',iptR);
  end;
end;

procedure SIRegister_TRegExp(cl : TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('Exception'),'ERegExpr') do
  begin
    RegisterProperty('ErrorCode', 'integer', iptrw);
    RegisterProperty('CompilerErrorPos', 'integer', iptrw);
  end;
  with cl.AddClassN(cl.FindClass('TObject'),'TRegExp') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Function VersionMajor : integer');
    RegisterMethod('Function VersionMinor : integer');
    RegisterProperty('Expression', 'String', iptrw);
    RegisterProperty('ModifierStr', 'String', iptrw);
    RegisterProperty('ModifierI', 'boolean', iptrw);
    RegisterProperty('ModifierR', 'boolean', iptrw);
    RegisterProperty('ModifierS', 'boolean', iptrw);
    RegisterProperty('ModifierG', 'boolean', iptrw);
    RegisterProperty('ModifierM', 'boolean', iptrw);
    RegisterProperty('ModifierX', 'boolean', iptrw);
    RegisterMethod('Function Exec( const AInputString : String) : boolean;');
    RegisterMethod('Function ExecNext : boolean');
    RegisterMethod('Function ExecPos( AOffset : integer) : boolean');
    RegisterProperty('InputString', 'String', iptrw);
    RegisterMethod('Function Substitute( const ATemplate : String) : String');
    RegisterMethod('Procedure Split( AInputStr : String; APieces : TStrings)');
    RegisterMethod('Function Replace( AInputStr : String; const AReplaceStr : String; AUseSubstitution : boolean) : String;');
    RegisterProperty('SubExprMatchCount', 'integer', iptr);
    RegisterProperty('MatchPos', 'integer integer', iptr);
    RegisterProperty('MatchLen', 'integer integer', iptr);
    RegisterProperty('Match', 'String integer', iptr);
    RegisterMethod('Function LastError : integer');
    RegisterMethod('Function ErrorMsg( AErrorID : integer) : String');
    RegisterProperty('CompilerErrorPos', 'integer', iptr);
    RegisterProperty('SpaceChars', 'String', iptrw);
    RegisterProperty('WordChars', 'String', iptrw);
    RegisterProperty('LineSeparators', 'String', iptrw);
    RegisterProperty('LinePairedSeparator', 'String', iptrw);
    RegisterMethod('Function InvertCaseFunction( const Ch : Char) : Char');
    RegisterProperty('InvertCase', 'TRegExprInvertCaseFunction', iptrw);
    RegisterMethod('Procedure Compile');
    RegisterMethod('Function Dump : String');
  end;
end;

procedure SIRegister_TMDTM(cl : TPSPascalCompiler);
begin
  with cl.AddClassN(cl.FindClass('TObject'),'TMDTM') do
  begin
    RegisterMethod('constructor Create;');
    RegisterMethod('procedure Free;');
    RegisterProperty('Name','String',iptrw);
    RegisterMethod('function ToString : string');
    RegisterMethod('function LoadFromString(const s : string) : boolean;');
    RegisterMethod('procedure Normalize;');
    RegisterMethod('function Valid: boolean');
    RegisterMethod('procedure DeletePoint(Point: integer);');
    RegisterMethod('procedure SwapPoint(P1, P2: integer);');
    RegisterMethod('procedure MovePoint(fromIndex,toIndex : integer);');
    RegisterMethod('function AddPoint(Point : TMDTMPoint): integer;');
    RegisterProperty('Count','Integer',iptrw);
    RegisterProperty('Index','Integer',iptr);
    RegisterProperty('Points','TMDTMPointArray',iptr);
  end;
end;

procedure SIRegister_TMMLSettingsSandbox(CL : TPSPascalCompiler);
begin
  with cl.AddClassN(nil,'TMMLSettingsSandbox') do
  begin;
    RegisterMethod('function IsKey(const KeyName: String): Boolean;');
    RegisterMethod('function IsDirectory(const KeyName: String): Boolean;');
    RegisterMethod('function SetKeyValue(const Keyname, Value : string) : boolean;');
    RegisterMethod('function GetKeyValue(const KeyName: String): String;');
    RegisterMethod('function GetKeyValueDef(const KeyName, defVal: String): String;');
    RegisterMethod('function ListKeys(const KeyName: String; out Keys :TStringArray): boolean;');
    RegisterMethod('function DeleteKey(const KeyName: String): Boolean;');
    RegisterMethod('function DeleteSubKeys(const KeyName: String): Boolean;');
    RegisterProperty('Prefix','String',iptR);
  end;
end;

procedure SIRegister_TMDTMS(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TMDTMS') do
  begin
    RegisterMethod('Function AddSDTM( const d : TSDTM) : Integer;');
    RegisterMethod('Function AddMDTM( const d : TMDTM) : Integer;');
    RegisterMethod('Function GetDTM( index : Integer) : TMDTM');
    RegisterMethod('Procedure FreeDTM( DTM : Integer)');
    RegisterMethod('Function StringToDTM( const S : String) : Integer');
    RegisterProperty('DTM', 'TMDTM integer', iptr);
    SetDefaultPropery('DTM');
    RegisterMethod('Constructor Create( Owner : TObject)');
  end;
end;

procedure SIRegister_TMFinder(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TMFinder') do
  begin
    RegisterProperty('WarnOnly', 'boolean', iptrw);
    RegisterMethod('Procedure DefaultOperations( var xs, ys, xe, ye : integer)');
    RegisterMethod('Function CountColorTolerance( Color, xs, ys, xe, ye, Tolerance : Integer) : Integer');
    RegisterMethod('Function CountColor( Color, xs, ys, xe, ye : Integer) : Integer');
    RegisterMethod('Function SimilarColors( Color1, Color2, Tolerance : Integer) : boolean');
    RegisterMethod('Function FindColor( out x, y : Integer; Color, xs, ys, xe, ye : Integer) : Boolean');
    RegisterMethod('Function FindColorSpiral( var x, y : Integer; color, xs, ys, xe, ye : Integer) : Boolean');
    RegisterMethod('Function FindColorSpiralTolerance( var x, y : Integer; color, xs, ys, xe, ye, Tol : Integer) : Boolean');
    RegisterMethod('Function FindColorTolerance( out x, y : Integer; Color, xs, ys, xe, ye, tol : Integer) : Boolean');
    RegisterMethod('Function FindColorsTolerance( out Points : TPointArray; Color, xs, ys, xe, ye, Tol : Integer) : Boolean');
    RegisterMethod('Function FindColorsSpiralTolerance( x, y : Integer; out Points : TPointArray; color, xs, ys, xe, ye : Integer; Tolerance : Integer) : boolean');
    RegisterMethod('Function FindColors( var TPA : TPointArray; Color, xs, ys, xe, ye : Integer) : Boolean');
    RegisterMethod('Function FindColoredArea( var x, y : Integer; color, xs, ys, xe, ye : Integer; MinArea : Integer) : Boolean');
    RegisterMethod('Function FindColoredAreaTolerance( var x, y : Integer; color, xs, ys, xe, ye : Integer; MinArea, tol : Integer) : Boolean');
    RegisterMethod('Function FindMaskTolerance( const mask : TMask; out x, y : Integer; xs, ys, xe, ye : Integer; Tolerance, ContourTolerance : Integer) : Boolean');
    RegisterMethod('Procedure CheckMask( const Mask : TMask)');
    RegisterMethod('Function FindBitmap( bitmap : TMufasaBitmap; out x, y : Integer) : Boolean');
    RegisterMethod('Function FindBitmapIn( bitmap : TMufasaBitmap; out x, y : Integer; xs, ys, xe, ye : Integer) : Boolean');
    RegisterMethod('Function FindBitmapToleranceIn( bitmap : TMufasaBitmap; out x, y : Integer; xs, ys, xe, ye : Integer; tolerance : Integer) : Boolean');
    RegisterMethod('Function FindBitmapSpiral( bitmap : TMufasaBitmap; var x, y : Integer; xs, ys, xe, ye : Integer) : Boolean');
    RegisterMethod('Function FindBitmapSpiralTolerance( bitmap : TMufasaBitmap; var x, y : Integer; xs, ys, xe, ye, tolerance : integer) : Boolean');
    RegisterMethod('Function FindBitmapsSpiralTolerance( bitmap : TMufasaBitmap; x, y : Integer; out Points : TPointArray; xs, ys, xe, ye, tolerance : Integer) : Boolean');
    RegisterMethod('Function FindDeformedBitmapToleranceIn( bitmap : TMufasaBitmap; out x, y : Integer; xs, ys, xe, ye : Integer; tolerance : Integer; Range : Integer; AllowPartialAccuracy : Boolean; out accuracy : Extended) : Boolean');
    RegisterMethod('Function FindDTM( DTM : TMDTM; out x, y : Integer; x1, y1, x2, y2 : Integer) : Boolean');
    RegisterMethod('Function FindDTMs( DTM : TMDTM; out Points : TPointArray; x1, y1, x2, y2 : integer; maxToFind : Integer) : Boolean');
    RegisterMethod('Function FindDTMRotated( DTM : TMDTM; out x, y : Integer; x1, y1, x2, y2 : Integer; sAngle, eAngle, aStep : Extended; out aFound : Extended; Alternating : boolean) : Boolean');
    RegisterMethod('Function FindDTMsRotated( DTM : TMDTM; out Points : TPointArray; x1, y1, x2, y2 : Integer; sAngle, eAngle, aStep : Extended; out aFound : T2DExtendedArray; Alternating : boolean; maxToFind : Integer) : Boolean');
    RegisterMethod('Function GetColors( const Coords : TPointArray) : TIntegerArray');
    RegisterMethod('Procedure SetToleranceSpeed( nCTS : Integer)');
    RegisterMethod('Function GetToleranceSpeed : Integer');
    RegisterMethod('Procedure SetToleranceSpeed2Modifiers( const nHue, nSat : Extended)');
    RegisterMethod('Procedure GetToleranceSpeed2Modifiers( out hMod, sMod : Extended)');
    RegisterMethod('Constructor Create( aClient : TObject)');
  end;
end;

procedure SIRegister_TMBitmaps(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TMBitmaps') do
  begin
    RegisterMethod('Function GetBMP( Index : integer) : TMufasaBitmap');
    RegisterProperty('Bmp', 'TMufasaBitmap integer', iptr);
    SetDefaultPropery('Bmp');
    RegisterMethod('Function CreateBMP( w, h : integer) : Integer');
    RegisterMethod('Function AddBMP( _bmp : TMufasaBitmap) : Integer');
    RegisterMethod('Function CopyBMP( Bitmap : integer) : Integer');
    RegisterMethod('Function CreateMirroredBitmap( bitmap : Integer; MirrorStyle : TBmpMirrorStyle) : Integer');
    RegisterMethod('Function CreateBMPFromFile( const Path : string) : integer');
    RegisterMethod('Function CreateBMPFromString( width, height : integer; Data : string) : integer;');
    RegisterMethod('Procedure FreeBMP( Number : integer)');
    RegisterMethod('Constructor Create( Owner : TObject)');
  end;
end;

procedure SIRegister_TTarget(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TTarget') do
  begin
    RegisterMethod('Procedure GetTargetDimensions( var w, h : integer)');
    RegisterMethod('Function GetColor( x, y : integer) : TColor');
    RegisterMethod('Function ReturnData( xs, ys, width, height : Integer) : TRetData');
    RegisterMethod('Procedure FreeReturnData');
    RegisterMethod('Procedure ActivateClient');
    RegisterMethod('Function TargetValid : boolean');
    RegisterMethod('Function GetError : String');
    RegisterMethod('Function ReceivedError : Boolean');
    RegisterMethod('Procedure ResetError');
    RegisterMethod('Procedure GetMousePosition( var x, y : integer)');
    RegisterMethod('Procedure MoveMouse( x, y : integer)');
    RegisterMethod('Procedure ScrollMouse( x, y : integer; Lines : integer)');
    RegisterMethod('Procedure HoldMouse( x, y : integer; button : TClickType)');
    RegisterMethod('Procedure ReleaseMouse( x, y : integer; button : TClickType)');
    RegisterMethod('Function IsMouseButtonHeld( button : TClickType) : boolean');
    RegisterMethod('Procedure SendString( str : string; keywait: integer)');
    RegisterMethod('Procedure HoldKey( key : integer)');
    RegisterMethod('Procedure ReleaseKey( key : integer)');
    RegisterMethod('Function IsKeyHeld( key : integer) : boolean');
    RegisterMethod('Function GetKeyCode( C : char) : integer');
  end;
end;

procedure SIRegister_TRawTarget(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TTarget'),'TRawTarget') do
  begin
    RegisterMethod('Constructor Create( rgb : Integer; w, h : integer; CopyData : boolean)');
  end;
end;

procedure SIRegister_TBitmapTarget(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TTarget'),'TBitmapTarget') do
  begin
    RegisterMethod('Constructor Create( bitmap : TMufasaBitmap)');
  end;
end;

procedure SIRegister_TWindow_Abstract(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TTarget'),'TWindow_Abstract') do
  begin
  end;
end;

procedure SIRegister_TEIOS_Target(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TTarget'),'TEIOS_Target') do
  begin
    RegisterMethod('Constructor Create( client : TEIOS_Client; initval : pointer)');
  end;
end;

procedure SIRegister_TWindow(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TWindow_Abstract'),'TWindow') do
  begin
    {$ifdef mswindows}
    RegisterMethod('Constructor Create(target : Hwnd)');
    {$endif}
    RegisterMethod('Function GetNativeWindow : TNativeWindow');
  end;
end;

procedure SIRegister_TIOManager_Abstract(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TIOManager_Abstract') do
  begin
    RegisterMethod('Constructor Create( plugin_dir : string)');
    RegisterMethod('Function GetError : String');
    RegisterMethod('Function ReceivedError : Boolean');
    RegisterMethod('Procedure ResetError');
    RegisterMethod('Procedure SetDesktop');
    RegisterMethod('Function SetTargetArray( ArrPtr : Integer; Size : TPoint) : integer;');
    RegisterMethod('Function SetTargetBitmap( bmp : TMufasaBitmap) : integer;');
    RegisterMethod('Function TargetValid : Boolean');
    RegisterMethod('Procedure BitmapDestroyed( Bitmap : TMufasaBitmap)');
    RegisterMethod('Function GetColor( x, y : integer) : TColor');
    RegisterMethod('Function ReturnData( xs, ys, width, height : Integer) : TRetData');
    RegisterMethod('Procedure FreeReturnData');
    RegisterMethod('Procedure GetDimensions( var W, H : Integer)');
    RegisterMethod('Procedure ActivateClient');
    RegisterMethod('Function IsFrozen : boolean');
    RegisterMethod('Procedure SetFrozen( makefrozen : boolean)');
    RegisterMethod('Procedure GetMousePos( var X, Y : Integer)');
    RegisterMethod('Procedure MoveMouse( X, Y : Integer)');
    RegisterMethod('Procedure ScrollMouse( x, y : integer; Lines : integer)');
    RegisterMethod('Procedure HoldMouse( x, y : integer; button : TClickType)');
    RegisterMethod('Procedure ReleaseMouse( x, y : integer; button : TClickType)');
    RegisterMethod('Procedure ClickMouse( X, Y : Integer; button : TClickType)');
    RegisterMethod('Function IsMouseButtonDown( button : TClickType) : boolean');
    RegisterMethod('Procedure KeyUp( key : Word)');
    RegisterMethod('Procedure KeyDown( key : Word)');
    RegisterMethod('Procedure PressKey( key : Word)');
    RegisterMethod('Procedure SendText( text : string; keywait: integer)');
    RegisterMethod('Function isKeyDown( key : Word) : Boolean');
    RegisterMethod('Function GetKeyCode( c : char) : integer');
    RegisterMethod('Function GetImageTarget : TTarget;');
    RegisterMethod('Function GetKeyMouseTarget : TTarget;');
    RegisterMethod('Function ExportImageTarget : TTarget_Exported;');
    RegisterMethod('Function ExportKeyMouseTarget : TTarget_Exported;');
    RegisterMethod('Procedure GetImageTarget( var idx : integer);');
    RegisterMethod('Procedure GetKeyMouseTarget( var idx : integer);');
    RegisterMethod('Procedure SetImageTarget( idx : integer)');
    RegisterMethod('Procedure SetKeyMouseTarget( idx : integer)');
    RegisterMethod('Procedure FreeTarget( idx : integer)');
    RegisterMethod('Procedure SetState( val : Boolean)');
  end;
end;

procedure SIRegister_TIOManager(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TIOManager_Abstract'),'TIOManager') do
  begin
    RegisterMethod('Constructor Create( plugin_dir : string)');
    RegisterMethod('procedure SetDesktop;');
    RegisterMethod('Function SetTarget( target : TNativeWindow) : integer;');
  end;
end;

procedure SIRegister_TMMLTimer(CL: TPSPascalCompiler);
begin
CL.AddTypeS('TThreadPriority', '(tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, tpTimeCritical)');
  with CL.AddClassN(CL.FindClass('TObject'),'TMMLTimer') do
  begin
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('Interval', 'Integer', iptrw);
    RegisterProperty('OnTimer', 'TNotifyEvent', iptrw);
    RegisterProperty('ThreadPriority', 'TThreadPriority', iptrw);
    RegisterMethod('constructor Create');
    RegisterMethod('destructor Destroy');
    RegisterMethod('Procedure On');
    RegisterMethod('Procedure Off');
  end;
end;

procedure SIRegister_IOManager(CL: TPSPascalCompiler);
begin
  SIRegister_TTarget(CL);
  SIRegister_TRawTarget(CL);
  SIRegister_TBitmapTarget(CL);
  SIRegister_TWindow_Abstract(CL);
  SIRegister_TEIOS_Target(CL);
  SIRegister_TWindow(cl);
  SIRegister_TIOManager_Abstract(CL);
  SIRegister_TIOManager(cl);
end;

procedure SIRegister_TClient(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TClient') do
  begin
    RegisterProperty('IOManager', 'TIOManager', iptrw);
    RegisterProperty('MFiles', 'TMFiles', iptrw);
    RegisterProperty('MFinder', 'TMFinder', iptrw);
    RegisterProperty('MBitmaps', 'TMBitmaps', iptrw);
    RegisterProperty('MDTMs', 'TMDTMS', iptrw);
    RegisterProperty('MOCR', 'TMOCR', iptrw);
    RegisterProperty('WritelnProc', 'TWritelnProc', iptrw);
    RegisterMethod('Procedure WriteLn( s : string)');
    RegisterMethod('Constructor Create( const plugin_dir : string; const UseIOManager : TIOManager)');
  end;
end;

procedure SIRegister_MML(cl: TPSPascalCompiler);
begin
  SIRegister_TMufasaBitmap(cl);
  SIRegister_TRegExp(cl);
  SIRegister_TMDTM(cL);
  SIRegister_TMMLSettingsSandbox(cl);
  SIRegister_TMDTMS(cl);
  SIRegister_TMFinder(cl);
  SIRegister_TMBitmaps(cl);
  SIRegister_IOManager(cl);
  SIRegister_TClient(cl);
  SIRegister_TMMLTimer(cl);
end;

end.
