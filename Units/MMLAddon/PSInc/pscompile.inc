
Sender.Comp.AddTypeS('TIntegerArray', 'Array of integer');
Sender.Comp.AddTypeS('TPointArray','Array of TPoint');
Sender.Comp.AddTypeS('TBmpMirrorStyle','(MirrorWidth,MirrorHeight,MirrorLine)');


Sender.Comp.AddTypes('TDTMPointDef', 'record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
Sender.Comp.AddTypes('TDTMPointDefArray', 'Array Of TDTMPointDef;');
Sender.Comp.AddTypes('TDTM','record MainPoint: TDTMPointDef; SubPoints: TDTMPointDefArray; end;');
Sender.Comp.AddTypeS('pDTM','record p: TPointArray; c, t, asz, ash: TIntegerArray; end');
Sender.Comp.AddTypeS('T2DExtendedArray', 'array of array of extended');

Sender.AddFunction(@ThreadSafeCall,'function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;');
Sender.AddFunction(@psWriteln,'procedure writeln(s : string);');

{ DTM }

Sender.AddFunction(@PrintpDTM, 'Procedure PrintpDTM(tDTM : pDTM);');
Sender.AddFunction(@ps_GetDTM ,'function GetDTM(index: Integer; var dtm: pDTM): Boolean;');
Sender.AddFunction(@pDTMToTDTM, 'Function pDTMToTDTM(Const DTM: pDTM): TDTM;');
Sender.AddFunction(@tDTMTopDTM, 'Function tDTMTopDTM(Const DTM: TDTM): pDTM;');
Sender.AddFunction(@ps_DTMFromString, 'function DTMFromString(DTMString: String): Integer;');
Sender.AddFunction(@ps_FreeDTM, 'procedure FreeDTM(DTM: Integer);');
Sender.AddFunction(@ps_FindDTM, 'function FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;');
Sender.AddFunction(@ps_FindDTMs, 'function FindDTMs(DTM: Integer; var p: TPointArray; x1, y1, x2, y2: Integer): Boolean;');
Sender.AddFunction(@ps_FindDTMRotated, 'function FindDTMRotated(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean;');
Sender.AddFunction(@ps_FindDTMsRotated, 'function FindDTMsRotated(DTM: Integer; var Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;');
Sender.AddFunction(@ps_addDTM, 'function AddDTM(d: TDTM): Integer;');
Sender.AddFunction(@ps_addpDTM, 'function AddpDTM(d: pDTM): Integer;');

{maths}
sender.AddFunction(@power,'function pow(base,exponent : extended) : extended');
Sender.AddFunction(@max,'function Max(a, b: Integer): Integer;');
Sender.AddFunction(@min,'function Min(a, b: Integer): Integer;');
Sender.AddFunction(@pssqr,'function Sqr(e : extended) : extended;');
Sender.AddFunction(@point,'function Point(x,y:integer) : TPoint;');

Sender.AddFunction(@Freeze, 'function freeze:boolean;');
Sender.AddFunction(@Unfreeze, 'function unfreeze: boolean;');

Sender.AddFunction(@GetColor,'function GetColor(x, y: Integer): Integer;');
Sender.AddFunction(@FindColor, 'function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@FindColorTolerance, 'function findcolortolerance(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
Sender.AddFunction(@FindColors, 'function findcolors(var TPA: TPointArray; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@SimilarColors,'function SimilarColors(Col1,Col2,Tolerance : integer) : boolean');
Sender.AddFunction(@CountColorTolerance,'function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;');
Sender.AddFunction(@FindColorsTolerance,'function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');

Sender.AddFunction(@MoveMouse, 'procedure MoveMouse(x, y: integer);');
Sender.AddFunction(@GetMousePos, 'procedure GetMousePos(var x, y: integer);');

Sender.AddFunction(@Wait, 'procedure wait(t: integer);');
Sender.AddFunction(@GetClientDimensions, 'procedure GetClientDimensions(var w, h:integer);');
Sender.AddFunction(@SetColorToleranceSpeed, 'procedure SetColorToleranceSpeed(cts: integer);');
Sender.AddFunction(@GetTickCount, 'function GetSystemTime: Integer;');

Sender.AddFunction(@CreateBitmap,'function CreateBitmap(w,h :integer) : integer;');
Sender.AddFunction(@FreeBitmap,'procedure FreeBitmap(Bmp : integer);');
Sender.AddFunction(@SaveBitmap,'procedure SaveBitmap(Bmp : integer; path : string);');
Sender.AddFunction(@BitmapFromString,'function BitmapFromString(Width,Height : integer; Data : string): integer;');
Sender.AddFunction(@LoadBitmap,'function LoadBitmap(Path : string) : integer;');
Sender.AddFunction(@SetBitmapSize,'procedure SetBitmapSize(Bmp,NewW,NewH : integer);');
Sender.AddFunction(@GetBitmapSize,'procedure GetBitmapSize(Bmp : integer; Var BmpW,BmpH : integer);');
Sender.AddFunction(@CreateMirroredBitmap,'function CreateMirroredBitmap(Bmp : integer) : integer;');
Sender.AddFunction(@CreateMirroredBitmapEx,'function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;');
Sender.AddFunction(@FastSetPixel,'procedure FastSetPixel(bmp,x,y : integer; Color : TColor);');
Sender.AddFunction(@FastSetPixels,'procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);');
Sender.AddFunction(@FastGetPixel,'function FastGetPixel(bmp, x,y : integer) : TColor;');
Sender.AddFunction(@FastGetPixels,'function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;');
Sender.AddFunction(@FastDrawClear,'procedure FastDrawClear(bmp : integer; Color : TColor)');
Sender.AddFunction(@FastDrawTransparent,'procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);');
Sender.AddFunction(@SetTransparentColor,'procedure SetTransparentColor(bmp : integer; Color : TColor);');
Sender.AddFunction(@GetTransparentColor,'function GetTransparentColor(bmp: integer) : TColor;');
Sender.AddFunction(@FastReplaceColor,'procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);');







