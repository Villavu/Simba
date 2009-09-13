
Sender.Comp.AddTypeS('TIntegerArray', 'Array of integer');
Sender.Comp.AddTypeS('TPointArray','Array of TPoint');


Sender.AddFunction(@ThreadSafeCall,'function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;');
Sender.AddFunction(@psWriteln,'procedure writeln(s : string);');

Sender.AddFunction(@FindColor, 'function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@FindColorTolerance, 'function findcolortolerance(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
Sender.AddFunction(@SimilarColors,'function SimilarColors(Col1,Col2,Tolerance : integer) : boolean');

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
Sender.AddFunction(@FastSetPixel,'procedure FastSetPixel(bmp,x,y : integer; Color : TColor);');
Sender.AddFunction(@FastSetPixels,'procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);');
Sender.AddFunction(@FastGetPixel,'function FastGetPixel(bmp, x,y : integer) : TColor;');
Sender.AddFunction(@FastGetPixels,'function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;');
Sender.AddFunction(@FastDrawClear,'procedure FastDrawClear(bmp : integer; Color : TColor)');
Sender.AddFunction(@FastDrawTransparent,'procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);');
Sender.AddFunction(@SetTransparentColor,'procedure SetTransparentColor(bmp : integer; Color : TColor);');
Sender.AddFunction(@GetTransparentColor,'function GetTransparentColor(bmp: integer) : TColor;');
Sender.AddFunction(@FastReplaceColor,'procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);');







