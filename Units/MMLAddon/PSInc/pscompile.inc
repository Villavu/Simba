Sender.AddFunction(@ThreadSafeCall,'function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;');
Sender.AddFunction(@psWriteln,'procedure writeln(s : string);');
Sender.AddFunction(@FindColor, 'function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@GetClientDimensions, 'procedure GetClientDimensions(var w, h:integer);');
Sender.AddFunction(@CreateBitmap,'function CreateBitmap(w,h :integer) : integer;');
Sender.AddFunction(@FreeBitmap,'procedure FreeBitmap(Bmp : integer);');
Sender.AddFunction(@SaveBitmap,'procedure SaveBitmap(Bmp : integer; path : string);');
Sender.AddFunction(@BitmapFromString,'function BitmapFromString(Width,Height : integer; Data : string): integer;');
Sender.AddFunction(@LoadBitmap,'function LoadBitmap(Path : string) : integer;');
Sender.AddFunction(@SetBitmapSize,'procedure SetBitmapSize(Bmp,NewW,NewH : integer);');
Sender.AddFunction(@GetBitmapSize,'procedure GetBitmapSize(Bmp : integer; Var BmpW,BmpH : integer);');
Sender.AddFunction(@CreateMirroredBitmap,'function CreateMirroredBitmap(Bmp : integer) : integer;');

