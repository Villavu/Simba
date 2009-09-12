Sender.AddFunction(@ThreadSafeCall,'function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;');
Sender.AddFunction(@Writeln,'procedure writeln(s : string);');
Sender.AddFunction(@FindColor, 'function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@GetClientDimensions, 'procedure GetClientDimensions(var w, h:integer);');
