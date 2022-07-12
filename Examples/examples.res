        ��  ��                  �  @   ��
 A R R A Y   S O R T . S I M B A         	        // Will sort High to Low
function CustomSort(constref Left, Right: Integer): Integer;
begin
  if Left < Right then
    Result := 1
  else
    Result := -1;
end;

var
  Arr: TIntegerArray;
begin
  Arr := [6,3,1,5,1,0,10];
  Arr.Sort();
  WriteLn('Sorted using ">" and "<" operators: ', Arr);

  Arr := [6,3,1,5,1,0,10];
  Arr.Sort(@CustomSort);
  WriteLn('Sorted using CustomSort func: ', Arr);
end.   �  4   ��
 A R R A Y . S I M B A       	        var
  Arr: array of TPoint;
  P: TPoint;
  I: Integer;
begin
  Arr.SetLength(2);
  Arr[0] := [100, 100];
  Arr[1] := [200, 200];
  Arr.Append([400, 400]);

  I := Arr.IndexOf([200, 200]);
  WriteLn('Index of [200, 200] = ', I);

  WriteLn('For loop');
  for I := 0 to Arr.Length() - 1 do
    WriteLN(Arr[I]);

  WriteLn('For in loop');
  for P in Arr.Reversed() do
    WriteLn(P);
end.
 �  8   ��
 B I T M A P . S I M B A         	        var
  Bitmap: TMufasaBitmap;
begin
  Bitmap := TMufasaBitmap.Create(500, 500);
  Bitmap.DrawBox([100,100,400,400], $0000FF);
  Bitmap.DrawLine([50, 50], [450, 50], 5, $00FFFF);

  Bitmap.SetPixel(250, 250, $FFFFFF);
  WriteLn Bitmap.GetPixel(250, 250);

  Bitmap.SetFontSize(25);
  Bitmap.SetFontAntialiasing(False);
  Bitmap.DrawText('Hello world', [125, 125], $00FF00);

  Bitmap.Show();
  Bitmap.Free();
end.
   �  4   ��
 T I M E R . S I M B A       	        procedure Something;
var
  counter: Integer;
begin
  while (counter < 100) do
  begin
    Sleep(Random(50));
    Inc(counter);
  end;
end;

var
  Timer: TSimbaTimer;
  ms: Integer;
  str: String;
begin
  Timer.Start();
  Something();
  Timer.Stop();

  ms := Timer.Elapsed();
  str := Timer.ElapsedFmt('s');

  WriteLn('ms: ' + ToString(ms));
  WriteLN('seconds: ' + str);
end. 