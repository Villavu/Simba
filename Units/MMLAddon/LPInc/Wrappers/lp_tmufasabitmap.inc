procedure Lape_TMufasaBitmap_LoadFromFile(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.LoadFromFile(PString(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PBoolean(Result)^ := Self.SaveToFile(PString(Params^[1])^);
end;

(*
TODO:

procedure SetSize(AWidth,AHeight : integer);
procedure StretchResize(AWidth,AHeight : integer);
procedure SetPersistentMemory(mem: PtrUInt; awidth, aheight: integer);
procedure ResetPersistentMemory;
procedure FastSetPixel(x,y : integer; Color : TColor);
procedure FastSetPixels(TPA : TPointArray; Colors : TIntegerArray);
procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);
procedure DrawTPA(TPA : TPointArray; Color : TColor);
procedure DrawToCanvas(x, y: Integer; Canvas: TCanvas);
function FastGetPixel(x,y : integer) : TColor;
procedure CopyClientToBitmap(IOManager : TObject; Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);
procedure Rectangle(const Box : TBox; FillCol : TColor);
procedure FloodFill(const StartPT : TPoint; const SearchCol,ReplaceCol : TColor);
procedure SetTransparentColor(Col : TColor);
function GetTransparentColor : TColor;
procedure FastDrawClear(Color : TColor);
procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);
procedure FastReplaceColor(OldColor, NewColor: TColor);
procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
procedure Desaturate(TargetBitmap : TMufasaBitmap);
procedure GreyScale(TargetBitmap : TMufasaBitmap);
procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer);
procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);
procedure Invert(TargetBitmap : TMufasaBitmap);
procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);
function Copy(const xs,ys,xe,ye : integer) : TMufasaBitmap;
function ToString : string;
function ToTBitmap : TBitmap;
function CreateTMask : TMask;
constructor Create;
procedure Free;
procedure LoadFromTBitmap(bmp: TBitmap);
RegisterProperty('TransparentColorSet','Boolean',iptR);
RegisterProperty('Width','Integer',iptR);
RegisterProperty('Height','Integer',iptR);
RegisterProperty('Index','Integer',iptR);
RegisterProperty('Name','String',iptRW);
RegisterProperty('FData','PtrUInt',iptR);
*)
