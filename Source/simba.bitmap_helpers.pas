{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Extends TMufasaBitmap with features from other Simba units.
}
unit simba.bitmap_helpers;

{$i simba.inc}

interface

uses
  classes, sysutils,
  neuralnetwork, neuralvolume, neuraldatasets,
  simba.mufasatypes, simba.matchtemplate, simba.bitmap;

type
  TMufasaBitmapHelpers = class helper for TMufasaBitmap
  public
    function MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
    function MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;

    function FindColors(out Points: TPointArray; Color: Integer): Boolean; overload;
    function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean; overload;
    function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended): Boolean; overload;

    function FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;
    function FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;

    function ClassifyBitmap(out labels: TStringArray; out scores: TSingleArray; out Best: String): Boolean;
  end;

implementation

uses
  simba.finder_color, simba.finder_bitmap, Graphics;

function TMufasaBitmapHelpers.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
begin
  Result := simba.matchtemplate.MatchTemplate(Self.ToMatrixBGR(), Template.ToMatrixBGR(), Formula);
end;

function TMufasaBitmapHelpers.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
begin
  Result := simba.matchtemplate.MatchTemplateMask(Self.ToMatrixBGR(), Template.ToMatrixBGR(), Formula);
end;

function TMufasaBitmapHelpers.FindColors(out Points: TPointArray; Color: Integer): Boolean;
var
  Buffer: TFindColorBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.Find(Points, Color);
end;

function TMufasaBitmapHelpers.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean;
var
  Buffer: TFindColorBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.FindCTS1(Points, Color, Tolerance);
end;

function TMufasaBitmapHelpers.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended): Boolean;
var
  Buffer: TFindColorBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.FindCTS2(Points, Color, Tolerance, HueMod, SatMod);
end;

function TMufasaBitmapHelpers.FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;
var
  Buffer: TFindBitmapBuffer;
  Points: TPointArray;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.Find(Bitmap, Points, Tolerance);
  if Result then
  begin
    X := Points[0].X;
    Y := Points[0].Y;
  end;
end;

function TMufasaBitmapHelpers.FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;
var
  Buffer: TFindBitmapBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.Find(Bitmap, Points, Tolerance);
end;

function TMufasaBitmapHelpers.ClassifyBitmap(out labels: TStringArray; out scores: TSingleArray; out Best: String): Boolean;
var
  Copy: TMufasaBitmap;
  TI: TTinyImage;
  MaxScore: Single;
  i: Integer;
  NNLoaded: Boolean = False;
  NN: TNNet;
  NNLabels: TStringArray;
  InputV, OutputV: TNNetVolume;

  procedure LoadNeuralNet;
  var
    LabelFile: TextFile;
    Line: String;
  begin
    NN := TNNet.Create;
    NN.LoadFromFile('./Third-Party/neural-api/Cifar100CaiDenseNet.nn');
    InputV := TNNetVolume.Create;
    OutputV := TNNetVolume.Create;
    SetLength(NNLabels, 0);
    AssignFile(LabelFile, './Third-Party/neural-api/fine_label_names.txt');

    try
      reset(LabelFile);
      repeat
        ReadLn(LabelFile, line);
        Insert(line, NNLabels, Length(NNLabels));
      until eof(LabelFile);
    finally
      Close(LabelFile);
    end;

    NNLoaded := True;
  end;

  procedure LoadBitmapIntoTinyImage(const Bitmap: TMufasaBitmap; out TI: TTinyImage);
  const
    useStretchDraw: Boolean = True;
  var
    I, J: integer;
    r,g,b: byte;
    Bmp: TBitmap;
    Tmp: TBitmap;
    Copy: TMufasaBitmap;
  begin

    if useStretchDraw then
    begin
        Tmp := Bitmap.ToTBitmap;
        Bmp := TBitmap.Create;
        Bmp.SetSize(32,32);
        Bmp.Canvas.StretchDraw(TRect.Create(0,0,32,32), Tmp);
    end
    else
    begin
      Copy := Self.Copy;
      Copy.ResizeBilinear(32,32);
      Bmp := Copy.ToTBitmap;
    end;

    try
      for I := 0 to 31 do
      begin
        for J := 0 to 31 do
        begin
          {TI.R[I, J] := Bitmap.FData[J * 32 + I].r;
          TI.G[I, J] := Bitmap.FData[J * 32 + I].g;
          TI.B[I, J] := Bitmap.FData[J * 32 + I].b; }
          {RedGreenBlue(Bitmap.Pixel[J, I], r,g,b);}
          RedGreenBlue(Bmp.Canvas.Pixels[J, I], r,g,b);
          TI.R[I, J] := r;
          TI.G[I, J] := g;
          TI.B[I, J] := b;
        end;
      end;
    finally
      bmp.Free;
      if useStretchDraw then
          Tmp.Free
      else
          Copy.Free;
    end;
  end;
begin
  // This code assumes single threading
  if not NNLoaded then
    LoadNeuralNet;

  LoadBitmapIntoTinyImage(Self, TI);
  LoadTinyImageIntoNNetVolume(TI, InputV);
  InputV.RgbImgToNeuronalInput(csEncodeRGB);

  NN.Compute(InputV);
  NN.GetOutput(OutputV);

  Labels := NNLabels;
  SetLength(Scores, 0);

  MaxScore := -9999;
  Best := 'Not found';
  for i := 0 to OutputV.SizeX - 1 do
  begin
    if OutputV.Raw[i] > MaxScore then
    begin
      MaxScore := OutputV.Raw[i];
      Best := NNLabels[i];
    end;
    Insert(OutputV.Raw[i], Scores, Length(Scores));
  end;

  Result := True;
end;



end.

