unit simba.import_pixelocr;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportPixelOCR(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.pixelocr, simba.pixelocr_font, simba.image, simba.canvas,
  simba.script_objectutil;

type
  PSimbaCanvas = ^TSimbaCanvas;

(*
Pixel OCR
=========
Reads text drawn in a pixel font: one with no antialiasing, where a character is always the same pixels.

A font is an image per character. In each image the character is white, an optional shadow is red and everything else is black.

 - A font with no shadow is read by its background: a character matches when all of its pixels are one colour and fewer than half of the pixels round it are that colour too.
 - A font with a shadow is read by its shadow: all of the character's pixels are one colour and all of its shadow pixels are dark.

If the image is only black and white the text must be white and a shadow black, whatever the tolerances.

`TPixelOCR` fields:

 - `Tolerance`: how far a character's pixels can be from its first pixel, 0..100.
 - `ShadowTolerance`: how bright a shadow pixel can be, 0..255: red and green up to this, blue up to 5 more.
 - `Whitelist`: if not empty, only these characters are returned.
 - `MaxWalk`: stop after this many columns with no character. 0 for no limit.
 - `MaxLen`: stop at this many characters, spaces included. 0 for no limit.
 - `Matches`: the text, hits and bounds of what the last call read.

```
var
  OCR: TPixelOCR;
  Font: TPixelFont;
  Image: TImage;
begin
  Font := TPixelOCR.LoadFont('Fonts/Bold 12', 4);
  Image := new TImage('text.png');

  WriteLn(OCR.Recognize(Image, Font));      // the line with the most hits
  WriteLn(OCR.Matches[0].Bounds);           // and where it is
  WriteLn(OCR.RecognizeLines(Image, Font)); // every line
end;
```
*)

(*
TPixelFont
----------
```
type
  TPixelFont = record
    Glyphs: array of TPixelFontGlyph;
    GlyphSimilarities: array of TByteArray;
    SpaceWidth: Integer;
    Reach: TBox;
    IsShadow: Boolean;
  end;
```
A font, made with `TPixelOCR.LoadFont`.

 - `Glyphs`: the characters. They are in the order a read tries them in, not in character order: use `Glyphs[i].Value` to tell which is which.
 - `GlyphSimilarities`: how alike each two characters are, 0..255, by index in `Glyphs`. 255 is exactly the same pixels. See `TPixelOCR.GetSimilarGlyphs`.
 - `SpaceWidth`: how many empty columns between two characters read as a space.
 - `Reach`: the box round every pixel that any character of the font looks at, from where it is placed.
 - `IsShadow`: the characters have a shadow. A font's characters all have one, or none do: `TPixelOCR.LoadFont` raises an error for a font with both.
*)

(*
TPixelFontGlyph
---------------
```
type
  TPixelFontGlyph = record
    Value: Char;
    Width: Integer;
    Left: Integer;
    Points: TPointArray;
    Shadow: TPointArray;
    Background: TPointArray;
    BackgroundBounds: TBox;
    BestMatch: Integer;
    Group: Integer;
  end;
```
One character of a font.

 - `Value`: the character.
 - `Width`: the width of its image, which is how far `TextToTPA` and `DrawText` move on after it.
 - `Left`: the first column of its image that has a pixel of the character.
 - `Points`: its pixels, the white ones of the image. X is from `Left`.
 - `Shadow`: its shadow's pixels, the red ones, if it has a shadow. X as `Points`.
 - `Background`: for a character with no shadow, every other pixel of `BackgroundBounds` and of one pixel more on every side. Empty for one with a shadow, which is not read by them.
 - `BackgroundBounds`: the box round `Points` and `Shadow`. Its width is how far a read moves on after the character.
 - `BestMatch`: the most hits the character can have. With a shadow that is its points and shadow pixels, without one its points and background pixels.
 - `Group`: how many characters after this one in `Glyphs` look at the same pixel first. A read tests that pixel once for all of them.
*)

(*
TPixelOCRMatch
--------------
```
type
  TPixelOCRMatch = record
    Text: String;
    Hits: Integer;
    Bounds: TBox;
  end;
```
What a call read, in `TPixelOCR.Matches`.

 - `Text`: the text.
 - `Hits`: how well it matched: the hits of every character in it added up. From `TPixelOCR.Locate` it is the match as 0..100.
 - `Bounds`: where the text is in the image.
*)

(*
TPixelOCR.LoadFont
------------------
```
function TPixelOCR.LoadFont(Path: String; SpaceWidth: Integer): TPixelFont; static;
function TPixelOCR.LoadFont(Images: TImageArray; SpaceWidth: Integer): TPixelFont; static;
function TPixelOCR.LoadFont(ZipFile, ZipDir: String; SpaceWidth: Integer): TPixelFont; static;
```
Loads a font from a directory of `.bmp` or `.png` images named by character code from 32 to 126 (`65.png` is `A`), from an array of images indexed by character code where missing characters are nil, or from such a directory inside a zip file.

`SpaceWidth` is how many empty columns between two characters read as a space. A space image (`32.png`) is only used by `TextToTPA` and `DrawText`, as the width of a space.

An error is raised if the directory or the zip file does not exist, if no characters are found, or if some characters of the font have a shadow and others do not.
*)
procedure _LapePixelOCR_LoadFont(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPixelFont(Result)^ := TPixelFont.Load(PString(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapePixelOCR_LoadFontImages(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  TLapeObjectArray = array of TLapeObject;
var
  Objs: TLapeObjectArray;
  Images: TSimbaImageArray;
  I: Integer;
begin
  Objs := TLapeObjectArray(Params^[0]^);
  SetLength(Images, Length(Objs));
  for I := 0 to High(Objs) do
    if (Pointer(Objs[I]) <> nil) then
      Images[I] := PLapeObjectImage(@Objs[I])^^;

  PPixelFont(Result)^ := TPixelFont.Load(Images, PInteger(Params^[1])^);
end;

procedure _LapePixelOCR_LoadFontZip(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPixelFont(Result)^ := TPixelFont.Load(PString(Params^[0])^, PString(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TPixelOCR.TextToTPA
-------------------
```
function TPixelOCR.TextToTPA(constref Font: TPixelFont; Text: String; Offset: TPoint = [0,0]; Scale: Integer = 1): TPointArray; static;
```
The pixels of `Text` in the font, without its shadow. Row 0 is the top of the text and column 0 the left edge of its first character's image.

`Scale` makes each pixel a square of that many pixels, so the text is never distorted. `Offset` is then added to every point.

An error is raised if a character is not in the font.

```
img.Canvas.DrawTPA(TPixelOCR.TextToTPA(Font, 'Twice the size', [10, 30], 2));
```
*)
procedure _LapePixelOCR_TextToTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PPixelFont(Params^[0])^.TextToTPA(PString(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TPixelOCR.GetSimilarGlyphs
--------------------------
```
function TPixelOCR.GetSimilarGlyphs(constref Font: TPixelFont; c: Char; MinMatch: Single = 1): array of Char; static;
```
The characters of the font that look like `c`, the most alike first. With the default `MinMatch` of 1 they are the ones with exactly the same pixels, which cannot be told apart when read: in some fonts `l` and `I`.

How alike two characters are is the pixels both have, of the pixels either has: `MinMatch` is 0..1 of that. `Font.GlyphSimilarities` holds it for every pair, as 0..255 by index in `Font.Glyphs`.

An error is raised if `c` is not in the font.
*)
procedure _LapePixelOCR_GetSimilarGlyphs(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  PCharArray = ^TCharArray;
begin
  PCharArray(Result)^ := PPixelFont(Params^[0])^.GetSimilarGlyphs(PChar(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TPixelOCR.Locate
----------------
```
function TPixelOCR.Locate(Image: TImage; constref Font: TPixelFont; Text: String): Single;
```
Finds `Text` anywhere in the image and returns how good the best place is, from 0 (not found) to 1: the share of the pixels round the text that are not the text's colour.

The text must be all one colour and spaced as the font lays it out, each character's image straight after the one before. Where a character is a column further on, the text is not found, though `Recognize` reads it.

`Matches[0].Bounds` is where it was found. `Matches` is empty if it was not.
*)
procedure _LapePixelOCR_Locate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PPixelOCR(Params^[0])^.Locate(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2]), PString(Params^[3])^);
end;

(*
TPixelOCR.Recognize
-------------------
```
function TPixelOCR.Recognize(Image: TImage; constref Font: TPixelFont; P: TPoint): String;
function TPixelOCR.Recognize(Image: TImage; constref Font: TPixelFont; Bounds: TBox = [-1,-1,-1,-1]): String;
```
Reads one line of text.

 - From a point: `P` is where the first character's image would be placed, so `P.Y` is the top of the font's images and not the top of the text. The row is read to the right edge of the image.
 - In a box: the text with the most hits is returned. Only text that starts in the rows of the box is read, so its top row of pixels is in the box, and no character starts left of the box or past its right edge. The default box is the whole image.

A character is only read where all of it is in the image. With a font that has no shadow it also needs one pixel of image on every side of it.

`Matches[0]` has the hits and the bounds of the text.
*)
procedure _LapePixelOCR_Recognize1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PPixelOCR(Params^[0])^.Recognize(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2]), PPoint(Params^[3])^);
end;

procedure _LapePixelOCR_Recognize2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PPixelOCR(Params^[0])^.Recognize(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2]), PBox(Params^[3])^);
end;

(*
TPixelOCR.RecognizeLines
------------------------
```
function TPixelOCR.RecognizeLines(Image: TImage; constref Font: TPixelFont; Bounds: TBox = [-1,-1,-1,-1]): TStringArray;
```
Reads every line of text that starts in the rows of the box, top to bottom. The default box is the whole image. `Matches` has one entry per line.

Rows that read text within half the height of the font's `Reach` of each other are one line: the row with the most hits.

A line is only kept if about half of its characters or more are letters, digits or one of `%&#$[]{}@!?`. `MaxWalk` and `MaxLen` are not used.
*)
procedure _LapePixelOCR_RecognizeLines(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PPixelOCR(Params^[0])^.RecognizeLines(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2]), PBox(Params^[3])^);
end;

(*
TCanvas.DrawText
----------------
```
procedure TCanvas.DrawText(constref Font: TPixelFont; Text: String; Position: TPoint; Scale: Integer = 1);
```
Draws `Text` in a pixel font, with the top left of its first character's image at `Position`.

The text is drawn in the canvas's `DrawColor` and `DrawAlpha`, and a shadow in black. `Scale` draws each pixel of the font as a square of that many pixels, so the text is never distorted.

An error is raised if a character is not in the font.

```
img.Canvas.DrawColor := Colors.YELLOW;
img.Canvas.DrawText(Font, 'Hello world', [10, 10]);
img.Canvas.DrawText(Font, 'Twice the size', [10, 30], 2);
```
*)
procedure _LapePixelOCR_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPixelFont(Params^[1])^.DrawText(PSimbaCanvas(Params^[0])^, PString(Params^[2])^, PPoint(Params^[3])^, PInteger(Params^[4])^);
end;

procedure ImportPixelOCR(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addGlobalType([
      'record',
      '  Value: Char;',
      '',
      '  Width: Integer;',
      '  Left: Integer;',
      '',
      '  Points: TPointArray;',
      '  Shadow: TPointArray;',
      '  Background: TPointArray;',
      '  BackgroundBounds: TBox;',
      '',
      '  BestMatch: Integer;',
      '  Group: Integer;',
      'end;'],
      'TPixelFontGlyph'
    );

    addGlobalType([
      'record',
      '  Glyphs: array of TPixelFontGlyph;',
      '  GlyphSimilarities: array of TByteArray;',
      '  SpaceWidth: Integer;',
      '  Reach: TBox;',
      '  IsShadow: Boolean;',
      'end;'],
      'TPixelFont'
    );

    addGlobalType([
      'record',
      '  Text: String;',
      '  Hits: Integer;',
      '  Bounds: TBox;',
      'end;'],
      'TPixelOCRMatch'
    );

    addGlobalType([
      'record',
      '  Tolerance: Single;',
      '  ShadowTolerance: Integer;',
      '  Whitelist: set of Char;',
      '  MaxWalk: Integer;',
      '  MaxLen: Integer;',
      '  Matches: array of TPixelOCRMatch;',
      'end;'],
      'TPixelOCR'
    );

    if (getGlobalType('TPixelFontGlyph').Size <> SizeOf(TPixelFontGlyph)) then
      SimbaException('TPixelFontGlyph import is wrong');
    if (getGlobalType('TPixelFont').Size <> SizeOf(TPixelFont)) then
      SimbaException('TPixelFont import is wrong');
    if (getGlobalType('TPixelOCRMatch').Size <> SizeOf(TPixelOCRMatch)) then
      SimbaException('TPixelOCRMatch import is wrong');
    if (getGlobalType('TPixelOCR').Size <> SizeOf(TPixelOCR)) then
      SimbaException('TPixelOCR import is wrong');

    addGlobalFunc('function TPixelOCR.LoadFont(Path: String; SpaceWidth: Integer): TPixelFont; static; overload;', @_LapePixelOCR_LoadFont);
    addGlobalFunc('function TPixelOCR.LoadFont(Images: TImageArray; SpaceWidth: Integer): TPixelFont; static; overload;', @_LapePixelOCR_LoadFontImages);
    addGlobalFunc('function TPixelOCR.LoadFont(ZipFile, ZipDir: String; SpaceWidth: Integer): TPixelFont; static; overload;', @_LapePixelOCR_LoadFontZip);
    addGlobalFunc('function TPixelOCR.TextToTPA(constref Font: TPixelFont; Text: String; Offset: TPoint = [0,0]; Scale: Integer = 1): TPointArray; static;', @_LapePixelOCR_TextToTPA);
    addGlobalFunc('function TPixelOCR.GetSimilarGlyphs(constref Font: TPixelFont; c: Char; MinMatch: Single = 1): array of Char; static;', @_LapePixelOCR_GetSimilarGlyphs);
    addGlobalFunc('function TPixelOCR.Locate(Image: TImage; constref Font: TPixelFont; Text: String): Single;', @_LapePixelOCR_Locate);
    addGlobalFunc('function TPixelOCR.Recognize(Image: TImage; constref Font: TPixelFont; P: TPoint): String; overload;', @_LapePixelOCR_Recognize1);
    addGlobalFunc('function TPixelOCR.Recognize(Image: TImage; constref Font: TPixelFont; Bounds: TBox = [-1,-1,-1,-1]): String; overload;', @_LapePixelOCR_Recognize2);
    addGlobalFunc('function TPixelOCR.RecognizeLines(Image: TImage; constref Font: TPixelFont; Bounds: TBox = [-1,-1,-1,-1]): TStringArray;', @_LapePixelOCR_RecognizeLines);

    addGlobalFunc('procedure TCanvas.DrawText(constref Font: TPixelFont; Text: String; Position: TPoint; Scale: Integer = 1); overload;', @_LapePixelOCR_DrawText);
  end;
end;

end.

