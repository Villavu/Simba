unit simba.import_pixelocr;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportPixelOCR(Script: TSimbaScript);

implementation

uses
  lptypes, lpvartypes,
  simba.pixelocr, simba.image,
  simba.script_objectutil;

type
  PPixelFont = ^TPixelFont;
  PPixelOCR = ^TPixelOCR;

procedure _LapePixelOCR_LoadFont(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPixelFont(Result)^ := TPixelOCR.LoadFont(PString(Params^[0])^, PInteger(Params^[1])^);
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

  PPixelFont(Result)^ := TPixelOCR.LoadFont(Images, PInteger(Params^[1])^);
end;

procedure _LapePixelOCR_TextToTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TPixelOCR.TextToTPA(PPixelFont(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapePixelOCR_Locate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PPixelOCR(Params^[0])^.Locate(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapePixelOCR_Recognize1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PPixelOCR(Params^[0])^.Recognize(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2])^, PPoint(Params^[3])^);
end;

procedure _LapePixelOCR_Recognize2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PPixelOCR(Params^[0])^.Recognize(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapePixelOCR_RecognizeLines(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PPixelOCR(Params^[0])^.RecognizeLines(PLapeObjectImage(Params^[1])^^, PPixelFont(Params^[2])^, PBox(Params^[3])^);
end;

procedure ImportPixelOCR(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addGlobalType([
      'record',
      '  Value: Char;',
      '',
      '  Width: Int16;',
      '  Height: Int16;',
      '  ForegroundBounds: TBox;',
      '',
      '  Points: TPointArray;',
      '  Shadow: TPointArray;',
      '  Background: TPointArray;',
      '  BackgroundBounds: TBox;',
      '  PointsShadowWidth: Int16;',
      '',
      '  BestMatch: Int16;',
      'end;'],
      'TPixelFontGlyph'
    );

    addGlobalType([
      'record',
      '  Glyphs: array of TPixelFontGlyph;',
      '  GlyphSimilarities: array of TByteArray;',
      '  SpaceWidth: Integer;',
      '  MaxGlyphHeight: Integer;',
      '  MaxGlyphWidth: Integer;',
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
      '  ShadowTolerance: Single;',
      '  Whitelist: set of Char;',
      '  MaxWalk: Integer;',
      '  MaxLen: Integer;',
      '  Matches: array of TPixelOCRMatch;',
      'end'],
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
    addGlobalFunc('function TPixelOCR.TextToTPA(constref Font: TPixelFont; Text: String): TPointArray; static;', @_LapePixelOCR_TextToTPA);
    addGlobalFunc('function TPixelOCR.Locate(Image: TImage; constref Font: TPixelFont; Text: String): Single;', @_LapePixelOCR_Locate);
    addGlobalFunc('function TPixelOCR.Recognize(Image: TImage; constref Font: TPixelFont; P: TPoint): String; overload;', @_LapePixelOCR_Recognize1);
    addGlobalFunc('function TPixelOCR.Recognize(Image: TImage; constref Font: TPixelFont; Bounds: TBox = [-1,-1,-1,-1]): String; overload;', @_LapePixelOCR_Recognize2);
    addGlobalFunc('function TPixelOCR.RecognizeLines(Image: TImage; constref Font: TPixelFont; Bounds: TBox= [-1,-1,-1,-1]): TStringArray;', @_LapePixelOCR_RecognizeLines);
  end;
end;

end.

