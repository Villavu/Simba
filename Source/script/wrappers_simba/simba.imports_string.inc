pushSection('String');

addGlobalType('(Numbers, Letters, Others)', 'StrExtr');
addGlobalFunc('function CompressString(S: String): String', @_LapeCompressString);
addGlobalFunc('function DecompressString(S: String): String', @_LapeDecompressString);
addGlobalFunc('function Base64Encode(S: String): String', @_LapeBase64Encode);
addGlobalFunc('function Base64Decode(S: String): String', @_LapeBase64Decode);
addGlobalFunc('function ExtractFromStr(S: String; Extract: StrExtr): String', @_LapeExtractFromStr);
addGlobalFunc('function Between(S1, S2, S: String): String', @_LapeBetween);
addGlobalFunc('function Implode(Glue: String; Pieces: TStringArray): String', @_LapeImplode);
addGlobalFunc('function Explode(Delimiter, S: String): TStringArray', @_LapeExplode);
addGlobalFunc('function ExecRegExpr(RegExpr, InputStr: String): Boolean', @_LapeExecRegExpr);
addGlobalFunc('function SplitRegExpr(RegExpr, InputStr: String): TStringArray', @_LapeSplitRegExpr);
addGlobalFunc('function ReplaceRegExpr(RegExpr, InputStr, ReplaceStr: String; UseSubstitution: Boolean): String', @_LapeReplaceRegExpr);
addGlobalFunc('function PosEx(Needle, Haystack: String; Offset: Int32): Int32', @_LapePosEx);
addGlobalFunc('function LevDistance(Source, Target: String): Int32', @_LapeLevDist);
addGlobalFunc('function NormLevDistance(Source, Target: String): Extended', @_LapeNormLevDist);
addGlobalFunc('function StringMatch(Source, Target: String): Extended', @_LapeStringMatch);
addGlobalFunc('function MultiBetween(S, S1, S2: String): TStringArray', @_LapeMultiBetween);
addGlobalFunc('function IsArrInStr(constref Arr: TStringArray; S: String): Boolean', @_LapeIsArrInStr);
addGlobalFunc('function IsStrInArr(S: String; const UsePos: Boolean; const Arr: TStringArray): Boolean', @_LapeIsStrInArr);
addGlobalFunc('function PosMulti(const SubStr: String; Text: String): TIntegerArray', @_LapePosMulti);
addGlobalFunc('function UTF8Decode(constref S: AnsiString): WideString', @_LapeUTF8Decode);
addGlobalFunc('function UTF8Encode(constref S: WideString): AnsiString', @_LapeUTF8Encode);

popSection();

