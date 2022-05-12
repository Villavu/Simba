pushSection('String');

addGlobalType('(Numbers, Letters, Others)', 'StrExtr');
addGlobalFunc('function ExtractFromStr(S: String; Extract: StrExtr): String', @_LapeExtractFromStr);
addGlobalFunc('function Between(S1, S2, S: String): String', @_LapeBetween);
addGlobalFunc('function Implode(Glue: String; Pieces: TStringArray): String', @_LapeImplode);
addGlobalFunc('function Explode(Delimiter, S: String): TStringArray', @_LapeExplode);
addGlobalFunc('function ExecRegExpr(RegExpr, InputStr: String): Boolean', @_LapeExecRegExpr);
addGlobalFunc('function SplitRegExpr(RegExpr, InputStr: String): TStringArray', @_LapeSplitRegExpr);
addGlobalFunc('function ReplaceRegExpr(RegExpr, InputStr, ReplaceStr: String; UseSubstitution: Boolean): String', @_LapeReplaceRegExpr);
addGlobalFunc('function PosEx(Needle, Haystack: String; Offset: Integer): Integer', @_LapePosEx);
addGlobalFunc('function LevDistance(Source, Target: String): Integer', @_LapeLevDist);
addGlobalFunc('function MultiBetween(S, S1, S2: String): TStringArray', @_LapeMultiBetween);
addGlobalFunc('function IsArrInStr(Arr: TStringArray; S: String): Boolean', @_LapeIsArrInStr);
addGlobalFunc('function IsStrInArr(S: String;  UsePos: Boolean; Arr: TStringArray): Boolean', @_LapeIsStrInArr);
addGlobalFunc('function PosMulti(Needle, Haystack: String): TIntegerArray', @_LapePosMulti);
addGlobalFunc('function UTF8Decode(S: AnsiString): WideString', @_LapeUTF8Decode);
addGlobalFunc('function UTF8Encode(S: WideString): AnsiString', @_LapeUTF8Encode);

popSection();

