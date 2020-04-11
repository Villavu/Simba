unit simbascript.import_string;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_String(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.stringutil, strutils, regexpr;

procedure Lape_Capitalize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Capitalize(Pstring(Params^[1])^);
end;

procedure Lape_CompressString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := CompressString(Pstring(Params^[1])^);
end;

procedure Lape_DecompressString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := DecompressString(Pstring(Params^[1])^);
end;

procedure Lape_Base64Encode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Base64Encode(Pstring(Params^[1])^);
end;

procedure Lape_Base64Decode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Base64Decode(Pstring(Params^[1])^);
end;

procedure Lape_ExtractFromStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := ExtractFromStr(Pstring(Params^[1])^, PStrExtr(Params^[2])^);
end;

procedure Lape_Replace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PReplaceFlags = ^TReplaceFlags;
begin
  Pstring(Result)^ := StringReplace(Pstring(Params^[1])^, Pstring(Params^[2])^, Pstring(Params^[3])^, PReplaceFlags(Params^[4])^);
end;

procedure Lape_Between(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Between(Pstring(Params^[1])^, Pstring(Params^[2])^, Pstring(Params^[3])^);
end;

procedure Lape_Implode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Implode(Pstring(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure Lape_Explode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := Explode(Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_ExecRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := ExecRegExpr(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_SplitRegExpr(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SplitRegExpr(PString(Params^[1])^, PString(Params^[2])^, PStrings(Params^[3])^);
end;

procedure Lape_ReplaceRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := ReplaceRegExpr(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^, Pboolean(Params^[4])^);
end;

procedure Lape_posex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := posex(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_LevDist(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := LevDistance(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_NormLevDist(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := NormLevDistance(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_StringMatch(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := StringMatch(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_MultiBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := MultiBetween(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_IsArrInStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsArrInStr(PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_PosMulti(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PosMulti(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_IsStrInArr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsStrInArr(PString(Params^[1])^, PBoolean(Params^[2])^, PStringArray(Params^[3])^);
end;

procedure Lape_UTF8Decode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := UTF8Decode(PString(Params^[1])^);
end;

procedure Lape_UTF8Encode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := UTF8Encode(PWideString(Params^[1])^);
end;

procedure Lape_Import_String(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'String';

    addGlobalType('(Numbers, Letters, Others)', 'StrExtr');

    addGlobalMethod('function Capitalize(S: String): String', @Lape_Capitalize, Data);
    addGlobalMethod('function CompressString(S: String): String', @Lape_CompressString, Data);
    addGlobalMethod('function DecompressString(S: String): String', @Lape_DecompressString, Data);
    addGlobalMethod('function Base64Encode(S: String): String', @Lape_Base64Encode, Data);
    addGlobalMethod('function Base64Decode(S: String): String', @Lape_Base64Decode, Data);
    addGlobalMethod('function ExtractFromStr(S: String; Extract: StrExtr): String', @Lape_ExtractFromStr, Data);
    addGlobalMethod('function Replace(S, FindStr, ReplaceStr: String; Flags: TReplaceFlags): String', @Lape_Replace, Data);
    addGlobalMethod('function Between(S1, S2, S: String): String', @Lape_Between, Data);
    addGlobalMethod('function Implode(Glue: String; Pieces: TStringArray): String', @Lape_Implode, Data);
    addGlobalMethod('function Explode(Delimiter, S: String): TStringArray', @Lape_Explode, Data);
    addGlobalMethod('function ExecRegExpr(RegExpr, InputStr: String): Boolean', @Lape_ExecRegExpr, Data);
    addGlobalMethod('procedure SplitRegExpr(RegExpr, InputStr: String; Pieces: TStrings);', @Lape_SplitRegExpr, Data);
    addGlobalMethod('function ReplaceRegExpr(RegExpr, InputStr, ReplaceStr: String; UseSubstitution: Boolean): String', @Lape_ReplaceRegExpr, Data);
    addGlobalMethod('function PosEx(Needle, Haystack: String; Offset: Int32): Int32', @Lape_PosEx, Data);
    addGlobalMethod('function LevDistance(Source, Target: String): Int32', @Lape_LevDist, Data);
    addGlobalMethod('function NormLevDistance(Source, Target: String): Extended', @Lape_NormLevDist, Data);
    addGlobalMethod('function StringMatch(Source, Target: String): Extended;', @Lape_StringMatch, Data);
    addGlobalMethod('function MultiBetween(S, S1, S2: String): TStringArray;', @Lape_MultiBetween, Data);
    addGlobalMethod('function IsArrInStr(constref Arr: TStringArray; S: String): Boolean;', @Lape_IsArrInStr, Data);
    addGlobalMethod('function IsStrInArr(S: String; const UsePos: Boolean; const Arr: TStringArray): Boolean;', @Lape_IsStrInArr, Data);
    addGlobalMethod('function PosMulti(const SubStr: String; Text: String): TIntegerArray;', @Lape_PosMulti, Data);
    addGlobalMethod('function UTF8Decode(constref S: AnsiString): WideString;', @Lape_UTF8Decode, Data);
    addGlobalMethod('function UTF8Encode(constref S: WideString): AnsiString;', @Lape_UTF8Encode, Data);
  end;
end;

end.

