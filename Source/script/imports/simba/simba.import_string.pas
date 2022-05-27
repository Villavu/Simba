unit simba.import_string;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,  uregexpr, strutils,
  simba.script_compiler, simba.mufasatypes, simba.stringutil;

procedure _LapeExtractFromStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := ExtractFromStr(Pstring(Params^[0])^, PStrExtr(Params^[1])^);
end;

procedure _LapeBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Between(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure _LapeImplode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Implode(Pstring(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeExplode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := Explode(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure _LapeExecRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := ExecRegExpr(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeSplitRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  List: TStringList;
begin
  List := TStringList.Create();
  try
    SplitRegExpr(PString(Params^[0])^, PString(Params^[1])^, List);

    PStringArray(Result)^ := List.ToStringArray();
  finally
    List.Free();
  end;
end;

procedure _LapeReplaceRegExpr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := ReplaceRegExpr(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, Pboolean(Params^[3])^);
end;

procedure _LapePosEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PosEx(PString(Params^[0])^, PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeLevDist(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := LevDistance(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeMultiBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := MultiBetween(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeIsArrInStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsArrInStr(PStringArray(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapePosMulti(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PosMulti(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeIsStrInArr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsStrInArr(PString(Params^[0])^, PBoolean(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure _LapeUTF8Decode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := UTF8Decode(PString(Params^[0])^);
end;

procedure _LapeUTF8Encode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := UTF8Encode(PWideString(Params^[0])^);
end;

procedure ImportString(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
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
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportString);

end.

