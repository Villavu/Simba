unit simba.script_import_lclregexpr;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLRegExpr(Compiler: TSimbaScript_Compiler);

implementation

uses
  regexpr;

type
  PRegExpr = ^TRegExpr;
  PStrings = ^TStrings;
  PPtrInt = ^PtrInt;

//constructor Create;
procedure Lape_TRegExpr_Init(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRegExpr(Params^[0])^ := TRegExpr.Create();
end;

//function Exec (const AInputString : RegExprString) : boolean; {$IFDEF OverMeth} overload;
procedure Lape_TRegExpr_Exec(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PRegExpr(Params^[0])^.Exec(PLPString(Params^[1])^);
end;

//function ExecNext : boolean;
procedure Lape_TRegExpr_ExecNext(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PRegExpr(Params^[0])^.ExecNext();
end;

//function ExecPos: boolean;
procedure Lape_TRegExpr_ExecPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PRegExpr(Params^[0])^.ExecPos(PPtrInt(Params^[1])^);
end;

//Read: property InputString : RegExprString read GetInputString write SetInputString;
procedure Lape_TRegExpr_InputString_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.InputString;
end;

//Write: property InputString : RegExprString read GetInputString write SetInputString;
procedure Lape_TRegExpr_InputString_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRegExpr(Params^[0])^.InputString := PLPString(Params^[1])^;
end;

//function Substitute (const ATemplate : RegExprString) : RegExprString;
procedure Lape_TRegExpr_Substitute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Substitute(PLPString(Params^[1])^);
end;

//procedure Split (AInputStr : RegExprString; APieces : TStrings);
procedure Lape_TRegExpr_Split(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRegExpr(Params^[0])^.Split(PLPString(Params^[1])^, PStrings(Params^[2])^);
end;

//function Replace (AInputStr : RegExprString;
procedure Lape_TRegExpr_Replace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Replace(PLPString(Params^[1])^, PLPString(Params^[2])^, PBoolean(Params^[2])^);
end;

//Read: Expression
procedure Lape_TRegExpr_Expression_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Expression;
end;

//Write: Expression
procedure Lape_TRegExpr_Expression_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRegExpr(Params^[0])^.Expression := PLPString(Params^[1])^;
end;

//Read: property SubExprMatchCount : integer read GetSubExprMatchCount;
procedure Lape_TRegExpr_SubExprMatchCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PRegExpr(Params^[0])^.SubExprMatchCount;
end;

//Read: property MatchPos [Idx : integer] : PtrInt read GetMatchPos;
procedure Lape_TRegExpr_MatchPos_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrInt(Result)^ := PRegExpr(Params^[0])^.MatchPos[PInteger(Params^[1])^];
end;

//Read: property MatchLen [Idx : integer] : PtrInt read GetMatchLen;
procedure Lape_TRegExpr_MatchLen_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrInt(Result)^ := PRegExpr(Params^[0])^.MatchLen[PInteger(Params^[1])^];
end;

//Read: property Match [Idx : integer] : RegExprString read GetMatch;
procedure Lape_TRegExpr_Match_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Match[PInteger(Params^[1])^];
end;

//procedure Free();
procedure Lape_TRegExpr_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRegExpr(Params^[0])^.Free();
end;

procedure Lape_Import_LCLRegExpr(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TRegExpr', 'TObject');

    addGlobalFunc('procedure TRegExpr.Init(); override;', @Lape_TRegExpr_Init);
    addClassVar('TRegExpr', 'Expression', 'String', @Lape_TRegExpr_Expression_Read, @Lape_TRegExpr_Expression_Write);
    addGlobalFunc('function TRegExpr.Exec(const AInputString :String): boolean; constref;', @Lape_TRegExpr_Exec);
    addGlobalFunc('function TRegExpr.ExecNext(): boolean; constref;', @Lape_TRegExpr_ExecNext);
    addGlobalFunc('function TRegExpr.ExecPos(AOffset: PtrInt): boolean; constref;', @Lape_TRegExpr_ExecPos);
    addClassVar('TRegExpr', 'InputString', 'String', @Lape_TRegExpr_InputString_Read, @Lape_TRegExpr_InputString_Write);
    addGlobalFunc('function TRegExpr.Substitute(const ATemplate : String): String; constref;', @Lape_TRegExpr_Substitute);
    addGlobalFunc('procedure TRegExpr.Split(AInputStr: String; APieces: TStrings); constref;', @Lape_TRegExpr_Split);
    addGlobalFunc('function TRegExpr.Replace(AInputStr : String; const AReplaceStr : String; AUseSubstitution : Boolean): String; constref;', @Lape_TRegExpr_Replace);
    addClassVar('TRegExpr', 'SubExprMatchCount', 'integer', @Lape_TRegExpr_SubExprMatchCount_Read, nil);
    addClassVar('TRegExpr', 'MatchPos', 'PtrInt', @Lape_TRegExpr_MatchPos_Read, nil, True);
    addClassVar('TRegExpr', 'MatchLen', 'PtrInt', @Lape_TRegExpr_MatchLen_Read, nil, True);
    addClassVar('TRegExpr', 'Match', 'String', @Lape_TRegExpr_Match_Read, nil, True);
    //addGlobalFunc('procedure TRegExpr.Free(); constref;', @Lape_TRegExpr_Free);
  end;
end;


end.

