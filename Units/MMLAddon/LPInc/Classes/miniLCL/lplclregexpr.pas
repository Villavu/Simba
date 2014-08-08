unit lplclregexpr;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, RegExpr, lpcompiler, lptypes, lpClassHelper;

procedure RegisterLCLTRegExpr(Compiler: TLapeCompiler);

implementation

type
  PRegExpr = ^TRegExpr;
  PStrings = ^TStrings;
  PPtrInt = ^PtrInt;

//constructor Create;
procedure TRegExpr_Init(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRegExpr(Params^[0])^ := TRegExpr.Create();
end;

//function Exec (const AInputString : RegExprString) : boolean; {$IFDEF OverMeth} overload;
procedure TRegExpr_Exec(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PRegExpr(Params^[0])^.Exec(PLPString(Params^[1])^);
end;

//function ExecNext : boolean;
procedure TRegExpr_ExecNext(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PRegExpr(Params^[0])^.ExecNext();
end;

//function ExecPos: boolean;
procedure TRegExpr_ExecPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PRegExpr(Params^[0])^.ExecPos(PPtrInt(Params^[1])^);
end;

//Read: property InputString : RegExprString read GetInputString write SetInputString;
procedure TRegExpr_InputString_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.InputString;
end;

//Write: property InputString : RegExprString read GetInputString write SetInputString;
procedure TRegExpr_InputString_Write(const Params: PParamArray); lape_extdecl
begin
  PRegExpr(Params^[0])^.InputString := PLPString(Params^[1])^;
end;

//function Substitute (const ATemplate : RegExprString) : RegExprString;
procedure TRegExpr_Substitute(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Substitute(PLPString(Params^[1])^);
end;

//procedure Split (AInputStr : RegExprString; APieces : TStrings);
procedure TRegExpr_Split(const Params: PParamArray); lape_extdecl
begin
  PRegExpr(Params^[0])^.Split(PLPString(Params^[1])^, PStrings(Params^[2])^);
end;

//function Replace (AInputStr : RegExprString;
procedure TRegExpr_Replace(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Replace(PLPString(Params^[1])^, PLPString(Params^[2])^, PBoolean(Params^[2])^);
end;

//Read: Expression
procedure TRegExpr_Expression_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Expression;
end;

//Write: Expression
procedure TRegExpr_Expression_Write(const Params: PParamArray); lape_extdecl
begin
  PRegExpr(Params^[0])^.Expression := PLPString(Params^[1])^;
end;

//Read: property SubExprMatchCount : integer read GetSubExprMatchCount;
procedure TRegExpr_SubExprMatchCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PRegExpr(Params^[0])^.SubExprMatchCount;
end;

//Read: property MatchPos [Idx : integer] : PtrInt read GetMatchPos;
procedure TRegExpr_MatchPos_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPtrInt(Result)^ := PRegExpr(Params^[0])^.MatchPos[PInteger(Params^[1])^];
end;

//Read: property MatchLen [Idx : integer] : PtrInt read GetMatchLen;
procedure TRegExpr_MatchLen_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPtrInt(Result)^ := PRegExpr(Params^[0])^.MatchLen[PInteger(Params^[1])^];
end;

//Read: property Match [Idx : integer] : RegExprString read GetMatch;
procedure TRegExpr_Match_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PRegExpr(Params^[0])^.Match[PInteger(Params^[1])^];
end;

//procedure Free();
procedure TRegExpr_Free(const Params: PParamArray); lape_extdecl
begin
  PRegExpr(Params^[0])^.Free();
end;

procedure RegisterLCLTRegExpr(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TRegExpr', 'TObject');

    addGlobalFunc('procedure TRegExpr.Init();', @TRegExpr_Init);
    addClassVar('TRegExpr', 'Expression', 'String', @TRegExpr_Expression_Read, @TRegExpr_Expression_Write);
    addGlobalFunc('function TRegExpr.Exec(const AInputString :String): boolean;', @TRegExpr_Exec);
    addGlobalFunc('function TRegExpr.ExecNext(): boolean;', @TRegExpr_ExecNext);
    addGlobalFunc('function TRegExpr.ExecPos(AOffset: PtrInt): boolean;', @TRegExpr_ExecPos);
    addClassVar('TRegExpr', 'InputString', 'String', @TRegExpr_InputString_Read, @TRegExpr_InputString_Write);
    addGlobalFunc('function TRegExpr.Substitute(const ATemplate : String): String;', @TRegExpr_Substitute);
    addGlobalFunc('procedure TRegExpr.Split(AInputStr: String; APieces: TStrings);', @TRegExpr_Split);
    addGlobalFunc('function TRegExpr.Replace(AInputStr : String; const AReplaceStr : String; AUseSubstitution : Boolean): String;', @TRegExpr_Replace);
    addClassVar('TRegExpr', 'SubExprMatchCount', 'integer', @TRegExpr_SubExprMatchCount_Read, nil);
    addClassVar('TRegExpr', 'MatchPos', 'PtrInt', @TRegExpr_MatchPos_Read, nil, True);
    addClassVar('TRegExpr', 'MatchLen', 'PtrInt', @TRegExpr_MatchLen_Read, nil, True);
    addClassVar('TRegExpr', 'Match', 'String', @TRegExpr_Match_Read, nil, True);
    addGlobalFunc('procedure TRegExpr.Free();', @TRegExpr_Free);
  end;
end;

end.

