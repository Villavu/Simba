unit simba.import_random;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.random;

(*
Random
======
Methods relating to generating random numbers.
*)

(*
RandomCenterTPA
~~~~~~~~~~~~~~~
function RandomCenterTPA(Amount:Integer; Box: TBox): TPointArray;
*)
procedure _LapeRandomCenterTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RandomCenterTPA(PInteger(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapeRandomTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := RandomTPA(PInteger(Params^[0])^, PBox(Params^[1])^);
end;

procedure ImportRandom(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Random';

    addGlobalFunc('function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray', @_LapeRandomCenterTPA);
    addGlobalFunc('function RandomTPA(Amount: Integer; Box: TBox): TPointArray', @_LapeRandomTPA);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportRandom);

end.
