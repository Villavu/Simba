{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
 Jarl Holta - https://github.com/slackydev

  - RandomLeft
  - RandomRight
  - RandomMean
  - RandomMode
}
unit simba.import_random;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportRandom(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.random;

(*
Random
======
Methods relating to generating random numbers.
*)

(*
RandSeed
----------
> var RandSeed: UInt32;

The random seed used for all random number generation.
*)

(*
RandCutoff
----------
> var RandCutoff: Double;

Cutoff for `RandomLeft`, `RandomRight`, `RandomMode`, `RandomMean`
*)

(*
RandomCenterTPA
---------------
> function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray;

Generates random points in `Box` weighted towards the center.
*)
procedure _LapeRandomCenterTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := RandomCenterTPA(PInteger(Params^[0])^, PBox(Params^[1])^);
end;

(*
RandomTPA
---------
> function RandomTPA(Amount: Integer; Box: TBox): TPointArray;

Generates random points in `Box`.
*)
procedure _LapeRandomTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := RandomTPA(PInteger(Params^[0])^, PBox(Params^[1])^);
end;

(*
RandomLeft
----------
> function RandomLeft(Lo, Hi: Double): Double;
> function RandomLeft(Lo, Hi: Int64): Int64;

Generates a random number between `Lo` and `Hi` weighted towards `Lo`
*)
procedure _LapeRandomLeft(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := RandomLeft(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

procedure _LapeRandomLeftI(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := RandomLeft(PInt64(Params^[0])^, PInt64(Params^[1])^);
end;

(*
RandomRight
-----------
> function RandomRight(Lo, Hi: Double): Double;
> function RandomRight(Lo, Hi: Int64): Int64;

Generates a random number between `Lo` and `Hi` weighted towards `Hi`
*)
procedure _LapeRandomRight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := RandomRight(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

procedure _LapeRandomRightI(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := RandomRight(PInt64(Params^[0])^, PInt64(Params^[1])^);
end;

(*
RandomMean
----------
> function RandomMean(Lo, Hi: Double): Double;
> function RandomMean(Lo, Hi: Int64): Int64;

Generates a random number between `Lo` and `Hi` weighted towards the mean of Lo,Hi
*)
procedure _LapeRandomMean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := RandomMean(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

procedure _LapeRandomMeanI(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := RandomMean(PInt64(Params^[0])^, PInt64(Params^[1])^);
end;

(*
RandomMode
----------
> function RandomMode(Mode, Lo, Hi: Double): Double;
> function RandomMode(Mode, Lo, Hi: Int64): Int64;

Generates a random number between `Lo` and `Hi` weighted torwards `Mode`
*)
procedure _LapeRandomMode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := RandomMode(PDouble(Params^[0])^, PDouble(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeRandomModeI(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := RandomMode(PInt64(Params^[0])^, PInt64(Params^[1])^, PInt64(Params^[2])^);
end;

(*
GaussRand
---------
> function GaussRand(Mean, Dev: Double): Double;

Generates a random gaussian/normal number.
*)
procedure _LapeGaussRand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := GaussRand(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

(*
Randomize
---------
> procedure Randomize;

Generates a new `RandSeed`
*)
procedure _LapeRandomize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  BetterRandomize();
end;

procedure ImportRandom(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Random';

    addGlobalVar(ltDouble, @RandCutoff, 'RandCutoff');

    addGlobalFunc('procedure Randomize; override;', @_LapeRandomize);

    addGlobalFunc('function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray', @_LapeRandomCenterTPA);
    addGlobalFunc('function RandomTPA(Amount: Integer; Box: TBox): TPointArray', @_LapeRandomTPA);

    addGlobalFunc('function RandomLeft(Lo, Hi: Double): Double; overload', @_LapeRandomLeft);
    addGlobalFunc('function RandomLeft(Lo, Hi: Int64): Int64; overload', @_LapeRandomLeftI);

    addGlobalFunc('function RandomRight(Lo, Hi: Double): Double; overload', @_LapeRandomRight);
    addGlobalFunc('function RandomRight(Lo, Hi: Int64): Int64; overload', @_LapeRandomRightI);

    addGlobalFunc('function RandomMean(Lo, Hi: Double): Double; overload', @_LapeRandomMean);
    addGlobalFunc('function RandomMean(Lo, Hi: Int64): Int64; overload', @_LapeRandomMeanI);

    addGlobalFunc('function RandomMode(Mode, Lo, Hi: Double): Double; overload', @_LapeRandomMode);
    addGlobalFunc('function RandomMode(Mode, Lo, Hi: Int64): Int64; overload', @_LapeRandomModeI);

    addGlobalFunc('function GaussRand(Mean, Dev: Double): Double;', @_LapeGaussRand);

    ImportingSection := '';
  end;
end;

end.
