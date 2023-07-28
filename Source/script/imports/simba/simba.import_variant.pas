{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Variant script imports.
}
unit simba.import_variant;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportVariant(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, variants;

(*
Variant
=======
The variant "magic" datatype can store most base types.
*)

type
  {$SCOPEDENUMS ON}
  PVariantType = ^EVariantType;
  EVariantType = (Unknown, Unassigned, Null, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Single, Double, DateTime, Currency, Boolean, Variant, AString, UString, WString);
  {$SCOPEDENUMS OFF}

(*
Variant.VarType
~~~~~~~~~~~~~~~
function Variant.VarType: EVariantVarType;

Returns the variants var type.

Example::

  if (v.VarType = EVariantVarType.Int32) then
    WriteLn('Variant contains a Int32');
*)
procedure _LapeVariantVarType(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function GetVarType(const v: Variant): EVariantType;
  begin
    case VarType(v) of
      varBoolean:  Result := EVariantType.Boolean;

      varShortInt: Result := EVariantType.Int8;
      varSmallInt: Result := EVariantType.Int16;
      varInteger:  Result := EVariantType.Int32;
      varInt64:    Result := EVariantType.Int64;

      varByte:     Result := EVariantType.UInt8;
      varWord:     Result := EVariantType.UInt16;
      varLongWord: Result := EVariantType.UInt32;
      varQWord:    Result := EVariantType.UInt64;

      varSingle:   Result := EVariantType.Single;
      varDouble:   Result := EVariantType.Double;
      varDate:     Result := EVariantType.DateTime;
      varCurrency: Result := EVariantType.Currency;

      varOleStr:   Result := EVariantType.WString;
      varUString:  Result := EVariantType.UString;
      varString:   Result := EVariantType.AString;
      else
        Result := EVariantType.Unknown;
    end;
  end;

begin
  PVariantType(Result)^ := GetVarType(PVariant(Params^[0])^);
end;

(*
Variant.IsNumeric
~~~~~~~~~~~~~~~~~
function Variant.IsNumeric: Boolean;
*)
procedure _LapeVariantIsNumeric(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PEvalBool(Result)^ := VarIsNumeric(PVariant(Params^[0])^);
end;

(*
Variant.IsString
~~~~~~~~~~~~~~~~
function Variant.IsString: Boolean;
*)
procedure _LapeVariantIsString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PEvalBool(Result)^ := VarIsStr(PVariant(Params^[0])^);
end;

(*
Variant.IsOrdinal
~~~~~~~~~~~~~~~~~
function Variant.IsOrdinal: Boolean;
*)
procedure _LapeVariantIsOrdinal(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PEvalBool(Result)^ := VarIsOrdinal(PVariant(Params^[0])^);
end;

(*
Variant.IsFloat
~~~~~~~~~~~~~~~
function Variant.IsFloat: Boolean;
*)
procedure _LapeVariantIsFloat(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PEvalBool(Result)^ := VarIsFloat(PVariant(Params^[0])^);
end;

(*
Variant.IsBoolean
~~~~~~~~~~~~~~~~~
function Variant.IsBoolean: Boolean;
*)
procedure _LapeVariantIsBoolean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PEvalBool(Result)^ := VarIsBool(PVariant(Params^[0])^);
end;

(*
Variant.Null
~~~~~~~~~~~~
function Variant.Null: Variant; static;

Static method that returns a null variant variable.

Example:

```
  v := Variant.Null;
```
*)
procedure _LapeVariantNull(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := Null;
end;

(*
Variant.Unassigned
~~~~~~~~~~~~~~~~~~
function Variant.Unassigned: Variant; static;

Static method that returns a unassigned variant variable.

Example:

```
  if (v = Variant.Unassigned) then
    WriteLn('The variant has not been assigned to!');
```
*)
procedure _LapeVariantUnassigned(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := Unassigned;
end;

procedure ImportVariant(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Variant';

    addGlobalType('enum(Unknown, Unassigned, Null, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Single, Double, DateTime, Currency, Boolean, Variant, AString, UString, WString)', 'EVariantVarType');

    addGlobalFunc('function Variant.VarType: EVariantVarType;', @_LapeVariantVarType);
    addGlobalFunc('function Variant.IsNumeric: Boolean;', @_LapeVariantIsNumeric);
    addGlobalFunc('function Variant.IsString: Boolean;', @_LapeVariantIsString);
    addGlobalFunc('function Variant.IsOrdinal: Boolean;', @_LapeVariantIsOrdinal);
    addGlobalFunc('function Variant.IsFloat: Boolean;', @_LapeVariantIsFloat);
    addGlobalFunc('function Variant.IsBoolean: Boolean;', @_LapeVariantIsBoolean);

    addGlobalFunc('function Variant.Null: Variant; static;', @_LapeVariantNull);
    addGlobalFunc('function Variant.Unassigned: Variant; static;', @_LapeVariantUnassigned);

    ImportingSection := '';
  end;
end;

end.

