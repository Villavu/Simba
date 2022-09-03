{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Variant script imports.
}
unit simba.import_variant;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,  variants,
  simba.script_compiler;


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
procedure _LapeVariantVarType(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function GetVarType(const v: Variant): EVariantType;
  begin
    case VarType(v) of
      varboolean:  Result := EVariantType.Boolean;

      varshortint: Result := EVariantType.Int8;
      varsmallint: Result := EVariantType.Int16;
      varinteger:  Result := EVariantType.Int32;
      varint64:    Result := EVariantType.Int64;

      varbyte:     Result := EVariantType.UInt8;
      varword:     Result := EVariantType.UInt16;
      varlongword: Result := EVariantType.UInt32;
      varqword:    Result := EVariantType.UInt64;

      varsingle:   Result := EVariantType.Single;
      vardouble:   Result := EVariantType.Double;
      vardate:     Result := EVariantType.DateTime;
      varcurrency: Result := EVariantType.Currency;

      varolestr:   Result := EVariantType.WString;
      varustring:  Result := EVariantType.UString;
      varstring:   Result := EVariantType.AString;
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
procedure _LapeVariantIsNumeric(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsNumeric(PVariant(Params^[0])^);
end;

(*
Variant.IsString
~~~~~~~~~~~~~~~~
function Variant.IsString: Boolean;
*)
procedure _LapeVariantIsString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsStr(PVariant(Params^[0])^);
end;

(*
Variant.IsOrdinal
~~~~~~~~~~~~~~~~~
function Variant.IsOrdinal: Boolean;
*)
procedure _LapeVariantIsOrdinal(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsOrdinal(PVariant(Params^[0])^);
end;

(*
Variant.IsFloat
~~~~~~~~~~~~~~~
function Variant.IsFloat: Boolean;
*)
procedure _LapeVariantIsFloat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsFloat(PVariant(Params^[0])^);
end;

(*
Variant.IsBoolean
~~~~~~~~~~~~~~~~~
function Variant.IsBoolean: Boolean;
*)
procedure _LapeVariantIsBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsBool(PVariant(Params^[0])^);
end;

(*
Variant.Null
~~~~~~~~~~~~
function Variant.Null: Variant; static;

Static method that returns a null variant variable.

Example::

  v := Variant.Null;
*)
procedure _LapeVariantNull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := Null;
end;

(*
Variant.Unassigned
~~~~~~~~~~~~~~~~~~
function Variant.Unassigned: Variant; static;

Static method that returns a unassigned variant variable.

Example::

  if (v = Variant.Unassigned) then
    WriteLn('The variant has not been assigned to!');
*)
procedure _LapeVariantUnassigned(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := Unassigned;
end;

procedure ImportVariant(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Variant');

    addGlobalType('enum(Unknown, Unassigned, Null, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Single, Double, DateTime, Currency, Boolean, Variant, AString, UString, WString)', 'EVariantVarType');

    addGlobalFunc('function Variant.VarType: EVariantVarType;', @_LapeVariantVarType);
    addGlobalFunc('function Variant.IsNumeric: Boolean;', @_LapeVariantIsNumeric);
    addGlobalFunc('function Variant.IsString: Boolean;', @_LapeVariantIsString);
    addGlobalFunc('function Variant.IsOrdinal: Boolean;', @_LapeVariantIsOrdinal);
    addGlobalFunc('function Variant.IsFloat: Boolean;', @_LapeVariantIsFloat);
    addGlobalFunc('function Variant.IsBoolean: Boolean;', @_LapeVariantIsBoolean);

    addGlobalFunc('function Variant.Null: Variant; static;', @_LapeVariantNull);
    addGlobalFunc('function Variant.Unassigned: Variant; static;', @_LapeVariantUnassigned);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportVariant);

end.

