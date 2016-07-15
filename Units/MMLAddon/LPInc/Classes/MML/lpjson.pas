unit lpjson;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils,ujson, lpcompiler, lptypes, lpClassHelper;
type
  PJSONObject = ^TJSONObject;
  PJSONArray = ^TJSONArray;
  PZAbstractObject = ^TZAbstractObject;
  PStringList = ^TStringList;

 Procedure Register_JSON(Compiler: TLapeCompiler);



implementation

//constructor create ; overload;
procedure TJSONArray_create(const Params: PParamArray); lape_extdecl
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create();
end;

//constructor create (s : string);  overload;
procedure TJSONArray_createExExEx(const Params: PParamArray); lape_extdecl
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create(PlpString(Params^[1])^);
end;

//function get (index : integer) : TZAbstractObject;
procedure TJSONArray_get(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PZAbstractObject(Result)^ := PJSONArray(Params^[0])^.get(Pinteger(Params^[1])^);
end;

//function getBoolean (index : integer) : boolean;
procedure TJSONArray_getBoolean(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.getBoolean(Pinteger(Params^[1])^);
end;

//function getDouble (index : integer) : double;
procedure TJSONArray_getDouble(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PJSONArray(Params^[0])^.getDouble(Pinteger(Params^[1])^);
end;

//function getInt (index : integer): integer;
procedure TJSONArray_getInt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.getInt(Pinteger(Params^[1])^);
end;

//function getJSONArray (index : integer) : TJSONArray;
procedure TJSONArray_getJSONArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.getJSONArray(Pinteger(Params^[1])^);
end;

//function getJSONObject (index : integer) : TJSONObject;
procedure TJSONArray_getJSONObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.getJSONObject(Pinteger(Params^[1])^);
end;

//function getString (index : integer) : string;
procedure TJSONArray_getString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.getString(Pinteger(Params^[1])^);
end;

//function isNull (index : integer): boolean;
procedure TJSONArray_isNull(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.isNull(Pinteger(Params^[1])^);
end;

//function join (separator : string) : string;
procedure TJSONArray_join(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.join(PlpString(Params^[1])^);
end;

//function length : integer;
procedure TJSONArray_length(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.length();
end;

//function opt (index : integer) : TZAbstractObject;
procedure TJSONArray_opt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PZAbstractObject(Result)^ := PJSONArray(Params^[0])^.opt(Pinteger(Params^[1])^);
end;

//function optBoolean ( index : integer) : boolean; overload;
procedure TJSONArray_optBoolean(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.optBoolean(Pinteger(Params^[1])^);
end;

//function optBoolean ( index : integer; defaultValue : boolean) : boolean; overload;
procedure TJSONArray_optBooleanEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.optBoolean(Pinteger(Params^[1])^, Pboolean(Params^[2])^);
end;

//function optDouble (index : integer) : double; overload;
procedure TJSONArray_optDouble(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PJSONArray(Params^[0])^.optDouble(Pinteger(Params^[1])^);
end;

//function optDouble (index : integer; defaultValue :double ) : double ; overload;
procedure TJSONArray_optDoubleEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PJSONArray(Params^[0])^.optDouble(Pinteger(Params^[1])^, Pdouble(Params^[2])^);
end;

//function optInt (index : integer) : integer; overload;
procedure TJSONArray_optInt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.optInt(Pinteger(Params^[1])^);
end;

//function optInt (index : integer; defaultValue : integer) : integer; overload;
procedure TJSONArray_optIntEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.optInt(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function optJSONArray (index : integer) : TJSONArray ; overload;
procedure TJSONArray_optJSONArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.optJSONArray(Pinteger(Params^[1])^);
end;

//function optJSONObject (index : integer) : TJSONObject ; overload;
procedure TJSONArray_optJSONObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.optJSONObject(Pinteger(Params^[1])^);
end;

//function optString (index : integer) : string; overload;
procedure TJSONArray_optString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.optString(Pinteger(Params^[1])^);
end;

//function optString (index : integer; defaultValue : string) : string; overload;
procedure TJSONArray_optStringEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.optString(Pinteger(Params^[1])^, PlpString(Params^[2])^);
end;

//function put ( value : boolean) : TJSONArray; overload ;
procedure TJSONArray_put(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pboolean(Params^[1])^);
end;

//function put ( value : double ) : TJSONArray;   overload ;
procedure TJSONArray_putEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pdouble(Params^[1])^);
end;

//function put ( value : integer) : TJSONArray;   overload ;
procedure TJSONArray_putExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^);
end;

//function put ( value : TZAbstractObject) : TJSONArray;  overload ;
procedure TJSONArray_putExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(PZAbstractObject(Params^[1])^);
end;

//function put ( value: string): TJSONArray; overload;
procedure TJSONArray_putExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(PlpString(Params^[1])^);
end;

//function put ( index : integer ; value : boolean): TJSONArray;  overload ;
procedure TJSONArray_putExExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, Pboolean(Params^[2])^);
end;

//function put ( index : integer ; value : double) : TJSONArray;  overload ;
procedure TJSONArray_putExExExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, Pdouble(Params^[2])^);
end;

//function put ( index : integer ; value : integer) : TJSONArray;  overload ;
procedure TJSONArray_putExExExExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function put ( index : integer ; value : TZAbstractObject) : TJSONArray;  overload ;
procedure TJSONArray_putExExExExExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

//function put ( index: integer; value: string): TJSONArray; overload;
procedure TJSONArray_putExExExExExExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, PlpString(Params^[2])^);
end;

//function toJSONObject (names  :TJSONArray ) : TJSONObject ;  overload ;
procedure TJSONArray_toJSONObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.toJSONObject(PJSONArray(Params^[1])^);
end;

//function toString : string; overload; override;
procedure TJSONArray_toString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLpstring(Result)^ := PJSONArray(Params^[0])^.toString();
end;

//function toString (indentFactor : integer) : string; overload;
procedure TJSONArray_toStringEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.toString(Pinteger(Params^[1])^);
end;

//function toString (indentFactor, indent : integer) : string; overload;
procedure TJSONArray_toStringExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.toString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;


//constructor Create();
procedure TJSONArray_Init(const Params: PParamArray); lape_extdecl
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create();
end;

//procedure Free();
procedure TJSONArray_Free(const Params: PParamArray); lape_extdecl
begin
  PJSONArray(Params^[0])^.Free();
end;

procedure Register_TJSONArray(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TZAbstractObject','TObject');
    addClass('TJSONObject','TZAbstractObject');
    addClass('TJSONArray','TZAbstractObject');
    addGlobalFunc('procedure TJSONArray.Init(); overload;', @TJSONArray_create);
    addGlobalFunc('procedure TJSONArray.Init(s : string); overload;', @TJSONArray_createExExEx);
    addGlobalFunc('function TJSONArray.get(index : integer): Pointer; constref;', @TJSONArray_get);
    addGlobalFunc('function TJSONArray.getBoolean(index : integer): boolean; constref;', @TJSONArray_getBoolean);
    addGlobalFunc('function TJSONArray.getDouble(index : integer): double; constref;', @TJSONArray_getDouble);
    addGlobalFunc('function TJSONArray.getInt(index : integer): integer; constref;', @TJSONArray_getInt);
    addGlobalFunc('function TJSONArray.getJSONArray(index : integer): TJSONArray; constref;', @TJSONArray_getJSONArray);
    addGlobalFunc('function TJSONArray.getJSONObject(index : integer): TJsonObject; constref;', @TJSONArray_getJSONObject);
    addGlobalFunc('function TJSONArray.getString(index : integer): string; constref;', @TJSONArray_getString);
    addGlobalFunc('function TJSONArray.isNull(index : integer): boolean; constref;', @TJSONArray_isNull);
    addGlobalFunc('function TJSONArray.join(separator : string): string; constref;', @TJSONArray_join);
    addGlobalFunc('function TJSONArray.length(): integer; constref;', @TJSONArray_length);
    addGlobalFunc('function TJSONArray.opt(index : integer): Pointer; constref;', @TJSONArray_opt);
    addGlobalFunc('function TJSONArray.optBoolean( index : integer): boolean; constref;', @TJSONArray_optBoolean);
    addGlobalFunc('function TJSONArray.optBoolean( index : integer; defaultValue : boolean): boolean; constref; overload;', @TJSONArray_optBooleanEx);
    addGlobalFunc('function TJSONArray.optDouble(index : integer): double; constref;', @TJSONArray_optDouble);
    addGlobalFunc('function TJSONArray.optDouble(index : integer; defaultValue :double ): double; constref; overload;', @TJSONArray_optDoubleEx);
    addGlobalFunc('function TJSONArray.optInt(index : integer): integer; constref;', @TJSONArray_optInt);
    addGlobalFunc('function TJSONArray.optInt(index : integer; defaultValue : integer): integer; constref; overload;', @TJSONArray_optIntEx);
    addGlobalFunc('function TJSONArray.optJSONArray(index : integer): TJSONArray; constref;', @TJSONArray_optJSONArray);
    addGlobalFunc('function TJSONArray.optJSONObject(index : integer): TJSONObject; constref;', @TJSONArray_optJSONObject);
    addGlobalFunc('function TJSONArray.optString(index : integer): string; constref;', @TJSONArray_optString);
    addGlobalFunc('function TJSONArray.optString(index : integer; defaultValue : string): string; constref; overload;', @TJSONArray_optStringEx);
    addGlobalFunc('function TJSONArray.put( value : boolean): TJSONArray; constref; overload;', @TJSONArray_put);
    addGlobalFunc('function TJSONArray.put( value : double ): TJSONArray;   constref; overload; overload;', @TJSONArray_putEx);
    addGlobalFunc('function TJSONArray.put( value : integer): TJSONArray;   constref; overload; overload;', @TJSONArray_putExEx);
    addGlobalFunc('function TJSONArray.put( value : pointer): TJSONArray;  constref; overload; overload;', @TJSONArray_putExExEx);
    addGlobalFunc('function TJSONArray.put( value: string): TJSONArray; constref; overload;', @TJSONArray_putExExExEx);
    addGlobalFunc('function TJSONArray.put( index : integer ; value : boolean): TJSONArray;  constref; overload; overload;', @TJSONArray_putExExExExEx);
    addGlobalFunc('function TJSONArray.put( index : integer ; value : double): TJSONArray;  constref; overload; overload;', @TJSONArray_putExExExExExEx);
    addGlobalFunc('function TJSONArray.put( index : integer ; value : integer): TJSONArray;  constref; overload; overload;', @TJSONArray_putExExExExExExEx);
    addGlobalFunc('function TJSONArray.put( index : integer ; value : pointer): TJSONArray;  constref; overload; overload;', @TJSONArray_putExExExExExExExEx);
    addGlobalFunc('function TJSONArray.put( index: integer; value: string): TJSONArray; constref; overload;', @TJSONArray_putExExExExExExExExEx);
    addGlobalFunc('function TJSONArray.toJSONObject(names  :TJSONArray ): TJsonObject ;  constref; overload;', @TJSONArray_toJSONObject);
    addGlobalFunc('function TJSONArray.toString(): string; constref; overload; override;', @TJSONArray_toString);
    addGlobalFunc('function TJSONArray.toString(indentFactor : integer): string; constref; overload;', @TJSONArray_toStringEx);
    addGlobalFunc('function TJSONArray.toString(indentFactor, indent : integer): string; constref; overload;', @TJSONArray_toStringExEx);
    addGlobalFunc('procedure TJSONArray.Free();', @TJSONArray_Free);
  end;
end;

//constructor create;  overload;
procedure TJSONObject_create(const Params: PParamArray); lape_extdecl
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create();
end;

//constructor create (s : string); overload;
procedure TJSONObject_createExExExEx(const Params: PParamArray); lape_extdecl
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create(PlpString(Params^[1])^);
end;

//procedure clean;
procedure TJSONObject_clean(const Params: PParamArray); lape_extdecl
begin
  PJSONObject(Params^[0])^.clean();
end;

//function clone : pointer; override;
procedure TJSONObject_clone(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.clone();
end;

//function accumulate (key : string; value : pointer): TJSONObject;
procedure TJSONObject_accumulate(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.accumulate(PlpString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

//function get (key : string) : pointer;
procedure TJSONObject_get(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
 PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.get(PlpString(Params^[1])^);
end;

//function getBoolean (key : string): boolean;
procedure TJSONObject_getBoolean(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.getBoolean(PlpString(Params^[1])^);
end;

//function getDouble (key : string): double;
procedure TJSONObject_getDouble(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.getDouble(PlpString(Params^[1])^);
end;

//function getInt (key : string): integer;
procedure TJSONObject_getInt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.getInt(PlpString(Params^[1])^);
end;

//function getJSONArray (key : string) :TJSONArray;
procedure TJSONObject_getJSONArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.getJSONArray(PlpString(Params^[1])^);
end;

//function getJSONObject (key : string) : TJSONObject;
procedure TJSONObject_getJSONObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.getJSONObject(PlpString(Params^[1])^);
end;

//function getString (key : string): string;
procedure TJSONObject_getString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.getString(PlpString(Params^[1])^);
end;

//function has (key : string) : boolean;
procedure TJSONObject_has(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.has(PlpString(Params^[1])^);
end;

//function isNull (key : string) : boolean;
procedure TJSONObject_isNull(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.isNull(PlpString(Params^[1])^);
end;

//function keys : TStringList ;
procedure TJSONObject_keys(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStringList(Result)^ := PJSONObject(Params^[0])^.keys();
end;

//function length : integer;
procedure TJSONObject_length(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.length();
end;

//function names : TJSONArray;
procedure TJSONObject_names(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.names();
end;


//function opt (key : string) : pointer;
procedure TJSONObject_opt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PZabstractObject(Result)^ := PJSONObject(Params^[0])^.opt(PlpString(Params^[1])^);
end;

//function optBoolean (key : string): boolean; overload;
procedure TJSONObject_optBoolean(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.optBoolean(PlpString(Params^[1])^);
end;

//function optBoolean (key : string; defaultValue : boolean): boolean; overload;
procedure TJSONObject_optBooleanEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.optBoolean(PlpString(Params^[1])^, Pboolean(Params^[2])^);
end;

//function optDouble (key : string): double; overload;
procedure TJSONObject_optDouble(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.optDouble(PlpString(Params^[1])^);
end;

//function optDouble (key : string; defaultValue : double): double; overload;
procedure TJSONObject_optDoubleEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.optDouble(PlpString(Params^[1])^, Pdouble(Params^[2])^);
end;

//function optInt (key : string): integer; overload;
procedure TJSONObject_optInt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.optInt(PlpString(Params^[1])^);
end;

//function optInt (key : string; defaultValue : integer): integer; overload;
procedure TJSONObject_optIntEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.optInt(PlpString(Params^[1])^, Pinteger(Params^[2])^);
end;

//function optString (key : string): string; overload;
procedure TJSONObject_optString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.optString(PlpString(Params^[1])^);
end;

//function optString (key : string; defaultValue : string): string; overload;
procedure TJSONObject_optStringEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.optString(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function optJSONArray (key : string): TJSONArray; overload;
procedure TJSONObject_optJSONArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.optJSONArray(PlpString(Params^[1])^);
end;

//function optJSONObject (key : string): TJSONObject; overload;
procedure TJSONObject_optJSONObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.optJSONObject(PlpString(Params^[1])^);
end;

//function put (key : string; value : boolean): TJSONObject; overload;
procedure TJSONObject_put(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, Pboolean(Params^[2])^);
end;

//function put (key : string; value : double): TJSONObject; overload;
procedure TJSONObject_putEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, Pdouble(Params^[2])^);
end;

//function put (key : string; value : integer): TJSONObject; overload;
procedure TJSONObject_putExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, Pinteger(Params^[2])^);
end;

//function put (key : string; value : string): TJSONObject; overload;
procedure TJSONObject_putExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function put (key : string; value : pointer): TJSONObject; overload;
procedure TJSONObject_putExExExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

//function putOpt (key : string; value : pointer): TJSONObject;
procedure TJSONObject_putOpt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.putOpt(PlpString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

//function remove (key : string): pointer;
procedure TJSONObject_remove(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.remove(PlpString(Params^[1])^);
end;

//procedure assignTo(json: TJSONObject);
procedure TJSONObject_assignTo(const Params: PParamArray); lape_extdecl
begin
  PJSONObject(Params^[0])^.assignTo(PJSONObject(Params^[1])^);
end;

//function toJSONArray (names : TJSONArray) : TJSONArray;
procedure TJSONObject_toJSONArray(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.toJSONArray(PJSONArray(Params^[1])^);
end;

//function toString (): string ;  overload; override;
procedure TJSONObject_toString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlPstring (Result)^ := PJSONObject(Params^[0])^.toString();
end;

//function toString (indentFactor : integer): string; overload;
procedure TJSONObject_toStringEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.toString(Pinteger(Params^[1])^);
end;

//function toString (indentFactor, indent : integer): string; overload;
procedure TJSONObject_toStringExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.toString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//constructor Create();
procedure TJSONObject_Init(const Params: PParamArray); lape_extdecl
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create();
end;

//procedure Free();
procedure TJSONObject_Free(const Params: PParamArray); lape_extdecl
begin
  PJSONObject(Params^[0])^.Free();
end;

procedure Register_TJSONObject(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addGlobalFunc('procedure TJSONObject.Init(); overload;', @TJSONObject_create);
    addGlobalFunc('procedure TJSONObject.Init(s : string); overload;', @TJSONObject_createExExExEx);
    addGlobalFunc('procedure TJSONObject.clean(); constref;', @TJSONObject_clean);
//    addGlobalFunc('function TJSONObject.clone(): pointer; constref; override;', @TJSONObject_clone);
    addGlobalFunc('function TJSONObject.accumulate(key : string; value : pointer): TJSONObject; constref;', @TJSONObject_accumulate);
    addGlobalFunc('function TJSONObject.get(key : string): pointer; constref;', @TJSONObject_get);
    addGlobalFunc('function TJSONObject.getBoolean(key : string): boolean; constref;', @TJSONObject_getBoolean);
    addGlobalFunc('function TJSONObject.getDouble(key : string): double; constref;', @TJSONObject_getDouble);
    addGlobalFunc('function TJSONObject.getInt(key : string): integer; constref;', @TJSONObject_getInt);
    addGlobalFunc('function TJSONObject.getJSONArray(key : string): TJSONArray; constref;', @TJSONObject_getJSONArray);
    addGlobalFunc('function TJSONObject.getJSONObject(key : string): TJSONObject; constref;', @TJSONObject_getJSONObject);
    addGlobalFunc('function TJSONObject.getString(key : string): string; constref;', @TJSONObject_getString);
    addGlobalFunc('function TJSONObject.has(key : string): boolean; constref;', @TJSONObject_has);
    addGlobalFunc('function TJSONObject.isNull(key : string): boolean; constref;', @TJSONObject_isNull);
    addGlobalFunc('function TJSONObject.keys(): TStringList; constref;', @TJSONObject_keys);
    addGlobalFunc('function TJSONObject.length(): integer; constref;', @TJSONObject_length);
    addGlobalFunc('function TJSONObject.names(): TJSONArray; constref;', @TJSONObject_names);
    addGlobalFunc('function TJSONObject.opt(key : string): pointer; constref;', @TJSONObject_opt);
    addGlobalFunc('function TJSONObject.optBoolean(key : string): boolean; constref;', @TJSONObject_optBoolean);
    addGlobalFunc('function TJSONObject.optBoolean(key : string; defaultValue : boolean): boolean; constref; overload;', @TJSONObject_optBooleanEx);
    addGlobalFunc('function TJSONObject.optDouble(key : string): double; constref;', @TJSONObject_optDouble);
    addGlobalFunc('function TJSONObject.optDouble(key : string; defaultValue : double): double; constref; overload;', @TJSONObject_optDoubleEx);
    addGlobalFunc('function TJSONObject.optInt(key : string): integer; constref;', @TJSONObject_optInt);
    addGlobalFunc('function TJSONObject.optInt(key : string; defaultValue : integer): integer; constref; overload;', @TJSONObject_optIntEx);
    addGlobalFunc('function TJSONObject.optString(key : string): string; constref;', @TJSONObject_optString);
    addGlobalFunc('function TJSONObject.optString(key : string; defaultValue : string): string; constref; overload;', @TJSONObject_optStringEx);
    addGlobalFunc('function TJSONObject.optJSONArray(key : string): TJSONArray; constref;', @TJSONObject_optJSONArray);
    addGlobalFunc('function TJSONObject.optJSONObject(key : string): TJSONObject; constref;', @TJSONObject_optJSONObject);
    addGlobalFunc('function TJSONObject.put(key : string; value : boolean): TJSONObject; constref;', @TJSONObject_put);
    addGlobalFunc('function TJSONObject.put(key : string; value : double): TJSONObject; constref; overload;', @TJSONObject_putEx);
    addGlobalFunc('function TJSONObject.put(key : string; value : integer): TJSONObject; constref; overload;', @TJSONObject_putExEx);
    addGlobalFunc('function TJSONObject.put(key : string; value : string): TJSONObject; constref; overload;', @TJSONObject_putExExEx);
    addGlobalFunc('function TJSONObject.put(key : string; value : pointer): TJSONObject; constref; overload;', @TJSONObject_putExExExEx);
    addGlobalFunc('function TJSONObject.putOpt(key : string; value : pointer): TJSONObject; constref;', @TJSONObject_putOpt);
    addGlobalFunc('function TJSONObject.remove(key : string): pointer; constref;', @TJSONObject_remove);
    addGlobalFunc('procedure TJSONObject.assignTo(json: TJSONObject); constref;', @TJSONObject_assignTo);
    addGlobalFunc('function TJSONObject.toJSONArray(names : TJSONArray): TJSONArray; constref;', @TJSONObject_toJSONArray);
    addGlobalFunc('function TJSONObject.toString(): string ;  constref; overload; override;', @TJSONObject_toString);
    addGlobalFunc('function TJSONObject.toString(indentFactor : integer): string; constref; overload;', @TJSONObject_toStringEx);
    addGlobalFunc('function TJSONObject.toString(indentFactor, indent : integer): string; constref; overload;', @TJSONObject_toStringExEx);
    addGlobalFunc('procedure TJSONObject.Free();', @TJSONObject_Free);
  end;
end;

Procedure Register_JSON(Compiler: TLapeCompiler);
begin
  Register_TJSONArray(Compiler);
  Register_TJSONObject(Compiler);
end;


end.

