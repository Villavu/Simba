unit lplclspin;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpCompiler, lpTypes, lpClassHelper;

procedure RegisterLCLSpinCtrls(Compiler: TLapeCompiler);

implementation
  uses MufasaTypes, lplclsystem, Spin;

type
  PCustomFloatSpinEdit = ^TCustomFloatSpinEdit;
  PFloatSpinEdit = ^TFloatSpinEdit;
  PSpinEdit = ^TSpinEdit;
  PCustomSpinEdit = ^TCustomSpinEdit;

//function GetLimitedValue(const AValue: Double): Double; virtual;
procedure TCustomFloatSpinEdit_GetLimitedValue(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.GetLimitedValue(PDouble(Params^[1])^);
end;

//function ValueToStr(const AValue: Double): String; virtual;
procedure TCustomFloatSpinEdit_ValueToStr(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueToStr(PDouble(Params^[1])^);
end;

//function StrToValue(const S: String): Double; virtual;
procedure TCustomFloatSpinEdit_StrToValue(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.StrToValue(PlpString(Params^[1])^);
end;

//Read: property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
procedure TCustomFloatSpinEdit_DecimalPlaces_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces;
end;

//Write: property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
procedure TCustomFloatSpinEdit_DecimalPlaces_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces := PInteger(Params^[1])^;
end;

//Read: property Increment: Double read FIncrement write SetIncrement;
procedure TCustomFloatSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Increment;
end;

//Write: property Increment: Double read FIncrement write SetIncrement;
procedure TCustomFloatSpinEdit_Increment_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.Increment := PDouble(Params^[1])^;
end;

//Read: property MinValue: Double read FMinValue write SetMinValue;
procedure TCustomFloatSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MinValue;
end;

//Write: property MinValue: Double read FMinValue write SetMinValue;
procedure TCustomFloatSpinEdit_MinValue_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.MinValue := PDouble(Params^[1])^;
end;

//Read: property MaxValue: Double read FMaxValue write SetMaxValue;
procedure TCustomFloatSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MaxValue;
end;

//Write: property MaxValue: Double read FMaxValue write SetMaxValue;
procedure TCustomFloatSpinEdit_MaxValue_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.MaxValue := PDouble(Params^[1])^;
end;

//Read: property Value: Double read GetValue write SetValue;
procedure TCustomFloatSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Value;
end;

//Write: property Value: Double read GetValue write SetValue;
procedure TCustomFloatSpinEdit_Value_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.Value := PDouble(Params^[1])^;
end;

//Read: property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
procedure TCustomFloatSpinEdit_ValueEmpty_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueEmpty;
end;

//Write: property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
procedure TCustomFloatSpinEdit_ValueEmpty_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.ValueEmpty := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure TCustomFloatSpinEdit_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^ := TCustomFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomFloatSpinEdit_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomFloatSpinEdit(Params^[0])^.Free();
end;

procedure Register_TCustomFloatSpinEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomFloatSpinEdit', 'TCustomEdit');

    addGlobalFunc('function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double;', @TCustomFloatSpinEdit_GetLimitedValue);
    addGlobalFunc('function TCustomFloatSpinEdit.ValueToStr(const AValue: Double): String;', @TCustomFloatSpinEdit_ValueToStr);
    addGlobalFunc('function TCustomFloatSpinEdit.StrToValue(const S: String): Double;', @TCustomFloatSpinEdit_StrToValue);
    addClassVar('TCustomFloatSpinEdit', 'DecimalPlaces', 'Integer', @TCustomFloatSpinEdit_DecimalPlaces_Read, @TCustomFloatSpinEdit_DecimalPlaces_Write);
    addClassVar('TCustomFloatSpinEdit', 'Increment', 'Double', @TCustomFloatSpinEdit_Increment_Read, @TCustomFloatSpinEdit_Increment_Write);
    addClassVar('TCustomFloatSpinEdit', 'MinValue', 'Double', @TCustomFloatSpinEdit_MinValue_Read, @TCustomFloatSpinEdit_MinValue_Write);
    addClassVar('TCustomFloatSpinEdit', 'MaxValue', 'Double', @TCustomFloatSpinEdit_MaxValue_Read, @TCustomFloatSpinEdit_MaxValue_Write);
    addClassVar('TCustomFloatSpinEdit', 'Value', 'Double', @TCustomFloatSpinEdit_Value_Read, @TCustomFloatSpinEdit_Value_Write);
    addClassVar('TCustomFloatSpinEdit', 'ValueEmpty', 'Boolean', @TCustomFloatSpinEdit_ValueEmpty_Read, @TCustomFloatSpinEdit_ValueEmpty_Write);
    addGlobalFunc('procedure TCustomFloatSpinEdit.Init(TheOwner: TComponent);', @TCustomFloatSpinEdit_Init);
    addGlobalFunc('procedure TCustomFloatSpinEdit.Free();', @TCustomFloatSpinEdit_Free);
  end;
end;

//constructor Create();
procedure TFloatSpinEdit_Init(const Params: PParamArray); lape_extdecl
begin
  PFloatSpinEdit(Params^[0])^ := TFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TFloatSpinEdit_Free(const Params: PParamArray); lape_extdecl
begin
  PFloatSpinEdit(Params^[0])^.Free();
end;

procedure Register_TFloatSpinEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TFloatSpinEdit', 'TCustomFloatSpinEdit');

    addGlobalFunc('procedure TFloatSpinEdit.Init(TheOwner: TComponent);', @TFloatSpinEdit_Init);
    addGlobalFunc('procedure TFloatSpinEdit.Free();', @TFloatSpinEdit_Free);
  end;
end;

//Read: property Value: integer read GetValue write SetValue default 0;
procedure TCustomSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Value;
end;

//Write: property Value: integer read GetValue write SetValue default 0;
procedure TCustomSpinEdit_Value_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomSpinEdit(Params^[0])^.Value := Pinteger(Params^[1])^;
end;

//Read: property MinValue: integer read GetMinValue write SetMinValue default 0;
procedure TCustomSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MinValue;
end;

//Write: property MinValue: integer read GetMinValue write SetMinValue default 0;
procedure TCustomSpinEdit_MinValue_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomSpinEdit(Params^[0])^.MinValue := Pinteger(Params^[1])^;
end;

//Read: property MaxValue: integer read GetMaxValue write SetMaxValue default 100;
procedure TCustomSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MaxValue;
end;

//Write: property MaxValue: integer read GetMaxValue write SetMaxValue default 100;
procedure TCustomSpinEdit_MaxValue_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomSpinEdit(Params^[0])^.MaxValue := Pinteger(Params^[1])^;
end;

//Read: property Increment: integer read GetIncrement write SetIncrement default 1;
procedure TCustomSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Increment;
end;

//Write: property Increment: integer read GetIncrement write SetIncrement default 1;
procedure TCustomSpinEdit_Increment_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomSpinEdit(Params^[0])^.Increment := Pinteger(Params^[1])^;
end;

//constructor Create();
procedure TCustomSpinEdit_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomSpinEdit(Params^[0])^ := TCustomSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomSpinEdit_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomSpinEdit(Params^[0])^.Free();
end;

procedure Register_TCustomSpinEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomSpinEdit', 'TCustomFloatSpinEdit');

    addClassVar('TCustomSpinEdit', 'Value', 'integer', @TCustomSpinEdit_Value_Read, @TCustomSpinEdit_Value_Write);
    addClassVar('TCustomSpinEdit', 'MinValue', 'integer', @TCustomSpinEdit_MinValue_Read, @TCustomSpinEdit_MinValue_Write);
    addClassVar('TCustomSpinEdit', 'MaxValue', 'integer', @TCustomSpinEdit_MaxValue_Read, @TCustomSpinEdit_MaxValue_Write);
    addClassVar('TCustomSpinEdit', 'Increment', 'integer', @TCustomSpinEdit_Increment_Read, @TCustomSpinEdit_Increment_Write);
    addGlobalFunc('procedure TCustomSpinEdit.Init(TheOwner: TComponent);', @TCustomSpinEdit_Init);
    addGlobalFunc('procedure TCustomSpinEdit.Free();', @TCustomSpinEdit_Free);
  end;
end;

//constructor Create();
procedure TSpinEdit_Init(const Params: PParamArray); lape_extdecl
begin
  PSpinEdit(Params^[0])^ := TSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TSpinEdit_Free(const Params: PParamArray); lape_extdecl
begin
  PSpinEdit(Params^[0])^.Free();
end;

procedure Register_TSpinEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TSpinEdit', 'TCustomSpinEdit');

    addGlobalFunc('procedure TSpinEdit.Init(TheOwner: TComponent);', @TSpinEdit_Init);
    addGlobalFunc('procedure TSpinEdit.Free();', @TSpinEdit_Free);
  end;
end;

procedure RegisterLCLSpinCtrls(Compiler: TLapeCompiler);
begin
  Register_TCustomFloatSpinEdit(Compiler);
  Register_TFloatSpinEdit(Compiler);
  Register_TCustomSpinEdit(Compiler);
  Register_TSpinEdit(Compiler);
end;

end.

