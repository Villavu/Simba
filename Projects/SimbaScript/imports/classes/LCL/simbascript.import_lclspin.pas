unit simbascript.import_lclspin;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLSpinCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  spin;

type
  PComponent = ^TComponent;
  PCustomFloatSpinEdit = ^TCustomFloatSpinEdit;
  PFloatSpinEdit = ^TFloatSpinEdit;
  PSpinEdit = ^TSpinEdit;
  PCustomSpinEdit = ^TCustomSpinEdit;

//function GetLimitedValue(const AValue: Double): Double; virtual;
procedure Lape_TCustomFloatSpinEdit_GetLimitedValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.GetLimitedValue(PDouble(Params^[1])^);
end;

//function ValueToStr(const AValue: Double): String; virtual;
procedure Lape_TCustomFloatSpinEdit_ValueToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueToStr(PDouble(Params^[1])^);
end;

//function StrToValue(const S: String): Double; virtual;
procedure Lape_TCustomFloatSpinEdit_StrToValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.StrToValue(PlpString(Params^[1])^);
end;

//Read: property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
procedure Lape_TCustomFloatSpinEdit_DecimalPlaces_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces;
end;

//Write: property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
procedure Lape_TCustomFloatSpinEdit_DecimalPlaces_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces := PInteger(Params^[1])^;
end;

//Read: property Increment: Double read FIncrement write SetIncrement;
procedure Lape_TCustomFloatSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Increment;
end;

//Write: property Increment: Double read FIncrement write SetIncrement;
procedure Lape_TCustomFloatSpinEdit_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Increment := PDouble(Params^[1])^;
end;

//Read: property MinValue: Double read FMinValue write SetMinValue;
procedure Lape_TCustomFloatSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MinValue;
end;

//Write: property MinValue: Double read FMinValue write SetMinValue;
procedure Lape_TCustomFloatSpinEdit_MinValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.MinValue := PDouble(Params^[1])^;
end;

//Read: property MaxValue: Double read FMaxValue write SetMaxValue;
procedure Lape_TCustomFloatSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MaxValue;
end;

//Write: property MaxValue: Double read FMaxValue write SetMaxValue;
procedure Lape_TCustomFloatSpinEdit_MaxValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.MaxValue := PDouble(Params^[1])^;
end;

//Read: property Value: Double read GetValue write SetValue;
procedure Lape_TCustomFloatSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Value;
end;

//Write: property Value: Double read GetValue write SetValue;
procedure Lape_TCustomFloatSpinEdit_Value_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Value := PDouble(Params^[1])^;
end;

//Read: property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
procedure Lape_TCustomFloatSpinEdit_ValueEmpty_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueEmpty;
end;

//Write: property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
procedure Lape_TCustomFloatSpinEdit_ValueEmpty_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.ValueEmpty := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCustomFloatSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^ := TCustomFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomFloatSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomFloatSpinEdit(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomFloatSpinEdit', 'TCustomEdit');

    addGlobalFunc('function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double; constref;', @Lape_TCustomFloatSpinEdit_GetLimitedValue);
    addGlobalFunc('function TCustomFloatSpinEdit.ValueToStr(const AValue: Double): String; constref;', @Lape_TCustomFloatSpinEdit_ValueToStr);
    addGlobalFunc('function TCustomFloatSpinEdit.StrToValue(const S: String): Double; constref;', @Lape_TCustomFloatSpinEdit_StrToValue);
    addClassVar('TCustomFloatSpinEdit', 'DecimalPlaces', 'Integer', @Lape_TCustomFloatSpinEdit_DecimalPlaces_Read, @Lape_TCustomFloatSpinEdit_DecimalPlaces_Write);
    addClassVar('TCustomFloatSpinEdit', 'Increment', 'Double', @Lape_TCustomFloatSpinEdit_Increment_Read, @Lape_TCustomFloatSpinEdit_Increment_Write);
    addClassVar('TCustomFloatSpinEdit', 'MinValue', 'Double', @Lape_TCustomFloatSpinEdit_MinValue_Read, @Lape_TCustomFloatSpinEdit_MinValue_Write);
    addClassVar('TCustomFloatSpinEdit', 'MaxValue', 'Double', @Lape_TCustomFloatSpinEdit_MaxValue_Read, @Lape_TCustomFloatSpinEdit_MaxValue_Write);
    addClassVar('TCustomFloatSpinEdit', 'Value', 'Double', @Lape_TCustomFloatSpinEdit_Value_Read, @Lape_TCustomFloatSpinEdit_Value_Write);
    addClassVar('TCustomFloatSpinEdit', 'ValueEmpty', 'Boolean', @Lape_TCustomFloatSpinEdit_ValueEmpty_Read, @Lape_TCustomFloatSpinEdit_ValueEmpty_Write);
    addGlobalFunc('procedure TCustomFloatSpinEdit.Init(TheOwner: TComponent); override;', @Lape_TCustomFloatSpinEdit_Init);
    //addGlobalFunc('procedure TCustomFloatSpinEdit.Free(); constref;', @Lape_TCustomFloatSpinEdit_Free);
  end;
end;

//constructor Create();
procedure Lape_TFloatSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFloatSpinEdit(Params^[0])^ := TFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TFloatSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFloatSpinEdit(Params^[0])^.Free();
end;

procedure Lape_Import_TFloatSpinEdit(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TFloatSpinEdit', 'TCustomFloatSpinEdit');

    addGlobalFunc('procedure TFloatSpinEdit.Init(TheOwner: TComponent); override;', @Lape_TFloatSpinEdit_Init);
    //addGlobalFunc('procedure TFloatSpinEdit.Free(); constref;', @Lape_TFloatSpinEdit_Free);
  end;
end;

//Read: property Value: integer read GetValue write SetValue default 0;
procedure Lape_TCustomSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Value;
end;

//Write: property Value: integer read GetValue write SetValue default 0;
procedure Lape_TCustomSpinEdit_Value_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Value := Pinteger(Params^[1])^;
end;

//Read: property MinValue: integer read GetMinValue write SetMinValue default 0;
procedure Lape_TCustomSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MinValue;
end;

//Write: property MinValue: integer read GetMinValue write SetMinValue default 0;
procedure Lape_TCustomSpinEdit_MinValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.MinValue := Pinteger(Params^[1])^;
end;

//Read: property MaxValue: integer read GetMaxValue write SetMaxValue default 100;
procedure Lape_TCustomSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MaxValue;
end;

//Write: property MaxValue: integer read GetMaxValue write SetMaxValue default 100;
procedure Lape_TCustomSpinEdit_MaxValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.MaxValue := Pinteger(Params^[1])^;
end;

//Read: property Increment: integer read GetIncrement write SetIncrement default 1;
procedure Lape_TCustomSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Increment;
end;

//Write: property Increment: integer read GetIncrement write SetIncrement default 1;
procedure Lape_TCustomSpinEdit_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Increment := Pinteger(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCustomSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^ := TCustomSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomSpinEdit(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomSpinEdit', 'TCustomFloatSpinEdit');

    addClassVar('TCustomSpinEdit', 'Value', 'integer', @Lape_TCustomSpinEdit_Value_Read, @Lape_TCustomSpinEdit_Value_Write);
    addClassVar('TCustomSpinEdit', 'MinValue', 'integer', @Lape_TCustomSpinEdit_MinValue_Read, @Lape_TCustomSpinEdit_MinValue_Write);
    addClassVar('TCustomSpinEdit', 'MaxValue', 'integer', @Lape_TCustomSpinEdit_MaxValue_Read, @Lape_TCustomSpinEdit_MaxValue_Write);
    addClassVar('TCustomSpinEdit', 'Increment', 'integer', @Lape_TCustomSpinEdit_Increment_Read, @Lape_TCustomSpinEdit_Increment_Write);
    addGlobalFunc('procedure TCustomSpinEdit.Init(TheOwner: TComponent); override;', @Lape_TCustomSpinEdit_Init);
    //addGlobalFunc('procedure TCustomSpinEdit.Free(); constref;', @Lape_TCustomSpinEdit_Free);
  end;
end;

//constructor Create();
procedure Lape_TSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSpinEdit(Params^[0])^ := TSpinEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSpinEdit(Params^[0])^.Free();
end;

procedure Lape_Import_TSpinEdit(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TSpinEdit', 'TCustomSpinEdit');

    addGlobalFunc('procedure TSpinEdit.Init(TheOwner: TComponent); override;', @Lape_TSpinEdit_Init);
   // addGlobalFunc('procedure TSpinEdit.Free(); constref;', @Lape_TSpinEdit_Free);
  end;
end;

procedure Lape_Import_LCLSpinCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  Lape_Import_TCustomFloatSpinEdit(Compiler);
  Lape_Import_TFloatSpinEdit(Compiler);
  Lape_Import_TCustomSpinEdit(Compiler);
  Lape_Import_TSpinEdit(Compiler);
end;

end.

