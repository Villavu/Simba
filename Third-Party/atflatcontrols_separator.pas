{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATFlatControls_Separator;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$endif}

interface

uses
  {$ifndef FPC}
  System.Types,
  {$endif}
  SysUtils, StrUtils;

type
  { TATStringSeparator }

  TATStringSeparator = record
  private
    FSep: char;
    FStr: string;
    FPos: integer;
  public
    procedure Init(const AStr: string; ASep: char=',');
    function GetRest(out AValue: string): boolean; overload;
    function GetItemStr(out AValue: string): boolean; overload;
    //function GetItemStr(out AValue: UnicodeString): boolean; overload;
    function GetItemInt(out AValue: integer; const ADefault: integer): boolean; overload;
    function GetItemInt(out AValue: integer; const ADefault, AMin, AMax: integer): boolean; overload;
    //function GetItemInt64(out AValue: Int64; const ADefault: Int64): boolean;
    //function GetItemDWord(out AValue: DWord; const ADefault: DWord): boolean;
  end;

implementation

{ TATStringSeparator }

procedure TATStringSeparator.Init(const AStr: string; ASep: char);
begin
  FStr:= AStr;
  FSep:= ASep;
  FPos:= 1;
end;

function TATStringSeparator.GetRest(out AValue: string): boolean;
begin
  AValue:= Copy(FStr, FPos, MaxInt);
  Result:= true;
end;

function TATStringSeparator.GetItemStr(out AValue: string): boolean;
var
  N: integer;
begin
  if FPos>Length(FStr) then
  begin
    AValue:= '';
    exit(false);
  end;
  N:= PosEx(FSep, FStr, FPos);
  if N=0 then
    N:= Length(FStr)+1;
  AValue:= Copy(FStr, FPos, N-FPos);
  FPos:= N+1;
  Result:= true;
end;

{
function TATStringSeparator.GetItemStr(out AValue: UnicodeString): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  AValue:= SVal;
end;
}

function TATStringSeparator.GetItemInt(out AValue: integer; const ADefault: integer): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  if Result then
    AValue:= StrToIntDef(SVal, ADefault)
  else
    AValue:= ADefault;
end;

function TATStringSeparator.GetItemInt(out AValue: integer; const ADefault, AMin, AMax: integer): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  if Result then
  begin
    AValue:= StrToIntDef(SVal, ADefault);
    if AValue<AMin then
      AValue:= AMin;
    if AValue>AMax then
      AValue:= AMax;
  end
  else
    AValue:= ADefault;
end;

{
function TATStringSeparator.GetItemInt64(out AValue: Int64; const ADefault: Int64): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  if Result then
    AValue:= StrToInt64Def(SVal, ADefault)
  else
    AValue:= ADefault;
end;
}

{
function TATStringSeparator.GetItemDWord(out AValue: DWord; const ADefault: DWord): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  if Result then
    AValue:= StrToDWordDef(SVal, ADefault)
  else
    AValue:= ADefault;
end;
}

end.
