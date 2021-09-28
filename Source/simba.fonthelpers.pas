unit simba.fonthelpers;

{$mode ObjFPC}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  TSimbaFontHelpers = record
    function IsFontFixed(Name: String): Boolean;
    function GetFixedFonts: TStringArray;
  end;

var
  SimbaFontHelpers: TSimbaFontHelpers;

implementation

uses
  LCLIntf, LCLType;

function FontIsPitched(var Font: TEnumLogFontEx; var Metric: TNewTextMetricEx; FontType: LongInt; Data: LParam): LongInt; stdcall;
begin
  Result := 1;

  with Font.elfLogFont do
    if (lfPitchAndFamily and FIXED_PITCH) = FIXED_PITCH then
      Result := 0; // Stop enumeration
end;

function FontIsPitchedGetName(var Font: TEnumLogFontEx; var Metric: TNewTextMetricEx; FontType: LongInt; Data: LParam): LongInt; stdcall;
begin
  Result := 1;

  with Font.elfLogFont do
    if (lfPitchAndFamily and FIXED_PITCH) = FIXED_PITCH then
      TStringList(PtrUInt(Data)).Add(lfFaceName);
end;

function TSimbaFontHelpers.IsFontFixed(Name: String): Boolean;
var
  DC: HDC;
  Font: TLogFont;
begin
  Result := False;
  if (Name = '') then
    Exit;

  Font := Default(TLogFont);
  Font.lfCharSet := DEFAULT_CHARSET;
  Font.lfFaceName := PChar(Name);
  Font.lfPitchAndFamily := {$IFDEF LINUX}FIXED_PITCH{$ELSE}0{$ENDIF};

  DC := GetDC(0);

  try
    Result := EnumFontFamiliesEx(DC, @Font, @FontIsPitched, 0, 0) = 0;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TSimbaFontHelpers.GetFixedFonts: TStringArray;
var
  DC: HDC;
  Font: TLogFont;
  Strings: TStringList;
begin
  Result := nil;

  Strings := TStringList.Create();
  Strings.Sorted := True;
  Strings.Duplicates := dupIgnore;

  try
    Font := Default(TLogFont);
    Font.lfCharSet := DEFAULT_CHARSET;
    Font.lfFaceName := '';
    Font.lfPitchAndFamily := {$IFDEF LINUX}FIXED_PITCH{$ELSE}0{$ENDIF};

    DC := GetDC(0);
    try
      EnumFontFamiliesEx(DC, @Font, @FontIsPitchedGetName, PtrUInt(Strings), 0);
    finally
      ReleaseDC(0, DC);
    end;

    Result := Strings.ToStringArray();
  finally
    Strings.Free();
  end;
end;


end.

