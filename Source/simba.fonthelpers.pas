{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.fonthelpers;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls;

function GetDefaultFontSize: Integer;
function GetFontSize(Control: TWinControl; IncAmount: Integer = 0): Integer;
function IsFontFixed(FontName: String): Boolean;
function GetFixedFonts: TStringArray;

function GetDefaultFontName: String;

implementation

uses
  Graphics, LCLIntf, LCLType;

// Font size can be zero, so this is needed!
function GetDefaultFontSize: Integer;
begin
  with TBitmap.Create() do
  try
    Result := Round(Abs(GetFontData(Canvas.Font.Reference.Handle).Height) * 72 / Canvas.Font.PixelsPerInch);
  finally
    Free();
  end;
end;

// Font size can be zero, so this is needed!
function GetFontSize(Control: TWinControl; IncAmount: Integer): Integer;
begin
  Result := Round(Abs(GetFontData(Control.Font.Handle).Height) * 72 / Control.Font.PixelsPerInch) + IncAmount;
end;

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

function IsFontFixed(FontName: String): Boolean;
var
  DC: HDC;
  Font: TLogFont;
begin
  Result := False;
  if (FontName = '') then
    Exit;

  Font := Default(TLogFont);
  Font.lfCharSet := DEFAULT_CHARSET;
  Font.lfFaceName := PChar(FontName);
  Font.lfPitchAndFamily := {$IFDEF LINUX}FIXED_PITCH{$ELSE}0{$ENDIF};

  DC := GetDC(0);

  try
    Result := EnumFontFamiliesEx(DC, @Font, @FontIsPitched, 0, 0) = 0;
  finally
    ReleaseDC(0, DC);
  end;
end;

function GetFixedFonts: TStringArray;
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

function GetDefaultFontName: String;
begin
  with TBitmap.Create() do
  try
    Result := GetFontData(Canvas.Font.Reference.Handle).Name;
  finally
    Free();
  end;
end;

end.

