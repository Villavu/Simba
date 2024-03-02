{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.theme;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Forms,
  ATScrollBar;

type
  TSimbaTheme = class
  protected
    procedure DoFormAdded(Sender: TObject; Form: TCustomForm);

    function GetScrollBarArrowSize: Integer;
    function GetScrollBarSize: Integer;

    procedure SetScrollBarArrowSize(Value: Integer);
    procedure SetScrollBarSize(Value: Integer);
  public
    ColorBackground: TColor;
    ColorFrame: TColor;
    ColorActive: TColor;
    ColorScrollBarActive: TColor;
    ColorScrollBarInActive: TColor;

    ColorFont: TColor;
    ColorLine: TColor;

    procedure AddNativeWindowColoring(Sender: TObject);

    property ScrollBarSize: Integer read GetScrollBarSize write SetScrollBarSize;
    property ScrollBarArrowSize: Integer read GetScrollBarArrowSize write SetScrollBarArrowSize;

    constructor Create;
  end;

var
  SimbaTheme: TSimbaTheme;

implementation

{$IFDEF WINDOWS}
uses
  DwmApi;
{$ENDIF}

procedure TSimbaTheme.DoFormAdded(Sender: TObject; Form: TCustomForm);
begin
  Form.AddHandlerOnVisibleChanged(@AddNativeWindowColoring);
end;

function TSimbaTheme.GetScrollBarArrowSize: Integer;
begin
  Result := ATScrollbarTheme.ArrowSize;
end;

function TSimbaTheme.GetScrollBarSize: Integer;
begin
  Result := ATScrollbarTheme.InitialSize;
end;

procedure TSimbaTheme.SetScrollBarArrowSize(Value: Integer);
begin
  ATScrollbarTheme.ArrowSize := Value;
end;

procedure TSimbaTheme.SetScrollBarSize(Value: Integer);
begin
  ATScrollbarTheme.InitialSize := Value;
end;

procedure TSimbaTheme.AddNativeWindowColoring(Sender: TObject);
{$IFDEF WINDOWS}
const
  DWMWA_CAPTION_COLOR = 35;
begin
  if not TCustomForm(Sender).Visible then
    Exit;

  // DWMWA_CAPTION_COLOR (Sadly need windows 11)
  if (Win32BuildNumber >= 22000) and (Sender is TCustomForm) and Assigned(DwmSetWindowAttribute) then
    DwmSetWindowAttribute(TCustomForm(Sender).Handle, DWMWA_CAPTION_COLOR, @ColorFrame, SizeOf(TColor));
end;
{$ELSE}
begin
end;
{$ENDIF}

constructor TSimbaTheme.Create;
begin
  ColorFrame := $262628;
  ColorBackground := $1C1E1E;
  ColorActive := $854F31;
  ColorScrollBarActive := $414346;
  ColorScrollBarInActive := $2D2E2F;
  ColorLine := $657076;
  ColorFont := $F2F2F2;

  with ATScrollbarTheme do
  begin
    InitialSize := 14;
    ThumbMinSize := 24;
    ThumbRoundedRect := False;
    DirectJumpOnClickPageUpDown := True;

    ColorCorner := ColorFrame;
    ColorBG := ColorScrollBarInActive;
    ColorThumbBorder := ColorScrollBarActive;
    ColorThumbFill := ColorScrollBarActive;
    ColorThumbFillOver := ColorScrollBarActive;
    ColorThumbFillPressed := ColorScrollBarActive;
    ColorThumbDecor := ColorScrollBarActive;
    ColorArrowFill := ColorScrollBarActive;
    ColorArrowBorder := ColorScrollBarActive;
    ColorArrowSign := ColorLine;

    ColorArrowFillOver := ColorScrollBarActive;
    ColorArrowFillPressed := ColorScrollBarActive;
  end;

  Screen.AddHandlerFormAdded(@Self.DoFormAdded);
end;

initialization
  SimbaTheme := TSimbaTheme.Create();

end.

