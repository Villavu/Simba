{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_theme;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Graphics;

type
  TSimbaComponentTheme = class(TObject)
  private
    {$IFDEF WINDOWS}
    procedure DoFormAdded(Sender: TObject; Form: TCustomForm);
    procedure DoWindowsFrameColoring(Sender: TObject);
    {$ENDIF}

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

    property ScrollBarSize: Integer read GetScrollBarSize write SetScrollBarSize;
    property ScrollBarArrowSize: Integer read GetScrollBarArrowSize write SetScrollBarArrowSize;

    constructor Create;
  end;

var
  SimbaComponentTheme: TSimbaComponentTheme;

implementation

uses
  {$IFDEF WINDOWS}
  DwmApi,
  {$ENDIF}
  ATScrollBar,
  LCLType,
  simba.initializations;

{$IFDEF WINDOWS}
procedure TSimbaComponentTheme.DoFormAdded(Sender: TObject; Form: TCustomForm);
begin
  // seems to need applying everytime a window is made visible
  Form.AddHandlerOnVisibleChanged(@DoWindowsFrameColoring);
end;

procedure TSimbaComponentTheme.DoWindowsFrameColoring(Sender: TObject);
const
  DWMWA_CAPTION_COLOR = 35;
begin
  if not TCustomForm(Sender).Visible then
    Exit;

  // DWMWA_CAPTION_COLOR (Sadly need windows 11)
  if (Win32BuildNumber >= 22000) and (Sender is TCustomForm) and Assigned(DwmSetWindowAttribute) then
    DwmSetWindowAttribute(TCustomForm(Sender).Handle, DWMWA_CAPTION_COLOR, @ColorFrame, SizeOf(TColor));
end;
{$ENDIF}

function TSimbaComponentTheme.GetScrollBarArrowSize: Integer;
begin
  Result := ATScrollbarTheme.ArrowSize;
end;

function TSimbaComponentTheme.GetScrollBarSize: Integer;
begin
  Result := ATScrollbarTheme.InitialSize;
end;

procedure TSimbaComponentTheme.SetScrollBarArrowSize(Value: Integer);
begin
  ATScrollbarTheme.ArrowSize := Value;
end;

procedure TSimbaComponentTheme.SetScrollBarSize(Value: Integer);
begin
  ATScrollbarTheme.InitialSize := Value;
end;

constructor TSimbaComponentTheme.Create;
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
    InitialSize := 18;
    ThumbMinSize := 60;
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

  {$IFDEF WINDOWS}
  Screen.AddHandlerFormAdded(@Self.DoFormAdded);
  {$ENDIF}
end;

procedure DoCreate;
begin
  SimbaComponentTheme := TSimbaComponentTheme.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaComponentTheme);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.CREATE, @DoCreate, 'SimbaComponentTheme', 20); // Priority 20 = init before settings
  SimbaInitialization_Add(ESimbaInit.DESTROY, @DoDestroy, 'SimbaComponentTheme', -20); // Priority -20 = finalize after settings

end.

