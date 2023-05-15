unit simba.theme;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Forms;

type
  TSimbaTheme = class
  protected
    {$IFDEF WINDOWS}
    procedure DoFormAdded(Sender: TObject; Form: TCustomForm);
    procedure DoColorTitle(Data: PtrInt);
    {$ENDIF}
  public
    ColorBackground: TColor;
    ColorFrame: TColor;
    ColorActive: TColor;
    ColorScrollBarActive: TColor;
    ColorScrollBarInActive: TColor;

    ColorFont: TColor;
    ColorLine: TColor;

    constructor Create;
  end;

var
  SimbaTheme: TSimbaTheme;

implementation

{$IFDEF WINDOWS}
uses
  DwmApi;

procedure TSimbaTheme.DoFormAdded(Sender: TObject; Form: TCustomForm);
begin
  Application.QueueAsyncCall(@DoColorTitle, PtrUInt(Form));
end;

procedure TSimbaTheme.DoColorTitle(Data: PtrInt);
const
  DWMWA_CAPTION_COLOR = 35;
begin
  if Assigned(DwmSetWindowAttribute) then
    DwmSetWindowAttribute(TCustomForm(Data).Handle, DWMWA_CAPTION_COLOR, @Self.ColorFrame, SizeOf(TColor));
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
  ColorFont := $f2f2f2;

  {$IFDEF WINDOWS}
  if (Win32BuildNumber >= 22000) then // DWMWA_CAPTION_COLOR
    Screen.AddHandlerFormAdded(@Self.DoFormAdded);
  {$ENDIF}
end;

initialization
  SimbaTheme := TSimbaTheme.Create();

end.

