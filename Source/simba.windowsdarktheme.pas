{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.windowsdarktheme;

{$I simba.inc}

interface

const
  BackColor:      Integer = $1E1E1E;
  TextColor:      Integer = $F0F0F0;
  InputBackColor: Integer = $303030;

implementation

{$IF DEFINED(SIMBA_WINDOWS_DARKTHEME) and DEFINED(WINDOWS)}
uses
  Controls, Forms, UxTheme, Windows;

type
  TApplicationHelper = class helper for TApplication
    procedure SetDarkTheme(Data: PtrInt);
  end;

procedure TApplicationHelper.SetDarkTheme(Data: PtrInt);
var
  Control: TWinControl;
begin
  Control := FindControl(Data);

  if Assigned(Control) then
  begin
    Control := FindControl(Data);
    if (not Control.IsParentColor) then
      Control.Color := InputBackColor;
    Control.Font.Color := TextColor;

    SetWindowTheme(Data, 'DarkMode_Explorer', nil);
  end;
end;

function HookProc(Code: LongInt; Win: WPARAM; Data: LPARAM): LRESULT; stdcall;
begin
  if (Code = HCBT_CREATEWND) then
    Application.QueueAsyncCall(@Application.SetDarkTheme, PtrInt(Win));

  Result := CallNextHookEx(0, Code, Win, Data);
end;

initialization
  SetWindowsHookEx(WH_CBT, @HookProc, GetModuleHandle(nil), GetCurrentThreadID());
{$ENDIF}

end.

