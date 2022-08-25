unit simba.windowsdarktheme;

{$I simba.inc}

interface

const
  BackColor:      Integer = $1E1E1E;
  TextColor:      Integer = $F0F0F0;
  InputBackColor: Integer = $303030;

implementation

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
    if not Control.IsParentColor then
      Control.Color := InputBackColor;
    Control.Font.Color := TextColor;

    SetWindowTheme(Data, 'DarkMode_Explorer', nil);
  end;
end;

function HookProc(code: LongInt; win: WPARAM; data: LPARAM): LRESULT; stdcall;
begin
  if (code = HCBT_CREATEWND) then
    Application.QueueAsyncCall(@Application.SetDarkTheme, PtrInt(win));

  Result := CallNextHookEx(0, code, win, data);
end;

initialization
  SetWindowsHookEx(WH_CBT, @proc, GetModuleHandle(nil), GetCurrentThreadID());

end.

