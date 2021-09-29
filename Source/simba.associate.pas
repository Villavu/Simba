{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.associate;

{$i simba.inc}

interface

uses
  classes, sysutils;

procedure Associate;

implementation

{$IFDEF WINDOWS}
uses
  forms, registry, windows, shellapi;

procedure Associate;
var
  Info: TShellExecuteInfo;
begin
  if not Application.HasOption('associate') then
  begin
    Info := Default(TShellExecuteInfo);
    Info.cbSize := SizeOf(TShellExecuteInfo);
    Info.Wnd := Application.MainFormHandle;
    Info.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NO_CONSOLE;
    Info.lpVerb := 'runas';
    Info.lpFile := PAnsiChar(Application.ExeName);
    Info.lpParameters := PAnsiChar('--associate');
    Info.nShow := SW_HIDE;

    ShellExecuteExA(@Info);
  end else
  begin
    with TRegistry.Create() do
    try
      RootKey := HKEY_CLASSES_ROOT;
      OpenKey('.simba', True);
      WriteString('', 'simbafile');
      CloseKey();
      CreateKey('simbafile');
      OpenKey('simbafile\DefaultIcon', True);
      WriteString('', ParamStr(0) + ',0');
      CloseKey();
      OpenKey('simbafile\shell\Open\command', True);
      WriteString('', ParamStr(0) + ' "%1"');
      CloseKey();
      OpenKey('simbafile\shell\Run\command', True);
      WriteString('', ParamStr(0) + ' --open --run "%1"');
      CloseKey();
      OpenKey('simbafile\shell\Run (Headless)\command', True);
      WriteString('', ParamStr(0) + ' --run "%1"');
      CloseKey();
    finally
      Free();
    end;

    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  end;
end;
{$ELSE}
procedure Associate;
begin
end;
{$ENDIF}

end.

