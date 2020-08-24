unit simba.associate;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils;

procedure Associate;

implementation

{$IFDEF WINDOWS}
uses
  registry, shlobj;
{$ENDIF}

procedure Associate;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create();

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('.simba', True);
    Reg.WriteString('', 'Simba');
    Reg.CloseKey();
    Reg.CreateKey('Simba');
    Reg.OpenKey('Simba\DefaultIcon', True);
    Reg.WriteString('', ParamStr(0) + ',0');
    Reg.CloseKey();
    Reg.OpenKey('Simba\shell\Open\command', True);
    Reg.WriteString('', ParamStr(0) + ' "%1"');
    Reg.CloseKey();
    Reg.OpenKey('Simba\shell\Run\command', True);
    Reg.WriteString('', ParamStr(0) + ' --run "%1"');
    Reg.CloseKey();
    Reg.OpenKey('Simba\shell\Run (Headless)\command', True);
    Reg.WriteString('', ExtractFilePath(ParamStr(0)) + 'SimbaScript.exe' + ' --run "%1"');
    Reg.CloseKey();
  finally
    Reg.Free();
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;
{$ELSE}
begin
end;
{$ENDIF}

end.

