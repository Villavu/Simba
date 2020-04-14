program SimbaAssociate;

{$mode objfpc}{$H+}

uses
  Classes, Registry, ShlObj;

procedure RegisterFileType(FileType: string; Application: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create();

  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('.' + FileType, True);
    Reg.WriteString('', FileType + 'file');
    Reg.CloseKey();
    Reg.CreateKey(FileType + 'file');
    Reg.OpenKey(FileType + 'file\DefaultIcon', True);
    Reg.WriteString('', Application + ',0');
    Reg.CloseKey();
    Reg.OpenKey(FileType + 'file\shell\open\command', True);
    Reg.WriteString('', Application + ' "%1"');
    Reg.CloseKey();
  finally
    Reg.Free();
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

{$R *.res}

begin
  RegisterFileType('simba', ParamStr(1));
end.

