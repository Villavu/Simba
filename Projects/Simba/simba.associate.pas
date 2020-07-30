unit simba.associate;

{$mode objfpc}{$H+}

interface

uses
  classes;

procedure AssociateFileType(FileType: String);

implementation

{$IFDEF WINDOWS}
uses
  registry, shlobj;
{$ENDIF}

procedure AssociateFileType(FileType: String);
{$IFDEF WINDOWS}
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
    Reg.WriteString('', ParamStr(0) + ',0');
    Reg.CloseKey();
    Reg.OpenKey(FileType + 'file\shell\open\command', True);
    Reg.WriteString('', ParamStr(0) + ' "%1"');
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

