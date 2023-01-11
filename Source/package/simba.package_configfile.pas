unit simba.package_configfile;

{$i simba.inc}

interface

uses
  Classes, SysUtils, LazFileUtils,
  simba.mufasatypes;

type
  TSimbaPackageConfigFile = record
    Scripts: TStringArray;
    Examples: TStringArray;
    Ignore: TStringArray;
    Flat: Boolean;
    AutoUpdate: Boolean;
    Hash: UInt32;
  end;
  TSimbaPackageConfigFileArray = array of TSimbaPackageConfigFile;

function ParsePackageConfigFile(FileName: String): TSimbaPackageConfigFile;

implementation

function ParsePackageConfigFile(FileName: String): TSimbaPackageConfigFile;
var
  I: Integer;
  PackageDir, Value: String;
begin
  Result := Default(TSimbaPackageConfigFile);

  if FileExists(FileName) then
    with TStringList.Create() do
    try
      LoadFromFile(FileName);

      PackageDir := CleanAndExpandDirectory(ExtractFileDir(FileName));
      for I := 0 to Count - 1 do
      begin
        Value := ValueFromIndex[I];

        case Names[I] of
          'script':     Result.Scripts    := Result.Scripts  + [CleanAndExpandFilename(PackageDir + Value)];
          'example':    Result.Examples   := Result.Examples + [CleanAndExpandFilename(PackageDir + Value)];
          'ignore':     Result.Ignore     := Result.Ignore   + [CleanAndExpandFilename(PackageDir + Value)];
          'autoupdate': Result.AutoUpdate := Value.EqualsIgnoreCase('true');
          'flat':       Result.Flat       := Value.EqualsIgnoreCase('true');
        end;
      end;

      Result.Hash := Text.Hash();
    finally
      Free();
    end;
end;

end.

