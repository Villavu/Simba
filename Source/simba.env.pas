{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.env;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

  function FindFile(var FileName: string; Extension: String; const Directories: array of String): Boolean;
  function FindFiles(Directories: TStringArray; WildCard: String; Recursive: Boolean = False): TStringArray;
  function FindPlugin(var FileName: String; const Directories: array of String): Boolean;
  procedure CopyPlugin(var FileName: String);

  function HashFile(const FileName: String): String;
  function ReadFile(const FileName: String): String;
  function WriteFile(const FileName, Contents: String): Boolean;
  function CreateTempFile(const Contents, Prefix: String): String;

  function GetSimbaPath: String;
  function GetDataPath: String;
  function GetIncludePath: String;
  function GetPluginPath: String;
  function GetPluginCopyPath: String;
  function GetScriptPath: String;
  function GetPackagePath: String;
  function GetOldPackagePath: String;
  function GetBackupsPath: String;
  function GetDumpPath: String;
  function GetScreenshotPath: String;

implementation

uses
  forms, inifiles, fileutil, zipper, sha1;

function FindFile(var FileName: string; Extension: String; const Directories: array of String): Boolean;
var
  I: Int32;
begin
  Result := False;

  if FileExists(FileName) then
  begin
    FileName := ExpandFileName(FileName);

    Exit(True);
  end;

  for I := 0 to High(Directories) do
    if FileExists(IncludeTrailingPathDelimiter(Directories[I]) + FileName + Extension) then
    begin
      FileName := ExpandFileName(IncludeTrailingPathDelimiter(Directories[I]) + FileName + Extension);

      Exit(True);
    end;
end;

function FindFiles(Directories: TStringArray; WildCard: String; Recursive: Boolean): TStringArray;
var
  I: Integer;
  Path: String;
begin
  Result := Default(TStringArray);
  if Length(Directories) = 0 then
    Exit;

  I := Pos('*', WildCard) - 1;
  if (I > 0) then
  begin
    Path := Copy(Wildcard, 1, I);
    Wildcard := Copy(WildCard, I+1);
  end else
    Path := '';

  for I := 0 to High(Directories) do
    if DirectoryExists(Directories[I]) then
    begin
      Directories[I] := ExpandFileName(ConcatPaths([Directories[I], Path]));

      with FindAllFiles(Directories[I], WildCard, Recursive) do
      try
        Sort(); // sort, else it's dependant on how filesystem orders

        Result := ToStringArray();
        if Length(Result) > 0 then
          Exit;
      finally
        Free();
      end;
    end;
end;

function FindPlugin(var FileName: String; const Directories: array of String): Boolean;
begin
  Result := FindFile(FileName, '', Directories) or
            {$IFDEF CPUAARCH64} 
            FindFile(FileName, '.' + SharedSuffix + '.aarch64', Directories) or
            FindFile(FileName, {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix + '.aarch64', Directories) or
            {$ENDIF}
            FindFile(FileName, '.' + SharedSuffix, Directories) or
            FindFile(FileName, {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix, Directories);
end;



// Make a copy of the plugin to data/plugins/ so we can delete/update if it's loaded
procedure CopyPlugin(var FileName: String);
var
  NewFileName: String;
begin
  NewFileName := GetPluginCopyPath() + HashFile(FileName) + ExtractFileExt(FileName);
  if CopyFile(FileName, NewFileName, [], True) then
    FileName := NewFileName;
end;



function HashFile(const FileName: String): String;
begin
  try
    Result := SHA1Print(SHA1File(FileName, 256*256));
  except
    Result := '';
  end;
end;

function ReadFile(const FileName: String): String;
var
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.Read(Result[1], Length(Result));
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

function WriteFile(const FileName, Contents: String): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;

  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    if Stream.Write(Contents[1], Length(Contents)) = Length(Contents) then
      Result := True;
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

function CreateTempFile(const Contents, Prefix: String): String;
var
  Number: Integer = 0;
begin
  Result := Format('%s%s.%d', [GetDataPath(), Prefix, Number]);
  while FileExists(Result) do
  begin
    Inc(Number);

    Result := Format('%s%s.%d', [GetDataPath(), Prefix, Number]);
  end;

  with TStringList.Create() do
  try
    Text := Contents;

    SaveToFile(Result);
  finally
    Free();
  end;
end;

function GetSimbaPath: String;
begin
  Result := IncludeTrailingPathDelimiter(Application.Location);
end;

function GetDataPath: String;
begin
  Result := GetSimbaPath() + 'Data' + DirectorySeparator;
end;

function GetIncludePath: String;
begin
  Result := GetSimbaPath() + 'Includes' + DirectorySeparator;
end;

function GetPluginPath: String;
begin
  Result := GetSimbaPath() + 'Plugins' + DirectorySeparator;
end;

function GetPluginCopyPath: String;
begin
  Result := GetDataPath() + 'plugincopies' + DirectorySeparator;
end;

function GetScriptPath: String;
begin
  Result := GetSimbaPath() + 'Scripts' + DirectorySeparator;
end;

function GetPackagePath: String;
begin
  Result := GetDataPath() + 'packages' + DirectorySeparator;
end;

function GetOldPackagePath: String;
begin
  Result := GetDataPath() + 'oldpackages' + DirectorySeparator;
end;

function GetBackupsPath: String;
begin
  Result := GetDataPath() + 'backups' + DirectorySeparator;
end;

function GetDumpPath: String;
begin
  Result := GetDataPath() + 'dumps' + DirectorySeparator;
end;

function GetScreenshotPath: String;
begin
  Result := GetSimbaPath() + 'Screenshots' + DirectorySeparator;
end;

procedure CreateBaseDirectories;
var
  Directory: String;
begin
  // Root directories
  for Directory in [GetIncludePath(), GetScriptPath(), GetPluginPath(), GetDataPath(), GetScreenshotPath()] do
  begin
     if DirectoryExists(Directory) then
       Continue;

    if CreateDir(Directory) then
      DebugLn('[CreateBaseDirectories]: ' + Directory)
    else
      DebugLn('[CreateBaseDirectories]: Failed ' + Directory);
  end;

  // Data directories
  for Directory in [GetPackagePath(), GetPluginCopyPath(), GetOldPackagePath(), GetBackupsPath(), GetDumpPath()] do
  begin
    if DirectoryExists(Directory) then
      Continue;

    if CreateDir(Directory) then
      DebugLn('[CreateBaseDirectories]: ' + Directory)
    else
      DebugLn('[CreateBaseDirectories]: Failed ' + Directory);
  end;
end;

function ReadINI(const Section, KeyName: string; FileName: string): string;
begin
  with TINIFile.Create(ExpandFileName(FileName)) do
  try
    Result := ReadString(Section, KeyName, '');
  finally
    Free();
  end;
end;

procedure DeleteINI(const Section, KeyName : string; FileName : string);
begin
  with TIniFile.Create(ExpandFileName(FileName)) do
  try
    if (KeyName = '') then
	    EraseSection(Section)
	  else
	    DeleteKey(Section, KeyName);
  finally
    Free();
  end;
end;

procedure WriteINI(const Section, KeyName, NewString : string; FileName : string);
begin
  with TINIFile.Create(ExpandFileName(FileName)) do
  try
	  WriteString(Section, KeyName, NewString);
  finally
	  Free();
  end;
end;

initialization
  CreateBaseDirectories();

end.

