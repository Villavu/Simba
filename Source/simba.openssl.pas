{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.openssl;

{$i simba.inc}

{$IFDEF SIMBA_WIN32}
  {$R resourcefiles/win32}
{$ENDIF}

{$IFDEF SIMBA_WIN64}
  {$R resourcefiles/win64}
{$ENDIF}

{$IFDEF SIMBA_DARWIN64}
  {$R resourcefiles/darwin64}
{$ENDIF}

{$IFDEF SIMBA_LINUX64}
  {$R resourcefiles/linux64}
{$ENDIF}

{$IFDEF SIMBA_AARCH64}
  {$R resourcefiles/aarch64}
{$ENDIF}

interface

uses
  classes, sysutils;

procedure InitializeOpenSSL;

implementation

uses
  openssl, lcltype, lazloggerbase, sha1, dynlibs,
  simba.gz_stream, simba.settings, simba.files;

function IsFileHash(FileName: String; Hash: String): Boolean;
begin
  Result := FileExists(FileName) and (SHA1Print(SHA1File(FileName, 512*512)) = Hash);
end;

function Uncompress(Stream: TStream; FileName: String): String;
var
  Buffer: array[1..512*512] of Byte;
  Count: Integer;
begin
  Result := '';

  DebugLn('Extracting: ', FileName);

  Stream := TGZFileStream.Create(Stream, False);
  try
    with TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyWrite) do
    try
      repeat
        Count := Stream.Read(Buffer[1], Length(Buffer));
        if (Count > 0) then
          Write(Buffer[1], Count);
      until (Count = 0);
    finally
      Free();
    end;

    Result := SHA1Print(SHA1File(FileName, Length(Buffer)));
  finally
    Stream.Free();
  end;
end;

function ExtractLibCrypto(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;
var
  Name: String;
begin
  Name := LowerCase(ResourceName);
  if Name.StartsWith('libcrypto') then
  begin
    DLLUtilName := GetSimbaPath() + Name;
    if not IsFileHash(DLLUtilName, SimbaSettings.Environment.LibCryptoHash.Value) then
      SimbaSettings.Environment.LibCryptoHash.Value := Uncompress(TResourceStream.Create(HINSTANCE, ResourceName, ResourceType), DLLUtilName);

    Result := False;
  end else
    Result := True; // Continue looping
end;

function ExtractLibSSL(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;
var
  Name: String;
begin
  Name := LowerCase(ResourceName);
  if Name.StartsWith('libssl') then
  begin
    DLLSSLName := GetSimbaPath() + Name;
    if not IsFileHash(DLLSSLName, SimbaSettings.Environment.LibSSLHash.Value) then
      SimbaSettings.Environment.LibSSLHash.Value := Uncompress(TResourceStream.Create(HINSTANCE, ResourceName, ResourceType), DLLSSLName);

    Result := False;
  end else
    Result := True; // Continue looping
end;

procedure InitializeOpenSSL;
var
  OldSSLName, OldUtilName: String;
begin
  DebugLn('simba.openssl :: InitializeOpenSSL');

  OldSSLName := DLLSSLName;
  OldUtilName := DLLUtilName;

  try
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractLibCrypto, 0);
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractLibSSL, 0);

    {$IFDEF UNIX}
    while ExtractFileExt(DLLSSLName) <> '' do
      SetLength(DLLSSLName, Length(DLLSSLName) - Length(ExtractFileExt(DLLSSLName)));
    while ExtractFileExt(DLLUtilName) <> '' do
      SetLength(DLLUtilName, Length(DLLUtilName) - Length(ExtractFileExt(DLLUtilName)));
    {$ENDIF}

    InitSSLInterface();
  except
    on E: Exception do
      DebugLn('Loading OpenSSL exception: ', E.Message);
  end;

  if not IsSSLLoaded() then
  begin
    DebugLn('Failed to load OpenSSL');
    DebugLn('Error: ', GetLoadErrorStr());
    DebugLn('LibSSL: ', DLLSSLName);
    DebugLn('LibCrypto: ', DLLUtilName);
  end;

  // Restore to FPC defaults
  DLLSSLName := OldSSLName;
  DLLUtilName := OldUtilName;
end;

end.

