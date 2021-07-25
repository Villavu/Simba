unit simba.openssl;

{$mode objfpc}{$H+}
{$i simba.inc}

{$IFDEF SIMBA_WIN32}
  {$R resources/win32}
{$ENDIF}

{$IFDEF SIMBA_WIN64}
  {$R resources/win64}
{$ENDIF}

{$IFDEF SIMBA_DARWIN64}
  {$R resources/darwin64}
{$ENDIF}

{$IFDEF SIMBA_LINUX64}
  {$R resources/linux64}
{$ENDIF}

{$IFDEF SIMBA_AARCH64}
  {$R resources/aarch64}
{$ENDIF}

interface

uses
  Classes, SysUtils;

procedure InitializeOpenSSL;

implementation

uses
  openssl, lcltype, LazLoggerBase, sha1,
  simba.gz_stream, simba.settings, simba.files;

const
  BUF_SIZE = 512 * 512;

function IsFileHash(FileName: String; Hash: String): Boolean;
begin
  Result := FileExists(FileName) and (SHA1Print(SHA1File(FileName, BUF_SIZE)) = Hash);
end;

function Uncompress(Stream: TStream; FileName: String): String;
var
  Buffer: array[1..BUF_SIZE] of Byte;
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

    Result := SHA1Print(SHA1File(FileName, BUF_SIZE));
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

    {$IFDEF DARWIN}
    while ExtractFileExt(DLLSSLName) <> '' do
      SetLength(DLLSSLName, Length(DLLSSLName) - Length(ExtractFileExt(DLLSSLName)));
    while ExtractFileExt(DLLUtilName) <> '' do
      SetLength(DLLUtilName, Length(DLLUtilName) - Length(ExtractFileExt(DLLUtilName)));
    {$ENDIF}

    DebugLn('Loading: ', DLLSSLName);
    DebugLn('Loading: ', DLLUtilName);

    InitSSLInterface();
  except
    on E: Exception do
      DebugLn('Exception raised while loading OpenSSL: ', E.Message);
  end;

  if IsSSLLoaded() then
    DebugLn('OpenSSL Loaded');

  // Restore to FPC defaults
  DLLSSLName := OldSSLName;
  DLLUtilName := OldUtilName;
end;

end.

