{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.openssl;

{$i simba.inc}

{$IF DEFINED(WINDOWS) AND DEFINED(CPU32)}
  {$R ../Third-Party/openssl/win32}
{$ELSEIF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  {$R ../Third-Party/openssl/win64}
{$ENDIF}

interface

uses
  classes, sysutils;

procedure ExtractOpenSSL;
function LoadSSL(Debug: Boolean = False): Boolean;

implementation

uses
  LCLType, Forms, openssl, gz,
  simba.base, simba.settings, simba.env, simba.fs, simba.ide_initialization
  {$IF defined(WINDOWS)},
  windows
  {$ELSEIF defined(LINUX)},
  dl
  {$ENDIF};

function IsFileHash(FileName: String; Hash: String): Boolean;
begin
  Result := FileExists(FileName) and (TSimbaFile.FileHash(FileName) = Hash);
end;

function ExtractLib(Stream: TStream; FileName: String): String;
var
  Buffer: array[1..4096] of Byte;
  InputStream, OutputStream: TStream;
  Count: Integer;
begin
  Result := '';

  InputStream := TGZFileStream.Create(Stream, False);
  OutputStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyWrite);

  repeat
    Count := InputStream.Read({%H-}Buffer[1], Length(Buffer));
  until OutputStream.Write(Buffer[1], Count) = 0;

  InputStream.Free();
  OutputStream.Free();

  Result := TSimbaFile.FileHash(FileName);
end;

function ExtractLibCrypto(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;
var
  Name, FileName: String;
begin
  Name := LowerCase(ResourceName);

  if Name.StartsWith('libcrypto') then
  begin
    FileName := SimbaEnv.SimbaPath + Name;
    if not IsFileHash(FileName, SimbaSettings.General.OpenSSLCryptoHash.Value) then
      SimbaSettings.General.OpenSSLCryptoHash.Value := ExtractLib(TResourceStream.Create(HINSTANCE, ResourceName, ResourceType), FileName);

    Result := False;
  end else
    Result := True; // Continue looping
end;

function ExtractLibSSL(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;
var
  Name, FileName: String;
begin
  Name := LowerCase(ResourceName);

  if Name.StartsWith('libssl') then
  begin
    FileName := SimbaEnv.SimbaPath + Name;
    if not IsFileHash(FileName, SimbaSettings.General.OpenSSLHash.Value) then
      SimbaSettings.General.OpenSSLHash.Value := ExtractLib(TResourceStream.Create(HINSTANCE, ResourceName, ResourceType), FileName);

    Result := False;
  end else
    Result := True; // Continue looping
end;

procedure ExtractOpenSSL;
begin
  if Application.HasOption('no-openssl-extract') then
    Exit;

  try
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractLibCrypto, 0);
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractLibSSL, 0);
  except
    on E: Exception do
      DebugLn('ExtractOpenSSL Exception: ' + E.Message);
  end;
end;

function LoadSSL(Debug: Boolean): Boolean;

  function LibPath(Handle: TLibHandle): String;
  {$IF defined(WINDOWS)}
  begin
    SetLength(Result, MAX_PATH);
    SetLength(Result, GetModuleFileName(Handle, @Result[1], Length(Result)));
  end;
  {$ELSEIF defined(LINUX)}
  var
    map: plink_map;
  begin
    if (dlinfo(Pointer(Handle), RTLD_DI_LINKMAP, @map) = 0) and (map <> nil) then
      Result := map^.l_name
    else
      Result := '';
  end;
  {$ELSE}
  begin
    Result := '';
  end;
  {$ENDIF}

begin
  Result := InitSSLInterface();

  if Debug then
    if Result then
    begin
      DebugLn('SSL is loaded');
      DebugLn('SSLLib: ' + LibPath(SSLLibHandle));
      DebugLn('SSLUtil: ' + LibPath(SSLUtilHandle));
    end else
      DebugLn('SSL is not loaded: ' + GetLoadErrorStr());
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@ExtractOpenSSL, 'Extract OpenSSL', True);

end.

