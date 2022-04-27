{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.openssl;

{$i simba.inc}

{$IF DEFINED(WINDOWS) AND DEFINED(CPU32)}
  {$R resourcefiles/win32}
{$ELSEIF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  {$R resourcefiles/win64}
{$ENDIF}

interface

uses
  classes, sysutils;

procedure ExtractOpenSSL;

implementation

uses
  lazloggerbase, lcltype,
  simba.gz_stream, simba.settings, simba.files;

function IsFileHash(FileName: String; Hash: String): Boolean;
begin
  Result := FileExists(FileName) and (HashFile(FileName) = Hash);
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

  Count := InputStream.Read(Buffer[1], Length(Buffer));
  repeat
    Count := InputStream.Read(Buffer[1], Length(Buffer));
    if (Count > 0) then
      OutputStream.Write(Buffer[1], Count);
  until (Count <= 0);

  InputStream.Free();
  OutputStream.Free();

  Result := HashFile(FileName);
end;

function ExtractLibCrypto(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;
var
  Name, FileName: String;
begin
  Name := LowerCase(ResourceName);

  if Name.StartsWith('libcrypto') then
  begin
    FileName := GetSimbaPath() + Name;
    if not IsFileHash(FileName, SimbaSettings.Environment.LibCryptoHash.Value) then
      SimbaSettings.Environment.LibCryptoHash.Value := ExtractLib(TResourceStream.Create(HINSTANCE, ResourceName, ResourceType), FileName);

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
    FileName := GetSimbaPath() + Name;
    if not IsFileHash(FileName, SimbaSettings.Environment.LibSSLHash.Value) then
      SimbaSettings.Environment.LibSSLHash.Value := ExtractLib(TResourceStream.Create(HINSTANCE, ResourceName, ResourceType), FileName);

    Result := False;
  end else
    Result := True; // Continue looping
end;

procedure ExtractOpenSSL;
begin
  try
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractLibCrypto, 0);
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractLibSSL, 0);
  except
    on E: Exception do
      DebugLn('Exception while extracting OpenSSL: ' + E.Message);
  end;
end;

end.

