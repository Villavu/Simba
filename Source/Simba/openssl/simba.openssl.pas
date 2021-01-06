unit simba.openssl;

{$mode objfpc}{$H+}

{$IF DEFINED(WINDOWS) AND DEFINED(CPU32)}
  {$R openssl-win32}
{$ELSEIF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  {$R openssl-win64}
{$ELSEIF DEFINED(DARWIN) AND DEFINED(CPU64)}
  {$R openssl-darwin64}
{$ELSEIF DEFINED(LINUX) AND DEFINED(CPU64)}
  {$R openssl-linux64}
{$ELSEIF DEFINED(LINUX) AND DEFINED(CPUAARCH64)}
  {$R openssl-aarch64}
{$ENDIF}

interface

uses
  Classes, SysUtils;

procedure InitializeOpenSSL(Extract: Boolean);

implementation

uses
  openssl, forms, LCLType,
  simba.settings;

function ExtractOpenSSL(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;

  function SameFile(Path: String; Stream: TResourceStream): Boolean;
  begin
    with TMemoryStream.Create() do
    try
      LoadFromFile(Path);

      Result := (Stream.Size - Stream.Position = Size) and CompareMem(Stream.Memory + Stream.Position, Memory, Size);
    finally
      Free();
    end;
  end;

var
  Name: String;
  FileName: ShortString;
  Stream: TResourceStream;
begin
  Name := LowerCase(ResourceName);

  if Name.StartsWith(DLLSSLName) or Name.StartsWith(DLLUtilName) then
  begin
    Stream := TResourceStream.Create(HINSTANCE, ResourceName, ResourceType);

    try
      FileName := SimbaSettings.Environment.OpenSSLPath.Value + Name;
      if FileExists(FileName) and SameFile(FileName, Stream) then
        Exit;

      try
        WriteLn('Writing file: ' + FileName);

        with TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyWrite) do
        try
          CopyFrom(Stream, Stream.Size);
        finally
          Free();
        end;
      except
        on E: EFOpenError do
          WriteLn('Unable to write file ' + FileName + '.');
      end;
    finally
      Stream.Free();
    end;
  end;

  Result := True; // Loop all resources
end;

procedure InitializeOpenSSL(Extract: Boolean);
begin
  if Extract then
    EnumResourceNames(HINSTANCE, RT_RCDATA, @ExtractOpenSSL, 0);

  DLLUtilName := SimbaSettings.Environment.OpenSSLPath.Value + DLLUtilName;
  DLLSSLName := SimbaSettings.Environment.OpenSSLPath.Value + DLLSSLName;

  try
    InitSSLInterface();
  except
  end;

  if IsSSLLoaded() then
    Exit;

  // Revert back to using system libraries
  DLLSSLName := ExtractFileName(DLLSSLName);
  DLLUtilName := ExtractFileName(DLLUtilName);
end;

end.

