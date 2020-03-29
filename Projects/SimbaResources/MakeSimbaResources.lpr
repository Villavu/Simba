program MakeSimbaResources;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, resource, reswriter, sysutils, zipper;

var
  Resources: TResources;

procedure WriteResource(Name: String; Stream: TStream);
var
  Resource: TGenericResource;
begin
  Resource := TGenericResource.Create(TResourceDesc.Create(RT_RCDATA), TResourceDesc.Create(Name));
  Resource.RawData.CopyFrom(Stream, Stream.Size);

  Resources.Add(Resource);
end;

var
  FileStream: TFileStream;

begin
  Resources := TResources.Create();

  // SimbaScript
  with TZipper.Create() do
  try
    FileName := ExtractFileDir(ParamStr(0)) + '/SimbaScript.zip';
    with Entries do
      AddFileEntry(ExtractFileDir(ParamStr(0)) + '/../SimbaScript/SimbaScript' {$IFDEF WINDOWS} + '.exe' {$ENDIF}, 'SimbaScript' {$IFDEF WINDOWS} + '.exe' {$ENDIF});

    ZipAllFiles();
  finally
    Free();
  end;

  FileStream := TFileStream.Create(ExtractFileDir(ParamStr(0)) + '/SimbaScript.zip', fmOpenRead);

  try
    WriteResource('SIMBASCRIPT', FileStream);
  finally
    FileStream.Free();
  end;

  // OpenSSL
  with TZipper.Create() do
  try
    FileName := ExtractFileDir(ParamStr(0)) + '/OpenSSL.zip';
    with Entries do
    begin
      {$IFDEF SIMBA_WIN64}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/win64/libeay32.dll', 'libeay32.dll');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/win64/ssleay32.dll', 'ssleay32.dll');
      {$ENDIF}

      {$IFDEF SIMBA_WIN32}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/win32/libeay32.dll', 'libeay32.dll');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/win32/ssleay32.dll', 'ssleay32.dll');
      {$ENDIF}

      {$IFDEF SIMBA_LINUX64}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/linux64/libssl.so', 'libssl.so');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/linux64/libcrypto.so', 'libcrypto.so');
      {$ENDIF}

      {$IFDEF SIMBA_ARM64}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/arm64/libssl.so', 'libssl.so');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/arm64/libcrypto.so', 'libcrypto.so');
      {$ENDIF}

      {$IFDEF SIMBA_DARWIN}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/darwin64/libssl.dylib', 'libssl.dylib');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/darwin64/libcrypto.dylib', 'libcrypto.dylib');
      {$ENDIF}
    end;

    ZipAllFiles();
  finally
    Free();
  end;

  FileStream := TFileStream.Create(ExtractFileDir(ParamStr(0)) + '/OpenSSL.zip', fmOpenRead);

  try
    WriteResource('OPENSSL', FileStream);
  finally
    FileStream.Free();
  end;

  Resources.WriteToFile(ExtractFileDir(ParamStr(0)) + '/SimbaResources.res');
end.

