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
      {$IFDEF WIN64}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/openssl/libeay64.dll', 'libeay32.dll');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/openssl/ssleay64.dll', 'ssleay32.dll');
      {$ENDIF}
      {$IFDEF WIN32}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/openssl/libeay32.dll', 'libeay32.dll');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/openssl/ssleay32.dll', 'ssleay32.dll');
      {$ENDIF}
      {$IFDEF LINUX}
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/openssl/libssl64.so', 'libssl.so');
        AddFileEntry(ExtractFileDir(ParamStr(0)) + '/openssl/libcrypto64.so', 'libcrypto.so');
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

