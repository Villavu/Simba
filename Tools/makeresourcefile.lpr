program makeresourcefile;

// Example:
// makeresourcefile.exe openssl-win32 ssleay32.dll libeay32.dll

{$mode objfpc}{$H+}

uses
  classes, sysutils, resource, reswriter;

var
  Resources: TResources;
  GenericResource: TGenericResource;
  Stream: TFileStream;
  Name: ShortString;
  I: Int32;
begin
  Resources := TResources.Create();

  for I := 2 to ParamCount do
  begin
    if not FileExists(ParamStr(I)) then
    begin
      WriteLn('Resource "', ParamStr(I), '" does not exist');

      Halt(1);
    end;

    Stream := TFileStream.Create(ParamStr(I), fmOpenRead);
    Name := ExtractFileName(ParamStr(I));

    try
      GenericResource := TGenericResource.Create(TResourceDesc.Create(RT_RCDATA), TResourceDesc.Create(ExtractFileName(ParamStr(I))));
      GenericResource.RawData.CopyFrom(Stream, Stream.Size);

      Resources.Add(GenericResource);
    finally
      Stream.Free();
    end;
  end;

  if (Resources.Count > 0) then
    Resources.WriteToFile(ChangeFileExt(ParamStr(1), '.res'));

  Resources.Free();
end.


