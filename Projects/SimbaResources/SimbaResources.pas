program SimbaResources;

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

  for I := 1 to ParamCount do
  begin
    if not FileExists(ParamStr(I)) then
    begin
      WriteLn('Resource "', ParamStr(I), '" does not exist');
      Halt(1);
    end;

    Stream := TFileStream.Create(ParamStr(I), fmOpenRead);
    Name := ExtractFileName(ParamStr(I));

    try
      GenericResource := TGenericResource.Create(TResourceDesc.Create(RT_RCDATA), TResourceDesc.Create(Format('SIMBA-RESOURCE-%d', [I])));
      GenericResource.RawData.Write(Name, SizeOf(ShortString));
      GenericResource.RawData.CopyFrom(Stream, Stream.Size);

      Resources.Add(GenericResource);
    finally
      Stream.Free();
    end;
  end;

  if (Resources.Count > 0) then
    Resources.WriteToFile(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'SimbaResources.res');

  Resources.Free();
end.

