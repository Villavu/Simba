program MakeSimbaResources;

{$mode objfpc}{$H+}

uses
  classes, resource, reswriter, sysutils, zipper, fileutil;

const
  Target =
    {$IF Defined(SIMBA_WIN32)}
    'win32'
    {$ELSEIF Defined(SIMBA_WIN64)}
    'win64'
    {$ELSEIF Defined(SIMBA_ARM64)}
    'arm64'
    {$ELSEIF Defined(SIMBA_LINUX64)}
    'linux64'
    {$ELSEIF Defined(SIMBA_DARWIN64)}
    'darwin64'
    {$ENDIF};

var
  Stream: TMemoryStream;
  Resources: TResources;
  GenericResource: TGenericResource;
  List: TStringList;
  I: Int32;

begin
  Resources := TResources.Create();
  Stream := TMemoryStream.Create();

  List := FindAllFiles(ExtractFileDir(ParamStr(0)) + '/' + Target);
  List.Add(ExtractFileDir(ParamStr(0)) + '/../SimbaScript/SimbaScript' {$IFDEF WINDOWS} + '.exe' {$ENDIF});

  with TZipper.Create() do
  try
    Entries.AddFileEntries(List);
    for I := 0 to Entries.Count - 1 do
      Entries[I].ArchiveFileName := ExtractFileName(Entries[I].ArchiveFileName);

    SaveToStream(Stream);
  finally
    Free();
  end;

  Stream.Position := 0;

  GenericResource := TGenericResource.Create(TResourceDesc.Create(RT_RCDATA), TResourceDesc.Create('SIMBARESOURCES'));
  GenericResource.RawData.CopyFrom(Stream, Stream.Size);

  Resources.Add(GenericResource);
  Resources.WriteToFile(ExtractFileDir(ParamStr(0)) + '/SimbaResources.res');

  List.Free();
  Stream.Free();
  Resources.Free();
end.

