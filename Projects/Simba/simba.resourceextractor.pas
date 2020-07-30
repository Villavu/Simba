unit simba.resourceextractor;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils;

procedure ExtractSimbaResources;

implementation

uses
  dialogs, forms, lcltype,
  simba.debugform;

function LoadSimbaResource(ResourceHandle: TFPResourceHMODULE; ResourceType, ResourceName: PChar; Param: PtrInt): LongBool; stdcall;

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
  FileName: ShortString = '';
  Stream: TResourceStream;
begin
  Result := True; // Loop all resources

  Name := ResourceName;
  if Name.StartsWith('SIMBA-RESOURCE') then
  begin
    Stream := TResourceStream.Create(HINSTANCE, ResourceName, ResourceType);

    try
      if Stream.Read(FileName, SizeOf(ShortString)) = SizeOf(ShortString) then // Filename
      begin
        FileName := Application.Location + FileName;
        Writeln(FileName);
        if FileExists(FileName) and SameFile(FileName, Stream) then
          Exit;

        try
          SimbaDebugForm.Add('Writing file: ' + FileName);

          with TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyWrite) do
          try
            CopyFrom(Stream, Stream.Size - Stream.Position);
          finally
            Free();
          end;
        except
          on E: EFOpenError do
          begin
            WriteLn(E.Message, ' (', E.ClassName, ')');

            SimbaDebugForm.Add('Unable to write file ' + FileName + '.');
            SimbaDebugForm.Add('The file is likely in use, close all Simba''s and try again.');
          end;
        end;
      end;
    finally
      Stream.Free();
    end;
  end;
end;

procedure ExtractSimbaResources;
begin
  EnumResourceNames(HINSTANCE, RT_RCDATA, @LoadSimbaResource, 0);
end;

end.

