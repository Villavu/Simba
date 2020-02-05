unit simba.resourceextractor;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, zipper;

type
  TSimbaResourceLoader = object
  protected
    FName: String;
    FOutputPath: String;

    procedure CreateInputStream(Sender: TObject; var Stream: TStream);
    procedure CreateOutputStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
    procedure Done(Sender : TObject; var Stream: TStream; Item: TFullZipFileEntry);
  public
    procedure Extract(Name: String; OutputPath: String);
  end;

var
  SimbaResourceExtractor: TSimbaResourceLoader;

implementation

uses
  lcltype, fileutil, dialogs,
  simba.debugform;

procedure TSimbaResourceLoader.CreateInputStream(Sender: TObject; var Stream: TStream);
begin
  Stream := TResourceStream.Create(HInstance, FName, RT_RCDATA);
end;

procedure TSimbaResourceLoader.CreateOutputStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
begin
  Stream := TMemoryStream.Create();
end;

procedure TSimbaResourceLoader.Done(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);

  function SameFile(Path: String; Stream: TMemoryStream): Boolean;
  begin
    Result := False;

    if FileExists(Path) then
    begin
      with TMemoryStream.Create() do
      try
        LoadFromFile(Path);

        Result := (Stream.Size = Size) and (CompareMem(Stream.Memory, Memory, Size));
      finally
        Free();
      end;
    end;
  end;

var
  Path: String;
begin
  Path := IncludeTrailingPathDelimiter(FOutputPath) + Item.DiskFileName;

  try
    if not SameFile(Path, Stream as TMemoryStream) then
    begin
      SimbaDebugForm.Add('Writing file: ' + Path);

      with TFileStream.Create(Path, fmCreate or fmOpenWrite or fmShareDenyWrite) do
      try
        Stream.Position := 0;

        CopyFrom(Stream, Stream.Size);
      finally
        Free();
      end;
    end;
  except
    on E: EFOpenError do
    begin
      WriteLn(E.Message, ' (', E.ClassName, ')');

      SimbaDebugForm.Add('Unable to write file "' + Path + '". The file is currently in use!' + LineEnding +
                         'You likely have another Simba open that is a different version/bitness.' + LineEnding +
                         'Things may not work correctly!');
    end;
  end;

  Stream.Free();
end;

procedure TSimbaResourceLoader.Extract(Name: String; OutputPath: String);
begin
  FName := Name;
  FOutputPath := OutputPath;

  with TUnZipper.Create() do
  try
    OnOpenInputStream := @CreateInputStream;
    OnCreateStream := @CreateOutputStream;
    OnDoneStream := @Done;

    UnZipAllFiles();
  finally
    Free();
  end;
end;

end.

