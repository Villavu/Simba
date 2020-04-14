unit simba.resourceextractor;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, zipper;

type
  TSimbaResourceExtractor = object
  protected
    procedure OpenInput(Sender: TObject; var Stream: TStream);
    procedure CloseInput(Sender: TObject; var Stream: TStream);

    procedure OpenOutput(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
    procedure CloseOutput(Sender : TObject; var Stream: TStream; Item: TFullZipFileEntry);
  public
    procedure Extract;
  end;

implementation

uses
  lcltype, fileutil, dialogs, forms,
  simba.debugform;

procedure TSimbaResourceExtractor.OpenInput(Sender: TObject; var Stream: TStream);
begin
  Stream := TResourceStream.Create(HInstance, 'SIMBARESOURCES', RT_RCDATA);
  if (Stream.Size = 0) then
    raise Exception.Create('Simba resources are missing');
end;

procedure TSimbaResourceExtractor.CloseInput(Sender: TObject; var Stream: TStream);
begin
  Stream.Free();
end;

procedure TSimbaResourceExtractor.OpenOutput(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
begin
  Stream := TMemoryStream.Create();
end;

procedure TSimbaResourceExtractor.CloseOutput(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);

  function SameFile(Path: String; Stream: TMemoryStream): Boolean;
  begin
    with TMemoryStream.Create() do
    try
      LoadFromFile(Path);

      Result := (Stream.Size = Size) and CompareMem(Stream.Memory, Memory, Size);
    finally
      Free();
    end;
  end;

var
  FileName: String;
begin
  Stream.Position := 0;

  try
    FileName := Application.Location + Item.DiskFileName;
    if FileExists(FileName) and SameFile(FileName, TMemoryStream(Stream)) then
      Exit;

    SimbaDebugForm.Add('Writing file: ' + FileName);

    try
      with TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyWrite) do
      try
        CopyFrom(Stream, Stream.Size);
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
  finally
    Stream.Free();
  end;
end;

procedure TSimbaResourceExtractor.Extract;
begin
  with TUnZipper.Create() do
  try
    OnOpenInputStream := @Self.OpenInput;
    // OnCloseInputStream := @Self.CloseInput; // Stream is handled by UnZipper

    OnCreateStream := @Self.OpenOutput;
    OnDoneStream := @Self.CloseOutput;

    UnZipAllFiles();
  finally
    Free();
  end;
end;

end.

