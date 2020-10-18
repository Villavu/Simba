unit simba.archive;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils;

type
  TSimbaArchiveProgressEvent = procedure(Sender: TObject; FileName: String; Position, Size: Int64) of object;
  TSimbaArchiveCompressor = class
  protected
    FArchiveFileNames: TStringList;
    FDiskFileNames: TStringList;
    FFileName: String;
    FOnProgress: TSimbaArchiveProgressEvent;
  public
    procedure Compress; virtual; abstract;
  end;

  TSimbaArchiveExtractorClass = class of TSimbaArchiveExtractor;
  TSimbaArchiveExtractor = class
  protected
    FInputStream: TStream;
    FOutputPath: String;
    FFlat: Boolean;
    FOnProgress: TSimbaArchiveProgressEvent;

    procedure SetOutputPath(Value: String);
  public
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputPath: String read FOutputPath write SetOutputPath;
    property Flat: Boolean read FFlat write FFlat;
    property OnProgress: TSimbaArchiveProgressEvent read FOnProgress write FOnProgress;

    procedure Extract; virtual;
  end;

implementation

procedure TSimbaArchiveExtractor.SetOutputPath(Value: String);
begin
  FOutputPath := SetDirSeparators(IncludeTrailingPathDelimiter(Value));
end;

procedure TSimbaArchiveExtractor.Extract;
begin
  FInputStream.Position := 0;
end;

end.

