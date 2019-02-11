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
    FPosition: Int32;
    FSize: Int32;
  public
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputPath: String read FOutputPath write FOutputPath;
    property Flat: Boolean read FFlat write FFlat;
    property OnProgress: TSimbaArchiveProgressEvent read FOnProgress write FOnProgress;

    procedure Extract; virtual;
  end;

implementation

procedure TSimbaArchiveExtractor.Extract;
begin
  FPosition := 0;
  FSize := 0;
  FOutputPath := SetDirSeparators(IncludeTrailingPathDelimiter(FOutputPath));
  FInputStream.Position := 0;
end;

end.

