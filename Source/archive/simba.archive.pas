{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.archive;

{$i simba.inc}

interface

uses
  classes, sysutils;

type
  TSimbaArchiveExtractor = class
  public
  type
    TProgressEvent = procedure(Sender: TObject; FileName: String; Position, Size: Int64) of object;
  protected
    FInputStream: TStream;
    FOutputPath: String;
    FFlat: Boolean;
    FOnProgress: TProgressEvent;
    FIgnoreList: TStringList;

    procedure SetOutputPath(Value: String);
  public
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputPath: String read FOutputPath write SetOutputPath;
    property Flat: Boolean read FFlat write FFlat;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property IgnoreList: TStringList read FIgnoreList;

    procedure Extract; virtual;

    constructor Create;
    destructor Destroy; override;
  end;

  TSimbaArchiveExtractorClass = class of TSimbaArchiveExtractor;

implementation

procedure TSimbaArchiveExtractor.SetOutputPath(Value: String);
begin
  FOutputPath := IncludeTrailingPathDelimiter(ExpandFileName(Value));
end;

procedure TSimbaArchiveExtractor.Extract;
begin
  FInputStream.Position := 0;
  while FIgnoreList.IndexOf('') > -1 do
    FIgnoreList.Delete(FIgnoreList.IndexOf(''));
end;

constructor TSimbaArchiveExtractor.Create;
begin
  FIgnoreList := TStringList.Create();
end;

destructor TSimbaArchiveExtractor.Destroy;
begin
  if (FIgnoreList <> nil) then
    FIgnoreList.Free();

  inherited Destroy();
end;

end.

