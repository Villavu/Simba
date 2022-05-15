{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.tar;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.archive;

type
  TSimbaTarExtractor = class(TSimbaArchiveExtractor)
  protected
    function Decompress: TMemoryStream; virtual;
  public
    procedure Extract; override;
  end;

implementation

uses
  libtar;

function TSimbaTarExtractor.Decompress: TMemoryStream;
begin
  Result := nil;
end;

procedure TSimbaTarExtractor.Extract;
const
  HEADER_FILENAME = 'pax_global_header';
var
  Stream: TStream;
  Tar: TTarArchive;
  Entry: TTarDirRec;
  Path, Directory: String;
  Position, Size: Int64;
begin
  Position := 0;
  Size := 0;

  Stream := Self.Decompress();

  try
    Tar := TTarArchive.Create(Stream);
    Tar.Reset();

    try
      if Tar.FindNext(Entry) then
      begin
        if Entry.Name = HEADER_FILENAME then
        begin
          Size := Size + Entry.Size;

          if Tar.FindNext(Entry) then
          begin
            Size := Size + Entry.Size;

            Directory := Entry.Name;
          end;
        end else
          Directory := Entry.Name;

        while Tar.FindNext(Entry) do
        begin
          Size := Size + Entry.Size;
          if (Copy(Entry.Name, 1, Length(Directory)) <> Directory) then
            Directory := '';
        end;
      end;

      Tar.Reset();

      while Tar.FindNext(Entry) do
      begin
        if FFlat then
          Path := SetDirSeparators(FOutputPath + ExtractFileName(Entry.Name))
        else
        if Directory <> '' then
          Path := SetDirSeparators(FOutputPath + Entry.Name.SubString(Length(Directory)))
        else
          Path := SetDirSeparators(FOutputPath + Entry.Name);

        case Entry.FileType of
          ftNormal:
            if Entry.Name <> HEADER_FILENAME then
              Tar.ReadFile(Path);

          ftDirectory:
            ForceDirectories(Path);
        end;

        Position := Position + Entry.Size;
        if (FOnProgress <> nil) then
          FOnProgress(Self, Path, Position, Size);
      end;
    finally
      Tar.Free();
    end;
  finally
    if (Stream <> FInputStream) then
      Stream.Free();
  end;
end;

end.

