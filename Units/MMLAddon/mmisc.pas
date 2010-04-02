unit mmisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bzip2, bzip2comn,bzip2stream, libtar,mufasabase,mufasatypes;

function DecompressBZip2(const input : TStream; const BlockSize : Cardinal = 4096) : TMemoryStream;
function UnTar(const Input : TStream) : TStringArray;overload;
function UnTar(const Input : TStream; outputdir : string; overwrite : boolean): boolean;overload;

implementation

function DecompressBZip2(const input: TStream; const BlockSize: Cardinal): TMemoryStream;
var
  Unzipper : TDecompressBzip2Stream;
  Blocks : array of Byte;
  ReadSize : cardinal;
begin
  SetLength(Blocks,BlockSize);
  try
    Unzipper := TDecompressBzip2Stream.Create(input);
  except
    on e : exception do
    begin;
      mDebugLn(e.message);
      exit;
    end;
  end;
  Result := TMemoryStream.Create;
  try
    repeat
      ReadSize := BlockSize;
      ReadSize := Unzipper.read(blocks[0],readsize);  //Read ReadSize amount of bytes.
      Result.Write(Blocks[0],ReadSize);
    until readsize = 0;
  except
    on e : EBzip2 do
     if E.ErrCode <> bzip2_endoffile then
       raise Exception.CreateFmt('Decompression error: %s %d',[e.message,e.errcode]);
  end;
  Unzipper.Free;
end;

function UnTar(const Input : TStream) : TStringArray;overload;
var
  Tar : TTarArchive;
  DirRec : TTarDirRec;
  Len : integer;
begin;
  Tar := TTarArchive.Create(input);
  Tar.reset;
  Len := 0;
  while Tar.FindNext(DirRec) do
  begin
    inc(len);
    SetLength(result,len*2);
    result[len*2-2] := DirRec.Name;
    result[len*2-1] := Tar.ReadFile;
  end;
  Tar.Free;
end;

function UnTar(const Input: TStream; outputdir: string; overwrite: boolean): boolean; overload;
var
  Tar : TTarArchive;
  DirRec : TTarDirRec;
begin;
  result := false;
  if not DirectoryExists(outputdir) then
    if not CreateDir(outputdir) then
      exit;
  Tar := TTarArchive.Create(input);
  Tar.reset;
  while Tar.FindNext(DirRec) do
  begin
    if (DirRec.FileType = ftDirectory) then
    begin;
      if not DirectoryExists(outputdir + DirRec.Name) and not CreateDir(outputdir + DirRec.Name) then
        exit
    end else if (DirRec.FileType = ftNormal) then
    begin;
      if FileExists(outputdir + dirrec.name) and not overwrite then
        continue;
      Tar.ReadFile(outputdir + dirrec.name);
    end else
      mDebugLn(format('Unknown filetype in archive. %s',[dirrec.name]));
  end;
  Tar.Free;
  Result := true;

end;

end.

