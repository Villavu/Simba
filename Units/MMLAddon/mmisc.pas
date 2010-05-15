unit mmisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bzip2, bzip2comn,bzip2stream, libtar,mufasabase,mufasatypes;

function DecompressBZip2(const input : TStream; const BlockSize : Cardinal = 4096) : TMemoryStream;
function UnTar(const Input : TStream) : TStringArray;overload;
function UnTar(const Input : TStream;const outputdir : string; overwrite : boolean): boolean;overload;

type
  { TProcThread }
  TProcThread = class(TThread)
  public
    StartWait : Cardinal;
    ClassProc : procedure of object;
    NormalProc : procedure;
    constructor Create;
    procedure Execute; override;
  end;

  { TDecompressThread }

  TDecompressThread = class(TThread)
  private
    FFinished : boolean;
    Finput : TStream;
    FBlockSize : Cardinal;
  public
    Result : TMemoryStream;
    constructor Create(const input : TStream; const BlockSize : Cardinal = 4096);
    procedure Execute; override;
    property Finished : boolean read FFinished;
  end;

  { TUntarThread }

  TUntarThread = class(TThread)
  private
    FFinished : boolean;
    Finput : TStream;
    FOverWrite : boolean;
    FOutputDir : string;
  public
    Result : boolean;
    constructor Create(const Input : TStream;const outputdir : string; overwrite : boolean);
    procedure Execute; override;
    property Finished : boolean read FFinished;
  end;

implementation

uses
  FileUtil;

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

function UnTar(const Input: TStream; const outputdir: string; overwrite: boolean): boolean; overload;
var
  Tar : TTarArchive;
  Succ : boolean;
  DirRec : TTarDirRec;
  FS : TFileStream;
begin;
  result := false;
  if not DirectoryExists(outputdir) then
    if not CreateDir(outputdir) then
      exit;
  Tar := TTarArchive.Create(input);
  Tar.reset;
  Succ := True;
  while Tar.FindNext(DirRec) do
  begin
    if (DirRec.FileType = ftDirectory) then
    begin;
      if not DirectoryExists(outputdir + DirRec.Name) and not CreateDir(outputdir + DirRec.Name) then
      begin
        Succ := false;
        break;
      end;
    end else if (DirRec.FileType = ftNormal) then
    begin;
      if FileExistsUTF8(outputdir + dirrec.name) and not overwrite then
        continue;
      try
        FS := TFileStream.Create(UTF8ToSys(outputdir +dirrec.name),fmCreate);
        tar.ReadFile(fs);
        FS.Free;
      except
        Succ := false;
        break;
      end;
    end else
      mDebugLn(format('Unknown filetype in archive. %s',[dirrec.name]));
  end;
  Tar.Free;
  Result := Succ;
end;

constructor TProcThread.Create;
begin
  inherited Create(true);
  FreeOnTerminate:= True;
end;


{ TProcThread }

procedure TProcThread.Execute;
begin
  if startwait <> 0 then
    sleep(StartWait);
  if NormalProc <> nil then
    NormalProc;
  if ClassProc <> nil then
    ClassProc;
end;

constructor TDecompressThread.Create(const input: TStream;
  const BlockSize: Cardinal);
begin
  inherited Create(True);
  FFinished:= False;
  FBlockSize:= BlockSize;
  FInput := Input;
  Result := nil;
end;

{ TDecompressThread }

procedure TDecompressThread.Execute;
begin
  if Finput <> nil then
    result := DecompressBZip2(Finput,FBlocksize);
  Ffinished := True;
end;

{ TUntarThread }

constructor TUntarThread.Create(const Input: TStream; const outputdir: string;
  overwrite: boolean);
begin
  inherited Create(true);
  FFinished:= false;
  FInput := Input;
  FOutputDir:= OutputDir;
  FOverWrite:= overwrite;
  Result:= False;
end;

procedure TUntarThread.Execute;
begin
  if (Finput <> nil) and (FOutputDir <> '') then
    result := UnTar(FInput,Foutputdir,FOverWrite);
  FFinished:= True;
end;

end.

