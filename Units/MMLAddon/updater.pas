unit updater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend,MufasaBase,blcksock
  {$IFDEF LINUX}
  ,BaseUnix
  {$ENDIF};

type
  {  TMMLUpdateThread = class(TThread)
          procedure Execute; override;
          constructor Create(CreateSuspended: Boolean);
          destructor Destroy; override;
    end;       }
    { TDownloadThread }

    TDownloadThread = class(TThread)
    private
      InputURL : string;
      ResultStr : PString;
    public
      Done : boolean;
      constructor Create(const URL : string; const Output : PString);
      procedure Execute; override;
    end;

    TMemory = pointer;
    TMMLFunctionBoolean = function: boolean of object;


    TMMLFileDownloader = class(TObject)
         private
           FFileURL: String;
           FBasePath: String;
           FReplacementFile: String;
           FTotal: Integer;
           HTTPSend: THTTPSend;
           FFileSize: Integer;
           FDownloaded: Boolean;
           FOnChange: TMMLFunctionBoolean;
           FOnBeat: TMMLFunctionBoolean;
         private
           procedure SetBasePath(s: string);

         public
            constructor Create;
            destructor Destroy; override;

            { Download and Save the file }
            function DownloadAndSave: Boolean;

            { Replace the new file with the one we downloaded }
            function Replace: Boolean;

            { Where do we get the file from? }
            property FileURL: String read FFileURL write FFileURL;

            { The file to write to. Note that downloading downloads to this
              file with a '_' added to the end of the FileName;
              Replace; is called to replace the old file with the downloaded
              one }
            property ReplacementFile: String read FReplacementFile write FReplacementFile;

            { Return true if we have downloaded the file completely }
            property Downloaded: Boolean read FDownloaded;

            property BasePath: String read FBasePath write SetBasePath;

            { If either of these events return "True", an exception is thrown
              and the download is cancelled. This way we can easily `cancel'
              a download. }
            property OnChange: TMMLFunctionBoolean read FOnChange write FOnChange;

            { Called every 50ms }
            property OnBeat: TMMLFunctionBoolean read FOnBeat write FOnBeat;

            property DownloadedSize : Integer read FTotal;
            property FileSize : integer read FFileSize;
            function GetPercentage: Integer;
         private
            procedure TryToGetFileSize;

            procedure OnMonitor(Sender: TObject; Writing: Boolean;
                      const Buffer: TMemory; Len: Integer);
            procedure OnStatus(Sender: TObject; Reason: THookSocketReason;
                      const Value: String);
            procedure OnHeartBeat(Sender: TObject);
    end;






implementation

uses
  FileUtil,internets;

constructor TDownloadThread.Create(const url : String; const Output : PString);
begin
  inherited Create(true);
  FreeOnTerminate:= True;
  InputURL:= url;
  ResultStr:= Output;
end;

{ TDownloadThread }

procedure TDownloadThread.Execute;
begin
  ResultStr^:= GetPage(InputURL);
  done := true;
end;

procedure TMMLFileDownloader.SetBasePath(s: string);
begin
  if s[length(s) - 1] <> DirectorySeparator then
    s := s + DirectorySeparator;
  FBasePath:=s;
end;

procedure TMMLFileDownloader.TryToGetFileSize;
var
  i,p:integer;
begin
  if assigned(HTTPSend.Headers) then
    for i := 0 to HTTPSend.headers.count - 1 do
    begin
      p :=  Pos('Content-Length: ', HTTPSend.headers.strings[i]);
      if p <> 0 then
        FFileSize := StrToInt(Copy(HTTPSend.headers.strings[i],
        p+length('Content-Length: '),length( HTTPSend.headers.strings[i]) - p) );
    end;
end;

procedure TMMLFileDownloader.OnStatus(Sender: TObject; Reason: THookSocketReason;
          const Value: String);
begin
  if FFileSize = 0 then
    TryToGetFileSize;
  if Assigned(FOnChange) then
    if FOnChange() then
      raise Exception.Create('OnChange event called for a quit');
end;

procedure TMMLFileDownloader.OnHeartBeat(Sender: TObject);
begin
  if Assigned(FOnBeat) then
    if FOnBeat() then
      raise Exception.Create('OnBeat event called for a quit');
end;

procedure TMMLFileDownloader.OnMonitor(Sender: TObject; Writing: Boolean;
  const Buffer: TMemory; Len: Integer);
begin
  if writing then exit;
  Inc(FTotal, len);

  if FFileSize = 0 then
    TryToGetFileSize;
  if Assigned(FOnChange) then
    if FOnChange() then
      raise Exception.Create('OnChange event called for a quit');
end;

function TMMLFileDownloader.GetPercentage: Integer;
begin
  if FFileSize <> 0 then
    Exit( Round( (FTotal / FFileSize) * 100.0) )
  else
    Exit(-1);
end;

function TMMLFileDownloader.DownloadAndSave: Boolean;

var
  response: TStream;

begin
  Result := False;
  HTTPSend := THTTPSend.Create;
  HTTPSend.Sock.OnMonitor:=@Self.OnMonitor;
  HTTPSend.Sock.OnStatus:=@Self.OnStatus;
  HTTPSend.Sock.OnHeartbeat:=@Self.OnHeartBeat;
  HTTPSend.Sock.HeartbeatRate:=50;

  if FReplacementFile = '' then
    raise Exception.Create('ReplacementFile not set');
  if FileURL = '' then
    raise Exception.Create('FileURL not set');

  Response := TFileStream.Create(UTF8ToSys(FBasePath + FReplacementFile + '_'), fmCreate);
  try
    Result := HTTPSend.HTTPMethod('GET', FileURL);

    if Result then
    begin
      Response.Seek(0, soFromBeginning);
      Response.CopyFrom(HTTPSend.Document, 0);
    end;
    FDownloaded := True;
  except
    mDebugLn('DownloadAndSave: Exception Occured');
    Result := False;
  end;
  HTTPSend.Free;
  Response.Free;
  if not result then
    Raise Exception.Create('Throwing it so we can catch it later');
end;

function TMMLFileDownloader.Replace: Boolean;
begin
  { Change to messages + Exit(False) instead of exceptions? }
  if not Downloaded then
  begin
    mDebugLn('Nothing downloaded');
    exit(False);
   // raise Exception.Create('Nothing downloaded');
  end;
  if FReplacementFile = '' then
  begin
    mDebugLn('ReplacementFile not se');
    exit(False);
    //raise Exception.Create('ReplacementFile not set');
  end;
  if not fileExistsUTF8(FBasePath + FReplacementFile) then
  begin
    mDebugLn('ReplacementFile not found');
    exit(False);
    //raise Exception.Create('ReplacementFile not found');
  end;
  if not fileExistsUTF8(FBasePath + FReplacementFile+ '_') then
  begin
    mDebugLn('ReplacementFile + _ not found');
    exit(False);
    //raise Exception.Create('ReplacementFile + _ not found');
  end;
  RenameFileUTF8(FBasePath + FReplacementFile, FBasePath + FReplacementFile+'_old_');
  RenameFileUTF8(FBasePath + FReplacementFile +'_', FBasePath + FReplacementFile);
  DeleteFileUTF8(FBasePath + FReplacementFile+'_old_');
  {$IFDEF LINUX}
  fpchmod(FBasePath + FReplacementFile, S_IRUSR or S_IWUSR or S_IXUSR or S_IRGRP
                or S_IXGRP or S_IROTH or S_IXOTH);
  {$ENDIF}

end;

constructor TMMLFileDownloader.Create;
begin
  inherited Create;
  FTotal := 0;
  FFileSize := 0;
  FDownloaded := False;
  FReplacementFile:='';
  FFileURL := '';
  FBasePath:= '';
  FOnChange := nil;
  FOnBeat := nil;
end;


destructor TMMLFileDownloader.Destroy;
begin

  inherited;
end;

end.

