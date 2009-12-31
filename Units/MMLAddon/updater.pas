unit updater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend,blcksock;

type
  {  TMMLUpdateThread = class(TThread)
          procedure Execute; override;
          constructor Create(CreateSuspended: Boolean);
          destructor Destroy; override;
    end;       }
    TMemory = pointer;
    TFunctionBoolean = function: boolean;


    TMMLFileDownloader = class(TObject)
         private
           FFileURL: String;
           FReplacementFile: String;
           FTotal: Integer;
           HTTPSend: THTTPSend;
           FFileSize: Integer;
           FDownloaded: Boolean;
           FOnChange: TFunctionBoolean;
           FOnBeat: TFunctionBoolean;

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

            { If either of these events return "True", an exception is thrown
              and the download is cancelled. This way we can easily `cancel'
              a download. }
            property OnChange: TFunctionBoolean read FOnChange write FOnChange;

            { Called every 50ms }
            property OnBeat: TFunctionBoolean read FOnBeat write FOnBeat;

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
      raise Exception.Create('OnChange event called for a quit');
end;

procedure TMMLFileDownloader.OnMonitor(Sender: TObject; Writing: Boolean;
  const Buffer: TMemory; Len: Integer);

var
  i,p:integer;
begin
  if writing then exit;
  Inc(FTotal, len);

  if FFileSize = 0 then
    TryToGetFileSize;
  if Assigned(FOnChange) then
    if FOnChange() then
      raise Exception.Create('OnChange event called for a quit');
 // writeln('Percent done: ' + IntToStr(GetPercentage));
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
  i:integer;
  f: TFileStream;

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

  Response := TFileStream.Create(FReplacementFile + '_', fmCreate);
  try
    Result := HTTPSend.HTTPMethod('GET', FileURL);

    if Result then
    begin
      Response.Seek(0, soFromBeginning);
      Response.CopyFrom(HTTPSend.Document, 0);
    end;
    FDownloaded := True;
  except
    writeln('DownloadAndSave: Exception Occured');
    HTTPSend.Free;
    Response.Free;
  end;
end;

function TMMLFileDownloader.Replace: Boolean;
begin
  if not Downloaded then
    raise Exception.Create('Nothing downloaded');
  if FReplacementFile = '' then
    raise Exception.Create('ReplacementFile not set');
  if not FileExists(FReplacementFile) then
    raise Exception.Create('ReplacementFile not found');
  if not FileExists(FReplacementFile+ '_') then
    raise Exception.Create('ReplacementFile + _ not found');

  RenameFile(FReplacementFile, FReplacementFile+'_old_');
  RenameFile(FReplacementFile+'_', FReplacementFile);
  DeleteFile(FReplacementFile+'_old_');
end;

constructor TMMLFileDownloader.Create;
begin
  inherited Create;
  FTotal := 0;
  FFileSize := 0;
  FDownloaded := False;
  FReplacementFile:='';
  FFileURL := '';


end;


destructor TMMLFileDownloader.Destroy;
begin



  inherited;
end;

end.

