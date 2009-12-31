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

    TMMLFileDownloader = class(TObject)
         private
           FFileURL: String;
           FReplacementFile: String;
           FTotal: Integer;
           HTTPSend: THTTPSend;
           FFileSize: Integer;
           FDownloaded: Boolean;
           FOnChange: TProcedure;

         public
            constructor Create;
            destructor Destroy; override;

            function DownloadAndSave: Boolean;
            function Replace: Boolean;

            property FileURL: String read FFileURL write FFileURL;
            property ReplacementFile: String read FReplacementFile write FReplacementFile;
            property Downloaded: Boolean read FDownloaded;
            property OnChange: TProcedure read FOnChange write FOnChange;


            function GetPercentage: Integer;

         private
            procedure TryToGetFileSize;

            procedure OnMonitor(Sender: TObject; Writing: Boolean;
                      const Buffer: TMemory; Len: Integer);
            procedure OnStatus(Sender: TObject; Reason: THookSocketReason;
                      const Value: String);





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
    FOnChange();
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
    FOnChange();
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
  HTTPSend := THTTPSend.Create;
  HTTPSend.Sock.OnMonitor:=@Self.OnMonitor;
  HTTPSend.Sock.OnStatus:=@Self.OnStatus;
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
  finally
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

