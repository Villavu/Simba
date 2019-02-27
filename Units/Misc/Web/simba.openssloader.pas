unit simba.openssloader;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, zipper, lcltype;

type
  TSimbaOpenSSLLoader = class
  protected
    procedure CreateInputStream(Sender: TObject; var Stream: TStream);
    procedure CreateOutputStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
  public
    function Load: Boolean;
  end;

implementation

uses
  simba.environment,
  simba.openssl;

procedure TSimbaOpenSSLLoader.CreateInputStream(Sender: TObject; var Stream: TStream);
begin
  // resource files are added in `project options > resources`

  {$IFDEF WINDOWS}
    {$IFDEF CPU64}
      Stream := TResourceStream.Create(HInstance, 'OPENSSL_WIN64', RT_RCDATA);
    {$ELSE}
      Stream := TResourceStream.Create(HInstance, 'OPENSSL_WIN32', RT_RCDATA);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    Stream := TResourceStream.Create(HInstance, 'OPENSSL_LINUX64', RT_RCDATA);
  {$ENDIF}
end;

procedure TSimbaOpenSSLLoader.CreateOutputStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
begin
  if FileExists(SimbaEnvironment.LibPath + Item.DiskFileName) then
    Stream := TMemoryStream.Create() // have to return something
  else
    Stream := TFileStream.Create(SimbaEnvironment.LibPath + Item.DiskFileName, fmCreate);
end;

function TSimbaOpenSSLLoader.Load: Boolean;
begin
  // unpack and install from resource files. (project options > resources)
  with TUnZipper.Create() do
  try
    OnOpenInputStream := @CreateInputStream;
    OnCreateStream := @CreateOutputStream;
    UnZipAllFiles();
  finally
    Free();
  end;

  // load from our installed libs.
  DLLUtilName := SimbaEnvironment.LibPath + DLLUtilName;
  DLLSSLName := SimbaEnvironment.LibPath + DLLSSLName;

  try
    InitSSLInterface();
  except
  end;

  if IsSSLLoaded then
    Exit(True);

  // if error revert back to searching on the system.
  DLLSSLName := ExtractFileName(DLLSSLName);
  DLLUtilName := ExtractFileName(DLLUtilName);

  Exit(False);
end;

end.

