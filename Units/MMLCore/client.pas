unit Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes, Window, Input, Files, Finder;

type
    TClient = class(TObject)
        constructor Create;
        destructor Destroy; override;

        public
            MWindow: TMWindow;
            MInput: TMInput;
            MFiles: TMFiles;
            MFinder: TMFinder;

    end;

implementation

// Possibly pass arguments to a default window.
constructor TClient.Create;
begin
  inherited Create;

  MWindow := TMWindow.Create(Self);
  MInput := TMInput.Create(Self);
  MFiles := TMFiles.Create;
  MFinder := TMFinder.Create(Self);
end;

destructor TClient.Destroy;
begin
  MFinder.Destroy;
  MFiles.Destroy;
  MInput.Destroy;
  MWindow.Destroy;


  inherited;
end;

end.

