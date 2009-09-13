unit Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes, Window, Input, Files, Finder,Bitmaps;

type
    TClient = class(TObject)
        constructor Create;
        destructor Destroy; override;

        public
            MWindow: TMWindow;
            MInput: TMInput;
            MFiles: TMFiles;
            MFinder: TMFinder;
            MBitmaps : TMBitmaps;

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
  MBitmaps := TMBitmaps.Create(self);
end;

destructor TClient.Destroy;
begin
  MBitmaps.Free;
  MFinder.Free;
  MFiles.Free;
  MInput.Free;
  MWindow.Free;


  inherited;
end;

end.

