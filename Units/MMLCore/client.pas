unit Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes, Window, Input;

type
    TClient = class(TObject)
        constructor Create;
        destructor Destroy; override;

        public
            MWindow: TMWindow;
            MInput: TMInput;

    end;

implementation

// Possibly pass arguments to a default window.
constructor TClient.Create;
begin
  inherited Create;

  MWindow := TMWindow.Create(Self);
  MInput := TMInput.Create(Self);
end;

destructor TClient.Destroy;
begin
  MWindow.Destroy;
  MInput.Destroy;

  inherited;
end;

end.

