unit Input;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mufasatypes;
type
    TMInput = class(TObject)
            constructor Create(Client: TObject);
            destructor Destroy; override;

            procedure GetMousePos(var X, Y: Integer);

         public
            Client: TObject;

    end;

implementation

uses
    Client;

constructor TMInput.Create(Client: TObject);
begin
  inherited Create;
  Self.Client := Client;
end;

destructor TMInput.Destroy;
begin

  inherited;
end;

procedure TMInput.GetMousePos(var X, Y: Integer);
begin

end;

end.

