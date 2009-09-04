unit files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
    TMWindow = class(TObject)
            constructor Create(Client: TObject);
            destructor Destroy; override;
        public


    end;

implementation

constructor TMWindow.Create(Client: TObject);
begin
  inherited Create;
end;

destructor TMWindow.Destroy;
begin

  inherited;
end;

end.

