unit extensionmanager;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type

    (**
      TExtensionManager holds a list of VirtualExtensions, and
      has functions to easily handle hooks.
    *)

    TExtensionManager = class(TObject)
    public
           constructor Create;
           destructor Destroy; override;
    end;



implementation


constructor TExtensionManager.Create;
begin

end;

destructor TExtensionManager.Destroy;
begin

end;

end.

