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
    private
           Extensions: TList;
    public
           function HandleHook(HookName: String; Args: Array of Variant): Variant;
    end;



implementation
uses
  pseventextension, virtualextension;


constructor TExtensionManager.Create;
begin
  Extensions := TList.Create;
end;

destructor TExtensionManager.Destroy;
var
  i: Integer;
begin
  {
  for i := 0 to Extensions.Count - 1 do
    TVirtualSimbaExtension(Extensions.Items[i]).Free;
  }
  Extensions.Free;
end;

// How do we return more than one result?
function TExtensionManager.HandleHook(HookName: String; Args: Array of Variant): Variant;
var
  i: Integer;
begin
  for i := 0 to Extensions.Count -1 do
    if TVirtualSimbaExtension(Extensions.Items[i]).HookExists(HookName) then
      if TVirtualSimbaExtension(Extensions.Items[i]).ExecuteHook(HookName, Args, Result) <> 0 then
      begin
        // Not succesfull.
      end;
end;

end.

