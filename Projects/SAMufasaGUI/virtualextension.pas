unit virtualextension;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
    TVirtualSimbaExtension = class(TObject)
    public

       { Must be implemented }
       function HookExists(HookName: String): Boolean; virtual; abstract;

       { No Custom Arguments just yet... }
       function ExecuteHook(HookName: String): Integer; virtual; abstract;
    private
       FName: String;

    property GetName: String read Fname;

    end;

implementation

end.

