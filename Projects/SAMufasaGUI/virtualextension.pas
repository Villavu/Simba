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


    { Each hook has a HookName which the extension uses to identify the hook.
      Additionally, variables are exported. Currently we will use an Array of Variant.
      Therefore, we will have to specify what amount Arguments we will pass.
      ( So it is not type safe... Not like Variants are type safe... )
    }
    type TEventHook = record
         HookName: String;
         ArgumentCount: Integer;
    end;

var
    EventHooks: Array [0..7] of TEventHook =
      (    (HookName : 'onScriptCompile' ; ArgumentCount : 1),
	   (HookName : 'onScriptStart'   ; ArgumentCount : 1),
	   (HookName : 'onScriptPause'   ; ArgumentCount : 1),
	   (HookName : 'onScriptStop'    ; ArgumentCount : 1),
	   (HookName : 'onColourPick'    ; ArgumentCount : 3),
	   (HookName : 'onOpenFile'      ; ArgumentCount : 1),
	   (HookName : 'onOpenSocket'    ; ArgumentCount : 1),
	   (HookName : 'onWriteFile'     ; ArgumentCount : 1));

implementation

end.

