unit virtualextension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,settingssandbox,MufasaTypes;

type
    { TVirtualSimbaExtension }

    TVirtualSimbaExtension = class(TObject)
    protected
      FName: String;
      FVersion : string;
      FFilename : string;
      FEnabled : boolean;
      FSettings : TMMLSettingsSandbox;
      procedure SetEnabled(bool : boolean); virtual;
    public
      OnChange : TNotifyEvent;
       { Must be implemented }
       function HookExists(const HookName: String): Boolean; virtual; abstract;

       { No Custom Arguments just yet... }
       function ExecuteHook(const HookName: String;var Args: TVariantArray; out OutVariant : variant): Integer; virtual; abstract;

       function GetName : string;
       function GetVersion : String;
       property Settings : TMMLSettingsSandbox read FSettings write FSettings;
       property Filename : string read FFilename write FFilename;
       property Enabled : boolean read FEnabled write SetEnabled;
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

const
    SExt_ok = 0;
    SExt_error = 1;
    SExt_OnColourPick = 0;
    SExt_onOpenFile = 1;
    SExt_onWriteFile = 2;
    SExt_onOpenConnection = 3;
    SExt_onScriptStart = 4;

    EventHooks: Array [0..8] of TEventHook =
    (	   (HookName : 'onColourPick'    ; ArgumentCount : 3), //const colour,colourx,coloury : integer;
	   (HookName : 'onOpenFile'      ; ArgumentCount : 2), //var filename : string; var Continue : boolean
	   (HookName : 'onWriteFile'     ; ArgumentCount : 2), //var filename : string; var Continue : boolean
           (HookName : 'onOpenConnection'; ArgumentCount : 2), //var url : string; var Continue : boolean
           (HookName : 'onScriptStart'   ; ArgumentCount : 2), //var Script : string; var Continue : boolean;   This is called BEFORE it compiles/executes
           (HookName : 'onScriptCompile' ; ArgumentCount : 1),
	   (HookName : 'onScriptExecute' ; ArgumentCount : 1),
	   (HookName : 'onScriptPause'   ; ArgumentCount : 1),
	   (HookName : 'onScriptStop'    ; ArgumentCount : 1));


implementation

{ TVirtualSimbaExtension }

procedure TVirtualSimbaExtension.SetEnabled(bool: boolean);
begin
  if assigned(OnChange) then
    OnChange(self);
  FEnabled:= bool;
end;

function TVirtualSimbaExtension.GetName: string;
var
  OutPut : Variant;
  Args : TVariantArray;
begin
  Result := '';
  if FName <> '' then
    Result := FName
  else if self.HookExists('GetName') then
  begin;
    if ExecuteHook('GetName',Args,OutPut) <> SExt_ok then
      FName := ''
    else
      FName := OutPut;
    result := FName;
  end;
end;

function TVirtualSimbaExtension.GetVersion: String;
var
  OutPut : Variant;
  Args : TVariantArray;
begin
  Result := '';
  if FVersion <> '' then
    Result := FVersion
  else if self.HookExists('GetVersion') then
  begin;
    if ExecuteHook('GetVersion',Args,OutPut) <> SExt_ok then
      FVersion := ''
    else
      FVersion := Output;
    result := FVersion;
  end;
end;

end.

