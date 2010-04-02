unit virtualextension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,settingssandbox;

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
       function HookExists(HookName: String): Boolean; virtual; abstract;

       { No Custom Arguments just yet... }
       function ExecuteHook(HookName: String; fArgs: Array of Variant; out OutVariant : variant): Integer; virtual; abstract;

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
begin
  Result := '';
  if FName <> '' then
    Result := FName
  else if self.HookExists('GetName') then
  begin;
    if ExecuteHook('GetName',[],OutPut) <> SExt_ok then
      FName := ''
    else
      FName := OutPut;
    result := FName;
  end;
end;

function TVirtualSimbaExtension.GetVersion: String;
var
  OutPut : Variant;
begin
  Result := '';
  if FVersion <> '' then
    Result := FVersion
  else if self.HookExists('GetVersion') then
  begin;
    if ExecuteHook('GetVersion',[],OutPut) <> SExt_ok then
      FVersion := ''
    else
      FVersion := Output;
    result := FVersion;
  end;
end;

end.

