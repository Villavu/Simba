unit psextension;

{$mode objfpc}

interface

uses
  Classes, SysUtils,mufasabase, virtualextension,
  uPSComponent,uPSCompiler, uPSRuntime, uPSPreProcessor,forms;



type

    { TSimbaPSExtension }

    TSimbaPSExtension = class(TVirtualSimbaExtension)
    public
      constructor Create(FileStr: String; StartDisabled : boolean = false);
      destructor Destroy; override;
    private
      PSInstance: TPSScript;
      FWorking: Boolean;
      Script: TStringList;
      procedure StartExtension;
    private
      function FreeScript: boolean;
      function InitScript: Boolean;
      procedure OutputMessages;

    public
       function HookExists(HookName: String): Boolean;override;
       function ExecuteHook(HookName: String; fArgs: Array of Variant; out OutVariant : Variant): Integer;override;
       property Working : boolean read FWorking;
    protected
       procedure RegisterPSCComponents(Sender: TObject; x: TPSPascalCompiler);
       procedure RegisterPSRComponents(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
       procedure RegisterMyMethods(Sender: TPSScript);
       procedure OnPSExecute(Sender: TPSScript);
       procedure SetEnabled(bool : boolean);override;
    end;


implementation
uses
  uPSC_std, uPSC_controls,uPSC_classes,uPSC_graphics,uPSC_stdctrls,uPSC_forms,
  uPSC_extctrls,uPSC_menus, //Compile libs
  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_extctrls,uPSR_menus, //Runtime-libs
  testunit//Writeln
  ;

function TSimbaPSExtension.HookExists(HookName: String): Boolean;
begin
  Result := False;
  if FWorking then
    if PSInstance.Exec.GetProc(HookName) <> InvalidVal then
      result := True;
end;

function TSimbaPSExtension.ExecuteHook(HookName: String; fArgs: Array of Variant; out OutVariant : Variant): Integer;
begin
  result := SExt_error;
  if not FWorking then
    exit;
  try
    outvariant := PSInstance.ExecuteFunction(fArgs, HookName);
    result := SExt_ok;
  except
    on e : exception do
      formWritelnEx(format('Error in Simba extension (%s): %s',[Self.GetName,e.message]));
  end;
end;

constructor TSimbaPSExtension.Create(FileStr: String; StartDisabled: boolean = false);
begin
  inherited create;
  FWorking := False;
  FileName := FileStr;
  try
    Script := TStringList.Create;
    Script.LoadFromFile(FileName);
  except
    raise Exception.CreateFmt('File %s could not be read', [FileName]);
  end;
  FEnabled := false;
  PSInstance := nil;
  if not StartDisabled then
    StartExtension;
end;

function TSimbaPSExtension.InitScript: Boolean;
begin
  if not HookExists('init') then
    exit(false);
  result := true;
  try
    PSInstance.ExecuteFunction([], 'init');
  except
    result := false;
  end;
end;

function TSimbaPSExtension.FreeScript: boolean;
var
  bla : variant;
begin
  if not HookExists('Free') then
    exit(false);
  result := ExecuteHook('Free',[],bla) = SExt_ok;
end;


procedure TSimbaPSExtension.RegisterMyMethods(Sender: TPSScript);
begin
  Sender.AddFunction(@formWritelnEx,'procedure Writeln(s : string)');
  Sender.AddRegisteredVariable('Simba','TForm');
  Sender.AddRegisteredVariable('Simba_MainMenu','TMainMenu');
end;

procedure TSimbaPSExtension.OnPSExecute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('simba',Form1);
  Sender.SetVarToInstance('Simba_MainMenu',Form1.MainMenu);
end;

procedure TSimbaPSExtension.SetEnabled(bool: boolean);
var
  temp : variant;
begin
  if bool <> FEnabled then
  begin
    if bool then
    begin;
      if not assigned(PSInstance) then //We enable it for the first time, calls SetEnabled.
        StartExtension
      else
      begin
        if not FWorking then
          Exit;
        if hookexists('attach') then
          ExecuteHook('attach',[],temp);
      end;
    end else
      if HookExists('detach') then
        ExecuteHook('detach',[],temp);
  end;
  inherited SetEnabled(bool);
end;

procedure TSimbaPSExtension.RegisterPSCComponents(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Controls(x);
  SIRegister_Graphics(x, True);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_ExtCtrls(x);
  SIRegister_Menus(x);
end;

procedure TSimbaPSExtension.RegisterPSRComponents(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Controls(x);
  RIRegister_Graphics(x, True);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_ExtCtrls(x);
  RIRegister_Menus(x);
end;

destructor TSimbaPSExtension.Destroy;
begin
  FreeScript;
  if Assigned(PSInstance) then
    FreeAndNil(PSInstance);
  inherited;
end;

procedure TSimbaPSExtension.StartExtension;
begin
  if assigned(PSInstance) then
    exit;//Already started..
  { Create script, and see if the extension is valid. (If it compiles) }
  PSInstance := TPSScript.Create(nil);

  PSInstance.Script := Self.Script;
  PSInstance.OnCompImport:=@RegisterPSCComponents;
  PSInstance.OnExecImport:=@RegisterPSRComponents;
  PSInstance.OnCompile:=@RegisterMyMethods;
  PSInstance.OnExecute:=@OnPSExecute;

  formWritelnEx(Format('Loading extension %s', [FileName]));
  try
    FWorking := PSInstance.Compile;
  except
    on e : exception do
      FormWritelnEx(format('Error in Simba extension (%s) : %s',[FileName,e.message]));
  end;
  if FWorking then
    formWritelnEx('Extension Enabled')
  else
  begin
    formWritelnEx('Extension Disabled - Did not compile');
    OutputMessages;
  end;

  if InitScript then
    mDebugLn('Init procedure succesfully called')
  else
    mDebugLn('Init procedure didn''t execute right, or couldn''t be found');
  Enabled:= FWorking;
end;

procedure TSimbaPSExtension.OutputMessages;
var
  l: Longint;
begin
  for l := 0 to PSInstance.CompilerMessageCount - 1 do
    formWritelnEx(PSInstance.CompilerErrorToStr(l) + ' at line ' + inttostr(PSInstance.CompilerMessages[l].Row));
end;



end.

