unit psextension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,mufasabase, virtualextension, MufasaTypes,
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
       function HookExists(const HookName: String): Boolean;override;
       function ExecuteHook(const HookName: String;var Args:TVariantArray; out OutVariant : Variant): Integer;override;
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
  uPSC_extctrls,uPSC_menus, uPSC_mml, //Compile libs
  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_extctrls,uPSR_menus, uPSR_mml, //Runtime-libs
  SimbaUnit,updateform,settingssandbox,bitmaps,files,Dialogs, mmisc//Writeln
  ;

function TSimbaPSExtension.HookExists(const HookName: String): Boolean;
begin
  Result := False;
  if FWorking then
    if PSInstance.Exec.GetProc(HookName) <> InvalidVal then
      result := True;
end;

function TSimbaPSExtension.ExecuteHook(const HookName: String;var Args: TVariantArray; out OutVariant : Variant): Integer;
begin
  result := SExt_error;
  if not FWorking then
    exit;
  try
    outvariant := PSInstance.ExecuteFunction(Args, HookName);
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
  Args : TVariantArray;
begin
  if not HookExists('Free') then
    exit(false);
  result := ExecuteHook('Free',Args,bla) = SExt_ok;
end;

{$I ../../Units/MMLAddon/PSInc/Wrappers/extensions.inc}

procedure TSimbaPSExtension.RegisterMyMethods(Sender: TPSScript);
begin
  Sender.Comp.AddTypes('TStringArray','Array of String');
  Sender.Comp.AddConstantN('AppPath','string').SetString(MainDir + DirectorySeparator);
  Sender.Comp.AddConstantN('IncludePath','string').SetString(SimbaForm.IncludePath);
  Sender.Comp.AddConstantN('PluginPath','string').SetString(SimbaForm.PluginPath);
  Sender.Comp.AddConstantN('FontPath','string').SetString(SimbaForm.FontPath);
  Sender.Comp.AddConstantN('ExtPath','string').SetString(SimbaForm.ExtPath);
  Sender.Comp.AddTypeS('TMsgDlgType', '( mtWarning, mtError, mtInformation, mtConfirmati'
                                      +'on, mtCustom )');
  Sender.Comp.AddTypeS('TMsgDlgBtn', '( mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, m'
                                     +'bIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp )');
  Sender.Comp.AddTypeS('TMsgDlgButtons', 'set of TMsgDlgBtn');
  Sender.AddFunction(@formWritelnEx,'procedure Writeln(s : string)');
  Sender.AddFunction(@ext_GetPage,'function GetPage(const url : string) : string');
  Sender.AddFunction(@ext_DecompressBZip2,'function DecompressBZip2(const input: string;out output : string; const BlockSize: Cardinal): boolean;');
  Sender.AddFunction(@ext_UnTar,'function UnTar(const Input : string; out Content : TStringArray) : boolean;');
  Sender.AddFunction(@ext_UnTarEx,'function UnTarEx(const Input : string;const outputdir : string; overwrite : boolean): boolean;');
  Sender.AddFunction(@DirectoryExists,'Function DirectoryExists (Const Directory : String) : Boolean;');
  Sender.AddFunction(@FileExists,'Function FileExists (Const FileName : String) : Boolean;');
  Sender.AddFunction(@ForceDirectories,'function ForceDirectories(Const Dir: string): Boolean;');
  Sender.AddFunction(@GetFiles, 'function GetFiles(Path, Ext: string): TStringArray;');
  Sender.AddFunction(@GetDirectories,'function GetDirectories(Path: string): TstringArray;');
  Sender.AddFunction(@ext_MessageDlg,'function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;');
  Sender.AddRegisteredPTRVariable('Settings','TMMLSettingsSandbox');
  Sender.AddRegisteredVariable('Simba','TForm');
  Sender.AddRegisteredVariable('Simba_MainMenu','TMainMenu');
end;

procedure TSimbaPSExtension.OnPSExecute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('Simba',SimbaForm);
  Sender.SetVarToInstance('Simba_MainMenu',SimbaForm.MainMenu);
  Sender.SetPointerToData('Settings',@Self.Settings,Sender.FindNamedType('TMMLSettingsSandbox'));
end;

procedure TSimbaPSExtension.SetEnabled(bool: boolean);
var
  temp : variant;
  Args : TVariantArray;
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
          ExecuteHook('attach',Args,temp);
      end;
    end else
      if HookExists('detach') then
        ExecuteHook('detach',Args,temp);
  end;
  inherited SetEnabled(bool);
end;


procedure TMufasaBitmapCopyClientToBitmap(self : TMufasaBitmap; Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);
begin
  self.CopyClientToBitmap(SimbaForm.Manager,resize,x,y,xs,ys,xe,ye);
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
  SIRegister_MML(x);
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
  RIRegister_MML(x);
  with x.FindClass('TMufasaBitmap') do
    RegisterMethod(@TMufasaBitmapCopyClientToBitmap,'CopyClientToBitmap');
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

