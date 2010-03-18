unit eventextension;

{$mode objfpc}

interface

uses
  Classes, SysUtils, virtualextension,
  uPSComponent,uPSCompiler, uPSRuntime, uPSPreProcessor,forms;



type
    TSimbaPSEventExtension = class(TVirtualSimbaExtension)
    public
        constructor Create(FileName: String);
        destructor Destroy; override;
    private
      PSInstance: TPSScript;
      FEnabled: Boolean;
      Script: TStringList;

    private
      function InitScript: Boolean;
      procedure OutputMessages;

    public
       function HookExists(HookName: String): Boolean; override;
       function ExecuteHook(HookName: String): Integer; override;
    protected
       procedure RegisterPSCComponents(Sender: TObject; x: TPSPascalCompiler);
       procedure RegisterPSRComponents(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
       procedure RegisterMyMethods(Sender: TPSScript);

    end;


implementation
uses
  uPSC_std, uPSC_controls,uPSC_classes,uPSC_graphics,uPSC_stdctrls,uPSC_forms,
  uPSC_extctrls, //Compile libs
  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_extctrls //Runtime-libs;
  ;

procedure createf;
var
  a: TForm;
begin
  a:=TForm.Create(nil);
  a.ShowModal;
  a.Free;
end;


function TSimbaPSEventExtension.HookExists(HookName: String): Boolean;
begin

end;

function TSimbaPSEventExtension.ExecuteHook(HookName: String): Integer;
begin
end;

constructor TSimbaPSEventExtension.Create(FileName: String);
var
  fStr:TFileStream;

begin
  inherited create;

  try
    Script := TStringList.Create;
    fStr := TFileStream.Create(FileName, fmOpenRead);
    Script.LoadFromStream(fStr);
    fStr.Free;
  except
    raise Exception.CreateFmt('File %s could not be read', [FileName]);
  end;

  FEnabled := False;

  { Create script, and see if the extension is valid. (If it compiles) }

  PSInstance := TPSScript.Create(nil);

  PSInstance.Script := Self.Script;
  PSInstance.OnCompImport:=@RegisterPSCComponents;
  PSInstance.OnExecImport:=@RegisterPSRComponents;
  PSInstance.OnCompile:=@RegisterMyMethods;

  Writeln(Format('%s: Script: %s', [FileName, Self.Script.Text]));

  try
    FEnabled := PSInstance.Compile;
  finally
    if FEnabled then
      writeln('Extension Enabled')
    else
    begin
      writeln('Extension Disabled - Did not compile');
      OutputMessages;
    end;
  end;

  FEnabled := InitScript();
  if FEnabled then
    writeln('It exists')
  else
    writeln('It does not exist - or something went wrong while executing it.');
 // writeln(PSInstance.ExecuteFunction([], 'test'));
end;

function TSimbaPSEventExtension.InitScript: Boolean;
begin
  if PSInstance.GetProcMethod('init').Data = nil then
    exit(false);

  try
    PSInstance.ExecuteFunction([], 'init');
  except
    result := false;
    exit;
  end;
  exit(true);
end;


procedure TSimbaPSEventExtension.RegisterMyMethods(Sender: TPSScript);
begin
  Sender.AddFunction(@createf, 'procedure createf;');
end;

procedure TSimbaPSEventExtension.RegisterPSCComponents(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Controls(x);
  SIRegister_Graphics(x, True);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_ExtCtrls(x);
end;

procedure TSimbaPSEventExtension.RegisterPSRComponents(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Controls(x);
  RIRegister_Graphics(x, True);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_ExtCtrls(x);
end;

destructor TSimbaPSEventExtension.Destroy;
begin
  if Assigned(PSInstance) then
    FreeAndNil(PSInstance);


  WriteLn('Closing extension');

  inherited;
end;


procedure TSimbaPSEventExtension.OutputMessages;
var
  l: Longint;
  b: Boolean;
begin
  b := False;
  for l := 0 to PSInstance.CompilerMessageCount - 1 do
  begin
    if (not b) and (PSInstance.CompilerMessages[l] is TIFPSPascalCompilerError) then
    begin
      b := True;
       with PSInstance.CompilerMessages[l] do
         writeln(MessageToString);
      {if OnError <> nil then
        with PSInstance.CompilerMessages[l] do
          HandleError(Row, Col, Pos, MessageToString,errCompile, ModuleName)
      else   }
        writeln(PSInstance.CompilerErrorToStr(l) + ' at line ' + inttostr(PSInstance.CompilerMessages[l].Row));
    end else
      Writeln(PSInstance.CompilerErrorToStr(l) + ' at line ' + inttostr(PSInstance.CompilerMessages[l].Row));

  end;
end;



end.

