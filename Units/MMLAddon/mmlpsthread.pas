unit mmlpsthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, client, uPSComponent,uPSCompiler,uPSRuntime,SynMemo;

type

    { TMMLPSThread }

    TMMLPSThread = class(TThread)
    protected
//      PSScript : TPSScript;
//      PSClient : TPSScript;
//        Client: TClient;
//      DebugTo : TStrings;
      PSScript : TPSScript;
      DebugTo : TSynMemo;
      procedure OnCompile(Sender: TPSScript);
      procedure AfterExecute(Sender : TPSScript);
      function RequireFile(Sender: TObject; const OriginFileName: String;
                          var FileName, OutPut: string): Boolean;
      procedure OnCompImport(Sender: TObject; x: TPSPascalCompiler);
      procedure OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
      procedure OutputMessages;
      procedure OnThreadTerminate(Sender: TObject);
      procedure Execute; override;
    public
      Client : TClient;
      procedure SetPSScript(Script : string);
      procedure SetDebug( Strings : TSynMemo );
//      function CompilePSScript : boolean;
//      function
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
    end;

implementation
uses
  MufasaTypes,
  {$ifdef mswindows}windows,{$endif}
  uPSC_std, uPSC_controls,uPSC_classes,uPSC_graphics,uPSC_stdctrls,uPSC_forms,
  uPSC_extctrls, //Compile-libs

  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_extctrls, //Runtime-libs

  lclintf; // for GetTickCount and others.


threadvar
  CurrThread : TMMLPSThread;

{Some General PS Functions here}
procedure Writeln(str : string);
begin;
  if CurrThread.DebugTo <> nil then
    CurrThread.DebugTo.Lines.Add(Str);
  //Just overwriting itz.. soz.
end;

function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;
var
  i : integer;
begin;
  Writeln('We have a length of: '  + inttostr(length(v)));
  Try
    Result := CurrThread.PSScript.Exec.RunProcPVar(v,CurrThread.PSScript.Exec.GetProc(Procname));
  Except
    Writeln('We has some errors :-(');
  end;
end;

{
  Note to Raymond: For PascalScript, Create it on the .Create,
  Execute it on the .Execute, and don't forget to Destroy it on .Destroy.

  Furthermore, all the wrappers can be in the unit "implementation" section.
  Better still to create an .inc for it, otherwise this unit will become huge.
  (You can even split up the .inc's in stuff like color, bitmap, etc. )

  Also, don't add PS to this unit, but make a seperate unit for it.
  Unit "MMLPSThread", perhaps?

  See the TestUnit for use of this thread, it's pretty straightforward.

  It may also be wise to turn the "Importing of wrappers" into an include as
  well, it will really make the unit more straightforward to use and read.
}


constructor TMMLPSThread.Create(CreateSuspended : boolean);
begin
  {if Client <> nil then
    Writeln('ThreadClient seems to be set, so not recreating it.') //reset client to defaults?
    //ThreadClient.ResetToDefaults
  else  }

  // ^ Every time a script is compiled, a new thread is created. There will no
  // existing client left. I commented the above code out.
  Client := TClient.Create;

 { if PSScript <> nil then
    PSScript.Free;      }
  // ^ Same, makes no sense. :-)

  // Create Stuff here
  PSScript := TPSScript.Create(nil);
  PSScript.UsePreProcessor:= True;
  PSScript.OnNeedFile := @RequireFile;

  PSScript.OnCompile:= @OnCompile;
  PSScript.OnCompImport:= @OnCompImport;
  PSScript.OnExecImport:= @OnExecImport;
  PSScript.OnAfterExecute:= @AfterExecute;

  // Set some defines
  {$I PSInc/psdefines.inc}

  FreeOnTerminate := True;
  Self.OnTerminate := @Self.OnThreadTerminate;
  inherited Create(CreateSuspended);
end;

procedure TMMLPSThread.OnThreadTerminate(Sender: TObject);
begin
//  Writeln('Terminating the thread');
end;

destructor TMMLPSThread.Destroy;
begin
  Client.Free;
  PSScript.Free;
  inherited;
end;

// include PS wrappers

{$I PSInc/Wrappers/colour.inc}


procedure TMMLPSThread.OnCompile(Sender: TPSScript);
begin
  //Here we add all the initalizing, of BMPArray etc

  // ^ This will all be done with Client.Create;

  // Here we add all the functions to the engine.
  {$I PSInc/pscompile.inc}
end;

procedure TMMLPSThread.AfterExecute(Sender: TPSScript);
begin
  //Here we add all the Script-freeing-leftovers (like BMParray etc)
  // ^ This will all be done with Client.Destroy;
end;

function TMMLPSThread.RequireFile(Sender: TObject;
  const OriginFileName: String; var FileName, OutPut: string): Boolean;
begin

end;

procedure TMMLPSThread.OnCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Controls(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x, true);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_ExtCtrls(x);
end;

procedure TMMLPSThread.OnExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Controls(x);
  RIRegister_Graphics(x, True);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_ExtCtrls(x);
end;

procedure TMMLPSThread.OutputMessages;
var
  l: Longint;
  b: Boolean;
begin
  b := False;
  for l := 0 to PSScript.CompilerMessageCount - 1 do
  begin
    Writeln(PSScript.CompilerErrorToStr(l));
    if (not b) and (PSScript.CompilerMessages[l] is TIFPSPascalCompilerError) then
    begin
      b := True;
//      FormMain.CurrSynEdit.SelStart := PSScript.CompilerMessages[l].Pos;

    end;
  end;
end;

procedure TMMLPSThread.Execute;
var
  time, i, ii: Integer;
begin;
  CurrThread := Self;
  time := lclintf.GetTickCount;
  try
    if PSScript.Compile then
    begin
      OutputMessages;
      Writeln('Compiled succesfully in ' + IntToStr(GetTickCount - time) + ' ms.');
//      if not (ScriptState = SCompiling) then
        if not PSScript.Execute then
        begin
//          FormMain.CurrSynEdit.SelStart := Script.PSScript.ExecErrorPosition;
          Writeln(PSScript.ExecErrorToString +' at '+Inttostr(PSScript.ExecErrorProcNo)+'.'
                  +Inttostr(PSScript.ExecErrorByteCodePosition));
        end else Writeln('Succesfully executed');
    end else
    begin
      OutputMessages;
      Writeln('Compiling failed');
    end;
  except
     on E : Exception do
       Writeln('Error: ' + E.Message);
  end;
end;

procedure TMMLPSThread.SetPSScript(Script: string);
begin
   PSScript.Script.Text:= Script;
end;

procedure TMMLPSThread.SetDebug(Strings: TSynMemo);
begin
  DebugTo := Strings;
end;


{ Include stuff here? }

//{$I inc/colors.inc}
//{$I inc/bitmaps.inc}


end.


