unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uPSComponent, uPSCompiler, uPSRuntime,UPSUtils,testps;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure ScriptCompile(Sender: TPSScript);
    procedure ScriptCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure ScriptExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation


uses
  uPSR_std,
  uPSC_std,
  uPSR_stdctrls,
  uPSC_stdctrls,
  uPSR_forms,
  uPSC_forms,
  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes;
{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Script : TPSScript;
  procedure OutputMessages;
  var
    l: Longint;
    b: Boolean;
  begin
    b := False;

    for l := 0 to Script.CompilerMessageCount - 1 do
    begin
      Memo2.Lines.Add('Compiler: '+ Script.CompilerErrorToStr(l));
      if (not b) and (Script.CompilerMessages[l] is TIFPSPascalCompilerError) then
      begin
        b := True;
        Memo1.SelStart := Script.CompilerMessages[l].Pos;
      end;
    end;
  end;
begin
  Script := TPSScript.Create(self);
  Script.OnCompImport:=@ScriptCompImport;
  Script.OnExecImport:=@ScriptExecImport;
  Script.OnCompile:=@ScriptCompile;
  Memo2.Lines.Clear;
  Script.Script.Assign(Memo1.Lines);
  Memo2.Lines.Add('Compiling');
  if Script.Compile then
  begin
    OutputMessages;
    Memo2.Lines.Add('Compiled succesfully');
    if not Script.Execute then
    begin
      Memo1.SelStart := Script.ExecErrorPosition;
      Memo2.Lines.Add(Script.ExecErrorToString +' at '+Inttostr(Script.ExecErrorProcNo)+'.'+Inttostr(Script.ExecErrorByteCodePosition));
    end else Memo2.Lines.Add('Succesfully executed');
  end else
  begin
    OutputMessages;
    Memo2.Lines.Add('Compiling failed');
  end;
  Script.free;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;



function teststdcall(num1,num2,num3,num4,num5,num6 : LongInt) : boolean;  stdcall;
begin;
  Result := false;
  Form1.Memo2.Lines.add(Format('stdcall AddFunctionEx: %d %d %d %d %d %d',[num1,num2,num3,num4,num5,num6]));
end;

function testcdecl(num1,num2,num3,num4,num5,num6 : LongInt) : boolean; cdecl;
begin;
  Result := false;
  Form1.Memo2.Lines.add(Format('cdecl AddFunctionEx: %d %d %d %d %d %d',[num1,num2,num3,num4,num5,num6]));
end;


function testnormal(num1,num2,num3,num4,num5,num6 : LongInt) : boolean;
begin;
  Result := false;
  Form1.Memo2.Lines.add(Format('Normal AddFunction: %d %d %d %d %d %d',[num1,num2,num3,num4,num5,num6]));
end;

function DiffTest(num : integer; str : string; byt : byte; wor : longword; bool : boolean) : boolean;
begin;
  Result := false;
  Form1.Memo2.Lines.add(inttostr(num) +  '-' +  str +'-' +  inttostr(byt) +'-' +  inttostr(wor) +'-' +  booltostr(bool,true));
end;


procedure TForm1.ScriptCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x, true);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
end;

procedure TForm1.ScriptExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
end;

procedure Writeln(s : string);
begin;
  Form1.Memo2.Lines.add(s);
end;

procedure TForm1.ScriptCompile(Sender: TPSScript);
begin
  Sender.Comp.AddTypeS('TStringArray','Array of string');
  Sender.Comp.AddTypeS('w_TPoint', 'record x, y: integer; end;');
  Sender.AddFunction(@Writeln,'procedure writeln(s:string)');
  Sender.AddFunction(@testnormal,'function testnormal(num1,num2,num3,num4,num5,num6 : LongInt) : boolean;');
  Sender.AddFunctionEx(@teststdcall, 'function teststdcall(num1,num2,num3,num4,num5,num6 : LongInt) : boolean;stdcall;',cdStdCall);
  Sender.AddFunctionEx(@testcdecl, 'function testcdecl(num1,num2,num3,num4,num5,num6 : LongInt) : boolean; cdecl;',cdCdecl);
  Sender.AddFunction(@DiffTest,'function DiffTest(num : integer; str : string; byt : byte; wor : longword; bool : boolean) : boolean;');
  Sender.AddFunction(@TestParameters,'procedure TestParameters(Int1,Int2,Int3,Int4,Int5,Int6 : integer);');
  Sender.AddFunction(@TestResult,'function TestResult(Int1,Int2,Int3,Int4,Int5,Int6 : integer): Integer;');
  Sender.AddFunction(@TestString,'function TestString(Str1,Str2,Str3 : string) : string;');
  Sender.AddFunction(@TestStringEdit,'function TestStringEdit(var Str : string) : String;');
  Sender.AddFunction(@TestArrayPassing,'procedure TestArrayPassing(const Arr : TStringArray);');
  Sender.AddFunction(@TestArrayEdit,'Procedure TestArrayEdit(var Arr : TStringArray);');
  Sender.AddFunction(@TestArrayFull,'function TestArrayFull(var Arr1: TStringArray; Arr2 : TStringArray): TStringArray;');
  Sender.AddFunction(@MakeArr,'function MakeArr : TStringArray;');
  Sender.AddFunction(@makePoint, 'function makePoint(x, y: integer): w_Tpoint;');
end;


initialization
  {$I unit1.lrs}

end.

