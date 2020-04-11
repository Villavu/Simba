unit simbascript.dumper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSimbaScript_PluginDumper = class(TThread)
  protected
    FFileName: String;

    procedure Execute; override;
  public
    property FileName: String read FFileName write FFileName;
  end;

  TSimbaScript_CompilerDumper = class(TThread)
  protected
    procedure Execute; override;
  end;

implementation

uses
  simbascript.plugin, simbascript.compilerdump;

procedure TSimbaScript_PluginDumper.Execute;
var
  Plugin: TSimbaScriptPlugin;
begin
  Plugin := nil;

  try
    Plugin := TSimbaScriptPlugin.Create(FFileName);

    WriteLn(#0 + Plugin.Dump.Text + #0);
  finally
    if (Plugin <> nil) then
      Plugin.Free();
  end;
end;

procedure TSimbaScript_CompilerDumper.Execute;
var
  Compiler: TSimbaScript_CompilerDump;
begin
  Compiler := TSimbaScript_CompilerDump.Create();

  try
    Writeln(#0 + Compiler.Dump.Text + #0);
  finally
    if (Compiler <> nil) then
      Compiler.Free();
  end;
end;

end.

