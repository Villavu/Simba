unit simbascript.dumper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSimbaScript_PluginDumper = class(TThread)
  protected
    procedure Execute; override;
  public
    Plugin: String;
    Output: String;
  end;

  TSimbaScript_CompilerDumper = class(TThread)
  protected
    procedure Execute; override;
  public
    Output: String;
  end;

implementation

uses
  simbascript.plugin, simbascript.compilerdump;

procedure TSimbaScript_PluginDumper.Execute;
begin
  with TSimbaScriptPlugin.Create(Plugin) do // Don't free plugin, it's not worth it. Let the OS clean up...
    Dump.SaveToFile(Output);
end;

procedure TSimbaScript_CompilerDumper.Execute;
var
  Compiler: TSimbaScript_CompilerDump;
begin
  Compiler := TSimbaScript_CompilerDump.Create();

  try
    Compiler.Dump.SaveToFile(Output);
  finally
    if (Compiler <> nil) then
      Compiler.Free();
  end;
end;

end.

