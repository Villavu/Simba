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
    OnDestroyed: TNotifyEvent;

    Plugin: String;
    Output: String;

    destructor Destroy; override;
  end;

  TSimbaScript_CompilerDumper = class(TThread)
  protected
    procedure Execute; override;
  public
    OnDestroyed: TNotifyEvent;

    Output: String;

    destructor Destroy; override;
  end;

implementation

uses
  simbascript.plugin, simbascript.compilerdump;

procedure TSimbaScript_PluginDumper.Execute;
begin
  with TSimbaScriptPlugin.Create(Plugin) do // Don't free plugin, it's not worth it. Let the OS clean up...
    Dump.SaveToFile(Output);
end;

destructor TSimbaScript_PluginDumper.Destroy;
begin
  inherited Destroy();

  if (OnDestroyed <> nil) then
    OnDestroyed(Self);
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

destructor TSimbaScript_CompilerDumper.Destroy;
begin
  inherited Destroy();

  if (OnDestroyed <> nil) then
    OnDestroyed(Self);
end;

end.

