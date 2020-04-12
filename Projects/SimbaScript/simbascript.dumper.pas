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
  streamio,
  simbascript.plugin, simbascript.compilerdump;

procedure TSimbaScript_PluginDumper.Execute;
var
  Plugin: TSimbaScriptPlugin;
  OrginalOutput: TextFile;
  Stream: TStringStream;
begin
  OrginalOutput := Output;
  Stream := TStringStream.Create('');

  AssignStream(Output, Stream);
  Rewrite(Output);

  try
    Plugin := TSimbaScriptPlugin.Create(FFileName);

    WriteLn(OrginalOutput, Plugin.Dump.Text);
  except
    on E: Exception do
    begin
      WriteLn(OrginalOutput, Stream.DataString);

      raise;
    end;
  end;
end;

procedure TSimbaScript_CompilerDumper.Execute;
var
  Compiler: TSimbaScript_CompilerDump;
begin
  Compiler := TSimbaScript_CompilerDump.Create();

  try
    WriteLn(Compiler.Dump.Text);
  finally
    if (Compiler <> nil) then
      Compiler.Free();
  end;
end;

end.

