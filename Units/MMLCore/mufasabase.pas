unit mufasabase;

{$mode objfpc}

interface

uses
  files, Classes, SysUtils{$ifdef MSWindows},windows{$endif};

procedure mDebugLn( s : string);overload;
procedure mDebugLn( s : string; f : array of const);overload;
procedure InitmDebug;
procedure FreemDebug;
implementation

uses
  TestUnit;
var
  CanDebug : boolean = false;

procedure mDebugLn(s: string);
begin
  if CanDebug then
    Writeln(s);
end;

procedure mDebugLn(s: string; f: array of const); overload;
begin
  mDebugLn(format(s,f));
end;

procedure InitmDebug;
begin
  CanDebug := true;
  {$ifdef MSWindows}
  IsConsole:= True;
  SysInitStdIO;
  {$endif}
end;

procedure FreemDebug;
begin
  CanDebug := false;
  {$ifdef MSWindows}
  IsConsole := false;
  {$endif}
end;

end.

