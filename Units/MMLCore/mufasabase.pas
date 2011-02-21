unit mufasabase;

{$mode objfpc}{$H+}

interface
{$undef mDebug}

uses
  Classes, SysUtils{$ifdef MSWindows},windows{$endif};

procedure mDebugLn( s : string);overload;
procedure mDebugLn( s : string; f : array of const);overload;
procedure InitmDebug;
procedure FreemDebug;
implementation

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
end;

procedure FreemDebug;
begin
  CanDebug := false;
end;

end.

