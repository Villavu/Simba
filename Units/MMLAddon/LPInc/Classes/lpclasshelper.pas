unit lpClassHelper;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpvartypes;

type
  TLapeCompilerHelper = class helper for TLapeCompiler
  public
    procedure addClass(const Name: string; const Parent: string = 'TObject');
    procedure addClassVar(const Obj, Item, Typ: string; const Read: Pointer; const Write: Pointer = nil);
  end;

implementation

procedure TLapeCompilerHelper.addClass(const Name: string; const Parent: string = 'TObject');
begin
  addGlobalType(Format('type %s', [Parent]), Name);
end;

procedure TLapeCompilerHelper.addClassVar(const Obj, Item, Typ: string; const Read: Pointer; const Write: Pointer = nil);
begin
  if (Assigned(Read)) then
    addGlobalFunc(Format('function %s.get%s(): %s;', [Obj, Item, Typ]), Read);

  if (Assigned(Write)) then
    addGlobalFunc(Format('procedure %s.set%s(const value: %s);', [Obj, Item, Typ]), Write);
end;

end.

