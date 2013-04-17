unit lpClassHelper;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpvartypes;

procedure addClass(const Compiler: TLapeCompiler; const Name: string; const Parent: string = 'TObject');
procedure addClassVar(const Compiler: TLapeCompiler; const Obj, Item, Typ: string; const Read: Pointer; const Write: Pointer = nil);

implementation

procedure addClass(const Compiler: TLapeCompiler; const Name: string; const Parent: string = 'TObject');
var
  ParentType, ClassType: TLapeType;
begin
  {$IFDEF SIMBA_VERBOSE}
  WriteLn(Format('Creating type "%s" from "%s".', [Name, Parent]));
  {$ENDIF}

  ParentType := Compiler.getGlobalType(Parent);
  if (not Assigned(ParentType)) then
    ParentType := Compiler.getBaseType(Parent);
    if (not Assigned(ParentType)) then
      raise Exception.CreateFmt('Invalid Type "%s".', [Parent]);

  ClassType := ParentType.CreateCopy(True);
  if (not Assigned(ClassType)) then
    raise Exception.CreateFmt('Problem creating copy of type "%s".', [Parent]);

  Compiler.addGlobalType(ClassType, Name);
end;

procedure addClassVar(const Compiler: TLapeCompiler; const Obj, Item, Typ: string; const Read: Pointer; const Write: Pointer = nil);
begin
  with Compiler do
  begin
    if (Assigned(Read)) then
      addGlobalFunc(format('function %s.get%s(): %s;', [Obj, Item, Typ]), Read);

    if (Assigned(Write)) then
      addGlobalFunc(format('procedure %s.set%s(const value: %s);', [Obj, Item, Typ]), Write);
  end;
end;

end.

