{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler_onterminate;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lpcompiler;

procedure InitializeAddOnTerminate(Compiler: TLapeCompiler);
procedure CallOnTerminateMethods(Compiler: TLapeCompiler);

implementation

uses
  lptypes, lptree, lpvartypes, lpinterpreter;

procedure AddCallOnTerminateMethods(Compiler: TLapeCompiler);
var
  Decl: TLapeDeclaration;
begin
  with TStringList.Create() do
  try
    Add('procedure _CallOnTerminateMethods; override;');
    Add('var i: Int32;');
    Add('begin');
    Add('  for i := 0 to High(_OnTerminateMethods) do');
    Add('    with _OnTerminateMethods[i] do');
    Add('    begin');
    Add('      if (@Proc <> nil) then');
    Add('        Proc();');
    Add('      if (@ProcObj <> nil) then');
    Add('        ProcObj();');
    Add('      if (ProcName <> "") then');
    Add('        case UpperCase(ProcName) of');
    for Decl in Compiler.GlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue) do
    begin
      if (TLapeGlobalVar(Decl).VarType.ClassType = TLapeType_Method) then
        with TLapeType_Method(TLapeGlobalVar(Decl).VarType) do
        begin
          if (Params.Count > 0) or (Res <> nil) then
            Continue;

          Add('          "' + UpperCase(Name) + '": ' + Name + '();');
        end;
    end;
    Add('          else raise "Only a procedure with no parameters can be passed to AddOnTerminate";');
    Add('        end;');
    Add('    end;');
    Add('end;');

    Compiler.addDelayedCode(Text, '!_CallTerminateMethods');
  finally
    Free();
  end;
end;

procedure InitializeAddOnTerminate(Compiler: TLapeCompiler);
begin
  Compiler.AddDelayedCode(
    'const IsTerminated: Boolean = False;                                     ' + LineEnding +
    'const IsTerminatedByUser: Boolean = False;                               '
  );

  Compiler.AfterParsing.AddProc(@AddCallOnTerminateMethods);
  Compiler.addDelayedCode(
    'var _OnTerminateMethods: array of record                                 ' + LineEnding +
    '      ProcObj: procedure of object;                                      ' + LineEnding +
    '      Proc: procedure;                                                   ' + LineEnding +
    '      ProcName: String;                                                  ' + LineEnding +
    '    end;                                                                 ' + LineEnding +
    '                                                                         ' + LineEnding +
    'procedure _CallOnTerminateMethods;                                       ' + LineEnding +
    'begin                                                                    ' + LineEnding +
    'end;',
    '!AddOnTerminate'
  );

  Compiler.addDelayedCode(
    'procedure AddOnTerminate(Proc: procedure); overload;                     ' + LineEnding +
    'begin                                                                    ' + LineEnding +
    '  SetLength(_OnTerminateMethods, Length(_OnTerminateMethods) + 1);       ' + LineEnding +
    '  _OnTerminateMethods[High(_OnTerminateMethods)].Proc := @Proc;          ' + LineEnding +
    'end;                                                                     ' + LineEnding +
    '                                                                         ' + LineEnding +
    'procedure AddOnTerminate(Proc: procedure of object); overload;           ' + LineEnding +
    'begin                                                                    ' + LineEnding +
    '  SetLength(_OnTerminateMethods, Length(_OnTerminateMethods) + 1);       ' + LineEnding +
    '  _OnTerminateMethods[High(_OnTerminateMethods)].ProcObj := @Proc;       ' + LineEnding +
    'end;                                                                     ' + LineEnding +
    '                                                                         ' + LineEnding +
    'procedure AddOnTerminate(Proc: String); overload;                        ' + LineEnding +
    'begin                                                                    ' + LineEnding +
    '  SetLength(_OnTerminateMethods, Length(_OnTerminateMethods) + 1);       ' + LineEnding +
    '  _OnTerminateMethods[High(_OnTerminateMethods)].ProcName := Proc;       ' + LineEnding +
    'end;                                                                     '
  );
end;

procedure CallOnTerminateMethods(Compiler: TLapeCompiler);
var
  Method: TLapeGlobalVar;
begin
  if (Compiler['IsTerminated'] <> nil) then
    PBoolean(Compiler['IsTerminated'].Ptr)^ := True;

  Method := Compiler['_CallOnTerminateMethods'];
  if (Method <> nil) then
    RunCode(Compiler.Emitter.Code, Compiler.Emitter.CodeLen, [], PCodePos(Method.Ptr)^);
end;

end.

