{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Jumps to the declaration of a identifier/expression.
  Automatically happens on link click, or can manually be called
}
unit simba.ide_editor_finddecl;

{$i simba.inc}

interface

uses
  Classes, SysUtils, SynEdit, SynEditMouseCmds,
  simba.base;

type
  TSimbaEditor_FindDecl = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    function DoSimbaHandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
  public
    procedure FindDeclAt(X, Y: Integer);
  end;

implementation

uses
  simba.ide_controller,
  simba.ide_codetools_parser,
  simba.ide_codetools_insight,
  simba.ide_editor,
  simba.ide_editor_codetools;

procedure TSimbaEditor_FindDecl.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  Value.RegisterMouseActionExecHandler(@DoSimbaHandleMouseAction);
end;

function TSimbaEditor_FindDecl.DoSimbaHandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;
  if (AnAction.Command = emcMouseLink) then
  begin
    FindDeclAt(Editor.CaretX, Editor.CaretY);
    Result := True;
  end;
end;

procedure TSimbaEditor_FindDecl.FindDeclAt(X, Y: Integer);
var
  Decl: TDeclaration;
  Decls: TDeclarationArray;
  Codeinsight: TCodeinsight;
  Expr: String;
begin
  Expr := TSimbaEditor(Editor).Codetools.GetExpressionAt(X,Y);
  Codeinsight := TSimbaEditor(Editor).Codetools.GetAndRunParser();
  try
    Decl := Codeinsight.ParseExpr(Expr);

    // need to need to check for overloads
    if (Decl is TDeclaration_Method) then
    begin
      if (Decl is TDeclaration_MethodOfType) then
        Decls := Codeinsight.Get(TDeclaration_MethodOfType(Decl).ObjectName).GetByClass(TDeclaration_Type)
      else
        Decls := [];

      if (Length(Decls) > 0) then
        Decls := Codeinsight.GetTypeMembers(Decls[0] as TDeclaration_Type).GetByClassAndName(Decl.Name, TDeclaration_MethodOfType)
      else
        Decls := Codeinsight.Get(Decl.Name).GetByClassAndName(Decl.Name, TDeclaration_Method, True);

      if (Length(Decls) > 1) then
        SimbaController.SelectAndShowDecl(Decls)
      else if (Length(Decls) = 1) then
        SimbaController.ShowDecl(Decls[0]);

      Exit;
    end;

    SimbaController.ShowDecl(Decl);
  finally
    Codeinsight.Free();
  end;
end;

end.

