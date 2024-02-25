{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler_imagefromstring;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lpcompiler;

procedure InitializeImageFromString(Compiler: TLapeCompiler);

implementation

uses
  lptypes, lptree, lpvartypes, lpmessages,
  simba.image;

type
  TLapeTree_InternalMethod_ImageFromString = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos=nil); override;
  end;

procedure InitializeImageFromString(Compiler: TLapeCompiler);
begin
  Compiler.InternalMethodMap['ImageFromString'] := TLapeTree_InternalMethod_ImageFromString;
end;

function TLapeTree_InternalMethod_ImageFromString.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getGlobalType('TImage');
  Result := inherited;
end;

function TLapeTree_InternalMethod_ImageFromString.Evaluate: TLapeGlobalVar;
var
  Param: TLapeGlobalVar;
begin
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  Param := FParams[0].Evaluate;
  if (Param.BaseType <> ltAnsiString) then
    LapeExceptionFmt(lpeExpected, ['String parameter'], DocPos);

  Result := resType().NewGlobalVarP();

  PSimbaImage(Result.Ptr)^ := TSimbaImage.CreateFromString(PAnsiString(Param.Ptr)^);
  PSimbaImage(Result.Ptr)^.FreeOnTerminate := True;
end;

constructor TLapeTree_InternalMethod_ImageFromString.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FConstant := bTrue;
end;

end.

