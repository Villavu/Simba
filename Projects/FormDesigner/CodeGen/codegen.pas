unit CodeGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,SCList,AbstractCodeGen,CodeGenFactory;
type

  { TCodeGenerator }

  TCodeGenerator = class
    private
      FCodeGen: TAbstractCodeGen;
    public
      constructor Create(const CodeType: integer);
      destructor Destroy;override;
      function GetScript(const list: TSimbaComponentList): TStrings;
  end;

implementation

{ TCodeGenerator }

constructor TCodeGenerator.Create(const CodeType: integer);
begin
 FCodeGen:= TCodeGenFactory.GetCodeGen(TInterpreterType(CodeType));
end;

destructor TCodeGenerator.Destroy;
begin
  if (FCodeGen <> nil) then
   FCodeGen.Free;
  inherited Destroy;
end;

function TCodeGenerator.GetScript(const list: TSimbaComponentList): TStrings;
begin
  result:=FCodeGen.GetScript(list);
end;

end.

