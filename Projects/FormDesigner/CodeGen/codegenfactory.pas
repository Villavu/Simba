unit CodeGenFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,AbstractCodeGen,PSCodeGen,LPCodeGen;
type
  TInterpreterType = (itPS = 0, itLape = 1);

  { TCodeGenFactory }

  TCodeGenFactory = class
    class function GetCodeGen(const CodeGenType: TInterpreterType): TAbstractCodeGen;
  end;

implementation

{ TCodeGenFactory }

class function TCodeGenFactory.GetCodeGen(const CodeGenType: TInterpreterType
  ): TAbstractCodeGen;
begin
  case CodeGenType of
   itPS: Result:=TPSCodeGen.Create;
   itLape: Result:=TLPCodeGen.Create;
  end;
end;

end.

