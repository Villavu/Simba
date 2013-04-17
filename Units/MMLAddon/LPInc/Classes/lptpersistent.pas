unit lpTPersistent;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TPersistent(Compiler: TLapeCompiler);

implementation

type
  PPersistent = ^TPersistent;
  PFiler = ^TFiler;

procedure TPersistent_Init(const Params: PParamArray);
begin
  PPersistent(Params^[0])^ := TPersistent.Create();
end;

procedure TPersistent_Free(const Params: PParamArray);
begin
  PPersistent(Params^[0])^.Free();
end;

procedure TPersistent_AssignTo(const Params: PParamArray);
begin
  PPersistent(Params^[0])^.AssignTo(PPersistent(Params^[1])^);
end;

procedure TPersistent_DefineProperties(const Params: PParamArray);
begin
  PPersistent(Params^[0])^.DefineProperties(PFiler(Params^[1])^);
end;

procedure TPersistent_GetOwner(const Params: PParamArray; const Result: Pointer);
begin
  PPersistent(Result)^ := PPersistent(Params^[0])^.GetOwner();
end;

procedure TPersistent_Assign(const Params: PParamArray);
begin
  PPersistent(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure TPersistent_GetNamePath(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PPersistent(Params^[0])^.GetNamePath();
end;

procedure Register_TPersistent(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TPersistent');

    addGlobalFunc('procedure TPersistent.Init();', @TPersistent_Init);
    addGlobalFunc('procedure TPersistent.Free();', @TPersistent_Free);
	
	addGlobalFunc('procedure TPersistent.AssignTo(Dest: TPersistent);', @TPersistent_AssignTo);
	//addGlobalFunc('procedure TPersistent.DefineProperties(Filer: TFiler);', @TPersistent_DefineProperties);
	addGlobalFunc('function TPersistent.GetOwner(): TPersistent;', @TPersistent_GetOwner);
	addGlobalFunc('procedure TPersistent.Assign(Source: TPersistent);' @TPersistent_Assign);
	addGlobalFunc('function TPersistent.GetNamePath(): string;', @TPersistent_GetNamePath);
  end;
end;

end.

