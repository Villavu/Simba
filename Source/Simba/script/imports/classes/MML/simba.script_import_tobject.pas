unit simba.script_import_tobject;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_TObject(Compiler: TSimbaScript_Compiler);

implementation

type
  PObject = ^TObject;

procedure TObject_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Params^[0])^ := TObject.Create();
end;

procedure TObject_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Params^[0])^.Free();
end;

procedure TObject_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PObject(Params^[0])^.ToString();
end;

procedure Lape_Import_TObject(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'Classes';

    addClass('TObject', 'Pointer');

    addGlobalFunc('procedure TObject.Init();', @TObject_Init);
    addGlobalFunc('procedure TObject.Free(); constref;', @TObject_Free);
    addGlobalFunc('function TObject.ToString(): string; constref;', @TObject_ToString);
  end;
end;

end.

