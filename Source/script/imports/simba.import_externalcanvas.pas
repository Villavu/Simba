unit simba.import_externalcanvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportExternalCanvas(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.canvas_external, simba.script_objectutil;

type
  PSimbaExternalCanvas = ^TSimbaExternalCanvas;

procedure _LapeExternalCanvas_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := TSimbaExternalCanvas.Create();
end;

procedure _LapeExternalCanvas_SetMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.SetMemory(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeExternalCanvas_UserData_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaExternalCanvas(Params^[0])^.UserData
end;

procedure _LapeExternalCanvas_UserData_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.UserData := PPointer(Params^[1])^;
end;

procedure _LapeExternalCanvas_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawImage(PLapeObjectImage(Params^[1])^^, PPoint(Params^[2])^);
end;

procedure ImportExternalCanvas(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    // a TCanvas: every inherited method dispatches to TSimbaExternalCanvas's locked override
    addGlobalType('type TCanvas', 'TExternalCanvas');

    addGlobalFunc('function TExternalCanvas.Create: TExternalCanvas; static;', @_LapeExternalCanvas_Create);
    addGlobalFunc('procedure TExternalCanvas.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);', @_LapeExternalCanvas_SetMemory);
    addGlobalFunc('procedure TExternalCanvas.DrawImage(Image: TImage; Position: TPoint);', @_LapeExternalCanvas_DrawImage);

    addProperty('TExternalCanvas', 'UserData', 'Pointer', @_LapeExternalCanvas_UserData_Read, @_LapeExternalCanvas_UserData_Write);
  end;
end;

end.

