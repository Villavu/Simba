{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Handler for managing initialization and finalization
  This is used to not rely on unit import order with initialization/finalization sections
}
unit simba.initializations;

{$i simba.inc}
{.$define SIMBA_DEBUG_INITIALZATIONS}

interface

uses
  Classes, SysUtils, Forms,
  simba.base;

{$PUSH}
{$SCOPEDENUMS ON}
type
  ESimbaInit = (
    IDE_BEFORE_CREATE,
    IDE_BEFORE_SHOW,
    IDE_BEFORE_SHOW_BACKGROUND,
    IDE_DESTROY,
    CREATE,
    DESTROY
  );
  TSimbaInitName = String[64];
{$POP}

procedure SimbaInitialization_Add(Init: ESimbaInit; Proc: TProcedure; Name: TSimbaInitName; Priority: Integer = 0);
procedure SimbaInitialization_Call(Init: ESimbaInit);

implementation

uses
  TypInfo,
  simba.threading;

type
  TInitMethod = record
    Init: ESimbaInit;
    Proc: TProcedure;
    Name: TSimbaInitName;
    Priority: Integer;
  end;
  TInitMethods = array of TInitMethod;

var
  InitMethods: TInitMethods;

procedure SimbaInitialization_Add(Init: ESimbaInit; Proc: TProcedure; Name: TSimbaInitName; Priority: Integer = 0);
var
  Item: TInitMethod;
begin
  Item := Default(TInitMethod);
  Item.Init := Init;
  Item.Proc := Proc;
  Item.Name := Name;
  Item.Priority := Priority;

  InitMethods := InitMethods + [Item];
end;

procedure Call(Init: ESimbaInit);

  // no need for anything more advanced than this for such small array
  // also make a copy to be safe
  function GetMethods: TInitMethods;
  var
    I, J: Integer;
    Temp: TInitMethod;
  begin
    Result := Copy(InitMethods);

    for I := 0 to High(Result) do
      for J := 0 to High(Result) do
        if (Result[I].Priority > Result[J].Priority) then
        begin
          Temp := Result[I];
          Result[I] := Result[J];
          Result[J] := Temp;
        end;
  end;

var
  Prefix: String;
  Method: TInitMethod;
begin
  Prefix := '[' + GetEnumName(TypeInfo(ESimbaInit), Ord(Init)) + '] ';

  for Method in GetMethods() do
    if (Method.Init = Init) then
    try
      {$IFDEF SIMBA_DEBUG_INITIALZATIONS}
      DebugLn(Prefix + 'Calling ' + Method.Name);
      {$ENDIF}
      Method.Proc();
    except
      on E: Exception do
        DebugLn(Prefix + Method.Name + ' Exception: ' + E.Message);
    end;
end;

procedure CallBackground;
begin
  Call(ESimbaInit.IDE_BEFORE_SHOW_BACKGROUND);
end;

procedure SimbaInitialization_Call(Init: ESimbaInit);
begin
  // Spin up a thread for this
  if (Init = ESimbaInit.IDE_BEFORE_SHOW_BACKGROUND) then
  begin
    RunInThread(@CallBackground, True);
    Exit;
  end;

  Call(Init);
end;

type
  TApplicationHelper = type helper for TApplication
    procedure CallDestroy(Sender: TObject);
  end;

procedure TApplicationHelper.CallDestroy(Sender: TObject);
begin
  if (SimbaProcessType = ESimbaProcessType.IDE) then
    SimbaInitialization_Call(ESimbaInit.IDE_DESTROY);
end;

initialization
  Application.OnDestroy := @Application.CallDestroy;

finalization
  SimbaInitialization_Call(ESimbaInit.DESTROY);

end.

