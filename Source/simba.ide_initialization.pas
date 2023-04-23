{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Handler for managing initialization methods setup Simba's IDE.

  RegisterMethodOnCreate -> Called before any forms are created.
  RegisterMethodOnAfterCreate -> Called **on another thread** after all forms are created just before show.
}
unit simba.ide_initialization;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.datetime;

type
  SimbaIDEInitialization = class
  protected
  type
    TMethods = array of record
      Proc: TProcedure;
      Name: String;
    end;
    class procedure Call(Methods: TMethods; Name: String);
  class var
    FCreateMethods: TMethods;
    FAfterCreateMethods: TMethods;
  public
    class procedure RegisterMethodOnCreate(Proc: TProcedure; Name: String);
    class procedure RegisterMethodOnAfterCreate(Proc: TProcedure; Name: String);

    class procedure CallOnCreateMethods;
    class procedure CallOnAfterCreateMethods;
  end;

implementation

uses
  simba.threading;

class procedure SimbaIDEInitialization.Call(Methods: TMethods; Name: String);
var
  I: Integer;
  T: Double;
begin
  for I := 0 to High(Methods) do
  begin
    T := HighResolutionTime();

    DebugLn('[%s]: %s'.Format([Name, Methods[I].Name]));
    try
      Methods[I].Proc();

      DebugLn('[%s]: %s finished in %f ms'.Format([Name, Methods[I].Name, HighResolutionTime() - T]));
    except
      on E: Exception do
        DebugLn('[%s]: %s failed (%s) in %f ms'.Format([Name, Methods[I].Name, E.Message, HighResolutionTime() - T]));
    end;
  end;
end;

class procedure SimbaIDEInitialization.RegisterMethodOnCreate(Proc: TProcedure; Name: String);
begin
  SetLength(FCreateMethods, Length(FCreateMethods) + 1);

  FCreateMethods[High(FCreateMethods)].Proc := Proc;
  FCreateMethods[High(FCreateMethods)].Name := Name;
end;

class procedure SimbaIDEInitialization.RegisterMethodOnAfterCreate(Proc: TProcedure; Name: String);
begin
  SetLength(FAfterCreateMethods, Length(FAfterCreateMethods) + 1);

  FAfterCreateMethods[High(FAfterCreateMethods)].Proc := Proc;
  FAfterCreateMethods[High(FAfterCreateMethods)].Name := Name;
end;

class procedure SimbaIDEInitialization.CallOnCreateMethods;
begin
  Call(FCreateMethods, 'SimbaIDEInitialization.CallOnCreateMethods');
end;

class procedure SimbaIDEInitialization.CallOnAfterCreateMethods;

  procedure Execute;
  begin
    Call(FAfterCreateMethods, 'SimbaIDEInitialization.CallOnAfterCreateMethods');
  end;

begin
  ThreadedAndForget(@Execute);
end;

end.

