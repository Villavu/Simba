{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Handler for managing initialization methods setup Simba's IDE.

  RegisterMethodOnBeforeCreate -> Called before form creation.
  RegisterMethodOnCreated      -> Called when forms have been created, just before show.
  RegisterMethodOnAfterCreate  -> Called **on another thread** after create.
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
    FBeforeCreateMethods: TMethods;
    FCreatedMethods: TMethods;
    FAfterCreateMethods: TMethods;
  public
    class procedure RegisterMethodOnBeforeCreate(Proc: TProcedure; Name: String);
    class procedure RegisterMethodOnCreated(Proc: TProcedure; Name: String);
    class procedure RegisterMethodOnAfterCreate(Proc: TProcedure; Name: String);

    class procedure CallOnBeforeCreateMethods;
    class procedure CallOnCreatedMethods;
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

      DebugLn('[%s]: %s (%f ms)'.Format([Name, Methods[I].Name, HighResolutionTime() - T]));
    except
      on E: Exception do
        DebugLn('[%s]: %s Exception: (%s)'.Format([Name, Methods[I].Name, E.Message]));
    end;
  end;
end;

class procedure SimbaIDEInitialization.RegisterMethodOnBeforeCreate(Proc: TProcedure; Name: String);
begin
  SetLength(FBeforeCreateMethods, Length(FBeforeCreateMethods) + 1);

  FBeforeCreateMethods[High(FBeforeCreateMethods)].Proc := Proc;
  FBeforeCreateMethods[High(FBeforeCreateMethods)].Name := Name;
end;

class procedure SimbaIDEInitialization.RegisterMethodOnCreated(Proc: TProcedure; Name: String);
begin
  SetLength(FCreatedMethods, Length(FCreatedMethods) + 1);

  FCreatedMethods[High(FCreatedMethods)].Proc := Proc;
  FCreatedMethods[High(FCreatedMethods)].Name := Name;
end;

class procedure SimbaIDEInitialization.RegisterMethodOnAfterCreate(Proc: TProcedure; Name: String);
begin
  SetLength(FAfterCreateMethods, Length(FAfterCreateMethods) + 1);

  FAfterCreateMethods[High(FAfterCreateMethods)].Proc := Proc;
  FAfterCreateMethods[High(FAfterCreateMethods)].Name := Name;
end;

class procedure SimbaIDEInitialization.CallOnBeforeCreateMethods;
begin
  Call(FBeforeCreateMethods, 'SimbaIDEInitialization.CallOnBeforeCreateMethods');
end;

class procedure SimbaIDEInitialization.CallOnCreatedMethods;
begin
  Call(FCreatedMethods, 'SimbaIDEInitialization.CallOnCreatedMethods');
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

