{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Handler for managing initialization methods setup Simba's IDE.
}
unit simba.ide_initialization;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TProcedureOfObject = procedure of object;

procedure SimbaIDEInitialization_AddBeforeCreate(Proc: TProcedureOfObject; Name: String); overload;
procedure SimbaIDEInitialization_AddBeforeCreate(Proc: TProcedure; Name: String); overload;

procedure SimbaIDEInitialization_AddBeforeShow(Proc: TProcedureOfObject; Name: String; BackgroundTask: Boolean = False); overload;
procedure SimbaIDEInitialization_AddBeforeShow(Proc: TProcedure; Name: String; BackgroundTask: Boolean = False); overload;

procedure SimbaIDEInitialization_CallBeforeCreate;
procedure SimbaIDEInitialization_CallBeforeShow;

implementation

uses
  simba.threading;

type
  TInitializationMethod = record
    Name: String;
    Proc: TProcedure;
    ProcObj: TProcedureOfObject;
    BackgroundTask: Boolean;
  end;

var
  BeforeCreateMethods: array of TInitializationMethod;
  BeforeShowMethods: array of TInitializationMethod;

procedure SimbaIDEInitialization_AddBeforeCreate(Proc: TProcedureOfObject; Name: String);
var
  Item: TInitializationMethod;
begin
  Item := Default(TInitializationMethod);
  Item.ProcObj := Proc;
  Item.Name := Name;

  BeforeCreateMethods := BeforeCreateMethods + [Item];
end;

procedure SimbaIDEInitialization_AddBeforeCreate(Proc: TProcedure; Name: String);
var
  Item: TInitializationMethod;
begin
  Item := Default(TInitializationMethod);
  Item.Proc := Proc;
  Item.Name := Name;

  BeforeCreateMethods := BeforeCreateMethods + [Item];
end;

procedure SimbaIDEInitialization_AddBeforeShow(Proc: TProcedureOfObject; Name: String; BackgroundTask: Boolean);
var
  Item: TInitializationMethod;
begin
  Item := Default(TInitializationMethod);
  Item.ProcObj := Proc;
  Item.Name := Name;
  Item.BackgroundTask := BackgroundTask;

  BeforeShowMethods := BeforeShowMethods + [Item];
end;

procedure SimbaIDEInitialization_AddBeforeShow(Proc: TProcedure; Name: String; BackgroundTask: Boolean);
var
  Item: TInitializationMethod;
begin
  Item := Default(TInitializationMethod);
  Item.Proc := Proc;
  Item.Name := Name;
  Item.BackgroundTask := BackgroundTask;

  BeforeShowMethods := BeforeShowMethods + [Item];
end;

procedure SimbaIDEInitialization_CallBeforeCreate;
var
  Method: TInitializationMethod;
begin
  for Method in BeforeCreateMethods do
  try
    if Assigned(Method.Proc)    then Method.Proc() else
    if Assigned(Method.ProcObj) then Method.ProcObj();
  except
    on E: Exception do
      DebugLn('[SimbaIDEInitialization]: %s (exception: %s)', [Method.Name, E.Message]);
  end;
end;

procedure SimbaIDEInitialization_CallBeforeShow_Background;
var
  Method: TInitializationMethod;
begin
  for Method in BeforeShowMethods do
    if Method.BackgroundTask then
    try
      if Assigned(Method.Proc)    then Method.Proc() else
      if Assigned(Method.ProcObj) then Method.ProcObj();
    except
      on E: Exception do
        DebugLn('[SimbaIDEInitialization]: %s (exception: %s)', [Method.Name, E.Message]);
    end;
end;

procedure SimbaIDEInitialization_CallBeforeShow;
var
  Method: TInitializationMethod;
begin
  for Method in BeforeShowMethods do
    if not Method.BackgroundTask then
    try
      if Assigned(Method.Proc)    then Method.Proc() else
      if Assigned(Method.ProcObj) then Method.ProcObj();
    except
      on E: Exception do
        DebugLn('[SimbaIDEInitialization]: %s (exception: %s)', [Method.Name, E.Message]);
    end;

  RunInThread(@SimbaIDEInitialization_CallBeforeShow_Background, True);
end;

end.

