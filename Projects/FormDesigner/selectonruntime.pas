{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SelectOnRuntime; 

interface

uses
  cSelectOnRunTime, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('cSelectOnRunTime', @cSelectOnRunTime.Register); 
end; 

initialization
  RegisterPackage('SelectOnRuntime', @Register); 
end.
