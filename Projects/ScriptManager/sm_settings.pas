unit sm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type TOption = record
    XMLSrvDesc: string;//host for server side description file
    XMLStorage: string;//local path for local script storage
    SrvFileNAme: string;
    Autoupdate: boolean;//autoupdate -> yes\no
    Simba_Scripts: string;//path to simba scripts folder
    Simba_SRL: string;//path to simba srl folder
    Simba_SPS:string;//path to Simba sps folder
    Simba_Fonts: string;//path to Simba fonts folder
    Simba_Plugins: string;//path to Simba plugins folder
    Simba: string;//path to Simba folder
    Simba_include: string;//path to Simba include folder
  end;
procedure SetOptionsPaths(Server,Storage,SimbaPath,SrvFile: string;var opt: TOption);
implementation

procedure SetOptionsPaths(Server, Storage, SimbaPath, SrvFile: string; var opt: TOption);
begin
   with opt do
   begin
     XMLStorage:= storage;
     XMLSrvDesc:=Server;
     Simba:=SimbaPath;
     Simba_include:=Simba+'Includes/';
     Simba_SPS:=Simba_include+'SPS/';
     Simba_Scripts:=Simba+'Scripts/';
     Simba_Plugins:=Simba+'Plugins/';
     Simba_Fonts:=Simba+'Fonts/';
     Simba_SRL:=Simba_include+'SRL/';
     SrvFileName:=SrvFile;
   end;
end;

end.

