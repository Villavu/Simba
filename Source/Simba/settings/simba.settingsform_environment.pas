unit simba.settingsform_environment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, StdCtrls,
  DividerBevel;

type
  TEnvironmentFrame = class(TFrame)
    OpenSSLOnLaunch: TCheckBox;
    DividerBevel1: TDividerBevel;
  private

  public

  end;

implementation

initialization
  {$I simba.settingsform_environment.lrs}

end.

