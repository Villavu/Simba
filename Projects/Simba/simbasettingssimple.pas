unit SimbaSettingsSimple;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  settings { We need this for the Simba Settings API }
  ;

type
  TSettingsSimpleForm = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SettingsSimpleForm: TSettingsSimpleForm;

implementation

initialization
  {$I simbasettingssimple.lrs}

end.

