{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.associate;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms;

procedure Associate;

implementation

{$IFDEF WINDOWS}
uses
  FileAssoc;

procedure Associate;
var
  assoc: TFileAssociation;
begin
  assoc := TFileAssociation.Create(nil);
  assoc.ApplicationName := 'Simba';

  // Requires admin
  assoc.RegisterForAllUsers := False;

  assoc.Extension := '.simba';
  assoc.ExtensionName := 'Simba';
  assoc.ExtensionIcon := '"' + Application.ExeName + '",0';

  assoc.WriteFileAssociationClass();
  assoc.WriteFileAssociation();
  assoc.WriteDefaultPrograms();
  assoc.WriteDefaultProgramsAddExt();

  assoc.ActionName := 'Open';
  assoc.ActionText := 'Open';
  assoc.ActionIcon := '"' + Application.ExeName + '",0';
  assoc.Action := '"' + Application.ExeName + '" --open "%1"';

  assoc.WriteFileAssociationClassCommand();

  assoc.ActionName := 'Open and Run';
  assoc.ActionText := 'Open and Run';
  assoc.ActionIcon := '"' + Application.ExeName + '",0';
  assoc.Action := '"' + Application.ExeName + '" --open --run "%1"';

  assoc.WriteFileAssociationClassCommand();

  assoc.ActionName := 'Run';
  assoc.ActionText := 'Run';
  assoc.ActionIcon := '"' + Application.ExeName + '",0';
  assoc.Action := '"' + Application.ExeName + '" --run "%1"';

  assoc.WriteFileAssociationClassCommand();

  assoc.ClearIconCache();
end;
{$ELSE}
procedure Associate;
begin
end;
{$ENDIF}

end.

