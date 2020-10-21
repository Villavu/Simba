unit simba.settingsform_environment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, StdCtrls,
  DividerBevel;

type
  TEnvironmentFrame = class(TFrame)
    IncludePathOpenButton: TButton;
    PluginPathOpenButton: TButton;
    FontPathOpenButton: TButton;
    ScriptPathOpenButton: TButton;
    OpenSSLOnLaunch: TCheckBox;
    DividerBevel1: TDividerBevel;
    IncludePathEdit: TEdit;
    PluginPathEdit: TEdit;
    FontPathEdit: TEdit;
    ScriptPathEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure OpenButtonClick(Sender: TObject);
  private

  public

  end;

implementation

uses
  dialogs;

procedure TEnvironmentFrame.OpenButtonClick(Sender: TObject);
var
  Edit: TEdit;
  Directory: String;
begin
  Edit := nil;

  if (Sender = IncludePathOpenButton) then Edit := IncludePathEdit;
  if (Sender = PluginPathOpenButton) then Edit := PluginPathEdit;
  if (Sender = FontPathOpenButton) then Edit := FontPathEdit;
  if (Sender = ScriptPathOpenButton) then Edit := ScriptPathEdit;

  if (Edit <> nil) then
  begin
    Directory := ExtractFileDir(Edit.Text);
    if (not DirectoryExists(Directory)) then
      Directory := Application.Location;

    if SelectDirectory('Select Directory', Directory, Directory) then
      Edit.Text := Directory;

    Exit;
  end;
end;

initialization
  {$I simba.settingsform_environment.lrs}

end.

