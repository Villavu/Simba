{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_about;

{$i simba.inc}
{$WARN 6018 off : unreachable code}

interface

uses
  classes, sysutils, fileutil, forms, controls, graphics, dialogs,
  stdctrls, extctrls;

type
  TSimbaAboutForm = class(TForm)
  protected
    procedure DoFirstShow; override;
  published
    AboutMemo: TMemo;
    ButtonExit: TButton;
    ImageSimba: TImage;
    VersionLabel: TLabel;
    LabelTitle: TLabel;
    procedure ButtonExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VersionLabelClick(Sender: TObject);
    procedure VersionLabelMouseEnter(Sender: TObject);
    procedure VersionLabelMouseLeave(Sender: TObject);
  end; 

var
  SimbaAboutForm: TSimbaAboutForm;

implementation

{$R *.lfm}

uses
  lclintf, lazversion,
  simba.fs;

procedure TSimbaAboutForm.DoFirstShow;
begin
  LabelTitle.Caption := Format('Simba %d', [SIMBA_VERSION]);
  if (SIMBA_COMMIT <> '') then
    VersionLabel.Caption := Format('(%s)', [SIMBA_COMMIT]);

  AboutMemo.Lines.Clear();
  AboutMemo.Lines.Add('Simba is released under the GPL license.');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('You are currently using version: %d', [SIMBA_VERSION]);
  AboutMemo.Lines.Add('Please report bugs at: %s', [SIMBA_BUGS_URL]);
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Built with:');
  AboutMemo.Lines.Add('  FPC %d.%d.%d', [FPC_VERSION, FPC_RELEASE, FPC_PATCH]);
  AboutMemo.Lines.Add('  Lazarus version %s', [LAZ_VERSION]);
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Simba SHA1 hash: %s', [TSimbaFile.FileHash(Application.ExeName)]);

  if (SIMBA_COMMIT <> '') then
  begin
    AboutMemo.Lines.Add('Commit hash: %s', [SIMBA_COMMIT]);
    AboutMemo.Lines.Add('Commit URL: %s', [SIMBA_COMMIT_URL]);
  end;
end;

procedure TSimbaAboutForm.ButtonExitClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaAboutForm.FormCreate(Sender: TObject);
begin
  Width := Scale96ToScreen(550);
  Height := Scale96ToScreen(450);
end;

procedure TSimbaAboutForm.VersionLabelClick(Sender: TObject);
begin
  if (SIMBA_COMMIT <> '') then
    OpenURL(SIMBA_COMMIT_URL);
end;

procedure TSimbaAboutForm.VersionLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := True;
end;

procedure TSimbaAboutForm.VersionLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := False;
end;

end.

