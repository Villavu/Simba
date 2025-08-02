{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Custom drawn dialogs to replace MessageDlg, ShowMessage etc
}
unit simba.dialog;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaDialogIcon = (NONE, ERROR, WARNING, INFO);
  ESimbaDialogButton = (YES, NO, OK, CANCEL, ABORT, RETRY, IGNORE, ALL, NO_TO_ALL, YES_TO_ALL, CLOSE);
  ESimbaDialogButtons = set of ESimbaDialogButton;
  {$POP}

  function ShowDialog(Icon: ESimbaDialogIcon; Buttons: ESimbaDialogButtons; Title, Msg: String): ESimbaDialogButton;

  function ShowQuestionDialog(Title: String; Question: String; Args: array of const): ESimbaDialogButton; overload;
  function ShowQuestionDialog(Title: String; Question: TStringArray; Args: array of const): ESimbaDialogButton; overload;

  procedure ShowErrorDialog(Title: String; Err: String; Args: array of const); overload;
  procedure ShowErrorDialog(Title: String; Err: TStringArray; Args: array of const); overload;

implementation

uses
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  DialogRes,
  LCLType,
  simba.component_theme,
  simba.component_button;

type
  TSimbaDialog = class(TForm)
  private type
    TLabelProtectedAccess = class(TLabel);
  private const
    ButtonTexts: array[ESimbaDialogButton] of String = ('Yes', 'No', 'Ok', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'No to all', 'Yes to all', 'Close');
    ButtonIcons: array[ESimbaDialogIcon] of Integer = (0, idDialogError, idDialogWarning, idDialogInfo);
  private
    FIcon: TImage;
    FMessageLabel: TLabel;
    FMainPanel: TPanel;
    FButtonPanel: TPanel;

    procedure AddButtons(Buttons: ESimbaDialogButtons);
    procedure SetIcon(AIcon: ESimbaDialogIcon);
    procedure DoButtonClick(Sender: TObject);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
  public
    DialogResult: ESimbaDialogButton;

    constructor Create(AIcon: ESimbaDialogIcon; AButtons: ESimbaDialogButtons; ATitle, AMessage: String); reintroduce;
  end;

procedure TSimbaDialog.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
var
  W, H: Integer;
  I: Integer;
  ButtonsWidth: Integer;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  W := 0;
  H := 0;
  TLabelProtectedAccess(FMessageLabel).CalculateSize(Monitor.Width div 2, W, H);

  ButtonsWidth := 0;
  for I := 0 to FButtonPanel.ControlCount - 1 do
    ButtonsWidth += FButtonPanel.Controls[I].Width;
  PreferredWidth := FMessageLabel.Left + FMessageLabel.BorderSpacing.Right + Max(W, ButtonsWidth);
end;

procedure TSimbaDialog.DoButtonClick(Sender: TObject);
begin
  DialogResult := ESimbaDialogButton(TSimbaButton(Sender).Tag);
  Close();
end;

procedure TSimbaDialog.AddButtons(Buttons: ESimbaDialogButtons);
var
  ButtonTyp: ESimbaDialogButton;
  Button: TSimbaButton;
begin
  for ButtonTyp in Buttons do
  begin
    Button := TSimbaButton.Create(Self);
    Button.Parent := FButtonPanel;
    Button.Caption := ButtonTexts[ButtonTyp];
    Button.AutoSize := True;
    Button.Align := alRight;
    Button.BorderSpacing.Around := 5;
    Button.Tag := Ord(ButtonTyp);
    Button.OnClick := @DoButtonClick;
  end;
end;

procedure TSimbaDialog.SetIcon(AIcon: ESimbaDialogIcon);
begin
  if (AIcon = ESimbaDialogIcon.NONE) then
    FIcon.Visible := False
  else
    DialogGlyphs.GetBitmap(DialogGlyphs.DialogIcon[ButtonIcons[AIcon]], FIcon.Picture.Bitmap);
end;

constructor TSimbaDialog.Create(AIcon: ESimbaDialogIcon; AButtons: ESimbaDialogButtons; ATitle, AMessage: String);
begin
  CreateNew(Application);

  // default to cancel for when close button is clicked
  DialogResult := ESimbaDialogButton.CANCEL;

  Constraints.MinWidth := Scale96ToScreen(200);
  Constraints.MinHeight := Scale96ToScreen(100);
  Caption := ATitle;
  Position := poScreenCenter;
  AutoSize := True;

  FMainPanel := TPanel.Create(Self);
  FMainPanel.Parent := Self;
  FMainPanel.Align := alClient;
  FMainPanel.Color := SimbaComponentTheme.ColorBackground;
  FMainPanel.AutoSize := True;
  FMainPanel.BevelOuter := bvNone;

  FIcon := TImage.Create(FMainPanel);
  FIcon.Parent := FMainPanel;
  FIcon.AutoSize := True;

  FMessageLabel := TLabel.Create(Self);
  FMessageLabel.Parent := FMainPanel;
  FMessageLabel.Anchors := [akLeft, akTop, akRight];
  FMessageLabel.AnchorSide[akLeft].Side := asrRight;
  FMessageLabel.AnchorSide[akLeft].Control := FIcon;
  FMessageLabel.AnchorSide[akRight].Side := asrRight;
  FMessageLabel.AnchorSide[akRight].Control := FMainPanel;
  FMessageLabel.AnchorSide[akTop].Side := asrCenter;
  FMessageLabel.AnchorSide[akTop].Control := FMainPanel;
  FMessageLabel.Font.Color := SimbaComponentTheme.ColorFont;
  FMessageLabel.WordWrap := True;
  FMessageLabel.BorderSpacing.Around := 15;
  FMessageLabel.Caption := AMessage;

  FIcon.Anchors := [akLeft, akTop];
  FIcon.AnchorSide[akLeft].Control := FMainPanel;
  FIcon.AnchorSide[akLeft].Side := asrLeft;
  FIcon.AnchorSide[akTop].Side := asrCenter;
  FIcon.AnchorSide[akTop].Control := FMessageLabel;
  FIcon.BorderSpacing.Around := 15;

  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := Self;
  FButtonPanel.Align := alBottom;
  FButtonPanel.Color := SimbaComponentTheme.ColorFrame;
  FButtonPanel.AutoSize := True;
  FButtonPanel.BevelOuter := bvNone;

  SetIcon(AIcon);
  AddButtons(AButtons);
end;

function ShowDialog(Icon: ESimbaDialogIcon; Buttons: ESimbaDialogButtons; Title, Msg: String): ESimbaDialogButton;
begin
  with TSimbaDialog.Create(Icon, Buttons, Title, Msg) do
  try
    ShowModal();

    Result := DialogResult;
  finally
    Free();
  end;
end;

function ShowQuestionDialog(Title: String; Question: String; Args: array of const): ESimbaDialogButton;
begin
  Result := ShowDialog(ESimbaDialogIcon.INFO, [ESimbaDialogButton.YES, ESimbaDialogButton.NO, ESimbaDialogButton.CANCEL], Title, Question.Format(Args));
end;

function ShowQuestionDialog(Title: String; Question: TStringArray; Args: array of const): ESimbaDialogButton;
begin
  Result := ShowQuestionDialog(Title, ''.Join(LineEnding, Question), Args);
end;

procedure ShowErrorDialog(Title: String; Err: String; Args: array of const);
begin
  ShowDialog(ESimbaDialogIcon.INFO, [ESimbaDialogButton.OK], Title, Err.Format(Args));
end;

procedure ShowErrorDialog(Title: String; Err: TStringArray; Args: array of const);
begin
  ShowErrorDialog(Title, ''.Join(LineEnding, Err), Args);
end;

end.

