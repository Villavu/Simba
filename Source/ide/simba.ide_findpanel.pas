{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_findpanel;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, Graphics, SynEdit, Forms, LCLType,
  simba.component_theme,
  simba.component_edit,
  simba.component_button;

type
  TSimbaFindPanel = class(TCustomControl)
  protected
    FEdit: TSimbaEdit;
    FButtonDown: TSimbaButton;
    FButtonUp: TSimbaButton;
    FButtonCaseSens: TSimbaToggleButton;
    FButtonWholeWord: TSimbaToggleButton;
    FFindButtonClose: TSimbaTransparentButton;

    procedure DoResize(Sender: TObject);
    procedure DoButtonResize(Sender: TObject);
    procedure DoCloseButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    property Edit: TSimbaEdit read FEdit;
    property ButtonDown: TSimbaButton read FButtonDown;
    property ButtonUp: TSimbaButton read FButtonUp;
    property ButtonCaseSens: TSimbaToggleButton read FButtonCaseSens;
    property ButtonWholeWord: TSimbaToggleButton read FButtonWholeWord;
  end;

implementation

uses
  simba.component_images;

procedure TSimbaFindPanel.DoResize(Sender: TObject);
begin
  FEdit.Width := ClientWidth div 3;
end;

// Make all the buttons the same width based on the largest.
procedure TSimbaFindPanel.DoButtonResize(Sender: TObject);

  procedure EnsureWidth(const A, B: TWinControl);
  begin
    if (A.Constraints.MinWidth < B.Width) then
      A.Constraints.MinWidth := B.Width;
  end;

begin
  EnsureWidth(FButtonDown, TWinControl(Sender));
  EnsureWidth(FButtonUp, TWinControl(Sender));
  EnsureWidth(FButtonCaseSens, TWinControl(Sender));
  EnsureWidth(FButtonWholeWord, TWinControl(Sender));
end;

procedure TSimbaFindPanel.DoCloseButtonClick(Sender: TObject);
begin
  Visible := False;
end;

constructor TSimbaFindPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := True;
  OnResize := @DoResize;
  Color := SimbaComponentTheme.ColorFrame;

  FEdit := TSimbaEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alLeft;
  FEdit.BorderSpacing.Around := 5;

  FButtonWholeWord := TSimbaToggleButton.Create(Self);
  with FButtonWholeWord do
  begin
    Parent := Self;
    Caption := 'W';

    Hint := 'Match whole words';
    ShowHint := True;

    OnResize := @DoButtonResize;

    Anchors := [akTop, akLeft, akBottom];
    AnchorSide[akTop].Control := FEdit;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akLeft].Control := FEdit;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akBottom].Control := FEdit;
    AnchorSide[akBottom].Side := asrBottom;

    BorderSpacing.Left := 5;
    Font.Bold := True;
  end;

  FButtonCaseSens := TSimbaToggleButton.Create(Self);
  with FButtonCaseSens do
  begin
    Parent := Self;
    Caption := 'Aa';
    Hint := 'Case sensitive';
    ShowHint := True;
    OnResize := @DoButtonResize;

    Anchors := [akTop, akLeft, akBottom];
    AnchorSide[akTop].Control := FEdit;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akLeft].Control := FButtonWholeWord;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akBottom].Control := FEdit;
    AnchorSide[akBottom].Side := asrBottom;

    BorderSpacing.Left := 5;
    Font.Bold := True;
  end;

  FButtonDown := TSimbaButton.Create(Self);
  with FButtonDown do
  begin
    Parent := Self;
    ImageIndex := SimbaImages.ARROW_DOWN_RED;
    Hint := 'Find Next';
    ShowHint := True;
    ImageList := SimbaImages;

    OnResize := @DoButtonResize;

    Anchors := [akTop, akLeft, akBottom];
    AnchorSide[akTop].Control := FEdit;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akLeft].Control := FButtonCaseSens;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akBottom].Control := FEdit;
    AnchorSide[akBottom].Side := asrBottom;

    BorderSpacing.Left := 5;
  end;

  FButtonUp := TSimbaButton.Create(Self);
  with FButtonUp do
  begin
    Parent := Self;
    ImageIndex := SimbaImages.ARROW_UP_GREEN;
    Hint := 'Find Previous';
    ShowHint := True;
    ImageList := SimbaImages;
    OnResize := @DoButtonResize;

    Anchors := [akTop, akLeft, akBottom];
    AnchorSide[akTop].Control := FEdit;
    AnchorSide[akTop].Side := asrTop;
    AnchorSide[akLeft].Control := FButtonDown;
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akBottom].Control := FEdit;
    AnchorSide[akBottom].Side := asrBottom;

    BorderSpacing.Left := 5;
  end;

  FFindButtonClose := TSimbaTransparentButton.Create(Self);
  with FFindButtonClose do
  begin
    Parent := Self;
    Align := alRight;
    Image := ESimbaButtonImage.CLOSE;
    OnClick := @DoCloseButtonClick;

    BorderSpacing.Around := 5;
  end;
end;

end.

